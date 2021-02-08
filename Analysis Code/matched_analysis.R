# This file takes data from matched_clean and matched_jobs_clean
# to compute summary statistics

rm(list = ls())

library(weights)
library(magrittr)
library(estimatr)
library(data.table)
library(openxlsx)
library(BSDA)
library(srvyr)
library(DescTools)
library(tidyverse)

# Folders of interest
clean_folder <- "../Cleaned Data/"
table_folder <- "../Tables/"
figure_folder <- "../Figures/NLSY 79 Matched/"
d_table_folder <- "../Drafts/Draft Tables/"
s_table_folder <- "../Slides/Slide Tables/"

# For saving graphs
aspect_ratio <- 1.62
height <- 7
width <- height * aspect_ratio

# Download data and filter out women (only look at men)
matched <- read_csv(str_c(clean_folder, "matched.csv"),
  col_types = cols(
    .default = col_double(),
    week_start_job = col_date(format = ""),
    week_end_job = col_date(format = "")
  )) %>% 
  filter(female == 0)

matched_jobs <- read_csv(str_c(clean_folder, "matched_jobs.csv"),
                         col_types = cols(
                           .default = col_double(),
                           week_start_job = col_date(format = ""),
                           week_end_job = col_date(format = "")
                         )) %>% 
  filter(female == 0)

# Some figures calculated are useful for calbirating the model.
# Use these to update data_moments
data_moments <- read_csv(str_c(clean_folder, "data_moments.csv"),
                         col_types = cols(
                           variable = col_character(),
                           value = col_double()
                         ))

# Create a function to update data_moments given variable
# with correct name
update_parameters <- function(name, val) {
  data_moments$value[data_moments$variable == name] <- val
  data_moments
}

# Create a function that takes regression p-values and reports
# * if 10%, ** if 5%, and *** if 1% significant
p_stars <- function(p){
  if (p < .01){
    stars <- "\\textsuperscript{***}"
  } else if (p < .05){
    stars <- "\\textsuperscript{**}"
  } else if (p < .1){
    stars <- "\\textsuperscript{*}"
  } else{
    stars <- ""
  }
  return(stars)
}

# Create a function that finds difference of means or proportions and reports
# * if 10%, ** if 5%, and *** if 1% different
# means uses cond, proportion uses var_2
test <- function(data, var, weight, type, cond = T, cond_y = T, divider = NULL) {
  if (type == "mean") {
    data_x <- data[cond,]
    data_y <- data[cond_y,]
    outcome <- wtd.t.test(x=data_x[[var]], y=data_y[[var]],
                          weight=data_x[[weight]],
                          weighty=data_y[[weight]],
                          samedata = FALSE)
    p <- outcome$coefficients["p.value"]
  } else if (type == "prop") {
    # (round outcome to make sure it's 0/1 (useful when we sometimes time averages))
    outcome <- wtd.chi.sq(round(data[[var]]), data[[divider]], weight = data[[weight]])
    p <- outcome[["p.value"]]
  } else {
    return(warning("Not a valid test"))
  }
  
  p_stars(p)
}

# Create a function to properly format inputs
format_it <- function(var, r = 2, s = 2) {
  format(round(var, r), nsmall = s, scientific = F)
}

# Create a function to put values in tables
format_val <- function(var, star = "", r = 2, s = 2) {
  str_c(" & ", format_it(var, r, s), star, " ")
}

# Create a function to put standard errors in tables
format_se <- function(var, r = 2, s = 2) {
  str_c(" & (", format_it(var, r, s), ") ")
}

# Create a function to put N obs in tables
format_n <- function(n) {
  str_c(" & {", format(n, big.mark = ",", trim = T), "} ")
}

# Create a function to format percents
format_per <- function(var, r = 2, s = 2) {
  str_c(" & ", format(round(var * 100, r), nsmall = s), " ")
}

# Create a function to sum rows of a variable, droppin NAs
r_sum <- function(...) {
  rowSums(cbind(...), na.rm = T)
}

# Create a function that takes an lm_robust model and returns residuals
# Drop residuals == 0 (these are ones with single-use fixed effects)
lm_residuals <- function(mod) {
  resids <- model.frame(mod)[[mod$outcome]] - mod$fitted.values
}

# Create a function that takes an lm_robust model and calculates
# "effective N" or values actually used to estimate coefficients
# (drops single FE)
lm_N <- function(mod) {
  sum(lm_residuals(mod) != 0)
}

# I often want to run code on whole sample and on ever_ho_occ. 
# Pair these dfs in a list
split_data <- function(df) {
  list(df, filter(df, ever_ho_occ == 1))
}

# Create a function that makes a formula given dependent variable 
# and list of independent variables
create_formula <- function(y, x_list) {
  vars <- x_list %>% 
    map(str_c, collapse = "+") %>% 
    str_c(collapse = "+")
  
  eq <- formula(str_c(y, vars, sep = "~"))
}

# Define our default table top
table_top <- "\\documentclass[12pt]{article}
\\usepackage[margin=.5in]{geometry}
\\usepackage{siunitx}
\\usepackage{booktabs}
\\usepackage{graphicx}
\\begin{document}
\\begin{table}
\\centering 
\\resizebox{\\textwidth}{!}{ \n"

# If using siunitx, include this too
siunitx <- "\\sisetup{
table-number-alignment = center,
table-figures-integer = 3,
table-figures-decimal = 3,
input-symbols=(),
table-space-text-post = \\textsuperscript{***},
table-align-text-post = false,
group-digits          = false
}"

# Create a default top for Draft tables (no resizebox)
d_table_top <- "\\begin{table}[h!]
\\centering 
{ \n"

# Create a default top for Slide tables (but sometimes use it for Drafts)
s_table_top <- "\\begin{table}[h!]
\\centering 
\\resizebox{\\textwidth}{!}{ \n"

# Create a default bottom for Slide tables (no details)
s_bot <- "\\bottomrule
\\end{tabular}
}
\\end{table}"

# Create married/single if certain marital status
matched %<>%
  mutate(
    single = 1 * (marital_status == 1),
    married = 1 * (marital_status == 2)
  )

matched_jobs %<>%
  mutate(
    single = 1 * (marital_status == 1),
    married = 1 * (marital_status == 2)
  )

splits <- c(split_data(matched), split_data(matched_jobs))

# Demographic Summary Statistics ------------------------------------------------------

# First, look at people who ever outsource vs those who never do (ever_out_oj == 1/0)
# Overall and ever in HO Occupations
# And those ever in HO occupations vs not (ever_ho_occ == 1/0)
# Weight based on population
# Look only at first observation for demographics
vars <- c("black", "hispanic", "less_hs", "hs", "aa", "ba", "plus_ba",
          "single", "married", "tot_child", "hh_child", "n")

vars_ho <- c(vars, "ever_out_oj")

vars <- c(vars, "ever_ho_occ")

sum_demo <- c(split_data(matched), list(matched))
# For running t/p tests
comp <- split_data(matched)

for (i in 1:2) {
  sum_demo[[i]] %<>%
    group_by(case_id) %>% 
    filter(row_number() == min(row_number())) %>% 
    as_survey_design(ids = case_id, weights = weight) %>% 
    group_by(ever_out_oj) %>%
    mutate(n = n()) %>% 
    summarise_at(vars, survey_mean, na.rm = T) %>% 
    arrange(desc(ever_out_oj)) 
  
  
  comp[[i]] %<>%
    group_by(case_id) %>% 
    filter(row_number() == min(row_number())) 
}

sum_demo[[3]] %<>%
  group_by(case_id) %>% 
  filter(row_number() == min(row_number())) %>% 
  as_survey_design(ids = case_id, weights = weight) %>% 
  group_by(ever_ho_occ) %>%
  mutate(n = n()) %>% 
  summarise_at(vars_ho, survey_mean, na.rm = T) %>% 
  arrange(desc(ever_ho_occ)) 

# Baseline data same as comp[[1]]
comp <- c(comp, list(comp[[1]]))

# Create table in Latex
vars_p <- c("black", "hispanic", "less_hs", "hs", "aa", "ba", "plus_ba",
            "single", "married")

# Create a table with both ever_out_oj and ever_ho_occ in one
n_center <- rbind("Black", "", "Hispanic", "", "No HS Diploma", "", "HS Diploma",
                "", "AA Degree", "", "BA Degree", "", "Post Graduate", "Degree",
                "Single", "", "Married", "", "Total Number", "of Children",
                "Children in", "Household", "Observations")

# Needed to space out table
space <- cbind(rbind(" & {1}", " & "),
               rbind(" & {0}", " & ")
)

for (ho in 1:3) {
  
  c_i <- c()
  
  if (ho == 1) {
    description <- "who work at least one outsourced job (in On Jobs survey)"
    label <- "_ever_out"
    save <- ""
    header <- "Outsourced"
    divider <- "ever_out_oj"
  } else if (ho == 2) {
    description <- " who ever work in a high outsourcing occupation, comparing 
    those who work at least one outsourced job (in On Jobs survey)"
    label <- "_ever_out_ever_ho"
    save <- " Ever Outsourced in Ever HO Occupation"
    header <- "Outsourced"
    divider <- "ever_out_oj"
  } else {
    description <- " who work at least one job in a high outsourcing occupation"
    label <- "_ever_ho"
    save <- " Ever HO Occupation"
    header <- "HO Occupation"
    divider <- "ever_ho_occ"
  }
  
  cond <- comp[[ho]][[divider]] == 1
  
  for (i in 1:2) {
    col_i <- c()
    for (var in vars[c(-length(vars), -(length(vars)- 1))]) {
      se <- str_c(var, "_se")
      stars <- ""
      if (i == 2){
        mode <- if (var %in% vars_p) "prop" else "mean"
        stars <- test(comp[[ho]], var, "weight", mode, cond, !cond, divider)
      }
      col_i %<>% rbind(format_val(sum_demo[[ho]][[var]][i], star = stars),
                       format_se(sum_demo[[ho]][[se]][i]))
    }
    col_i %<>% rbind(format_n(sum_demo[[ho]][["n"]][i])) 
    center <- cbind(n_center, col_i)
    c_i %<>% cbind(col_i)
  }
  
  center %<>% cbind( 
    rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\")
  )
  
  # If looking at ever_out_oj, also look at ever_ho_occ and add to combined
  if (ho == 1) {
    var <- "ever_ho_occ"
    se <- str_c(var, "_se")
    stars <- test(comp[[ho]], var, "weight", "prop", cond, !cond, divider)
    add_out <- cbind(rbind("Percent Ever", "HO Occupation"),
                 rbind(format_val(sum_demo[[ho]][[var]][1]),
                       format_se(sum_demo[[ho]][[se]][1])),
                 rbind(format_val(sum_demo[[ho]][[var]][2], star = stars),
                       format_se(sum_demo[[ho]][[se]][2])),
                 space
    )
    c_center <- cbind(n_center, c_i)
  }
  
  # If looking at Ever HO Occupation, also look at percent ever outsourced 
  # and add to combined
  if (ho == 3) {
    var <- "ever_out_oj"
    se <- str_c(var, "_se")
    stars <- test(comp[[ho]], var, "weight", "prop", cond, !cond, divider)
    add_ho <- cbind(rbind("Percent Ever", "Outsourced"),
                 space,
                 rbind(format_val(sum_demo[[ho]][[var]][1]),
                       format_se(sum_demo[[ho]][[se]][1])),
                 rbind(format_val(sum_demo[[ho]][[var]][2], star = stars),
                       format_se(sum_demo[[ho]][[se]][2]))
            )
    c_center %<>% cbind(c_i)
    c_center <- rbind(add_out, add_ho, c_center)
    c_center %<>% cbind(
      rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
            "\\\\", "\\\\[2pt]", "\\\\")
    )  
  }
  
  # Do weird stuff to create LaTeX output
  j_folder <- str_c(table_folder, "Junk/")
  file_1 <- str_c(j_folder, "center.txt")
  write.table(center, file_1, quote=T, col.names=F, row.names=F)
  center <- read.table(file_1, sep = "")
  write.table(center, file_1, quote=F, col.names=F, row.names=F, sep = "")
  center <- readChar(file_1, nchars = 1e6)
  
  top <- str_c(
  "\\begin{tabular}{lSS}
\\toprule
Variable & {Ever ", header, "} & {Never ", header, "} \\\\ \\midrule
")
  
  bot <- str_c(
  "\\bottomrule
\\end{tabular}
}
\\caption{Demographic statistics for men ", description, " versus those who never do.
Observations are at the person level for first survey post-2000 
and summary statistics are weighted.
Stars represent significant difference at the .10 level *,
.05 level **, and .01 level ***.}
\\label{demo", label, "}
\\end{table}"
  )

  write.table(str_c(table_top, siunitx, top, center, bot, "\n \\end{document"),
              str_c(table_folder, "NLSY79 Demographics/Demographics", save, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
}


# For slides, keep only some rows
d_center <- c_center[c(1:18, 27),]

top <- "\\begin{tabular}{lSSSS}
\\toprule
& \\multicolumn{2}{c}{{Outsourced}} & \\multicolumn{2}{c}{{HO Occupation}} \\\\
Variable & {Ever} & {Never} & {Ever} & {Never} \\\\ \\midrule
"

bot <- "\\bottomrule
\\end{tabular}
}
\\caption{Demographic statistics from the NLSY for those who are ever outsourced in On Jobs
versus those who never are and for those who ever work in high outsourcing (HO)
occupations versus those who never do. Observations are at the person level
from an individual's first survey post-2000 and summary statistics are weighted at
the person level. Stars represent significant difference at the .10 level *,
.05 level **, and .01 level ***.}
\\label{demo_comp}
\\end{table}"

# Do weird stuff to create LaTeX output
j_folder <- str_c(table_folder, "Junk/")
file_1 <- str_c(j_folder, "center.txt")
write.table(c_center, file_1, quote=T, col.names=F, row.names=F)
c_center <- read.table(file_1, sep = "")
write.table(c_center, file_1, quote=F, col.names=F, row.names=F, sep = "")
c_center <- readChar(file_1, nchars = 1e6)

# Do weird stuff to create LaTeX output
j_folder <- str_c(table_folder, "Junk/")
file_1 <- str_c(j_folder, "center.txt")
write.table(d_center, file_1, quote=T, col.names=F, row.names=F)
d_center <- read.table(file_1, sep = "")
write.table(d_center, file_1, quote=F, col.names=F, row.names=F, sep = "")
d_center <- readChar(file_1, nchars = 1e6)

# Save for Drafts and Slides
write.table(str_c(d_table_top, top, c_center, bot),
            str_c(d_table_folder, "Demographics Comparison.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Table to long for resize to work, so manually set size
write.table(str_c(d_table_top, "\n \\tiny \n", top, d_center, s_bot),
            str_c(s_table_folder, "Demographics Comparison.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# Job Number Statistics --------------------------------------------------

life_jobs <- split_data(matched_jobs)

for (i in 1:2) {
  life_jobs[[i]] %<>% 
    group_by(case_id) %>% 
    mutate(
      n_jobs = n(),
      n_out_jobs = sum(outsourced)
    ) %>% 
    filter(row_number() == min(row_number())) %>% 
    as_survey_design(ids = case_id, weights = weight) %>% 
    mutate(ever_out_oj = factor(ever_out_oj)) %>%
    group_by(ever_out_oj) %>% 
    summarise(
      n_jobs_mean = survey_mean(n_jobs),
      n_out_jobs_mean = survey_mean(n_out_jobs),
      n = unweighted(n()),
      n_only_out = unweighted(sum(n_out_jobs == n_jobs))
    ) %>% 
    arrange(desc(ever_out_oj))
}

# # How many people only outsource in dataset?
# only_outsource <- matched_jobs %>% 
#   group_by(case_id) %>% 
#   mutate(per_out = mean(outsourced)) %>% 
#   filter(per_out == 1)

# Record results in LateX
table <- str_c(table_top, siunitx,
"
\\begin{tabular}{lSS}
\\toprule
Variable & {Ever Outsourced} & {Never Outsourced} \\\\ \\midrule \n"
)

labels <- c("Whole Sample", "Ever HO Occupation")

for (ho in 1:2) {
  table %<>% str_c(
    labels[ho], " & & \\\\ \\midrule \n
    Number of Jobs", format_val(life_jobs[[ho]]$n_jobs_mean[1]),
                   format_val(life_jobs[[ho]]$n_jobs_mean[2]), "\\\\ \n",
    format_se(life_jobs[[ho]]$n_jobs_mean_se[1]),
    format_se(life_jobs[[ho]]$n_jobs_mean_se[2]), "\\\\ \n 
    Number of Outsourced Jobs", format_val(life_jobs[[ho]]$n_out_jobs_mean[1]),
    " & {--} \\\\ \n ", format_se(life_jobs[[ho]]$n_out_jobs_mean_se[1]), " & \\\\ \n
    Number Only Outsourced", format_n(life_jobs[[ho]]$n_only_out[1]), " & {--} \\\\ \n
    Observations", format_n(life_jobs[[ho]]$n[1]), format_n(life_jobs[[ho]]$n[2]), 
    "\\\\"
  )
  if (ho == 1) {
    table %<>% str_c("\\midrule \n")
  }
}

table %<>% str_c(
"\n
\\bottomrule
\\end{tabular}
}
\\caption{Job numbers for men.}
\\label{job_numbers}
\\end{table}
\\end{document}"
)

write.table(table, str_c(table_folder, "NLSY79 Jobs/Job Numbers.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# Jobs Summary Statistics -------------------------------------------------

# Summarize jobs at the year level using matched and job level using
# matched_jobs. Split by outsourced or not and by all job types

vars_sum <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week",
          "part_time", "tenure", "union", "job_sat", "any_benefits",
          "health", "retirement", "childcare", "dental", "flex_sched",
          "life", "maternity", "profit_share", "train_school", "n")

sum_jobs <- c(splits, splits)

for (i in 1:4) {
  sum_jobs[[i]] %<>%
    as_survey_design(ids = case_id, weights = weight) %>% 
    group_by(outsourced) %>% 
    mutate(n = n()) %>% 
    summarise_at(vars_sum, survey_mean, na.rm = T) %>% 
    arrange(desc(outsourced))
  
  sum_jobs[[i + 4]] %<>%
    as_survey_design(ids = case_id, weights = weight) %>% 
    group_by(temp_work, on_call, indep_con, self_emp, traditional, outsourced) %>% 
    mutate(n = n()) %>% 
    summarise_at(vars_sum, survey_mean, na.rm = T)
}

# Create Latex tables
top_j <- "\\begin{tabular}{lSS}
\\toprule
& {Outsourced} & {Non-Outsourced} \\\\  \\midrule
"

top_j_t <- "\\begin{tabular}{lSSSSSS}
\\toprule
Variable & {Outsourced} & {Traditional} & {Self-Employed} & {Ind. Contractor} & {On-Call} & {Temp} \\\\ \\midrule 
"

top_j_o_t <- "\\begin{tabular}{lSS}
\\toprule
& {Outsourced} & {Traditional} \\\\  \\midrule
"

types <- c("outsourced", "traditional", "self_emp", 
           "indep_con", "on_call", "temp_work")

# Create table in Latex
# Use all vars except n
vars <- vars_sum[-length(vars_sum)]

# Divide variables by mean or prop (they are different below)
# vars_m <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week",
#               "tenure", "job_sat")

vars_p <- c("part_time", "union", "any_benefits", "health", "retirement",
            "childcare", "dental", "flex_sched", "life", "maternity",
            "profit_share", "train_school")

dividers <- c()
sample <- c("", " for workers who are ever in high outsourcing industries")
observation <- c("person-job-year", "person-job")
explanation <- c(
  "", ", where jobs observed more than once use average or modal characteristics"
  )
label <- c("_year", "_year_ho", "_job", "_job_ho")
folder <- c("Jobs", "Job Types")
save <- c("Year", "Year HO Occupations", "Job", "Job HO Occupations")

for (ty in 1:2) {
  for (ho in 1:2) {
    for (ob in 1:2) { 
    
    j <- 4 * (ty - 1) + 2 * (ob - 1) + ho 
    k <- 2 * (ob - 1) + ho 
    
    center <- rbind(
      "Log Real", "Hourly Wage", "Log Real", "Weekly Wage", "Hours Worked", 
      "Weekly", "Part Time", "", "Tenure", "(Weeks)", "Union", "",
      "Job Satisfaction", "(Lower Better)", "Any Benefits", "", "Health",
      "Insurance", "Retirement", "Plan", "Subsidized", "Childcare", "Dental",
      "Insurance", "Flex", "Schedule", "Life", "Insurance", "Maternity",
      "Leave", "Profit", "Sharing", "Training", "", "Observations")
    
    # What is the comparison dataset for test of differences?
    comp_r <- splits[[k]]
    
    for (i in 1:NROW(sum_jobs[[j]])){
      col_i <- c()
      # If ty == 2 (looking at all types), then for proportion tests,
      # need to get rid of all types but that type and outsourced
      type <- types[i]
      if (ty == 2) {
        cond <- comp_r$outsourced == 1
        cond_y <- comp_r[[type]] == 1
        comp <- comp_r[cond | cond_y, ]
        # For these, need to recreate cond and cond_y using comp
        cond <- comp$outsourced == 1
        cond_y <- comp[[type]] == 1
      } else {
        cond <- comp_r$outsourced == 1
        cond_y <- comp_r$outsourced == 0
        comp <- comp_r
      }
      for (var in vars){
        se <- str_c(var, "_se")
        mode <- if (var %in% vars_p) "prop" else "mean"
        stars <- if (i > 1) test(
          comp, var, "weight", mode, cond, cond_y, "outsourced"
          ) else ""
        col_i %<>% rbind(
          format_val(sum_jobs[[j]][[var]][i], star = stars),
          format_se(sum_jobs[[j]][[se]][i])
        )
      }
      col_i %<>% rbind(format_n(sum_jobs[[j]]$n[i]))
      center %<>% cbind(col_i)
    }
  
    center %<>% cbind(
      rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",  "\\\\"))
    
    # For Slides, (only if ty == 2) do not want self_emp or on_call
    # and only care about a few variables
    if (ty == 2) {
      s_center <- center[c(1:4, 7:12, 15:18, 35), c(1:3, 5, 7:8)]
      
      
      # For Draft and Slides, (only if ty == 2) create tables with
      # outsourced and traditional.
      # For slides only care about a few variables
      center_o_t <- center[, c(1:3, 8)]
      s_center_o_t <- center[c(1:4, 7:12, 15:18, 35), c(1:3, 8)]
    }
  
    # Do weird stuff to create LaTeX output
    j_folder <- str_c(table_folder, "Junk/")
    file_1 <- str_c(j_folder, "center.txt")
    write.table(center, file_1, quote=T, col.names=F, row.names=F)
    center <- read.table(file_1, sep = "")
    write.table(center, file_1, quote=F, col.names=F, row.names=F, sep = "")
    center <- readChar(file_1, nchars = 1e6)
  
    if (ty == 1) {
      top <- top_j
      bot <- str_c(
"\\bottomrule
\\end{tabular}
}
\\caption{Summary statistics of jobs in the NLSY divided by outsourced 
    and non-outsourced jobs",
sample[ho], ". 
Observations are at the ", observation[ob], " level", explanation[ob],
". All statistics are weighted at the person level. 
Stars represent significant difference from outsourced jobs at the .10 level *, 
.05 level **, and .01 level ***.}
\\label{jobs", label[k], "}
\\end{table}"
      )
    } else {
      top <- top_j_t
      bot <- str_c(
"\\bottomrule
\\end{tabular}
}
\\caption{Summary statistics of jobs in the NLSY divided by job types",
        sample[ho], ". 
Observations are at the ", observation[ob], " level", explanation[ob],
". All statistics are weighted at the person level.
Stars represent significant difference from outsourced jobs at the .10 level *, 
.05 level **, and .01 level ***.}
\\label{job_types", label[k], "}
\\end{table}"
    )
    
    # Do weird stuff to create LaTeX output
    j_folder <- str_c(table_folder, "Junk/")
    file_1 <- str_c(j_folder, "center.txt")
    write.table(center_o_t, file_1, quote=T, col.names=F, row.names=F)
    center_o_t <- read.table(file_1, sep = "")
    write.table(center_o_t, file_1, quote=F, col.names=F, row.names=F, sep = "")
    center_o_t <- readChar(file_1, nchars = 1e6)
    
    bot_o_t <- str_c(
      "\\bottomrule
\\end{tabular}
}
\\caption{Summary statistics of jobs in the NLSY divided by outsourced and traditional
  jobs",
  sample[ho], ". 
Observations are at the ", observation[ob], " level", explanation[ob],
  ". All statistics are weighted at the person level.
Stars represent significant difference from outsourced jobs at the .10 level *, 
.05 level **, and .01 level ***.}
\\label{job_types", label[k], "_o_t}
\\end{table}"
      )
      
      # If ho == 1, save a version in Drafts and Slides
      if (ho == 1) {
        write.table(str_c(s_table_top, top, center, bot),
                    str_c(d_table_folder, "Job Summary Statistics ", save[k], ".tex"),
                    quote=F, col.names=F, row.names=F, sep="")
        
        # Do weird stuff to create LaTeX output
        j_folder <- str_c(table_folder, "Junk/")
        file_1 <- str_c(j_folder, "center.txt")
        write.table(s_center, file_1, quote=T, col.names=F, row.names=F)
        s_center <- read.table(file_1, sep = "")
        write.table(s_center, file_1, quote=F, col.names=F, row.names=F, sep = "")
        s_center <- readChar(file_1, nchars = 1e6)
        
        s_top <- "\\begin{tabular}{lSSSS}
\\toprule
Variable & {Outsourced} & {Traditional} & {Ind. Contractor} & {Temp} \\\\ \\midrule 
"
        
        write.table(str_c(s_table_top, s_top, s_center, s_bot),
                    str_c(s_table_folder, "Job Summary Statistics ", save[k], ".tex"),
                    quote=F, col.names=F, row.names=F, sep="")
        
        # Just save outsourced and traditional
        if (ty == 2){
          write.table(str_c(d_table_top, top_j_o_t, center_o_t, bot_o_t),
                      str_c(d_table_folder, "Job Summary Statistics ", save[k], 
                            " Out + Trad.tex"),
                      quote=F, col.names=F, row.names=F, sep="")
          
          # Do weird stuff to create LaTeX output
          j_folder <- str_c(table_folder, "Junk/")
          file_1 <- str_c(j_folder, "center.txt")
          write.table(s_center_o_t, file_1, quote=T, col.names=F, row.names=F)
          s_center_o_t <- read.table(file_1, sep = "")
          write.table(s_center_o_t, file_1, quote=F, col.names=F, row.names=F, sep = "")
          s_center_o_t <- readChar(file_1, nchars = 1e6)
    
          
          write.table(str_c(s_table_top, top_j_o_t, s_center_o_t, s_bot),
                      str_c(s_table_folder, "Job Summary Statistics ", save[k],
                            " Out + Trad.tex"),
                      quote=F, col.names=F, row.names=F, sep="")
        }
      }
    }
  
  write.table(str_c(table_top, siunitx, top, center, bot, "\n \\end{document}"),
              str_c(table_folder, "NLSY79 ", folder[ty] ,"/Jobs ", save[k], ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
    }
  }
}

# HO Occupation Job Characteristics --------------------------------------------

# How do HO Occupations compare to non-HO Occupations? Use matched_jobs
vars_sum <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week",
              "part_time", "tenure", "union", "job_sat", "any_benefits",
              "health", "retirement", "childcare", "dental", "flex_sched",
              "life", "maternity", "profit_share", "train_school", "n")

ho_sum <- list(matched, matched_jobs)
comp <- list(matched, matched_jobs)

for (i in 1:2) {
  ho_sum[[i]] %<>%
    as_survey_design(ids = case_id, weights = weight) %>% 
    group_by(ho_occ) %>% 
    mutate(n = n()) %>% 
    summarise_at(vars_sum, survey_mean, na.rm = T) %>% 
    arrange(desc(ho_occ))
}

observation <- c("person-job-year", "person-job")
label <- c("_year", "_job")
save <- c("Year", "Job")

# Create a Latex table
top <- str_c(table_top, siunitx, 
               "
\\begin{tabular}{lSS}
\\toprule
& {High Outsourcing} & {Others} \\\\  \\midrule
"
)

# Drop n from vars
vars <- vars_sum[-length(vars_sum)]

# Proportion varables
vars_p <- c("part_time", "union", "any_benefits", "health", "retirement", "childcare",
            "dental", "flex_sched", "life", "maternity", "profit_share", "train_school")

for (ob in 1:2) { 
  
  cond <- comp[[ob]]$ho_occ == 1
  cond_y <- comp[[ob]]$ho_occ == 0
  
  center <- rbind(
    "Log Real", "Hourly Wage", "Log Real", "Weekly Wage", "Hours Worked", 
    "Weekly", "Part Time", "", "Tenure", "(Weeks)", "Union", "",
    "Job Satisfaction", "(Lower Better)", "Any Benefits", "", "Health",
    "Insurance", "Retirement", "Plan", "Subsidized", "Childcare", "Dental",
    "Insurance", "Flex", "Schedule", "Life", "Insurance", "Maternity",
    "Leave", "Profit", "Sharing", "Training", "", "Observations")
  
  for (i in 1:2){
    col_i <- c()
    for (var in vars){
      se <- str_c(var, "_se")
      mode <- if (var %in% vars_p) "prop" else "mean"
      stars <- if (i > 1) test(
        comp[[ob]], var, "weight", mode, cond, cond_y, "ho_occ"
      ) else ""
      col_i %<>% rbind(
        format_val(ho_sum[[ob]][[var]][i], star = stars),
        format_se(ho_sum[[ob]][[se]][i])
      )
    }
    col_i %<>% rbind(format_n(ho_sum[[ob]]$n[i]))
    center %<>% cbind(col_i)
  }
  
  center %<>% cbind(
    rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",  "\\\\"))
  
  # Do weird stuff to create LaTeX output
  j_folder <- str_c(table_folder, "Junk/")
  file_1 <- str_c(j_folder, "center.txt")
  write.table(center, file_1, quote=T, col.names=F, row.names=F)
  center <- read.table(file_1, sep = "")
  write.table(center, file_1, quote=F, col.names=F, row.names=F, sep = "")
  center <- readChar(file_1, nchars = 1e6)
  
  bot <- str_c(
      "\\bottomrule
\\end{tabular}
}
\\caption{Summary statistics of jobs divided by high outsourcing ($\\geq$ 4.4\\%)
vs other occupations.
Observations are at the ", observation[ob], " level and 
statistics are weighted at the person level. 
Stars represent significant difference from outsourced jobs at the .10 level *, 
.05 level **, and .01 level ***.}
\\label{jobs", label[ob], "}
\\end{table}
\\end{document}"
    )
  
  write.table(str_c(top, center, bot),
              str_c(table_folder, "NLSY79 Jobs/Jobs ", save[ob],
                    " HO Occupation vs Not.tex"),
              quote=F, col.names=F, row.names=F, sep="")
}


# Summarize Union NA ------------------------------------------------------

# Note: always run this code after previous

# # Many people are missing union. Do these people/jobs look any different
# union_type <- c(split_data(matched), split_data(matched_jobs))
# for (i in 1:4) {
#   union_type[[i]] %<>%
#     filter(!is.na(union_fill)) %>% 
#     as_survey_design(ids = case_id, weights = weight) %>% 
#     group_by(union_fill, outsourced) %>% 
#     mutate(n = n()) %>% 
#     summarise_at(vars_sum, survey_mean, na.rm = T) %>% 
#     arrange(desc(union_fill))
# }


# Within Job Wage Distribution --------------------------------------------

# Does the within job wage distribution (wage - starting wage) look
# different for outsourced vs traditional (for jobs with >=2 obs)

multi_year <- matched %>% 
  filter(!is.na(log_real_wkly_wage), !is.na(log_real_hrly_wage)) %>% 
  group_by(case_id, emp_id) %>% 
  mutate(
    ever_ho_occ = max(ever_ho_occ),
    rank = rank(int_year),
    lrww_min_start = log_real_wkly_wage - log_real_wkly_wage[which(rank == 1)]
    ) %>% 
  filter(rank > 1, rank <= 7, lrww_min_start > -2.5, lrww_min_start < 2.5)

temp <- multi_year %>% 
  filter((outsourced == 1 | traditional == 1)) %>% 
  ggplot() +
  geom_density(aes(lrww_min_start, fill = factor(outsourced)), alpha = 0.2) +
  labs(x = "Log Real Weekly Wages", y = "Density") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                    values = c("blue", "red"),
                    labels = c("Traditional", "Outsourced")) +
  theme_light(base_size = 16) + 
  facet_wrap(~ rank)
  
ggsave(str_c(figure_folder, "LRW Wage Compared to First Observation.pdf"),
       height = height, width = width)

temp <- multi_year %>% 
  filter(ever_ho_occ == 1, (outsourced == 1 | traditional == 1)) %>% 
  ggplot() +
  geom_density(aes(lrww_min_start, fill = factor(outsourced)), alpha = 0.2) +
  labs(x = "Log Real Weekly Wages", y = "Density") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                    values = c("blue", "red"),
                    labels = c("Traditional", "Outsourced")) +
  theme_light(base_size = 16) + 
  facet_wrap(~ rank)

ggsave(str_c(figure_folder, "LRW Wage Compared to First Observation Ever HO.pdf"),
       height = height, width = width)

# Dube and Kaplan ---------------------------------------------------------

# Compare outsourced vs not for janitors and security guards based on
# self-reported outsourced and Dube and Kaplan's measure (outsourced_2), 
# which uses those in ceratin industries as outsourced. 
# See if they are significantly different

janitor <- matched_jobs %>%
  filter(occ == 4220) %>%
  mutate(
    # outsourced_a = 1 - traditional - on_call,
    outsourced_2 = 1 * (ind == 7690),
    out_1_v_2 = outsourced - outsourced_2
  ) %>% 
  filter(!is.na(outsourced_2)) 

sg <- matched_jobs %>%
  filter(occ == 3920) %>%
  mutate(
    # outsourced_a = 1 - traditional - on_call,
    outsourced_2 = 1 * (ind == 7680),
    out_1_v_2 = outsourced - outsourced_2
  ) %>% 
  filter(!is.na(outsourced_2)) 

vars_sum_js <- c("log_real_hrly_wage", "log_real_wkly_wage", 
                 "hours_week", "part_time",
                 "any_benefits", "health", "union", "job_sat",
                 "less_hs", "hs", "aa", "ba", "plus_ba",
                 "black", "hispanic", "age", "n")

sum_js <- list(janitor, janitor, sg, sg)
comp_js <- list(janitor, sg)

for (i in 1:4) {
  out<- if (i %% 2 == 1) "outsourced" else "outsourced_2"
  
  sum_js[[i]] %<>%
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(out) %>%
    mutate(n = n()) %>% 
    summarise_at(vars_sum_js, survey_mean, na.rm = T) %>% 
    arrange_at(desc(out))
}

top_js <- "\\begin{tabular}{lSSSS}
\\toprule
& \\multicolumn{2}{c}{{Self-Reported (This Paper)}} & 
\\multicolumn{2}{c}{{Industry-Occupation (Dube and Kaplan)}} \\\\
Variable & {Outsourced} & {Not Outsourced} & {Outsourced} & {Not Outsourced} \\\\ \\midrule
"

# Slides need a slightly tighter top
s_top <- "\\begin{tabular}{lSSSS}
\\toprule
& \\multicolumn{2}{c}{{Self-Reported (This Paper)}} & 
\\multicolumn{2}{c}{{Ind-Occ (Dube and Kaplan)}} \\\\
Variable & {Outsourced} & {Not Outsourced} & {Outsourced} & {Not Outsourced} \\\\ \\midrule
"

# Create table in Latex
vars_js <- vars_sum_js[-length(vars_sum_js)]

vars_js_p <- c("part_time", "black", "hispanic", "less_hs", "hs", "aa", "ba", "plus_ba", 
               "any_benefits", "health", "union")

# vars_js_m <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "job_sat", "age")

desc_js <- c("janitors (occupation 4220)", "security guards (occupation 3920)")
label_js <- c("janitor", "sg")
occ_js <- c("Janitors", "Security Guards")
ind_js <- c("services to buildings and dwellings (industry 7690)", 
            "protective services (industry 7680)") 

for (js in 1:2){
  
  center_js <- rbind("Log Real", "Hourly Wage", "Log Real", "Weekly Wage",
                     "Hours Worked", "per Week", "Part Time", "",
                     "Any Benefits", "", "Health Insurance", "", 
                     "Union", "", "Job Satisfaction", "(Lower Better)", "No HS Diploma",
                     "", "HS Diploma", "","AA Degree", "", "BA Degree", "",
                     "Post Graduate", "Degree", "Black", "",
                     "Hispanic", "", "Age", "", "Observations")
  
  comp <- comp_js[[js]]
  
  for (def in 1:2) {
    
    j <- 2 * (js - 1) + def
    
    for (i in 1:2) {
      r <- i + 2 * (def - 1)
      # Different comparison groups
      if (r == 2){
        divider <- "outsourced"
        cond <- comp$outsourced == 1
        cond_y <- comp$outsourced == 0
      } else if (r == 4) {
        divider <- "outsourced_2"
        cond <- comp$outsourced_2 == 1
        cond_y <- comp$outsourced_2 == 0
      } else {
        cond <- T
        cond_y <- T
      }
      # This is not true for our Xi test (because there is overlap)
      # else if (r == 3) {
        # divider <- "outsourced"
        # cond <- comp_r$outsourced == 1
        # cond_y <- comp_r$outsourced_2 == 1
        # comp <- comp_r[cond | cond_y, ]
        # # Need to make these the same size as comp
        # cond <- comp$outsourced_a == 1
        # cond_y <- comp$outsourced_2 == 1
      # } 
      
      col_i <- c()
      for (var in vars_js){
        se <- str_c(var, "_se")
        mode <- if (var %in% vars_js_p) "prop" else "mean"
        stars <- if (i > 1) test(
          comp, var, "weight", mode, cond, cond_y, divider
        ) else ""
        col_i %<>% rbind(
          format_val(sum_js[[j]][[var]][i], star = stars),
          format_se(sum_js[[j]][[se]][i])
        )
      }
      col_i %<>% rbind(format_n(sum_js[[j]]$n[i]))
      center_js %<>% cbind(col_i)
    }
  }
  
  center_js %<>% cbind(
    rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", 
          "\\\\", "\\\\[2pt]", "\\\\")
    )
  
  # Save only some variables for Slides
  s_center <- center_js[c(3:4, 7:10, 17:20, 27:30, 33),]
  
  # Do weird stuff to create LaTeX output
  j_folder <- str_c(table_folder, "Junk/")
  file_1 <- str_c(j_folder, "center.txt")
  write.table(center_js, file_1, quote=T, col.names=F, row.names=F)
  center_js <- read.table(file_1, sep = "")
  write.table(center_js, file_1, quote=F, col.names=F, row.names=F, sep = "")
  center_js <- readChar(file_1, nchars = 1e6)
  
  bot_js <- str_c(
    "\\bottomrule
\\end{tabular}
}
\\caption{Summary statistics for ", desc_js[js], " who are outsourced vs
not outsourced in the NLSY. In the left two columns, outsourced is self-reported 
by the worker as in the rest of this paper. In the right two, it is inferred if the worker 
is in ", 
    ind_js[js], 
" following \\citet{dubekaplan2010}. Observations are at the
person-job level and summary statistics are weighted at the person level. 
Stars represent significant difference from outsourced of same determination method
at the .10 level *, .05 level **, and .01 level ***.}
\\label{", label_js[js], "}
\\end{table}"
  )
  
  write.table(str_c(table_top, siunitx, top_js, center_js, bot_js, "\n \\end{document}"),
              str_c(table_folder, "NLSY79 Dube Kaplan/", occ_js[js], " Summary.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save a version to Drafts
  write.table(str_c(s_table_top, top_js, center_js, bot_js),
              str_c(d_table_folder, "Dube Kaplan ", occ_js[js], " Summary.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save a version to Slides
  # Do weird stuff to create LaTeX output
  j_folder <- str_c(table_folder, "Junk/")
  file_1 <- str_c(j_folder, "center.txt")
  write.table(s_center, file_1, quote=T, col.names=F, row.names=F)
  s_center <- read.table(file_1, sep = "")
  write.table(s_center, file_1, quote=F, col.names=F, row.names=F, sep = "")
  s_center <- readChar(file_1, nchars = 1e6)
  
  write.table(str_c(s_table_top, s_top, s_center, s_bot),
              str_c(s_table_folder, "Dube Kaplan ", occ_js[js], " Summary.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
}

# What are the self-reported job types of DK's outsourced?
# Use data.frame's SQL like capabilities to make these tables
top <- "\\begin{tabular}{lrr|r}
\\toprule
& \\multicolumn{3}{c} {Industry-Occupation (Dube and Kaplan)} \\\\
Self-Reported & Outsourced & Not Outsourced & Total \\\\ \\midrule
"

occ_js <- c("janitors (occupation 4220)", "security guards (occupation 3920)")
ind_js <- c("services to buildings and dwellings (industry 7690)", 
            "protective services (industry 7680)")
label <- c("janitors", "sg")
save <- c("Janitors", "Security Guards")

types <- c("outsourced", "indep_con", "temp_work", "on_call", "self_emp", "traditional")

dt_js <- list(data.table(janitor), data.table(sg))

for (js in 1:2){
  
  center <- c("Contracted Out", "Independent Contractor", "Temp Worker",
                "On-Call Worker", "Self-Employed", "Traditional Employee", "Total" )
  
  col_i <- c()
  
  for (type in types){
    # Some observations may not occur, set to 0
    a <- tryCatch(dt_js[[js]][outsourced_2 == 1 & dt_js[[js]][[type]] == 1, .N], 
                  error = function(e) {a <- 0})
    b <- tryCatch(dt_js[[js]][outsourced_2 == 0 & dt_js[[js]][[type]] == 1, .N],
                  error = function(e) {b <- 0})
    b <- b * !is.na(b) + 0  
    col_i %<>% rbind(cbind(str_c(" & ", a),
                           str_c(" & ", b),
                           str_c(" & ", a + b)))
  }
  
  col_i %<>% rbind(
    cbind(str_c(" & ", dt_js[[js]][outsourced_2 == 1, .N]),
          str_c(" & ", dt_js[[js]][outsourced_2 == 0, .N]),
          str_c(" & ", NROW(dt_js[[js]]))))
  
  center %<>% cbind(col_i, 
                    rbind("\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]",
                          "\\\\[2pt]", "\\\\[2pt] \\midrule", "\\\\")) 
  
  # Do weird stuff to create LaTeX output
  j_folder <- str_c(table_folder, "Junk/")
  file_1 <- str_c(j_folder, "center.txt")
  write.table(center, file_1, quote=T, col.names=F, row.names=F)
  center <- read.table(file_1, sep = "")
  write.table(center, file_1, quote=F, col.names=F, row.names=F, sep = "")
  center <- readChar(file_1, nchars = 1e6)
  
  bot <- str_c(
    "\\bottomrule
\\end{tabular}
}
\\caption{Counts of \\citet{dubekaplan2010} (DK) method of measuring outsourcing versus
NLSY self-reported job type for ", occ_js[js], " in the NLSY. 
For columns, following DK, workers are consider outsourced if they are in ",
    ind_js[js], ". 
For rows, I show the worker's self-reported job type. Observations are at the
person-job level.}
\\label{dk_types_", label[js], "}
\\end{table}"
  )
  
  write.table(str_c(table_top, top, center, bot, "\n \\end{document}"),
              str_c(table_folder, "NLSY79 Dube Kaplan/", save[js], " Types.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save in Drafts
  write.table(str_c(d_table_top, top, center, bot),
              str_c(d_table_folder, "Dube Kaplan ", save[js], " Outsourced.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save in Slides
  write.table(str_c(s_table_top, top, center, s_bot),
              str_c(s_table_folder, "Dube Kaplan ", save[js], " Outsourced.tex"),
              quote=F, col.names=F, row.names=F, sep="")
}

# Some jobs were preassigned traditional (pre-trad). How many are there of each type
# and how many of these are outsourced according to DK?
table <- str_c(
"\\begin{tabular}{lcc}
\\toprule
Variable & Janitors & Security Guards \\\\ \\midrule
Traditional", format_val(sum(janitor$traditional), s = 0),
format_val(sum(sg$traditional), s = 0), "\\\\ \n",
"Traditional Outsourced (DK)", format_val(sum(janitor$outsourced_2), s = 0), 
format_val(sum(sg$outsourced_2), s = 0), "\\\\ \n",
"Pre-assigned Traditional", format_val(sum(janitor$pre_trad), s = 0), 
format_val(sum(sg$pre_trad), s = 0), "\\\\ \n",
"Pre-assigned Traditional Outsourced (DK)", 
format_val(sum(janitor$outsourced_2[janitor$pre_trad > 0]), s = 0), 
format_val(sum(sg$outsourced_2[sg$pre_trad > 0]), s = 0), 
"\\\\
\\bottomrule
\\end{tabular}
}
\\caption{For janitors and security guards who were in traditional jobs,
how many were outsourced accoring to \\citet{dubekaplan2010} (DK), how
many were pre-assigned traditional, and how many pre-assigned jobs 
were outsourced accoring to DK.}
\\label{dk_pre_trad}
\\end{table}"
)

write.table(str_c(table_top, table, "\n \\end{document}"),
            str_c(table_folder, "NLSY79 Dube Kaplan/Traditional Split.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# PBS Industries by Type --------------------------------------------------

# Break down workers by type again, but do this for both PBS and non-PBS industries
m_j_dt <- data.table(matched_jobs)

top <- "\\begin{tabular}{lrr|r}
\\toprule
Self-Reported & PBS & Not PBS & Total \\\\ \\midrule
"

types <- c("outsourced", "indep_con", "temp_work", "on_call",
           "self_emp", "traditional")

center <- c("Contracted Out", "Independent Contractor", "Temp Worker",
            "On-Call Worker", "Self-Employed", "Traditional Employee", "Total" )

col_i <- c()

for (type in types){
  # Some observations may not occur, set to 0
  a <- tryCatch(m_j_dt[pbs == 1 & m_j_dt[[type]] == 1, .N], 
                error = function(e) {a <- 0})
  b <- tryCatch(m_j_dt[pbs == 0 & m_j_dt[[type]] == 1, .N],
                error = function(e) {b <- 0})
  b <- b * !is.na(b) + 0  
  col_i %<>% rbind(cbind(str_c(" & ", a),
                         str_c(" & ", b),
                         str_c(" & ", a + b)))
}

col_i %<>% rbind(
  cbind(str_c(" & ", m_j_dt[pbs == 1, .N]),
        str_c(" & ", m_j_dt[pbs == 0, .N]),
        str_c(" & ", NROW(m_j_dt))))

center %<>% cbind(col_i, 
                  rbind("\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]",
                        "\\\\[2pt]", "\\\\[2pt] \\midrule", "\\\\"))  

# Do weird stuff to create LaTeX output
j_folder <- str_c(table_folder, "Junk/")
file_1 <- str_c(j_folder, "center.txt")
write.table(center, file_1, quote=T, col.names=F, row.names=F)
center <- read.table(file_1, sep = "")
write.table(center, file_1, quote=F, col.names=F, row.names=F, sep = "")
center <- readChar(file_1, nchars = 1e6)

bot <- str_c(
  "\\bottomrule
\\end{tabular}
}
\\caption{Job types of workers in Professional Business Service (PBS) industries versus
all other industries in the NLSY. PBS Industries have Census 2000 Industry Codes between 
7270 and 7790.}
\\label{pbs_types}
\\end{table}"
)

write.table(str_c(table_top, top, center, bot, "\n \\end{document}"),
            str_c(table_folder, "NLSY79 Job Types/PBS Types.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save in Drafts
write.table(str_c(d_table_top, top, center, bot),
            str_c(d_table_folder, "PBS Types.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save in Slides
write.table(str_c(s_table_top, top, center, s_bot),
            str_c(s_table_folder, "PBS Types.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# Before next sections, remove some larger files to free memory.
rm("dt_js", "janitor", "job_type_sum", "life_jobs", "sg", "sum_demo",
   "sum_jobs", "sum_js", "matched_jobs_dt")

# Regressions -------------------------------------------------------------

# Create a loop that takes various desired outcomes, runs series of regressions,
# and creates table in Latex
# 1. Basic Controls (age:age quartic, black, hispanic, education,
# union_fill, region, msa/cc, marital status, children, hours week / part time, year)
# 2. Add tenure quartic and occ factors
# 3. Individual FE + some controls (age quartic, union_fill, region, msa/cc,
# marital stutus, children, hours week / part time, year)
# 4. Add tenure quartic
# 5. Add occ factors
# 6. Add 4+5

# Also create a table for Drafts and Slides with just 5 and 6 for some outcomes
var_r <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "part_time",
           "job_sat", "any_benefits", "health")

var_names <- c("log real hourly wages", "log real weekly wages",
               "hours worked per week", 
               "part-time status ($<$35 hours per week)", 
               "job satisfaction (lower is better)",
               "receiving any employment benefits",
               "receiving health insurance through employer")

save_names <- c("LRH Wages", "LRW Wages",  "Hours Week", "Part Time",
                "Job Satisfaction", "Benefits", "H Insurance")

types <- c("outsourced", "self_emp", "indep_con", "temp_work", "on_call")

controls <- c("age", "I((age)^2)", "I((age)^3)", "I((age)^4)",
              "tot_child", "hh_child", "factor(union_fill)")

hours <- c("hours_week", "part_time")

ols_controls <- c("black", "hispanic", "hs", "aa", "ba", "plus_ba")

tenure <- c("I((tenure/100))", "I((tenure/100)^2)", "I((tenure/100)^3)",
            "I((tenure/100)^4)")

fixed_effects <- c("region", "marital_status", "msa")

labels <- c("lrhw", "lrww", "hours", "pt", "job_sat", "benefits", "health")

dfs <- list(matched, matched_jobs)
samples <- c("person-job-year", "person-job")
s_labels <- c("_year", "_job")
s_saves <- c("Years", "Jobs")
s_explanations <- c(
  "", ", where jobs observed more than once use average or modal characteristics"
)

top <- str_c(table_top, siunitx, 
             "
\\begin{tabular}{lSSSSSS}
\\toprule
& \\multicolumn{2}{c}{{OLS}} & \\multicolumn{4}{c}{{Worker Fixed Effects}} \\\\
& {Basic} & {Full}  & {Basic}  & {Tenure} & {Occ FE} & {Full}  \\\\\\midrule
"
)

center_d <- rbind("Log Real", "Hourly Wages", "Log Real", "Weekly Wages",
                  "Hours Worked", "Per Week", "Part-Time", "",
                  "Job Satisfaction", "(Lower Better)", 
                  "Any Benefits", "", "Health", "Insurance")

top_d <- "\\begin{tabular}{lSSS}
\\toprule
Outcome & {Outsourced} & {$R^2$}  & {Observations} \\\\\\midrule \n"

for (loop in 1:2) {
  
  sample <- samples[loop]
  s_label <- s_labels[loop]
  s_save <- s_saves[loop]
  s_explanation <- s_explanations[loop]
  y <- if (loop == 1) "dummies for year" else "dummies for year started and ended job"
  
  # Reset Draft Table Centers
  c_5 <- c()
  c_6 <- c()
  
  for (ind in seq_along(var_r)) {
    
    center <- rbind("Outsourced", "", "Worker FE", "Tenure Quartic", "Occupation FE",
                    "$R^2$", "Observations")
    
    var <- var_r[ind]
    var_name <- var_names[ind]
    label <- labels[ind]
    save <- str_c(" ", save_names[ind])
    
    for (reg_ind in 1:6){
      
      ind_vars <- c(types, controls)
      if (loop == 1) {
        fe_vars <- c(fixed_effects, "int_year")
      } else {
        fe_vars <- c(fixed_effects, "I(year(week_start_job))", "I(year(week_end_job))")
      }
      
      hours_text <- "" 
      if (!(var %in% c("log_real_wkly_wage", "health", "any_benefits",
                       "hours_week", "part_time"))){
        ind_vars %<>% c(hours)
        hours_text <- " hours worked per week, part-time status,"
      }
      
      if (reg_ind <= 2){
        ife_ind <- "{No}"
        ind_vars %<>% c(ols_controls)
      } else{
        ife_ind <- "{Yes}"
        fe_vars %<>% c("case_id")
      }
      
      if (reg_ind %in% c(2, 4, 6)){
        ten_ind <- "{Yes}"
        ind_vars %<>% c(tenure)
      } else{
        ten_ind <- "{No}"
      }
      
      if (reg_ind %in% c(2, 5, 6)){
        occ_ind <- "{Yes}"
        fe_vars %<>% c("occ")
      } else{
        occ_ind <- "{No}"
      }
      
      eq <- create_formula(var, ind_vars)
      fe <- create_formula("", fe_vars)
      
      # Run regression both for matched and matched_jobs
      temp <- lm_robust(eq, data = dfs[[loop]], weights = weight,
                        subset = !is.na(region) & !is.na(marital_status)
                        & !is.na(msa) & !is.na(occ),
                        fixed_effects = !!fe,
                        clusters = as_factor(sample_id),
                        se_type = "stata", try_cholesky = T)
      
      stars <- p_stars(temp$p.value["outsourced"])
      
      center %<>% cbind(
        rbind(format_val(temp$coefficients["outsourced"], r=3, s=3, star = stars),
              format_se(temp$std.error["outsourced"], r=3, s=3),
              str_c(" & ", ife_ind),
              str_c(" & ", ten_ind),
              str_c(" & ", occ_ind),
              format_val(temp$r.squared),
              format_n(lm_N(temp))
        )
      )
      
      if (reg_ind == 5) {
        c_5 %<>% rbind(
          cbind(format_val(temp$coefficients["outsourced"], r=3, s=3, star = stars),
                format_val(temp$r.squared), format_n(lm_N(temp)), "\\\\"),
          cbind(format_se(temp$std.error["outsourced"], r=3, s=3),
                " & ", " & ", "\\\\[2pt]")
        )
      }
      
      if (reg_ind == 6) {
        c_6 %<>% rbind(
          cbind(format_val(temp$coefficients["outsourced"], r=3, s=3, star = stars),
                format_val(temp$r.squared), format_n(lm_N(temp)), "\\\\"),
          cbind(format_se(temp$std.error["outsourced"], r=3, s=3),
                " & ", " & ", "\\\\[2pt]")
        )
      }
    }
    
    center %<>% cbind(
      rbind("\\\\", "\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]",
            "\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]"))
    
    
    # Do weird stuff to create LaTeX output
    r_folder <- str_c(table_folder, "Junk/")
    file_1 <- str_c(r_folder, "center.txt")
    write.table(center, file_1, quote=T, col.names=F, row.names=F)
    center <- read.table(file_1, sep = "")
    write.table(center, file_1, quote=F, col.names=F, row.names=F, sep = "")
    center <- readChar(file_1, nchars = 1e6)
    
    bot <- str_c(
      "\\bottomrule
      \\end{tabular}
      }
      \\caption{Regressions of outsourced on ", var_name, " in the NLSY. All regressions
  include controls for job type (traditional job is default), 
      a quartic in age, ", y, ", union status,", hours_text,
      " dummies for region, whether in an MSA or central city,
  marital status, and number of children total and in household. 
  The first two columns run OLS and also contain controls for
  race and education. The last four columns use worker fixed effects.
  All observations are at the ", sample, " level and all standard errors are
  clustered by demographic sample.
  Stars represent significant at the .10 level *, .05 level **, and .01 level ***.}
  \\label{reg_" , label, s_label, "}
  \\end{table}
  \\end{document}"
    )
    
    write.table(str_c(top, center, bot),
                str_c(table_folder, "NLSY79 Regressions/", s_save, save, ".tex"),
                quote=F, col.names=F, row.names=F, sep="")
    
  }
  
  # Save Draft Tables
  center_5 <- cbind(center_d, c_5)
  center_6 <- cbind(center_d, c_6)
  
  # Do weird stuff to create LaTeX output
  r_folder <- str_c(table_folder, "Junk/")
  file_1 <- str_c(r_folder, "center.txt")
  write.table(center_5, file_1, quote=T, col.names=F, row.names=F)
  center_5 <- read.table(file_1, sep = "")
  write.table(center_5, file_1, quote=F, col.names=F, row.names=F, sep = "")
  center_5 <- readChar(file_1, nchars = 1e6)
  
  bot_5 <- str_c("\\bottomrule
     \\end{tabular}
     }
     \\caption{Regressions of worker outsourcing status on job outcomes in the NLSY.
     All regressions include controls for job type (traditional job is default), 
     worker and occupation fixed effects, a quartic in age, ", y, ", union status,
     dummies for region, whether in an MSA or central city,
     marital status, and number of children total and in household. 
     Regressions for log real hourly wages and job satisfaction also
     include controls for hours worked per week and part-time status.
  All observations are at the ", sample, " level,", s_explanation,
    ". All regressions are weighted at the person level and all standard errors are
  clustered by demographic sample.
  Stars represent significant at the .10 level *, .05 level **, and .01 level ***.}
  \\label{regs_no_tenure", s_label, "}
  \\end{table}")
  
  write.table(str_c(d_table_top, top_d, center_5, bot_5),
              str_c(d_table_folder, "Job Regressions ", s_save, " No Tenure.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Also save to Slides
  write.table(str_c(d_table_top, "\n \\footnotesize \n", top_d, center_5, s_bot),
              str_c(s_table_folder, "Job Regressions ", s_save, " No Tenure.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  file_1 <- str_c(r_folder, "center.txt")
  write.table(center_6, file_1, quote=T, col.names=F, row.names=F)
  center_6 <- read.table(file_1, sep = "")
  write.table(center_6, file_1, quote=F, col.names=F, row.names=F, sep = "")
  center_6 <- readChar(file_1, nchars = 1e6)
  
  bot_6 <- str_c("\\bottomrule
     \\end{tabular}
     }
     \\caption{Regressions of worker outsourcing status on job outcomes in the NLSY.
     All regressions include controls for job type (traditional job is default), 
     worker and occupation fixed effects, a quartic in age and job tenure, ", y, ",
     union status, dummies for region, whether in an MSA or central city,
     marital status, and number of children total and in household. 
     Regressions for log real hourly wages and job satisfaction also
     include controls for hours worked per week and part-time status.
  All observations are at the ", sample, " level", s_explanation,
    ". All regressions are weighted at the person level and all standard errors are
  clustered by demographic sample.
  Stars represent significant at the .10 level *, .05 level **, and .01 level ***.}
  \\label{regs", s_label, "}
  \\end{table}"  )
  
  write.table(str_c(d_table_top, top_d, center_6, bot_6),
              str_c(d_table_folder, "Job Regressions ", s_save, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Also save to Slides
  write.table(str_c(d_table_top, "\n \\footnotesize \n",  top_d, center_6, s_bot),
              str_c(s_table_folder, "Job Regressions ", s_save, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
}

# How much does type matter? Run full regression but add types in groups
# 1. Just outsourced vs not
# 2. Separate self_emp and indep_con
# 3. Separate on_call and temp_work
keep_var <- function(model, var, keep) {
  if (keep == F) {
    c(" & {--}", " & ")
  } else {
    stars <- p_stars(model$p.value[var])
    val <- format_val(model$coefficients[var], r=3, s=3, star = stars)
    se <- format_se(model$std.error[var], r=3, s=3)
    c(val, se)
  }
}

top <- "\\begin{tabular}{lSSS}
\\toprule
& {Outsourced} & {Self-Employed} & {Full} \\\\\\midrule
"

for (loop in 1:2) {
  
  sample <- samples[loop]
  s_label <- s_labels[loop]
  s_save <- s_saves[loop]
  s_explanation <- s_explanations[loop]
  y <- if (loop == 1) " and year" else "and year started and ended job"
  
  for (ind in seq_along(var_r)) {
    
    center <- rbind("Outsourced", "", "Self-Employed", "", "Independent", "Contractor",
                    "On-Call", "", "Temp Worker", "", "$R^2$", "Observations")
    
    var <- var_r[ind]
    var_name <- var_names[ind]
    label <- labels[ind]
    save <- str_c(" ", save_names[ind])
    
    for (reg_ind in c(1:3)){
      
      ind_vars <- c(controls, tenure)
      fe_vars <- c(fixed_effects, "case_id", "occ")
      if (loop == 1) {
        fe_vars <- c(fe_vars, "int_year")
      } else {
        fe_vars <- c(fe_vars, "I(year(week_start_job))", "I(year(week_end_job))")
      }
      
      hours_text <- "" 
      if (!(var %in% c("log_real_wkly_wage", "health", "hours_week", "part_time",
                       "any_benefits"))){
        ind_vars %<>% c(hours)
        hours_text <- " hours worked per week, part-time status,"
      }
      
      types <- c("outsourced")
      types <- if (reg_ind >= 2) c(types, "self_emp", "indep_con") else types
      types <- if (reg_ind == 3) c(types, "on_call", "temp_work") else types
      
      ind_vars %<>% c(types)
      
      eq <- create_formula(var, ind_vars)
      fe <- create_formula("", fe_vars)
      
      # Run regression both for matched and matched_jobs
      temp <- lm_robust(eq, data = dfs[[loop]], weights = weight,
                        subset = !is.na(region) & !is.na(marital_status)
                        & !is.na(msa) & !is.na(occ),
                        fixed_effects = !!fe,
                        clusters = as_factor(sample_id),
                        se_type = "stata", try_cholesky = T)
      
      keep_se <- if (reg_ind >= 2) T else F
      keep_oc <- if (reg_ind == 3) T else F
      
      ou <- keep_var(temp, "outsourced", T)
      se <- keep_var(temp, "self_emp", keep_se)
      ic <- keep_var(temp, "indep_con", keep_se)
      oc <- keep_var(temp, "on_call", keep_oc)
      tw <- keep_var(temp, "temp_work", keep_oc)
      
      center %<>% cbind(
        rbind(ou[1], ou[2],
              se[1], se[2],
              ic[1], ic[2],
              oc[1], oc[2],
              tw[1], tw[2],
              format_val(temp$r.squared),
              format_n(lm_N(temp))
        )
      )
    }
    
    center %<>% cbind(
      rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]"))
    
    # Do weird stuff to create LaTeX output
    r_folder <- str_c(table_folder, "Junk/")
    file_1 <- str_c(r_folder, "center.txt")
    write.table(center, file_1, quote=T, col.names=F, row.names=F)
    center <- read.table(file_1, sep = "")
    write.table(center, file_1, quote=F, col.names=F, row.names=F, sep = "")
    center <- readChar(file_1, nchars = 1e6)
    
    bot <- str_c(
      "\\bottomrule
      \\end{tabular}
      }
      \\caption{Regressions of job type on ", var_name, " in the NLSY.
      Missing type in final row is traditional jobs. 
      All regressions use worker and occupation fixed effects and 
      include a quartic in age and job tenure", y, ", union status,", hours_text, 
      " dummies for region, whether in an MSA or central city,
      marital status, and number of children in household and total.
      All observations are at the ", sample, " level", s_explanation, 
      ". All regressions are weighted at the person level and all standard errors are
      clustered by demographic sample.
      Stars represent significant at the .10 level *, .05 level **, and .01 level ***.}
      \\label{type_reg_" , label, s_label, "}
      \\end{table}"
    )
    
    write.table(str_c(table_top, siunitx, top, center, bot, "\n \\end{document}"),
                str_c(table_folder, "NLSY79 Regressions/", s_save, save, " Types.tex"),
                quote=F, col.names=F, row.names=F, sep="")
    
    # If looking at log real weekly wages, save in Drafts and Slides
    if (ind == 2) {
      write.table(str_c(d_table_top, top, center, bot),
                  str_c(d_table_folder, "Job Regressions LRW Wages ",
                        s_save, " Types.tex"),
                  quote=F, col.names=F, row.names=F, sep="")
      
      # Table to long for slide, set size manually
      write.table(str_c(d_table_top, "\n \\footnotesize \n", top, center, s_bot),
                  str_c(s_table_folder, "Job Regressions LRW Wages ",
                        s_save, " Types.tex"),
                  quote=F, col.names=F, row.names=F, sep="")
    }
    
    # Code sometimes very slow here. Maybe deleting some files will help
    rm("temp", "eq", "fe", "center", "ou", "se", "ic", "oc", "tw")
    
  }
}


# Plots -------------------------------------------------------------------

# Use ggplot2 to graph some things
# Graph some variables by year for outsourced vs traditional
# Both all workers and ever_ho_occ

# Also plot log_real_hrly/wkly_wage residuals 
# from full FE regression about (but without outsourcing)
# See how these compare for outsourced vs traditional

# Create filter_ever which filters by ever_ho_occ if condition is T
filter_ever <- function(df, condition) {
  if (condition) {
    filter(df, ever_ho_occ == 1)
  } else {
    df
  }
}

var_g <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "tenure")

var_names <- c("Log Real Hourly Wage", "Log Real Weekly Wage", 
               "Hours Worked per Week", "Weeks of Tenure")

var_saves <- c("LRH Wage", "LRW Wage", "Hours", "Tenure")

types <- c("self_emp", "indep_con", "temp_work", "on_call")

controls <- c("age", "I((age)^2)", "I((age)^3)", "I((age)^4)",
              "tot_child", "hh_child", "factor(union_fill)")

hours <- c("hours_week", "part_time")

tenure <- c("I((tenure/100))", "I((tenure/100)^2)", "I((tenure/100)^3)",
            "I((tenure/100)^4)")

fixed_effects <- c("region", "marital_status", "msa", "occ", "case_id")

dfs <- c(split_data(matched), split_data(matched_jobs))
s_saves <- c("Years ", "Jobs ")
ho_saves <- c("", " Ever HO Occupation")

c_i <- c()

for (ob in 1:2) {
  
  c_i %<>% rbind(
    cbind(" & ", " & ", " & ", " & ", " & ", " & ", " & ", " & ")
  )
  
  for (i in seq_along(var_g)) {
    
    # Will plot residuals from regressions below for wages (1 and 2). 
    # Run regressions with full data. 
    if (i <= 2) {
      
      ind_vars <- c(controls, tenure, types)
      
      if (var_g[i] != "log_real_wkly_wage"){
        ind_vars %<>% c(hours)
      }
      
      if (ob == 1) {
        fe_vars <- c(fixed_effects, "int_year")
      } else {
        fe_vars <- c(fixed_effects, "I(year(week_start_job))", "I(year(week_end_job))")
      }
      
      eq <- create_formula(var_g[i], ind_vars)
      fe <- create_formula("~", fe_vars)
      
      k <- if (ob == 1) 1 else 3
      reg <- lm_robust(eq, data = dfs[[k]],
                       subset = !is.na(region) & !is.na(marital_status) &
                         !is.na(msa) & !is.na(occ),
                       weights = weight,
                       fixed_effects = !!fe,
                       clusters = sample_id, 
                       se_type = "stata", try_cholesky = T)
    }
    
    for (ho in 1:2) {
      
      j <- 2 * (ob - 1) + ho 
      
      # Plot raw figures
      temp <- dfs[[j]] %>%
        filter(!is.na(.[[var_g[i]]]), (outsourced == 1 | traditional == 1)) %>%
        ggplot(aes_string(var_g[i], fill = "factor(outsourced)")) +
        geom_density(alpha = 0.2) +
        labs(x = var_names[i], y = "Density") +
        scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                          values = c("blue", "red"),
                          labels = c("Traditional", "Outsourced")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        theme_light(base_size = 16) 
      
      ggsave(str_c(figure_folder, s_saves[ob],
                   var_saves[i], ho_saves[ho], ".pdf"),
             height = height, width = width)
      
      # If looking at wages, plot residuals too. 
      # Also create a table of the standard deviation and skew of residuals
      # for both outsourced and not outsourced
      if (i <= 2) {
        df <- dfs[[k]] %>% 
          filter(!is.na(.[[var_g[i]]]), !is.na(tenure), 
                 !is.na(tot_child), !is.na(hh_child),
                 !is.na(union_fill), !is.na(region), !is.na(marital_status),
                 !is.na(msa), !is.na(occ)) %>% 
          mutate(residual = lm_residuals(reg)) %>% 
          filter_ever(ho == 2) %>% 
          filter((outsourced == 1 | traditional == 1))
        
        # Observations with unique case_ids/occupations have artificially 0 residuals
        # Keep only people/occupations with mulitple observtions
        df <- df[duplicated(df$case_id),]
        df <- df[duplicated(df$occ),]
        
        df_sum <- df %>% 
          as_survey_design(id = case_id, weight = weight) %>% 
          group_by(outsourced) %>% 
          summarise(
            variance = survey_var(residual, vartype = "ci"),
            skew = unweighted(Skew(residual, conf.level = .95, ci.type = "basic")[1]),
            skew_low = unweighted(Skew(residual, conf.level = .95, ci.type = "basic")[2]),
            skew_upp = unweighted(Skew(residual, conf.level = .95, ci.type = "basic")[3]),
            obs = unweighted(n())
          ) %>% 
          arrange(desc(outsourced))
        
        # Save these in a df for later
        c_i %<>% rbind(
          cbind(
            format_val(df_sum$variance_low[1], r = 3, s = 3),
            format_val(df_sum$variance_upp[1], r = 3, s = 3),
            format_val(df_sum$skew_low[1]),
            format_val(df_sum$skew_upp[1]),
            format_val(df_sum$variance_low[2], r = 3, s = 3),
            format_val(df_sum$variance_upp[2], r = 3, s = 3),
            format_val(df_sum$skew_low[2]),
            format_val(df_sum$skew_upp[2])
          )
        )
        
        temp <- df %>% 
          ggplot(aes(residual, fill = factor(outsourced))) +
          geom_density(alpha = 0.2) +
          labs(x = str_c("Residual ", var_names[i]), y = "Density") +
          scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                            values = c("blue", "red"),
                            labels = c("Traditional", "Outsourced")) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
          theme_light(base_size = 16) 
        
        ggsave(str_c(figure_folder, s_saves[ob], var_saves[i],
                     " Residuals", ho_saves[ho], ".pdf"),
               height = height, width = width)
          
        # If job -year ob == 2, log real wkly wage i == 2, and ho workers ho == 2
        if (ob == 2 & i == 2 & ho == 2) {
          # Update with mean lrw wage for both job types
          # To do this, take overall mean wage (for all types, including discarded ones)
          # Because this is what the regression was run on. Then take average 
          # residual of each type and add together
          # Recreate this df just to find mean wage of entire regression sample
          df_mean <- dfs[[k]] %>% 
            filter(!is.na(.[[var_g[i]]]), !is.na(tenure), 
                   !is.na(tot_child), !is.na(hh_child),
                   !is.na(union_fill), !is.na(region), !is.na(marital_status),
                   !is.na(msa), !is.na(occ))
          
          w_bar_overall <- weighted.mean(df_mean$log_real_wkly_wage,
                                         df_mean$weight,  na.rm = T)
          
          m_resid <- weighted.mean(df$residual[df$traditional == 1],
                                   df$weight[df$traditional == 1], na.rm = T)
          m_resid_o <- weighted.mean(df$residual[df$outsourced == 1],
                                     df$weight[df$outsourced == 1], na.rm = T)
          
          w_bar <- w_bar_overall + m_resid
          w_o_bar <- w_bar_overall + m_resid_o
          
          # Also find sd of resiuals for each type
          var_resid <- (
            sum(df$weight[df$traditional == 1] * 
                  (df$residual[df$traditional == 1] - m_resid)^2) /
              ((length(df$weight[df$traditional == 1] != 0) - 1) / 
                 length(df$weight[df$traditional == 1] != 0)
               * sum(df$weight[df$traditional == 1]))) 
          w_sd <- sqrt(var_resid)
          
          var_resid_o <- (
            sum(df$weight[df$outsourced == 1] * 
                  (df$residual[df$outsourced == 1] - m_resid)^2) /
              ((length(df$weight[df$outsourced == 1] != 0) - 1) / 
                 length(df$weight[df$outsourced == 1] != 0)
               * sum(df$weight[df$outsourced == 1]))) 
          w_o_sd <- sqrt(var_resid_o)
          
          data_moments <- update_parameters("w_bar", w_bar)
          data_moments <- update_parameters("w_o_bar", w_o_bar)
          data_moments <- update_parameters("w_sd", w_sd)
          data_moments <- update_parameters("w_o_sd", w_o_sd)
          
          # Plot ecdf
          temp <- df %>% 
            ggplot(aes(residual, color = factor(outsourced))) +
            stat_ecdf(geom = "line") +
            labs(x = str_c("Residual ", var_names[i]), y = "CDF") +
            scale_color_manual(name = "Job Type", breaks = c(0, 1),
                              values = c("blue", "red"),
                              labels = c("Traditional", "Outsourced")) +
            theme_light(base_size = 16) 
          
          ggsave(str_c(figure_folder, s_saves[ob], var_saves[i],
                       " Residual CDF", ho_saves[ho], ".pdf"),
                 height = height, width = width)
            
          # Save this data set, rename x as wage and add w/w_o bar to 
          # revolve around mean wage overall. Rename y as cdf, group as outsourced
          # discard rest
          ecdf <- ggplot_build(temp)$data[[1]] %>% 
            filter(x > -10, x < 10) %>% # filter out -Inf and Inf 
            rename(density = y) %>% 
            mutate(wage = w_bar_overall + x,
                   outsourced = group - 1) %>% 
            select(outsourced, wage, density)
          }
        }
      }
      
      # Also plot job_sat as a density histogram (only do this once)
      if (i == 1) {
        temp <- dfs[[j]] %>%
          filter(!is.na(job_sat), (outsourced == 1 | traditional == 1)) %>%
          ggplot(aes(round(job_sat, 0), fill = factor(outsourced))) +
          geom_histogram(aes(y=.5*..density..), alpha=0.5, position="dodge",
                         binwidth = .5) +
          labs(x = "Job Satisfaction", y = "Density") +
          scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                            values = c("blue", "red"),
                            labels = c("Traditional", "Outsourced")) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
          theme_light(base_size = 16) 
        
        ggsave(str_c(figure_folder, s_saves[ob],
                     "Job Satisfaction", ho_saves[ho], ".pdf"),
               height = height, width = width)
      }
    }
  }


write_csv(data_moments, str_c(clean_folder, "data_moments.csv"))
write_csv(ecdf, str_c(clean_folder, "data_ecdf.csv"))

# For regressions, create a table of residual's variance and skew
top <- str_c(table_top, "\\setlength{\\tabcolsep}{0.175em} \n", siunitx, 
             "
\\begin{tabular}{lSSSSSSSS}
\\toprule
& \\multicolumn{4}{c}{Outsourced} & \\multicolumn{4}{c}{{Not Outsourced}} \\\\
& \\multicolumn{2}{c}{Variance} & \\multicolumn{2}{c}{{Skew}} &
  \\multicolumn{2}{c}{Variance} & \\multicolumn{2}{c}{{Skew}} \\\\
& {Lower} & {Upper} & {Lower} & {Upper} 
& {Lower} & {Upper} & {Lower} & {Upper} \\\\\\midrule
"
)

center <- rbind("Years", "LRH Wage", "LRH Wage Ever HO Occupation",
                "LRW Wage", "LRW Wage Ever HO Occupation",
                "Jobs", "LRH Wage", "LRH Wage Ever HO Occupation",
                "LRW Wage", "LRW Wage Ever HO Occupation")

center %<>% cbind(c_i,
                  rbind("\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]",
                        "\\\\[2pt] \\midrule", "\\\\ [2pt]", "\\\\ [2pt]",
                        "\\\\ [2pt]", "\\\\ [2pt]", "\\\\ [2pt]"))

# Do weird stuff to create LaTeX output
r_folder <- str_c(table_folder, "Junk/")
file_1 <- str_c(r_folder, "center.txt")
write.table(center, file_1, quote=T, col.names=F, row.names=F)
center <- read.table(file_1, sep = "")
write.table(center, file_1, quote=F, col.names=F, row.names=F, sep = "")
center <- readChar(file_1, nchars = 1e6)

bot <- str_c(
"\\bottomrule
\\end{tabular}
}
\\caption{Confidence intervals for variance and skew for residuals of regressions
run on log real hourly and weekly wages divided by outsourced and not outsourced
workers.}
\\label{residuals}
\\end{table}
\\end{document}"
)

write.table(str_c(top, center, bot),
            str_c(table_folder, "NLSY79 Regressions/Residuals.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# Look at log real hrly wages in matched_jobs for traditional
# ho_occ vs not. Also run a full regression (minus occ fe) 
# and compare residuals

temp <- matched_jobs %>% 
  filter(!is.na(log_real_wkly_wage), !is.na(ho_occ), traditional == 1) %>% 
  ggplot(aes(log_real_wkly_wage, fill = factor(ho_occ))) +
  geom_density(alpha = 0.2) +
  labs(x = "Log Real Weekly Wage", y = "Density") +
  scale_fill_manual(name = "HO Occupation", breaks = c(0, 1),
                    values = c("blue", "red"),
                    labels = c("No", "Yes")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_light(base_size = 16) 

ggsave(str_c(figure_folder, "Jobs LRW Wage Traditional HO Occ v Not.pdf"),
       height = height, width = width)

eq <- create_formula("log_real_wkly_wage", c(controls, tenure, "outsourced", types))
fe <- create_formula("~", c("region", "marital_status", "msa", "case_id"))

reg <- lm_robust(eq, matched_jobs,
                 subset = !is.na(region) & !is.na(marital_status) &
                   !is.na(msa) & !is.na(ho_occ),
                 weights = weight,
                 fixed_effects = !!fe,
                 clusters = sample_id, 
                 se_type = "stata", try_cholesky = T)

df <- matched_jobs %>% 
  filter(!is.na(log_real_wkly_wage), !is.na(tenure), 
         !is.na(tot_child), !is.na(hh_child),
         !is.na(union_fill), !is.na(region), !is.na(marital_status),
         !is.na(msa), !is.na(ho_occ)) %>% 
  mutate(residual = lm_residuals(reg)) %>% 
  filter(residual != 0, residual >= - 3.5 * sd(residual),
         residual <= 3.5 * sd(residual), traditional == 1)

df_sum <- df %>% 
  as_survey_design(id = case_id, weight = weight) %>% 
  group_by(ho_occ) %>% 
  summarise(
    variance = survey_var(residual, vartype = "ci"),
    skew = unweighted(Skew(residual, conf.level = .95, ci.type = "basic")[1]),
    skew_low = unweighted(Skew(residual, conf.level = .95, ci.type = "basic")[2]),
    skew_upp = unweighted(Skew(residual, conf.level = .95, ci.type = "basic")[3]),
    obs = unweighted(n())
  ) %>% 
  arrange(desc(ho_occ))

temp <- df %>% 
  ggplot(aes(residual, fill = factor(ho_occ))) +
  geom_density(alpha = 0.2) +
  labs(x = str_c("Residual Log Real Weekly Wage"), y = "Density") +
  scale_fill_manual(name = "HO Occupation", breaks = c(0, 1),
                    values = c("blue", "red"),
                    labels = c("No", "Yes")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_light(base_size = 16) 

ggsave(str_c(figure_folder, "Jobs LRW Wage Residuals Traditional HO Occ v Not.pdf"),
       height = height, width = width)


# Residual LRW Wages vs Max Tenure ----------------------------------------

# Plot Residual LRW Wages (without Tenure controls) vs Tenure
# for outsourced and traditional
types <- c("self_emp", "indep_con", "temp_work", "on_call")

controls <- c("age", "I((age)^2)", "I((age)^3)", "I((age)^4)",
              "tot_child", "hh_child", "factor(union_fill)")

fixed_effects <- c("region", "marital_status", "msa", "occ", "case_id")

eq <- create_formula("log_real_wkly_wage", c(types, controls))
fe <- create_formula("~", fixed_effects)

reg <- lm_robust(eq, data = matched_jobs,
                 subset = !is.na(region) & !is.na(marital_status) &
                   !is.na(msa) & !is.na(occ),
                 weights = weight,
                 fixed_effects = !!fe,
                 clusters = sample_id, 
                 se_type = "stata", try_cholesky = T)

df <- matched_jobs %>% 
  filter(!is.na(log_real_wkly_wage),  
         !is.na(tot_child), !is.na(hh_child),
         !is.na(union_fill), !is.na(region), !is.na(marital_status),
         !is.na(msa), !is.na(occ)) %>% 
  mutate(residual = lm_residuals(reg)) %>% 
  filter(ever_ho_occ == 1, (outsourced == 1 | traditional == 1),
         abs(residual) > 1e-12)

temp <- df %>%
  ggplot(aes(x = residual, y = max_tenure, color = factor(outsourced))) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(x = str_c("Residual Log Real Weekly Wage"), y = "Max Tenure") +
  scale_color_manual(name = "Job Type", breaks = c(0, 1),
                     values = c("blue", "red"),
                     labels = c("Traditional", "Outsourced")) +
  theme_light(base_size = 16)

ggsave(str_c(figure_folder,
             "Jobs LRW Wage Residuals vs Max Tenure Ever HO Occupation.pdf"),
       height = height, width = width)
