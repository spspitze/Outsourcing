# This file takes data from matched_clean and matched_jobs_clean
# to compute summary statistics

rm(list = ls())

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
figure_folder <- "../Figures/NLSY 79/"

# For saving graphs
aspect_ratio <- 1.62
height <- 7
width <- height * aspect_ratio

# Download data and filter out women (only look at men)
matched <- read_csv(str_c(clean_folder, "matched.csv"),
  col_types = cols(
    .default = col_double(),
    week_start_job = col_date(format = ""),
    week_end_job = col_date(format = ""),
    ho_occ = col_logical(),
    ever_ho_occ = col_logical()
  )) %>% 
  filter(female == 0)

matched_jobs <- read_csv(str_c(clean_folder, "matched_jobs.csv"),
                         col_types = cols(
                           .default = col_double(),
                           week_start_job = col_date(format = ""),
                           week_end_job = col_date(format = ""),
                           ho_occ = col_logical(),
                           ever_ho_occ = col_logical()
                         ))%>% 
  filter(female == 0)

# Create a function that finds difference of means or proportions and reports
# * if 10%, ** if 5%, and *** if 1% different
test <- function(data, var, obs, row_1, row_2, type) {
  if (type == "mean") {
    test <- tsum.test(mean.x=data[[var]][row_1],
                      s.x=data[[str_c(var, "_se")]][row_1] * sqrt(data[[obs]][row_1]),
                      n.x=data[[obs]][row_1],
                      mean.y=data[[var]][row_2],
                      s.y=data[[str_c(var, "_se")]][row_2] * sqrt(data[[obs]][row_2]),
                      n.y=data[[obs]][row_2])
  } else if (type == "prop") {
    # If value is 0, return ""
    if (data[[var]][row_1] == 0 | data[[var]][row_2] == 0){
      return("")
    }
    test <- prop.test(x = c(data[[var]][row_1] * data[[obs]][row_1], 
                            data[[var]][row_2] * data[[obs]][row_2]),
                      n = c(data[[obs]][row_1], data[[obs]][row_2]),
                      correct = FALSE)
  } else {
    return(warning("Not a valid test"))
  }
  
  p <- test$p.value
  if (p < .01) {
    stars <- "\\textsuperscript{***}"
  } else if (p < .05) {
    stars <- "\\textsuperscript{**}"
  } else if (p < .1) {
    stars <- "\\textsuperscript{*}"
  } else {
    stars <- ""
  }
}

# Create a function that finds proportion test and reports
# * if 10%, ** if 5%, and *** if 1% different
p_test_1 <- function(data, var, obs, row){
  test <- prop.test(x = data[[var]][row] * data[[obs]][row],
                    n = data[[obs]][row], correct = FALSE)
  p <- test$p.value
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
\\begin{document}
\\begin{table}
\\footnotesize
\\centering \n"

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

# Create married/single if certain matrital statuses
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

# Demographic Summary Statistics ------------------------------------------------------

# First, look at people who ever outsource vs those who never do (ever_out == 1/0)
# Overall and in HO Occupations
# Weight based on population
# Look only at first observation for demographics
vars <- c("black", "hispanic", "less_hs", "hs", "aa", "ba", "plus_ba",
          "single", "married", "tot_child", "hh_child", "n")

sum_demo <- split_data(matched)

for (i in 1:2) {
  sum_demo[[i]] <- matched %>%
    group_by(case_id) %>% 
    filter(row_number() == min(row_number())) %>% 
    as_survey_design(ids = case_id, weights = weight) %>% 
    group_by(ever_out_oj) %>%
    mutate(n = n()) %>% 
    summarise_at(vars, survey_mean, na.rm = T) %>% 
    arrange(desc(ever_out_oj)) 
}

# Create table in Latex
vars_d <- c("black", "hispanic", "less_hs", "hs", "aa", "ba", "plus_ba",
            "single", "married")

vars_c <- c("tot_child", "hh_child")

for (ho in 1:2) {
  
  center <- rbind("Black", "", "Hispanic", "", "No HS Diploma", "", "HS Diploma",
                    "", "AA Degree", "", "BA Degree", "", "Post Graduate", "Degree",
                    "Single", "", "Married", "", "Total Number", "of Children",
                    "Children in", "Household", "Observations")
  
  if (ho == 1) {
    description <- ""
    label <- ""
    save <- ""
  } else {
    description <- " who ever work in a high outsourcing occupation"
    label <- "_ever_ho"
    save <- " Ever HO Occupation"
  }
  
  for (i in 1:2) {
    col_i <- c()
    for (var in vars_d){
      se <- str_c(var, "_se")
      stars <- ""
      if (i == 2){
        stars <- test(sum_demo[[ho]], var, "n", i, i - 1, type = "prop")
      }
        col_i %<>% rbind(format_val(sum_demo[[ho]][[var]][i], star = stars),
                         format_se(sum_demo[[ho]][[se]][i]))
      
    }
    # Don"t forget about total/hh children, which are means
    for (var in vars_c){
      se <- str_c(var, "_se")
      stars <- ""
      if (i == 2){
        stars <- test(sum_demo[[ho]], var, "n", i, i - 1, type = "mean")
      }
      col_i %<>% rbind(format_val(sum_demo[[ho]][[var]][i], star = stars),
                       format_se(sum_demo[[ho]][[se]][i]))
    }
    col_i %<>% rbind(format_n(sum_demo[[ho]][["n"]][i]))
    center %<>% cbind(col_i)
  }
  
  center %<>% cbind( 
    rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\")
  )
  
  # Do weird stuff to create LaTeX output
  j_folder <- str_c(table_folder, "Junk/")
  file_1 <- str_c(j_folder, "center.txt")
  write.table(center, file_1, quote=T, col.names=F, row.names=F)
  center <- read.table(file_1, sep = "")
  write.table(center, file_1, quote=F, col.names=F, row.names=F, sep = "")
  center <- readChar(file_1, nchars = 1e6)
  
  top <- str_c(table_top, siunitx,
                 "
\\begin{tabular}{lSS}
\\toprule
Variable & {Ever Outsourced} & {Never Outsourced} \\\\ \\midrule
"
  )
  
  bot <- str_c(
  "\\bottomrule
\\end{tabular}
\\caption{Demographic statistics for men", description, 
" for those who work at least
one outsourced job (in On Jobs survey) versus those who never do.
Observations are at the person level for first survey post-2000 
and summary statistics are weighted.
Stars represent significant difference from ever outsourced at the .10 level *,
.05 level **, and .01 level ***.}
\\label{demo", label, "}
\\end{table}
\\end{document}"
  )
  
  write.table(str_c(top, center, bot),
              str_c(table_folder, "NLSY79 Demographics/Demographics", save, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
}


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
\\caption{Job numbers for men.}
\\label{job_numbers}
\\end{table}
\\end{document}"
)

write.table(table, str_c(table_folder, "NLSY79 Jobs/Job Numbers.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# Jobs Summary Statistics -------------------------------------------------

# Summarize jobs at the year level using matched and job level using
# matched_jobs. Split by outsourced or not
# And the by all job types

vars_sum <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week",
          "part_time", "tenure", "union", "job_sat", "any_benefits",
          "health", "retirement", "childcare", "dental", "flex_sched",
          "life", "maternity", "profit_share", "train_school", "single",
          "married", "tot_child", "hh_child", "n")

sum_jobs <- c(split_data(matched), split_data(matched_jobs),
              split_data(matched), split_data(matched_jobs))
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
top_j <- str_c(table_top, siunitx, 
"
\\begin{tabular}{lSS}
\\toprule
& {Outsourced} & {Non-Outsourced} \\\\  \\midrule
"
)

top_j_t <- str_c(table_top, siunitx, 
"
\\begin{tabular}{lSSSSSS}
\\toprule
Variable & {Outsourced} & {Traditional} & {Self-Employed} & {Ind. Contractor} & {On-Call} & {Temp} \\\\ \\midrule 
"
)

# Create table in Latex
# Use all vars except n
vars <- vars_sum[-length(vars_sum)]

# Divide variables by mean or prop (they are different below)
vars_m <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week",
              "tenure", "job_sat", "wks_work_prev",
              "tot_child", "hh_child")

vars_p <- c("part_time", "union", "any_benefits", "health", "retirement", "childcare",
              "dental", "flex_sched", "life", "maternity", "profit_share", "train_school",
              "single", "married")

sample <- c("", " for workers who are ever in high outsourcing industries")
observation <- c("person-job-year", "person-job")
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
      "Leave", "Profit", "Sharing", "Training", "", "Single", "", "Married",
      "", "Total Number", "of Children", "Children in", "Household", "Observations")
    
    for (i in 1:NROW(sum_jobs[[j]])){
      col_i <- c()
      for (var in vars){
        se <- str_c(var, "_se")
        mode <- if (var %in% vars_p) "prop" else "mean"
        stars <- if (i > 1) test(sum_jobs[[j]], var, "n", 1, i, mode) else ""
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
            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
            "\\\\"))
  
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
\\caption{Summary statistics of jobs divided by outsourced and non-outsourced jobs",
sample[ho], ". 
Observations are at the ", observation[ob], " level and 
statistics are weighted at the person level. 
Stars represent significant difference from outsourced jobs at the .10 level *, 
.05 level **, and .01 level ***.}
\\label{jobs", label[k], "}
\\end{table}
\\end{document}"
      )
    } else {
      top <- top_j_t
      bot <- str_c(
"\\bottomrule
\\end{tabular}
\\caption{Summary statistics of jobs divided by job types",
        sample[ho], ". 
Observations are at the ", observation[ob], " level and 
statistics are weighted at the person level. 
Stars represent significant difference from outsourced jobs at the .10 level *, 
.05 level **, and .01 level ***.}
\\label{job_types", label[k], "}
\\end{table}
\\end{document}"
      )
    }
  
  write.table(str_c(top, center, bot),
              str_c(table_folder, "NLSY79 ", folder[ty] ,"/Jobs ", save[k], ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
    }
  }
}

# HO Occupation Characteristics --------------------------------------------

# How do HO Occupations compare to non-HO Occupations? Use matched_jobs
# and aggregate to occupation level
vars_sum <- c("outsourced", "age", "black", "hispanic",
              "less_hs", "hs", "aa", "ba", "plus_ba",
              "log_real_hrly_wage", "log_real_wkly_wage", "hours_week",
              "part_time", "tenure", "union", "job_sat", "any_benefits",
              "health", "retirement", "single", "married",
              "n", "weight_w", "ho_occ")

occupation_type <- matched_jobs %>% 
  filter(!is.na(ho_occ), !is.na(occ)) %>% 
  as_survey_design(ids = case_id, weights = weight) %>% 
  group_by(occ) %>% 
  mutate(
    weight_w = sum(weight),
    n = n()
    ) %>% 
  summarise_at(vars_sum, survey_mean, na.rm = T) %>%
  as_survey_design(ids = occ, weights = weight_w) %>% 
  mutate(ho_occ = round(ho_occ)) %>% 
  group_by(ho_occ) %>% 
  mutate(n = n()) %>% 
  summarise_at(vars_sum[-length(vars_sum):-(length(vars_sum)-1)],
               survey_mean, na.rm = T) %>% 
  arrange(desc(ho_occ))

# Create a Latex table
top <- str_c(table_top, siunitx, 
               "
\\begin{tabular}{lSS}
\\toprule
& {High Outsourcing} & {Others} \\\\  \\midrule
"
)

# Drop last three vars (n, weight, and ho_occ) from vars
vars <- vars_sum[-length(vars_sum):-(length(vars_sum)-2)]

# Proportion varables
vars_p <- c("outsourced", "black", "hispanic", 
            "less hs", "hs", "aa", "ba", "plus ba", "part_time", "union",
            "any_benefits", "health", "retirement", "single", "married")

center <- rbind("Percent", "Outsourced", "Age", "", "Percent", "Black", "Percent", 
                "Hispanic", "Less", "High School", "High School", "",
                "Associates", "Degree", "Bachelor's", "Degree", "Plus", "Degree",
                "Log Real", "Hourly Wage", "Log Real", 
                "Weekly Wage", "Hours Worked", "per Week", "Part-Time", "",
                "Weeks Tenure", "", "Union", "", "Job Satisfaction", "(Lower Better)", 
                "Any Benefits", "", "Health Insurance", "", "Retirement", "Plan",
                "Single", "", "Married", "", "Observations")

for (i in 1:2) {
  
  c_i <- c()
  for (var in vars) {
    
    se <- str_c(var, "se", sep = "_")
    t <- if (var %in% vars_p) "prop" else "mean"
    stars <- if (i == 2) test(occupation_type, var, "n", 1, 2, type = t) else ""
    
    c_i %<>% rbind(
      format_val(occupation_type[[var]][i], star = stars),
      format_se(occupation_type[[se]][i])
      )
  }
  
  c_i %<>% rbind(format_n(occupation_type$n[i]))
  
  center %<>% cbind(c_i)
}

center %<>% cbind(
  rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
        "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
        "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
        "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
        "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
        "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
        "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
        "\\\\[2pt]"))

# Do weird stuff to create LaTeX output
j_folder <- str_c(table_folder, "Junk/")
file_1 <- str_c(j_folder, "center.txt")
write.table(center, file_1, quote=T, col.names=F, row.names=F)
center <- read.table(file_1, sep = "")
write.table(center, file_1, quote=F, col.names=F, row.names=F, sep = "")
center <- readChar(file_1, nchars = 1e6)

bot <- "
\\bottomrule
\\end{tabular}
\\caption{Summary statistics of jobs divided by High Outsourcing (HO) occupations
(all occupations with outsourcing two times more than average) vs not.
Stars represent significant difference from HO occupations at the .10 level *, 
.05 level **, and .01 level ***.}
\\label{ho_occ}
\\end{table}
\\end{document}"

write.table(str_c(top, center, bot),
            str_c(table_folder, "NLSY79 Occupation Info/HO Occupations.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# Summarize Union NA ------------------------------------------------------

# Note: always run this code after previous

# Many people are missing union. Do these people/jobs look any different
union_type <- c(split_data(matched), split_data(matched_jobs))
for (i in 1:4) {
  union_type[[i]] %<>%
    filter(!is.na(union_fill)) %>% 
    as_survey_design(ids = case_id, weights = weight) %>% 
    group_by(union_fill, outsourced) %>% 
    mutate(n = n()) %>% 
    summarise_at(vars_sum, survey_mean, na.rm = T) %>% 
    arrange(desc(union_fill))
}


# Dube and Kaplan ---------------------------------------------------------

# Compare outsourced vs not for janitors and security guards based on
# self-reported outsourced and Dube and Kaplan's measure (outsourced_2), 
# whic uses those in ceratin industries as outsourced. 
# See if they are significantly different

janitor <- matched_jobs %>%
  filter(occ == 4220) %>%
  mutate(
    outsourced_2 = 1 * (ind == 7690),
    out_1_v_2 = outsourced - outsourced_2
  ) %>% 
  filter(!is.na(outsourced_2)) 

sg <- matched_jobs %>%
  filter(occ == 3920) %>%
  mutate(
    outsourced_2 = 1 * (ind == 7680),
    out_1_v_2 = outsourced - outsourced_2
  ) %>% 
  filter(!is.na(outsourced_2)) 

vars_sum_js <- c("log_real_hrly_wage", "log_real_wkly_wage", "any_benefits", 
                 "health", "hours_week", "part_time", "union", "job_sat",
                 "less_hs", "hs", "aa", "ba", "plus_ba",
                 "black", "hispanic", "age", "n")

sum_js <- c(split_data(janitor), split_data(sg))

for (i in 1:4) {
  out<- if (i %% 2 == 1) "outsourced" else "outsourced_2"
  
  sum_js[[i]] %<>%
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(out) %>%
    mutate(n = n()) %>% 
    summarise_at(vars_sum_js, survey_mean, na.rm = T) %>% 
    arrange_at(desc(out))
}

top_js <- str_c(table_top, siunitx, 
"
\\begin{tabular}{lSSSS}
\\toprule
& \\multicolumn{2}{c}{{Self-Reported (This Paper)}} & 
\\multicolumn{2}{c}{{Industry-Occupation (Dube and Kaplan)}} \\\\
Variable & {Outsourced} & {Not Outsourced} & {Outsourced} & {Not Outsourced} \\\\ \\midrule
"
)

# Create table in Latex
vars_js <- vars_sum_js[-length(vars_sum_js)]

vars_js_p <- c("part_time", "black", "hispanic", "less_hs", "hs", "aa", "ba", "plus_ba", 
               "female", "any_benefits", "health", "union")

vars_js_m <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "job_sat", "age")

desc_js <- c("janitors (occupation 4220)", "security guards (occupation 3920)")
label_js <- c("janitor", "sg")
occ_js <- c("Janitors", "Security Guards")
ind_js <- c("4220", "3920") 

for (js in 1:2){
  
  center_js <- rbind("Log Real", "Hourly Wage", "Log Real", "Weekly Wage",
                     "Any Benefits", "", "Health Insurance", "", "Hours Worked",
                     "per Week", "Part Time", "",
                     "Union", "", "Job Satisfaction", "(Lower Better)", "No HS Diploma",
                     "", "HS Diploma", "","AA Degree", "", "BA Degree", "",
                     "Post Graduate", "Degree", "Black", "",
                     "Hispanic", "", "Age", "", "Observations")
  
  for (def in 1:2) {
    
    j <- 2 * (js - 1) + def
    
    for (i in 1:2) {
      
      col_i <- c()
      for (var in vars_js){
        se <- str_c(var, "_se")
        mode <- if (var %in% vars_js_p) "prop" else "mean"
        stars <- if (i == 2) test(sum_js[[j]], var, "n", 1, i, mode) else ""
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
\\caption{Summary statistics for ", desc_js[js], " that are outsourced vs
not outsourced. In the left two columns, outsourced is self-reported by the worker.
In the right two, it is inferred if the worker is in industry ", ind_js[js], 
" following Dube and Kaplan (2010). Observations are at the
worker-job-year level and summary statistics are weighted. 
Stars represent significant difference from outsourced (the second and fourth column)
or from self-reported outsourced (the third column) at the .10 level *,
.05 level **, and .01 level ***.}
\\label{", "}
\\end{table}
\\end{document}"
  )
  
  write.table(str_c(top_js, center_js, bot_js),
              str_c(table_folder, "NLSY79 Dube Kaplan/", occ_js[js], " Summary.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
}

# What are the self-reported job types of DK's outsourced?
# Use data.frame's SQL like capabilities to make these tables
top <- str_c(table_top, 
"
\\begin{tabular}{lrr|r}
\\toprule
& \\multicolumn{3}{c} {Industry-Occupation (Dube and Kaplan)} \\\\
Self-Reported & Outsourced & Not Outsourced & Total \\\\ \\midrule
"
)

occ_js <- c("janitors (occupation 4220)", "security guards (occupation 3920)")
ind_js <- c("7690", "7680")
label <- c("janitors", "sg")
save <- c("Janitors", "Security Guards")

types <- c("outsourced", "indep_con", "temp_work", "on_call", "self_emp", "traditional")

dt_js <- list(data.table(janitor), data.table(sg))

for (js in 1:2){
  
  center <- c("Outsourced", "Independent Contractor", "Temp Worker",
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
\\caption{Counts of Dube and Kaplan (2010) method of measuring outsourcing versus
NLSY 79 self-reported job type for ", occ_js[js], ". 
For columns, workers are consider outsourced if they are in industry ",
    ind_js[js], ". 
For rows, I use the worker's self-reported job type.}
\\label{dk_types_", label[js], "}
\\end{table}
\\end{document}"
  )
  
  write.table(str_c(top, center, bot),
              str_c(table_folder, "NLSY79 Dube Kaplan/", save[js], " Types.tex"),
              quote=F, col.names=F, row.names=F, sep="")
}


# Katz and Krueger --------------------------------------------------------

# From Katz and Krueger (2019), look at workers by job types (Table 2)

vars_types <- c("outsourced", "self_emp", "indep_con", "temp_work", "on_call", 
                "traditional", "n")

job_type_sum <- matched_jobs %>% 
  mutate(n = n()) %>% 
  as_survey_design(ids = case_id, weights=weight) %>% 
  summarise_at(vars_types, survey_mean, na.rm = T)

# job_type_sum_gender <- matched_jobs %>% 
#   mutate(
#   alternative = indep_con + temp_work + on_call + outsourced,
#   n = n()
#   ) %>%
#   as_survey_design(ids = case_id, weights=weight) %>% 
#   group_by(female) %>% 
#   summarise_at(var_types, survey_mean, na.rm = T)

vars <- vars_types[-length(vars_types)]

name_type <- c("Outsourced", "Self-Employed", "Independent Contractor",
              "Temp Worker", "On-Call Worker", "Traditional Employee" )

top <- str_c(table_top, 
"
\\begin{tabular}{lr}
\\toprule
Job Type & Total \\\\ \\midrule
")

for (i in seq_along(vars)) {
  
  top %<>% str_c(
    name_type[i], format_per(job_type_sum[[vars[i]]]), " \\\\ \n")
}

top %<>% str_c(
  " \\midrule \n Total", format_n(job_type_sum$n), " \\\\ \n ")

bot <- "\\bottomrule
\\end{tabular}
\\caption{Percent of jobs in each job type for men. 
Observations weighted at the person level.}
\\label{job_types}
\\end{table}
\\end{document}"

write.table(str_c(top, bot),
            str_c(table_folder, "NLSY79 Job Types/Katz and Kruger.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# Before next sections, remove some larger files to free memory.
rm("dt_js", "janitor", "job_type_sum", "life_jobs", "sg", "sum_demo",
   "sum_jobs", "sum_js", "union_type")

# Regressions -------------------------------------------------------------

# Create a loop that takes various desired outcomes, runs series of regressions,
# and creates table in Latex
# 1. Basic Controls (age:age quartic, black, hispanic, education,
# union_fill, region, msa/cc, marital status, children, hours week / part time, year)
# 2. Add tenure quartic
# 3. Add occ factors
# 4. Add 2+3
# 5. Individual FE + some controls (age quartic, union_fill, region, msa/cc,
# marital stutus, children, hours week / part time, year)
# 6. Add tenure quartic
# 7. Add occ factors
# 8. Add 6+7

top <- str_c(table_top, siunitx, 
             "
\\begin{tabular}{lSSSSSS}
\\toprule
& \\multicolumn{2}{c}{{OLS}} & \\multicolumn{4}{c}{{Worker Fixed Effects}} \\\\
& {Basic} & {Full}  & {Basic}  & {Tenure} & {Occ FE} & {Full}  \\\\\\midrule
"
)

var_r <- c("log_real_hrly_wage", "log_real_wkly_wage", "job_sat",
           "any_benefits", "health")

var_names <- c("log real hourly wages", "log real weekly wages", 
               "job satisfaction (lower is better)",
               "receiving any employment benefits",
               "receiving health insurance through employer")

save_names <- c("LRH Wages", "LRW Wages", "Job Satisfaction", "Benefits",
                "H Insurance")

types <- c("outsourced", "self_emp", "indep_con", "temp_work", "on_call")

controls <- c("age", "I((age)^2)", "I((age)^3)", "I((age)^4)",
              "tot_child", "hh_child", "factor(union_fill)")

hours <- c("hours_week", "part_time")

ols_controls <- c("black", "hispanic", "hs", "aa", "ba", "plus_ba")

tenure <- c("I((tenure/100))", "I((tenure/100)^2)", "I((tenure/100)^3)",
            "I((tenure/100)^4)")

fixed_effects <- c("region", "marital_status", "msa")

labels <- c("lrhw", "lrww", "job_sat", "benefits", "health")

dfs <- list(matched, matched_jobs)
samples <- c("person-job-year", "person-job")
s_labels <- c("_year", "_job")
s_saves <- c("Years ", "Jobs ")

for (loop in 1:2) {
  
  sample <- samples[loop]
  s_label <- s_labels[loop]
  s_save <- s_saves[loop]
  y <- if (loop == 1) " and year" else ""
  
  for (ind in seq_along(var_r)) {
    
    center <- rbind("Outsourced", "", "Worker FE", "Tenure Quartic", "Occupation FE",
                    "$R^2$", "Observations")
    
    var <- var_r[ind]
    var_name <- var_names[ind]
    label <- labels[ind]
    save <- save_names[ind]
    
    for (reg_ind in 1:6){
      
      ind_vars <- c(types, controls)
      fe_vars <- if (loop == 1) c(fixed_effects, "int_year") else c(fixed_effects)
      
      hours_text <- "" 
      if (var != "log_real_wkly_wage"){
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
              str_c(" & ", ten_ind),
              str_c(" & ", occ_ind),
              str_c(" & ", ife_ind),
              format_val(temp$r.squared),
              format_n(lm_N(temp))
        )
      )
      
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
      \\caption{Regressions of outsourced on ", var_name, ". All regressions
  include controls for job type (traditional job is default), 
      a quartic in age, union status,", hours_text,
      " dummies for region", y, ", whether in an MSA or central city,
  marital status, and number of children total and in household. 
  The first two columns run OLS and also contain controls for
  race and education. The last four columns use worker fixed effects.
  All observations are at the ", sample, " level and all standard errors are
  clustered by occupation category.
  Stars represent significant at the .10 level *, .05 level **, and .01 level ***.}
  \\label{reg_" , label, s_label, "}
  \\end{table}
  \\end{document}"
    )
    
    write.table(str_c(top, center, bot),
                str_c(table_folder, "NLSY79 Regressions/", s_save, save, ".tex"),
                quote=F, col.names=F, row.names=F, sep="")
    
  }
}

# Free up space for type
rm("c_i")

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

top <- str_c(table_top, siunitx, 
             "
\\begin{tabular}{lSSS}
\\toprule
& {Outsourced} & {Self-Employed} & {Full} \\\\\\midrule
"
)

for (loop in 1:2) {
  
  sample <- samples[loop]
  s_label <- s_labels[loop]
  s_save <- s_saves[loop]
  y <- if (loop == 1) " and year" else ""
  
  for (ind in seq_along(var_r)) {
    
    center <- rbind("Outsourced", "", "Self-Employed", "", "Independent", "Contractor",
                    "On-Call", "", "Temp Worker", "", "$R^2$", "Observations")
    
    var <- var_r[ind]
    var_name <- var_names[ind]
    label <- labels[ind]
    save <- save_names[ind]
    
    for (reg_ind in 1:3){
      
      ind_vars <- c(controls)
      fe_vars <- c(fixed_effects, "case_id", "occ")
      fe_vars <- if (loop == 1) c(fe_vars, "int_year") else fe_vars
      
      hours_text <- "" 
      if (var != "log_real_wkly_wage"){
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
      temp <- lm_robust(eq, data = matched, weights = weight,
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
      \\caption{Regressions of job type on ", var_name, ". Missing type is traditional
      jobs. All regressions  use worker and occupation fixed effects and include
      a quartic in age, union status,", hours_text, 
      " dummies for region", y, ", whether in an MSA or central city,
      marital status, and number of children in household and total.
      All observations are at the ", sample, " level and all standard errors are
      clustered by occupation category.
      Stars represent significant at the .10 level *, .05 level **, and .01 level ***.}
      \\label{type_reg_" , label, s_label, "}
      \\end{table}
      \\end{document}"
    )
    
    write.table(str_c(top, center, bot),
                str_c(table_folder, "NLSY79 Regressions/", s_save, save, " Types.tex"),
                quote=F, col.names=F, row.names=F, sep="")
    
    # Code sometimes very slow here. Maybe deleting some files will help
    rm("temp", "eq", "fe", "center", "top", "bot",
       "ou", "se", "ic", "oc", "tw")
    
  }
}


# Plots -------------------------------------------------------------------

# # Use ggplot 2 to graph some things
# Graph some variables by year for outsourced vs not
# Both all workers and ever_ho_occ

# Also plot log_real_hrly/wkly_wage residuals 
# from full FE regression about (but without outsourcing)
# See how these compare for outsourced vs non-outsourced

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
      
      ind_vars <- c(controls, tenure)
      
      if (var_g[i] != "log_week_earn"){
        ind_vars %<>% c(hours)
      }
      
      if (ob == 1) {
        fe_vars <- c(fixed_effects, "int_year")
      } else {
        fe_vars <- fixed_effects
      }
      
      eq <- create_formula(var_g[i], ind_vars)
      fe <- create_formula("~", fe_vars)
      
      reg <- lm_robust(eq, data = dfs[[ob]],
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
        filter(!is.na(.[[var_g[i]]])) %>%
        ggplot(aes_string(var_g[i], fill = "factor(outsourced)")) +
        geom_density(alpha = 0.2) +
        labs(x = var_names[i], y = "Density") +
        scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                          values = c("blue", "red"),
                          labels = c("Not Outsourced", "Outsourced")) +
        scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
        theme_light(base_size = 16) 
      
      ggsave(str_c(figure_folder, s_saves[ob],
                   var_saves[i], ho_saves[ho], ".pdf"),
             height = height, width = width)
      
      # If looking at wages, plot residuals too. 
      # Also create a table of the standard deviation and skew of residuals
      # for both outsourced and not outsourced
      if (i <= 2) {
        k <- if (ob == 1) 1 else 3
        df <- dfs[[k]] %>% 
          filter(!is.na(.[[var_g[i]]]), !is.na(tenure), 
                 !is.na(tot_child), !is.na(hh_child),
                 !is.na(union_fill), !is.na(region), !is.na(marital_status),
                 !is.na(msa), !is.na(occ)) %>% 
          mutate(residual = lm_residuals(reg)) %>% 
          filter_ever(ho == 2) %>% 
          filter(residual != 0, residual >= - 3.5 * sd(residual),
                 residual <= 3.5 * sd(residual))
        
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
          scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                            values = c("blue", "red"),
                            labels = c("Not Outsourced", "Outsourced")) +
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
          theme_light(base_size = 16) 
        
        ggsave(str_c(figure_folder, s_saves[ob], var_saves[i],
                     " Residuals", ho_saves[ho], ".pdf"),
               height = height, width = width)
        
        
      }
      
      # Also plot job_sat as a density histogram (only do this once)
      if (i == 1) {
        temp <- dfs[[j]] %>%
          filter(!is.na(job_sat)) %>%
          ggplot(aes(round(job_sat, 0), fill = factor(outsourced))) +
          geom_histogram(aes(y=.5*..density..), alpha=0.5, position="dodge",
                         binwidth = .5) +
          labs(x = "Job Satisfaction", y = "Density") +
          scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                            values = c("blue", "red"),
                            labels = c("Not Outsourced", "Outsourced")) +
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
          theme_light(base_size = 16) 
        
        ggsave(str_c(figure_folder, s_saves[ob],
                     "Job Satisfaction", ho_saves[ho], ".pdf"),
               height = height, width = width)
      }
    }
  }
}

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
