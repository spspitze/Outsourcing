# This file takes data from matched_transition
# to compute summary statistics, run regressions, and make graphs

rm(list = ls())

# library(lpirfs)
library(magrittr)
library(data.table)
library(estimatr)
library(data.table)
library(openxlsx)
library(BSDA)
library(srvyr)
library(DescTools)
library(lubridate)
library(tidyverse)

# Folders of interest
clean_folder <- "../Cleaned Data/"
table_folder <- "../Tables/"
figure_folder <- "../Figures/NLSY 79 Transition/"

# For saving graphs
aspect_ratio <- 1.62
height <- 7
width <- height * aspect_ratio

# Download the data
transition <- read.csv(str_c(clean_folder, "matched_transition.csv"))

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

# Create a function that makes a formula given dependent variable 
# and list of independent variables
create_formula <- function(y, x_list) {
  vars <- x_list %>% 
    map(str_c, collapse = "+") %>% 
    str_c(collapse = "+")
  
  eq <- formula(str_c(y, vars, sep = "~"))
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
lm_residuals <- function(model) {
  resids <- model.frame(model)[[model$outcome]] - model$fitted.values
  return(resids[resids != 0])
}

# Create a function that takes an lm_robust model and calculates
# "effective N" or values actually used to estimate coefficients
# (drops single FE)
lm_N <- function(model) {
  length(lm_residuals(model))
}

# I often want to run code on whole sample and on ever_ho_occ. 
# Pair these dfs in a list
split_data <- function(df) {
  list(df, filter(df, ever_ho_occ == 1))
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
input-symbols=()-,
table-space-text-post = \\textsuperscript{***},
table-align-text-post = false,
group-digits          = false
}"

# Summary Statistics ------------------------------------------------------

# Rename union_fill to make matching easier
# Trim top 1% of time to find job
# Create weeks_prev/next_job_2 that sets 1 week to NA
# Create same occ/ind prev/next
transition %<>%
  mutate(
    u_f_curr = union_fill_curr, 
    u_f_prev = union_fill_prev,
    u_f_next = union_fill_next,
    weeks_job_prev = ifelse(
      weeks_prev_job < quantile(weeks_prev_job, .99, na.rm = T), 
      weeks_prev_job, NA),
    weeks_job_next = ifelse(
      weeks_next_job < quantile(weeks_next_job, .99, na.rm = T), 
      weeks_next_job, NA),
    wj_2_prev = ifelse(weeks_prev_job > 1, weeks_prev_job, NA),
    wj_2_next = ifelse(weeks_next_job > 1, weeks_next_job, NA),
    same_occ_prev = (occ_curr == occ_prev),
    same_ind_prev = (ind_curr == ind_prev),
    same_occ_next = (occ_curr == occ_next),
    same_ind_next = (ind_curr == ind_next)
  )

# Create a function that appends all variable names with each of a vector
# of given suffixes
add_suffix <- function(var_vec, suffix_vec, sep = "_") {
  return_vec <- c()
  for (var in var_vec) {
    for (suffix in suffix_vec) {
      return_vec %<>% append(str_c(var, suffix, sep = sep))
    }
  }
  return(return_vec)
}

vars_sum_all <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "part_time",
                  "union", "u_f", "job_sat", "any_benefits", "health", 
                  "retirement", "self_emp", "indep_con", "on_call",
                  "temp_work", "traditional")

vars_sum_np <- c("same_occ", "same_ind", "outsourced", "weeks_job", "wj_2")

vars_mean <- c(add_suffix(vars_sum_all, c("curr", "prev", "next")),
               add_suffix(vars_sum_np, c("prev", "next")),
               "n")

vars_match <- str_c(str_c(vars_sum_all, collapse = "|"),
                    str_c(vars_sum_np, collapse = "|"),
                    sep = "|")

constant <- c("outsourced_curr", "n", "n_se")

# Summary Statistics
transition_summary <- split_data(transition)

for (i in 1:2) { 
  transition_summary[[i]] %<>%
    filter(!is.na(outsourced_curr)) %>% 
    as_survey_design(ids = case_id, weights=weight) %>%
    group_by(outsourced_curr) %>%
    mutate(n = n()) %>% 
    summarise_at(vars_mean, survey_mean, na.rm = T) %>%
    gather(key=key, value=value, -constant) %>%
    mutate(
      var = str_subset(key, vars_match) %>% str_extract(vars_match),
      se = ifelse(str_detect(key, "_se"), "_se", ""),
      curr = str_detect(key, "_curr"),
      prev = str_detect(key, "_prev")) %>%
    unite(variable, var, se, sep = "") %>%
    select(-key) %>%
    spread(key=variable, value=value) %>% 
    select(-n_se) %>% 
    arrange(desc(outsourced_curr), desc(curr), desc(prev))
}

top <- str_c(table_top, siunitx, 
"
\\begin{tabular}{lSSSSSS}
\\toprule
& \\multicolumn{3}{c}{{Outsourced Currently}} & \\multicolumn{3}{c}{{Non-Outsourced Currently}} \\\\
& {Previous} & {Current} & {Next} & {Previous} & {Current} & {Next}  \\\\  \\midrule
"
)

# Create table in Latex
vars_t <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "part_time",
            "union", "job_sat", "health", "any_benefits")

# Divide variables by mean or percent (they are different below)
vars_t_m <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "job_sat")

vars_t_p <- c("part_time", "union", "health", "any_benefits")

# Also check if new job has same occ/ind
vars_t_o <- c("occ", "ind")

for (ho in 1:2) {
  if (ho == 1) {
    occ_name <- ""
    Occ_name <- ""
    occ_label <- ""
  } else {
    occ_name <- " in high outsourcing occupations"
    Occ_name <- " HO Occupations"
    occ_label <- "_ho"
  }
  
  center <- rbind("Outsourced", "", "Same", "Occupation", "Same", "Industry",
                    "Log Real", "Hourly Wage", "Log Real", "Weekly Earnings", "Hours Worked",
                    "Weekly", "Part Time", "", "Union", "","Job Satisfaction",
                    "(Lower Better)", "Health", "Insurance", "Any Benefits", "",
                    "Weeks To", "Find Job", "Weeks To", "Find Job ($>1$ week)",
                    "Observations")
    
  for (out in c(1, 4)){
    
    i <- out
    i_p <- out + 1
    i_n <- out + 2
    
    # Start with outsourced
    temp <- cbind(
      rbind(
        format_val(transition_summary[[ho]]$outsourced[i_p], 
                   star = p_test_1(transition_summary[[ho]], "outsourced", "n", i_p)),
        format_se(transition_summary[[ho]]$outsourced_se[i_p])
      ),
      rbind(
        str_c(" & {", format_it(1 * (out == 1), 3, 0), "}"),
        str_c(" & {--} ")
      ),
      rbind(
        format_val(transition_summary[[ho]]$outsourced[i_n], 
                   star = p_test_1(transition_summary[[ho]], "outsourced", "n", i_n)),
        format_se(transition_summary[[ho]]$outsourced_se[i_n])
      )
    )
    
    # Now do same occ/ind
    for (var in vars_t_o){
      var_n <- str_c("same", var, sep = "_")
      se_n <- str_c("same", var, "se", sep = "_")
      temp <- rbind(temp,
                    cbind(
                      rbind(
                        format_val(transition_summary[[ho]][[var_n]][i_p],
                                   star = p_test_1(transition_summary[[ho]],
                                                   var_n, "n", i_p)),
                        format_se(transition_summary[[ho]][[se_n]][i_p])
                      ),
                      rbind(str_c(" &  {--} "), str_c(" & ")),
                      rbind(
                        format_val(transition_summary[[ho]][[var_n]][i_n],
                                   star = p_test_1(transition_summary[[ho]],
                                                   var_n, "n", i_n)),
                        format_se(transition_summary[[ho]][[se_n]][i_n])
                      )
                    )
      )
    }
    
    # Now for everything else
    for (var in vars_t){
      se <- str_c(var, "se", sep = "_")
      if (var %in% vars_t_p){
        t <- "prop"
      } else{
        t <- "mean"
      }
      p_star_p <- test(transition_summary[[ho]], var, "n", i, i_p, type = t)
      p_star_n <- test(transition_summary[[ho]], var, "n", i, i_n, type = t)
      temp %<>% rbind(
        cbind(
          rbind(
            format_val(transition_summary[[ho]][[var]][i_p], star = p_star_p),
            format_se(transition_summary[[ho]][[se]][i_p])
          ),
          rbind(
            format_val(transition_summary[[ho]][[var]][i]),
            format_se(transition_summary[[ho]][[se]][i])
          ),
          rbind(
            format_val(transition_summary[[ho]][[var]][i_n], star = p_star_n),
            format_se(transition_summary[[ho]][[se]][i_n])
          )
        )
      )
    }
    
    # Weeks to find job (raw and >1) and n observations
    temp %<>% rbind(
      cbind(rbind(
              format_val(transition_summary[[ho]]$weeks_job[i_p]),
              format_se(transition_summary[[ho]]$weeks_job_se[i_p], 3),
              format_val(transition_summary[[ho]]$wj_2[i_p]),
              format_se(transition_summary[[ho]]$wj_2_se[i_p], 3),
              " & "),
            rbind(" & {--}", " & {--}", " & {--}", " & {--}",
                  format_n(transition_summary[[ho]]$n[i])),
            rbind(
              format_val(transition_summary[[ho]]$weeks_job[i_n]),
              format_se(transition_summary[[ho]]$weeks_job_se[i_n], 3),
              format_val(transition_summary[[ho]]$wj_2[i_n]),
              format_se(transition_summary[[ho]]$wj_2_se[i_n], 3),
              " & ")
            )
      )
    
    if (out == 1){
      center %<>% cbind(temp)
    } else{
      center %<>% cbind(
        temp,
        rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
              "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
              "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
              "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
              "\\\\", "\\\\[2pt]", "\\\\"))
    }
    
  }
  
  bot <- str_c(
    "\\bottomrule
\\end{tabular}
\\caption{Job statistics for men", occ_name,
    " at current and previous job for workers
who are currently outsourced compared to those who are not. Observations are at the
person-job level and summary statistics are weighted at the person level.
Stars represent significant difference from
current job (except for outsourced, same occupation, and same industry, which represent
significant difference from 0) at the .10 level *, .05 level **, and .01 level ***.}
\\label{jobs_t", occ_label ,"}
\\end{table}
\\end{document}"
  )
  
  # Do weird stuff to create LaTeX output
  t_folder <- str_c(table_folder, "Junk/")
  file_1 <- str_c(t_folder, "center.txt")
  write.table(center, file_1, quote=T, col.names=F, row.names=F)
  center <- read.table(file_1, sep = "")
  write.table(center, file_1, quote=F, col.names=F, row.names=F, sep = "")
  center <- readChar(file_1, nchars = 1e6)
  
  write.table(str_c(top, center, bot),
              str_c(table_folder,
                    "NLSY79 Job Transitions/Summary Statistics", Occ_name, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
}


# Weeks to Job ------------------------------------------------------------

# Focus on weeks_job_prev and wj_2_prev using HO Occupations 
# (transition_summary[[2]])

# Plot mean 
table <- str_c(table_top, siunitx, 
"
\\begin{tabular}{lSSSS}
\\toprule
& \\multicolumn{2}{c}{{All Transitions}} &  \\multicolumn{2}{c}{{$>1$ Week}} \\\\
& {Outsourced} & {Not Outsourced} & {Outsourced} & {Not Outsourced} \\\\  \\midrule
"
)

table %<>% str_c(
  format_val(transition_summary[[2]]$weeks_job[2]),
  format_val(transition_summary[[2]]$weeks_job[5],
             test(transition_summary[[2]], "weeks_job", "n",
                    2, 5, type = "mean")),
  format_val(transition_summary[[2]]$wj_2[2]),
  format_val(transition_summary[[2]]$wj_2[5], 
             test(transition_summary[[2]], "wj_2", "n",
                    2, 5, type = "mean")), "\\\\ \n",
  format_se(transition_summary[[2]]$weeks_job_se[2]),
  format_se(transition_summary[[2]]$weeks_job_se[5]),
  format_se(transition_summary[[2]]$wj_2_se[2]),
  format_se(transition_summary[[2]]$wj_2_se[5]),
  "\\\\ \n")

table %<>% str_c(
"\\bottomrule
\\end{tabular}
\\caption{Mean weeks to find current job for workers in high outsourcing
occupations both overall and for periods longer than 1 week. 
Stars represent significant difference from outsourced jobs at 
the .10 level *, .05 level **, and .01 level ***.}
\\label{weeks_find_job}
\\end{table}
\\end{document}")

write.table(
  table,
  str_c(table_folder,
        "NLSY79 Job Transitions/Weeks to Find Job HO Occupations.tex"),
  quote=F, col.names=F, row.names=F, sep="")

# Plot figures 
var_g <- c("weeks_job_prev", "wj_2_prev")
var_g_s <- c("weeks_job", "wj_2")
var_names <- c("Weeks to Find Job", "Weeks to Find Job")
var_save <- c("Weeks to Job", "Weeks to Job G1")

ho_save <- c("", " HO Occupations")
trans_plot <- split_data(transition)

for (ho in 1:2) {
  for (i in seq_along(var_g)) {
      
    # All occupations
    temp <- trans_plot[[ho]] %>%
      filter(!is.na(.[[var_g[i]]]), !is.na(outsourced_curr),
             .[[var_g[i]]] < quantile(.[[var_g[i]]], .99, na.rm = T)) %>% 
      ggplot() +
      geom_density(aes_string(var_g[i], fill = "factor(outsourced_curr)"),
                   alpha = 0.2) +
      geom_vline(aes(xintercept = transition_summary[[ho]][[var_g_s[i]]][2]),
                 color = "red", size=1) +
      geom_vline(aes(xintercept = transition_summary[[ho]][[var_g_s[i]]][5]),
                 color = "blue", size=1) +
      labs(x = var_names[i], y = "Density") +
      scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                        values = c("blue", "red"),
                        labels = c("Not Outsourced", "Outsourced")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
      theme_light(base_size = 16)
    
    ggsave(str_c(figure_folder, var_save[i], ho_save[ho], ".pdf"),
           height = height, width = width)
  }
}


# Regression on Weeks to Jobs ---------------------------------------------

top <- str_c(table_top, siunitx, 
               "
\\begin{tabular}{lSSSSSS}
\\toprule
&  \\multicolumn{3}{c}{{OLS}} & \\multicolumn{3}{c}{{FE}} \\\\
Variables & {Basic}  & {Job Info} & {Occ FE}   & {Basic} & {Job Info} & {Occ FE} \\\\\\midrule
"
)

# Type of current job (compare to traditional)
types <- c("outsourced", "self_emp", "indep_con", "temp_work", "on_call")

# Demographic Controls
dem_controls <- c("age_curr", "I(age_curr^2)", "I(age_curr^3)", "I(age_curr^4)",
                  "tot_child_curr", "hh_child_curr")

# Current job control
job_controls <- c("hours_week", "part_time", "u_f", "health", "retirement",
              "any_benefits","log_real_wkly_wage")

# Tenure controls from previous job (/100 to keep small)
tenure_controls <- c("I(tenure_prev / 100)", "I((tenure_prev / 100)^2)",
                     "I((tenure_prev / 100)^3)",
                     "I((tenure_prev / 100)^4)")

# OLS controls
ols_controls <- c("black", "hispanic", "hs_curr", "aa_curr", "ba_curr", "plus_ba_curr")

# Fixed Effects for all regresssions
fixed_effects <- c("region_curr", "marital_status_curr", "msa_curr")

# Occupation fixed effects
occ_effects <- c("occ_curr", "occ_prev")

# Individual fixed effects
id_effects <- c("case_id")

vars_1 <- list(add_suffix(types, c("curr", "prev")), dem_controls)
vars_2 <- append(vars_1, 
                 list(add_suffix(job_controls, c("curr", "prev")), tenure_controls))
var_list <- list(vars_1, vars_2, vars_2)

job_t <- c("No", "Yes", "Yes")
occ_t <- c("No", "No", "Yes")
worker_t <- c("No", "Yes")

center <- rbind("Outsourced", "Current", "Outsourced", "Previous",
                  "Job Info", "Occupation FE", "Worker FE", "$R^2$", "Obs")

for (j in 1:2){
  for (i in 1:3) {
    
    vars <- var_list[[i]]
    fes <- fixed_effects
    
    if (i == 3) {
      fes %<>% append(occ_effects)
    }
    
    if (j == 1) {
      vars %<>% append(ols_controls)
    } else {
      fes %<>% append(id_effects)
    }
    
    eq <- create_formula("weeks_job_prev", vars)
    fe <- create_formula("", fes)
    
    temp <- lm_robust(
      eq, data = transition, 
      subset = (!is.na(marital_status_curr)
                & !is.na(msa_curr) & !is.na(occ_curr) & !is.na(occ_prev)),
      weights = weight,
      fixed_effects = !!fe,
      clusters = as_factor(sample_id),
      se_type = "stata", try_cholesky = T)
    
    # Put regession results in Tables
    center %<>% cbind(
      rbind(format_val(temp$coefficients["outsourced_curr"],
                       p_stars(temp$p.value["outsourced_curr"])),
            format_se(temp$std.error["outsourced_curr"]),
            format_val(temp$coefficients["outsourced_prev"],
                       p_stars(temp$p.value["outsourced_prev"])),
            format_se(temp$std.error["outsourced_prev"]),
            str_c(" & {", job_t[i], "}"),
            str_c(" & {", occ_t[i], "}"),
            str_c(" & {", worker_t[j], "}"),
            format_val(temp$r.squared),
            format_n(lm_N(temp))
      )
    )
    
    # If full regression, plot actual vs predicted weeks
    if (i == 3 & j == 2) {
      
      df <- tibble(est = temp$fitted.values, 
                   act = model.frame(temp)$weeks_job_prev) 
      
      temp_plot <- df %>% 
        filter(act < quantile(act, .99), act != est) %>% 
        ggplot() +
        geom_abline(slope = 1, intercept = 0, color = "blue") +
        geom_jitter(aes(est, act)) +
        labs(x = "Estimated Weeks to Find Job", y = "Actual Weeks to Find Job") +
        theme_light(base_size = 16) 
      
      ggsave(str_c(figure_folder, "Weeks to Find Job Actual v Estimated.pdf"),
             height = height, width = width)
      
      temp_plot <- df %>% 
        filter(act < quantile(act, .99), act != est) %>% 
        ggplot() +
        geom_density(aes(est, fill = "red"), alpha = 0.2, show.legend = T) +
        geom_density(aes(act, fill = "blue"), alpha = 0.2, show.legend = T) +
        labs(x = "Weeks to Find Job", y = "Density") +
        scale_fill_manual(name = "Weeks to\nFind Job", breaks = c("blue", "red"),
                          values = c("blue", "red"),
                          labels = c("Actual", "Estimated")) +
        scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
        theme_light(base_size = 16) 
      
      ggsave(str_c(figure_folder, 
                   "Weeks to Find Job Actual v Estimated Distribution.pdf"),
             height = height, width = width)
    }
    
  }
}

center %<>% cbind( 
  rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\[2pt]",
        "\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]")
)

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
\\caption{Regressions of outsourced at current and previous job on 
weeks to find current job. All regressions
include controls for current and previous job type (reported coefficients are 
compared to traditional jobs), a quartic in age, dummies for region,
whether in an MSA or central city, marital status, number of children total
and in household, and dummies for observation year. The first three columns 
run OLS and also contain controls for race and education. The last three columns
use worker fixed effects. Regression with job info contain current and previous
hours worked per week, part-time status, log real weekly wage, union status, and
whether received health insurance, retirement benefits, or any benefits. They also
contain a quartic of previous tenure. Regressions with occupation fixed effects
contain fixed effects for current and previous occupation.
All observations are at the person-job level and all standard errors are
clustered by demographic sampling group.
Stars represent significant at the .10 level *, .05 level **, and .01 level ***.}
\\label{reg_weeks_between_jobs}
\\end{table}
\\end{document}"
)

write.table(str_c(top, center, bot),
            str_c(table_folder, 
                  "NLSY79 Job Transitions/Weeks to Find Job Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")
  