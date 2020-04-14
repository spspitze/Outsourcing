# This file takes data from matched_clean and matched_jobs clean
# to compute summary statistics, run regressions, and make graphs

rm(list = ls())

library(estimatr)
library(data.table)
library(openxlsx)
library(BSDA)
library(srvyr)
library(DescTools)
library(tidyverse)

# Folders of interest
clean_folder <- "Cleaned Data/"
table_folder <- "../Tables/"
figure_folder <- "../Figures/NLSY 79/"

# For saving graphs
aspect_ratio <- 1.62
height <- 7
width <- height * aspect_ratio

matched <- read_csv(str_c(clean_folder, "matched_clean_occ.csv"),
  col_types = cols(
    .default = col_double(),
    month_start_job = col_date(format = ""),
    month_end_job = col_date(format = ""),
    week_start_job = col_date(format = ""),
    week_end_job = col_date(format = ""),
    outsourcing_occ = col_logical(),
    ever_out_occ = col_logical()
  ))

matched_jobs <- read_csv(str_c(clean_folder, "matched_jobs_clean_occ.csv"),
                         col_types = cols(
                           .default = col_double(),
                           week_start_job = col_date(format = ""),
                           week_end_job = col_date(format = ""),
                           outsourcing_occ = col_logical(),
                           ever_out_occ = col_logical()
                         ))

# Create a function that finds difference of means and reports
# * if 10%, ** if 5%, and *** if 1% different
t_test <- function(data, var, obs, row_1, row_2){
  test <- tsum.test(mean.x=data[[var]][row_1],
                    s.x=data[[str_c(var, "_se")]][row_1] * sqrt(data[[obs]][row_1]),
                    n.x=data[[obs]][row_1],
                    mean.y=data[[var]][row_2],
                    s.y=data[[str_c(var, "_se")]][row_2] * sqrt(data[[obs]][row_2]),
                    n.y=data[[obs]][row_2])
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

# Create a function that finds difference of proportions and reports
# * if 10%, ** if 5%, and *** if 1% different
p_test <- function(data, var, obs, row_1, row_2){
  # If value is 0, return ""
  if (data[[var]][row_1] == 0 | data[[var]][row_2] == 0){
    return("")
  }
  
  test <- prop.test(x = c(data[[var]][row_1] * data[[obs]][row_1], 
                          data[[var]][row_2] * data[[obs]][row_2]),
                    n = c(data[[obs]][row_1], data[[obs]][row_2]), correct = FALSE)
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

###############################################################################

# In this section, create tables with summary statistics

# First, look at people who ever outsource vs those who never do (ever_out == 1/0)
# Divide by men and women and weight based on population
# Look only at first observation for demographics
sum_demo <- matched %>%
  group_by(case_id) %>% 
  filter(row_number() == min(row_number())) %>% 
  as_survey_design(ids = case_id, strata=sample_id, weights=weight) %>% 
  mutate(ever_out = factor(ever_out)) %>%
  group_by(female, ever_out) %>% 
  summarise(
    proportion = survey_mean(),
    black_per = survey_mean(black, na.rm = T),
    hispanic_per = survey_mean(hispanic, na.rm = T),
    less_hs_per = survey_mean(less_hs, na.rm = T),
    hs_per = survey_mean(hs, na.rm = T),
    aa_per = survey_mean(aa, na.rm = T),
    ba_per = survey_mean(ba, na.rm = T),
    plus_ba_per = survey_mean(plus_ba, na.rm = T),
    single_per = survey_mean(marital_status == 1, na.rm = T),
    married_per = survey_mean(marital_status == 2, na.rm = T),
    tot_child_mean = survey_mean(tot_child, na.rm = T),
    hh_child_mean = survey_mean(hh_child, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(female, desc(ever_out)) 

# Create table in Latex
vars_d <- c("black", "hispanic", "less_hs", "hs", "aa", "ba", "plus_ba",
            "single", "married")

vars_c <- c("tot", "hh")

center_d <- rbind("Black", "", "Hispanic", "", "No HS Diploma", "", "HS Diploma",
                  "", "AA Degree", "", "BA Degree", "", "Post Graduate", "Degree",
                  "Single", "", "Married", "", "Total Number", "of Children",
                  "Children in", "Household", "Observations")

for (i in seq(1, NROW(sum_demo), by = 1)){
  col_i <- c()
  for (var in vars_d){
    var_n = str_c(var, "_per")
    se_n = str_c(var, "_per_se")
    if (i %% 2 > 0){
      col_i <- rbind(col_i,
                     str_c(" & ", format(round(sum_demo[[var_n]][i], 3), nsmall=3)),
                     str_c(" & (", format(round(sum_demo[[se_n]][i], 3), nsmall=3), ")"))
    }
    if (i %% 2 == 0){
      stars <- p_test(sum_demo, var_n, "n", i, i - 1)
      col_i <- rbind(col_i,
                     str_c(" & ", format(round(sum_demo[[var_n]][i], 3), nsmall=3),
                            stars),
                     str_c(" & (", format(round(sum_demo[[se_n]][i], 3), nsmall=3), ")"))
    }
  }
  # Don"t forget about total/hh children, which are means
  for (var in vars_c){
    var_n = str_c(var, "_child_mean")
    se_n = str_c(var, "_child_mean_se")
    if (i %% 2 > 0){
      col_i <- rbind(col_i,
                     str_c(" & ", format(round(sum_demo[[var_n]][i], 3), nsmall=3)),
                     str_c(" & (", format(round(sum_demo[[se_n]][i], 3), nsmall=3), ")"))
    }
    if (i %% 2 == 0){
      stars <- t_test(sum_demo, var_n, "n", i, i - 1)
      col_i <- rbind(col_i,
                     str_c(" & ", format(round(sum_demo[[var_n]][i], 3), nsmall=3),
                            stars),
                     str_c(" & (", format(round(sum_demo[[se_n]][i], 3), nsmall=3), ")"))
    }
  }
  col_i <- rbind(col_i, 
                 str_c(" & {", format(sum_demo$n[i], big.mark = ",", trim = T), "}"))
  center_d <- cbind(center_d, col_i)
}

center_d <- cbind(center_d, 
                  rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                        "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                        "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                        "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\"))

# Do weird stuff to create LaTeX output
j_folder <- str_c(table_folder, "Junk/")
file_1 <- str_c(j_folder, "center_d.txt")
file_2 <- str_c(j_folder, "center_d.txt")
write.table(center_d, file_1, quote=T, col.names=F, row.names=F)
center_d <- read.table(file_1, sep = "")
write.table(center_d, file_2, quote=F, col.names=F, row.names=F, sep = "")
center_d <- readChar(file_2, nchars = 1e6)

top_d <- str_c(table_top, siunitx,
"
\\begin{tabular}{lSSSS}
\\toprule
& \\multicolumn{2}{c}{{Men}} & \\multicolumn{2}{c}{{Women}} \\\\
Variable & {Ever} & {Never} & {Ever} & {Never} \\\\ \\midrule
"
)

bot_d <- "\\bottomrule
\\end{tabular}
\\caption{Demographic statistics for men and women for those who work at least
one outsourced job versus those who never do. Observations are at the person level for
their first year in the survey post-2000 and summary statistics are weighted.
Stars represent significant difference from ever outsourced at the .10 level *,
.05 level **, and .01 level ***.}
\\label{demo}
\\end{table}
\\end{document}"

write.table(str_c(top_d, center_d, bot_d),
            str_c(table_folder, "NLSY79 Demographics/NLSY79 Demographics.tex"),
            quote=F, col.names=F, row.names=F, sep="")

###############################################################################

# At the person level, look at lifetime number of jobs and outsourced jobs
life_jobs <- matched_jobs %>% 
  group_by(case_id) %>% 
  mutate(
    n_jobs = n(),
    n_out_jobs = sum(outsourced)
  ) %>% 
  filter(row_number() == min(row_number())) %>% 
  as_survey_design(ids = case_id, strata=sample_id, weights=weight) %>% 
  mutate(ever_out = factor(ever_out)) %>%
  group_by(female, ever_out) %>% 
  summarise(
    mean_n_jobs = survey_mean(n_jobs),
    mean_n_out_jobs = survey_mean(n_out_jobs),
    n = unweighted(n())
  )

# How many people only outsource in dataset?
only_outsource <- matched_jobs %>% 
  group_by(case_id) %>% 
  mutate(per_out = mean(outsourced)) %>% 
  filter(per_out == 1)

##################################################################################

# Now look at job characteristics for outsourceing jobs vs non-outsourceing jobs
sum_jobs_y <- matched %>% 
  as_survey_design(ids = case_id, strata=sample_id, weights=weight) %>% 
  mutate(outsourced = factor(outsourced)) %>% 
  group_by(female, outsourced) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_hrly_wage_mean = survey_mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    tenure_mean = survey_mean(tenure, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    wks_work_prev_mean = survey_mean(wks_work_prev, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    any_benefits_per = survey_mean(any_benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    retirement_per = survey_mean(retirement, na.rm = T),
    childcare_per = survey_mean(childcare, na.rm = T),
    dental_per = survey_mean(dental, na.rm = T),
    flex_sched_per = survey_mean(flex_sched, na.rm = T),
    life_per = survey_mean(life, na.rm = T),
    maternity_per = survey_mean(maternity, na.rm = T),
    profit_share_per = survey_mean(profit_share, na.rm = T),
    train_school_per = survey_mean(train_school, na.rm = T),
    single_per = survey_mean(marital_status == 1, na.rm = T),
    married_per = survey_mean(marital_status == 1, na.rm = T),
    tot_child_mean = survey_mean(tot_child, na.rm = T),
    hh_child_mean = survey_mean(hh_child, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(female, desc(outsourced))


# This table is similar to the one above, but only looks at those who 
# were ever outsourced out
sum_ever_jobs_y <- matched %>% 
  filter(ever_out == 1) %>% 
  as_survey_design(ids = case_id, weights=weight) %>% 
  mutate(outsourced = factor(outsourced)) %>% 
  group_by(female, outsourced) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_hrly_wage_mean = survey_mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    tenure_mean = survey_mean(tenure, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    wks_work_prev_mean = survey_mean(wks_work_prev, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    any_benefits_per = survey_mean(any_benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    retirement_per = survey_mean(retirement, na.rm = T),
    childcare_per = survey_mean(childcare, na.rm = T),
    dental_per = survey_mean(dental, na.rm = T),
    flex_sched_per = survey_mean(flex_sched, na.rm = T),
    life_per = survey_mean(life, na.rm = T),
    maternity_per = survey_mean(maternity, na.rm = T),
    profit_share_per = survey_mean(profit_share, na.rm = T),
    train_school_per = survey_mean(train_school, na.rm = T),
    single_per = survey_mean(marital_status == 1, na.rm = T),
    married_per = survey_mean(marital_status == 2, na.rm = T),
    tot_child_mean = survey_mean(tot_child, na.rm = T),
    hh_child_mean = survey_mean(hh_child, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(female, desc(outsourced))

# Do the same as above except do summary at person-job level rather than
# person-job-year level
sum_jobs_j <- matched_jobs %>%  
  as_survey_design(ids = case_id, strata=sample_id, weights=weight) %>% 
  mutate(outsourced = factor(outsourced)) %>% 
  group_by(female, outsourced) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_hrly_wage_mean = survey_mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    tenure_mean = survey_mean(tenure, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    wks_work_prev_mean = survey_mean(wks_work_prev, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    any_benefits_per = survey_mean(any_benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    retirement_per = survey_mean(retirement, na.rm = T),
    childcare_per = survey_mean(childcare, na.rm = T),
    dental_per = survey_mean(dental, na.rm = T),
    flex_sched_per = survey_mean(flex_sched, na.rm = T),
    life_per = survey_mean(life, na.rm = T),
    maternity_per = survey_mean(maternity, na.rm = T),
    profit_share_per = survey_mean(profit_share, na.rm = T),
    train_school_per = survey_mean(train_school, na.rm = T),
    single_per = survey_mean(single, na.rm = T),
    married_per = survey_mean(married, na.rm = T),
    tot_child_mean = survey_mean(tot_child, na.rm = T),
    hh_child_mean = survey_mean(hh_child, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(female, desc(outsourced))


# This table is similar to the one above, but only looks at those who 
# were ever outsourced out
sum_ever_jobs_j <- matched_jobs %>% 
  filter(ever_out == 1) %>% 
  as_survey_design(ids = case_id, weights=weight) %>% 
  mutate(outsourced = factor(outsourced)) %>% 
  group_by(female, outsourced) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_hrly_wage_mean = survey_mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    tenure_mean = survey_mean(tenure, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    wks_work_prev_mean = survey_mean(wks_work_prev, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    any_benefits_per = survey_mean(any_benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    retirement_per = survey_mean(retirement, na.rm = T),
    childcare_per = survey_mean(childcare, na.rm = T),
    dental_per = survey_mean(dental, na.rm = T),
    flex_sched_per = survey_mean(flex_sched, na.rm = T),
    life_per = survey_mean(life, na.rm = T),
    maternity_per = survey_mean(maternity, na.rm = T),
    profit_share_per = survey_mean(profit_share, na.rm = T),
    train_school_per = survey_mean(train_school, na.rm = T),
    single_per = survey_mean(single, na.rm = T),
    married_per = survey_mean(married, na.rm = T),
    tot_child_mean = survey_mean(tot_child, na.rm = T),
    hh_child_mean = survey_mean(hh_child, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(female, desc(outsourced))

# Create tables for both types

top_j <- str_c(table_top, siunitx, "
\\begin{tabular}{lSSSSSS}
\\toprule
& \\multicolumn{3}{c}{{Men}} & \\multicolumn{3}{c}{{Women}} \\\\
& {Outsourced} & {Non-Outsourced} & {Non-Outsourced} & {Outsourced} & {Non-Outsourced} & {Non-Outsourced} \\\\ 
Variable &  & {(All)} & {(Ever Outsourced)} &  & {(All)} & {(Ever Outsourced)} \\\\ \\midrule
"
)

bot_y <- "\\bottomrule
\\end{tabular}
\\caption{Job statistics for men and women. The three categories are outsourced 
jobs, all non-outsourced jobs, and non-outsourced jobs for people who ever had
an outsourced job. Observations are at the person-job-year level and summary 
statistics are weighted at the person level. 
Stars represent significant difference from outsourced jobs at the .10 level *, 
.05 level **, and .01 level ***.}
\\label{jobs_y}
\\end{table}
\\end{document}"


bot_j <- "\\bottomrule
\\end{tabular}
\\caption{Job statistics for men and women. The three categories are outsourced 
jobs, all non-outsourced jobs, and non-outsourced jobs for people who ever had
an outsourced job. Observations are at the person-job level (jobs with mulitple 
years of observations have average characteristics of all years) and summary 
statistics are weighted at the person level. 
Stars represent significant difference from outsourced jobs at the .10 level *, 
.05 level **, and .01 level ***.}
\\label{jobs_j}
\\end{table}
\\end{document}"

# Create table in Latex
vars_j <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "part_time",
            "tenure", "union", "job_sat", "wks_work_prev",
            "any_benefits", "health",
            "retirement", "childcare", "dental", "flex_sched", "life", "maternity",
            "profit_share", "train_school", "single", "married", "tot_child", "hh_child")

# Divide variables by mean or percent (they are different below)
vars_m_j <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week",
              "tenure", "job_sat", "wks_work_prev",
              "tot_child", "hh_child")

vars_p_j <- c("part_time", "union", "any_benefits", "health", "retirement", "childcare",
              "dental", "flex_sched", "life", "maternity", "profit_share", "train_school",
              "single", "married")

for (loop in c(1, 2)){
  # Set up parameters
  if (loop == 1){
    sum_jobs <- sum_jobs_y
    sum_ever_jobs <- sum_ever_jobs_y
    bot_v <- bot_y
    name <- "Year"
  } else{
    sum_jobs <- sum_jobs_j
    sum_ever_jobs <- sum_ever_jobs_j
    bot_v <- bot_j
    name <- "Jobs"
    
  }
  
  center_j <- rbind("Log Real", "Hourly Wage", "Log Real", "Weekly Wage", "Hours Worked", 
                    "Weekly", "Part Time", "", "Tenure", "(Weeks)", "Union", "",
                    "Job Satisfaction", "(Lower Better)", "Weeks Worked", "Previous Year",
                    "Any Benefits", "", "Health", "Insurance", "Retirement", "Plan",
                    "Subsidized", "Childcare", "Dental", "Insurance", "Flex", "Schedule",
                    "Life", "Insurance", "Maternity", "Leave", "Profit", "Sharing",
                    "Training", "", "Single", "", "Married", "", "Total Number",
                    "of Children", "Children in", "Household", "Observations")
  
  
  for (i in seq(1, NROW(sum_jobs) + 2, by = 1)){
    col_i <- c()
    if (i %% 3 > 0){
      data <- sum_jobs
      j <- i - (i > 3)
    } else{
      data <- sum_ever_jobs
      j <- i - (i > 0) - (i > 3)
    }
    for (var in vars_j){
      if (var %in% vars_p_j){
        var_n = str_c(var, "_per")
        se_n = str_c(var, "_per_se")
        if (j %% 2 == 0){
          stars <- p_test(data, var_n, "n", j, j - 1)
        }
      } else{
        var_n = str_c(var, "_mean")
        se_n = str_c(var, "_mean_se")
        if (j %% 2 == 0){
          stars <- t_test(data, var_n, "n", j, j - 1)
        }
      }
      if (j %% 2 > 0){
        col_i <- rbind(col_i,
                       str_c(" & ", format(round(data[[var_n]][j], 3), nsmall=3)),
                       str_c(" & (", format(round(data[[se_n]][j], 3), nsmall=3), ")"))
      }
      if (j %% 2 == 0){
        col_i <- rbind(col_i,
                       str_c(" & ", format(round(data[[var_n]][j], 3), nsmall=3),
                              stars),
                       str_c(" & (", format(round(data[[se_n]][j], 3), nsmall=3), ")"))
      }
    }
    col_i <- rbind(col_i, 
                   str_c(" & {", format(data$n[j], big.mark = ",", trim = T), "}"))
    center_j <- cbind(center_j, col_i)
  }
  
  center_j <- cbind(center_j, 
                    rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                          "\\\\", "\\\\[2pt]", "\\\\"))
  
  # Do weird stuff to create LaTeX output
  j_folder <- str_c(table_folder, "Junk/")
  file_3 <- str_c(j_folder, "center_j.txt")
  file_4 <- str_c(j_folder, "center_j.txt")
  write.table(center_j, file_3, quote=T, col.names=F, row.names=F)
  center_j <- read.table(file_3, sep = "")
  write.table(center_j, file_4, quote=F, col.names=F, row.names=F, sep = "")
  center_j <- readChar(file_4, nchars = 1e6)
  
  
  write.table(str_c(top_j, center_j, bot_v),
              str_c(table_folder, "NLSY79 Jobs/NLSY79 Jobs ", name, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
}

#############################################################################
# Sumarise jobs by all job types
# Outsourced, traditional, self-employed, independent contractor, 
# temp worker, and on-call worker
# Both at person-job-year level and person-job level

sum_jobs_types_y <- matched %>% 
  as_survey_design(ids = case_id, strata=sample_id, weights=weight) %>% 
  mutate(outsourced = factor(outsourced),
         traditional = factor(traditional),
         self_emp = factor(self_emp),
         indep_con = factor(indep_con),
         on_call = factor(on_call),
         temp_work = factor(temp_work)) %>% 
  group_by(female, temp_work, on_call, indep_con,
           self_emp, traditional, outsourced) %>% 
  summarise(
    log_real_hrly_wage_mean = survey_mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    tenure_mean = survey_mean(tenure, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    wks_work_prev_mean = survey_mean(wks_work_prev, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    any_benefits_per = survey_mean(any_benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    retirement_per = survey_mean(retirement, na.rm = T),
    childcare_per = survey_mean(childcare, na.rm = T),
    dental_per = survey_mean(dental, na.rm = T),
    flex_sched_per = survey_mean(flex_sched, na.rm = T),
    life_per = survey_mean(life, na.rm = T),
    maternity_per = survey_mean(maternity, na.rm = T),
    profit_share_per = survey_mean(profit_share, na.rm = T),
    train_school_per = survey_mean(train_school, na.rm = T),
    single_per = survey_mean(marital_status == 1, na.rm = T),
    married_per = survey_mean(marital_status == 1, na.rm = T),
    tot_child_mean = survey_mean(tot_child, na.rm = T),
    hh_child_mean = survey_mean(hh_child, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(female)

sum_jobs_types_j <- matched_jobs %>%  
  mutate(outsourced = factor(outsourced),
         traditional = factor(traditional),
         self_emp = factor(self_emp),
         indep_con = factor(indep_con),
         on_call = factor(on_call),
         temp_work = factor(temp_work)) %>% 
  as_survey_design(ids = case_id, strata=sample_id, weights=weight) %>% 
  mutate(outsourced = factor(outsourced)) %>% 
  group_by(female, temp_work, on_call, indep_con,
           self_emp, traditional, outsourced) %>% 
  summarise(
    log_real_hrly_wage_mean = survey_mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    tenure_mean = survey_mean(tenure, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    wks_work_prev_mean = survey_mean(wks_work_prev, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    any_benefits_per = survey_mean(any_benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    retirement_per = survey_mean(retirement, na.rm = T),
    childcare_per = survey_mean(childcare, na.rm = T),
    dental_per = survey_mean(dental, na.rm = T),
    flex_sched_per = survey_mean(flex_sched, na.rm = T),
    life_per = survey_mean(life, na.rm = T),
    maternity_per = survey_mean(maternity, na.rm = T),
    profit_share_per = survey_mean(profit_share, na.rm = T),
    train_school_per = survey_mean(train_school, na.rm = T),
    single_per = survey_mean(single, na.rm = T),
    married_per = survey_mean(married, na.rm = T),
    tot_child_mean = survey_mean(tot_child, na.rm = T),
    hh_child_mean = survey_mean(hh_child, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(female)

# Create tables for men and women for both year and jobs 

top_j_t <- str_c(table_top, siunitx, 
"
\\begin{tabular}{lSSSSSS}
\\toprule
Variable & {Outsourced} & {Traditional} & {Self-Employed} & {Ind. Contractor} & {On-Call} & {Temp} \\\\ \\midrule 
"
)

vars_j <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "part_time",
            "tenure", "union", "job_sat", "wks_work_prev",
            "any_benefits", "health",
            "retirement", "childcare", "dental", "flex_sched", "life", "maternity",
            "profit_share", "train_school", "single", "married", "tot_child", "hh_child")

# Divide variables by mean or percent (they are different below)
vars_m_j <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week",
              "tenure", "job_sat", "wks_work_prev", 
              "tot_child", "hh_child")

vars_p_j <- c("part_time", "union", "any_benefits", "health", "retirement", "childcare",
              "dental", "flex_sched", "life", "maternity", "profit_share", "train_school",
              "single", "married")

for (loop in c(1, 2)){
  # Set up parameters
  for (sex in c(0, 6)){
    sex_name <- "women"
    sex_save <- "Women"
    if (sex == 0){
      sex_name <- "men"
      sex_save <- "Men"
    }
    if (sex == 6){
      sex_name <- "women"
      sex_save <- "Women"
    }
    if (loop == 1){
      data <- sum_jobs_types_y
      level_name <- "person-job-year"
      label <- str_c(sex_name, "_job_types_y")
      save_name <- str_c(sex_save, " Years")
    } else{
      data <- sum_jobs_types_j
      level_name <- "person-job"
      label <- str_c(sex_name, "_job_types_j")
      save_name <- str_c(sex_save, " Jobs")
    }
    
    center_j_t <- rbind("Log Real", "Hourly Wage", "Log Real", "Weekly Wage",
                        "Hours Worked", "Weekly", "Part Time", "", "Tenure", "(Weeks)",
                        "Union", "", "Job Satisfaction", "(Lower Better)", "Weeks Worked",
                        "Previous Year", "Any Benefits", "",
                        "Health", "Insurance", "Retirement", "Plan", "Subsidized", 
                        "Childcare", "Dental", "Insurance", "Flex", "Schedule", "Life",
                        "Insurance", "Maternity", "Leave", "Profit", "Sharing", 
                        "Training", "", "Single", "", "Married", "", "Total Number",
                        "of Children", "Children in", "Household", "Observations")
    
    
    for (j in seq(1, 6, by = 1)){
      i <- j + sex
      col_i <- c()
      
      for (var in vars_j){
        stars <- ""
        
        if (var %in% vars_p_j){
          var_n = str_c(var, "_per")
          se_n = str_c(var, "_per_se")
          if (j > 1){
            stars <- p_test(data, var_n, "n", i, sex + 1)
          }
        } else{
          var_n = str_c(var, "_mean")
          se_n = str_c(var, "_mean_se")
          if (j > 1){
            stars <- t_test(data, var_n, "n", i, sex + 1)
          }
        }
        
        col_i <- rbind(col_i,
                       str_c(" & ", format(round(data[[var_n]][i], 3), nsmall=3),
                             stars),
                       str_c(" & (", format(round(data[[se_n]][i], 3), nsmall=3), ")"))
        
      }
      col_i <- rbind(col_i, 
                     str_c(" & {", format(data$n[i], big.mark = ",", trim = T), "}"))
      center_j_t <- cbind(center_j_t, col_i)
    }
    
    center_j_t <- cbind(center_j_t, 
                      rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                            "\\\\", "\\\\[2pt]", "\\\\"))
    
    # Do weird stuff to create LaTeX output
    j_folder <- str_c(table_folder, "Junk/")
    file_3_t <- str_c(j_folder, "center_j_t.txt")
    file_4_t <- str_c(j_folder, "center_j_t.txt")
    write.table(center_j_t, file_3_t, quote=T, col.names=F, row.names=F)
    center_j_t <- read.table(file_3_t, sep = "")
    write.table(center_j_t, file_4_t, quote=F, col.names=F, row.names=F, sep = "")
    center_j_t <- readChar(file_4_t, nchars = 1e6)
    
    bot_j_t <- str_c(
      "\\bottomrule
      \\end{tabular}
      \\caption{Job statistics for ", sex_name,
      ". Jobs are broken into mutually exclusive
      categories: outsourced, traditional, self-employed, independent contractors,
      on-call workers, and temp workers. Observations are at the ", level_name,
      " level and summary statistics are weighted at the person level. 
      Stars represent significant difference from outsourced jobs at the .10 level *, 
      .05 level **, and .01 level ***.}
      \\label{", label, "}
      \\end{table}
      \\end{document}")
    
    
    write.table(str_c(top_j_t, center_j_t, bot_j_t),
                str_c(table_folder, "NLSY79 Jobs/NLSY79 Job Types ", save_name, ".tex"),
                quote=F, col.names=F, row.names=F, sep="")
  }
}

####################################################################################

# Now summarise each job and look at the traits of the job before and
# after
# Merge to get previous and next jobs
job_transitions <- matched_jobs %>%
  left_join(matched_jobs,
            by = c("case_id" = "case_id", "previous_job" = "emp_id"),
            suffix = (c("" , "_prev"))) %>%
  left_join(matched_jobs,
            by = c("case_id" = "case_id", "next_job" = "emp_id"),
            suffix = (c("" , "_next")))

# Sumarise job transitions for outsourced and not outsourced
# (I also rearrange dataset to make it look like past ones,
# this makes making tables easier)
constant <- c("female", "outsourced", "n")

job_transitions_sum <- job_transitions %>%
  as_survey_design(ids = case_id, weights=weight) %>%
  mutate(outsourced = factor(outsourced)) %>%
  group_by(female, outsourced) %>%
  summarise(
    proportion = survey_mean(),
    log_real_hrly_wage_mean = survey_mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    any_benefits_per = survey_mean(any_benefits, na.rm = T),
    outsourced_prev_per = survey_mean(outsourced_prev, na.rm = T),
    log_real_hrly_wage_prev_mean = survey_mean(log_real_hrly_wage_prev, na.rm = T),
    log_real_wkly_wage_prev_mean = survey_mean(log_real_wkly_wage_prev, na.rm = T),
    hours_week_prev_mean = survey_mean(hours_week_prev, na.rm = T),
    part_time_prev_per = survey_mean(part_time_prev, na.rm = T),
    job_sat_prev_mean = survey_mean(job_sat_prev, na.rm = T),
    union_prev_per = survey_mean(union_prev, na.rm = T),
    any_benefits_prev_per = survey_mean(any_benefits_prev, na.rm = T),
    same_occ_prev_per = survey_mean(occ == occ_prev, na.rm = T),
    same_ind_prev_per = survey_mean(ind == ind_prev, na.rm = T),
    outsourced_next_per = survey_mean(outsourced_next, na.rm = T),
    log_real_hrly_wage_next_mean = survey_mean(log_real_hrly_wage_next, na.rm = T),
    log_real_wkly_wage_next_mean = survey_mean(log_real_wkly_wage_next, na.rm = T),
    hours_week_next_mean = survey_mean(hours_week_next, na.rm = T),
    part_time_next_per = survey_mean(part_time_next, na.rm = T),
    job_sat_next_mean = survey_mean(job_sat_next, na.rm = T),
    union_next_per = survey_mean(union_next, na.rm = T),
    any_benefits_next_per = survey_mean(any_benefits_next, na.rm = T),
    same_occ_next_per = survey_mean(occ == occ_next, na.rm = T),
    same_ind_next_per = survey_mean(ind == ind_next, na.rm = T),
    n = unweighted(n())
  ) %>%
  select(
    female, outsourced, n,
    log_real_hrly_wage_mean_curr = log_real_hrly_wage_mean,
    log_real_hrly_wage_mean_se_curr = log_real_hrly_wage_mean_se,
    log_real_wkly_wage_mean_curr = log_real_wkly_wage_mean,
    log_real_wkly_wage_mean_se_curr = log_real_wkly_wage_mean_se,
    hours_week_mean_curr = hours_week_mean,
    hours_week_mean_se_curr = hours_week_mean_se,
    part_time_per_curr = part_time_per,
    part_time_per_se_curr = part_time_per_se,
    job_sat_mean_curr = job_sat_mean,
    job_sat_mean_se_curr = job_sat_mean_se,
    union_per_curr = union_per,
    union_per_se_curr = union_per_se,
    any_benefits_per_curr = any_benefits_per,
    any_benefits_per_se_curr = any_benefits_per_se,
    log_real_hrly_wage_mean_prev = log_real_hrly_wage_prev_mean,
    log_real_hrly_wage_mean_se_prev = log_real_hrly_wage_prev_mean_se,
    log_real_wkly_wage_mean_prev = log_real_wkly_wage_prev_mean,
    log_real_wkly_wage_mean_se_prev = log_real_wkly_wage_prev_mean_se,
    hours_week_mean_prev = hours_week_prev_mean,
    hours_week_mean_se_prev = hours_week_prev_mean_se,
    part_time_per_prev = part_time_prev_per,
    part_time_per_se_prev = part_time_prev_per_se,
    job_sat_mean_prev = job_sat_prev_mean,
    job_sat_mean_se_prev = job_sat_prev_mean_se,
    union_per_prev = union_prev_per,
    union_per_se_prev = union_prev_per_se,
    any_benefits_per_prev = any_benefits_prev_per,
    any_benefits_per_se_prev = any_benefits_prev_per_se,
    outsourced_per_prev = outsourced_prev_per,
    outsourced_per_se_prev = outsourced_prev_per_se,
    same_occ_per_prev = same_occ_prev_per,
    same_occ_per_se_prev = same_occ_prev_per_se,
    same_ind_per_prev = same_ind_prev_per,
    same_ind_per_se_prev = same_ind_prev_per_se,
    log_real_hrly_wage_mean_next = log_real_hrly_wage_next_mean,
    log_real_hrly_wage_mean_se_next = log_real_hrly_wage_next_mean_se,
    log_real_wkly_wage_mean_next = log_real_wkly_wage_next_mean,
    log_real_wkly_wage_mean_se_next = log_real_wkly_wage_next_mean_se,
    part_time_per_next = part_time_next_per,
    part_time_per_se_next = part_time_next_per_se,
    hours_week_mean_next = hours_week_next_mean,
    hours_week_mean_se_next = hours_week_next_mean_se,
    job_sat_mean_next = job_sat_next_mean,
    job_sat_mean_se_next = job_sat_next_mean_se,
    union_per_next = union_next_per,
    union_per_se_next = union_next_per_se,
    any_benefits_per_next = any_benefits_next_per,
    any_benefits_per_se_next = any_benefits_next_per_se,
    outsourced_per_next = outsourced_next_per,
    outsourced_per_se_next = outsourced_next_per_se,
    same_occ_per_next = same_occ_next_per,
    same_occ_per_se_next = same_occ_next_per_se,
    same_ind_per_next = same_ind_next_per,
    same_ind_per_se_next = same_ind_next_per_se
  ) %>%
  gather(key=key, value=val, -constant) %>%
  mutate(
    var = substring(key, 1, nchar(key) - 5),
    time = substring(key, nchar(key) - 3)) %>%
  select(-key) %>%
  filter(!is.na(var), !is.na(time)) %>%
  spread(key=var, value=val) %>%
  arrange(female, desc(outsourced))

top_t <- str_c(table_top, siunitx, "
\\begin{tabular}{lSSSSSS}
\\toprule
& \\multicolumn{3}{c}{{Outsourced Currently}} & \\multicolumn{3}{c}{{Non-Outsourced Currently}} \\\\
& {Previous} & {Current} & {Next} & {Previous} & {Current} & {Next} \\\\  \\midrule
"
)

# Create table in Latex
vars_t <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "part_time",
            "union", "job_sat", "any_benefits")

# Divide variables by mean or percent (they are different below)
vars_m_t <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "job_sat")

vars_p_t <- c("part_time", "union", "any_benefits")

# Also check if new job has same occ/ind
vars_o_t <- c("occ", "ind")

for (sex in c(1, 7)){
  
  Sex_name <- "Men"
  sex_name <- "men"
  if (sex == 7){
    Sex_name <- "Women"
    sex_name <- "women"
  }
  
  center_t <- rbind("Outsourced", "", "Same", "Occupation", "Same", "Industry",
                    "Log Real", "Hourly Wage", "Log Real", "Weekly Earnings", "Hours Worked",
                    "Weekly", "Part Time", "", "Union", "","Job Satisfaction",
                    "(Lower Better)", "Any Benefits", "",
                    "Observations")
  
  for (out in c(0, 3)){

    i <- sex + out
    i_n <- sex + out + 1
    i_p <- sex + out + 2

    # Start with outsourced
    temp <- cbind(
      rbind(
        str_c(" & ", format(round(job_transitions_sum$outsourced_per[i_p], 3), nsmall=3),
               p_test_1(job_transitions_sum, "outsourced_per", "n", i_p)),
        str_c(" & (", format(round(job_transitions_sum$outsourced_per_se[i_p], 3), nsmall=3), ")")
      ),
      rbind(
        str_c(" & ", format(round(1 - (out / 3), 3), nsmall=3)),
        str_c(" & {--} ")
      ),
      rbind(
        str_c(" & ", format(round(job_transitions_sum$outsourced_per[i_n], 3), nsmall=3),
               p_test_1(job_transitions_sum, "outsourced_per", "n", i_n)),
        str_c(" & (", format(round(job_transitions_sum$outsourced_per_se[i_n], 3), nsmall=3), ")")
      )
    )

    # Now do same occ/ind
    for (var in vars_o_t){
      var_n <- str_c("same_", var, "_per")
      se_n <- str_c("same_", var, "_per_se")
      temp <- rbind(temp,
                    cbind(
                      rbind(
                        str_c(" & ", format(round(job_transitions_sum[[var_n]][i_p], 3), nsmall=3),
                               p_test_1(job_transitions_sum, var_n, "n", i_p)),
                        str_c(" & (", format(round(job_transitions_sum[[se_n]][i_p], 3), nsmall=3), ")")
                      ),
                      rbind(str_c(" &  {--} "), str_c(" & ")),
                      rbind(
                        str_c(" & ", format(round(job_transitions_sum[[var_n]][i_n], 3), nsmall=3),
                               p_test_1(job_transitions_sum, var_n, "n", i_n)),
                        str_c(" & (", format(round(job_transitions_sum[[se_n]][i_n], 3), nsmall=3), ")")
                      )
                    )
      )
    }

    # Now for everything else
    for (var in vars_t){
      if (var %in% vars_p_t){
        var_n = str_c(var, "_per")
        se_n = str_c(var, "_per_se")
        p_star <- p_test(job_transitions_sum, var_n, "n", i, i_p)
        n_star <- p_test(job_transitions_sum, var_n, "n", i, i_n)
      } else{
        var_n = str_c(var, "_mean")
        se_n = str_c(var, "_mean_se")
        p_star <- t_test(job_transitions_sum, var_n, "n", i, i_p)
        n_star <- t_test(job_transitions_sum, var_n, "n", i, i_n)
      }
      temp <- rbind(temp,
                    cbind(
                      rbind(
                        str_c(" & ", format(round(job_transitions_sum[[var_n]][i_p], 3), nsmall=3),
                               p_star),
                        str_c(" & (", format(round(job_transitions_sum[[se_n]][i_p], 3), nsmall=3), ")")
                      ),
                      rbind(
                        str_c(" & ", format(round(job_transitions_sum[[var_n]][i], 3), nsmall=3)),
                        str_c(" & (", format(round(job_transitions_sum[[se_n]][i], 3), nsmall=3), ")")
                      ),
                      rbind(
                        str_c(" & ", format(round(job_transitions_sum[[var_n]][i_n], 3), nsmall=3),
                               n_star),
                        str_c(" & (", format(round(job_transitions_sum[[se_n]][i_n], 3), nsmall=3), ")")
                      )
                    )
      )
    }

    temp <- rbind(
      temp,
      cbind(" & ",
            str_c(" & {", format(job_transitions_sum$n[i], big.mark = ",", trim = T), "}"), " & "))

    if (out == 0){
      center_t <- cbind(center_t, temp)
    } else{
      center_t <- cbind(
          center_t, temp,
          rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\"))
    }

  }
  
  bot_t <- str_c("\\bottomrule
\\end{tabular}
\\caption{Job statistics for ", sex_name, 
" at current, previous, and next job for workers
who are currently outsourced compared to those who are not. Observations are at the
person-job level and summary statistics are weighted at the person level. 
Stars represent significant difference from
current job (except for outsourced, same occupation, and same industry, which represent
significant difference from 0) at the .10 level *, .05 level **, and .01 level ***.}
\\label{jobs_t}
\\end{table}
\\end{document}"
  )
  
  # Do weird stuff to create LaTeX output
  t_folder <- str_c(table_folder, "Junk/")
  file_5 <- str_c(t_folder, "center_t.txt")
  file_6 <- str_c(t_folder, "center_t.txt")
  write.table(center_t, file_5, quote=T, col.names=F, row.names=F)
  center_t <- read.table(file_5, sep = "")
  write.table(center_t, file_6, quote=F, col.names=F, row.names=F, sep = "")
  center_t <- readChar(file_6, nchars = 1e6)
  
  write.table(str_c(top_t, center_t, bot_t),
              str_c(table_folder, 
                     "NLSY79 Job Transitions Rough/NLSY79 Job Transitions Rough",
                     Sex_name, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
}

###################################################################################

# Many people are missing union. Do these people/jobs look any different

union_y <- matched %>%  
  as_survey_design(ids = case_id, strata=sample_id, weights=weight) %>% 
  mutate(union_ = factor(union_),
         outsourced = factor(outsourced)) %>% 
  group_by(female, union_, outsourced) %>% 
  summarise(
    log_real_hrly_wage_mean = survey_mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    tenure_mean = survey_mean(tenure, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    wks_work_prev_mean = survey_mean(wks_work_prev, na.rm = T),
    any_benefits_per = survey_mean(any_benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    retirement_per = survey_mean(retirement, na.rm = T),
    childcare_per = survey_mean(childcare, na.rm = T),
    dental_per = survey_mean(dental, na.rm = T),
    flex_sched_per = survey_mean(flex_sched, na.rm = T),
    life_per = survey_mean(life, na.rm = T),
    maternity_per = survey_mean(maternity, na.rm = T),
    profit_share_per = survey_mean(profit_share, na.rm = T),
    train_school_per = survey_mean(train_school, na.rm = T),
    tot_child_mean = survey_mean(tot_child, na.rm = T),
    hh_child_mean = survey_mean(hh_child, na.rm = T),
    n = unweighted(n())
  ) 

union_j <- matched_jobs %>%  
  as_survey_design(ids = case_id, strata=sample_id, weights=weight) %>% 
  mutate(union_ = factor(union_),
         outsourced = factor(outsourced)) %>% 
  group_by(female, union_, outsourced) %>% 
  summarise(
    log_real_hrly_wage_mean = survey_mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    tenure_mean = survey_mean(tenure, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    wks_work_prev_mean = survey_mean(wks_work_prev, na.rm = T),
    any_benefits_per = survey_mean(any_benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    retirement_per = survey_mean(retirement, na.rm = T),
    childcare_per = survey_mean(childcare, na.rm = T),
    dental_per = survey_mean(dental, na.rm = T),
    flex_sched_per = survey_mean(flex_sched, na.rm = T),
    life_per = survey_mean(life, na.rm = T),
    maternity_per = survey_mean(maternity, na.rm = T),
    profit_share_per = survey_mean(profit_share, na.rm = T),
    train_school_per = survey_mean(train_school, na.rm = T),
    single_per = survey_mean(single, na.rm = T),
    married_per = survey_mean(married, na.rm = T),
    tot_child_mean = survey_mean(tot_child, na.rm = T),
    hh_child_mean = survey_mean(hh_child, na.rm = T),
    n = unweighted(n())
  ) 

#######################################################################################

# From Dube and Kaplan (2010), look at janitors and security guards
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

# Compare outsourced vs not for janitors and security guards based on
# self-reported outsourced and Dube and Kaplan"s measure. See
# If they are significantly different

# Janitors
# Dube Kaplan
janitor_dk_sum <- janitor %>%  
  as_survey_design(ids = case_id, weights=weight) %>% 
  mutate(outsourced = factor(outsourced_2)) %>% 
  group_by(outsourced) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_hrly_wage_mean = survey_mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    any_benefits_per = survey_mean(any_benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    female_per = survey_mean(female, na.rm = T),
    black_per = survey_mean(black, na.rm = T),
    hispanic_per = survey_mean(hispanic, na.rm = T),
    less_hs_per = survey_mean(less_hs, na.rm = T),
    hs_per = survey_mean(hs, na.rm = T),
    aa_per = survey_mean(aa, na.rm = T),
    ba_per = survey_mean(ba, na.rm = T),
    plus_ba_per = survey_mean(plus_ba, na.rm = T),
    age_mean = survey_mean(age, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(desc(outsourced)) 

# Self reported (add DK to end for table)
janitor_sr_sum <- janitor %>% 
  as_survey_design(ids = case_id, weights=weight) %>% 
  mutate(outsourced = factor(outsourced)) %>% 
  group_by(outsourced) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_hrly_wage_mean = survey_mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    any_benefits_per = survey_mean(any_benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    female_per = survey_mean(female, na.rm = T),
    black_per = survey_mean(black, na.rm = T),
    hispanic_per = survey_mean(hispanic, na.rm = T),
    less_hs_per = survey_mean(less_hs, na.rm = T),
    hs_per = survey_mean(hs, na.rm = T),
    aa_per = survey_mean(aa, na.rm = T),
    ba_per = survey_mean(ba, na.rm = T),
    plus_ba_per = survey_mean(plus_ba, na.rm = T),
    age_mean = survey_mean(age, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(desc(outsourced)) %>% 
  bind_rows(janitor_dk_sum)

# Security Guards
# Dube Kaplan
sg_dk_sum <- sg %>%  
  as_survey_design(ids = case_id, weights=weight) %>% 
  mutate(outsourced = factor(outsourced_2)) %>% 
  group_by(outsourced) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_hrly_wage_mean = survey_mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    any_benefits_per = survey_mean(any_benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    female_per = survey_mean(female, na.rm = T),
    black_per = survey_mean(black, na.rm = T),
    hispanic_per = survey_mean(hispanic, na.rm = T),
    less_hs_per = survey_mean(less_hs, na.rm = T),
    hs_per = survey_mean(hs, na.rm = T),
    aa_per = survey_mean(aa, na.rm = T),
    ba_per = survey_mean(ba, na.rm = T),
    plus_ba_per = survey_mean(plus_ba, na.rm = T),
    age_mean = survey_mean(age, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(desc(outsourced)) 

# Self reported (add DK to end for table)
sg_sr_sum <- sg %>% 
  as_survey_design(ids = case_id, weights=weight) %>% 
  mutate(outsourced = factor(outsourced)) %>% 
  group_by(outsourced) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_hrly_wage_mean = survey_mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    any_benefits_per = survey_mean(any_benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    female_per = survey_mean(female, na.rm = T),
    black_per = survey_mean(black, na.rm = T),
    hispanic_per = survey_mean(hispanic, na.rm = T),
    less_hs_per = survey_mean(less_hs, na.rm = T),
    hs_per = survey_mean(hs, na.rm = T),
    aa_per = survey_mean(aa, na.rm = T),
    ba_per = survey_mean(ba, na.rm = T),
    plus_ba_per = survey_mean(plus_ba, na.rm = T),
    age_mean = survey_mean(age, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(desc(outsourced)) %>% 
  bind_rows(sg_dk_sum)

top_js <- str_c(table_top, siunitx, "
\\begin{tabular}{lSSSS}
\\toprule
& \\multicolumn{2}{c}{{Self-Reported (This Paper)}} & 
\\multicolumn{2}{c}{{Industry-Occupation (Dube and Kaplan)}} \\\\
Variable & {Outsourced} & {Not Outsourced} & {Outsourced} & {Not Outsourced} \\\\ \\midrule
"
)

bot_jan <- "\\bottomrule
\\end{tabular}
\\caption{Summary statistics for janitors (occupation 4220) that are outsourced vs
not outsourced. In the left two columns, outsourced is self-reported by the worker.
In the right two, it is infered if the worker is in industry 7690. Observations are at the
worker-job-year level and summary statistics are weighted. 
Stars represent significant difference from outsourced (the second and fourth column)
or from self-reported outsourced (the third column) at the .10 level *,
.05 level **, and .01 level ***.}
\\label{demo}
\\end{table}
\\end{document}"

bot_sg <- "\\bottomrule
\\end{tabular}
\\caption{Summary statistics for security guards (occupation 3920) that are outsourced
vs not outsourced out. In the left two columns, outsourced is self-reported by the worker.
In the right two, it is infered if the worker is in industry 7680. Observations are at the
worker-job-year level and summary statistics are weighted. 
Stars represent significant difference from outsourced (the second and fourth column)
or from self-reported outsourced (the third column) at the .10 level *,
.05 level **, and .01 level ***.}
\\label{demo}
\\end{table}
\\end{document}"

# Create table in Latex
vars_js <- c("log_real_hrly_wage", "log_real_wkly_wage", "any_benefits", "health",
             "hours_week", 
             "part_time", "union", "job_sat", "less_hs", "hs", "aa", "ba", "plus_ba",
             "black", "hispanic", "female", "age")

vars_js_p <- c("part_time", "black", "hispanic", "less_hs", "hs", "aa", "ba", "plus_ba", 
               "female", "any_benefits", "health", "union")

vars_js_m <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "job_sat", "age")

for (count in c(1, 2)){
  
  if (count == 1){
    data_sum <- janitor_sr_sum
    bot_js <- bot_jan
    occ <- "Janitors"
  } else {
    data_sum <- sg_sr_sum
    bot_js <- bot_sg
    occ <- "Security Guards" 
  }
  
  center_js <- rbind("Log Real", "Hourly Wage", "Log Real", "Weekly Wage",
                     "Any Benefits", "", "Health Insurance", "", "Hours Worked",
                     "per Week", "Part Time", "",
                     "Union", "", "Job Satisfaction", "(Lower Better)", "No HS Diploma",
                     "", "HS Diploma", "","AA Degree", "", "BA Degree", "",
                     "Post Graduate", "Degree", "Black", "",
                     "Hispanic", "", "Female", "", "Age", "", "Observations")
  
  for (i in seq(1, 4, by = 1)){
    j <- 1 + 2 * (i == 4) 
    col_i <- c()
    for (var in vars_js){
      if (var %in% vars_js_p){
        var_n = str_c(var, "_per")
        se_n = str_c(var, "_per_se")
        if (i != j){
          stars <- p_test(data_sum, var_n, "n", i, j)
        } else {
          stars <- ""
        }
      } else{
        var_n = str_c(var, "_mean")
        se_n = str_c(var, "_mean_se")
        if (i != j){
          stars <- t_test(data_sum, var_n, "n", i, j)
        } else {
          stars <- ""
        }
        
      }
      col_i <- rbind(col_i,
                     str_c(" & ", format(round(data_sum[[var_n]][i], 3), nsmall=3),
                            stars),
                     str_c(" & (", format(round(data_sum[[se_n]][i], 3), nsmall=3), ")"))
    }
    col_i <- rbind(col_i, 
                   str_c(" & {", format(data_sum$n[i], big.mark = ",", trim = T), "}"))
    center_js <- cbind(center_js, col_i)
  }
  
  center_js <- cbind(center_js, 
                     rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                           "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                           "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                           "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                           "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", 
                           "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\"))
  
  # Do weird stuff to create LaTeX output
  j_folder <- str_c(table_folder, "Junk/")
  file_7 <- str_c(j_folder, "center_jan.txt")
  file_8 <- str_c(j_folder, "center_jan.txt")
  write.table(center_js, file_7, quote=T, col.names=F, row.names=F)
  center_js <- read.table(file_7, sep = "")
  write.table(center_js, file_8, quote=F, col.names=F, row.names=F, sep = "")
  center_js <- readChar(file_8, nchars = 1e6)
  
  
  write.table(str_c(top_js, center_js, bot_js),
              str_c(table_folder, "NLSY79 ", occ,  "/NLSY79 ", occ, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
}

# What are the self-reported job types of DK"s outsourced?
# Use data.frame"s SQL like capabilities to make these tables
top_t <- str_c(table_top, "
\\begin{tabular}{lrr|r}
\\toprule
& \\multicolumn{3}{c} {Industry-Occupation (Dube and Kaplan)} \\\\
Self-Reported & Outsourced & Not Outsourced & Total \\\\ \\midrule
"
)

bot_t_j <- "\\bottomrule
\\end{tabular}
\\caption{Counts of Dube and Kaplan (2010) method of measuring outsourcing versus
NLSY 79 self-reported job type for janitors (occupation 4220). 
For columns, workers are consider outsourced if they are in industry 7690. 
For rows, I use the worker's self-reported job type.}
\\label{dk_types}
\\end{table}
\\end{document}"

bot_t_sg <- "\\bottomrule
\\end{tabular}
\\caption{Counts of Dube and Kaplan (2010) method of measuring outsourcing versus
NLSY 79 self-reported job type for security guards (occupation 3920).
For columns, workers are consider outsourced if they are in industry 7680. 
For rows, I use the worker's self-reported job type.}
\\label{dk_types}
\\end{table}
\\end{document}"

types <- c("outsourced", "indep_con", "temp_work", "on_call", "self_emp", "traditional")

for (d_i in c(1, 2)){
  
  if (d_i == 1){
    data <- data.table(janitor)
    occ <- "Janitors"
    bot_t <- bot_t_j
  } else{
    data <- data.table(sg)
    occ <- "Security Guards"
    bot_t <- bot_t_sg
  }
  
  center_t <- c("Outsourced", "Independent Contractor", "Temp Worker",
                "On-Call Worker", "Self-Employed", "Traditional Employee", "Total" )
  
  col_i <- c()
  
  for (type in types){
    # Some observations may not occur, set to 0
    a <- tryCatch(data[outsourced_2 == 1 & data[[type]] == 1, .N], 
                  error = function(e) {a <- 0})
    b <- tryCatch(data[outsourced_2 == 0 & data[[type]] == 1, .N],
                  error = function(e) {b <- 0})
    b <- b * !is.na(b) + 0  
    col_i <- rbind(col_i, cbind(str_c(" & ", a),
                                str_c(" & ", b),
                                str_c(" & ", a + b)))
  }
  
  col_i <- rbind(col_i, 
                 cbind(str_c(" & ", data[outsourced_2 == 1, .N]),
                       str_c(" & ", data[outsourced_2 == 0, .N]),
                       str_c(" & ", NROW(data))))
  
  center_t <- cbind(center_t, col_i)
  
  center_t <- cbind(center_t, 
                    rbind("\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]",
                          "\\\\[2pt]", "\\\\[2pt] \\midrule", "\\\\"))
  
  # Do weird stuff to create LaTeX output
  j_folder <- str_c(table_folder, "Junk/")
  file_9 <- str_c(j_folder, "center_t.txt")
  file_10 <- str_c(j_folder, "center_t.txt")
  write.table(center_t, file_9, quote=T, col.names=F, row.names=F)
  center_t <- read.table(file_9, sep = "")
  write.table(center_t, file_10, quote=F, col.names=F, row.names=F, sep = "")
  center_t <- readChar(file_10, nchars = 1e6)
  
  
  write.table(str_c(top_t, center_t, bot_t),
              str_c(table_folder, "NLSY79 ", occ,  "/NLSY79 ", occ, " Types.tex"),
              quote=F, col.names=F, row.names=F, sep="")
}

#####################################################################

# From Katz and Krueger (2019), look at workers by job types (Table 2)

job_type_sum <- matched_jobs %>% 
  mutate(alternative = indep_con + temp_work + on_call + outsourced) %>% 
  as_survey_design(ids = case_id, weights=weight) %>% 
  summarise(
    alternative_per = survey_mean(alternative),
    indep_con_per = survey_mean(indep_con),
    temp_work_per = survey_mean(temp_work),
    on_call_per = survey_mean(on_call),
    outsourced_per = survey_mean(outsourced),
    self_emp_per = survey_mean(self_emp),
    traditional_per = survey_mean(traditional),
    n = unweighted(n())
  )

job_type_sum_gender <- matched_jobs %>% 
  as_survey_design(ids = case_id, weights=weight) %>%
  group_by(female) %>% 
  summarise(
    indep_con_per = survey_mean(indep_con),
    temp_work_per = survey_mean(temp_work),
    on_call_per = survey_mean(on_call),
    outsourced_per = survey_mean(outsourced),
    self_emp_per = survey_mean(self_emp),
    traditional_per = survey_mean(traditional),
    n = unweighted(n())
  )

vars_type <- c("outsourced", "indep_con", "temp_work", "on_call", 
               "self_emp", "traditional")

name_type <- c("Outsourced", "Self-Employed", "Independent Contractor",
              "Temp Worker", "On-Call Worker", "Traditional Employee", "Total" )

top_type <- str_c(table_top, "
\\begin{tabular}{lrrr}
\\toprule
Job Type & Men & Women & Total \\\\ \\midrule
")

for (i in seq(1, length(vars_type))){
  var <- str_c(vars_type[i], "_per")
  top_type <- str_c(
    top_type,
    name_type[i], " & ",
    format(round(100 * job_type_sum_gender[[var]][1], 2), nsmall=2), " & ",
    format(round(100 * job_type_sum_gender[[var]][2], 2), nsmall=2), " & ",
    format(round(100 * job_type_sum[[var]], 2), nsmall=2), " \\\\ \n")
  
}

top_type <- str_c(top_type, 
                   " \\midrule \n Total", " & ",
                   format(job_type_sum_gender$n[1], big.mark = "," ), " & ",
                   format(job_type_sum_gender$n[2], big.mark = "," ), " & ",
                   format(job_type_sum$n, big.mark = "," ), " \\\\ \n ")

bot_type <- "\\bottomrule
\\end{tabular}
\\caption{Percent of jobs in each job type for men, women, and overall. 
Observations weighted at the person level.}
\\label{oj_types}
\\end{table}
\\end{document}"

write.table(str_c(top_type, bot_type),
            str_c(table_folder, "NLSY79 Job Types/NLSY79 Job Types.tex"),
            quote=F, col.names=F, row.names=F, sep="")


######################################################################

# Run regressions

# Create a loop that takes various desired outcomes, runs series of regressions,
# and creates table in Latex
# For men and women
# 1. Basic Controls (age, age_2, age_3, age_4, black, hispanic, education,
# union, region, msa/cc, marital status, children, hours week / part time, year)
# 2. Add tenure quartic
# 3. Add Ind factors
# 4. Add 2+3
# 5. Individual FE + some controls (age, age_2, age_3, union, region, msa/cc,
# marital stutus, children, hours week / part time, year)
# 6. Add tenure
# 7. Add Ind factors
# 8. Add 6+7

top_r <- str_c(table_top, siunitx, "
\\begin{tabular}{lSccccS}
\\toprule
Model & {Outsourced} & Tenure  & Occupation  & Individual & Obs & {$R^2$}  \\\\
&  & Quartic & FE &  FE & &   \\\\\\midrule
"
)

var_r <- c("log_real_hrly_wage", "log_real_wkly_wage", "job_sat",
           "any_benefits", "health")

var_names <- c("log real hourly wages", "log real weekly wages", 
               "job satisfaction (lower is better)", "receiving any employment benefits",
               "receiving health insurance through employer")

sex_names <- c("men", "women")

types <- c("outsourced", "self_emp", "indep_con", "temp_work", "on_call")

controls <- c("age", "age_2", "age_3", "age_4", "tot_child", "hh_child", "factor(union_)")

hours <- c("hours_week", "part_time")

ols_controls <- c("black", "hispanic", "hs", "aa", "ba", "plus_ba")

tenure <- c("tenure", "tenure_2", "tenure_3", "tenure_4")

for (sex in c(0, 1)){
  for (ind in seq(1, length(var_r))){
    var <- var_r[ind]
    var_name <- var_names[ind]
    sex_name <- sex_names[sex + 1]
    
    center_r <- rbind("OLS", "Basic", "", "Tenure Quartic", "", "Occ FE", "",
                      "Tenure + Occ", "", "FE", "Basic", "", "Tenure Quartic", "",
                      "Occ FE", "", "Tenure + Occ", "")
    
    center_r_m <- rbind("OLS", "Basic", "", "Tenure Quartic", "", "Occ FE", "",
                        "Tenure + Occ", "", "FE", "Basic", "", "Tenure Quartic", "",
                        "Occ FE", "", "Tenure + Occ", "")
    
    c_r <- cbind("& ", "& ", "& ", "& ", "& ", "& ")
    c_r_m <- cbind("& ", "& ", "& ", "& ", "& ", "& ")
    
    for (reg_ind in seq(1, 8)){
      
      ind_vars <- str_c(str_c(types, collapse = "+"),
                        str_c(controls, collapse = "+"), sep="+")
      
      hours_text <- "" 
      if (var != "log_real_wkly_wage"){
        ind_vars <- str_c(ind_vars, str_c(hours, collapse = "+"), sep="+")
        hours_text <- " hours worked per week, part-time status,"
      }
      
      if (reg_ind <= 4){
        ife_ind <- "No"
        ind_vars <- str_c(ind_vars, str_c(ols_controls, collapse = "+"), sep="+")
      } else{
        ife_ind <- "Yes"
      }
      
      if (reg_ind %in% c(2, 4, 6, 8)){
        ten_ind <- "Yes"
        ind_vars <- str_c(ind_vars, str_c(tenure, collapse = "+"), sep="+")
      } else{
        ten_ind <- "No"
      }
      
      if (reg_ind %in% c(3, 4, 7, 8)){
        oi_ind <- "Yes"
      } else{
        oi_ind <- "No"
      }
      
      if (reg_ind == 5){
        c_r <- rbind(c_r, cbind("& ", "& ", "& ", "& ", "& ", "& "))
        c_r_m <- rbind(c_r_m, cbind("& ", "& ", "& ", "& ", "& ", "& "))
      }
      
      eq_t <- formula(str_c(var, ind_vars, sep = "~"))
      
      # Have to do this because eq for fixed effects not behaving
      if (reg_ind %in% c(1, 2)){
        temp <- lm_robust(eq_t, data = matched, subset = (female == sex),
                          weights = weight,
                          fixed_effects = ~ year + region + marital_status + msa,
                          clusters = sample_id, se_type = "stata", try_cholesky = T)
        
        temp_m <- lm_robust(eq_t, data = matched_jobs, subset = (female == sex),
                          weights = weight,
                          fixed_effects = ~ region + marital_status + msa,
                          clusters = sample_id, se_type = "stata", try_cholesky = T)
      } else if (reg_ind %in% c(3, 4)){
        temp <- lm_robust(eq_t, data = matched, subset = (female == sex),
                          weights = weight,
                          fixed_effects = ~ year + region + marital_status + msa + occ,
                          clusters = sample_id, se_type = "stata", try_cholesky = T)
        
        temp_m <- lm_robust(eq_t, data = matched_jobs, subset = (female == sex),
                          weights = weight,
                          fixed_effects = ~ region + marital_status + msa + occ,
                          clusters = sample_id, se_type = "stata", try_cholesky = T)
      } else if (reg_ind %in% c(5, 6)){
        temp <- lm_robust(eq_t, data = matched, subset = (female == sex),
                          weights = weight,
                          fixed_effects = ~ year + region + marital_status + msa + case_id,
                          clusters = sample_id, se_type = "stata", try_cholesky = T)
        
        temp_m <- lm_robust(eq_t, data = matched_jobs, subset = (female == sex),
                          weights = weight,
                          fixed_effects = ~ region + marital_status + msa + case_id,
                          clusters = sample_id, se_type = "stata", try_cholesky = T)
      }else if (reg_ind %in% c(7, 8)){
        temp <- lm_robust(eq_t, data = matched, subset = (female == sex),
                          weights = weight,
                          fixed_effects = ~ year + region + marital_status + msa
                          + case_id + occ,
                          clusters = sample_id, se_type = "stata", try_cholesky = T)
        
        temp_m <- lm_robust(eq_t, data = matched_jobs, subset = (female == sex),
                          weights = weight,
                          fixed_effects = ~ region + marital_status + msa
                          + case_id + occ,
                          clusters = sample_id, se_type = "stata", try_cholesky = T)
      }
      
      stars <- p_stars(temp$p.value["outsourced"])
      stars_m <- p_stars(temp_m$p.value["outsourced"])
      
      c_r <- rbind(c_r,
                   cbind(
                     str_c(" & ", format(round(temp$coefficients["outsourced"], 3),
                                          nsmall=3),
                            stars),
                     str_c(" & ", ten_ind),
                     str_c(" & ", oi_ind),
                     str_c(" & ", ife_ind),
                     str_c(" & ", format(temp$N, big.mark = ",", trim = T)),
                     str_c(" & ", format(round(temp$r.squared, 2), nsmall=2))
                   ),
                   cbind(
                     str_c(" & (", format(round(temp$std.error["outsourced"], 3),
                                           nsmall=3), ")"),
                     " & ", " & ", " & ", " & ", " & "
                   )
      )
      
      c_r_m <- rbind(c_r_m,
                    cbind(
                      str_c(" & ", format(round(temp_m$coefficients["outsourced"], 3),
                                          nsmall=3),
                            stars_m),
                      str_c(" & ", ten_ind),
                      str_c(" & ", oi_ind),
                      str_c(" & ", ife_ind),
                      str_c(" & ", format(temp_m$N, big.mark = ",", trim = T)),
                      str_c(" & ", format(round(temp_m$r.squared, 2), nsmall=2))
                    ),
                    cbind(
                      str_c(" & (", format(round(temp_m$std.error["outsourced"], 3),
                                           nsmall=3), ")"),
                      " & ", " & ", " & ", " & ", " & "
                    )
      )
    }
    
    center_r <- cbind(center_r, c_r,
                      rbind("\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt] \\midrule",
                            "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", 
                            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]"))
    
    center_r_m <- cbind(center_r_m, c_r_m,
                      rbind("\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt] \\midrule",
                            "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", 
                            "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]"))
    
    # Do weird stuff to create LaTeX output
    r_folder <- str_c(table_folder, "Junk/")
    file_11 <- str_c(r_folder, "center_r.txt")
    file_12 <- str_c(r_folder, "center_r.txt")
    write.table(center_r, file_11, quote=T, col.names=F, row.names=F)
    center_r <- read.table(file_11, sep = "")
    write.table(center_r, file_12, quote=F, col.names=F, row.names=F, sep = "")
    center_r <- readChar(file_12, nchars = 1e6)
    
    
    file_11_m <- str_c(r_folder, "center_r_m.txt")
    file_12_m <- str_c(r_folder, "center_r_m.txt")
    write.table(center_r_m, file_11_m, quote=T, col.names=F, row.names=F)
    center_r_m <- read.table(file_11_m, sep = "")
    write.table(center_r_m, file_12_m, quote=F, col.names=F, row.names=F, sep = "")
    center_r_m <- readChar(file_12_m, nchars = 1e6)
    
    bot_r <- str_c(
      "\\bottomrule
      \\end{tabular}
      \\caption{Regressions of outsourced on ", var_name, " for ", sex_name,
      ". All regressions
include controls for a quartic in age, union status,", hours_text,
      " dummies for region, whether in an MSA or central city,
marital status, number of children total and in household, and dummies for
observation year. The first four columns run OLS and also contain controls for
race and education. The last four columns use worker fixed effects.
All observations are at the person-job-year level and all standard errors are
clustered by occupation category.
Stars represent significant at the .10 level *, .05 level **, and .01 level ***.}
\\label{reg_" , var, "_", sex_name, "}
\\end{table}
\\end{document}"
    )
    
    bot_r_m <- str_c(
"\\bottomrule
\\end{tabular}
\\caption{Regressions of outsourced on ", var_name, " for ", sex_name,
". All regressions
include controls for a quartic in age, union status,", hours_text,
" dummies for region, whether in an MSA or central city,
marital status, number of children total and in household, and dummies for
observation year. The first four columns run OLS and also contain controls for
race and education. The last four columns use worker fixed effects.
All observations are at the person-job level and all standard errors are
clustered by occupation category.
Stars represent significant at the .10 level *, .05 level **, and .01 level ***.}
\\label{reg_" , var, "_", sex_name, "}
\\end{table}
\\end{document}"
    )
    
    
    write.table(str_c(top_r, center_r, bot_r),
                str_c(table_folder, "NLSY79 Regressions/Years/NLSY79 ",
                       var, "_", sex_name, ".tex"),
                quote=F, col.names=F, row.names=F, sep="")
    
    write.table(str_c(top_r, center_r_m, bot_r_m),
                str_c(table_folder, "NLSY79 Regressions/Jobs/NLSY79 ",
                      var, "_", sex_name, ".tex"),
                quote=F, col.names=F, row.names=F, sep="")
    
      }
  }

###############################################################################

# # Use ggplot 2 to graph some things
# Graph some variables by year for outsourced vs not for men and women
# Both all workers and ever outsourced

# Also plot log_real_wage/log_week_earn residuals 
# from full FE regression about (but without outsourcing)
# See how these compare for outsourced vs non-outsourced

var_g <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "tenure")

var_names <- c("Log Real Hourly Wage", "Log Real Weekly Wage", 
               "Hours Worked per Week", "Weeks of Tenure")

# sex same as above
sex_names <- c("Men's ", "Women's ")
sex_save <- c("men", "women")

# Bring down types for a regression, but don"t want outsourced in there
types <- c("self_emp", "indep_con", "temp_work", "on_call")

for (sex in c(0, 1)){
  for (i in seq(1, length(var_g))){
    
    temp <- matched %>%
      filter(female == sex, !is.na(.[[var_g[i]]])) %>%
      ggplot(aes_string(var_g[i], fill = "factor(outsourced)")) +
      geom_density(alpha = 0.2) +
      labs(x = var_names[i], y = "Density") +
      scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                        values = c("blue", "red"),
                        labels = c("Not Outsourced", "Outsourced")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
      theme_light(base_size = 16) 
    
    ggsave(str_c(figure_folder, var_g[i], "_", sex_save[sex + 1], "_", "all.pdf"),
           height = height, width = width)
    
    temp <- matched %>%
      filter(female == sex, ever_out == 1, !is.na(.[[var_g[i]]])) %>%
      ggplot(aes_string(var_g[i], fill = "factor(outsourced)")) +
      geom_density(alpha = 0.2) +
      labs(x = var_names[i], y = "Density") +
      scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                        values = c("blue", "red"),
                        labels = c("Not Outsourced", "Outsourced")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
      theme_light(base_size = 16) 
    
    ggsave(str_c(figure_folder, var_g[i], "_", sex_save[sex + 1], "_", "ever.pdf"),
           height = height, width = width)
    
    temp <- matched %>%
      filter(female == sex, ever_out_occ, !is.na(.[[var_g[i]]])) %>%
      ggplot(aes_string(var_g[i], fill = "factor(outsourced)")) +
      geom_density(alpha = 0.2) +
      labs(x = var_names[i], y = "Density") +
      scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                        values = c("blue", "red"),
                        labels = c("Not Outsourced", "Outsourced")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
      theme_light(base_size = 16) 
    
    ggsave(str_c(figure_folder, var_g[i], "_", sex_save[sex + 1], "_", "ever_out_occ.pdf"),
           height = height, width = width)
  }
  
  for (i in c(1, 2)){
    
    ind_vars <- str_c(controls, collapse = "+")
    ind_vars <- str_c(ind_vars, str_c(tenure, collapse = "+"), sep="+")
    ind_vars <- str_c(ind_vars, str_c(types, collapse = "+"), sep="+")
    if (var_r[i] != "log_week_earn"){
      ind_vars <- str_c(ind_vars, str_c(hours, collapse = "+"), sep="+")
    }
    
    eq_t <- formula(str_c(var_g[i], ind_vars, sep = "~"))
    
    reg <- lm_robust(eq_t, data = matched, subset = female == sex,
                     weights = weight,
                     fixed_effects = ~ year + region + marital_status + msa
                     + case_id + occ,
                     clusters = sample_id, se_type = "stata", try_cholesky = T)
    
    # Only look at outsourced vs traditional
    temp_df <- matched %>% 
      filter(
        female == sex, !is.na(.[[var_g[i]]]), !is.na(occ), !is.na(region), 
        !is.na(msa), !is.na(union_), !is.na(marital_status),
        !is.na(tenure), !is.na(hh_child), !is.na(tot_child), !is.na(hours_week), 
        !is.na(part_time)
      ) %>% 
      mutate(resid = .[[var_g[i]]] - reg$fitted.values) %>% 
      filter(resid >= - 3 * sd(resid) & resid <= 3 * sd(resid),
             outsourced == 1 | traditional == 1)
    
    print(str_c(sex_names[sex + 1], var_names[i]))
    print(table(
      temp_df$resid[matched$traditional == 1 & matched$ever_out == 1] < 
        mean(temp_df$resid[matched$traditional == 1 & matched$ever_out == 1],
               na.rm = T)))
    print(str_c("Traditional standard deviation = ", 
          sd(temp_df$resid[matched$traditional == 1 & matched$ever_out == 1],
             na.rm = T)))
    print(str_c("Outsourced standard deviation = ", 
          sd(temp_df$resid[matched$outsourced == 1 & matched$ever_out == 1],
             na.rm = T)))
    
    temp <- temp_df %>%
      ggplot(aes(resid, fill = factor(outsourced))) +
      geom_density(alpha = 0.2) +
      labs(x = str_c("Residual ", var_names[i]), y = "Density") +
      scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                        values = c("blue", "red"),
                        labels = c("Traditional", "Outsourced")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
      theme_light(base_size = 16) 
    
    ggsave(str_c(figure_folder, "resid_", var_g[i], "_",
                  sex_save[sex + 1], "_", "all.pdf"),
           height = height, width = width)
    
    temp <- temp_df %>%
      filter(ever_out == 1) %>%
      ggplot(aes(resid, fill = factor(outsourced)))  +
      geom_density(alpha = 0.2) +
      labs(x = str_c("Residual ", var_names[i]), y = "Density") +
      scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                        values = c("blue", "red"),
                        labels = c("Traditional", "Outsourced")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
      theme_light(base_size = 16) 
    
    ggsave(str_c(figure_folder, "resid_", var_g[i], "_",
                  sex_save[sex + 1], "_", "ever.pdf"),
           height = height, width = width)
    
    temp <- temp_df %>%
      filter(ever_out_occ) %>%
      ggplot(aes(resid, fill = factor(outsourced)))  +
      geom_density(alpha = 0.2) +
      labs(x = str_c("Residual ", var_names[i]), y = "Density") +
      scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                        values = c("blue", "red"),
                        labels = c("Traditional", "Outsourced")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
      theme_light(base_size = 16) 
    
    ggsave(str_c(figure_folder, "resid_", var_g[i], "_",
                 sex_save[sex + 1], "_", "ever_out_occ.pdf"),
           height = height, width = width)
  }
  
  temp <- matched %>%
    filter(female == sex, !is.na(job_sat)) %>%
    ggplot(aes(round(job_sat, 0), fill = factor(outsourced))) +
    geom_histogram(aes(y=.5*..density..), alpha=0.5, position="dodge",
                   binwidth = .5) +
    labs(x = "Job Satisfaction", y = "Density") +
    scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = c("Not Outsourced", "Outsourced")) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "job_sat_", sex_save[sex + 1], "_", "all.pdf"),
         height = height, width = width)
  
  temp <- matched %>%
    filter(female == sex, ever_out == 1, !is.na(job_sat)) %>%
    ggplot(aes(round(job_sat, 0), fill = factor(outsourced))) +
    geom_histogram(aes(y=.5*..density..), alpha=0.7, position="dodge",
                   binwidth = .5) +
    labs(x = "Job Satisfaction", y = "Density") +
    scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = c("Not Outsourced", "Outsourced")) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "job_sat_", sex_save[sex + 1], "_", "ever.pdf"),
         height = height, width = width)
  
  temp <- matched %>%
    filter(female == sex, ever_out_occ == 1, !is.na(job_sat)) %>%
    ggplot(aes(round(job_sat, 0), fill = factor(outsourced))) +
    geom_histogram(aes(y=.5*..density..), alpha=0.7, position="dodge",
                   binwidth = .5) +
    labs(x = "Job Satisfaction", y = "Density") +
    scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = c("Not Outsourced", "Outsourced")) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "job_sat_", sex_save[sex + 1], "_", "ever_out_occ.pdf"),
         height = height, width = width)
}

# Plot a the number of outsourced jobs by interview (if age > 36, where
# most of my data lives)
# Do this for year and age
# Plot workers in highly outsoucred occupations over time
# and percent outsourced in highly outsourced occupations
# For these groups, plot wages / median wages
# Also do a plot with all alternative arrangements over time

vars_time <- c("year", "age")
vars_name <- c("Year", "Age")
age_min <- 38
age_max <- 59

for (i in seq(1, length(vars_time))){
  temp <- matched %>% 
    filter(age >= age_min, age <= age_max) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per"), color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp"),
              linetype="dashed", color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low"), 
              linetype="dashed", color = "red") +
    labs(x = vars_name[i], y = "Percent Outsourced") +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "timeline_outsourced_", vars_time[i], ".pdf"),
         height = height, width = width)
  
  temp <- matched %>% 
    filter(age >= age_min, age <= age_max) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(outsourcing_occ_per = 
                survey_mean(outsourcing_occ, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "outsourcing_occ_per"), color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "outsourcing_occ_per_upp"),
              linetype="dashed", color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "outsourcing_occ_per_low"), 
              linetype="dashed", color = "red") +
    labs(x = vars_name[i], y = "Percent in Outsourcing Occupations") +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "timeline_out_occ_", vars_time[i], ".pdf"),
         height = height, width = width)
  
  temp <- matched %>% 
    filter(age >= age_min, age <= age_max, ever_out_occ) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per"), color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp"),
              linetype="dashed", color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low"), 
              linetype="dashed", color = "red") +
    labs(x = vars_name[i], y = "Percent Outsourced in Outsourcing Occupations") +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "timeline_out_occ_outsourced_", vars_time[i], ".pdf"),
         height = height, width = width)
  
  temp <- matched %>% 
    filter(age >= age_min, age <= age_max, !is.na(log_real_wkly_wage)) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    group_by(outsourced, add = T) %>% 
    summarise(lrww_mean = 
                survey_mean(log_real_wkly_wage, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "lrww_mean",
                         color = "factor(outsourced)")) +
    geom_line(aes_string(x = vars_time[i], y = "lrww_mean_upp",
                         color = "factor(outsourced)"), linetype="dashed") +
    geom_line(aes_string(x = vars_time[i], y = "lrww_mean_low",
                         color = "factor(outsourced)"), linetype="dashed") +
    labs(x = vars_name[i], y = "Log Real Weekly Wage") +
    scale_color_manual(name = "Outsourced", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = c("Not Outsourced", "Outsourced")) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "timeline_lrww_", vars_time[i], ".pdf"),
         height = height, width = width)
  
  breaks <- c("outsourced_per", "indep_con_per", "temp_work_per", "on_call_per")
  labels <- c("Outsourced", "Independent Contractor", "Temp Worker", "On-Call Worker")
  colors <- c( "blue", "dark green", "red", "purple")
  
  temp <- matched %>% 
    filter(age >= age_min, age <= age_max) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(
      outsourced_per = survey_mean(outsourced * 100),
      indep_con_per = survey_mean(indep_con * 100),
      temp_work_per = survey_mean(temp_work * 100),
      on_call_per = survey_mean(on_call * 100)) %>%
    select(-ends_with("_se")) %>% 
    gather(key = "var", value = "value", 2:5) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "value", color = "var"))  +
    scale_color_manual(name = "Job Type", breaks = breaks, labels = labels,
                       values = colors) +
    labs(x = vars_name[i], y = "Percent Workers") +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "timeline_all_types_", vars_time[i], ".pdf"),
         height = height, width = width)
}