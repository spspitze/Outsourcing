# This file takes data from timeline_clean and transition_clean
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
figure_folder <- "../Figures/NLSY 79 Timeline/"

# For saving graphs
aspect_ratio <- 1.62
height <- 7
width <- height * aspect_ratio

# Dataset is large, need to read as data table
timeline <- fread(str_c(clean_folder, "timeline_clean_occ.csv"))

timeline[, week := ymd(week)]

# Create w_month and w_year from week in timeline (drop first observation
# that looks like it's in 2000)
timeline[week > min(week), `:=`(w_month = round_date(week, unit = "month"),
                                w_year = round_date(week, unit = "year"))]

# This one is smaller, so no need for fread
transition <- read.csv(str_c(clean_folder, "transition_clean.csv"))

# Results noisy at end, so drop after max week, around end of 2016
week_max <- round_date(ymd("2016-09-11"), "week")

# If I want a quick run through
# week_max <- round_date(ymd("2003-09-11"), "week")

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
  format(round(var, r), nsmall = s)
}

# Create a function to put values in tables
format_val <- function(var, star = "", r = 2, s = 2) {
  str_c(" & ", format_it(var, r, s), star)
}

# Create a function to put standard errors in tables
format_se <- function(var, r = 2, s = 2) {
  str_c(" & (", format_it(var, r, s), ") ")
}

# Create a function to put N obs in tables
format_n <- function(n) {
  str_c(" & {", format(n, big.mark = ",", trim = T), "}")
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


##########################################################################
# outsoucing occupations = occupations with >= 3.4% outsourcing (twice average)
# Plot time series by Weeks, Months, Year, and Age
# 1. Percent of workers outsourced 
# 2. Percent of workers in outsourcing occupations 
# 3. Percent of workers in outsourcing occupations who are outsourced 
# 4. Log weekly wages of outsourced v traditional workers in outsourcing occupations 
# 5. Weeks of tenure of outsourced v traditional workers in outsourcing occupations
# 6. Number of workers unemployed based on ever outsourcing occupation
# 7. Length of unemployement|unemployed based on ever outsourcing occupation
# 8. Number of workers in all job types

vars_time <- c("week", "w_month", "w_year", "age")
vars_save <- c("week", "month", "year", "age")
vars_label <- c("Year", "Year", "Year", "Age")

# Create a function scale which takes index i and returns an
# x_scale_date for week, month and year and an 
# x_scale_continuous for age
scale <- function(i){
  if (i <= 3){
    return(scale_x_date(date_breaks = "2 years", date_labels = "%Y"))
  } else{
    scale_x_continuous(breaks = seq(min(timeline$age), max(timeline$age), by = 2))
  }
}


for (i in 1:length(vars_time)){
  # 1. Percent of workers outsourced
  temp <- timeline %>% 
    filter(!is.na(outsourced), week <= week_max, !is.na(.[[vars_time[i]]])) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per"), color = "red",
            n = 4) +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp"),
              linetype="dashed", color = "red", n = 4) +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low"), 
              linetype="dashed", color = "red", n = 4) +
    labs(x = vars_label[i], y = "Percent Outsourced") +
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "timeline_outsourced_", vars_save[i], ".pdf"),
         height = height, width = width)
  
  # 2. Percent of workers in outsourcing occupations
  temp <- timeline %>% 
    filter(!is.na(outsourcing_occ), week <= week_max, !is.na(.[[vars_time[i]]])) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(outsourcing_occ_per = survey_mean(outsourcing_occ * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "outsourcing_occ_per"), color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "outsourcing_occ_per_upp"),
              linetype="dashed", color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "outsourcing_occ_per_low"), 
              linetype="dashed", color = "red") +
    labs(x = vars_label[i], y = "% in Outsourcing Occupations") +
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "timeline_outsourcing_occ_", vars_save[i], ".pdf"),
         height = height, width = width)
  
  # 3. Percent of workers in outsourcing occupations who are outsourced
  temp <- timeline %>% 
    filter(outsourcing_occ, !is.na(outsourced), week <= week_max,
           !is.na(.[[vars_time[i]]])) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per"), color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp"),
              linetype="dashed", color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low"), 
              linetype="dashed", color = "red") +
    labs(x = vars_label[i], y = "% Outsourced in Outsourcing Occupations") +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "timeline_outsourcing_occ_outsourced_", vars_save[i], ".pdf"),
         height = height, width = width)
  
  # 4. Log weekly wages of outsourced v traditional workers in outsourcing occupations 
  temp <- timeline %>% 
    filter(!is.na(log_real_wkly_wage), week <= week_max, outsourcing_occ,
           !is.na(.[[vars_time[i]]])) %>% 
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
    labs(x = vars_label[i], y = "Log Real Weekly Wage") +
    scale_color_manual(name = "Outsourced", breaks = c(0, 1),
                       values = c("blue", "red"),
                       labels = c("Not Outsourced", "Outsourced")) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "timeline_outsourcing_occ_weekly_wage_",
               vars_save[i], ".pdf"),
         height = height, width = width)
  
  # 5. Weeks of tenure of outsourced v traditional workers in outsourcing occupations
  temp <- timeline %>% 
    filter(!is.na(w_tenure), week <= week_max, outsourcing_occ,
           !is.na(.[[vars_time[i]]])) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    group_by(outsourced, add = T) %>% 
    summarise(tenure_mean = 
                survey_mean(w_tenure, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "tenure_mean",
                         color = "factor(outsourced)")) +
    geom_line(aes_string(x = vars_time[i], y = "tenure_mean_upp",
                         color = "factor(outsourced)"), linetype="dashed") +
    geom_line(aes_string(x = vars_time[i], y = "tenure_mean_low",
                         color = "factor(outsourced)"), linetype="dashed") +
    labs(x = vars_label[i], y = "Weeks of Tenure") +
    scale_color_manual(name = "Outsourced", breaks = c(0, 1),
                       values = c("blue", "red"),
                       labels = c("Not Outsourced", "Outsourced")) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "timeline_outsourcing_occ_tenure_",
               vars_save[i], ".pdf"),
         height = height, width = width)
  
  # 6. Number of workers unemployed based on ever outsourcing occupation
  temp <- timeline %>% 
    filter(week <= week_max, !is.na(.[[vars_time[i]]])) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    # Code is not allowing me to group by both at once for some reason
    group_by_at(vars_time[i]) %>% 
    group_by(ever_out_occ, add = T) %>% 
    summarise(unemployed_per = survey_mean(unemployed, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "unemployed_per",
                         color = "factor(ever_out_occ)")) +
    geom_line(aes_string(x = vars_time[i], y = "unemployed_per_upp",
                         color = "factor(ever_out_occ)"), linetype="dashed") +
    geom_line(aes_string(x = vars_time[i], y = "unemployed_per_low",
                         color = "factor(ever_out_occ)"), linetype="dashed") +
    labs(x = vars_label[i], y = "Unemployment Rate") +
    scale_color_manual(name = "Ever Outsourced Occ", breaks = c(F, T),
                       values = c("blue", "red"),
                       labels = c("Never", "Ever")) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "timeline_unemployed_", vars_save[i], ".pdf"),
         height = height, width = width)
  
  # 7. Length of unemployement|unemployed based on ever outsourcing occupation
  temp <- timeline %>% 
    filter(week <= week_max, unemployed == 1, !is.na(.[[vars_time[i]]]),
           !is.na(unemployed_duration)) %>% 
    as_survey_design(ids = case_id, weights=weight) %>%
    # Code is not allowing me to group by both at once for some reason
    group_by_at(vars_time[i]) %>% 
    group_by(ever_out_occ, add = T) %>% 
    summarise(
      unemployed_duration_mean = survey_mean(unemployed_duration, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "unemployed_duration_mean",
                         color = "factor(ever_out_occ)")) +
    geom_line(aes_string(x = vars_time[i], y = "unemployed_duration_mean_upp",
                         color = "factor(ever_out_occ)"), linetype="dashed") +
    geom_line(aes_string(x = vars_time[i], y = "unemployed_duration_mean_low",
                         color = "factor(ever_out_occ)"), linetype="dashed") +
    labs(x = vars_label[i], y = "Length of Unemployment") +
    scale_color_manual(name = "Ever Outsourced Occ", breaks = c(F, T),
                       values = c("blue", "red"),
                       labels = c("Never", "Ever")) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "timeline_unemployment_length_", vars_save[i], ".pdf"),
         height = height, width = width)
  
  # 8. Number of workers in all job types
  breaks <- c("outsourced_per", "indep_con_per", "temp_work_per", "on_call_per")
  labels <- c("Outsourced", "Independent Contractor", "Temp Worker", "On-Call Worker")
  colors <- c( "blue", "dark green", "red", "purple")
  
  temp <- timeline %>% 
    filter(!is.na(outsourced), !is.na(indep_con), 
           !is.na(temp_work), !is.na(on_call), week <= week_max, 
           !is.na(.[[vars_time[i]]])) %>% 
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
    labs(x = vars_label[i], y = "Percent Workers") +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "timeline_all_types_", vars_save[i], ".pdf"),
         height = height, width = width)
}


# For my own benefit, also plot number of observations each week
temp <- timeline %>% 
  group_by(week) %>% 
  summarise(obs = n()) %>% 
  ggplot() +
  geom_line(aes(x = week, y = obs), color = "red") +
  geom_vline(xintercept = week_max) +
  labs(x = "Year", y = "Observations") +
  theme_light(base_size = 16) 

ggsave(str_c(figure_folder, "timeline_observations_week.pdf"),
       height = height, width = width)

# Obsevarvations by employed/unemployed
temp <- timeline %>% 
  group_by(week) %>% 
  summarise(emp_obs = n() - sum(unemployed, na.rm = T), 
            unemp_obs = sum(unemployed, na.rm = T)) %>% 
  ggplot() +
  geom_line(aes(x = week, y = emp_obs), color = "blue") +
  geom_line(aes(x = week, y = unemp_obs), color = "red") +
  geom_vline(xintercept = week_max) +
  labs(x = "Year", y = "Observations") +
  theme_light(base_size = 16) 

ggsave(str_c(figure_folder, "timeline_emp_unemp_obs_week.pdf"),
       height = height, width = width)

# Some graphs playing around with things
# temp <- timeline %>% 
#   filter(week <= week_max, !is.na(outsourced_per), !is.na(outsourced)) %>% 
#   group_by_at(vars_time[i]) %>% 
#   summarise(
#     outsourced = mean(outsourced),
#     outsourcing_occ = mean(outsourcing_occ, na.rm = T),
#     outsourcing_occ = mean(outsourcing_occ, na.rm = T),
#     # outsourced_occ_50 = mean(outsourced_per >= quantile(outsourced_per, .5)),
#     # outsourced_occ_75 = mean(outsourced_per >= quantile(outsourced_per, .75)),
#     # outsourced_occ_85 = mean(outsourced_per >= quantile(outsourced_per, .85)),
#     obs = n()) %>% 
#   ggplot() +
#   geom_line(aes_string(x = vars_time[i], y = "outsourced"), color = "blue") +
#   # geom_line(aes_string(x = vars_time[i], y = "outsourcing_occ"), color = "purple") +
#   # geom_line(aes_string(x = vars_time[i], y = "outsourcing_occ"), color = "black") +
#   # geom_line(aes_string(x = vars_time[i], y = "outsourced_occ_50"), color = "green") +
#   # geom_line(aes_string(x = vars_time[i], y = "outsourced_occ_75"), color = "orange") +
#   # geom_line(aes_string(x = vars_time[i], y = "outsourced_occ_85"), color = "red") +
#   labs(x = vars_name[i], y = "Observations") +
#   theme_light(base_size = 16) 
# 
# temp <- timeline %>% 
#   filter(!is.na(log_real_wkly_wage), week <= week_max, !is.na(outsourcing_occ),
#          outsourced == 1 | traditional == 1) %>% 
#   mutate(
#     # group = ((occ_per_out >= quantile(occ_per_out, .5)) + 
#     #            (occ_per_out >= quantile(occ_per_out, .75)) +
#     #            (occ_per_out >= quantile(occ_per_out, .85)))
#     group = outsourcing_occ + outsourcing_occ
#   ) %>% 
#   group_by_at(vars_time[i]) %>%
#   # mutate(
#   #   lrww_median = 
#   #     log_real_wkly_wage / median(log_real_wkly_wage)) %>% 
#   group_by(group, outsourced, add = T) %>% 
#   summarise(lrww_median_mean = mean(log_real_wkly_wage)) %>% 
#   ggplot() +
#   geom_line(aes_string(x = vars_time[i], y = "lrww_median_mean",
#                        color = "factor(group)", linetype="factor(outsourced)")) +
#   theme_light(base_size = 16) 
# 
# temp <- timeline %>% 
#   filter(week <= week_max, occ == 1040) %>% 
#   group_by_at(vars_time[i]) %>% 
#   summarise(
#     outsourced = mean(outsourced),
#     obs = n()) %>% 
#   ggplot() +
#   geom_line(aes_string(x = vars_time[i], y = "outsourced"), color = "blue") +
#   labs(x = vars_name[i], y = "Observations") +
#   theme_light(base_size = 16) 

# # If later want to run hp_filter, use package lpirfs and looks something like this
# hp_weekly <- 52^4 * 6.25
# temp <- timeline %>% 
#   filter(!is.na(outsourced), week <= week_max) %>% 
#   as_survey_design(ids = case_id, weights=weight) %>% 
#   group_by_at(vars_time[i]) %>% 
#   summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
#   mutate(
#     outsourced_per = hp_filter(as.matrix(.$outsourced_per), hp_weekly)[[2]],
#     outsourced_per_upp = hp_filter(as.matrix(.$outsourced_per_upp), hp_weekly)[[2]],
#     outsourced_per_low = hp_filter(as.matrix(.$outsourced_per_low), hp_weekly)[[2]]
#   ) %>% 
#   ggplot() +
#   geom_line(aes_string(x = vars_time[i], y = "outsourced_per"), color = "red",
#             n = 4) +
#   geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp"),
#             linetype="dashed", color = "red", n = 4) +
#   geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low"), 
#             linetype="dashed", color = "red", n = 4) +
#   labs(x = vars_name[i], y = "Percent Outsourced") +
#   theme_light(base_size = 16) 

# Correlation -------------------------------------------------------------

# Group occupations by week and compare trend in employment to 
# trend in outsourcing

# Unweighted
occ_timeline <- timeline %>%
  filter(!is.na(occ), !is.na(outsourced), week <= week_max) %>%
  group_by(week, occ, outsourced) %>%
  summarise(
    outsourcing_occ = mean(outsourcing_occ),
    n = sum(weight),
    log_real_wkly_wage = mean(log_real_wkly_wage, na.rm = T),
    hours_week = mean(hours_week, na.rm = T),
    part_time = mean(part_time, na.rm = T),
    tenure = mean(tenure, na.rm = T),
    health = mean(health, na.rm = T),
    retirement = mean(retirement, na.rm = T),
    any_benefits = mean(any_benefits, na.rm = T),
    n_female = sum(female * weight),
    n_black = sum(black * weight),
    n_hispanic = sum(hispanic * weight),
    n_union = sum(union * weight, na.rm = T),
    n_union_defined = sum((!is.na(union) %in% T) * weight),
    tot_age = sum(age * weight),
    tot_age_2 = sum(age_2 * weight)
  ) %>%
  pivot_wider(names_from = outsourced,
              values_from = n:tot_age_2) %>%
  ungroup() %>% 
  mutate(
    workers = r_sum(n_0, n_1),
    outsourced_per = ifelse(!is.na(n_1), n_1 / workers * 100, 0),
    female_per = r_sum(n_female_0, n_female_1) / workers * 100,
    black_per = r_sum(n_black_0, n_black_1) / workers * 100,
    hispanic_per = r_sum(n_hispanic_0, n_hispanic_1) / workers * 100,
    union_per = (r_sum(n_union_0, n_union_1) / 
                   r_sum(n_union_defined_0, n_union_defined_1) * 100),
    age = r_sum(n_0 * tot_age_0, n_1 * tot_age_1) / workers,
    age_2 = r_sum(n_0 * tot_age_2_0, n_1 * tot_age_2_1) / workers 
  ) %>% 
  group_by(week) %>% 
  mutate(workers_per = workers / sum(workers) * 100) %>% 
  ungroup()
  
occ_corr <- occ_timeline %>% 
  group_by(occ) %>%
  summarise(
    corr_emp = cor(outsourced_per, workers_per, use = "na.or.complete"),
    corr_out_wages = 
      cor(outsourced_per[which(log_real_wkly_wage_1 > 0)],
          log_real_wkly_wage_1[which(log_real_wkly_wage_1 > 0)], 
          use = "na.or.complete"),
    corr_wages = cor(outsourced_per, log_real_wkly_wage_0, use = "na.or.complete"),
    outsourcing_occ = mean(outsourcing_occ),
    workers_mean = mean(workers)
    ) %>% 
  filter(!is.na(corr_emp))

occ_corr_w <- occ_corr %>% 
  as_survey_design(ids = occ, weights=workers_mean) %>% 
  summarise(
    corr_emp = unweighted(mean(corr_emp)),
    corr_emp_se = unweighted(sd(corr_emp) / sqrt(n())),
    w_corr_emp = survey_mean(corr_emp, vartype = "se"),
    
    corr_out_wages = unweighted(mean(corr_out_wages, na.rm = T)),
    corr_out_wages_se = unweighted(sd(corr_out_wages, na.rm = T) /
                                     sqrt(sum(!is.na(corr_out_wages)))),
    w_corr_out_wages = survey_mean(corr_out_wages, na.rm = T, vartype = "se"),
    
    corr_wages = unweighted(mean(corr_wages, na.rm = T)),
    corr_wages_se = unweighted(sd(corr_wages, na.rm = T) /
                                 sqrt(sum(!is.na(corr_wages)))),
    w_corr_wages = survey_mean(corr_wages, na.rm = T, vartype = "se")
    ) 

occ_corr_w_g <- occ_corr %>% 
  as_survey_design(ids = occ, weights=workers_mean) %>% 
  group_by(outsourcing_occ) %>% 
  summarise(
    corr_emp = unweighted(mean(corr_emp)),
    corr_emp_se = unweighted(sd(corr_emp) / sqrt(n())),
    w_corr_emp = survey_mean(corr_emp, vartype = "se"),
    
    corr_out_wages = unweighted(mean(corr_out_wages, na.rm = T)),
    corr_out_wages_se = unweighted(sd(corr_out_wages, na.rm = T) /
                                     sqrt(sum(!is.na(corr_out_wages)))),
    w_corr_out_wages = survey_mean(corr_out_wages, na.rm = T, vartype = "se"),
    
    corr_wages = unweighted(mean(corr_wages, na.rm = T)),
    corr_wages_se = unweighted(sd(corr_wages, na.rm = T) /
                                 sqrt(sum(!is.na(corr_wages)))),
    w_corr_wages = survey_mean(corr_wages, na.rm = T, vartype = "se")
  ) %>% 
  arrange(outsourcing_occ)

# # # Weighted (not working)
# occ_timeline <- timeline %>%
#   filter(!is.na(occ), !is.na(outsourced), week <= week_max, occ < 100) %>%
#   as_survey_design(ids = case_id, weights=weight) %>%
#   group_by(week, occ, outsourced) %>%
#   summarise(
#     outsourcing_occ = unweighted(mean(outsourcing_occ)),
#     # n = survey_count(),
#     log_real_wkly_wage = survey_mean(log_real_wkly_wage, na.rm = T),
#     hours_week = survey_mean(hours_week, na.rm = T),
#     part_time = survey_mean(part_time, na.rm = T),
#     tenure = survey_mean(tenure, na.rm = T),
#     health = survey_mean(health, na.rm = T),
#     retirement = survey_mean(retirement, na.rm = T),
#     any_benefits = survey_mean(any_benefits, na.rm = T),
#     n_female = survey_total(female),
#     n_black = survey_total(black),
#     n_hispanic = survey_total(hispanic),
#     n_union = survey_total(union * weight),
#     n_union_defined = survey_total(!is.na(union)),
#     tot_age = survey_total(age),
#     tot_age_2 = survey_total(age_2)
#   ) %>%
#   pivot_wider(names_from = outsourced,
#               values_from = n:tot_age_2) %>%
#   ungroup() %>% 
#   mutate(
#     workers = r_sum(n_0, n_1),
#     outsourced_per = ifelse(!is.na(n_1), n_1 / workers * 100, 0),
#     female_per = r_sum(n_female_0, n_female_1) / workers * 100,
#     black_per = r_sum(n_black_0, n_black_1) / workers * 100,
#     hispanic_per = r_sum(n_hispanic_0, n_hispanic_1) / workers * 100,
#     union_per = (r_sum(n_union_0, n_union_1) / 
#                    r_sum(n_union_defined_0, n_union_defined_1) * 100),
#     age = r_sum(n_0 * tot_age_0, n_1 * tot_age_1) / workers,
#     age_2 = r_sum(n_0 * tot_age_2_0, n_1 * tot_age_2_1) / workers 
#   ) %>% 
#   group_by(week) %>% 
#   mutate(workers_per = workers / sum(workers) * 100) %>% 
#   group_by(occ) %>% 
#   mutate(workers_mean = mean(workers)) %>% 
#   ungroup()

# Create graphs of distributions and create a table with each t.test

vars_c <- c("emp", "wages", "out_wages")
var_names <- c("Employment", "Non-Outsourced Wages", "Outsourced Wages")
bins <- c(20, 20, 20)

table_c <- str_c(table_top, siunitx, 
"
\\begin{tabular}{lSSSS}
\\toprule
Group & {Unweighted Correlation} & {t-stat} & {Weighted Correlation} & {t-stat}  \\\\
\\midrule\n"
)

for (i in 1:3) {
  corr <- str_c("corr_", vars_c[i])
  corr_se <- str_c(corr, "_se")
  w_corr <- str_c("w_", corr)
  w_corr_se <- str_c("w_", corr_se)
  
  corr_all <- occ_corr_w[[corr]]
  corr_ho <- occ_corr_w_g[[corr]][2]
  corr_lo <- occ_corr_w_g[[corr]][1]
  t_all <- occ_corr_w[[corr]] / occ_corr_w[[corr_se]]
  t_ho <- occ_corr_w_g[[corr]][2] / occ_corr_w_g[[corr_se]][2]
  t_lo <- occ_corr_w_g[[corr]][1] / occ_corr_w_g[[corr_se]][1]
  
  w_corr_all <- occ_corr_w[[w_corr]]
  w_corr_ho <- occ_corr_w_g[[w_corr]][2]
  w_corr_lo <- occ_corr_w_g[[w_corr]][1]
  w_t_all <- occ_corr_w[[w_corr]] / occ_corr_w[[w_corr_se]]
  w_t_ho <- occ_corr_w_g[[w_corr]][2] / occ_corr_w_g[[w_corr_se]][2]
  w_t_lo <- occ_corr_w_g[[w_corr]][1] / occ_corr_w_g[[w_corr_se]][1]
  
  temp <- occ_corr %>% 
    filter(!is.na(.[[corr]])) %>% 
    ggplot() +
    geom_histogram(aes_string(corr, fill = "factor(outsourcing_occ)"),
                   alpha = 0.4, bins = bins[i]) +
    geom_vline(aes(xintercept = corr_all), size=1) +
    geom_vline(aes(xintercept = corr_ho), color = "red", size=1) +
    geom_vline(aes(xintercept = corr_lo), color = "blue", size=1) +
    labs(x = "Correlation", y = "Count") +
    scale_fill_manual(name = "Occupation\nOutsourcing", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = list(expression("Low (" < " 3.4%)"), 
                                    expression("High (" >= " 3.4%)"))) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "occ_", corr, ".pdf"), height = height, width = width)
  
  # What about correlation weighted by occupation size?
  temp <- occ_corr %>% 
    filter(!is.na(.[[corr]])) %>% 
    ggplot() +
    geom_point(aes_string(x = corr, y = "workers_mean",
                          color = "factor(outsourcing_occ)"), alpha = 0.4) +
    geom_vline(aes(xintercept = w_corr_all), size=1) +
    geom_vline(aes(xintercept = w_corr_ho), color = "red", size=1) +
    geom_vline(aes(xintercept = w_corr_lo), color = "blue", size=1)  +
    labs(x = "Correlation", y = "Average Number of Workers")+
    scale_color_manual(name = "Occupation\nOutsourcing", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = list(expression("Low (" < " 3.4%)"), 
                                    expression("High (" >= " 3.4%)"))) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "occ_", w_corr, ".pdf"),
         height = height, width = width) 
  
  # Append table
  table_c <- str_c(table_c, var_names[i], " & & & & \\\\ \\midrule\n",
                   "All & ",
                   format_it(corr_all), " & ",
                   format_it(t_all), " & ",
                   format_it(w_corr_all), " & ",
                   format_it(w_t_all), " \\\\ \n",
                   "High Outsourcing & ",
                   format_it(corr_ho), " & ",
                   format_it(t_ho), " & ",
                   format_it(w_corr_ho), " & ",
                   format_it(w_t_ho), " \\\\ \n",
                   "Low Outsourcing & ",
                   format_it(corr_lo), " & ",
                   format_it(t_lo), " & ",
                   format_it(w_corr_lo), " & ",
                   format_it(w_t_lo), " \\\\ \n")
  
  if (i %in% c(1, 2)) {
    table_c <- str_c(table_c, "\\midrule\n")
  } else {
    table_c <- str_c(table_c,
"
\\bottomrule
\\end{tabular}
\\caption{Mean correlation between an occupation's percent of all jobs and either
  percent of that occupation that is outsourced or wage level of non-outsourced
  and outsourced workers at the week level. High Outsourcing 
  occupations are occupations that are outsourced at more than twice the 
  average rate, non-outsourcing occupations are all others. Unweighted
  correlations treat all occupations the same, weighted weight by 
  average weekly observations.}
  \\label{occ_corr}
  \\end{table}
  \\end{document}
")
  }
}

write.table(table_c,
            str_c(table_folder,
                  "NLSY79 Occupation Info/NLSY79 Occupation Correlation.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# How do these correlations change with controls? Run regressions
controls <- c("female_per", "black_per", "hispanic_per", "union_per",
              "age", "age_2")

fixed_effects <- c("week", "occ")

outcomes <- c("log_real_wkly_wage", "tenure", "part_time", "hours worked",
              "health", "retirement", "any_benefits")

control_list <- list("outsourced_per", c("outsourced_per", controls))

for (i in 1:length(outcomes)){
  for (out in 0:1){
    for (j in 1:2) {
      
      eq <- create_formula(str_c(outcomes[[i]], "_", out), control_list[[j]])
      fe <- create_formula("", fixed_effects)
      
      temp <- lm_robust(eq, data = occ_timeline, 
                        se_type = "stata", try_cholesky = T)
      
      temp_fe <- lm_robust(eq, data = occ_timeline, fixed_effects = !!fe,
                           weight = workers,
                           se_type = "stata", try_cholesky = T)
      
      
      
    }
  }
}

eq <- create_formula("workers_per", list("outsourced_per", controls))
fe <- create_formula("", fixed_effects)




# Individual's Percent Outsourcing ----------------------------------------

# For workers ever outsourced, what percent of weeks are in outsourcing
# vs non-outsourcing jobs?

outsourcing_per <- timeline %>% 
  filter(week <= week_max, ever_out == 1, !is.na(outsourced)) %>% 
  group_by(case_id) %>% 
  summarise(
    outsourcing_per = mean(outsourced) * 100,
    ever_out_occ = mean(ever_out_occ),
    weight = mean(weight)
  ) %>% 
  filter(outsourcing_per > 0) %>% 
  ungroup() %>% 
  mutate(weight = weight / sum(weight))

outsourcing_per_mean <- weighted.mean(outsourcing_per$outsourcing_per, 
                                      outsourcing_per$weight)

temp <- outsourcing_per %>% 
  ggplot() +
  geom_density(aes(outsourcing_per, weight = weight), 
               alpha = 0.4, fill = "blue") +
  geom_vline(aes(xintercept = outsourcing_per_mean), size = 1) +
  geom_text(aes(x = outsourcing_per_mean + 1, y = .016, hjust = 0,
                label = str_c("Mean = ", round(outsourcing_per_mean, 2))),
            size = 6) +
  labs(x = "Percent Weeks Outsourced", y = "Density") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  theme_light(base_size = 16) 
  
ggsave(str_c(figure_folder, "Percent of Individual's Jobs Outsourced.pdf"),
       height = height, width = width) 

###############################################################################
# Look at job-to-job transitions

constant <- c("female", "outsourced", "n")

vars <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "part_time",
          "union", "job_sat", "any_benefits", "weeks_between_jobs_2",
          "total_weeks", "total_above_0", "same_occ", "same_ind", "outsourced")

vars_match <- str_c(vars, collapse = "|")

# Summary Statistics
transition_summary <- transition %>%
  # For weeks between jobs 2, set top 1\% as NA
  mutate(weeks_between_jobs_2 = ifelse(
    weeks_between_jobs_2 < quantile(weeks_between_jobs_2, .99),
    weeks_between_jobs_2, NA)) %>%
  as_survey_design(ids = case_id, weights=weight) %>%
  mutate(outsourced = factor(outsourced_curr)) %>%
  group_by(female, outsourced) %>%
  summarise(
    log_real_hrly_wage_curr_mean = survey_mean(log_real_hrly_wage_curr, na.rm = T),
    log_real_wkly_wage_curr_mean = survey_mean(log_real_wkly_wage_curr, na.rm = T),
    hours_week_curr_mean = survey_mean(hours_week_curr, na.rm = T),
    part_time_curr_per = survey_mean(part_time_curr, na.rm = T),
    job_sat_curr_mean = survey_mean(job_sat_curr, na.rm = T),
    union_curr_per = survey_mean(union_curr, na.rm = T),
    any_benefits_curr_per = survey_mean(any_benefits_curr, na.rm = T),
    weeks_between_jobs_2_curr_mean = survey_mean(weeks_between_jobs_2, na.rm = T),
    total_weeks_curr = survey_total(weeks_between_jobs_2, na.rm = T),
    total_above_0_curr = survey_total(weeks_between_jobs_2 > 0, na.rm = T),
    outsourced_last_per = survey_mean(outsourced_last, na.rm = T),
    log_real_hrly_wage_last_mean = survey_mean(log_real_hrly_wage_last, na.rm = T),
    log_real_wkly_wage_last_mean = survey_mean(log_real_wkly_wage_last, na.rm = T),
    hours_week_last_mean = survey_mean(hours_week_last, na.rm = T),
    part_time_last_per = survey_mean(part_time_last, na.rm = T),
    job_sat_last_mean = survey_mean(job_sat_last, na.rm = T),
    union_last_per = survey_mean(union_last, na.rm = T),
    any_benefits_last_per = survey_mean(any_benefits_last, na.rm = T),
    same_occ_last_per = survey_mean(occ_curr == occ_last, na.rm = T),
    same_ind_last_per = survey_mean(ind_curr == ind_last, na.rm = T),
    n = unweighted(n())
  ) %>%
  gather(key=key, value=value, -constant) %>%
  mutate(
    var = str_subset(key, vars_match) %>% str_extract(vars_match),
    m_p = ifelse(str_detect(key, "mean"),
                 ifelse(str_detect(key, "se"), "mean_se", "mean"),
                 ifelse(str_detect(key, "se"), "per_se", "per")),
    curr = str_detect(key, "curr")) %>%
  unite(variable, var, m_p, sep = "_") %>%
  select(-key) %>%
  spread(key=variable, value=value) %>%
  mutate(weeks_between_jobs_p = total_weeks_per / total_above_0_per) %>% 
  arrange(female, desc(outsourced), desc(curr))

# Also create a table just for outsourcing_occupations 
transition_summary_outsourcing <- transition %>%
  filter(outsourcing_occ) %>% 
  # For weeks between jobs 2, set top 1\% as NA
  mutate(weeks_between_jobs_2 = ifelse(
    weeks_between_jobs_2 < quantile(weeks_between_jobs_2, .99),
    weeks_between_jobs_2, NA)) %>%
  as_survey_design(ids = case_id, weights=weight) %>%
  mutate(outsourced = factor(outsourced_curr)) %>%
  group_by(female, outsourced) %>%
  summarise(
    log_real_hrly_wage_curr_mean = survey_mean(log_real_hrly_wage_curr, na.rm = T),
    log_real_wkly_wage_curr_mean = survey_mean(log_real_wkly_wage_curr, na.rm = T),
    hours_week_curr_mean = survey_mean(hours_week_curr, na.rm = T),
    part_time_curr_per = survey_mean(part_time_curr, na.rm = T),
    job_sat_curr_mean = survey_mean(job_sat_curr, na.rm = T),
    union_curr_per = survey_mean(union_curr, na.rm = T),
    any_benefits_curr_per = survey_mean(any_benefits_curr, na.rm = T),
    weeks_between_jobs_2_curr_mean = survey_mean(weeks_between_jobs_2, na.rm = T),
    total_weeks_curr = survey_total(weeks_between_jobs_2, na.rm = T),
    total_above_0_curr = survey_total(weeks_between_jobs_2 > 0, na.rm = T),
    outsourced_last_per = survey_mean(outsourced_last, na.rm = T),
    log_real_hrly_wage_last_mean = survey_mean(log_real_hrly_wage_last, na.rm = T),
    log_real_wkly_wage_last_mean = survey_mean(log_real_wkly_wage_last, na.rm = T),
    hours_week_last_mean = survey_mean(hours_week_last, na.rm = T),
    part_time_last_per = survey_mean(part_time_last, na.rm = T),
    job_sat_last_mean = survey_mean(job_sat_last, na.rm = T),
    union_last_per = survey_mean(union_last, na.rm = T),
    any_benefits_last_per = survey_mean(any_benefits_last, na.rm = T),
    same_occ_last_per = survey_mean(occ_curr == occ_last, na.rm = T),
    same_ind_last_per = survey_mean(ind_curr == ind_last, na.rm = T),
    n = unweighted(n())
  ) %>%
  gather(key=key, value=value, -constant) %>%
  mutate(
    var = str_subset(key, vars_match) %>% str_extract(vars_match),
    m_p = ifelse(str_detect(key, "mean"),
                 ifelse(str_detect(key, "se"), "mean_se", "mean"),
                 ifelse(str_detect(key, "se"), "per_se", "per")),
    curr = str_detect(key, "curr")) %>%
  unite(variable, var, m_p, sep = "_") %>%
  select(-key) %>%
  spread(key=variable, value=value) %>%
  mutate(weeks_between_jobs_p = total_weeks_per / total_above_0_per) %>% 
  arrange(female, desc(outsourced), desc(curr))

top_t <- str_c(table_top, siunitx, "
\\begin{tabular}{lSSSS}
               \\toprule
               & \\multicolumn{2}{c}{{Outsourced Currently}} & \\multicolumn{2}{c}{{Non-Outsourced Currently}} \\\\
               & {Previous} & {Current} & {Previous} & {Current}  \\\\  \\midrule
               "
)

# Create table in Latex
vars_t <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "part_time",
            "union", "job_sat", "any_benefits")

# Divide variables by mean or percent (they are different below)
vars_t_m <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "job_sat")

vars_t_p <- c("part_time", "union", "any_benefits")

# Also check if new job has same occ/ind
vars_t_o <- c("occ", "ind")

for (occ_g in c(1, 2)){
  if (occ_g == 1){
    data <- transition_summary
    occ_name <- ""
    Occ_name <- ""
  } else{
    data <- transition_summary_outsourcing
    occ_name <- " in high outsourcing occupations"
    Occ_name <- " Outsourcing Occupations"
  }
  
  for (sex in c(1, 5)){
  
    Sex_name <- "Men"
    sex_name <- "men"
    if (sex == 5){
      Sex_name <- "Women"
      sex_name <- "women"
    }
  
    center_t <- rbind("Outsourced", "", "Same", "Occupation", "Same", "Industry",
                      "Log Real", "Hourly Wage", "Log Real", "Weekly Earnings", "Hours Worked",
                      "Weekly", "Part Time", "", "Union", "","Job Satisfaction",
                      "(Lower Better)", "Any Benefits", "", "Weeks To", "Find Job",
                      "Observations")
  
    for (out in c(0, 2)){
  
      i <- sex + out
      i_p <- sex + out + 1
  
      # Start with outsourced
      temp <- cbind(
        rbind(
          format_val(data$outsourced_per[i_p], 
                     star = p_test_1(data, "outsourced_per", "n", i_p)),
          format_se(data$outsourced_per_se[i_p])
        ),
        rbind(
          str_c(" & {", format_it(1 - (out / 2), 3, 0), "}"),
          str_c(" & {--} ")
        )
      )
  
      # Now do same occ/ind
      for (var in vars_t_o){
        var_n <- str_c("same_", var, "_per")
        se_n <- str_c("same_", var, "_per_se")
        temp <- rbind(temp,
                      cbind(
                        rbind(
                          format_val(data[[var_n]][i_p],
                                star = p_test_1(data, var_n, "n", i_p)),
                          format_se(data[[se_n]][i_p])
                        ),
                        rbind(str_c(" &  {--} "), str_c(" & "))
                      )
        )
      }
  
      # Now for everything else
      for (var in vars_t){
        if (var %in% vars_t_p){
          var_n = str_c(var, "_per")
          se_n = str_c(var, "_per_se")
          p_star <- p_test(data, var_n, "n", i, i_p)
        } else{
          var_n = str_c(var, "_mean")
          se_n = str_c(var, "_mean_se")
          p_star <- t_test(data, var_n, "n", i, i_p)
        }
        temp <- rbind(
          temp,
          cbind(
            rbind(
              format_val(data[[var_n]][i_p], star = p_star),
              format_se(data[[se_n]][i_p])
              ),
            rbind(
              format_val(data[[var_n]][i]),
              format_se(round(data[[se_n]][i]))
              )
            )
          )
      }
  
      # Number of observations and weeks to find job for current job only
      temp <- rbind(
        temp,
        cbind(rbind(" & ", " & ", " & "),
              rbind(
                format_val(data$weeks_between_jobs_2_mean[i]),
                format_se(data$weeks_between_jobs_2_mean_se[i], 3),
                format_n(data$n[i])
                )
              )
        )
  
      if (out == 0){
        center_t <- cbind(center_t, temp)
      } else{
        center_t <- cbind(
          center_t, temp,
          rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
                "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\"))
      }
  
    }
  
    bot_t <- str_c(
  "\\bottomrule
  \\end{tabular}
  \\caption{Job statistics for ", sex_name, occ_name,
  " at current and previous job for workers
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
    file_1 <- str_c(t_folder, "center_t.txt")
    file_2 <- str_c(t_folder, "center_t.txt")
    write.table(center_t, file_1, quote=T, col.names=F, row.names=F)
    center_t <- read.table(file_1, sep = "")
    write.table(center_t, file_2, quote=F, col.names=F, row.names=F, sep = "")
    center_t <- readChar(file_2, nchars = 1e6)
  
    write.table(str_c(top_t, center_t, bot_t),
                str_c(table_folder,
                       "NLSY79 Job Transitions Fine/NLSY79 Job Transitions Fine ",
                       Sex_name, Occ_name, ".tex"),
                quote=F, col.names=F, row.names=F, sep="")
  }
}

# Specifically interested in weeks to find job conditional on it being 
# positive for outsourcing occupations (include other thing so can run 
# regressions below)
weeks_summary <- transition %>%
  filter(outsourcing_occ, 
         weeks_between_jobs_2 < quantile(weeks_between_jobs_2, .99),
         weeks_between_jobs_2 > 0) %>% 
  as_survey_design(ids = case_id, weights=weight) %>%
  mutate(outsourced = factor(outsourced_curr)) %>%
  group_by(female, outsourced) %>%
  summarise(
    weeks_between_jobs_2_curr_mean = survey_mean(weeks_between_jobs_2, na.rm = T),
    weeks_between_jobs_2_curr_median = survey_median(weeks_between_jobs_2, na.rm = T),
    log_real_hrly_wage_curr_mean = survey_mean(log_real_hrly_wage_curr, na.rm = T),
    log_real_wkly_wage_curr_mean = survey_mean(log_real_wkly_wage_curr, na.rm = T),
    hours_week_curr_mean = survey_mean(hours_week_curr, na.rm = T),
    part_time_curr_per = survey_mean(part_time_curr, na.rm = T),
    job_sat_curr_mean = survey_mean(job_sat_curr, na.rm = T),
    union_curr_per = survey_mean(union_curr, na.rm = T),
    any_benefits_curr_per = survey_mean(any_benefits_curr, na.rm = T),
    outsourced_last_per = survey_mean(outsourced_last, na.rm = T),
    log_real_hrly_wage_last_mean = survey_mean(log_real_hrly_wage_last, na.rm = T),
    log_real_wkly_wage_last_mean = survey_mean(log_real_wkly_wage_last, na.rm = T),
    hours_week_last_mean = survey_mean(hours_week_last, na.rm = T),
    part_time_last_per = survey_mean(part_time_last, na.rm = T),
    job_sat_last_mean = survey_mean(job_sat_last, na.rm = T),
    union_last_per = survey_mean(union_last, na.rm = T),
    any_benefits_last_per = survey_mean(any_benefits_last, na.rm = T),
    same_occ_last_per = survey_mean(occ_curr == occ_last, na.rm = T),
    same_ind_last_per = survey_mean(ind_curr == ind_last, na.rm = T),
    n = unweighted(n())
  ) %>%
  gather(key=key, value=value, -constant) %>%
  mutate(
    var = str_subset(key, vars_match) %>% str_extract(vars_match),
    m_p = ifelse(str_detect(key, "mean"),
                 ifelse(str_detect(key, "se"), "mean_se", "mean"),
                 ifelse(str_detect(key, "per"),
                 ifelse(str_detect(key, "se"), "per_se", "per"),
                 ifelse(str_detect(key, "se"), "median_se", "median"))),
    curr = str_detect(key, "curr")) %>%
  unite(variable, var, m_p, sep = "_") %>%
  select(-key) %>%
  spread(key=variable, value=value)

# Plot mean and median on one graph

table_w <- str_c(table_top, siunitx, 
"
\\begin{tabular}{lSSSS}
\\toprule
& \\multicolumn{2}{c}{{Mean}} & \\multicolumn{2}{c}{{Median}} \\\\
& {Outsourced} & {Not Outsourced} & {Outsourced} & {Not Outsourced}  \\\\  \\midrule
"
)

for (sex in c(2, 4)){
  Sex_name <- "Men"
  if (sex == 4){
    Sex_name <- "Women"
  }
  
  table_w <- str_c(
    table_w, Sex_name, 
    format_val(weeks_summary$weeks_between_jobs_2_mean[sex]),
    format_val(weeks_summary$weeks_between_jobs_2_mean[sex + 2],
               t_test(weeks_summary, "weeks_between_jobs_2_mean", "n",
                      sex, sex + 2)),
    format_val(weeks_summary$weeks_between_jobs_2_median[sex]),
    format_val(weeks_summary$weeks_between_jobs_2_median[sex + 2], 
               t_test(weeks_summary, "weeks_between_jobs_2_median", "n",
                      sex, sex + 2)), " \\\\",
    format_se(weeks_summary$weeks_between_jobs_2_mean_se[sex]),
    format_se(weeks_summary$weeks_between_jobs_2_mean_se[sex + 2]),
    format_se(weeks_summary$weeks_between_jobs_2_median_se[sex]),
    format_se(weeks_summary$weeks_between_jobs_2_median_se[sex + 2]),
    " \\\\ \n")
}

table_w <- str_c(table_w,
"\\bottomrule
\\end{tabular}
\\caption{Mean and median weeks to find current job for workers in high outsourcing
occupations conditional on positive (>0) number of weeks.
Stars represent significant difference from outsourced jobs at 
the .10 level *, .05 level **, and .01 level ***.}
\\label{weeks_find_job}
\\end{table}
\\end{document}")

write.table(
  table_w,
  str_c(table_folder,
        "NLSY79 Job Transitions Fine/NLSY79 Week to Find Job Outsourcing Occupations.tex"),
            quote=F, col.names=F, row.names=F, sep="")

###################################################################

# Plot figures using transition
sex_save <- c("men", "women")
var_g <- c("weeks_between_jobs_2")
var_names <- c("Weeks to Find Job")

for (i in 1:length(var_g)){
  for (sex in c(0, 1)){

    # All occupations
    temp <- transition %>%
      filter(female == sex, !is.na(.[[var_g[i]]]),
             .[[var_g[i]]] < quantile(.[[var_g[i]]], .99), .[[var_g[i]]] > 0) %>% 
      ggplot() +
      geom_density(aes_string(var_g[i], fill = "factor(outsourced_curr)"),
                   alpha = 0.2) +
      labs(x = var_names[i], y = "Density") +
      scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                        values = c("blue", "red"),
                        labels = c("Not Outsourced", "Outsourced")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
      theme_light(base_size = 16)

    ggsave(str_c(figure_folder, var_g[i], "_", sex_save[sex + 1],
                 ".pdf"),
           height = height, width = width)
    
    # Just workers in outsourcing occupations
    temp <- transition %>%
      filter(female == sex, !is.na(.[[var_g[i]]]), outsourcing_occ,
             .[[var_g[i]]] < quantile(.[[var_g[i]]], .99), .[[var_g[i]]] > 0) %>%
      ggplot(aes_string(var_g[i], fill = "factor(outsourced_curr)")) +
      geom_density(alpha = 0.2) +
      labs(x = var_names[i], y = "Density") +
      scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                        values = c("blue", "red"),
                        labels = c("Not Outsourced", "Outsourced")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
      scale_x_continuous(expand = expand_scale(add = c(2, 2))) +
      theme_light(base_size = 16)
    
    ggsave(str_c(figure_folder, var_g[i], "_", sex_save[sex + 1],
                 "_out_occ.pdf"),
           height = height, width = width)
  }
}

##################################################################

# Look at highest tenure of each job for outsourced vs not in 
# outsourcing occupations

tenure_summary <- timeline %>% 
  filter(!is.na(tenure), outsourcing_occ) %>% 
  group_by(case_id, emp_id) %>% 
  filter(tenure == max(tenure)) %>%
  as_survey_design(ids = case_id, weights=weight) %>%
  group_by(female, outsourced) %>% 
  summarise(tenure_mean = survey_mean(tenure, vartype = "ci"),
            tenure_median = survey_median(tenure))

# Plot this distribution for men and women
sex_save <- c("men", "women")

for (sex in c(0, 1)){
  
  # All occupations
  temp <- timeline %>%
    filter(female == sex, !is.na(tenure)) %>%
    group_by(case_id, emp_id) %>% 
    filter(tenure == max(tenure)) %>%
    ggplot() +
    geom_density(aes(tenure, fill = factor(outsourced)), alpha = 0.2) +
    labs(x = "Weeks Tenure", y = "Density") +
    scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = c("Not Outsourced", "Outsourced")) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    theme_light(base_size = 16)
  
  ggsave(str_c(figure_folder, "tenure_", sex_save[sex + 1],
               ".pdf"),
         height = height, width = width)
  
  # Just workers in outsourcing occupations
  temp <- timeline %>%
    filter(female == sex, !is.na(tenure), outsourcing_occ) %>%
    group_by(case_id, emp_id) %>% 
    filter(tenure == max(tenure)) %>%
    ggplot() +
    geom_density(aes(tenure, fill = factor(outsourced)), alpha = 0.2) +
    labs(x = "Weeks Tenure", y = "Density") +
    scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = c("Not Outsourced", "Outsourced")) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    theme_light(base_size = 16)
  
  ggsave(str_c(figure_folder, "outsourcing_occ_tenure_", sex_save[sex + 1],
               ".pdf"),
         height = height, width = width)
}

######################################################################

# What is distribution of log wages each year for outsourced vs
# non-outsourced jobs in outsourcing occupations
timeline_yearly <- timeline %>% 
  filter(!is.na(outsourced), !is.na(w_year), outsourcing_occ, week <= week_max) %>%
  group_by(case_id, emp_id, w_year) %>% 
  # Keep only last week observed each year
  filter(week == max(week))

ss_yearly <- timeline_yearly %>% 
  filter(!is.na(log_real_wkly_wage)) %>% 
  as_survey_design(id = case_id, weight = weight) %>% 
  group_by(female, w_year, outsourced) %>% 
  summarise(
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage),
    log_reak_wkly_wage_sd = survey_sd(log_real_wkly_wage),
    obs = unweighted(n()),
    log_real_wkly_wage_skew = unweighted(Skew(log_real_wkly_wage))
  )

temp <- timeline_yearly %>% 
  filter(outsourced == 0, !is.na(log_real_wkly_wage),
         year(w_year) %in% seq(2002, 2014, by = 4)) %>% 
  ggplot(aes(log_real_wkly_wage, fill = factor(w_year))) +
  geom_density(alpha = 0.2) +
  labs(x = "Log Real Weekly Wages", y = "Density") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  theme_light(base_size = 16)

temp <- timeline_yearly %>% 
  filter(outsourced == 1, !is.na(log_real_wkly_wage),
         year(w_year) %in% seq(2002, 2014, by = 4)) %>%
  ggplot(aes(log_real_wkly_wage, fill = factor(w_year))) +
  geom_density(alpha = 0.2) +
  labs(x = "Log Real Weekly Wages", y = "Density") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  theme_light(base_size = 16)

# #####################################################################
# # Run regressions on transitions

top_r <- str_c(table_top, siunitx, 
"
\\begin{tabular}{lSSSSSS}
\\toprule
          & {OLS} & {OLS}      & {OLS}      & {FE}  & {FE}       & {FE} \\\\
Variables &       & {Job Info} & {Occ FE}   &       & {Job Info} & {Occ FE} \\\\\\midrule
"
)

# Run separately on men and women
sex_names <- c("men", "women")

# Type of current job (compare to traditional)
curr_types <- c("outsourced_curr", "self_emp_curr", "indep_con_curr",
                "temp_work_curr", "on_call_curr")

# Type of previous job
last_types <- c("outsourced_last", "self_emp_last", "indep_con_last",
                "temp_work_last", "on_call_last")

# Demographic Controls
dem_controls <- c("age", "age_2", "age_3", "age_4", "tot_child", "hh_child")

# Current job control
curr_controls <- c("hours_week_curr", "part_time_curr", "union__curr", 
                   "health_curr", "retirement_curr",
                   "any_benefits_curr","log_real_wkly_wage_curr")

# Last job controls
last_controls <- c("hours_week_last", "part_time_last", "union__last", 
                   "health_last", "retirement_last",
                   "any_benefits_last", "log_real_wkly_wage_last",
                   "tenure_last", "tenure_2_last", "tenure_3_last", "tenure_4_last")

# OLS controls
ols_controls <- c("black", "hispanic", "hs", "aa", "ba", "plus_ba")

# Fixed Effects for all regresssions
fixed_effects <- c("year", "region", "marital_status", "msa")

# Occupation fixed effects
occ_effects <- c("occ_curr", "occ_last")

# Individual fixed effects
id_effects <- c("case_id")

vars_1 <- list(curr_types, last_types, dem_controls)
vars_2 <- append(vars_1, list(curr_controls, last_controls))
var_list <- list(vars_1, vars_2, vars_2)

job_t <- c("No", "Yes", "Yes")
occ_t <- c("No", "No", "Yes")
worker_t <- c("No", "Yes")

for (sex in c(0, 1)) {
  
  center_r <- rbind("Outsourced", "Current", "Outsourced", "Previous",
                    "Job Info", "Occupation FE", "Worker FE", "$R^2$", "Obs")
  
  for (j in 1:2){
    for (i in 1:3) {
      
      eq <- create_formula("weeks_between_jobs_2", var_list[[i]])
      
      fe_list <- fixed_effects
      
      if (i == 3) {
        fe_list %<>% append(occ_effects)
      }
      
      if (j == 2) {
        fe_list %<>% append(id_effects)
      }
      
      fe <- create_formula("", fe_list)
      
      temp <- lm_robust(
        eq, data = transition, 
        subset = (female == sex & !is.na(marital_status)
                  & !is.na(msa) & !is.na(occ_curr) & !is.na(occ_last)),
        weights = weight,
        fixed_effects = !!fe,
        clusters = as_factor(sample_id), se_type = "stata", try_cholesky = T)
      
      # Put regession results in Tables
      center_r %<>% cbind(
        rbind(format_val(temp$coefficients["outsourced_curr"],
                         p_stars(temp$p.value["outsourced_curr"])),
              format_se(temp$std.error["outsourced_curr"]),
              format_val(temp$coefficients["outsourced_last"],
                         p_stars(temp$p.value["outsourced_last"])),
              format_se(temp$std.error["outsourced_last"]),
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
                     act = model.frame(temp)$weeks_between_jobs_2) 
        
        temp_plot <- df %>% 
          filter(act < quantile(act, .99), act != est) %>% 
          ggplot() +
          geom_abline(slope = 1, intercept = 0, color = "blue") +
          geom_jitter(aes(act, est)) +
          labs(x = "Actual Weeks to Find Job", y = "Estimated Weeks to Find Job") +
          theme_light(base_size = 16) 
        
        ggsave(str_c(figure_folder, "Weeks to Find Job Actual v Estimated ",
                     str_to_title(sex_names[sex + 1]), ".pdf"),
               height = height, width = width)
        
        temp_plot <- df %>% 
          filter(act < 100, act != est, act > 0, est > 0) %>% 
          ggplot() +
          geom_density(aes(est, fill = "red"), alpha = 0.2, show.legend = T) +
          geom_density(aes(act, fill = "blue"), alpha = 0.2, show.legend = T) +
          labs(x = "Weeks to Find Job", y = "Density") +
          scale_fill_manual(name = "Weeks to\nFind Job", breaks = c("blue", "red"),
                            values = c("blue", "red"),
                            labels = c("Actual", "Estimated")) +
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
          theme_light(base_size = 16) 
          
          ggsave(str_c(figure_folder, "Weeks to Find Job Actual v Estimated Dist ",
                       str_to_title(sex_names[sex + 1]), ".pdf"),
                 height = height, width = width)
      }
      
    }
  }
  
  center_r %<>% cbind( 
    rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\[2pt]",
          "\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]")
    )
  
  # Do weird stuff to create LaTeX output
  r_folder <- str_c(table_folder, "Junk/")
  file_1 <- str_c(r_folder, "center_t_r.txt")
  file_2 <- str_c(r_folder, "center_t_r.txt")
  write.table(center_r, file_1, quote=T, col.names=F, row.names=F)
  center_r <- read.table(file_1, sep = "")
  write.table(center_r, file_2, quote=F, col.names=F, row.names=F, sep = "")
  center_r <- readChar(file_2, nchars = 1e6)
  
  bot_r <- str_c(
"\\bottomrule
\\end{tabular}
\\caption{Regressions of outsourced at current and previous job on 
weeks to find current job for ", sex_names[sex + 1], ". All regressions
include controls for current and previous job type (reported coefficients are 
compared to traditional jobs), a quartic in age, dummies for region,
whether in an MSA or central city, marital status, number of children total
and in household, and dummies for observation year. The first three columns 
run OLS and also contain controls for race and education. The last three columns
use worker fixed effects. Regression with job info contain current and previous
hours worked per week, part-time status, log real weekly wage, union status, and
whether recieved health insurance, retirement benefits, or any benefits. They also
contain a quartic of previous tenure. Regrssions with occupation fixed effects
contain fixed effects for current and previous occupation.
All observations are at the person-job level and all standard errors are
clustered by demographic sampling group.
Stars represent significant at the .10 level *, .05 level **, and .01 level ***.}
\\label{reg_weeks_between_jobs_", sex_names[sex + 1], "}
\\end{table}
\\end{document}"
)
  
  write.table(str_c(top_r, center_r, bot_r),
              str_c(table_folder, 
                    "NLSY79 Job Transitions Fine/NLSY79 Weeks to Find Job Regressions ",
                    str_to_title(sex_names[sex + 1]), ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
}


# Outsourcing vs Occupation Characteristics -------------------------------

# How does outsourcing percent in an occupation correlate with occupation
# characteristics like wages, hours worked, and benefits?


