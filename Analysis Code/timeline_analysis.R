# This file takes data from matched_timeline
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

# # Dataset is large, need to read as data table
# timeline <- fread(str_c(clean_folder, "matched_timeline.csv"), 
#                   colClasses = list(
#                     double = c(1, 3:52, 55:58),
#                     Date = c("week", "week_start_job", "week_end_job")
#                   ))

# fread having trouble with dates, so use read_csv
timeline <- read_csv(str_c(clean_folder, "matched_timeline.csv"),
                     col_types = cols(
                       .default = col_double(),
                       week = col_date(format = ""),
                       week_start_job = col_date(format = ""),
                       week_end_job = col_date(format = "")
                     )) %>% 
  data.table()

# Create w_month and w_year from week in timeline (drop first observation
# that looks like it's in 2000)
timeline[week > min(week), `:=`(month = round_date(week, unit = "month"),
                                year = round_date(week, unit = "year"))]

# # If I want a quick run through
# timeline <- timeline[week < ymd("2003-09-11")]

# Some figures calculated are useful for calbirating the model.
# Use these to update model_parameters
model_parameters <- read_csv(str_c(clean_folder, "model_parameters.csv"),
                             col_types = cols(
                               variable = col_character(),
                               value = col_double()
                             ))

# Create a function to update model_parameters given variable
# with correct name
update_parameters <- function(name, val) {
  model_parameters$value[model_parameters$variable == name] <- val
  model_parameters
}

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


# Plot Timelines ----------------------------------------------------------

# Plot time series by Weeks, Months, Year, Age, and Weeks for an age range
# 1. Percent of workers outsourced 
# 2. Percent of workers in HO occupations 
# 3. Percent of workers in HO occupations  who are outsourced 
# 4. Log weekly wages of outsourced v traditional workers in HO occupations  
# 5. Weeks of tenure of outsourced v traditional workers in HO occupations 
# 6. Number of workers unemployed based on ever HO occupations 
# 7. Number of workers not working based on ever HO occupations 
# 8. Number of workers in all job types

age_min <- 43
age_max <- 47
vars_time <- c("week", "month", "year", "age", "week")
vars_save <- c("Week", "Month", "Year", "Age", 
               str_c("Week Ages ", age_min, "-", age_max))
vars_label <- c("Year", "Year", "Year", "Age", "Year")

# Create a function to filter ages based on index
filter_ages <- function(df, i) {
  if (i == 5) {
    filter(df, age >= age_min, age <= age_max)
  } else {
    df
  }
}

# Create a function scale which takes index i and returns an
# x_scale_date for week, month and year and an 
# x_scale_continuous for age
scale <- function(i) {
  if (i == 4){
    scale_x_continuous(breaks = seq(min(timeline$age, na.rm = T),
                                    max(timeline$age, na.rm = T), by = 2))
  } else {
    scale_x_date(date_breaks = "2 years", date_labels = "%Y")
  }
}

for (i in seq_along(vars_time)) {
  
  # 1. Percent of workers outsourced
  temp <- timeline %>% 
    filter(!is.na(outsourced), !is.na(.[[vars_time[i]]])) %>% 
    filter_ages(i) %>% 
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
  
  ggsave(str_c(figure_folder, "Outsourced ", vars_save[i], ".pdf"),
         height = height, width = width)
  
  # 2. Percent of workers in HO occupations 
  temp <- timeline %>% 
    filter(!is.na(ho_occ), !is.na(.[[vars_time[i]]])) %>% 
    filter_ages(i) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(ever_ho_occ_per = survey_mean(ever_ho_occ * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "ever_ho_occ_per"), color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "ever_ho_occ_per_upp"),
              linetype="dashed", color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "ever_ho_occ_per_low"), 
              linetype="dashed", color = "red") +
    labs(x = vars_label[i], y = "% in HO Occupations") +
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "HO Occupation ", vars_save[i], ".pdf"),
         height = height, width = width)
  
  # 3. Percent of workers ever in HO occupations who are outsourced 
  temp <- timeline %>% 
    filter(ever_ho_occ, !is.na(outsourced), !is.na(.[[vars_time[i]]])) %>% 
    filter_ages(i) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per"), color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp"),
              linetype="dashed", color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low"), 
              linetype="dashed", color = "red") +
    labs(x = vars_label[i], y = "% Outsourced Ever in HO Occupation") +
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "Outsourced Ever HO Occupations ",
               vars_save[i], ".pdf"),
         height = height, width = width)
  
  # 4. Log weekly wages of outsourced v traditional workers ever in HO occupations  
  temp <- timeline %>% 
    filter(!is.na(log_real_wkly_wage), ever_ho_occ, !is.na(.[[vars_time[i]]])) %>% 
    filter_ages(i) %>% 
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
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "Ever HO Occupation Weekly Wages ", vars_save[i], ".pdf"),
         height = height, width = width)

  # # 5. Weeks of tenure of outsourced v traditional workers in HO occupations
  # temp <- timeline %>%
  #   filter(!is.na(w_tenure), ho_occ, !is.na(.[[vars_time[i]]])) %>%
  #   filter_ages(i) %>% 
  #   as_survey_design(ids = case_id, weights=weight) %>%
  #   group_by_at(vars_time[i]) %>%
  #   group_by(outsourced, add = T) %>%
  #   summarise(tenure_mean =
  #               survey_mean(w_tenure, vartype = "ci")) %>%
  #   ggplot() +
  #   geom_line(aes_string(x = vars_time[i], y = "tenure_mean",
  #                        color = "factor(outsourced)")) +
  #   geom_line(aes_string(x = vars_time[i], y = "tenure_mean_upp",
  #                        color = "factor(outsourced)"), linetype="dashed") +
  #   geom_line(aes_string(x = vars_time[i], y = "tenure_mean_low",
  #                        color = "factor(outsourced)"), linetype="dashed") +
  #   labs(x = vars_label[i], y = "Weeks of Tenure") +
  #   scale_color_manual(name = "Outsourced", breaks = c(0, 1),
  #                      values = c("blue", "red"),
  #                      labels = c("Not Outsourced", "Outsourced")) +
  #   scale(i) +
  #   theme_light(base_size = 16)
  # 
  # ggsave(str_c(figure_folder, "Ever HO Occupation Tenure ", vars_save[i], ".pdf"),
  #        height = height, width = width)

  # 6. Number of workers unemployed based on ever HO occupations 
  temp <- timeline %>% 
    filter(!is.na(.[[vars_time[i]]])) %>% 
    filter_ages(i) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    # Code is not allowing me to group by both at once for some reason
    group_by_at(vars_time[i]) %>% 
    group_by(ever_ho_occ, add = T) %>% 
    summarise(unemployed_per = survey_mean(unemployed, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "unemployed_per",
                         color = "factor(ever_ho_occ)")) +
    geom_line(aes_string(x = vars_time[i], y = "unemployed_per_upp",
                         color = "factor(ever_ho_occ)"), linetype="dashed") +
    geom_line(aes_string(x = vars_time[i], y = "unemployed_per_low",
                         color = "factor(ever_ho_occ)"), linetype="dashed") +
    labs(x = vars_label[i], y = "Unemployment Rate") +
    scale_color_manual(name = "Ever HO Occ", breaks = c(0, 1),
                       values = c("blue", "red"),
                       labels = c("Never", "Ever")) +
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "Unemployed ", vars_save[i], ".pdf"),
         height = height, width = width)
  
  # 7. Number of workers not working based on ever HO occupations 
  temp <- timeline %>% 
    filter(!is.na(.[[vars_time[i]]])) %>% 
    filter_ages(i) %>% 
    as_survey_design(ids = case_id, weights = weight) %>% 
    # Code is not allowing me to group by both at once for some reason
    group_by_at(vars_time[i]) %>% 
    group_by(ever_ho_occ, add = T) %>% 
    summarise(not_working_per = survey_mean((1 - working) * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "not_working_per",
                         color = "factor(ever_ho_occ)")) +
    geom_line(aes_string(x = vars_time[i], y = "not_working_per_upp",
                         color = "factor(ever_ho_occ)"), linetype="dashed") +
    geom_line(aes_string(x = vars_time[i], y = "not_working_per_low",
                         color = "factor(ever_ho_occ)"), linetype="dashed") +
    labs(x = vars_label[i], y = "Percent Not Working") +
    scale_color_manual(name = "Ever HO Occ", breaks = c(0, 1),
                       values = c("blue", "red"),
                       labels = c("Never", "Ever")) +
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "Not Working ", vars_save[i], ".pdf"),
         height = height, width = width)
  
  # 8. Number of workers in all job types
  breaks <- c("outsourced_per", "indep_con_per", "temp_work_per", "on_call_per")
  labels <- c("Outsourced", "Independent Contractor", "Temp Worker", "On-Call Worker")
  colors <- c( "blue", "dark green", "red", "purple")
  
  temp <- timeline %>% 
    filter(!is.na(outsourced), !is.na(indep_con), 
           !is.na(temp_work), !is.na(on_call), 
           !is.na(.[[vars_time[i]]])) %>% 
    filter_ages(i) %>% 
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
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "All Types ", vars_save[i], ".pdf"),
         height = height, width = width)
}

# Try to free some space to run all at once
rm("temp", "breaks", "labels", "colors", "filter_ages", "scale")

# Weekly Characteristics -------------------------------------------

# Create a table with average weekly charactersitcs
# for each week by ever_ho_occ

weekly_ss <- timeline %>%
  as_survey_design(id = case_id, weight = weight) %>% 
  # Create new_job = 1 if w_tenure = 1 else NA
  mutate(new_job_o = ifelse(w_tenure == 1, 1, NA)) %>% 
  group_by(week, ever_ho_occ) %>% 
  summarise(
    not_working = survey_mean(1 - working, na.rm = T),
    unemployed = survey_mean(unemployed, na.rm = T),
    outsourced = survey_mean(outsourced, na.rm = T),
    new_outsourced = survey_mean(new_job_o * outsourced, na.rm = T),
    new_job = survey_mean(w_tenure == 1, na.rm = T),
    weight_w = unweighted(sum(weight) / 1000)
  ) %>% 
  as_survey_design(id = week, weight = weight_w) %>%
  group_by(ever_ho_occ) %>% 
  summarise(
    not_working = survey_mean(not_working, na.rm = T),
    unemployed = survey_mean(unemployed, na.rm = T),
    outsourced = survey_mean(outsourced, na.rm = T),
    new_outsourced = survey_mean(new_outsourced, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(desc(ever_ho_occ))

# Plot in Latex
table <- str_c(table_top, siunitx,
               "
\\begin{tabular}{lSS}
\\toprule
& {Ever HO Occ} & {Never HO Occ}   \\\\  \\midrule \n
"
)

vars <- c("unemployed", "not_working", "outsourced", "new_outsourced")
var_names <- c("Unemployed", "Disemployed", "Outsourced", "New Jobs Outsourced")

for (i in seq_along(vars)) {
  var <- vars[i]
  se <- str_c(var, "se", sep = "_")
  stars <- test(weekly_ss, var, "n", 1, 2, "prop")
  table %<>% str_c(
    var_names[i], 
    format_val(weekly_ss[[var]][1], r = 4, s = 4),
    format_val(weekly_ss[[var]][2], star = stars, r = 4, s = 4), "\\\\ \n",
    format_se(weekly_ss[[se]][1], r = 4, s = 4),
    format_se(weekly_ss[[se]][2], r = 4, s = 4), "\\\\ \n")
}

table %<>% str_c(
  "Observations", format_n(weekly_ss$n[1]), format_n(weekly_ss$n[2]), "\\\\ \n",
  "\\bottomrule
\\end{tabular}
\\caption{Average weekly unemployment and disemployment rates based on
if worker is ever employed in a high outsourcing occupation (HO Occ).
Stars represent
significant difference from ever HO Occupation at the .10 level *,
.05 level **, and .01 level ***.}
\\label{unemp}
\\end{table}
\\end{document}")

write.table(table,
            str_c(table_folder,
                  "NLSY79 HO Occupations/Unemployment.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Use ever_ho_occ to update u, zeta, pi using unemployed, outsourced, and 
# new_outsourced
model_parameters <- update_parameters("u", weekly_ss$unemployed[1])
model_parameters <- update_parameters("zeta", weekly_ss$outsourced[1])
model_parameters <- update_parameters("pi", weekly_ss$new_outsourced[1])
  
write_csv(model_parameters, str_c(clean_folder, "model_parameters.csv"))


# Correlation -------------------------------------------------------------

# Group occupations by week and compare trend in employment to 
# trend in outsourcing

# Unweighted
occ_timeline <- timeline %>%
  filter(!is.na(occ), !is.na(outsourced)) %>%
  group_by(week, occ, outsourced) %>%
  summarise(
    ho_occ = mean(ho_occ),
    n = sum(weight),
    log_real_wkly_wage = mean(log_real_wkly_wage, na.rm = T),
    hours_week = mean(hours_week, na.rm = T),
    part_time = mean(part_time, na.rm = T),
    tenure = mean(tenure, na.rm = T),
    health = mean(health, na.rm = T),
    retirement = mean(retirement, na.rm = T),
    any_benefits = mean(any_benefits, na.rm = T),
    n_black = sum(black * weight),
    n_hispanic = sum(hispanic * weight),
    n_union = sum(union * weight, na.rm = T),
    n_union_defined = sum((!is.na(union) %in% T) * weight),
    tot_age = sum(age * weight),
    tot_age_2 = sum((age ^ 2) * weight)
  ) %>%
  pivot_wider(names_from = outsourced,
              values_from = n:tot_age_2) %>%
  ungroup() %>% 
  mutate(
    workers = r_sum(n_0, n_1),
    outsourced_per = ifelse(!is.na(n_1), n_1 / workers * 100, 0),
    black_per = r_sum(n_black_0, n_black_1) / workers * 100,
    hispanic_per = r_sum(n_hispanic_0, n_hispanic_1) / workers * 100,
    union_per = (r_sum(n_union_0, n_union_1) / 
                   r_sum(n_union_defined_0, n_union_defined_1) * 100),
    age = r_sum(tot_age_0, tot_age_1) / workers,
    age_2 = r_sum(tot_age_2_0, tot_age_2_1) / workers 
  ) %>% 
  group_by(week) %>% 
  mutate(workers_per = workers / sum(workers) * 100) %>% 
  ungroup()
  
# Save occ_timeline in cleaned_data to compare with cps data
write_csv(occ_timeline, str_c(clean_folder, "occ_timeline.csv"))

occ_corr <- occ_timeline %>% 
  group_by(occ) %>%
  summarise(
    corr_emp = cor(outsourced_per, workers_per, use = "na.or.complete"),
    corr_out_wages = 
      cor(outsourced_per[which(log_real_wkly_wage_1 > 0)],
          log_real_wkly_wage_1[which(log_real_wkly_wage_1 > 0)], 
          use = "na.or.complete"),
    corr_wages = cor(outsourced_per, log_real_wkly_wage_0, use = "na.or.complete"),
    ho_occ = mean(ho_occ),
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
  group_by(ho_occ) %>% 
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
  arrange(ho_occ)

# # # Weighted (not working)
# occ_timeline <- timeline %>%
#   filter(!is.na(occ), !is.na(outsourced), occ < 100) %>%
#   as_survey_design(ids = case_id, weights=weight) %>%
#   group_by(week, occ, outsourced) %>%
#   summarise(
#     ho_occ = unweighted(mean(ho_occ)),
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
bins <- c(15, 15, 15)

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
    geom_histogram(aes_string(corr, fill = "factor(ho_occ)"),
                   alpha = 0.4, bins = bins[i]) +
    geom_vline(aes(xintercept = corr_all), size=1) +
    geom_vline(aes(xintercept = corr_ho), color = "red", size=1) +
    geom_vline(aes(xintercept = corr_lo), color = "blue", size=1) +
    labs(x = "Correlation", y = "Count") +
    scale_fill_manual(name = "Occupation\nOutsourcing", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = list(expression("Low (" < " 3.4%)"), 
                                    expression("High (" >= " 3.4%)"))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, var_names[i], " Correlation.pdf"),
         height = height, width = width)
  
  # What about correlation weighted by occupation size?
  temp <- occ_corr %>% 
    filter(!is.na(.[[corr]])) %>% 
    ggplot() +
    geom_point(aes_string(x = corr, y = "workers_mean",
                          color = "factor(ho_occ)"), alpha = 0.4) +
    geom_vline(aes(xintercept = w_corr_all), size=1) +
    geom_vline(aes(xintercept = w_corr_ho), color = "red", size=1) +
    geom_vline(aes(xintercept = w_corr_lo), color = "blue", size=1)  +
    labs(x = "Correlation", y = "Average Number of Workers")+
    scale_color_manual(name = "Occupation\nOutsourcing", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = list(expression("Low (" < " 3.4%)"), 
                                    expression("High (" >= " 3.4%)"))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, var_names[i], " Weighted Correlation.pdf"),
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
                  "NLSY79 Occupation Info/Occupation Correlation.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# How do these correlations change with controls? Run regressions
controls <- c("outsourced_per", "black_per", "hispanic_per",
              "union_per", "age", "age_2")

fixed_effects <- c("week", "occ")
fe <- create_formula("", fixed_effects)

outcomes <- c("log_real_wkly_wage", "tenure", "part_time", "hours_week",
              "health", "retirement", "any_benefits")

outcome_names <- c("Log Real Weekly Wage", "Weeks Tenure", "Part-Time Status",
                   "Hours Worked", "Health Insurance", "Retirement Benefits",
                   "Any Benefits")

# Create a table with regression results, starting with workers_per
top <- str_c(table_top, siunitx, 
"
\\begin{tabular}{lSS}
\\toprule
Dependent Variable & {Outsourced Percent} & {Dependent Mean}   \\\\  \\midrule
"
)

eq <- create_formula("workers_per", controls)

temp <- lm_robust(eq, data = occ_timeline, fixed_effects = !!fe,
                  weights = workers, clusters = occ, 
                  se_type = "stata", try_cholesky = T)

t_mean <- weighted.mean(occ_timeline$workers_per, occ_timeline$workers) 

center <- rbind(
  cbind("Percent of Workers", 
        format_val(temp$coefficients["outsourced_per"], 
                   p_stars(temp$p.value["outsourced_per"]), r = 4, s = 4),
        format_val(t_mean)
        ),
  cbind("in Occupation",
        format_se(temp$std.error["outsourced_per"], r = 4, s = 4),
        " & "
  )
  )

for (out in 0:1){
  for (i in seq_along(outcomes)){
    
    # Set heading for section
    if (i == 1) {
      if (out == 0) {
        center %<>% rbind(
          cbind("Non-Outsourced Jobs", " & ", " & " )
        )
      } else {
        center %<>% rbind(
          cbind("Outsourced Jobs", " & ", " & " )
        )
      }
    }
      
    outcome <- str_c(outcomes[i], "_", out)
    eq <- create_formula(outcome, controls)
    
    temp <- lm_robust(eq, data = occ_timeline, fixed_effects = !!fe,
                         weight = workers, clusters = occ,
                         se_type = "stata", try_cholesky = T)
      
    t_mean <- weighted.mean(occ_timeline[[outcome]], occ_timeline$workers,
                            na.rm = T) 
    
    center %<>% rbind(
      cbind(outcome_names[i], 
            format_val(temp$coefficients["outsourced_per"], 
                       p_stars(temp$p.value["outsourced_per"]), r = 4, s = 4),
            format_val(t_mean)
      ),
      cbind("",
            format_se(temp$std.error["outsourced_per"], r = 4, s = 4),
            " &"
      )
    )
  }
}

center %<>% cbind(
  rbind("\\\\", "\\\\[2pt] \\midrule", "\\\\[2pt]", "\\\\", "\\\\[2pt]", 
        "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
        "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt] \\midrule",
        "\\\\[2pt]", "\\\\", "\\\\[2pt]", 
        "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
        "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]"
        )
)

bot <- str_c(
"\\bottomrule
\\end{tabular}
\\caption{Occupation level regressions of percent outsourced each week on
average job characteristics. Each regression contains controls for percent 
Black, Hispanic, and union member, average age and age squared, and fixed effects
at the occupation and week level. Outcomes include percent of jobs in the occupation
and average job characteristics of non-outsourced and outsourced jobs.
Regressions are weighted by number of observations and robust standard errors
are clustered at the occupation level. Stars represent
significant difference from 0 at the .10 level *, .05 level **, and .01 level ***.}
\\label{occ_corr_reg}
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
                   "NLSY79 Occupation Info/Occupation Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Individual's Percent Outsourcing ----------------------------------------

# For workers ever outsourced, what percent of weeks are in outsourcing
# vs non-outsourcing jobs?

outsourcing_per <- timeline %>% 
  filter(ever_out_oj == 1, !is.na(outsourced)) %>% 
  group_by(case_id) %>% 
  summarise(
    outsourcing_per = mean(outsourced) * 100,
    ever_ho_occ = mean(ever_ho_occ),
    weight = mean(weight)
  ) %>% 
  ungroup() %>% 
  mutate(weight = weight / sum(weight))

outsourcing_per_mean <- weighted.mean(outsourcing_per$outsourcing_per, 
                                      outsourcing_per$weight)

temp <- outsourcing_per %>% 
  ggplot() +
  geom_density(aes(outsourcing_per, weight = weight), 
               alpha = 0.4, fill = "blue", bounds = (0, 100)) +
  geom_vline(aes(xintercept = outsourcing_per_mean), size = 1) +
  geom_text(aes(x = outsourcing_per_mean + 1, y = .016, hjust = 0,
                label = str_c("Mean = ", round(outsourcing_per_mean, 2))),
            size = 6) +
  labs(x = "Percent of Weeks Worked Outsourced", y = "Density") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_light(base_size = 16) 
  
ggsave(str_c(figure_folder, "Percent Weeks Worked Outsourced.pdf"),
       height = height, width = width) 


# Max Tenure Distribution -------------------------------------------------------

# Look at highest tenure of each job for outsourced vs not overall and in 
# outsourcing occupations
tenure_summary <- split_data(timeline)

for (i in 1:2) {
  tenure_summary[[i]] %<>% 
    filter(!is.na(tenure)) %>% 
    group_by(case_id, emp_id) %>% 
    filter(tenure == max(w_tenure)) %>%
    as_survey_design(ids = case_id, weights=weight) %>%
    group_by(outsourced) %>% 
    summarise(tenure_mean = survey_mean(tenure, vartype = "ci"),
              tenure_median = survey_median(tenure)) %>% 
    arrange(desc(outsourced))
}

save_ho <- c("", " HO Ocupations")
time_plot <- split_data(timeline)  

# Plot Max Tenure
for (ho in 1:2) {
  temp <- time_plot[[ho]] %>%
    filter(!is.na(tenure)) %>%
    group_by(case_id, emp_id) %>% 
    filter(tenure == max(tenure)) %>%
    ggplot() +
    geom_density(aes(tenure, fill = factor(outsourced)), alpha = 0.2,
                 bounds = c(0, Inf)) +
    geom_vline(aes(xintercept = tenure_summary[[i]]$tenure_mean[1]),
               color = "red", size=1) +
    geom_vline(aes(xintercept = tenure_summary[[i]]$tenure_mean[2]),
               color = "blue", size=1) +
    labs(x = "Max Weeks Tenure", y = "Density") +
    scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = c("Not Outsourced", "Outsourced")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_light(base_size = 16)
  
  ggsave(str_c(figure_folder, "Max Tenure", save_ho[ho], ".pdf"),
         height = height, width = width)
}

# Use tenure_summary[[2]] (ever_ho_occ) to update tau and tau_o 
# (and the implied delta and delta_o)
# using median tenure for non-outsourced and outsourced workers
tau <- tenure_summary[[2]]$tenure_median[2]
tau_o <- tenure_summary[[2]]$tenure_median[1]
delta <- 1 - .5 ^ (1 / tau)
delta_o <- 1 - .5 ^ (1 / tau_o)

model_parameters <- update_parameters("tau", tau)
model_parameters <- update_parameters("tau_o", tau_o)
model_parameters <- update_parameters("delta", delta_o)
model_parameters <- update_parameters("delta_o", delta_o)

write_csv(model_parameters, str_c(clean_folder, "model_parameters.csv"))

# How do the model's implied job tenures look?

temp_plot <- time_plot[[ho]] %>%
  filter(!is.na(tenure)) %>%
  group_by(case_id, emp_id) %>% 
  filter(tenure == max(tenure))

max_w <- 1600
weeks <- 0:max_w
keep <- 1 - (1 - delta) ^ weeks
keep_o <- 1 - (1 - delta_o) ^ weeks

# Turn keep into length pdf and adjust for population size
length_pdf <- (keep[2:length(weeks)] - keep[1:(length(weeks) - 1)])
length_hist <- length_pdf * NROW(temp_plot)
length_o_pdf <- (keep_o[2:length(weeks)] - keep_o[1:(length(weeks) - 1)])
length_o_hist <- length_o_pdf * NROW(temp_plot)

d <- "N Out"
d_o <- "Out"
sim <- str_c("delta = ", round(delta, 3))
sim_o <- str_c("delta_o = ", round(delta_o, 3))
temp <- ggplot() +
  geom_density(aes(x = temp_plot$tenure[temp_plot$outsourced == 0],
                fill = d), fill = "blue", alpha = 0.3) +
  geom_density(aes(x = temp_plot$tenure[temp_plot$outsourced == 1],
                fill = d_o), fill = "red", alpha = 0.3) +
  geom_point(aes(x = 1:max_w, y = length_pdf, color = sim),
             alpha = 1) +
  geom_point(aes(x = 1:max_w, y = length_o_pdf, color = sim_o),
             alpha = 1) +
  labs(x = "Weeks Tenure", y = "Count") +
  scale_fill_manual(name = "Data", 
                    values = c("blue", "red"),
                    labels =c(d, d_o) ) +
  scale_color_manual((name = "Model"),
                     values = c("green4", "orange"),
                     labels = c(sim, sim_o)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_light(base_size = 16)

ggsave(str_c(figure_folder, "Max Tenure Data vs Model.pdf"),
       height = height, width = width)

# Log Weekly Wage Distribution --------------------------------------------

# What is distribution of log wages each year for outsourced vs
# non-outsourced jobs in outsourcing occupations
timeline_yearly <- timeline %>% 
  filter(!is.na(outsourced), !is.na(year), (ho_occ == 1)) %>%
  group_by(case_id, emp_id, year) %>% 
  # Keep only last week observed each year
  filter(week == max(week))

ss_yearly <- timeline %>% 
  filter(!is.na(log_real_wkly_wage)) %>% 
  as_survey_design(id = case_id, weight = weight) %>% 
  group_by(year, outsourced) %>% 
  summarise(
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage),
    log_reak_wkly_wage_sd = survey_sd(log_real_wkly_wage),
    obs = unweighted(n()),
    log_real_wkly_wage_skew = unweighted(Skew(log_real_wkly_wage))
  )

temp <- timeline_yearly %>% 
  filter(outsourced == 0, !is.na(log_real_wkly_wage),
         year(year) %in% seq(2002, 2014, by = 4)) %>% 
  ggplot(aes(log_real_wkly_wage, fill = factor(year))) +
  geom_density(alpha = 0.2) +
  labs(x = "Log Real Weekly Wages", y = "Density") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_light(base_size = 16)

temp <- timeline_yearly %>% 
  filter(outsourced == 1, !is.na(log_real_wkly_wage),
         year(year) %in% seq(2002, 2014, by = 4)) %>%
  ggplot(aes(log_real_wkly_wage, fill = factor(year))) +
  geom_density(alpha = 0.2) +
  labs(x = "Log Real Weekly Wages", y = "Density") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_light(base_size = 16)

# Outsourcing vs Occupation Characteristics -------------------------------

# How does outsourcing percent in an occupation correlate with occupation
# characteristics like wages, hours worked, and benefits?


