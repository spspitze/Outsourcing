# This file tests different ways of matching data

rm(list = ls())

library(outsourcing)
library(zeallot)
library(ipumsr)
library(data.table)
library(readxl)
library(estimatr)
library(data.table)
library(openxlsx)
library(srvyr)
library(lubridate)
library(tidyverse)

# Folders of interest
folders <- name_folders("NLSY 79 Timeline")
c(raw_folder, clean_folder, table_folder, figure_folder,
  d_table_folder, s_table_folder) %<-% folders 

# For saving graphs
c(height, width) %<-% fig_size()

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

# Timeline data (make sure week is a date)
timeline <- fread(str_c(clean_folder, "timeline_clean.csv"), 
                  colClasses = list(
                    double = c(1:2, 4:5),
                    Date = c("week")
                  ))

# Merge demographic info + weights first
demographics <- read_csv(str_c(clean_folder, "demographics_clean.csv"),
                         col_types = cols(.default = col_double())) %>% 
  group_by(case_id) %>% 
  filter(int_year == min(int_year)) %>% 
  select(case_id:birth_year, less_hs:educ_other) %>% 
  select(-female)

# Merge demographic data into weights
weights <- read_table2(str_c(raw_folder, "customweight.dat"),
                       col_types = cols(.default = col_double())) %>% 
  rename(case_id = `1`,
         weight = `0`) %>% 
  left_join(demographics, by = "case_id") %>% 
  data.table()

timeline <- weights[timeline, on = .(case_id == case_id)]
timeline <- timeline[, `:=`(age = year(week) - birth_year,
                            birth_year = NULL)]

rm(demographics, weights)

# Create week_start/end_match to keep original data
matched %<>% 
  select(case_id, emp_id, hours_week, occ, ind, tenure, 
         week_start_job, week_end_job,
         log_real_wkly_wage, self_emp:traditional, ho_occ,
         part_time, any_benefits, health, retirement, union) %>%
  mutate(week_start_match = week_start_job,
         week_end_match = week_end_job) %>% 
  data.table()

matched_jobs %<>% 
  select(case_id, emp_id, hours_week, occ, ind, tenure, 
         week_start_job, week_end_job, max_tenure, 
         log_real_wkly_wage, self_emp:traditional,
         ho_occ, any_benefits, health, retirement, 
         union, part_time) %>%
  mutate(week_start_match = week_start_job,
         week_end_match = week_end_job) %>% 
  data.table()


# Match to matched
week_max <- round_date(ymd("2016-10-08"), "week")
timeline <- timeline[(female == 0) & (week < week_max), ]
timeline <- timeline[, `:=`(week_match = week, female = NULL)]

timeline_1 <- matched[timeline,
                    on = .(case_id == case_id, 
                           week_start_match <= week_match,
                           week_end_match >= week_match), 
                    allow.cartesian = T]

timeline <- matched_jobs[timeline,
                      on = .(case_id == case_id, 
                             week_start_match <= week_match,
                             week_end_match >= week_match), 
                      allow.cartesian = T]

# If emp_id is matched to a week but working is 0, set job characteristics 
# to NA (esp emp_id and outsourced)
timeline_1 <- timeline_1[working == 0, 
                     c("emp_id", "outsourced", "tenure", 
                       "log_real_wkly_wage", "log_real_hrly_wage",
                       "hours_week", "part_time",
                       "week_start_job", "week_end_job", "indep_con",
                       "self_emp", "temp_work", "on_call",
                       "traditional")
                     := NA]

timeline <- timeline[working == 0, 
                     c("emp_id", "outsourced", "tenure",
                       "log_real_wkly_wage", "log_real_hrly_wage",
                       "hours_week", "part_time", "week_start_job",
                       "week_end_job", "indep_con", "self_emp", 
                       "temp_work", "on_call", "traditional")
                     := NA]

# Drop jobs held in same week. Keep jobs by largest/most
# 1. tenure
# 2. hours_week
# 3. log_real_wkly_wage
# Be careful not to drop non-workers
timeline <- timeline[, obs := sum(!is.na(emp_id)),
                     by = .(case_id, week)]
timeline_week_conflict <- timeline[obs > 1]

timeline_1 <- timeline_1[, obs := sum(!is.na(emp_id)),
                         by = .(case_id, week)]
timeline_week_conflict_1 <- timeline_1[obs > 1]

vars <- c("hours_week", "tenure", "log_real_wkly_wage", "occ")
for (var in vars) {
  timeline_week_conflict <-
    timeline_week_conflict[, `:=`(max = max(get(var), na.rm = T),
                                  non_na = sum(!is.na(get(var)))),
                           by = .(case_id, week)]
  timeline_week_conflict <- timeline_week_conflict[
    (get(var) == max) %in% T | (non_na == 0)]

  timeline_week_conflict_1 <-
    timeline_week_conflict_1[, `:=`(max = max(get(var), na.rm = T),
                                  non_na = sum(!is.na(get(var)))),
                           by = .(case_id, week)]
  timeline_week_conflict_1 <- timeline_week_conflict_1[
    (get(var) == max) %in% T | (non_na == 0)]
}

# If any remain, take lowest emp_id (and drop these varaibles)
timeline_week_conflict <-
  timeline_week_conflict[, max := min(emp_id, na.rm = T),
                         by = .(case_id, week)]
timeline_week_conflict <- timeline_week_conflict[(emp_id == max) %in% T]
timeline_week_conflict <- timeline_week_conflict[
  ,c("max", "non_na") := NULL]

timeline_week_conflict_1 <-
  timeline_week_conflict_1[, max := min(emp_id, na.rm = T),
                           by = .(case_id, week)]
timeline_week_conflict_1 <- timeline_week_conflict_1[
  (emp_id == max) %in% T]
timeline_week_conflict_1 <- timeline_week_conflict_1[
  ,c("max", "non_na") := NULL]

# Merge back into
timeline <- bind_rows(timeline[obs <= 1], timeline_week_conflict)
timeline_1 <- bind_rows(timeline_1[obs <= 1], timeline_week_conflict_1)

diff <- anti_join(timeline, timeline_1, 
                  by = c("case_id", "emp_id", "week", "occ"))
diff_1 <- anti_join(timeline_1, timeline, 
                    by = c("case_id", "emp_id", "week", "occ"))

# timeline <- anti_join(timeline, diff, 
#                       by = c("case_id", "emp_id", "week"))
# timeline_1 <- anti_join(timeline_1, diff_1,
#                         by = c("case_id", "emp_id", "week"))

# timeline_1 <- semi_join(timeline_1, timeline, 
#                         by = c("case_id", "emp_id", "week", "occ", "outsourced"))
# timeline <- semi_join(timeline, timeline_1, 
#                       by = c("case_id", "emp_id", "week", "occ", "outsourced"))

# temp <- timeline %>%
#   filter(!is.na(outsourced), !is.na(week)) %>%
#   as_survey_design(ids = case_id, weights=weight) %>%
#   group_by(week) %>%
#   summarise(outsourced_per = 
#               survey_mean(outsourced * 100, vartype = "ci")) %>%
#   ggplot() +
#   geom_line(aes(x = week, y = outsourced_per), color = "red",
#             n = 4) +
#   geom_line(aes(x = week, y = outsourced_per_upp),
#             linetype="dashed", color = "red", n = 4) +
#   geom_line(aes(x = week, y = outsourced_per_low),
#             linetype="dashed", color = "red", n = 4) +
#   labs(x = "Week", y = "Percent Outsourced") +
#   theme_light(base_size = 16)
# 
# temp_1 <- timeline_1 %>%
#   filter(!is.na(outsourced), !is.na(week)) %>%
#   as_survey_design(ids = case_id, weights=weight) %>%
#   group_by(week) %>%
#   summarise(outsourced_per = survey_mean(outsourced * 100, 
#                                          vartype = "ci")) %>%
#   ggplot() +
#   geom_line(aes(x = week, y = outsourced_per), color = "red",
#             n = 4) +
#   geom_line(aes(x = week, y = outsourced_per_upp),
#             linetype="dashed", color = "red", n = 4) +
#   geom_line(aes(x = week, y = outsourced_per_low),
#             linetype="dashed", color = "red", n = 4) +
#   labs(x = "Week", y = "Percent Outsourced") +
#   theme_light(base_size = 16)

# Try similar analysis, but aggregate to year level first. 
# Maybe will be easier to see changes in wages
# Unweighted
# occ_timeline_y <- timeline %>%
#   # filter(year(week) <= 2012) %>% 
#   filter(!is.na(occ), !is.na(outsourced)) %>%
#   mutate(year = year(week)) %>% 
#   filter(year > 2000) %>% 
#   group_by(year, occ, outsourced, self_emp, indep_con, temp_work, on_call) %>%
#   summarise(
#     ho_occ = mean(ho_occ),
#     n = sum(weight),
#     log_real_wkly_wage = mean(log_real_wkly_wage, na.rm = T),
#     hours_week = mean(hours_week, na.rm = T),
#     part_time = mean(part_time, na.rm = T),
#     tenure = mean(tenure, na.rm = T),
#     health = mean(health, na.rm = T),
#     retirement = mean(retirement, na.rm = T),
#     any_benefits = mean(any_benefits, na.rm = T),
#     n_black = sum(black * weight),
#     n_hispanic = sum(hispanic * weight),
#     n_union = sum(union * weight, na.rm = T),
#     n_union_defined = sum((!is.na(union) %in% T) * weight),
#     tot_age = sum(age * weight),
#     tot_age_2 = sum((age ^ 2) * weight)
#   ) %>%
#   pivot_wider(names_from = c(outsourced, self_emp, indep_con, temp_work, on_call),
#               values_from = n:tot_age_2) %>%
#   ungroup() %>% 
#   # Rename pivot table so easier to reference 
#   rename_at(vars(ends_with("_0_0_0_0_0")), ~ str_replace(., "_0_0_0_0_0", "_0")) %>% 
#   rename_at(vars(ends_with("_1_0_0_0_0")), ~ str_replace(., "_1_0_0_0_0", "_1")) %>% 
#   rename_at(vars(ends_with("_0_1_0_0_0")), ~ str_replace(., "_0_1_0_0_0", "_2")) %>% 
#   rename_at(vars(ends_with("_0_0_1_0_0")), ~ str_replace(., "_0_0_1_0_0", "_3")) %>% 
#   rename_at(vars(ends_with("_0_0_0_1_0")), ~ str_replace(., "_0_0_0_1_0", "_4")) %>% 
#   rename_at(vars(ends_with("_0_0_0_0_1")), ~ str_replace(., "_0_0_0_0_1", "_5")) %>% 
#   mutate(
#     workers = r_sum(n_0, n_1, n_2, n_3, n_4, n_5),
#     outsourced_per = ifelse(!is.na(n_1), n_1 / workers * 100, 0),
#     self_emp_per = ifelse(!is.na(n_2), n_2 / workers * 100, 0),
#     indep_con_per = ifelse(!is.na(n_3), n_3 / workers * 100, 0),
#     temp_work_per = ifelse(!is.na(n_4), n_4 / workers * 100, 0),
#     on_call_per = ifelse(!is.na(n_5), n_5 / workers * 100, 0),
#     black_per = (
#       r_sum(n_black_0, n_black_1, n_black_2, n_black_3, n_black_4, n_black_5)
#       / workers * 100),
#     hispanic_per = (
#       r_sum(n_hispanic_0, n_hispanic_1, n_hispanic_2, n_hispanic_3, n_hispanic_4,
#             n_hispanic_5) 
#       / workers * 100),
#     union_per = (r_sum(n_union_0, n_union_1, n_union_2, n_union_3, n_union_4,
#                        n_union_5) / 
#                    r_sum(n_union_defined_0, n_union_defined_1, n_union_defined_2,
#                          n_union_defined_3, n_union_defined_4, n_union_defined_5)
#                  * 100),
#     age = (r_sum(tot_age_0, tot_age_1, tot_age_2, tot_age_3, tot_age_4, tot_age_5) 
#            / workers),
#     age_2 = (r_sum(tot_age_2_0, tot_age_2_1, tot_age_2_2, tot_age_2_3,
#                    tot_age_2_4, tot_age_2_5) 
#              / workers)
#   ) %>% 
#   group_by(year) %>% 
#   mutate(workers_per = workers / sum(workers) * 100) %>% 
#   group_by(occ) %>% 
#   mutate(
#     average_size = mean(workers_per),
#     any_out = mean(outsourced_per, na.rm = T) > 0) %>% 
#   ungroup() %>% 
#   filter(!is.na(year))
# 
# # Recreate Regressions
# 
# # How do these correlations change with controls? Run regressions
# controls <- c("outsourced_per", "self_emp_per", "indep_con_per",
#               "temp_work_per", "on_call_per", "black_per", "hispanic_per",
#               "union_per", "age")
# 
# fixed_effects <- c("year", "occ")
# fe <- create_formula("", fixed_effects)
# 
# outcomes <- c("log_real_wkly_wage", "tenure", "part_time", "hours_week",
#               "health", "retirement", "any_benefits")
# 
# outcome_names <- c("Log Real Weekly Wage", "Weeks Tenure", "Part-Time Status",
#                    "Hours Worked", "Health Insurance", "Retirement Benefits",
#                    "Any Benefits")
# 
# # Create a table with regression results, starting with workers_per
# 
# eq <- create_formula("workers_per", controls)
# 
# temp <- lm_robust(eq, data = occ_timeline_y, fixed_effects = !!fe,
#                   clusters = occ, se_type = "stata", try_cholesky = T)
# 
# t_mean <- mean(occ_timeline_y$workers_per, na.rm = T) 
# 
# center <- rbind(
#   cbind("Percent of Workers", 
#         format_val(temp$coefficients["outsourced_per"], 
#                    p_stars(temp$p.value["outsourced_per"]), r = 5, s = 5),
#         format_val(t_mean)
#   ),
#   cbind("in Occupation",
#         format_se(temp$std.error["outsourced_per"], r = 5, s = 5),
#         " & "
#   )
# )
# 
# for (out in 0:1){
#   for (i in seq_along(outcomes)){
#     
#     # Set heading for section
#     if (i == 1) {
#       if (out == 0) {
#         center %<>% rbind(
#           cbind("Traditional Jobs", " & ", " & " )
#         )
#       } else {
#         center %<>% rbind(
#           cbind("Outsourced Jobs", " & ", " & " )
#         )
#       }
#     }
#     
#     outcome <- str_c(outcomes[i], "_", out)
#     eq <- create_formula(outcome, controls)
#     
#     temp <- lm_robust(eq, data = occ_timeline_y, fixed_effects = !!fe,
#                       clusters = occ, se_type = "stata", try_cholesky = T)
#     
#     t_mean <- mean(occ_timeline_y[[outcome]], na.rm = T) 
#     
#     center %<>% rbind(
#       cbind(outcome_names[i], 
#             format_val(temp$coefficients["outsourced_per"], 
#                        p_stars(temp$p.value["outsourced_per"]), r = 4, s = 4),
#             format_val(t_mean)
#       ),
#       cbind("",
#             format_se(temp$std.error["outsourced_per"], r = 4, s = 4),
#             " &"
#       )
#     )
#   }
# }
# 
# center %<>% cbind(
#   rbind("\\\\", "\\\\[2pt] \\midrule", "\\\\[2pt]", "\\\\", "\\\\[2pt]", 
#         "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
#         "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt] \\midrule",
#         "\\\\[2pt]", "\\\\", "\\\\[2pt]", 
#         "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
#         "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]"
#   )
# )
# 
# # Try similar analysis, but aggregate to year level first. 
# # Maybe will be easier to see changes in wages
# # Unweighted
# occ_timeline_y_1 <- timeline_1 %>%
#   # filter(year(week) <= 2012) %>% 
#   filter(!is.na(occ), !is.na(outsourced)) %>%
#   mutate(year = year(week)) %>%
#   filter(year > 2000) %>% 
#   group_by(year, occ, outsourced, self_emp, indep_con, temp_work, on_call) %>%
#   summarise(
#     ho_occ = mean(ho_occ),
#     n = sum(weight),
#     log_real_wkly_wage = mean(log_real_wkly_wage, na.rm = T),
#     hours_week = mean(hours_week, na.rm = T),
#     part_time = mean(part_time, na.rm = T),
#     tenure = mean(tenure, na.rm = T),
#     health = mean(health, na.rm = T),
#     retirement = mean(retirement, na.rm = T),
#     any_benefits = mean(any_benefits, na.rm = T),
#     n_black = sum(black * weight),
#     n_hispanic = sum(hispanic * weight),
#     n_union = sum(union * weight, na.rm = T),
#     n_union_defined = sum((!is.na(union) %in% T) * weight),
#     tot_age = sum(age * weight),
#     tot_age_2 = sum((age ^ 2) * weight)
#   ) %>%
#   pivot_wider(names_from = c(outsourced, self_emp, indep_con, temp_work, on_call),
#               values_from = n:tot_age_2) %>%
#   ungroup() %>% 
#   # Rename pivot table so easier to reference 
#   rename_at(vars(ends_with("_0_0_0_0_0")), ~ str_replace(., "_0_0_0_0_0", "_0")) %>% 
#   rename_at(vars(ends_with("_1_0_0_0_0")), ~ str_replace(., "_1_0_0_0_0", "_1")) %>% 
#   rename_at(vars(ends_with("_0_1_0_0_0")), ~ str_replace(., "_0_1_0_0_0", "_2")) %>% 
#   rename_at(vars(ends_with("_0_0_1_0_0")), ~ str_replace(., "_0_0_1_0_0", "_3")) %>% 
#   rename_at(vars(ends_with("_0_0_0_1_0")), ~ str_replace(., "_0_0_0_1_0", "_4")) %>% 
#   rename_at(vars(ends_with("_0_0_0_0_1")), ~ str_replace(., "_0_0_0_0_1", "_5")) %>% 
#   mutate(
#     workers = r_sum(n_0, n_1, n_2, n_3, n_4, n_5),
#     outsourced_per = ifelse(!is.na(n_1), n_1 / workers * 100, 0),
#     self_emp_per = ifelse(!is.na(n_2), n_2 / workers * 100, 0),
#     indep_con_per = ifelse(!is.na(n_3), n_3 / workers * 100, 0),
#     temp_work_per = ifelse(!is.na(n_4), n_4 / workers * 100, 0),
#     on_call_per = ifelse(!is.na(n_5), n_5 / workers * 100, 0),
#     black_per = (
#       r_sum(n_black_0, n_black_1, n_black_2, n_black_3, n_black_4, n_black_5)
#       / workers * 100),
#     hispanic_per = (
#       r_sum(n_hispanic_0, n_hispanic_1, n_hispanic_2, n_hispanic_3, n_hispanic_4,
#             n_hispanic_5) 
#       / workers * 100),
#     union_per = (r_sum(n_union_0, n_union_1, n_union_2, n_union_3, n_union_4,
#                        n_union_5) / 
#                    r_sum(n_union_defined_0, n_union_defined_1, n_union_defined_2,
#                          n_union_defined_3, n_union_defined_4, n_union_defined_5)
#                  * 100),
#     age = (r_sum(tot_age_0, tot_age_1, tot_age_2, tot_age_3, tot_age_4, tot_age_5) 
#            / workers),
#     age_2 = (r_sum(tot_age_2_0, tot_age_2_1, tot_age_2_2, tot_age_2_3,
#                    tot_age_2_4, tot_age_2_5) 
#              / workers)
#   ) %>% 
#   group_by(year) %>% 
#   mutate(workers_per = workers / sum(workers) * 100) %>% 
#   group_by(occ) %>% 
#   mutate(
#     average_size = mean(workers_per),
#     any_out = mean(outsourced_per, na.rm = T) > 0) %>% 
#   ungroup() %>% 
#   filter(!is.na(year))
# 
# # Recreate Regressions
# 
# # How do these correlations change with controls? Run regressions
# controls <- c("outsourced_per", "self_emp_per", "indep_con_per",
#               "temp_work_per", "on_call_per", "black_per", "hispanic_per",
#               "union_per", "age")
# 
# fixed_effects <- c("year", "occ")
# fe <- create_formula("", fixed_effects)
# 
# outcomes <- c("log_real_wkly_wage", "tenure", "part_time", "hours_week",
#               "health", "retirement", "any_benefits")
# 
# outcome_names <- c("Log Real Weekly Wage", "Weeks Tenure", "Part-Time Status",
#                    "Hours Worked", "Health Insurance", "Retirement Benefits",
#                    "Any Benefits")
# 
# # Create a table with regression results, starting with workers_per
# 
# eq <- create_formula("workers_per", controls)
# 
# temp_1 <- lm_robust(eq, data = occ_timeline_y_1, fixed_effects = !!fe,
#                   clusters = occ, se_type = "stata", try_cholesky = T)
# 
# t_mean_1 <- mean(occ_timeline_y_1$workers_per, na.rm = T) 
# 
# center_1 <- rbind(
#   cbind("Percent of Workers", 
#         format_val(temp_1$coefficients["outsourced_per"], 
#                    p_stars(temp$p.value["outsourced_per"]), r = 5, s = 5),
#         format_val(t_mean_1)
#   ),
#   cbind("in Occupation",
#         format_se(temp_1$std.error["outsourced_per"], r = 5, s = 5),
#         " & "
#   )
# )
# 
# for (out in 0:1){
#   for (i in seq_along(outcomes)){
#     
#     # Set heading for section
#     if (i == 1) {
#       if (out == 0) {
#         center_1 %<>% rbind(
#           cbind("Traditional Jobs", " & ", " & " )
#         )
#       } else {
#         center_1 %<>% rbind(
#           cbind("Outsourced Jobs", " & ", " & " )
#         )
#       }
#     }
#     
#     outcome <- str_c(outcomes[i], "_", out)
#     eq <- create_formula(outcome, controls)
#     
#     temp_1 <- lm_robust(eq, data = occ_timeline_y_1, fixed_effects = !!fe,
#                         subset = any_out == T,
#                       clusters = occ, se_type = "stata", try_cholesky = T)
#     
#     t_mean_1 <- mean(occ_timeline_y_1[[outcome]], na.rm = T) 
#     
#     center_1 %<>% rbind(
#       cbind(outcome_names[i], 
#             format_val(temp_1$coefficients["outsourced_per"], 
#                        p_stars(temp_1$p.value["outsourced_per"]), r = 4, s = 4),
#             format_val(t_mean_1)
#       ),
#       cbind("",
#             format_se(temp_1$std.error["outsourced_per"], r = 4, s = 4),
#             " &"
#       )
#     )
#   }
# }
# 
# center_1 %<>% cbind(
#   rbind("\\\\", "\\\\[2pt] \\midrule", "\\\\[2pt]", "\\\\", "\\\\[2pt]", 
#         "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
#         "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt] \\midrule",
#         "\\\\[2pt]", "\\\\", "\\\\[2pt]", 
#         "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
#         "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]"
#   )
# )
# 
# occ_timeline_y %>% 
#   group_by(occ) %>% 
#   mutate(
#     mean_workers = mean(workers_per, na.rm = T),
#     mean_outsourced = mean(outsourced_per, na.rm = T),
#     workers_per_diff = (workers_per - mean_workers),
#     outsourced_per_diff = (outsourced_per - mean_outsourced)
#   ) %>% 
#   filter(mean_outsourced > 0) %>% 
#   ggplot(aes(x=workers_per_diff, y=outsourced_per_diff, group = occ)) +
#   geom_jitter() + 
#   geom_smooth(method = "lm")
# 
# occ_timeline_y_1 %>% 
#   group_by(occ) %>% 
#   mutate(
#     mean_workers = mean(workers_per, na.rm = T),
#     mean_outsourced = mean(outsourced_per, na.rm = T),
#     workers_per_diff = (workers_per - mean_workers),
#     outsourced_per_diff = (outsourced_per - mean_outsourced)
#   ) %>% 
#   filter(mean_outsourced > 0) %>% 
#   ggplot(aes(x=workers_per_diff, y=outsourced_per_diff, group = occ)) +
#   geom_jitter() + 
#   geom_smooth(method = "lm")
# 
# 
# occ_timeline_comp <- left_join(occ_timeline_y, occ_timeline_y_1, by = c("occ", "year"),
#                               suffix = c("", "_1")) %>% 
#   mutate(outsourced_per_dif = outsourced_per - outsourced_per_1) %>% 
#   group_by(occ) %>% 
#   mutate(mean_outsourced = mean(outsourced_per, na.rm = T))
# 
# for (end in c("", "_1")) {
#   controls <- str_c(c("outsourced_per", "self_emp_per", "indep_con_per",
#                 "temp_work_per", "on_call_per", "black_per", "hispanic_per",
#                 "union_per", "age"), end)
#   
#   # Create a table with regression results, starting with workers_per
#   
#   eq <- create_formula(str_c("workers_per", end), controls)
#   
#   print(lm_robust(eq, data = occ_timeline_comp, fixed_effects = !!fe,
#                   subset = mean_outsourced > 0,
#                       clusters = occ, se_type = "stata", try_cholesky = T))
#   
#   
# }
