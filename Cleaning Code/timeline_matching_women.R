# This file takes matched_jobs, demographics, and weights and 
# matches it to timeline for women using the same apporoach as matching.R
# In a separate file because of memory issues

rm(list = ls())

library(data.table)
library(magrittr)
library(DescTools)
library(lubridate)
library(tidyverse)

# Folders of interest
raw_folder <- "../Raw Data/"
clean_folder <- "../Cleaned Data/"

# Get matched_jobs for women
matched_jobs <- read_csv(str_c(clean_folder, "matched_jobs.csv"),
                         col_types = cols(
                           .default = col_double(),
                           week_start_job = col_date(format = ""),
                           week_end_job = col_date(format = "")
                         )) %>% 
  filter(female == 1)

# Weights
weights <- read_table2(str_c(raw_folder, "customweight.dat"),
                       col_types = cols(.default = col_double())) %>% 
  rename(case_id = `1`,
         weight = `0`)

# Demographics data
demographics <- read_csv(str_c(clean_folder, "demographics_clean.csv"),
                         col_types = cols(.default = col_double()))

# Timeline data (make sure week is a date)
timeline <- fread(str_c(clean_folder, "timeline_clean.csv"), 
                  colClasses = list(
                    double = c(1:2, 4:5),
                    Date = c("week")
                  )) %>% 
  filter(female == 1) %>% 
  select(-female)

# Merge demographic info + weights first
demographics_merge <- demographics %>% 
  filter(female == 1) %>% 
  group_by(case_id) %>% 
  filter(int_year == min(int_year)) %>% 
  select(case_id:birth_year, hh_child:tot_child, less_hs:educ_other) %>% 
  select(-female)

# Merge demographic data into weights
weights_t <- left_join(weights, demographics_merge, by = "case_id") %>% 
  data.table()

timeline <- weights_t[timeline, on = .(case_id == case_id)]
timeline <- timeline[, `:=`(age = year(week) - birth_year, birth_year = NULL)]

# Create week_start/end_match to keep original data
temp_match <- matched_jobs %>% 
  select(case_id, emp_id, hours_week, part_time, occ, ind, pbs, tenure, max_tenure,
         week_start_job, week_end_job, log_real_hrly_wage,
         log_real_wkly_wage, self_emp:traditional, 
         any_benefits, health, retirement, union, union_fill, job_sat,
         ever_out_oj, ever_out_m) %>%
  mutate(week_start_match = week_start_job,
         week_end_match = week_end_job) %>% 
  data.table()

# Create week match so week stays in dataset
timeline <- timeline[, `:=`(week_match = week)]

# Back to main match
timeline <- temp_match[timeline,
                       on = .(case_id == case_id,
                              week_start_match <= week_match,
                              week_end_match >= week_match),
                       allow.cartesian = T]

rm(temp_match, temp_match_r, weights_t, demographics, demographics_merge)

# If emp_id is matched to a week but working is 0, set job characteristics 
# to NA (esp emp_id and outsourced)
timeline <- timeline[working == 0, 
                     c("emp_id", "outsourced", "tenure", "log_real_wkly_wage",
                       "log_real_hrly_wage", "hours_week", "part_time",
                       "week_start_job", "week_end_job", "indep_con",
                       "self_emp", "temp_work", "on_call", "traditional")
                     := NA]

# Some weeks have multiple jobs. Isolate these weeks and select 1 job
# From this group. Rank by highest
# 1. hours_week
# 2. tenure
# 3. log_real_wkly_wage
# 4. occ
# Then remaining by lowest emp_id

# This is all weeks with multiple jobs 
timeline <- timeline[, obs := sum(!is.na(emp_id)), by = .(case_id, week)]
timeline_week_conflict <- timeline[obs > 1]

vars <- c("hours_week", "tenure", "log_real_wkly_wage", "occ")
for (var in vars) {
  timeline_week_conflict <- 
    timeline_week_conflict[, `:=`(max = max(get(var), na.rm = T),
                                  non_na = sum(!is.na(get(var)))),
                           by = .(case_id, week)]
  timeline_week_conflict <- timeline_week_conflict[
    (get(var) == max) %in% T | (non_na == 0)]
}

# If any remain, take lowest emp_id
timeline_week_conflict <- 
  timeline_week_conflict[, max := min(emp_id, na.rm = T), by = .(case_id, week)]
timeline_week_conflict <- timeline_week_conflict[(emp_id == max) %in% T] 
timeline_week_conflict <- timeline_week_conflict[,c("max", "non_na") := NULL]

# Merge back into main data set
timeline <- bind_rows(timeline[obs <= 1], timeline_week_conflict) %>% 
  select(-obs) %>% 
  data.table()

# # Check how many observations each week
# timeline <- timeline[, count := .N, by = .(case_id, week)]
# temp <- timeline[count > 1]

# Create w_tenure in timeline to tack tenure week by week
# Try to account for fact that some jobs have already started with 
# tenure by using tenure
timeline <- timeline[
  !is.na(emp_id),
  w_tenure := max_tenure + time_length(week - week_end_job, unit = "week")
  ]
# If negative, assume weeks are correct
timeline <- timeline[
  w_tenure < 0, w_tenure := time_length(week - week_start_job, unit = "week")
  ]
# Set jobs "started" before 1979 to w_tenure = NA
timeline <- timeline[week_start_job < ymd("1979-01-01"), w_tenure := NA]

# Count working spells that aren't matched (NA == 0). Set these emp_ids
# to their number, ie 1, 2, ... . Also mark start and end week
timeline <- timeline[order(week)]
timeline <- timeline[,c("working_next", "working_prev") := shift(.SD, c(-1,1)), 
                     by = case_id, .SDcols = "working"]
timeline <- timeline[,c("emp_id_next", "emp_id_prev") := shift(.SD, c(-1,1)), 
                     by = case_id, .SDcols = "emp_id"]

# Mark start/end as week if prev/next working is 0 or NA or if emp_id prev/next exists
timeline <- timeline[
  (is.na(emp_id) & (working == 1) 
   & (working_prev == 0 | is.na(working_prev) | !is.na(emp_id_prev))),
  week_start_job := week]

timeline <- timeline[
  (is.na(emp_id) & (working == 1) 
   & (working_next == 0 | is.na(working_next) | !is.na(emp_id_next))),
  week_end_job := week]

# Set the emp_id of these jobs as the rank of their start date
# First set emp_id of week_start_job then fill in rest
timeline <- timeline[is.na(emp_id) & !is.na(week_start_job) & (working != 0),
                     emp_id := rank(week_start_job), by = case_id]

timeline <- timeline[working == 1 & (is.na(emp_id) | emp_id < 1000),
                     emp_id := nafill(emp_id, type = "locf"),
                     by = case_id]

# Drop uneeded variables. 
timeline <- timeline[, c(
  "max", "non_na", "week_start_match", "week_end_match", "working_next", "working_prev",
  "emp_id_next", "emp_id_prev") := NULL]

# Results noisy at end, so drop after max week, around end of 2016
week_max <- round_date(ymd("2016-10-08"), "week")
timeline <- timeline[week <= week_max]

fwrite(timeline, str_c(clean_folder, "matched_timeline_women.csv"), row.names = FALSE)