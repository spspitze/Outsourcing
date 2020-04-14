# This file takes cleaned data from matching and creates a timeline of jobs by week
# Start with 1st week of 2001 and end in final week and select job (if any).
# If more than 1 job, arrange by
# 1. Hours worked per week
# 2. Tenure
# 3. Log real hourly wage
# If no job now but a listed job before or after, put as unemployed

# Save data as timeline_clean

rm(list = ls())


# This will become a larger dataset, so data.table is needed
library(DescTools)
library(tidyverse)
library(data.table)
library(lubridate)

# Folders of interest
clean_folder <- "Cleaned Data/"

# Import data using tidyverse to make sure dates are read, then
# convert into data.table
matched <- read_csv(str_c(clean_folder, "matched_clean.csv"),
                    col_types = cols(
                      .default = col_double(),
                      month_start_job = col_date(),
                      month_end_job = col_date(),
                      week_start_job = col_date(),
                      week_end_job = col_date()
                    )) %>% 
  data.table()

# Require a value for hours_week, tenure, and wages below. If NA,
# set to -1 so still in T/F filtering
# Also drop if week_ start/end missing
matched <- matched[!is.na(week_start_job) & !is.na(week_end_job),
                   `:=`(hours_week = ifelse(!is.na(hours_week), hours_week, -5),
                        tenure = ifelse(!is.na(tenure), tenure, -5),
                        log_real_hrly_wage = ifelse(!is.na(log_real_hrly_wage),
                                                    log_real_hrly_wage, -5))]

# Create a function to take a variable name and data set. Return the varibale if
# filled. If NA, check if other"s in group that are not NA. If all are NA, return
# NA, otherwise return mode
fill_NA_mode <- function(vector){
  return(
    ifelse(
      !is.na(vector), vector,
      ifelse(all(is.na(vector)), NA, Mode(vector, na.rm = T))
    )
  )
}


# The first week of 2001 is our starting point
week_start <- round_date(ymd("2001-01-01"), "week")

# What is last week in timeline
week_end <- max(matched$week_end_job, na.rm = T)

# # If I want to test something on a smaller dataset
# week_end <- week_start + weeks(52)

# The sequence of weeks (without week start, see below)
week_seq <- seq(week_start + weeks(1), week_end, by = "weeks")

# Within how much time does a person need a job to count as unemployed
within_previous <- years(2)
within_next <- years(2)

# List all case_ids in sample
case_ids <- unique(matched$case_id)

# Create an empty timeline with all variables of matched plus week and unemployed
var_names <- 
  append(c("week", "unemployed", "unemployed_duration", "previous_job_2"),
         colnames(matched))
num_variables <- length(var_names)
timeline <- data.table()

# Lots of warnings coming, turn them off temporarily
options(warn = -1)

# Do week_start first to set up timeline 
# (so don't have to keep checking if week start)
# Create a temp timeline for this week
temp_timeline <- data.table(
  case_id = case_ids,
  week = week_start,
  unemployed = 0,
  unemployed_duration = 0,
  previous_job_2 = 0
)

# Jobs held right now, filtering out non-primary jobs. Create week for matching
temp <- matched[week_start_job <= week_start & week_end_job >= week_start] %>% 
  group_by(case_id) %>% 
  filter(hours_week == max(hours_week)) %>% 
  filter(tenure == max(tenure)) %>% 
  filter(log_real_hrly_wage == max(log_real_hrly_wage)) %>% 
  arrange(case_id) %>% 
  data.table()

# Did this person have a previous job within my timeframe?
temp_before <- 
  matched[, any((week_start - week_end_job < within_previous) &
                  (week_start >= week_end_job)), by = "case_id"] 
# Did this person have a next job within the next 2 years?
temp_after <- 
  matched[, any((week_start_job - week_start < within_next) &
                  (week_start_job >= week_start)), by = "case_id"]
# # Was this person interviewed this round? (Is this week within a year before
# # or a year after the start if an interview year?)
# temp_interviewed <- 
#   matched[, any(year %in% c(year(week_start), year(week_start) + years(1))),
#           by = "case_id"] 

# Match to timeline based on case_id and week
# If not matched (is.na(emp_id)) and have a job before and after, set unemployed = 1
# Mark previous job
setkey(temp_timeline, case_id)
setkey(temp, case_id)

temp_timeline <- merge(temp_timeline, temp, all.x = T)

temp_timeline[, c("unemployed", "previous_job_2") := 
                .(ifelse(is.na(emp_id) & temp_before$V1 & temp_after$V1, 1, unemployed),
                  0)] 

temp_timeline[, unemployed_duration := unemployed]

# Set this time_line to previous timeline
temp_timeline_previous <- temp_timeline

# If neither matched nor unemployed, drop from data_set
temp_timeline <- temp_timeline[!is.na(emp_id) | unemployed == 1]

# Append temp to timeline
timeline <- rbindlist(list(timeline, temp_timeline), use.names = T)

# Match based on week (this process may take a few minutes)
for (id in seq_along(week_seq)){
  week_id <- week_seq[id]
  
  # Create a temp timeline for this week
  temp_timeline <- data.table(
    case_id = case_ids,
    week = week_id,
    unemployed = 0,
    unemployed_duration = 0,
    previous_job_2 = 0
  )
  
  # Jobs held right now, filtering out non-primary jobs. Create week for matching
  temp <- matched[week_start_job <= week_id & week_end_job >= week_id] %>% 
    group_by(case_id) %>% 
    filter(hours_week == max(hours_week, na.rm = T)) %>% 
    filter(tenure == max(tenure, na.rm = T)) %>% 
    filter(log_real_hrly_wage == max(log_real_hrly_wage, na.rm = T)) %>% 
    arrange(case_id) %>% 
    data.table()
  
  # Did this person have a previous job within the last 2 years?
  temp_before <- 
    matched[, any((week_id - week_end_job < within_previous) &
                    (week_id >= week_end_job)), by = "case_id"] 
  # Did this person have a next job within the next 2 years?
  temp_after <- 
    matched[, any((week_start_job - week_id < within_next) &
                    (week_start_job >= week_id)), by = "case_id"]
  # # Was this person interviewed this round? (Is this week within a year before
  # # or a year after the start if an interview year?)
  # temp_interviewed <- 
  #   matched[, any(year %in% c(find_year(week_id), find_year(week_id) + 1)),
  #           by = "case_id"]
  
  # Match to timeline based on case_id and week
  # If not matched (is.na(emp_id)) and have a job before and after, set unemployed = 1
  # Mark previous job (pull forward if unemployed)
  temp_timeline <- merge(temp_timeline, temp, all.x = T)
  
  temp_timeline[, `:=`(
    unemployed = ifelse(is.na(emp_id) & temp_before$V1 & temp_after$V1, 1, unemployed),
    previous_job_2 = ifelse(
      # Employed today
      !is.na(emp_id),
      # In same job yesterday (set previous_job_2 to 0)
      ifelse((emp_id == temp_timeline_previous$emp_id) %in% T, 0,
             ifelse(!is.na(temp_timeline_previous$emp_id), 
                    # In another job
                    temp_timeline_previous$emp_id,
                    # Unemployed yesterday
                    temp_timeline_previous$previous_job_2)),
      # Unemployed today
      ifelse(!is.na(temp_timeline_previous$emp_id),
             # Employed yesterday
             temp_timeline_previous$emp_id,
             # Unemployed yesterday
             temp_timeline_previous$previous_job_2)))] 
  
  temp_timeline[, unemployed_duration := 
                  ifelse(unemployed != 1, 0,
                         1 + temp_timeline_previous$unemployed_duration)]
                  
  # Set this time_line to previous timeline
  temp_timeline_previous <- temp_timeline
  
  # If neither matched nor unemployed, drop from data_set
  temp_timeline <- temp_timeline[!is.na(emp_id) | unemployed == 1]
  
  # Append temp to timeline
  timeline <- rbindlist(list(timeline, temp_timeline), use.names = T)
}

# Turn warnings back on
options(warn = 0)

# Fill in missing year and demographic data for unemployed
timeline_clean <- timeline %>% 
  group_by(case_id) %>% 
  arrange(case_id, week) %>% 
  mutate(
    # Return -5 to NA
    hours_week = ifelse(hours_week >= 0, hours_week, NA),
    tenure = ifelse(tenure >= 0, tenure, NA),
    log_real_hrly_wage = ifelse(log_real_hrly_wage >= 0, log_real_hrly_wage, NA),
    # Fill these variables with fill_NA_mode (created above)
    sample_id = fill_NA_mode(sample_id),
    female = fill_NA_mode(female),
    black = fill_NA_mode(black),
    hispanic = fill_NA_mode(hispanic),
    birth_year = fill_NA_mode(birth_year),
    ever_out = fill_NA_mode(ever_out),
    ever_out_2 = fill_NA_mode(ever_out_2),
    # Find year based on week
    year = year(week),
    # Recaluclate these variables
    age = year - birth_year,
    age_2 = (age / 10) ^ 2,
    age_3 = (age / 10) ^ 3,
    age_4 = (age / 10) ^ 4,
    # Create w_tenure of tenure each week, calculated using week_end_job and tenure
    w_tenure = tenure - (week_end_job - week) / 7
  ) %>%
  # Fill these variables by fill "downup"
  fill(educ, hh_child, marital_status, msa, region, tot_child, 
       wks_work_prev, less_hs, hs, aa, ba, plus_ba, educ_other, weight,
       .direction = "downup") %>% 
  # Create weeks_between_jobs_2 which uses this timeline to measure weeks 
  # between jobs using unemployemnt duration
  # (rather than start/stop dates which allowed for job overlap
  # and did not distinguish main jobs)
  mutate(
    weeks_between_jobs_2 = ifelse(
      # If have a job
      !is.na(emp_id),
      # If previous_job not 0 or current job
      ifelse(previous_job_2 != 0 & previous_job_2 != emp_id, 
             # If come from another job, will be 0. If from unemployment,
             # will be unemployment duration (if it exists)
             ifelse(!is.na(lag(unemployed_duration)), lag(unemployed_duration), 
                    # Otherwise leave empty
                    NA), NA), NA)
  ) %>% 
  # Drop observations without weight (probably due to missing data)
  filter(!is.na(weight))

# Save the data
fwrite(timeline_clean, str_c(clean_folder, "timeline_clean.csv"), row.names = FALSE)
