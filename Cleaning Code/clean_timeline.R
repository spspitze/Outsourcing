# This file takes raw data from timeline_raw and creates a timeline of jobs by week.
# I start with the first week of 2001 and end with final recorded week so far.
# Save data as timeline_clean

rm(list = ls())

library(outsourcing)
library(zeallot)
library(lubridate)
library(tidyverse)

# Folders of interest
folders <- name_folders()
c(raw_folder, clean_folder, table_folder, figure_folder,
  d_table_folder, s_table_folder) %<-% folders 

new_data <- read_csv(str_c(raw_folder, "timeline_raw.csv"),  
                     col_types = cols(.default = col_double()))

# Handle missing values

new_data[new_data == -1] = NA  # Refused 
new_data[new_data == -2] = NA  # Dont know 
new_data[new_data == -3] = NA  # Invalid missing 
new_data[new_data == -4] = NA  # Valid missing 
new_data[new_data == -5] = NA  # Non-interview 

# Reshape and Clean Data --------------------------------------------------

# Rename case_id and set SAMPLE_SEX to female
# (Previous code used female, but no longer need it. Drop)
new_data <- new_data |>
  rename(case_id = CASEID,
         female = SAMPLE_SEX) |> 
  mutate(female = 1 * (female == 2)) |>
  select(-female)

constant <- c("case_id")

vary <- "STATUS_WK_NUM"

long <- new_data |> 
  gather(matches(str_c("^(", vary, ")")), 
         key=key, value=job_status) |>
  extract(key, into=c("variable", "week"), 
          regex="(\\D+)(\\d\\d\\d\\d)") |>
  filter(!is.na(variable), !is.na(week)) |> 
  # Because only 1 variable, do not need to gather, just drop variable
  select(-variable) |> 
  # If job_status is 0, have no info for this week. 
  # Drop these observations
  filter(job_status > 0) |> 
  # The first week begins in 2000 and ends in 2001. Causes some
  # issues for month, year, and age, so filter it out
  # This function from outsourcing does it for me.
  after_base_week() |> 
  # Turn week into a date
  # Create two variables. Working for working vs not working 
  # (drop unknowns)
  # For now, count active military service (== 7 as employed)
  # Unemployed for known unemployed (rather than olf)
  mutate(
    week = round_week(as.numeric(week)),
    working = 1 * (job_status >= 7),
    unemployed = 1 * (job_status == 4)
  ) |> 
  select(-job_status)

# Save the data
write_csv(long, str_c(clean_folder, "timeline_clean.csv"))