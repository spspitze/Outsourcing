# This file takes cleaned data from timeline and creates transition
# data between current job and previous job

# Save data as transition_clean

rm(list = ls())

# This uses a large dataset, so data.table is needed
library(DescTools)
library(tidyverse)
library(data.table)

# Folders of interest
clean_folder <- "Cleaned Data/"

timeline <- fread(str_c(clean_folder, "timeline_clean_occ.csv"))

# First look at each job's last observation in the data
job_final_week <- timeline %>% 
  group_by(case_id, emp_id) %>% 
  filter(week == max(week), !is.na(emp_id)) %>% 
  select(case_id, hours_week:ind, tenure:union, log_real_hrly_wage:tenure_4,
         union_, childcare:any_benefits, indep_con:traditional)

transition <- timeline %>% 
  filter(!is.na(weeks_between_jobs_2))

transition <- inner_join(transition, job_final_week,
                        by = c("case_id", "previous_job_2" = "emp_id"),
                        suffix = c("_curr", "_last")) 

write_csv(transition, str_c(clean_folder, "transition_clean.csv"))