# This file takes cleaned data from timeline and creates transition
# data between current job and previous job

# Save data as transitions_clean

rm(list = ls())

# This uses a large dataset, so data.table is needed
library(DescTools)
library(tidyverse)
library(data.table)

# Folders of interest
clean_folder <- "../Cleaned Data/"

timeline <- fread(str_c(clean_folder, "timeline_clean.csv"))

# First look at each job's last observation in the data
job_final_week <- timeline %>% 
  group_by(case_id, emp_id) %>% 
  filter(week == max(week)) %>% 
  select(case_id, emp_id, hours_week:occ, tenure, union, union_,
         log_real_hrly_wage:tenure_4, childcare:hispanic, age:weight,
         indep_con:traditional, ever_out, occ_out_above, ever_out_occ_above)

transition <- timeline %>% 
  filter(!is.na(weeks_between_jobs_2)) %>% 
  select(case_id, emp_id, previous_job_2, hours_week:occ, tenure, union, union_,
         log_real_hrly_wage:tenure_4, childcare:hispanic, age:weight,
         indep_con:traditional, ever_out, occ_out_above, ever_out_occ_above,
         weeks_between_jobs_2) 

# # If low memory, drop timeline
rm("timeline")
gc()

transition <- inner_join(transition, job_final_week,
                        by = c("previous_job_2" = "emp_id"),
                        suffix = c("_next", "_last")) 
