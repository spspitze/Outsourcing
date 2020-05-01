# This file takes cleaned data from matched_timeline and creates transition
# data between current job and previous job

# Save data as matched_transition

rm(list = ls())

# This uses a large dataset, so data.table is needed
library(DescTools)
library(tidyverse)
library(data.table)

# Folders of interest
clean_folder <- "../Cleaned Data/"

timeline <- fread(str_c(clean_folder, "matched_timeline.csv"))

# First look at each job's first and last observation in the data
# Order obervations by date
first_last <- timeline[!is.na(emp_id)][,`:=`(first = 1 * (week == min(week)),
                                             last = 1 * (week == max(week))),
                                       by = .(case_id, emp_id)]

first_last <- first_last[first == 1 | last == 1][order(week)]

# Look at job leads and lags. Look at lags for first and leads for last
first_last[, c("emp_id_next", "emp_id_prev") := shift(.SD, c(-1,1)), 
           by = case_id, .SDcols = "emp_id"]

first_last[, `:=`(emp_id_prev = first * emp_id_prev, emp_id_next = last * emp_id_next)]

# Combine each row of first last
match <- first_last[, .(emp_id_prev = max(emp_id_prev, na.rm = T),
                        emp_id_next = max(emp_id_next, na.rm = T)),
                    by = .(case_id, emp_id)]

transition <- first_last[first == 1][, c(
  "week", "working", "unemployed", "first", "last", "emp_id_next", "emp_id_prev"):=NULL]

setkey(match, case_id, emp_id)
setkey(transition, case_id, emp_id)

transition %<>% merge(match, all.x = T)

# Create a copy of transition with data about previous/next job that needs 
# to be known.
transition_temp <- first_last[first == 1][, c(
  "week", "working", "unemployed", "sample_id", "female", "black", "hispanic", "weight",
  "ever_out_m", "ever_out_oj", "ever_ho_occ", "emp_id_next", "emp_id_prev"):=NULL]

# Merge previous jobs (Use tidyverse because data.table setkey acting weird)
transition %<>% 
  left_join(transition_temp,
             by = c("case_id", "emp_id_prev" = "emp_id"),
             suffix = c("_curr", "_prev")) %>% 
  left_join(transition_temp,
             by = c("case_id", "emp_id_next" = "emp_id"),
             suffix = c("_curr", "_next"))
  
# Save the data
write_csv(transition, str_c(clean_folder, "matched_transition.csv"))