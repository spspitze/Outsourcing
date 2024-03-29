# This file takes cleaned data from matched_timeline and 
# creates transition data between current job and previous job

# Save data as matched_transition

rm(list = ls())

# This uses a large dataset, so data.table is needed
library(outsourcing)
library(zeallot)
library(lubridate)
library(tidyverse)
library(data.table)

# Folders of interest
folders <- name_folders()
c(raw_folder, clean_folder, table_folder, figure_folder,
  d_table_folder, s_table_folder) %<-% folders 

# timeline <- fread(str_c(clean_folder, "matched_timeline.csv"),
#                   colClasses = list(
#                     double = c(1, 3:52, 55:58),
#                     Date = c("week", "week_start_job", "week_end_job")))

# fread having trouble with dates, so use read_csv
timeline <- read_csv(str_c(clean_folder, "matched_timeline.csv"),
                     col_types = cols(
                       .default = col_double(),
                       week = col_date(format = ""),
                       week_start_job = col_date(format = ""),
                       week_end_job = col_date(format = "")
                     )) |> 
  data.table()

# First look at each job's first and last observation in the data
# Order observations by date
first_last <- timeline[!is.na(emp_id)][
  ,`:=`(first = 1 * (week == min(week, na.rm = T)),
        last = 1 * (week == max(week, na.rm = T))),
  by = .(case_id, emp_id)]

first_last <- first_last[first == 1 | last == 1]

# Rank jobs by first/last week
match <- first_last[, .(first_week = min(week),last_week = max(week)),
                    by = .(case_id, emp_id)]


# Keep only jobs whose first/last rank are the same
# (No overlapping jobs)
match <- match[, `:=`(first_rank = rank(first_week),
                      last_rank = rank(last_week)),
               by = case_id]

match <- match[first_rank == last_rank]
match <- match[, `:=`(rank = first_rank, first_rank = NULL,
                      last_rank = NULL)]

# Find previous and next job
match <- match[, `:=`(rank_prev = rank - 1, rank_next = rank + 1)]

transition <- first_last[first == 1][, c(
  "week", "working", "unemployed", "first", "last", "w_tenure") := NULL]

setkey(match, case_id, emp_id)
setkey(transition, case_id, emp_id)

transition <- merge(transition, match, all.x = T) 

# Drop unranked jobs
transition <- filter(transition, !is.na(rank))

# Merge previous jobs (Use tidyverse because data.table 
# setkey acting weird)
transition <- transition |>
  left_join(transition,
             by = c("case_id", "rank_prev" = "rank"),
             suffix = c("", "_prev")) |> 
  left_join(transition,
             by = c("case_id", "rank_next" = "rank"),
             suffix = c("_curr", "_next"))

# Create weeks_prev_job and weeks_next_job using start/stop weeks
transition <- transition |>
  mutate(
    weeks_job_prev = time_length(first_week_curr - last_week_prev, 
                                 unit = "week"),
    weeks_job_next = time_length(first_week_next - last_week_curr, 
                                 unit = "week")
  ) |> 
  # select and rename some variables
  select(-starts_with("rank"),
         -sample_id_next, -sample_id_prev, 
         -female_next, -female_prev, 
         -black_next, -black_prev, -hispanic_next, -hispanic_prev, 
         -less_hs_next, -less_hs_prev, -hs_next, -hs_prev, 
         -aa_next, -aa_prev, -ba_next, -ba_prev, 
         -plus_ba_next, -plus_ba_prev, -educ_other_next, -educ_other_prev,
         -weight_next, -weight_prev, -ever_out_m_next, -ever_out_m_prev,
         -ever_out_oj_next, -ever_out_oj_prev, -ever_ho_occ_next, 
         -ever_ho_occ_prev) |> 
  rename(
    sample_id = sample_id_curr,
    female = female_curr,
    black = black_curr,
    hispanic = hispanic_curr,
    less_hs = less_hs_curr,
    hs = hs_curr,
    aa = aa_curr,
    aa = aa_curr,
    ba = ba_curr,
    plus_ba = plus_ba_curr,
    educ_other = educ_other_curr,
    weight = weight_curr,
    ever_out_m = ever_out_m_curr,
    ever_out_oj = ever_out_oj_curr,
    ever_ho_occ = ever_ho_occ_curr
  ) 
  
# Save the data
write_csv(transition, str_c(clean_folder, "matched_transition.csv"))
