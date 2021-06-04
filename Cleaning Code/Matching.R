# This file takes emp_hist_rost_clean, emp_sum_clean, on_jobs_clean,
# demographics_clean, and weights and matches these data sets together 
# with the help of linking_clean (matched)
# It then creates a dataset of all jobs by averaging job characteristics 
# over each interview's observations (matched_jobs)
# I then combine matched_jobs with timeline_clean to create a 
# timeline of jobs (matched_timeline)
# I also create a timeline using matched where job characteristics
# change every 2 years (matched_timeline_robust)
# I use this timeline to see the prevalence of outsourcing for each
# occupation and merge this information into the three datasets
# It saves the data in matched, matched_jobs, matched_timeline, and
# matched_timeline_robust

rm(list = ls())

library(outsourcing)
library(zeallot)
library(xtable)
library(openxlsx)
library(readxl)
library(srvyr)
library(data.table)
library(DescTools)
library(lubridate)
library(tidyverse)

# Folders of interest
folders <- name_folders("NLSY 79 Matching")
c(raw_folder, clean_folder, table_folder, figure_folder,
  d_table_folder, s_table_folder) %<-% folders 

# For saving graphs
c(height, width) %<-% fig_size()

# Bottom of slide tables never changes, so make it once
s_bot <- make_bot(slide = TRUE)

# This is data from the Employer History Roster
hist_rost <- read_csv(str_c(clean_folder, "emp_hist_rost_clean.csv"),
                      col_types = cols(
                        .default = col_double(),
                        month_start_job = col_date(format = ""),
                        month_end_job = col_date(format = ""),
                        week_start_job = col_date(format = ""),
                        week_end_job = col_date(format = "")
                        )) 

# Data from Employer Supplement 
emp_sup <- read_csv(str_c(clean_folder, "emp_sup_clean.csv"),
                    col_types = cols(.default = col_double()))

# Get custom weights from raw_folder
weights <- read_table2(str_c(raw_folder, "customweight.dat"),
                       col_types = cols(.default = col_double())) |> 
  rename(case_id = `1`,
         weight = `0`)

# # Get custom weights from raw_folder (When only using 2012 and before)
# weights <- read_table2(str_c(raw_folder, "customweight_less2012.dat"),
#                        col_types = cols(.default = col_double())) |> 
#   rename(case_id = `1`,
#         weight = `0`)

# Demographics data
demographics <- read_csv(str_c(clean_folder, "demographics_clean.csv"),
                         col_types = cols(.default = col_double()))

# Because main dataset only looking at men, get gender by case_id
dem_gender <- demographics |> 
  select(case_id, female) |> 
  group_by(case_id) |> 
  unique()

# Data from On Jobs 
on_jobs <- read_csv(str_c(clean_folder, "on_jobs_clean.csv"),
                    col_types = cols(
                      .default = col_double(),
                      month_start_job = col_date(format = ""),
                      month_end_job = col_date(format = "")
                    ))

# Link Employer Supplement to On Jobs
linking <- read_csv(str_c(clean_folder, "linking_clean.csv"),
                    col_types = cols(.default = col_double()))

# Create Match Between Datasets and Clean Data -------------------------------

# Join Employer Histoy Roster, Employer Supplement, Demographics,
# and weights
# For hist_rost and emp_sup, use inner join because both
# have critical information
# For the others, use left_join to match data to jobs
ehr_es <- inner_join(hist_rost, emp_sup, 
                    by = c("case_id", "emp_id", "int_year"))

ehr_es <- left_join(ehr_es, demographics, by = c("case_id", "int_year"))

ehr_es <- left_join(ehr_es, weights, by = c("case_id")) 

# Matching ehr_es to on_jobs.
# First link in oj_job to ehr_es dataset
ehr_es <- left_join(ehr_es, linking, 
                    by = c("case_id" = "case_id", 
                           "int_year" = "int_year",
                           "job" = "es_job"))

# Now link in on_jobs
matched <- left_join(ehr_es, on_jobs, 
                      by = c("case_id" = "case_id", 
                             "int_year" = "int_year",
                             "oj_job" = "job"),
                      suffix = c("_ehr", "_oj"))

# Fill in missing data
fill_mean <- c("indep_con", "on_call", "outsourced", "self_emp",
               "temp_work", "traditional", "pre_trad", "ever_out_oj")

matched <- matched |>
  # Fill in missing data by case_id, emp_id
  group_by(case_id, emp_id) |> 
  mutate_at(fill_mean, fill_NA_mean) |> 
  mutate(
    # Flag matches where start month or end month not the same
    match_flag_1 = (
      ((month_start_job_ehr - month_start_job_oj != 0) %in% T) 
      | ((month_end_job_ehr - month_end_job_oj != 0) %in% T)
      ),
    # Flag matches where self_emp:traditional are not 0/1
    # Or when job type not consistent for entire job
    match_flag_2 = !(
      (mean(self_emp) %in% c(0, 1)) 
      & (mean(indep_con) %in% c(0, 1))
      & (mean(on_call) %in% c(0, 1)) 
      & (mean(outsourced) %in% c(0, 1))
      & (mean(temp_work) %in% c(0, 1))
      & (mean(traditional) %in% c(0, 1))
      ),
    # Record Max Tenure for each job
    max_tenure = ifelse(max(tenure, na.rm = T) > 0, 
                        max(tenure, na.rm = T), 0)
  ) |> 
  # Make sure there are unique case_id, int_year, emp_id matches
  group_by(int_year, .add = T) |>
  mutate(count = n()) |> 
  ungroup() |> 
  # Create pbs to measure if industry is in professional 
  # business services
  mutate(pbs = 1 * (ind >= 7270 & ind <= 7790)) |> 
  filter(count == 1) |>
  select(-looped, -count, -birth_year, 
         -month_start_job_ehr:-month_end_job_ehr,
         -month_start_job_oj:-month_end_job_oj)

# Match Quality -----------------------------------------------------------

# How good is the match? Create some tables showing match quality from
# both sides (OJ and EHR/ES). Make sure to only look at men for match
# quality

# First join in female to look only at men, who make up main dataset
hist_rost <- left_join(hist_rost, dem_gender, by = "case_id")
emp_sup <- left_join(emp_sup, dem_gender, by = "case_id")
on_jobs <- left_join(on_jobs, dem_gender, by = "case_id")

# Total jobs in each survey
ehr_es_obs <- NROW(ehr_es[ehr_es$female == 0,])
oj_obs <- NROW(on_jobs[on_jobs$female == 0,])

# Jobs in ehr/es not matched with oj
unmatched_obs <- ehr_es_obs - NROW(matched[matched$female == 0,])

# Jobs dropped from match because start/end dates don"t align
flag_1_obs <- sum(matched$match_flag_1[matched$female == 0])

matched <- filter(matched, match_flag_1 == F)

# Jobs dropped from match because no or conflicting job
# type info
flag_2_obs <- sum(matched$match_flag_2[matched$female == 0])

matched <- matched |>
  filter(match_flag_2 == F) |>
  group_by(case_id) |> 
  # How many ever_out are in matched dataset. 
  # How does this compare to On Jobs?
  mutate(ever_out_m = ifelse(all(is.na(outsourced)), 0,
                             max(outsourced, na.rm = T))) |> 
  ungroup()

# Define ever_out if report outsourced in oj. How would
# this be different if used es/ehr
ever_out_count <- matched |> 
  group_by(case_id) |> 
  filter(row_number() == min(row_number())) |>  
  group_by(ever_out_oj, ever_out_m) |>
  count()

# Look at oj missed
on_jobs_miss <- on_jobs |>
  anti_join(matched, 
            by = c("case_id", "int_year", "job" = "oj_job")) |> 
  filter(female == 0)

# Total jobs matched
m_obs <- NROW(matched[matched$female == 0,])
m_outsourced <- sum(matched$outsourced[matched$female == 0])

# How many missed, how many with info missed, how many 
# outsourcing missed. Note that we should account for jobs 
# matched using same type within year
oj_missed <- NROW(on_jobs_miss[on_jobs_miss$female == 0,])
oj_info <- sum(!is.na(on_jobs$looped[on_jobs_miss$female == 0]))
oj_info_missed <- 
  sum(!is.na(on_jobs_miss$looped[on_jobs_miss$female == 0])) 
oj_outsourced <- sum(on_jobs$outsourced[on_jobs$female == 0], na.rm = T)
oj_outsourced_missed <-
  sum(on_jobs_miss$outsourced[on_jobs$female == 0], na.rm = T) 

# Create Tables

# First, numbers being creating matched
obs <- c(ehr_es_obs, unmatched_obs, flag_1_obs, flag_2_obs, m_obs)

labels <- c("Starting Data Set",
            "Unmatched with On Jobs",
            "Conflicting Start or Stop Dates",
            "Missing/Conflicting Job Types",
            "Matched Data Set")

p_m <- c("", "-", "-", "-", "")

end <- c("\n", "\n", "\n", "\\midrule \n", "\n")

table <- "\\begin{tabular}{lr}
\\toprule
Subset & Number \\\\ \\midrule
"

for (i in seq_along(obs)){
  table <- str_c(table, labels[i], format_n(obs[i],  p_m[i]),
                 "\\\\ ", end[i])
}

name <- "Matching Steps"
label <- "match"
note <- "The matching process for the Employer History Roster/Employer 
Supplement of the NLSY and number of person-interview-job observations 
lost/gained step by step. An observation is considered matched with 
On Jobs if it is matched in at least one interview."

header <- make_header("", name, label)
d_header <- make_header("d", name, label)
s_header <- make_header("s")

bot <- make_bot(note)

write.table(str_c(header, table, bot, "\n \\end{document}"),
            str_c(table_folder, "NLSY79 Match Quality/Match Steps.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save to Draft
write.table(str_c(d_header, table, bot),
            str_c(d_table_folder, "Match Steps.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save table in Slide Tables
write.table(str_c(s_header, table, s_bot),
            str_c(s_table_folder, "Match Steps.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Look at match quality for on jobs

table <- "\\begin{tabular}{lrrr}
\\toprule
Subset & Unmatched & Total & Percent Missing \\\\ \\midrule
"

table <- str_c(
  table,
  str_c("On Jobs", 
        format_n(oj_missed),
        format_n(oj_obs),
        format_val(oj_missed / oj_obs * 100, r = 2, s= 2)),
  " \\\\ \n",
  str_c("On Jobs with Information",
        format_n(oj_info_missed),
        format_n(oj_info),
        format_val(oj_info_missed / oj_info * 100, r = 2, s= 2)),
  " \\\\ \n",
  str_c("On Jobs Outsourced",
        format_n(oj_outsourced_missed),
        format_n(oj_outsourced),
        format_val(oj_outsourced_missed / oj_outsourced * 100, 
                   r = 2, s= 2)),
  " \\\\ \n"
)

name <- "On Jobs Match Quality"
label <- "oj_match"
note <- "The match quality from the On Jobs section in final
data set. Observations are at the person-interview-job level.
A job is matched if is connected to a job from the 
Employer History Roster/Employer Supplement. Jobs with information
are jobs with information about job type in On Jobs."

header <- make_header("", name, label)
d_header <- make_header("d", name, label)
s_header <- make_header("s")

bot <- make_bot(note)

write.table(str_c(header, table, bot),
            str_c(table_folder, 
                  "NLSY79 Match Quality/Match On Jobs.tex"),
            quote=F, col.names=F, row.names=F, sep="")

write.table(str_c(d_header, table, bot),
            str_c(d_table_folder, "Match On Jobs.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save for Slides
write.table(str_c(s_header, table, s_bot),
            str_c(s_table_folder, "Match On Jobs.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Drop unneeded variables
matched <- matched |> 
  select(-match_flag_1, -match_flag_2, -job, -oj_job)

# Create Matched Jobs -----------------------------------------------------

# Variables to use mean, mode, max, and min 
mean_vars <- c("hours_week", "tenure", "union", "log_real_hrly_wage",
               "log_real_wkly_wage", "part_time", "childcare", 
               "dental", "flex_sched", "health", "job_sat", "life", 
               "maternity", "profit_share", "retirement", 
               "train_school", "any_benefits", "sample_id", "female",
               "black", "hispanic", "hh_child", "tot_child", 
               "less_hs", "hs", "aa", "ba", "plus_ba", "educ_other",
               "age", "weight", "self_emp", "indep_con", "on_call",
               "outsourced", "temp_work", "traditional",
               "ever_out_oj", "ever_out_m")

mode_vars <- c("ind", "occ", "ind_cat", "occ_cat",
               "marital_status", "msa", "region",
               "union_fill")

min_vars <- c("week_start_job")

max_vars <- c("week_end_job", "max_tenure", "pre_trad")

# Create mean, min, and max functions without na
mean_na <- function(vector) mean(vector, na.rm = T)
min_na <- function(vector) min(vector, na.rm = T)
max_na <- function(vector) max(vector, na.rm = T)

# Create a variable that finds Mode. 
# If multiple, takes first observation.
find_mode <- function(vector) {
  temp <- Mode(vector, na.rm = T)[1]
  ifelse(!is.na(temp), temp, vector[[1]])
}

# Create matched_jobs which groups data by jobs
matched_jobs <- list(.vars = lst(mean_vars, mode_vars, 
                                 min_vars, max_vars),
                     .funs = lst(mean_na, find_mode, min_na, max_na)
                     ) |> 
  pmap(~ matched |> group_by(case_id, emp_id) |> 
         summarise_at(.x, .y)) |> 
  reduce(inner_join, by = c("case_id", "emp_id")) |> 
  # Fix max_tenure -Inf as NA
  mutate(max_tenure = ifelse(max_tenure >= 0, max_tenure, NA))|> 
  # Create pbs to measure if industry is in professional business
  # services
  mutate(pbs = 1 * (ind >= 7270 & ind <= 7790))

# Remove unneeded data sets to free up memory
rm("on_jobs", "ehr_es", "emp_sup", "hist_rost", "linking",
   "dem_gender", "oj_info", "oj_info_missed", "oj_missed",
   "on_jobs_miss", "ever_out_count", "oj_obs", "obs",
   "labels", "end", "fill_mean", "mean_vars", "mode_vars",
   "max_vars", "min_vars")

# Merge Timeline With Job Info --------------------------------------------

# Merge job info from hist_rost using start and end weeks
# Because merging based on a condition, easier to use data.table
# Only merge males because otherwise data set is too big

# Timeline data (make sure week is a date)
timeline <- fread(str_c(clean_folder, "timeline_clean.csv"), 
                  colClasses = list(
                    double = c(1:2, 4:5),
                    Date = c("week")
                  )) |> 
  filter(female == 0) |> 
  select(-female)

# Merge demographic info + weights first
demographics_merge <- demographics |> 
  filter(female == 0) |> 
  group_by(case_id) |> 
  filter(int_year == min(int_year)) |> 
  select(case_id:birth_year, hh_child:tot_child, less_hs:educ_other) |> 
  select(-female)
  
# Merge demographic data into weights
weights_t <- left_join(weights, demographics_merge, by = "case_id") |> 
  data.table()

timeline <- weights_t[timeline, on = .(case_id == case_id)]
timeline <- timeline[, `:=`(age = year(week) - birth_year,
                            birth_year = NULL)]

# Create week_start/end_match to keep original data
temp_match <- matched_jobs |> 
  select(case_id, emp_id, hours_week, part_time, occ, ind, pbs, 
         tenure, max_tenure,
         week_start_job, week_end_job, log_real_hrly_wage,
         log_real_wkly_wage, self_emp:traditional, 
         any_benefits, health, retirement, union, union_fill, job_sat,
         ever_out_oj, ever_out_m) |>
  mutate(week_start_match = week_start_job,
         week_end_match = week_end_job) |> 
  data.table()

# Create week match so week stays in dataset
timeline <- timeline[, `:=`(week_match = week)]

# An aside, the main dataset connects to matched_jobs, so
# job characteristics do not change over time (as in the model)
# For robustness, also match to matched, so characteristics change
# each interview (do this now)
temp_match_r <- matched |>
  select(case_id, emp_id, int_year, hours_week, part_time, occ,
         ind, pbs, tenure, max_tenure,
         week_start_job, week_end_job, log_real_hrly_wage,
         log_real_wkly_wage, self_emp:traditional,
         any_benefits, health, retirement, union, union_fill, job_sat,
         ever_out_oj, ever_out_m) |>
  mutate(week_start_match = week_start_job,
         week_end_match = week_end_job) |>
  data.table()

timeline_r <- temp_match_r[timeline,
                           on = .(case_id == case_id,
                                  week_start_match <= week_match,
                                  week_end_match >= week_match),
                           allow.cartesian = T]

# Back to main match
timeline <- temp_match[timeline,
                      on = .(case_id == case_id,
                             week_start_match <= week_match,
                             week_end_match >= week_match),
                      allow.cartesian = T]

rm(temp_match, temp_match_r, weights_t, demographics, 
   demographics_merge, weights)

# If emp_id is matched to a week but working is 0, set 
# job characteristics to NA (esp emp_id and outsourced)
timeline <- timeline[working == 0, 
                     c("emp_id", "outsourced", "tenure", 
                       "log_real_wkly_wage",
                       "log_real_hrly_wage", "hours_week", "part_time",
                       "week_start_job", "week_end_job", "indep_con",
                       "self_emp", "temp_work", "on_call", 
                       "traditional")
                     := NA]

timeline_r <- timeline_r[working == 0,
                     c("emp_id", "outsourced", "tenure", 
                       "log_real_wkly_wage",
                       "log_real_hrly_wage", "hours_week", "part_time",
                       "week_start_job", "week_end_job", "indep_con",
                       "occ", "self_emp", "temp_work", "on_call",
                       "traditional")
                     := NA]

# Some weeks have multiple jobs. Isolate these weeks and select 1 job
# From this group. Rank by highest
# 1. hours_week
# 2. tenure
# 3. log_real_wkly_wage
# 4. occ
# Then remaining by lowest emp_id

# This is all weeks with multiple jobs 
timeline <- timeline[, obs := sum(!is.na(emp_id)), 
                     by = .(case_id, week)]
timeline_week_conflict <- timeline[obs > 1]

timeline_r <- timeline_r[, obs := sum(!is.na(emp_id)), 
                         by = .(case_id, week)]
timeline_week_conflict_r <- timeline_r[obs > 1]

vars <- c("hours_week", "tenure", "log_real_wkly_wage", "occ")
for (var in vars) {
  timeline_week_conflict <- 
    timeline_week_conflict[, `:=`(max = max(get(var), na.rm = T),
                                  non_na = sum(!is.na(get(var)))),
                           by = .(case_id, week)]
  timeline_week_conflict <- timeline_week_conflict[
    (get(var) == max) %in% T | (non_na == 0)]
  
  timeline_week_conflict_r <-
    timeline_week_conflict_r[, `:=`(max = max(get(var), na.rm = T),
                                  non_na = sum(!is.na(get(var)))),
                           by = .(case_id, week)]
  timeline_week_conflict_r <- timeline_week_conflict_r[
    (get(var) == max) %in% T | (non_na == 0)]
}

# If any remain, take lowest emp_id
timeline_week_conflict <- 
  timeline_week_conflict[, max := min(emp_id, na.rm = T),
                         by = .(case_id, week)]
timeline_week_conflict <- 
  timeline_week_conflict[(emp_id == max) %in% T] 
timeline_week_conflict <- 
  timeline_week_conflict[,c("max", "non_na") := NULL]

timeline_week_conflict_r <-
  timeline_week_conflict_r[, max := min(emp_id, na.rm = T),
                           by = .(case_id, week)]
timeline_week_conflict_r <- 
  timeline_week_conflict_r[(emp_id == max) %in% T]
timeline_week_conflict_r <- 
  timeline_week_conflict_r[,c("max", "non_na") := NULL]
                                                 
# Merge back into main data set
timeline <- bind_rows(timeline[obs <= 1], timeline_week_conflict) |> 
  select(-obs) |> 
  data.table()

timeline_r <- bind_rows(timeline_r[obs <= 1], 
                        timeline_week_conflict_r) |>
  select(-obs) |>
  data.table()

# # Check how many observations each week
# timeline <- timeline[, count := .N, by = .(case_id, week)]
# temp <- timeline[count > 1]

# Create w_tenure in timeline to tack tenure week by week
# Try to account for fact that some jobs have already started with 
# tenure by using tenure
timeline <- timeline[
  !is.na(emp_id),
  w_tenure := max_tenure 
  + time_length(week - week_end_job, unit = "week")
  ]
# If negative, assume weeks are correct
timeline <- timeline[
  w_tenure < 0, 
  w_tenure := time_length(week - week_start_job, unit = "week")
  ]
# Set jobs "started" before 1979 to w_tenure = NA
timeline <- timeline[week_start_job < ymd("1979-01-01"), w_tenure := NA]

# Note, for matched, use tenure not max_tenure
timeline_r <- timeline_r[
  !is.na(emp_id),
  w_tenure := tenure + time_length(week - week_end_job, unit = "week")
  ]
# If negative, assume weeks are correct
timeline_r <- timeline_r[
  w_tenure < 0, 
  w_tenure := time_length(week - week_start_job, unit = "week")
  ]
# Set jobs "started" before 1979 to w_tenure = NA
timeline_r <- timeline_r[week_start_job < ymd("1979-01-01"),
                         w_tenure := NA]

# Count working spells that aren't matched (NA == 0). Set these emp_ids
# to their number, ie 1, 2, ... . Also mark start and end week
timeline <- timeline[order(week)]
timeline <- 
  timeline[,c("working_next", "working_prev") := shift(.SD, c(-1,1)), 
           by = case_id, .SDcols = "working"]
timeline <- 
  timeline[,c("emp_id_next", "emp_id_prev") := shift(.SD, c(-1,1)), 
           by = case_id, .SDcols = "emp_id"]

timeline_r <- timeline_r[order(week)]
timeline_r <- 
  timeline_r[,c("working_next", "working_prev") := shift(.SD, c(-1,1)),
             by = case_id, .SDcols = "working"]
timeline_r <- 
  timeline_r[,c("emp_id_next", "emp_id_prev") := shift(.SD, c(-1,1)),
             by = case_id, .SDcols = "emp_id"]

# Mark start/end as week if prev/next working is 0 or NA 
# or if emp_id prev/next exists
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
timeline <- 
  timeline[is.na(emp_id) & !is.na(week_start_job) & (working != 0),
           emp_id := rank(week_start_job), by = case_id]

timeline <- timeline[working == 1 & (is.na(emp_id) | emp_id < 1000),
                       emp_id := nafill(emp_id, type = "locf"),
                       by = case_id]

# Drop unneeded variables. 
timeline <- timeline[, c(
  "week_start_match", "week_end_match", "working_next", "working_prev",
  "emp_id_next", "emp_id_prev") := NULL]

timeline_r <- timeline_r[, c(
  "week_start_match", "week_end_match", "working_next", "working_prev",
  "emp_id_next", "emp_id_prev") := NULL]

# Plot Timeline -----------------------------------------------------------

# Results noisy at end, so drop after max week, around end of 2016
week_max <- round_date(ymd("2016-10-08"), "week")

# # When using only surveys 2012 and before, use where tradional starts
# # dropping as week_max
# week_max <- round_date(ymd("2012-09-18"), "week")

# Graph observations (just graph men for final data set)
values <- c("Working" = "blue", "Traditional" = "green", 
            "Not Working" = "red", "Unemployed" = "purple")
            
temp <- timeline |> 
  group_by(week) |> 
  summarise(working_obs = sum(working, na.rm = T), 
            traditional_obs = sum(traditional, na.rm = T),
            non_working_obs = sum(1 - working, na.rm = T),
            unemp_obs = sum(unemployed, na.rm = T)) |> 
  ggplot() +
  geom_line(aes(x = week, y = working_obs), color = "blue") +
  geom_line(aes(x = week, y = traditional_obs), color = "green") +
  geom_line(aes(x = week, y = non_working_obs), color = "red") +
  geom_line(aes(x = week, y = unemp_obs), color = "purple") +
  geom_vline(xintercept = week_max) +
  scale_color_manual(name = "Observations", values = values) +
  labs(x = "Year", y = "Observations") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_light(base_size = 16) 

ggsave(str_c(figure_folder, "Observations.pdf"), 
       height = height, width = width)

timeline <- timeline[week <= week_max]

timeline_r <- timeline_r[week <= week_max]

# Free up space
rm(temp)

# Observations ------------------------------------------------------------

# How many observations? Number of workers, worker-jobs,
# worker-job-interviews, and worker-week

# Worker Level
num_worker <- matched_jobs |> 
  group_by(case_id) |> 
  summarise(female = mean(female)) |> 
  group_by(female) |> 
  count() |> 
  arrange(female)

# Worker-job Level
num_worker_job <- matched_jobs |> 
  group_by(case_id, emp_id) |> 
  summarise(female = mean(female)) |> 
  group_by(female) |> 
  count() |> 
  arrange(female)

# Worker-job-interview Level
num_worker_job_interview <- matched |> 
  group_by(female) |> 
  count() |> 
  arrange(female)
  
table <- "\\begin{tabular}{lSS}
\\toprule
Level & Just Men & Men + Women \\\\ \\midrule
"

table <- str_c(
  table,
  "Worker", format_n(num_worker$n[1]), 
  format_n(num_worker$n[1] + num_worker$n[2]),
  " \\\\ \n",
  "Worker-Job", format_n(num_worker_job$n[1]), 
  format_n(num_worker_job$n[1] + num_worker_job$n[2]), " \\\\ \n",
  "Worker-Job-Interview", format_n(num_worker_job_interview$n[1]), 
  format_n(num_worker_job_interview$n[1] 
           + num_worker_job_interview$n[2]),
  " \\\\ \n",
  "Worker-Week", format_n(NROW(timeline)), "& {--}", " \\\\ \n"
)

name <- "Number of Observations"
label <- "observations"
note <- "The number of observations in final data set at various
levels of aggregation."

header <- make_header("", name, label)

bot <- make_bot(note)

write.table(str_c(header, table, bot),
            str_c(table_folder, 
                  "NLSY79 Match Quality/Number of Observations.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Outsourcing Prevalence --------------------------------------------------

# How common is outsourcing? Take the average of all person-job-weeks
outsourcing_prevalence <- timeline |> 
  filter(!is.na(outsourced)) |> 
  as_survey_design(ids = case_id, weights=weight) |>
  summarise(
    outsourced_per = survey_mean(outsourced * 100, vartype = "ci"))

# How common is outsourcing in each occupation? Take the average of all 
# person-job-weeks. Also see if real weekly wage is above occupation
# average
outsourcing_occ <- timeline |> 
  filter(!is.na(outsourced), !is.na(occ)) |> 
  as_survey_design(ids = case_id, weights=weight) |>
  group_by(occ) |> 
  mutate(
    wage_above_occ =
      ifelse(outsourced == 1 %in% T,
             log_real_wkly_wage > mean(log_real_wkly_wage, na.rm = T),
             NA)
  ) |> 
  summarise(outsourced_per = survey_mean(outsourced * 100),
            outsourced_wage_above_per = 
              survey_mean(wage_above_occ * 100, na.rm = T),
            week_obs = unweighted(n()),
            outsourced_week_obs = unweighted(sum(outsourced))) |> 
  # Define high outsourcing occupation (ho_occ) if 
  # 2 * average outsourcing
  mutate(
    ho_occ = 1 * (outsourced_per >= 
                    2 * outsourcing_prevalence$outsourced_per)
  )

# Do similar things (but not as much) for industry
outsourcing_ind <- timeline |> 
  filter(!is.na(outsourced), !is.na(ind)) |> 
  as_survey_design(ids = case_id, weights=weight) |>
  group_by(ind) |> 
  summarise(outsourced_per = survey_mean(outsourced * 100),
            week_obs = unweighted(n()),
            outsourced_week_obs = unweighted(sum(outsourced)))

# Want to match this data with occupation names for my own use.
# Use census_occ/ind_2000_names from
# https://www.census.gov/topics/employment/industry-occupation/guidance/code-lists.html
# (Use 2002 excel sheets)
occ_names <- read_excel(str_c(raw_folder, 
                              "census_occ_2000_names.xls")) |> 
  select(description = `Occupation Code List`, occ = `...3`) |> 
  mutate(occ = as.numeric(occ)) |> 
  filter(!is.na(occ))

outsourcing_occ <- left_join(outsourcing_occ, occ_names, by = "occ")

ind_names <- read_excel(str_c(raw_folder, 
                              "census_ind_2000_names.xls")) |> 
  select(description = `American Community Survey`, ind = `...4`) |> 
  mutate(ind = as.numeric(ind)) |> 
  filter(!is.na(ind), !is.na(description))

outsourcing_ind <- left_join(outsourcing_ind, ind_names, by = "ind")

# Create an excel file with all industries and occupations 
# that are outsourced
l <- list(
  "Occ" = outsourcing_occ |> 
    select(occ, ho_occ, week_obs, outsourced_week_obs, outsourced_per, 
           outsourced_wage_above_per, description),
  "Ind" = outsourcing_ind |>
            select(ind, week_obs, outsourced_week_obs, 
                   outsourced_per, description))

write.xlsx(l, file=str_c(table_folder, "ind_occ.xlsx"))

# Clear some space
rm(outsouring_ind, ind_names, l, weights)

# Also make a table with relevant info 
# (outsourced %, number of occupations, etc)
outsourcing_occ_ss <- outsourcing_occ |>
  summarise(
    occupations = n(),
    occupations_any = sum(outsourced_per > 0),
    ho_occ = sum(ho_occ)
  )

# Save a latex table of top 10 most outsourced jobs for 
# low skilled (<ba) and high skilled (>ba) workers
outsourcing_occ_ls <- timeline |> 
  filter(!is.na(outsourced), !is.na(occ), 
         (less_hs == 1) | (hs == 1) | (aa == 1)) |> 
  as_survey_design(ids = case_id, weights=weight) |>
  group_by(occ) |>  
  summarise(outsourced_week_obs = unweighted(sum(outsourced))) |> 
  left_join(occ_names, by = "occ") |> 
  arrange(desc(outsourced_week_obs)) |> 
  head(10) |> 
  select(Occupation = description)

# Save Tables

center <- outsourcing_occ_ls |>
  mutate(
    Ra = row_number(),
    nk = ".",
    " & " = " & ",
    " \\\\" = " \\\\"
    ) |>
  select(Ra, nk, " & ", Occupation, " \\\\")
  
center <- center_to_latex(center)

top <- "\\begin{tabular}{rl}
\\toprule
Rank & Occupation \\\\ \\midrule
"

name <- "Most Commonly Outsourced Jobs For Low-Skilled Workers"
label <- "out_occs_ls"
note <- "The ten most common outsourced jobs for workers
with less than a college degree. 
Amount of outsourcing is measured as the number of weeks 
workers report ``contracted out'' as their job type."

header <- make_header("", name, label)
d_header <- make_header("d", name, label)
s_header <- make_header("s")

bot <- make_bot(note)
s_bot <- make_bot(slide = TRUE)

write.table(
  str_c(header, top, center, bot, "\n \\end{document}"),
  str_c(table_folder, "NLSY79 Occupation Info/",
        "Occupation Outsourcing Low-Skilled 10.tex"),
  quote=F, col.names=F, row.names=F, sep="")

# Make one for Drafts
write.table(str_c(d_header, top, center, bot),
            str_c(d_table_folder, 
                  "Occupation Outsourcing Low-Skilled 10.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Make one for Slides
write.table(str_c(s_header, top, center, s_bot),
            str_c(s_table_folder, 
                  "Occupation Outsourcing Low-Skilled 10.tex"),
            quote=F, col.names=F, row.names=F, sep="")

outsourcing_occ_hs <- timeline |> 
  filter(!is.na(outsourced), !is.na(occ), (ba == 1) | (plus_ba == 1)) |> 
  as_survey_design(ids = case_id, weights=weight) |>
  group_by(occ) |>  
  summarise(outsourced_week_obs = unweighted(sum(outsourced))) |> 
  left_join(occ_names, by = "occ") |> 
  arrange(desc(outsourced_week_obs)) |> 
  head(10) |> 
  select(Occupation = description)

# Save Tables

center <- outsourcing_occ_hs |>
  mutate(
    Ra = row_number(),
    nk = ".",
    " & " = " & ",
    " \\\\" = " \\\\"
  ) |>
  select(Ra, nk, " & ", Occupation, " \\\\")

center <- center_to_latex(center)

name <- "Most Commonly Outsourced Jobs For High-Skilled Workers"
label <- "out_occs_hs"
note <- "The ten most common outsourced jobs for workers
with a college degree or more. 
Amount of outsourcing is measured as the number of weeks 
workers report ``contracted out'' as their job type."

header <- make_header("", name, label)
d_header <- make_header("d", name, label)
s_header <- make_header("s")

bot <- make_bot(note)
s_bot <- make_bot(slide = TRUE)

write.table(
  str_c(header, top, center, bot, "\n \\end{document}"),
  str_c(table_folder, "NLSY79 Occupation Info/",
        "Occupation Outsourcing High-Skilled 10.tex"),
  quote=F, col.names=F, row.names=F, sep="")

# Drafts
write.table(str_c(d_header, top, center, bot),
            str_c(d_table_folder, 
                  "Occupation Outsourcing High-Skilled 10.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Slides
write.table(str_c(s_header, top, center, s_bot),
            str_c(s_table_folder, 
                  "Occupation Outsourcing High-Skilled 10.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Match Occupation Outsourcing Info ---------------------------------------

# Integrate occupation data into main datasets
matched <- data.table(matched)
matched_jobs <- data.table(matched_jobs)

outsourcing_occ <- outsourcing_occ |>
  select(occ, outsourced_per, ho_occ, outsourced_wage_above_per) |> 
  data.table()

setkey(matched, occ)
setkey(matched_jobs, occ)
setkey(timeline, occ)
setkey(outsourcing_occ, occ)

matched <- merge(matched, outsourcing_occ, all.x = T)

matched[order(case_id, int_year, emp_id), 
        ever_ho_occ := 1 * any(ho_occ %in% T), by = .(case_id)] |> 
  setcolorder(c("case_id", "int_year", "emp_id", "hours_week")) 

matched_jobs <- merge(matched_jobs, outsourcing_occ, all.x = T)

matched_jobs[order(case_id, emp_id),
             ever_ho_occ := 1 * any(ho_occ %in% T), by = .(case_id)] |> 
  setcolorder(c("case_id", "emp_id", "hours_week")) 

timeline <- merge(timeline, outsourcing_occ, all.x = T)
timeline[order(case_id, week),
         ever_ho_occ := 1 * any(ho_occ %in% T), by = .(case_id)] |> 
  setcolorder(c("case_id", "week", "emp_id", "working", "unemployed",
                "hours_week")) 

# Find outsourcing prevalence in HO Occupations
ho_outsourcing_prevalence <- timeline |> 
  filter(!is.na(outsourced), ho_occ == T) |> 
  as_survey_design(ids = case_id, weights=weight) |>
  summarise(
    outsourced_per = survey_mean(outsourced * 100, vartype = "ci"),
    ho_occ_per = survey_mean(ho_occ * 100, vartype = "ci"))

# Save table
table <- str_c(
  "\\begin{tabular}{lr}
  \\toprule
  Variable & Value \\\\ \\midrule
  \\textbf{All Occupations} & \\\\\n",
  "Number ", format_n(outsourcing_occ_ss$occupations), "\\\\\n",
  "Percent of Workers Outsourced ",
  format_val(outsourcing_prevalence$outsourced_per),
  "\\\\\n",
  "Occupations with any Outsourcing ",
  format_n(outsourcing_occ_ss$occupations_any),
  "\\\\\n",
  "\\textbf{High Outsourcing Occupations ($\\geq$", 
  round(2 * outsourcing_prevalence$outsourced_per, 2),
  "\\%) } & \\\\\n",
  "Number ", format_n(outsourcing_occ_ss$ho_occ), "\\\\\n",
  "Percent of Jobs ", format_val(
    outsourcing_occ_ss$ho_occ / outsourcing_occ_ss$occupations * 100),
  "\\\\\n",
  "Percent of Workers Outsourced ", 
  format_val(ho_outsourcing_prevalence$outsourced_per),
  "\\\\\n"
)

name <- "Occupations and Outsourcing"
label <- "outsourcing_occ"
note <- "Outsourcing prevalence among occupations and (male) workers.
High Outsourcing occupations have more than twice the average 
level of outsourcing than the rest of the population."

header <- make_header("", name, label)
d_header <- make_header("d", name, label)
s_header <- make_header("s")

bot <- make_bot(note)
s_bot <- make_bot(slide = TRUE)

write.table(str_c(header, table, bot, "\n \\end{document}"),
            str_c(table_folder, 
                  "NLSY79 Occupation Info/Occupation Outsourcing.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Make one for Drafts
write.table(str_c(d_header, table, bot),
            str_c(d_table_folder, "Occupation Outsourcing.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Make one for Slides
write.table(str_c(s_header, table, s_bot),
            str_c(s_table_folder, "Occupation Outsourcing.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save datasets
fwrite(matched, str_c(clean_folder, "matched.csv"), row.names = FALSE)
fwrite(matched_jobs, str_c(clean_folder, "matched_jobs.csv"), row.names = FALSE)
fwrite(timeline, str_c(clean_folder, "matched_timeline.csv"), row.names = FALSE)
fwrite(timeline_r, str_c(clean_folder, "matched_timeline_robust.csv"), row.names = FALSE)