# This file takes emp_hist_rost_clean, emp_sum_clean, on_jobs_clean, demographics_clean,
# and weights and matches these data sets together (matched)
# It then creates a dataset of all jobs by averaging job characteristics 
# over each interview's observations (matched_jobs)
# I then combine matched_jobs with timeline_clean to create a timeline of jobs
# (timeline)
# I use this timeline to see the prevalance of outsourcing for each
# occupation and merge this information into the three datasets
# It saves the data in matched, matched_jobs, and matched_timeline

rm(list = ls())

library(openxlsx)
library(readxl)
library(srvyr)
library(data.table)
library(magrittr)
library(DescTools)
library(lubridate)
library(tidyverse)

# Folders of interest
raw_folder <- "../Raw Data/"
clean_folder <- "../Cleaned Data/"
table_folder <- "../Tables/"
figure_folder <- "../Figures/NLSY 79 Timeline/"

# Create a default table_top
table_top <- "\\documentclass[12pt]{article}
\\usepackage[margin=.5in]{geometry}
\\usepackage{booktabs}
\\begin{document}
\\begin{table}
\\centering"

# For saving graphs
aspect_ratio <- 1.62
height <- 7
width <- height * aspect_ratio

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
                       col_types = cols(.default = col_double())) %>% 
  rename(case_id = `1`,
         weight = `0`)

# # Get custom weights from raw_folder (When only using 2012 and before)
# weights <- read_table2(str_c(raw_folder, "customweight_less2012.dat"),
#                        col_types = cols(.default = col_double())) %>% 
#   rename(case_id = `1`,
#         weight = `0`)


# Demographics data
demographics <- read_csv(str_c(clean_folder, "demographics_clean.csv"),
                         col_types = cols(.default = col_double()))

# Data from On Jobs 
on_jobs <- read_csv(str_c(clean_folder, "on_jobs_clean.csv"),
                    col_types = cols(
                      .default = col_double(),
                      month_start_job = col_date(format = ""),
                      month_end_job = col_date(format = "")
                    ))

# Create a function to take a variable name and data set. Return the varibale if
# filled. If NA, check if other"s in group that are not NA. If all are NA, return
# NA, otherwise return mean (or mode below)
fill_NA_mean <- function(vector){
  ifelse(
    !is.na(vector), vector,
    ifelse(all(is.na(vector)), NA, mean(vector, na.rm = T))
  )
}

fill_NA_mode <- function(vector){
  ifelse(
    !is.na(vector), vector,
    ifelse(all(is.na(vector)), NA, Mode(vector, na.rm = T))
  )
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
format_n <- function(n, front = "") {
  str_c(" & {", front, format(n, big.mark = ",", trim = T), "} ")
}

# Create Match Between Datasets -------------------------------------------------------

# Join Employer Histoy Roster, Employer Supplement, Demographics, and weights
# For hist_rost and emp_sup, use outer join because both are measures 
# of total number of jobs. 
# For the others, use left_join to match data to jobs
ehr_es <- full_join(hist_rost, emp_sup, by = c("case_id", "emp_id", "int_year"))

ehr_es <- left_join(ehr_es, demographics, by = c("case_id", "int_year"))

ehr_es <- left_join(ehr_es, weights, by = c("case_id")) 

# Matching ehr_es to on_jobs.
# There are many ways to do this, and some are more believable than others.
# Always match based on case_id and int_year

# A. First try to match based on month_ start/end _job and job #/rank. Start with all 
#    three of these and gradually take them away. Measure match quality from highest (1)
#    to lowest (9). Each time, take out already matched pairs.

#    1. If all three match, these are highest quality matches
#    2. My rank measure is imputed and may be wrong. Match start/end date
#    3. Start date is generally more helpful than end date, as this is 
#       when outsoursing (and other) questions are asked. Do start date + rank 
#    4. End date can also be helpful sometimes. Do end date + rank
#    5. Do start without rank
#    6. Do end without rank 
#    7. Many jobs are the only observation within it's year on both ends, so assume
#       these are matches. Only potential issue is that I drop some jobs that I don"t
#       have any info on, so some jobs might not truely be the only type. 
#    8. Some years have the same job information for all jobs. Downside of this is
#       we can't bring in date start/end job or rank.
#    9. Match only based on rank
#
# B. Once we have these separate matches, combine them all into one data set.
#    If multiple case_id-year-emp_id matches, only keep the one with highest quality.
#    If multple cases have same quality, drop them all
#
# C. Do one final match to create the matched data set. This will have many NA"s
#    for outsourced (and other), impute them from other case_id-emp_id matches.
#    Keep only matches where date start/end job are consistent (one/both NA fine),
#    And where outsourcing + other variables are 1 or 0

# Create a function to anti_join all datasets in a list based on a list
# of variables
anti_join_list <- function(data, data_list, var_vec, om = 0) {
  for (anti_data in data_list) {
    data %<>% 
      anti_join(anti_data, by = var_vec)
  }
  if (om == 1){
    data %<>%
      group_by(case_id, int_year) %>% 
      filter(n() == 1) %>% 
      ungroup() 
  }
  data
}

rename_if_cust <- function(.tbl, .predicate, .funs = list(), .cond = TRUE, ...) {
  if (.cond) {
    .tbl <- rename_if(.tbl, .predicate, .funs)
  }
  .tbl
}

# Create a function that takes these datasets and a list of variables and matches
# them together, potentially using 7.only matches (om) or 8.same info (si)
match_data <- function(oj, ehr, ajl, match_vec, mq = 0, om = 0, si = 0) {
  
  oj %<>% 
    rename(job = rank) %>% 
    filter_at(vars(!!match_vec), all_vars(!is.na(.))) 
  
  if (si == 1) {
    oj %<>%
      select(case_id:ever_out_oj) %>% 
      unique()
  }
  
  if (om == 1) {
    oj %<>%
      group_by(case_id, int_year) %>% 
      filter(n() == 1) %>% 
      ungroup() 
  }
  
  names_change <- str_c(match_vec[3:length(match_vec)], "$", collapse = "|")
  
  match <- ehr %>% 
    anti_join_list(ajl, c("case_id", "int_year", "emp_id"), om = om) %>% 
    inner_join(oj, by = match_vec, suffix = c("_ehr", "_oj")) %>% 
    mutate(match_quality = mq) 
  
  if (si == 0) {
    match %<>%
      rename_if_cust(.predicate = str_detect(names(.), names_change),
                     .funs = ~str_c(., "_oj"),
                     .cond = !is.na(names_change)) %>% 
      select(case_id, emp_id, int_year, job_oj, month_start_job_oj, month_end_job_oj,
             looped:ever_out_oj, match_quality) %>%
      unique()
  } else {
    match %<>%
      select(case_id, emp_id, int_year, looped:ever_out_oj, match_quality) %>%
      unique()
  }
}

# Create a list of variables to match over, and whether to use only matches
# or single info for each quality
match_base <- c("case_id", "int_year")
match_list <- 
  list(
    append(match_base, c("month_start_job", "month_end_job", "job")),
    append(match_base, c("month_start_job", "month_end_job")),
    append(match_base, c("month_start_job", "job")),
    append(match_base, c("month_end_job", "job")),
    append(match_base, c("month_start_job")),
    append(match_base, c("month_end_job")),
    match_base,
    match_base,
    append(match_base, c("job"))
)
om <- c(0, 0, 0, 0, 0, 0, 1, 1, 0)
si <- c(0, 0, 0, 0, 0, 0, 0, 1, 0)
matches <- c()

# Loop over quality 1:9
for (i in seq_along(om)) {
  matches %<>% c(list(match_data(on_jobs, ehr_es, matches, match_list[[i]],
                               mq = i, om = om[i], si = si[i])))
}

match_rhs <- bind_rows(matches) %>% 
  group_by(case_id, emp_id, int_year) %>% 
  filter(match_quality == min(match_quality)) %>%
  filter(n() == 1) %>% 
  ungroup()


# Match and Clean Data ----------------------------------------------------

fill_mean <- c("indep_con", "on_call", "outsourced", "self_emp", "temp_work",
               "traditional", "ever_out_oj", "match_quality")

View(matched %>% filter(match_flag_1))

matched <- 
  left_join(ehr_es, match_rhs, by = c("case_id", "emp_id", "int_year"),
            suffix = c("_ehr", "_oj")) %>% 
  # Fill in missing data by case_id, emp_id
  group_by(case_id, emp_id) %>% 
  mutate_at(fill_mean, fill_NA_mean) %>% 
  mutate(
    # Flag matches where month_start and month_end don"t line up
    match_flag_1 = (((month_start_job - month_start_job_oj != 0) %in% T) 
                  | ((month_end_job - month_end_job_oj != 0) %in% T)),
    
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
    
    # If match qualities disagree, set equal to lowest
    match_quality = 
      ifelse(!is.na(match_quality),
             min(match_quality, na.rm = T), NA),
    
    # Record Max Tenure for each job
    max_tenure = ifelse(max(tenure, na.rm = T) > 0, max(tenure, na.rm = T), 0)
  ) %>% 
  # Make sure there are unique case_id, int_year, emp_id matches
  group_by(int_year, add = T) %>%
  mutate(count = n()) %>% 
  ungroup() 


# Match Quality -----------------------------------------------------------

# How good is the match? Create some tables showing match quality from
# both sides (OJ and EHR/ES)

# Total jobs in each survey
ehr_es_obs <- NROW(ehr_es)
ehr_obs <- NROW(hist_rost)
es_obs <- NROW(emp_sup)
oj_obs <- NROW(on_jobs)

# What is overlap/exclusion between hist_rost and emp_sup
only_ehr_obs <- NROW(
  anti_join(hist_rost, emp_sup, by = c("case_id", "int_year", "emp_id")))

only_es_obs <- NROW(
  anti_join(emp_sup, hist_rost, by = c("case_id", "int_year", "emp_id")))

both_es_ehr_obs <- ehr_obs - only_ehr_obs

# Jobs in ehr/es not matched with oj
unmatched_obs <- sum(is.na(matched$match_quality))

matched <- filter(matched, !is.na(match_quality))

# Jobs dropped from match because start/end dates don"t align
flag_1_obs <- sum(matched$match_flag_1)

matched %<>% filter(match_flag_1 == F)

# Jobs dropped from match because no or conflicting job
# type info
flag_2_obs <- sum(matched$match_flag_2)

matched %<>% 
  filter(match_flag_2 == F) %>%
  group_by(case_id) %>% 
  # How many ever_out are in matched dataset. How does this compare to On Jobs?
  mutate(ever_out_m = ifelse(all(is.na(outsourced)), 0, max(outsourced, na.rm = T))) %>% 
  ungroup()

# Define ever_out if report outsourced in oj. How would
# this be different if used es/ehr
ever_out_count <- matched %>% 
  group_by(case_id) %>% 
  filter(row_number() == min(row_number())) %>%  
  group_by(ever_out_oj, ever_out_m) %>%
  count()

# Create a match_quality table
m_q_table <- table(matched$match_quality)
looped_m_q_table <- table(matched$match_quality[matched$looped == 1])
# For now, no quality 8 matches. Turn into factor to capture 0
outsourced_m_q_table <- table(
  factor(matched$match_quality[matched$outsourced == 1],
         levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9")))

# Look at oj missed
on_jobs_miss <- on_jobs %>%
  anti_join(matched, by = c("case_id", "int_year", "rank" = "job_oj"))

# Total jobs matched
m_obs <- NROW(matched)
m_outsourced <- sum(matched$outsourced)

# How many missed, how many with info missed, how many outsourcing missed
# Note that we should account for jobs matched using same type within year
oj_missed <- NROW(on_jobs_miss) - looped_m_q_table[["8"]]
oj_info <- sum(!is.na(on_jobs$looped))
oj_info_missed <- sum(!is.na(on_jobs_miss$looped)) - looped_m_q_table[["8"]]
oj_outsourced <- sum(on_jobs$outsourced, na.rm = T)
oj_outsourced_missed <-
  sum(on_jobs_miss$outsourced, na.rm = T) - outsourced_m_q_table[["8"]]

# Create Tables

# First, numbers behing creating matched
obs <- c(both_es_ehr_obs, only_ehr_obs, only_es_obs, ehr_es_obs,
         unmatched_obs, flag_1_obs, flag_2_obs, m_obs)

labels <- c("In Employer History Roster and Employer Supplement",
            "Only in Employer History Roster",
            "Only in Employer Supplement",
            "",
            "Unmatched with On Jobs",
            "Conflicting Start or Stop Dates",
            "Missing/Conflicting Job Types",
            "Total")

p_m <- c("", "+", "+", "", "-", "-", "-", "")

end <- c("\n", "\n", "\\midrule \n", "\\midrule \n", "\n", "\n", "\\midrule \n", "\n")

top <- str_c(table_top,
"\\begin{tabular}{lr}
\\toprule
Subset & Number \\\\ \\midrule
"
)

for (i in seq_along(obs)){
  top <- str_c(top, labels[i], format_n(obs[i],  p_m[i]), "\\\\ ", end[i])
}

bot <- "\\bottomrule
\\end{tabular}
\\caption{The matching process for the Employer History Roster/Employer Supplement
and observations lost/gained step by step.}
\\label{match}
\\end{table}
\\end{document}"

write.table(str_c(top, bot),
            str_c(table_folder, "NLSY79 Match Quality/Match Steps.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Look at match quality for on jobs

top_oj <- str_c(table_top, 
"\\begin{tabular}{lrrr}
\\toprule
Subset & Unmatched & Total & Percent Missing \\\\ \\midrule
"
)

top_oj <- str_c(
  top_oj,
  str_c("On Jobs", 
        format_n(oj_missed),
        format_n(oj_obs),
        format_val(oj_missed / oj_obs * 100, r = 0)),
  " \\\\ \n",
  str_c("On Jobs with Information",
        format_n(oj_info_missed),
        format_n(oj_info),
        format_val(oj_info_missed / oj_info * 100, r = 0)),
  " \\\\ \n",
  str_c("On Jobs Outsourced",
        format_n(oj_outsourced_missed),
        format_n(oj_outsourced),
        format_val(oj_outsourced_missed / oj_outsourced * 100, r = 0)),
  " \\\\ \n"
  )

bot_oj <- "\\bottomrule
\\end{tabular}
\\caption{The matching quality from On Jobs section.}
\\label{oj_match}
\\end{table}
\\end{document}"

write.table(str_c(top_oj, bot_oj),
            str_c(table_folder, "NLSY79 Match Quality/Match On Jobs.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Look at match quality in matched overall and for outsourced

labels <- c("1. Matched start date, end date, and rank",
            "2. Matched start date and end date",
            "3. Matched start date and rank",
            "4. Matched end date and rank",
            "5. Matched start date",
            "6. Matched end date",
            "7. Only unmatched job in year",
            "8. Only unmatched job type in year",
            "9. Matched rank")

top_q <- str_c(table_top, 
"
\\begin{tabular}{lrr}
\\toprule
Match Quality & Overall & Outsourced  \\\\ \\midrule
"
)

for (i in seq_along(labels)){
  top_q <- str_c(top_q, labels[i],
                format_n(m_q_table[[i]]),
                format_n(outsourced_m_q_table[[i]]), "\\\\ \n")
}

top_q <- str_c(top_q, " \\midrule \n Total",
                format_n(m_obs),
                format_n(m_outsourced), "\\\\ \n")

bot_q <- "\\bottomrule
\\end{tabular}
\\caption{Match quality of final dataset.}
\\label{match_quality}
\\end{table}
\\end{document}"

write.table(str_c(top_q, bot_q),
            str_c(table_folder, "NLSY79 Match Quality/Match Quality.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# What is match quality by int_year?
mq_year <- matched %>% 
  group_by(int_year) %>% 
  count(match_quality) %>% 
  pivot_wider(names_from = c(int_year), values_from = c(n))


# Create Matched Jobs -----------------------------------------------------

# Variables to use mean, mode, max, and min 
mean_vars <- c("hours_week", "tenure", "union", "log_real_hrly_wage",
               "log_real_wkly_wage", "part_time", "childcare", "dental",
               "flex_sched", "health", "job_sat", "life", "maternity", "profit_share",
               "retirement", "train_school", "any_benefits", "sample_id", "female",
               "black", "hispanic", "hh_child", "tot_child", "less_hs", "hs",
               "aa", "ba", "plus_ba", "educ_other", "age", "weight", "self_emp",
               "indep_con", "on_call", "outsourced", "temp_work", "traditional",
               "ever_out_oj", "ever_out_m")

mode_vars <- c("ind", "occ", "ind_cat", "occ_cat", "marital_status", "msa", "region",
               "union_fill")

min_vars <- c("week_start_job")

max_vars <- c("week_end_job", "max_tenure")

# Create mean, min, and max functions without na
mean_na <- function(vector) mean(vector, na.rm = T)
min_na <- function(vector) min(vector, na.rm = T)
max_na <- function(vector) max(vector, na.rm = T)

# Create a variable that finds Mode. If multiple, takes min
find_mode <- function(vector) {
  temp <- Mode(vector, na.rm = T)[1]
  ifelse(!is.na(temp), temp, 
         ifelse(min(vector, na.rm = T) < 1e10, min(vector, na.rm = T), NA))
}

# Create matched_jobs which groups data by jobs
matched_jobs <- list(.vars = lst(mean_vars, mode_vars, min_vars, max_vars),
                     .funs = lst(mean_na, find_mode, min_na, max_na)
                     ) %>% 
  pmap(~ matched %>% group_by(case_id, emp_id) %>% summarise_at(.x, .y)) %>% 
  reduce(inner_join, by = c("case_id", "emp_id")) %>% 
  # Fix max_tenure -Inf as NA
  mutate(max_tenure = ifelse(max_tenure >= 0, max_tenure, NA))

# Drop uneeded variables
matched <- matched %>% 
  select(-month_start_job, -month_end_job, -job, -job_oj, -month_start_job_oj,
         -month_end_job_oj, -looped, -match_flag_1, -match_flag_2,
         -count, -looped, -birth_year)

# Remove uneeded data sets to free up memory
rm("matches", "match_rhs", "on_jobs_miss", "ever_out_count", "match_list",
   "on_jobs", "ehr_es", "emp_sup", "hist_rost", 
   "si", "om", "p_m", "top", "bot", "oj_info", "oj_info_missed", "oj_missed",
   "oj_obs", "m_q_table", "obs", "match_base", "labels", "end", "fill_mean",
   "bot_oj", "bot_q", "mean_vars", "mode_vars", "max_vars", "min_vars")

# Merge Timeline With Job Info --------------------------------------------

# Merge job info from hist_rost using start and end weeks
# Because merging based on a condition, easier to use data.table

# Timeline data (make sure week is a date)
timeline <- fread(str_c(clean_folder, "timeline_clean.csv"), 
                  colClasses = list(
                    double = c(1:2, 4:5),
                    Date = c("week")
                  ))

# Merge demographic info + weights first
demographics_merge <- demographics %>% 
  group_by(case_id) %>% 
  filter(int_year == min(int_year)) %>% 
  select(case_id:birth_year, less_hs:educ_other) %>% 
  select(-female)
  
# Merge demographic data into weights
weights_t <- left_join(weights, demographics_merge, by = "case_id") %>% 
  data.table()

timeline <- weights_t[timeline, on = .(case_id == case_id)]
timeline <- timeline[, `:=`(age = year(week) - birth_year, birth_year = NULL)]

# Create week_start/end_match to keep original data
temp_match <- matched_jobs %>% 
  select(case_id, emp_id, hours_week, part_time, occ, ind, tenure, max_tenure,
         week_start_job, week_end_job, log_real_wkly_wage, self_emp:traditional, 
         any_benefits, health, retirement, union, ever_out_oj, ever_out_m) %>%
  mutate(week_start_match = week_start_job,
         week_end_match = week_end_job) %>% 
  data.table()

# For space reasons, only match males
# Create week match so week stays in dataset
timeline <- timeline[female == 0, ]
timeline <- timeline[, `:=`(week_match = week, female = NULL)]

timeline <- temp_match[timeline,
                      on = .(case_id == case_id, 
                             week_start_match <= week_match,
                             week_end_match >= week_match), 
                      allow.cartesian = T]

rm(temp_match, weights_t, demographics, demographics_merge)

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

# Plot Timeline -----------------------------------------------------------

# Results noisy at end, so drop after max week, around end of 2016
week_max <- round_date(ymd("2016-10-08"), "week")

# # When using only surveys 2012 and before, use where tradional starts
# # dropping as week_max
# week_max <- round_date(ymd("2012-09-18"), "week")

# Graph observations
temp <- timeline %>% 
  group_by(week) %>% 
  summarise(working_obs = sum(working, na.rm = T), 
            traditional_obs = sum(traditional, na.rm = T),
            non_working_obs = sum(1 - working, na.rm = T),
            unemp_obs = sum(unemployed, na.rm = T)) %>% 
  ggplot() +
  geom_line(aes(x = week, y = working_obs), color = "blue") +
  geom_line(aes(x = week, y = traditional_obs), color = "green") +
  geom_line(aes(x = week, y = non_working_obs), color = "red") +
  geom_line(aes(x = week, y = unemp_obs), color = "purple") +
  geom_vline(xintercept = week_max) +
  labs(x = "Year", y = "Observations") +
  theme_light(base_size = 16) 

ggsave(str_c(figure_folder, "Observations.pdf"), height = height, width = width)

timeline <- timeline[week <= week_max]

# Outsourcing Prevalence --------------------------------------------------

# How common is outsourcing? Take the average of all person-job-weeks
outsourcing_prevalence <- timeline %>% 
  filter(!is.na(outsourced)) %>% 
  as_survey_design(ids = case_id, weights=weight) %>%
  summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci"))

# How common is outsourcing in each occupation? Take the average of all 
# person-job-weeks. Also see if real weekly wage is above occupation
# average
outsourcing_occ <- timeline %>% 
  filter(!is.na(outsourced), !is.na(occ)) %>% 
  as_survey_design(ids = case_id, weights=weight) %>%
  group_by(occ) %>% 
  mutate(
    wage_above_occ = ifelse(outsourced == 1 %in% T,
                            log_real_wkly_wage > mean(log_real_wkly_wage, na.rm = T), NA)
  ) %>% 
  summarise(outsourced_per = survey_mean(outsourced * 100),
            outsourced_wage_above_per = survey_mean(wage_above_occ * 100, na.rm = T),
            week_obs = unweighted(n()),
            outsourced_week_obs = unweighted(sum(outsourced))) %>% 
  # Define high outsourcing occupation (ho_occ) if 2* average outsourcing
  mutate(
    ho_occ = 1 * (outsourced_per >= 2 * outsourcing_prevalence$outsourced_per)
  )

# Do similar things (but not as much) for industry
outsourcing_ind <- timeline %>% 
  filter(!is.na(outsourced), !is.na(ind)) %>% 
  as_survey_design(ids = case_id, weights=weight) %>%
  group_by(ind) %>% 
  summarise(outsourced_per = survey_mean(outsourced * 100),
            week_obs = unweighted(n()),
            outsourced_week_obs = unweighted(sum(outsourced)))

# Want to match this data with occupation names for my own use.
# Use census_occ/ind_2000_names from
# https://www.census.gov/topics/employment/industry-occupation/guidance/code-lists.html
# (Use 2002 excel sheets)
occ_names <- read_excel(str_c(raw_folder, "census_occ_2000_names.xls")) %>% 
  select(description = `Occupation Code List`, occ = `...3`) %>% 
  mutate(occ = as.numeric(occ)) %>% 
  filter(!is.na(occ))

outsourcing_occ %<>% left_join(occ_names, by = "occ")

ind_names <- read_excel(str_c(raw_folder, "census_ind_2000_names.xls")) %>% 
  select(description = `American Community Survey`, ind = `...4`) %>% 
  mutate(ind = as.numeric(ind)) %>% 
  filter(!is.na(ind), !is.na(description))

outsourcing_ind %<>% left_join(ind_names, by = "ind")

# Create an excel file with all industries and occupations that are outsourced
l <- list(
  "Occ" = outsourcing_occ %>% 
    select(occ, ho_occ, week_obs, outsourced_week_obs, outsourced_per, 
           outsourced_wage_above_per, description),
  "Ind" = outsourcing_ind %>%
            select(ind, week_obs, outsourced_week_obs, outsourced_per, description))

write.xlsx(l, file=str_c(table_folder, "ind_occ.xlsx"))

# Also make a table with relevant info (outsourced %, number of occupations, etc)
outsourcing_occ_ss <- outsourcing_occ %>% 
  summarise(
    occupations = n(),
    occupations_any = sum(outsourced_per > 0),
    ho_occ = sum(ho_occ)
  )

table <- str_c(
  "\\documentclass[12pt]{article}
  \\usepackage[margin=.5in]{geometry}
  \\usepackage{booktabs}
  \\begin{document}
  \\begin{table}
  \\centering 
  \\begin{tabular}{lr}
  \\toprule
  Variable & Value \\\\ \\midrule
  Occupations ", format_n(outsourcing_occ_ss$occupations), "\\\\\n",
  "Occupations with any Outsourcing ", format_n(outsourcing_occ_ss$occupations_any),
  "\\\\\n",
  "Occupations with 2$\\times$ Average Outsourcing ",
  format_n(outsourcing_occ_ss$ho_occ), "\\\\\n",
  "Average Outsourcing ", format_val(outsourcing_prevalence$outsourced_per, r = 3),
  "\\\\\n",
  "\\bottomrule
  \\end{tabular}
  \\caption{Outsourcing prevalence among workers and occupations}
  \\label{outsourcing_occ}
  \\end{table}
  \\end{document}"
)

write.table(table,
            str_c(table_folder, "NLSY79 Occupation Info/Occupation Outsourcing.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# Match Occupation Outsourcing Info ---------------------------------------

# Integrate occupation data into main datasets
matched %<>% data.table()
matched_jobs %<>% data.table()

outsourcing_occ %<>% 
  select(occ, outsourced_per, ho_occ, outsourced_wage_above_per) %>% 
  data.table()

setkey(matched, occ)
setkey(matched_jobs, occ)
setkey(timeline, occ)
setkey(outsourcing_occ, occ)

matched <- merge(matched, outsourcing_occ, all.x = T)

matched[order(case_id, int_year, emp_id), 
        ever_ho_occ := 1 * any(ho_occ %in% T), by = .(case_id)] %>% 
  setcolorder(c("case_id", "int_year", "emp_id", "hours_week")) 

matched_jobs <- merge(matched_jobs, outsourcing_occ, all.x = T)

matched_jobs[order(case_id, emp_id),
             ever_ho_occ := 1 * any(ho_occ %in% T), by = .(case_id)] %>% 
  setcolorder(c("case_id", "emp_id", "hours_week")) 

timeline <- merge(timeline, outsourcing_occ, all.x = T)
timeline[order(case_id, week),
         ever_ho_occ := 1 * any(ho_occ %in% T), by = .(case_id)] %>% 
  setcolorder(c("case_id", "week", "emp_id", "working", "unemployed",
                "hours_week")) 

# Save datasets
fwrite(matched, str_c(clean_folder, "matched.csv"), row.names = FALSE)
fwrite(matched_jobs, str_c(clean_folder, "matched_jobs.csv"), row.names = FALSE)
fwrite(timeline, str_c(clean_folder, "matched_timeline.csv"), row.names = FALSE)