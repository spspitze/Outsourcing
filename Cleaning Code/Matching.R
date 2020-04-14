# This file takes cleaned combined out of the cleaned combined folder
# and matches the combined data sets
# It saves the data in matched_clean

rm(list = ls())

library(DescTools)
library(lubridate)
library(tidyverse)

# Folders of interest
raw_folder <- "Raw Data/"
clean_folder <- "Cleaned Data/"
table_folder <- "../Tables/"

# This is data from the Employer History Roster
hist_rost <- read_csv(str_c(clean_folder, "emp_hist_rost_clean.csv")) 

# Data from Employer Supplement 
emp_sup <- read_csv(str_c(clean_folder, "emp_sup_clean.csv"))

# Get custom weights from raw_folder
weights <- read_table2(str_c(raw_folder, "customweight.dat"))
# Rename weights. Weight currently is number of people represented (*100)
# Change to fraction of people represented by dividing by sum
colnames(weights) <- c("case_id", "weight")
weights <- mutate(weights, weight = weight / sum(weight, na.rm=T))

# Demographics data
demographics <- read_csv(str_c(clean_folder, "demographics_clean.csv"))

# Data from On Jobs 
on_jobs <- read_csv(str_c(clean_folder, "on_jobs_clean.csv")) 

# Create a function to take a variable name and data set. Return the varibale if
# filled. If NA, check if other"s in group that are not NA. If all are NA, return
# NA, otherwise return mean (or mode below)
fill_NA_mean <- function(vector){
  return(
    ifelse(
      !is.na(vector), vector,
      ifelse(all(is.na(vector)), NA, mean(vector, na.rm = T))
      )
    )
}

fill_NA_mode <- function(vector){
  return(
    ifelse(
      !is.na(vector), vector,
      ifelse(all(is.na(vector)), NA, Mode(vector, na.rm = T))
      )
    )
}

########################################################################


# Join Employer Histoy Roster, Employer Supplement, Demographics, and weights
# For hist_rost and emp_sup, use outer join because both are measures 
# of total number of jobs. 
# For the others, use left_join to match data to jobs
ehr_es <- full_join(hist_rost, emp_sup, by = c("case_id", "emp_id", "year"))

ehr_es <- left_join(ehr_es, demographics, by = c("case_id", "year")) %>% 
  # Find age:age^4 using job_year (or year if that not avaiable) and birth_year
  # Divide higher powers by 10 to keep smaller
  mutate(
    age = ifelse(!is.na(job_year), job_year - birth_year, year - birth_year),
    age_2 = (age / 10) ^ 2,
    age_3 = (age / 10) ^ 3,
    age_4 = (age / 10) ^ 4
  )

ehr_es <- left_join(ehr_es, weights, by = c("case_id")) 

###########################################################################
# Matching

# Try to match hist_rost to on_jobs. There are many ways to do this, and 
# some are more believable than others. Always match based on case_id and year

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
#    7. Many jobs are the only observation within it"s year on both ends, so assume
#       these are matches. Only potential issue is that I drop some jobs that I don"t
#       have any info on, so some jobs might not truely be the only type. 
#    8. Some years have the same job information for all jobs. Downside of this is
#       we can"t bring in date start/end job or rank.
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

on_jobs_temp <- on_jobs %>% 
  filter(!is.na(month_start_job), !is.na(month_end_job))

ehr_es_temp_1 <- ehr_es %>% 
  inner_join(on_jobs_temp, 
             by = c("case_id", "year", "job" = "rank", "month_start_job", "month_end_job"),
             suffix = c("_ehr", "_oj")) %>% 
  mutate(match_quality = 1) %>% 
  rename(rank = job,
         month_start_job_oj = month_start_job,
         month_end_job_oj = month_end_job) %>% 
  select(case_id, emp_id, year, rank, month_start_job_oj, month_end_job_oj,
         indep_con:ever_out, match_quality) %>%
  unique()

ehr_es_temp_2 <- ehr_es %>% 
  anti_join(ehr_es_temp_1, by = c("case_id", "year", "emp_id")) %>%
  inner_join(on_jobs_temp, 
             by = c("case_id", "year", "month_start_job", "month_end_job"),
             suffix = c("_ehr", "_oj")) %>% 
  mutate(match_quality = 2) %>% 
  rename(month_start_job_oj = month_start_job,
         month_end_job_oj = month_end_job) %>%  
  select(case_id, emp_id, year, rank, month_start_job_oj, month_end_job_oj,
         indep_con:ever_out, match_quality) %>%
  unique()

on_jobs_temp <- on_jobs %>% 
  filter(!is.na(month_start_job))

ehr_es_temp_3 <- ehr_es %>% 
  anti_join(ehr_es_temp_1, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_2, by = c("case_id", "year", "emp_id")) %>%
  inner_join(on_jobs_temp, 
             by = c("case_id", "year", "job" = "rank", "month_start_job"),
             suffix = c("_ehr", "_oj")) %>% 
  mutate(match_quality = 3) %>% 
  rename(rank = job,
         month_start_job_oj = month_start_job) %>% 
  select(case_id, emp_id, year, rank, month_start_job_oj, month_end_job_oj,
         indep_con:ever_out, match_quality) %>%
  unique()

on_jobs_temp <- on_jobs %>% 
  filter(!is.na(month_end_job))

ehr_es_temp_4 <- ehr_es %>% 
  anti_join(ehr_es_temp_1, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_2, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_3, by = c("case_id", "year", "emp_id")) %>%
  inner_join(on_jobs_temp, 
             by = c("case_id", "year", "job" = "rank", "month_end_job"),
             suffix = c("_ehr", "_oj")) %>% 
  mutate(match_quality = 4) %>% 
  rename(rank = job,
         month_end_job_oj = month_end_job) %>% 
  select(case_id, emp_id, year, rank, month_start_job_oj, month_end_job_oj,
         indep_con:ever_out, match_quality) %>%
  unique()

on_jobs_temp <- on_jobs %>% 
  filter(!is.na(month_start_job))

ehr_es_temp_5 <- ehr_es %>% 
  anti_join(ehr_es_temp_1, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_2, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_3, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_4, by = c("case_id", "year", "emp_id")) %>%
  inner_join(on_jobs_temp, 
             by = c("case_id", "year", "month_start_job"),
             suffix = c("_ehr", "_oj")) %>% 
  mutate(match_quality = 5) %>% 
  rename(month_start_job_oj = month_start_job) %>% 
  select(case_id, emp_id, year, rank, month_start_job_oj, month_end_job_oj,
         indep_con:ever_out, match_quality) %>%
  unique()

on_jobs_temp <- on_jobs %>% 
  filter(!is.na(month_end_job))

ehr_es_temp_6 <- ehr_es %>% 
  anti_join(ehr_es_temp_1, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_2, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_3, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_4, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_5, by = c("case_id", "year", "emp_id")) %>%
  inner_join(on_jobs_temp, 
             by = c("case_id", "year", "month_end_job"),
             suffix = c("_ehr", "_oj")) %>% 
  mutate(match_quality = 6) %>% 
  rename(month_end_job_oj = month_end_job) %>% 
  select(case_id, emp_id, year, rank, month_start_job_oj, month_end_job_oj,
         indep_con:ever_out, match_quality) %>%
  unique()

on_jobs_temp <- on_jobs %>% 
  group_by(case_id, year) %>% 
  filter(n() == 1) %>% 
  ungroup() 

ehr_es_temp_7 <- ehr_es %>% 
  anti_join(ehr_es_temp_1, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_2, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_3, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_4, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_5, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_6, by = c("case_id", "year", "emp_id")) %>%
  group_by(case_id, year) %>% 
  filter(n() == 1) %>% 
  ungroup() %>% 
  inner_join(on_jobs_temp, by = c("case_id", "year"),
             suffix = c("_ehr", "_oj")) %>% 
  mutate(match_quality = 7) %>% 
  select(case_id, emp_id, year, month_start_job_oj, month_end_job_oj,
         indep_con:ever_out, match_quality) %>%
  unique() 

on_jobs_temp <- on_jobs %>% 
  select(-current_job, -rank, -month_end_job, -month_start_job) %>% 
  unique() %>% 
  group_by(case_id, year) %>% 
  filter(n() == 1) %>% 
  ungroup()

ehr_es_temp_8 <- ehr_es %>% 
  anti_join(ehr_es_temp_1, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_2, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_3, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_4, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_5, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_6, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_7, by = c("case_id", "year", "emp_id")) %>%
  group_by(case_id, year) %>% 
  filter(n() == 1) %>% 
  ungroup() %>% 
  inner_join(on_jobs_temp, by = c("case_id", "year"),
             suffix = c("_ehr", "_oj")) %>% 
  mutate(match_quality = 8) %>% 
  select(case_id, emp_id, year, indep_con:ever_out, match_quality) %>%
  unique() 

ehr_es_temp_9 <- ehr_es %>% 
  anti_join(ehr_es_temp_1, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_2, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_3, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_4, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_5, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_6, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_7, by = c("case_id", "year", "emp_id")) %>%
  anti_join(ehr_es_temp_8, by = c("case_id", "year", "emp_id")) %>%
  group_by(case_id, year) %>% 
  filter(n() == 1) %>% 
  ungroup() %>% 
  inner_join(on_jobs, by = c("case_id", "year", "job" = "rank"),
             suffix = c("_ehr", "_oj")) %>% 
  mutate(match_quality = 9,
         rank = job) %>% 
  select(case_id, emp_id, year, rank, indep_con:ever_out, match_quality) %>%
  unique() 

ehr_es_temp <- bind_rows(ehr_es_temp_1, ehr_es_temp_2, ehr_es_temp_3,
                        ehr_es_temp_4, ehr_es_temp_5, ehr_es_temp_6,
                        ehr_es_temp_7, ehr_es_temp_8, ehr_es_temp_9) %>% 
  group_by(case_id, emp_id, year) %>% 
  filter(match_quality == min(match_quality)) %>%
  filter(n() == 1) %>% 
  ungroup()

matched <- 
  left_join(ehr_es, ehr_es_temp, by = c("case_id", "emp_id", "year"),
            suffix = c("_ehr", "_oj")) %>% 
  # Fill in missing data by case_id, emp_id
  group_by(case_id, emp_id) %>% 
  mutate(
    indep_con = fill_NA_mean(indep_con),
    looped = 1 * !is.na(looped),
    on_call = fill_NA_mean(on_call),
    outsourced = fill_NA_mean(outsourced),
    self_emp = fill_NA_mean(self_emp),
    temp_work = fill_NA_mean(temp_work),
    traditional = fill_NA_mean(traditional),
    ever_out = fill_NA_mean(ever_out),
    match_quality = fill_NA_mean(match_quality),
    
    # Flag matches where month_start and month_end don"t line up
    match_flag_1 = (((month_start_job - month_start_job_oj != 0) %in% T) 
                  | ((month_end_job - month_end_job_oj != 0) %in% T)),
    
    # Flag matches where indep_con:traditional are not 0/1
    # Or when job type not consistent for entire job
    match_flag_2 = !((mean(indep_con) %in% c(0, 1))
                    & (mean(on_call) %in% c(0, 1)) 
                    & (mean(outsourced) %in% c(0, 1))
                    & (mean(self_emp) %in% c(0, 1))
                    & (mean(temp_work) %in% c(0, 1))
                    & (mean(traditional) %in% c(0, 1))),
    
    # If match qualities disagree, set equal to lowest where looped == 1
    match_quality = 
      ifelse(!is.na(match_quality),
             min(match_quality[which(looped == 1)], na.rm = T), NA),
    match_quality = ifelse(match_quality < 0, NA, match_quality)
  ) %>% 
  ungroup() %>%
  # Check if everyone has 1 type and how many outsourced jobs matched
  mutate(n_types = indep_con + on_call + outsourced + self_emp + temp_work
  + traditional,
  y_min_j_y = year - job_year) %>%
  group_by(case_id) %>%
  mutate(ever_out_2 = ifelse(all(is.na(outsourced)), 0, max(outsourced, na.rm = T)),
         e_o_d = ever_out - ever_out_2) %>%
  group_by(year, emp_id, add = T) %>%
  mutate(count = n()) %>% 
  ungroup() %>% 
  # Some union_ set as NA, set to -1
  mutate(union_ = ifelse(!is.na(union_), union_, -1))

#######################################################################

# How good is the match? Create some tables showing match quality from
# both sides (OJ and EHR/ES)

# Total jobs in each survey
ehr_es_obs <- NROW(ehr_es)
ehr_obs <- NROW(hist_rost)
es_obs <- NROW(emp_sup)
oj_obs <- NROW(on_jobs)


# What is overlap/exclusion between hist_rost and emp_sup
only_ehr_obs <- NROW(
  anti_join(hist_rost, emp_sup, by = c("case_id", "year", "emp_id")))

only_es_obs <- NROW(
  anti_join(emp_sup, hist_rost, by = c("case_id", "year", "emp_id")))

both_es_ehr_obs <- ehr_obs - only_ehr_obs

# Jobs in ehr/es not matched with oj
unmatched_obs <- sum(is.na(matched$match_quality))

matched <- filter(matched, !is.na(match_quality))

# Jobs dropped from match because start/end dates don"t align
flag_1_obs <- sum(matched$match_flag_1)

matched <- filter(matched, match_flag_1 == F)

# Jobs dropped from match because no or conflicting job
# type info
flag_2_obs <- sum(matched$match_flag_2)

matched <- filter(matched, match_flag_2 == F)

# Define ever_out if report outsourced in oj. How would
# this be different if used es/ehr
ever_out_count <- matched %>% 
  group_by(case_id) %>% 
  filter(row_number() == min(row_number())) %>%  
  group_by(ever_out, ever_out_2) %>%
  count()

# Create a match_quality table
m_q_table <- table(matched$match_quality)
looped_m_q_table <- table(matched$match_quality[matched$looped == 1])
outsourced_m_q_table <- table(matched$match_quality[matched$outsourced == 1])
  
# Look at oj missed
on_jobs_miss <- on_jobs %>%
  anti_join(matched, by = c("case_id", "year", "rank"))

# Total jobs matched
m_obs <- NROW(matched)
m_outsourced <- sum(matched$outsourced)

# How many missed, how many with info missed, how many outsourcing missed
# Note that we should account for jobs matched using same type 
# within year
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

top <- "\\documentclass[12pt]{article}
\\usepackage[margin=.5in]{geometry}
\\usepackage{booktabs}
\\begin{document}
\\begin{table}
\\centering 
\\begin{tabular}{lr}
\\toprule
Subset & Number \\\\ \\midrule
"

for (i in seq(1, length(obs))){
  top <- str_c(top, labels[i], " & ", p_m[i],
                format(obs[i], big.mark = ","), "\\\\ ", end[i])
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

top_oj <- "\\documentclass[12pt]{article}
\\usepackage[margin=.5in]{geometry}
\\usepackage{booktabs}
\\begin{document}
\\begin{table}
\\centering 
\\begin{tabular}{lrrr}
\\toprule
Subset & Unmatched & Total & Percent Missing \\\\ \\midrule
"

top_oj <- str_c(
  top_oj,
  str_c("On Jobs", 
        format(oj_missed, big.mark = ","),
        format(oj_obs, big.mark = ","),
        round(oj_missed / oj_obs * 100, 0), sep = " & "),
  " \\\\ \n",
  str_c("On Jobs with Information",
        format(oj_info_missed, big.mark = ","),
        format(oj_info, big.mark = ","),
        round(oj_info_missed / oj_info * 100, 0), sep = " & "),
  " \\\\ \n",
  str_c("On Jobs Outsourced",
        format(oj_outsourced_missed, big.mark = ","),
        format(oj_outsourced, big.mark = ","),
        round(oj_outsourced_missed / oj_outsourced * 100, 0), sep = " & "),
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

top_q <- "\\documentclass[12pt]{article}
\\usepackage[margin=.5in]{geometry}
\\usepackage{booktabs}
\\begin{document}
\\begin{table}
\\centering 
\\begin{tabular}{lrr}
\\toprule
Match Quality & Overall & Outsourced  \\\\ \\midrule
"

for (i in seq(1, length(labels))){
  top_q <- str_c(top_q, labels[i],
                " & ", format(m_q_table[[i]], big.mark = ","),
                " & ", format(outsourced_m_q_table[[i]], big.mark = ","), "\\\\ \n")
}

top_q <- str_c(top_q, " \\midrule \n Total",
                " & ", format(m_obs, big.mark = ","),
                " & ", format(m_outsourced, big.mark = ","), "\\\\ \n")

bot_q <- "\\bottomrule
\\end{tabular}
\\caption{Match quality of final dataset.}
\\label{match_quality}
\\end{table}
\\end{document}"

write.table(str_c(top_q, bot_q),
            str_c(table_folder, "NLSY79 Match Quality/Match Quality.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# Create matched_jobs which groups data by jobs
matched_jobs <- matched %>% 
  group_by(case_id, emp_id) %>% 
  summarise(
    hours_week = mean(hours_week, na.rm = T),
    ind_m = Mode(ind, na.rm = T)[1],
    occ_m = Mode(occ, na.rm = T)[1],
    # If ind/occ still NA, randomly take min
    ind = ifelse(!is.na(ind_m), ind_m, 
                 ifelse(min(ind, na.rm = T) < 1e10, min(ind, na.rm = T), NA)),
    occ = ifelse(!is.na(occ_m), occ_m, 
                 ifelse(min(occ, na.rm = T) < 1e10, min(occ, na.rm = T), NA)),
    week_start_job = min(week_start_job, na.rm = T),
    week_end_job = min(week_end_job, na.rm = T),
    tenure = mean(tenure, na.rm = T),
    union = mean(union, na.rm = T),
    # Round union_ for now
    union_ = round(mean(union_, na.rm = T)),
    log_real_hrly_wage = mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage = mean(log_real_wkly_wage, na.rm = T),
    part_time = mean(part_time, na.rm = T),
    tenure_2 = mean(tenure_2, na.rm = T),
    tenure_3 = mean(tenure_3, na.rm = T),
    tenure_4 = mean(tenure_4, na.rm = T),
    occ_cat = Mode(occ_cat, na.rm = T)[1],
    ind_cat = Mode(ind_cat, na.rm = T)[1],
    occ_ind = Mode(occ_ind, na.rm = T)[1],
    previous_job = ifelse(max(previous_job, na.rm = T) >= 0,
                          max(previous_job, na.rm = T), NA),
    next_job = ifelse(max(next_job, na.rm = T) >= 0,
                      max(next_job, na.rm = T), NA),
    weeks_between_jobs = ifelse(max(weeks_between_jobs, na.rm = T) >= -200,
                                max(weeks_between_jobs, na.rm = T), NA),
    childcare = mean(childcare, na.rm = T),
    dental = mean(dental, na.rm = T),
    flex_sched = mean(flex_sched, na.rm = T),
    health = mean(health, na.rm = T),
    job_sat = mean(job_sat, na.rm = T),
    life = mean(life, na.rm = T),
    maternity = mean(maternity, na.rm = T),
    profit_share = mean(profit_share, na.rm = T),
    retirement = mean(retirement, na.rm = T),
    train_school = mean(train_school, na.rm = T),
    any_benefits = mean(any_benefits, na.rm = T),
    sample_id = mean(sample_id, na.rm = T),
    female = mean(female, na.rm = T),
    black = mean(black, na.rm = T),
    hispanic = mean(hispanic, na.rm = T),
    hh_child = mean(hh_child, na.rm = T),
    marital_status = Mode(marital_status, na.rm = T)[1],
    msa = Mode(msa, na.rm = T)[1], 
    region = Mode(region, na.rm = T)[1],
    tot_child = mean(tot_child, na.rm = T),
    wks_work_prev = mean(wks_work_prev, na.rm = T),
    less_hs = mean(less_hs, na.rm = T),
    hs = mean(hs, na.rm = T),
    aa = mean(aa, na.rm = T),
    ba = mean(ba, na.rm = T),
    plus_ba = mean(plus_ba, na.rm = T),
    age = mean(age, na.rm = T),
    age_2 = mean(age_2, na.rm = T),
    age_3 = mean(age_3, na.rm = T),
    age_4 = mean(age_4, na.rm = T),
    weight = mean(weight, na.rm = T),
    indep_con = mean(indep_con, na.rm = T),
    on_call = mean(on_call, na.rm = T),
    outsourced = mean(outsourced, na.rm = T),
    self_emp = mean(self_emp, na.rm = T),
    temp_work = mean(temp_work, na.rm = T),
    traditional = mean(traditional, na.rm = T),
    ever_out = mean(ever_out, na.rm = T),
    single = mean(marital_status == 1, na.rm = T),
    married = mean(marital_status == 2, na.rm = T)
  ) %>% 
  # Drop uneeded variables
  select(-ind_m, -occ_m) %>% 
  ungroup()

# Save both data sets
# Drop uneeded variables
matched <- matched %>% 
  select(-match_flag_1, -match_flag_2, -month_start_job_oj, -month_end_job_oj,
         -e_o_d, -count, -n_types, -y_min_j_y, -rank)

# Save the data
write_csv(matched, str_c(clean_folder, "matched_clean.csv"))

# Save the data
write_csv(matched_jobs, str_c(clean_folder, "matched_jobs_clean.csv"))

