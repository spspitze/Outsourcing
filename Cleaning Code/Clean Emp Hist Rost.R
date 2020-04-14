# This file takes raw Employer History Supplement data from emp_hist_rost_raw
# (and some CPI Data from CPIAUSCL) and creates emp_hist_rost_clean

rm(list = ls())

# This is a larger dataset, so data.table is needed
library(DescTools)
library(data.table)
library(lubridate)
library(tidyverse)

# Folders of interest
raw_folder <- "Raw Data/"
clean_folder <- "Cleaned Data/"

new_data <- fread(str_c(raw_folder, "emp_hist_rost_raw.csv"))

# Handle missing values

new_data[new_data == -1] = NA  # Refused 
new_data[new_data == -2] = NA  # Dont know 
new_data[new_data == -3] = NA  # Invalid missing 
new_data[new_data == -4] = NA  # Valid missing 
new_data[new_data == -5] = NA  # Non-interview 

# Get CPI from raw_folder
cpi <- read_csv(str_c(raw_folder, "CPIAUCSL.csv"))

# Create a function that, given a variable name and data set, returns T
# if variable is in names, F else
check <- function(var, data){
  return(var %in% colnames(data))
}

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

# Create a function that takes the NLSY's week data and rounds it
# To the nearest date. Use the fact that the first week of
# 2001 is 1201
base_week <- 1201
base_date <- round_date(ymd("2001-01-01"), "week")

round_week <- function(week){
  return(base_date + weeks(week - base_week))
}

#########################################################################

# All questions start with this phrase
q <- "EMPLOYERS_ALL_"

# Month and Year
m <- "~M"
y <- "~Y"

date_vars <- c("STOPDATE_", "STADATE_")

# Now UID"s defined at job level, also define them at year level
# Rename month and year So it can be made long
for (year in seq(2002, 2016, by = 2)){
  # Becasue of the 0 in fron of 1-9, do this separately
  for (job in seq(1, 9)){
    
    if (check(str_c(q, "UID.0", job), new_data)){
      new_data[[str_c(q, "UID_", year, ".0", job)]] <- 
        new_data[[str_c(q, "UID.0", job)]]
    }
      
    new_data[[str_c(q, "STOPDATE_M_", year, ".0", job)]] <- 
      new_data[[str_c(q, "STOPDATE_", year, ".0", job, m)]]
    
    new_data[[str_c(q, "STOPDATE_Y_", year, ".0", job)]] <- 
      new_data[[str_c(q, "STOPDATE_", year, ".0", job, y)]]
    
    new_data[[str_c(q, "STADATE_M_", year, ".0", job)]] <- 
      new_data[[str_c(q, "STADATE_", year, ".0", job, m)]]
    
    new_data[[str_c(q, "STADATE_Y_", year, ".0", job)]] <- 
      new_data[[str_c(q, "STADATE_", year, ".0", job, y)]]
    }
  
  for (job in seq(10, 65)){
    
    if (check(str_c(q, "UID.", job), new_data)){
      new_data[[str_c(q, "UID_", year, ".", job)]] <- 
        new_data[[str_c(q, "UID.", job)]]
    }
    
    new_data[[str_c(q, "STOPDATE_M_", year, ".", job)]] <- 
      new_data[[str_c(q, "STOPDATE_", year, ".", job, m)]]
    
    new_data[[str_c(q, "STOPDATE_Y_", year, ".", job)]] <- 
      new_data[[str_c(q, "STOPDATE_", year, ".", job, y)]]
    
    new_data[[str_c(q, "STADATE_M_", year, ".", job)]] <- 
      new_data[[str_c(q, "STADATE_", year, ".", job, m)]]
    
    new_data[[str_c(q, "STADATE_Y_", year, ".", job)]] <- 
      new_data[[str_c(q, "STADATE_", year, ".", job, y)]]
    }
}

# In 2002, OCC and IND Codes did not have 0 on end, in all other years, 
# they do. Change this
for (job in seq(1, 9)){
  new_data[[str_c(q, "IND_2002.0", job)]] <- 
    10 * new_data[[str_c(q, "IND_2002.0", job)]]
  
  new_data[[str_c(q, "OCC_2002.0", job)]] <- 
    10 * new_data[[str_c(q, "OCC_2002.0", job)]]
}

for (job in seq(10, 53)){
  if (check(str_c(q, "IND_2002.", job), new_data)){
    new_data[[str_c(q, "IND_2002.", job)]] <- 
      10 * new_data[[str_c(q, "IND_2002.", job)]]
  }
  
  
  if (check(str_c(q, "OCC_2002.", job), new_data)){
    new_data[[str_c(q, "OCC_2002.", job)]] <- 
      10 * new_data[[str_c(q, "OCC_2002.", job)]]
  }
}


# Transform data from wide to long
vary <- c("UID_", "STOPDATE_", "STADATE_", "STOPWEEK_", "STARTWEEK_",
          "TENURE_", "HOURSWEEK_", "IND_", "OCC_", "HRLY_WAGE_", "UNION_")

constant <- c("CASEID")

# Variables we keep
keep <- str_c("^(", str_c(str_c(q, vary), collapse = "|"), "|", constant, ")")

# Data set is large, so going wide to long may take a minute or two
long <- new_data %>% 
  dplyr::select(matches(keep), -ends_with(m), -ends_with(y)) %>% 
  gather(matches(str_c("^(", str_c(str_c(q, vary), collapse="|"), ")")), 
         key=key, value=val) %>%
  extract(key, into=c("variable", "key"), regex="(\\D+)(_20[:digit:]{2}.[:digit:]{2})") %>%
  filter(!is.na(variable), !is.na(key)) %>%
  spread(key=variable, value=val) %>%
  mutate(key = str_sub(key, 2, -1)) %>% 
  separate(key, into=c("year", "job"), convert = T) %>% 
  # Rename variables 
  rename(
    case_id = CASEID,
    hours_week = EMPLOYERS_ALL_HOURSWEEK,
    hrly_wage = EMPLOYERS_ALL_HRLY_WAGE,
    ind = EMPLOYERS_ALL_IND,
    occ = EMPLOYERS_ALL_OCC,
    tenure = EMPLOYERS_ALL_TENURE,
    emp_id = EMPLOYERS_ALL_UID,
    union = EMPLOYERS_ALL_UNION
  ) %>% 
  # Create month start/stop_job based on month and year
  # Use round_week function to fill in week start/stop job
  mutate(
    month_start_job = make_date(EMPLOYERS_ALL_STADATE_Y, EMPLOYERS_ALL_STADATE_M, 1),
    month_end_job = make_date(EMPLOYERS_ALL_STOPDATE_Y, EMPLOYERS_ALL_STOPDATE_M, 1),
    week_start_job = round_week(EMPLOYERS_ALL_STARTWEEK),
    week_end_job = round_week(EMPLOYERS_ALL_STOPWEEK),
  ) %>% 
  # Keep only observations with at least one job specific bit of data
  filter_at(vars(-case_id, -job, -year, -emp_id), any_vars(!is.na(.))) %>% 
  # Some variables have NA emp_id somehow, drop them
  filter(!is.na(emp_id)) %>% 
  # Drop remaining EMPLOYER_ALL
  select(-starts_with("EMPLOYERS_ALL"))

# Change Date to year, report cpi of each year and cpi_a as average cpi of last 
# two years
cpi <- cpi %>% 
  rename(year = DATE, cpi = CPIAUCSL) %>% 
  mutate(year = as.numeric(substring(year, 1, 4)))

# For now, use cpi from year 2016 as base
base_cpi <- cpi$cpi[cpi$year == 2016]

# Do some cleaning of the emp_history_roster combined
long_clean <- long %>% 
  # In NLSY, jobs are reported retrospectively. To find real wages, I plan
  # on using CPI from year last reported working that job that survey.
  # Find job year, rememeber date = 12 * year + month, 
  # so (date - 1) %/% will give year
  # Create job_year and match CPI with it. 
  mutate(job_year = year(month_end_job),
         # Once person wrote that year is 1907, change this to 1979)
         job_year = ifelse(job_year < 1979, 1979, job_year)) %>% 
  # Join to cpi by year to get real wages
  left_join(cpi, by = c("job_year" = "year")) %>% 
  mutate(
    # Currently hrly_wage is * 100, fix this (and drop 0s)
    hrly_wage = ifelse(hrly_wage > 0, hrly_wage / 100, NA),
    # Create weekly wages by multiplying hours_week by hrly_wage (drop 0 hrs).
    # Make them real (in 2016 $"s) and take logs
    wkly_wage = ifelse(hours_week > 0, hours_week * hrly_wage, NA),
    log_real_hrly_wage = log(hrly_wage / cpi * base_cpi),
    log_real_wkly_wage = log(wkly_wage / cpi * base_cpi),
    # Create part time if hours_week < 35
    part_time = 1 * (hours_week < 35),
    # Generate tenure^2, ^3, and ^3 (divide by 100 to keep small)
    tenure_2 = (tenure / 100) ^ 2,
    tenure_3 = (tenure / 100) ^ 3,
    tenure_4 = (tenure / 100) ^ 4,
    # Create an occ_cat of high level occupation categories to cluster combined
    # see https://usa.ipums.org/usa/volii/occ2000.shtml
    # Create an ind_cat of high level industry categories to cluster combined
    # see https://usa.ipums.org/usa/volii/ind2000.shtml
    # Create occ_ind for occ_cat overlap with ind_cat to cluster combined
    occ_cat = 1 * (occ > 0) + 1 * (occ >= 3600) + 1 * (occ >= 4700) + 1 * (occ >= 6000) +
      1 * (occ >= 6200) + 1 * (occ >= 7700) + 1 * (occ >= 9800),
    ind_cat = 1 * (ind > 0) + 1 * (ind >= 370) + 1 * (ind >= 690) + 1 * (ind >= 770) +
      1 * (ind >= 1070) + 1 * (ind >= 4000) + 1 * (ind >= 4600) + 1 * (ind >= 6000) +
      1 * (ind >= 6400) + 1 * (ind >= 6800) + 1 * (ind >= 7200) + 1 * (ind >= 7800) +
      1 * (ind >= 8500) + 1 * (ind >= 8700) + 1 * (ind >= 9300) + 1 * (ind >= 9600), 
    occ_ind = occ_cat + ind_cat * 16
  ) %>% 
  # For missing combined, If other job years have combined, take averages (mode if category) 
  group_by(case_id, emp_id) %>% 
  mutate(
    hours_week = fill_NA_mean(hours_week),
    log_real_hrly_wage = fill_NA_mean(log_real_hrly_wage),
    log_real_wkly_wage = fill_NA_mean(log_real_wkly_wage),
    ind = fill_NA_mode(ind),
    occ = fill_NA_mode(occ),
    # Some 2014 and 2016 ind and occs seem to be missing 0's at end
    # Try to fill these in (some over 1000 look okay, leave these alone)
    ind = ifelse((ind %% 10 > 0) & (ind < 1000), ind * 10, ind),
    occ = ifelse((occ %% 10 > 0) & (occ < 1000), occ * 10, occ),
    # A special cases I can see
    occ = ifelse(occ %in% c(260), occ * 10, occ),
    ind_cat = fill_NA_mode(ind_cat),
    occ_cat = fill_NA_mode(occ_cat),
    occ_ind = fill_NA_mode(occ_ind),
    union = fill_NA_mean(union),
    # Create union_ which is -1 if union is NA
    union_ = ifelse(is.na(union), -1, round(union)),
    part_time = fill_NA_mode(part_time)
  ) %>% 
  ungroup() %>% 
  mutate(
    # If real wage below 3 or above 400 set wage and earn to NA
    # If hours_week is <=0 or >80, set to NA
    # (Do this after estimating missing combined)
    work_drop = (log_real_hrly_wage < log(3) | log_real_hrly_wage > log(400)
                 | hours_week > 80 | hours_week <= 0),
    log_real_hrly_wage = ifelse(work_drop, NA, log_real_hrly_wage),
    log_real_wkly_wage = ifelse(work_drop, NA, log_real_wkly_wage),
    hours_week = ifelse(work_drop, NA, hours_week)
  ) %>% 
  # If respondent holds a job for multiple periods, it seems the job_history
  # sometimes reports that date_job_end one period = date_job_begin 
  # next period, which is not what we want. 
  # 1. Sort by case_id, emp_id, year
  arrange(case_id, emp_id, year) %>% 
  # 2. Lag and lead case_id and emp_id, lag start and lead end
  # 3. If case_id = case_id_lag/lead; emp_id = emp_id_lag/lead
  # and end >= start_lead or start <= end_lag, set as NA
  mutate(
    case_id_lead = lead(case_id),
    case_id_lag = lag(case_id),
    emp_id_lead = lead(emp_id),
    emp_id_lag = lag(emp_id),
    month_start_job_lead = lead(month_start_job),
    month_end_job_lag = lag(month_end_job),
    same_job_lead = (case_id == case_id_lead) & (emp_id == emp_id_lead),
    same_job_lag = (case_id == case_id_lag) & (emp_id == emp_id_lag),
    dif_job_lag = (case_id == case_id_lag) & (emp_id != emp_id_lag),
    month_start_job = as_date(
      ifelse(same_job_lag & (month_start_job <= month_end_job_lag) %in% T,
                            NA, month_start_job)),
    month_end_job = as_date(
      ifelse(same_job_lead & (month_end_job >= month_start_job_lead) %in% T,
                          NA, month_end_job))
  ) %>% 
  # Drop uneeded variables
  select(-work_drop, -cpi, -hrly_wage, -wkly_wage, -case_id_lead,
         -case_id_lag, -emp_id_lead, -emp_id_lag,
         -month_start_job_lead, -month_end_job_lag, 
         -same_job_lead, -same_job_lag, -dif_job_lag, -job)

# Previous and next job can be tricky with overlapping jobs. Look for job transitions
# Merge this into rest of data.
long_transtitions <- long_clean %>% 
  arrange(case_id, emp_id, year) %>% 
  group_by(case_id, emp_id) %>% 
  fill(month_start_job, .direction = "downup") %>% 
  fill(month_end_job, .direction = "updown") %>% 
  group_by(case_id, emp_id, month_start_job, month_end_job) %>% 
  summarise(week_start_job = min(week_start_job),
            week_end_job = max(week_end_job)) %>% 
  group_by(case_id) %>% 
  mutate(start_rank = min_rank(week_start_job),
         end_rank = min_rank(week_end_job)) %>% 
  filter(start_rank <= end_rank) %>% 
  arrange(case_id, start_rank, end_rank) %>% 
  # Do it twice to get rid of some weird cases
  mutate(start_rank = min_rank(week_start_job),
         end_rank = min_rank(week_end_job)) %>%  
  filter(start_rank <= end_rank) %>% 
  arrange(case_id, start_rank, end_rank) %>% 
  # Do it thrice to get rid of some weird cases
  mutate(start_rank = min_rank(week_start_job),
         end_rank = min_rank(week_end_job)) %>%  
  filter(start_rank <= end_rank) %>% 
  arrange(case_id, start_rank, end_rank) %>% 
  mutate(week_end_job_lag = lag(week_end_job),
         previous_job = lag(emp_id),
         next_job = lead(emp_id),
         weeks_between_jobs = weeks(week_start_job - week_end_job_lag),
         # Set more than 6 months before or 2 years after to NA for 
         # length and previous/next job
         previous_job = ifelse(weeks_between_jobs < -52, NA, previous_job),
         next_job = ifelse(weeks_between_jobs > 104, NA, next_job),
         weeks_between_jobs = weeks(weeks_between_jobs)
         ) %>% 
  select(-week_end_job_lag, -start_rank, -end_rank)

# Merge transition data into rest of data
long_clean <- left_join(long_clean, long_transtitions)

# Save the data
fwrite(long_clean, str_c(clean_folder, "emp_hist_rost_clean.csv"), row.names = FALSE)