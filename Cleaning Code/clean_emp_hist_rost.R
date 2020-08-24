# This file takes raw Employer History Supplement data from emp_hist_rost_raw
# (and some CPI Data from CPIAUSCL) and creates emp_hist_rost_clean
  
rm(list = ls())

# This is a larger dataset, so data.table is needed
library(magrittr)
library(DescTools)
library(data.table)
library(lubridate)
library(tidyverse)

# Folders of interest
raw_folder <- "../Raw Data/"
clean_folder <- "../Cleaned Data/"

new_data <- fread(str_c(raw_folder, "emp_hist_rost_raw.csv"))

# Handle missing values

new_data[new_data == -1] = NA  # Refused 
new_data[new_data == -2] = NA  # Dont know 
new_data[new_data == -3] = NA  # Invalid missing 
new_data[new_data == -4] = NA  # Valid missing 
new_data[new_data == -5] = NA  # Non-interview 

# Create a function that, given a variable name and data set, returns T
# if variable is in names, F else
check <- function(var, data){
  var %in% colnames(data)
}

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

# Create a function that takes the NLSY's week data and rounds it
# To the nearest date. Use the fact that the first week of
# 2001 is 1201
base_week <- 1201
base_date <- round_date(ymd("2001-01-01"), "week")

round_week <- function(week){
  return(base_date + weeks(week - base_week))
}

# Rename Variables --------------------------------------------------------

# All questions start with this phrase
q <- "EMPLOYERS_ALL_"

# Month and Year
m <- "~M"
y <- "~Y"
my <- c(m, y)
my_2 <- c("M_", "Y_")

date_vars <- c("STOPDATE_", "STADATE_")

# Now UID"s defined at job level, also define them at year level
# Rename month and year So it can be made long
for (job_n in 1:65){
  
  job <- sprintf("%02d", job_n)
  
  for (year in seq(2002, 2016, by = 2)){
    
    # UID
    uid_old <- str_c(q, "UID.", job)
    uid_new <- str_c(q, "UID_", year, ".", job)
    if (check(uid_old, new_data)) {
      new_data[[uid_new]] <- new_data[[uid_old]]
    }
    
    for (date_n in 1:2) {
      for (var in date_vars) {
        my_old <- my[date_n]
        my_new <- my_2[date_n]
        date_old <- str_c(q, var, year, ".", job, my_old)
        date_new <- str_c(q, var, my_new, year, ".", job)
        if (check(date_old, new_data)) {
          new_data %<>% 
            rename(
              !!date_new := !!date_old
            )
        }
      }
    }
  }
}

# In 2002, OCC and IND Codes did not have 0 on end, in all other years, 
# they do. Change this
io_vars <- c("IND_", "OCC_")
  
for (job_n in 1:53){
  job <- sprintf("%02d", job_n)
  
  for (var in io_vars) {
    name <- str_c(q, var, "2002.", job)
    if (check(name, new_data)) {
      new_data[[name]] <-  new_data[[name]] * 10
    }
  }
}

# Reshape and Clean Data --------------------------------------------------

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
  extract(key, into=c("variable", "key"), 
          regex="(\\D+)(_20[:digit:]{2}.[:digit:]{2})") %>%
  filter(!is.na(variable), !is.na(key)) %>%
  spread(key=variable, value=val) %>%
  mutate(key = str_sub(key, 2, -1)) %>% 
  separate(key, into=c("int_year", "job"), convert = T) %>% 
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
  # Keep only observations with at least one job specific bit of data
  filter_at(vars(-case_id, -job, -int_year, -emp_id), any_vars(!is.na(.))) %>% 
  # Some variables have NA emp_id, drop them
  filter(!is.na(emp_id)) %>% 
  # Create month start/stop_job based on month and year
  # Use round_week function to fill in week start/stop job
  mutate(
    month_start_job = make_date(EMPLOYERS_ALL_STADATE_Y, EMPLOYERS_ALL_STADATE_M, 1),
    month_end_job = make_date(EMPLOYERS_ALL_STOPDATE_Y, EMPLOYERS_ALL_STOPDATE_M, 1),
    week_start_job = round_week(EMPLOYERS_ALL_STARTWEEK),
    week_end_job = round_week(EMPLOYERS_ALL_STOPWEEK)
  ) %>% 
  # In hist_roster, jobs are reported retrospectively. To find real wages, I plan
  # on using CPI from year last reported working that job that survey (job_year). 
  # (If missing, use interview year).
  
  # If missing week_start/end_job, replace with 
  # 1. month_start_end_job
  # 2. Other week
  # 3. Other month
  # 4. First day of job_year
  mutate(
    # Create job_year
    job_year = ifelse(!is.na(month_end_job), year(month_end_job), int_year),
    # If job year before 1979, change to int_year (to match with cpi)
    job_year = ifelse(job_year < 1979, int_year, job_year),
    # Replace missing week_start/end_job
    week_start_job = as_date(ifelse(
      !is.na(week_start_job), week_start_job,
      ifelse(!is.na(month_start_job), month_start_job,
             ifelse(!is.na(week_end_job), week_end_job,
                    ifelse(!is.na(month_end_job), month_end_job,
                    make_date(int_year, 1, 1)
                    )
                    )
             )
      )),
    week_end_job = as_date(ifelse(
      !is.na(week_end_job), week_end_job,
      ifelse(!is.na(month_end_job), month_end_job,
             ifelse(!is.na(week_start_job), week_start_job,
                    ifelse(!is.na(month_start_job), month_start_job,
                    make_date(int_year, 1, 1))
             )
      )
    ))
    ) %>% 
  # Drop remaining EMPLOYER_ALL
  select(-starts_with("EMPLOYERS_ALL"))

# Change Date to year, report cpi of each year 
# Get CPI from raw_folder
cpi <- read_csv(str_c(raw_folder, "CPIAUCSL.csv"),
                col_types = cols(
                  DATE = col_date(format = ""),
                  CPIAUCSL = col_double()
                ))

cpi <- cpi %>% 
  rename(year = DATE, cpi = CPIAUCSL) %>% 
  mutate(year = year(year))

# Use cpi from year 2016 as base
base_cpi <- cpi$cpi[cpi$year == 2016]

fill_mean <- c("hours_week", "log_real_hrly_wage", "log_real_hrly_wage",
               "union", "part_time")
fill_mode <- c("ind", "occ", "ind_cat", "occ_cat")

# Do some cleaning of the emp_history_roster combined
long %<>% 
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
    # Create an occ_cat of high level occupation categories
    # see https://usa.ipums.org/usa/volii/occ2000.shtml
    # Create an ind_cat of high level industry categories
    # see https://usa.ipums.org/usa/volii/ind2000.shtml
    occ_cat = 1 * (occ > 0) + 1 * (occ >= 3600) + 1 * (occ >= 4700) + 1 * (occ >= 6000) +
      1 * (occ >= 6200) + 1 * (occ >= 7700) + 1 * (occ >= 9800),
    ind_cat = 1 * (ind > 0) + 1 * (ind >= 370) + 1 * (ind >= 690) + 1 * (ind >= 770) +
      1 * (ind >= 1070) + 1 * (ind >= 4000) + 1 * (ind >= 4600) + 1 * (ind >= 6000) +
      1 * (ind >= 6400) + 1 * (ind >= 6800) + 1 * (ind >= 7200) + 1 * (ind >= 7800) +
      1 * (ind >= 8500) + 1 * (ind >= 8700) + 1 * (ind >= 9300) + 1 * (ind >= 9600)
  ) %>% 
  # For missing combined, If other job years have combined, take averages (mode if category) 
  group_by(case_id, emp_id) %>% 
  mutate_at(fill_mean, fill_NA_mean) %>% 
  mutate_at(fill_mode, fill_NA_mode) %>% 
  mutate(
    # Some 2014 and 2016 ind and occs seem to be missing 0's at end
    # Try to fill these in (some over 1000 look okay, leave these alone)
    ind = ifelse((ind %% 10 > 0) & (ind < 1000), ind * 10, ind),
    occ = ifelse((occ %% 10 > 0) & (occ < 1000), occ * 10, occ),
    # A special cases I can see
    occ = ifelse(occ %in% c(260), occ * 10, occ),
    # Create union_fill which is -1 if union is NA (useful for regression)
    union_fill = ifelse(is.na(union), -1, round(union))
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
  )

# If respondent holds a job for multiple periods, it seems the job_history
#  reports that date_job_end one period = date_job_begin 
# next period, which is not what we want. (Because On Jobs has this missing)
# 1. Group by case_id and emp_id
# 2. Rank by year. Only lowest ranked year keeps start month, 
# only highest rank year keeps end month
long %<>% 
  group_by(case_id, emp_id) %>% 
  mutate(
    year_rank = min_rank(int_year),
    month_start_job = as_date(ifelse(year_rank == 1, month_start_job, NA)),
    month_end_job = as_date(ifelse(year_rank == max(year_rank), month_end_job, NA))
  ) %>% 
  # Drop uneeded variables
  select(-job, -job_year, -hrly_wage, -wkly_wage, -cpi, -work_drop, -year_rank)

# # Data from 2014 and 2016 interviews in on jobs are less reliable. Drop these years
# long %<>% filter(int_year < 2014) 

# Save the data
fwrite(long, str_c(clean_folder, "emp_hist_rost_clean.csv"), row.names = FALSE)