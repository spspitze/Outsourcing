# This file contains IPUMS CPS data from cps_raw_codebook and cps_00003.dat.gz
# This is monthly data from 2001-2016 for all men. 
# Focus on occupation prevalance by outsourcing prevalance 
# (according to NLSY 79 data in occ_timeline).
# To compare cps occ1990 to nlsy (which uses 2000 census codes from 2001-2016),
# use the occ1990 sheet of integrated_ind_occ_crosswalks

rm(list = ls())

library(readxl)
library(srvyr)
library(magrittr)
library(DescTools)
library(lubridate)
library(ipumsr)
library(tidyverse)

raw_folder <- "../Raw Data/"
clean_folder <- "../Cleaned Data/"
table_folder <- "../Tables/"
figure_folder <- "../Figures/CPS/"

# Read the data using cps's custom formulas
ddi <- read_ipums_ddi(str_c(raw_folder, "cps_raw_codebook.xml"))
cps <- read_ipums_micro(ddi)

rm("ddi")

# Clean CPS data ----------------------------------------------------------

# Clean cps data, meging with cpi data and a crosswalk for 2000 occ codes
# Summarize each month by occupation

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

# Start by keeping only people over 18, marking clear NA's, and clarifying
# What variables mean
names(cps) %<>% tolower
cps %<>%
  filter(age >= 18, year <= 2016) %>% 
  # Merge with cpi data for real wages 
  # (note, different attributes cause warning)
  left_join(cpi, by = "year") %>% 
  mutate(
    # Call black if black or mixed race
    black = 1 * (race %in% c(200, 801, 805, 806, 807, 810, 811, 814, 816)),
    # Call hispanic if mentioned being hispanic
    hispanic = 1 * (hispan > 0 & hispan < 900),
    # Define as married if marst is 1 or 2 and sinle if 6
    married = 1 * (marst %in% c(1, 2)),
    single = 1 * (marst == 6),
    # Define working if empstat is 1, 10, or 12 and unemployed if 21 or 22
    working = 1 * (empstat %in% c(1, 10, 12)),
    unemployed = 1 * (empstat %in% c(21, 22)),
    # Define part_time if wkstat is 12, 20-41
    part_time = 1 * (wkstat == 12 | (wkstat >= 20 & wkstat <= 22)),
    # For education groups
    less_hs = 1 * (educ <= 72),
    hs = 1 * (educ %in% c(73, 81)),
    aa = 1 * (educ %in% c(91, 92)),
    ba = 1 * (educ == 111),
    plus_ba = 1 * (educ > 111),
    # For union
    union = 1 * (union == 2),
    # For occ1990 and ind1990, replace various NA's
    occ_1990 = ifelse(occ1990 < 999, occ1990, NA),
    ind_1990 = ifelse(ind1990 > 0, ind1990, NA),
    # Find log_real_wkly/hrly_wage, drop NA's
    log_real_hrly_wage = 
      ifelse(hourwage != 999.99, log(hourwage / cpi * base_cpi), NA),
    log_real_wkly_wage = 
      ifelse(earnweek != 9999.99, log(earnweek / cpi * base_cpi), NA)
  ) %>% 
  # Keep only variables needed
  select(year, month, wtfinl, cpsidp, age, earnwt,
         union, black:log_real_wkly_wage)

# Need every bit of space I can get
rm("cpi", "base_cpi")

# Read the crosswalk between occ_1990 and 2000
# Keep only needed variables, convert to numeric, and drop NA
crosswalk <- read_excel(str_c(raw_folder, "integrated_ind_occ_crosswalks.xlsx"),
                        sheet = "occ1990", na = "#") %>% 
  select(occ_1990 = OCC1990, occ_2000 = `2000`) %>% 
  mutate(
    occ_1990 = as.numeric(occ_1990),
    occ_2000 = as.numeric(occ_2000)
  ) %>% 
  filter(!is.na(occ_1990), !is.na(occ_2000))

# Merge with cps. Keep only occ_2000 as occ and muliply by 10 to make consistent
# with NLSY
cps %<>% 
  left_join(crosswalk, by = "occ_1990")  %>% 
  mutate(occ = occ_2000 * 10) %>% 
  select(-occ_1990, -occ_2000)

# Need every bit of space I can get
rm("crosswalk")

# Two different weights, so need to run weighted.mean separately
# Create function weighted_mean to run wieghted.mean on
# variables as matricies (seems to be needed)
weighted_mean <- function(x, w, cond = T, na.rm = F) {
  weighted.mean(x = as.matrix(x[cond]), w = as.matrix(w[cond]), na.rm = na.rm)
}

# Group cps by year, month, occ and summarise variables of interest.
# Look only at working. 
# Weights are either earnwt or wtfinal
occ_cps <- cps %>% 
  filter(working == 1, !is.na(occ)) %>% 
  group_by(year, month, occ) %>% 
  summarise(
    n = n(),
    log_real_wkly_wage = 
      weighted_mean(log_real_wkly_wage, earnwt, cond = !is.na(log_real_wkly_wage)),
    union = weighted_mean(union, earnwt),
    part_time = weighted_mean(part_time, wtfinl),
    black = weighted_mean(black, wtfinl),
    hispanic = weighted_mean(hispanic, wtfinl),
    age = weighted_mean(age, wtfinl),
  )


# Get cleaned occ_timeline, which will be my measure of outsourcing over time
occ_timeline <- read_csv(str_c(clean_folder, "occ_timeline.csv"),
                         col_types = cols(
                           .default = col_double(),
                           week = col_date(format = "")
                         ))
