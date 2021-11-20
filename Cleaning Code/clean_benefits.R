# This file takes raw data from ecec_raw, ebs_raw, and some
# hand input data (see Clean EBS below)
# It cleans them and merges them together,
# then saves the data as benefits_clean

rm(list = ls())

library(outsourcing)
library(DescTools)
library(zeallot)
library(readxl)
library(tidyverse)

# Folders of interest
folders <- name_folders("NCS Benefits")
c(raw_folder, clean_folder, table_folder, figure_folder,
  d_table_folder, s_table_folder) %<-% folders 

# For saving graphs
c(height, width) %<-% fig_size()

# Download ECEC data
ecec <- read_excel(str_c(raw_folder, "ecec_raw.xlsx"),
                       range="A4:BA22")

# Download EBS data 
ebs <- read_excel(str_c(raw_folder, "ebs_raw.xlsx"),
                   sheet = "Data")

# Clean ECEC data ---------------------------------------------------------

# 1. Each series (marked by Series ID) gives an occupation group
# (ex Managers + Professionals) and an outcome (ex Wages + salaries)
# create columns for each of these and drop Series ID. 
# 2. Flip data based on this occupation outcome pair.
# 3. Get year from data
# 4. Combine by outcome, occupation, and year
ecec <- ecec |>
  mutate(
    outcome = case_when(
      str_sub(`Series ID`, 5, 6) == "02" ~ "wages",
      str_sub(`Series ID`, 5, 6) == "15" ~ "health",
      str_sub(`Series ID`, 5, 6) == "18" ~ "retirement"
      ),
    occupation = case_when(
      str_sub(`Series ID`, 11, 11) == "0" ~ "all",
      str_sub(`Series ID`, 11, 11) == "1" ~ "managers",
      str_sub(`Series ID`, 11, 11) == "2" ~ "office",
      str_sub(`Series ID`, 11, 11) == "3" ~ "service",
      str_sub(`Series ID`, 11, 11) == "4" ~ "construction",
      str_sub(`Series ID`, 11, 11) == "5" ~ "production"
    )
  ) |>
  select(-`Series ID`) |>
  pivot_longer(cols = -outcome:-occupation,
               names_to = "period",
               values_to = "value") |>
  mutate(
    year = as.numeric(str_sub(period, 6, 9))
  ) |>
  group_by(outcome, occupation, year) |>
  summarise(percent_compensation = mean(value))


# Clean EBS ---------------------------------------------------------------

# Data contains lots of variables, we only need a few of them
# 1. Make year and estimate numeric
# 2. To match with ECEC above, keep
#   a. Years before 2016 
#   b. Measures for all civilians
#   c. Industry code == 000000: All workers
#   d. Needed occupation codes (to match ecec)
#   e. Subcell code == 00: All workers
#   f. Datatype code == 28: Percent of workers with access
#   g. Needed Provision codes (health and/or retirement)
# 3. Make 3 needed variables
#   a. occupation names to match ecec
#   b. whether measure includes access to health or
#      retirement benefits (some variables allow both)
# 4. Select only needed variables

occupation_numbers <- c("000000", "112900", "313900", 
                        "414300", "454900", "515300")
provision_numbers <- c("007", "008", "009")

ebs <- ebs |>
  mutate(year = as.numeric(Year), 
         estimate = as.numeric(Estimate)) |>
  filter(year <= 2016, 
         `Ownership code` == "1", 
         `Industry code` == "000000",
         `Occupation code` %in% occupation_numbers,
         `Subcell code` == "00",
         `Datatype code` == "28",
         `Provision code` %in% provision_numbers) |>
  mutate(
    occupation = case_when(
      `Occupation code` == "000000" ~ "all",
      `Occupation code` == "112900" ~ "managers",
      `Occupation code` == "414300" ~ "office",
      `Occupation code` == "313900" ~ "service",
      `Occupation code` == "454900" ~ "construction",
      `Occupation code` == "515300" ~ "production"
    ),
    health = ifelse(`Provision code` %in% c("007", "008"), 1, 0),
    retirement = ifelse(`Provision code` %in% c("007", "009"), 1, 0),
  ) |>
  select(occupation, year, estimate, health, retirement)

# This dataset has a weird structure: it combines health insurance
# and retirement benefits together, report what percent of
# workers have access to both, one but not the other, and none
# To get around this, create two merger data sets, one for
# health and one for retirement.

ebs_health <- ebs |>
  # Keep only variables for health (should be 2 each group)
  filter(health == 1) |>
  # Group by occupation and year
  group_by(occupation, year) |>
  # Sum up total access
  summarise(access = sum(estimate)) |>
  # Create label to merge with ecec
  mutate(outcome = "health")

ebs_retirement <- ebs |>
  # Keep only variables for retirement (should be 2 each group)
  filter(retirement == 1) |>
  # Group by occupation and year
  group_by(occupation, year) |>
  # Sum up total access
  summarise(access = sum(estimate)) |>
  # Create label to merge with ecec
  mutate(outcome = "retirement")

# Stack these datasets to merge latter
ebs_stack <- bind_rows(ebs_health, ebs_retirement)

# Annual 2008-2009 data for civilians is
# in text tables online but not in excel dataset 
# (because annual not quarterly). Manually 
# enter these years. 
occupations <- c("all", "managers", "office",
                 "service", "construction", "production")
hand_occupations <- rep(occupations, times = 4)
hand_years <- rep(c(2008, 2009), each = 6 * 2)
hand_outcomes <- 
  rep(rep(c("health", "retirement"), each = 6), times = 2)
# Data retrieved from
# https://www.bls.gov/ncs/ebs/benefits/2008/ownership/civilian/table05a.htm
health_2008 <- c(74, 87, 73, 52, 78, 78)
# https://www.bls.gov/ncs/ebs/benefits/2008/ownership/civilian/table02a.htm
retirement_2008 <- c(66, 81, 67, 44, 65, 66)
# https://www.bls.gov/ncs/ebs/benefits/2009/ownership/civilian/table05a.htm
health_2009 <- c(74, 87, 73, 51, 78, 77)
# https://www.bls.gov/ncs/ebs/benefits/2009/ownership/civilian/table02a.htm
retirement_2009 <- c(71, 83, 73, 51, 70, 70)

hand_data <- c(health_2008, retirement_2008,
               health_2009, retirement_2009)

hand_data <- tibble(occupation = hand_occupations,
                    year = hand_years,
                    access = hand_data,
                    outcome = hand_outcomes)

# Stack ebs_stack and hand_data
ebs_stack <- bind_rows(ebs_stack, hand_data)

# Merge Datasets ----------------------------------------------------------

# 1. Merge in ebs datasets into the ecec. While ecec data runs
# from 2004-2016, ebs data only runs from 2010-2016. Below, 
# will see if this might matter
# 2. Pivot wider
# 3. Calculate benefits (adjusted for access) as a share of wages

benefits <- ecec |>
  left_join(ebs_stack) |>
  pivot_wider(id_cols = c("occupation", "year"),
              names_from = "outcome",
              values_from = c("percent_compensation", "access")
              ) |>
  mutate(
    health_share = 
      ((percent_compensation_health / (access_health / 100))
       / percent_compensation_wages),
    retirement_share = 
      ((percent_compensation_retirement / (access_retirement / 100))
       / percent_compensation_wages)
  ) 
  

# Plot Trends Over Time ---------------------------------------------------

# Plot trends in variables over time to see how plausibly
# the 2008-2016 data can be extended to 2004-2007.

# First, plot health_share and retirement_share by group
temp <- benefits |>
  filter(!is.na(health_share)) |>
  ggplot(aes(x = year, y = health_share, 
             color = factor(occupation))) +
  geom_line()

ggsave(str_c(figure_folder, "Health Share.pdf"),
       height = height, width = width)

temp <- benefits |>
  filter(!is.na(retirement_share)) |>
  ggplot(aes(x = year, y = retirement_share, 
             color = factor(occupation))) +
  geom_line()

ggsave(str_c(figure_folder, "Retirement Share.pdf"),
       height = height, width = width)

# How has access to benefits changed over sample?
temp <- benefits |>
  filter(!is.na(access_health)) |>
  ggplot(aes(x = year, y = access_health, 
             color = factor(occupation))) +
  geom_line()

ggsave(str_c(figure_folder, "Health Access.pdf"),
       height = height, width = width)

temp <- benefits |>
  filter(!is.na(access_retirement)) |>
  ggplot(aes(x = year, y = access_retirement, 
             color = factor(occupation))) +
  geom_line()

ggsave(str_c(figure_folder, "Retirement Access.pdf"),
       height = height, width = width)

# Compare to percent_compensations (which do not account for access)
temp <- benefits |>
  ggplot(aes(x = year, y = percent_compensation_health, 
             color = factor(occupation))) +
  geom_line() +
  geom_vline(xintercept = 2008)

ggsave(str_c(figure_folder, "Health Share (Not Access).pdf"),
       height = height, width = width)

temp <- benefits |>
  ggplot(aes(x = year, y = percent_compensation_retirement, 
             color = factor(occupation))) +
  geom_line() +
  geom_vline(xintercept = 2008)

ggsave(str_c(figure_folder, "Retirement Share (Not Access).pdf"),
       height = height, width = width)

# Prepare data for saving. Keep only needed variables,
# drop NA's
benefits <- benefits |>
  select(year, occupation, health_share, retirement_share) |>
  filter(!is.na(health_share), !is.na(retirement_share))

# Save the data
write_csv(benefits, str_c(clean_folder, "benefits_clean.csv"))
