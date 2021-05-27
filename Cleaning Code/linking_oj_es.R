# This file takes raw data from linking_raw, which contains the NLSY 79's official
# matches between On Jobs and Employer Supplement.
# For more info see https://www.nlsinfo.org/content/cohorts/nlsy79/other-documentation/codebook-supplement/nlsy79-appendix-9-linking-employers
# It cleans it then saves it as linking_clean
rm(list = ls())

library(outsourcing)
library(zeallot)
library(lubridate)
library(tidyverse)

# Folders of interest
folders <- name_folders()
c(raw_folder, clean_folder, table_folder, figure_folder,
  d_table_folder, s_table_folder) %<-% folders 

col_names <- c(
  "CASEID_1979",
  "NEWLINK.01_2002",
  "NEWLINK.02_2002",
  "NEWLINK.03_2002",
  "NEWLINK.04_2002",
  "NEWLINK.05_2002",
  "NEWLINK.06_2002",
  "NEWLINK.07_2002",
  "NEWLINK.08_2002",
  "DLILINK.01_2002",
  "DLILINK.02_2002",
  "DLILINK.03_2002",
  "DLILINK.04_2002",
  "DLILINK.05_2002",
  "DLILINK.06_2002",
  "DLILINK.07_2002",
  "DLILINK.08_2002",
  "DLILINK.09_2002",
  "PDLILINK.01_2002",
  "PDLILINK.02_2002",
  "PDLILINK.03_2002",
  "PDLILINK.04_2002",
  "PDLILINK.05_2002",
  "PDLILINK.06_2002",
  "PDLILINK.07_2002",
  "PDLILINK.08_2002",
  "NEWLINK.01_2004",
  "NEWLINK.02_2004",
  "NEWLINK.03_2004",
  "NEWLINK.04_2004",
  "NEWLINK.05_2004",
  "NEWLINK.06_2004",
  "NEWLINK.07_2004",
  "NEWLINK.08_2004",
  "NEWLINK.09_2004",
  "NEWLINK.10_2004",
  "NEWLINK.11_2004",
  "DLILINK.01_2004",
  "DLILINK.02_2004",
  "DLILINK.03_2004",
  "DLILINK.04_2004",
  "DLILINK.05_2004",
  "DLILINK.06_2004",
  "DLILINK.07_2004",
  "DLILINK.08_2004",
  "DLILINK.12_2004",
  "PDLILINK.01_2004",
  "PDLILINK.02_2004",
  "PDLILINK.03_2004",
  "PDLILINK.04_2004",
  "PDLILINK.05_2004",
  "PDLILINK.06_2004",
  "NEWLINK.01_2006",
  "NEWLINK.02_2006",
  "NEWLINK.03_2006",
  "NEWLINK.04_2006",
  "NEWLINK.05_2006",
  "NEWLINK.06_2006",
  "NEWLINK.07_2006",
  "NEWLINK.08_2006",
  "NEWLINK.09_2006",
  "NEWLINK.10_2006",
  "NEWLINK.11_2006",
  "DLILINK.01_2006",
  "DLILINK.02_2006",
  "DLILINK.03_2006",
  "DLILINK.04_2006",
  "DLILINK.05_2006",
  "DLILINK.06_2006",
  "DLILINK.07_2006",
  "PDLILINK.01_2006",
  "PDLILINK.02_2006",
  "PDLILINK.03_2006",
  "PDLILINK.04_2006",
  "PDLILINK.05_2006",
  "PDLILINK.06_2006",
  "NEWLINK.01_2008",
  "NEWLINK.02_2008",
  "NEWLINK.03_2008",
  "NEWLINK.04_2008",
  "NEWLINK.05_2008",
  "NEWLINK.06_2008",
  "NEWLINK.07_2008",
  "NEWLINK.08_2008",
  "NEWLINK.09_2008",
  "NEWLINK.10_2008",
  "NEWLINK.11_2008",
  "DLILINK.01_2008",
  "DLILINK.02_2008",
  "DLILINK.03_2008",
  "DLILINK.04_2008",
  "DLILINK.05_2008",
  "DLILINK.06_2008",
  "DLILINK.07_2008",
  "DLILINK.09_2008",
  "DLILINK.10_2008",
  "PDLILINK.01_2008",
  "PDLILINK.02_2008",
  "PDLILINK.03_2008",
  "PDLILINK.04_2008",
  "PDLILINK.05_2008",
  "PDLILINK.07_2008",
  "NEWLINK.01_2010",
  "NEWLINK.02_2010",
  "NEWLINK.03_2010",
  "NEWLINK.04_2010",
  "NEWLINK.05_2010",
  "NEWLINK.06_2010",
  "NEWLINK.07_2010",
  "NEWLINK.08_2010",
  "NEWLINK.09_2010",
  "DLILINK.01_2010",
  "DLILINK.02_2010",
  "DLILINK.03_2010",
  "DLILINK.04_2010",
  "DLILINK.05_2010",
  "DLILINK.06_2010",
  "DLILINK.07_2010",
  "DLILINK.09_2010",
  "PDLILINK.01_2010",
  "PDLILINK.02_2010",
  "PDLILINK.03_2010",
  "PDLILINK.04_2010",
  "PDLILINK.05_2010",
  "NEWLINK.01_2012",
  "NEWLINK.02_2012",
  "NEWLINK.03_2012",
  "NEWLINK.04_2012",
  "NEWLINK.05_2012",
  "NEWLINK.06_2012",
  "NEWLINK.07_2012",
  "NEWLINK.08_2012",
  "DLILINK.01_2012",
  "DLILINK.02_2012",
  "DLILINK.03_2012",
  "DLILINK.04_2012",
  "DLILINK.05_2012",
  "DLILINK.06_2012",
  "DLILINK.07_2012",
  "DLILINK.08_2012",
  "DLILINK.09_2012",
  "PDLILINK.01_2012",
  "PDLILINK.02_2012",
  "PDLILINK.03_2012",
  "PDLILINK.04_2012",
  "PDLILINK.05_2012",
  "EMPLINK.01_2014",
  "EMPLINK.02_2014",
  "EMPLINK.03_2014",
  "EMPLINK.04_2014",
  "EMPLINK.05_2014",
  "EMPLINK.06_2014",
  "EMPLINK.07_2014",
  "EMPLINK.08_2014",
  "EMPLINK.09_2014",
  "EMPLINK.10_2014",
  "EMPLINK.11_2014",
  "EMPLINK.12_2014",
  "EMPLINK.13_2014",
  "EMPLINK.14_2014",
  "EMPLINK.01_2016",
  "EMPLINK.02_2016",
  "EMPLINK.03_2016",
  "EMPLINK.04_2016",
  "EMPLINK.05_2016",
  "EMPLINK.06_2016",
  "EMPLINK.07_2016",
  "EMPLINK.08_2016")

new_data <- read_table2(str_c(raw_folder, "linking_raw.dat"), 
                        col_names = col_names,
                        col_types = cols(.default = col_double()))

# Handle missing values

new_data[new_data == -1] = NA  # Refused 
new_data[new_data == -2] = NA  # Dont know 
new_data[new_data == -3] = NA  # Invalid missing 
new_data[new_data == -4] = NA  # Valid missing 
new_data[new_data == -5] = NA  # Non-interview 

# Reshape and Clean -------------------------------------------------------

# Each variable is On Jobs type
# (DLI, PDLI, NEW for 2002-2012, EMP for 2014-2016) 
# + "LINK."
# + Employer supplement job number (1-14, although only have data
# on 1st 5) + "_" + year
# The data entry is the number of the job type in on jobs 
# (or -4 if not applicable)
# Goal is to create a data set linking es_job number 
# (keep only 1-5 because only ones we know job type)
# To oj_job number (1-5 for DLI/EMP, 6-10 for PDLI, 11-15 for NEW)
# for each case_id and year

# rename CASEID_1979 to case_id
new_data <- rename(new_data, case_id = CASEID_1979)

# Reshape the data from wide to long to make cleaning easier
vars <- c("DLI", "PDLI", "NEW", "EMP")
vary <- str_c(vars, "LINK")

constant <- "case_id"

# Variables we keep
keep <- str_c("^(", str_c(vary, collapse = "|"), "|", constant, ")")

# Transform data from wide to long
long <- new_data |> 
  dplyr::select(matches(keep)) |> 
  gather(matches(str_c("^(", str_c(vary, collapse="|"), ")")),
         key=key, value=val) |>
  extract(key, into=c("variable", "key"), 
          regex="(.+)(.[01]?._20..)$") |>
  filter(!is.na(variable), !is.na(key)) |>
  spread(key=variable, value=val) |>
  mutate(key = substring(key, 2)) |>
  separate(key, sep="_", into=c("es_job", "int_year"),
           convert = T) |> 
  rename(dli = DLILINK., pli = PDLILINK., nj = NEWLINK.,
         emp = EMPLINK.) |> 
  # Keep only observations with at least one job specific bit of data
  filter_at(vars(-case_id, -es_job, -int_year), any_vars(!is.na(.))) 

# Construct oj_job based on which job is present. Drop intermediate 
# variables
long <- long |>
  # Only es_jobs from 1-5 have data
  filter(es_job >= 1 & es_job <= 5) |> 
  mutate(
    oj_job = case_when(
      !is.na(dli) ~ dli,
      !is.na(emp) ~ emp,
      !is.na(pli) ~ 5 + pli,
      !is.na(nj) ~ 10 + nj
    )) |> 
  select(-dli:-pli)

# Save the data
write_csv(long, str_c(clean_folder, "linking_clean.csv"))