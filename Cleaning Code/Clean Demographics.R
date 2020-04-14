# This file takes raw data from demographics_raw 
# It cleans it then saves it as demographics_clean
rm(list = ls())

library(DescTools)
library(multiplex)
library(tidyverse)

# Folders of interest
raw_folder <- "../Raw Data/"
clean_folder <- "../Cleaned Data/"

new_data <- read_table2(str_c(raw_folder, "demographics_raw.dat"))
names(new_data) <- c(
  "CASEID_1979",
  "Q1-3_A~Y_1979",
  "SAMPLE_ID_1979",
  "SAMPLE_RACE_78SCRN",
  "SAMPLE_SEX_1979",
  "Q3-10B_1988",
  "Q3-10B_1989",
  "Q3-10B_1990",
  "Q3-10B_1991",
  "Q3-10B_1992",
  "Q3-10B_1993",
  "Q3-10B_1994",
  "Q3-10B_1996",
  "Q3-10B_1998",
  "Q3-10B_2000",
  "Q3-10B_2002",
  "REGION_2002",
  "MARSTAT-COL_2002",
  "WKSWK-PCY_2002",
  "SMSARES_2002",
  "NUMKID02_2002",
  "NUMCH02_2002",
  "Q3-10B_2004",
  "REGION_2004",
  "MARSTAT-COL_2004",
  "WKSWK-PCY_2004",
  "SMSARES_2004",
  "NUMKID04_2004",
  "NUMCH04_2004",
  "Q3-10B_2006",
  "REGION_2006",
  "MARSTAT-COL_2006",
  "WKSWK-PCY_2006",
  "SMSARES_2006",
  "NUMKID06_2006",
  "NUMCH06_2006",
  "Q3-10D_2008",
  "REGION_2008",
  "MARSTAT-COL_2008",
  "WKSWK-PCY_2008",
  "SMSARES_2008",
  "NUMKID08_2008",
  "NUMCH08_2008",
  "Q3-10D_2010",
  "REGION_2010",
  "MARSTAT-COL_2010",
  "WKSWK-PCY_2010",
  "SMSARES_2010",
  "NUMKID10_2010",
  "NUMCH10_2010",
  "Q3-10D_2012",
  "REGION_2012",
  "MARSTAT-COL_2012",
  "WKSWK-PCY_2012",
  "SMSARES_2012",
  "NUMKID12_2012",
  "NUMCH12_2012",
  "Q3-10D_2014",
  "REGION_2014",
  "MARSTAT-COL_2014",
  "WKSWK-PCY_2014",
  "SMSARES_2014",
  "NUMKID14_2014",
  "NUMCH14_2014",
  "Q3-10D_2016",
  "REGION_2016",
  "MARSTAT-COL_2016",
  "WKSWK-PCY_2016",
  "SMSARES_2016",
  "NUMKID16_2016",
  "NUMCH16_2016"
  )

# Handle missing values

new_data[new_data == -1] = NA  # Refused 
new_data[new_data == -2] = NA  # Dont know 
new_data[new_data == -3] = NA  # Invalid missing 
new_data[new_data == -4] = NA  # Valid missing 
new_data[new_data == -5] = NA  # Non-interview 


#************************************************************************************************************

# Start cleaning data

# Rename case_id and sample_id
new_data <- rename(new_data, "case_id" = CASEID_1979, "sample_id" = SAMPLE_ID_1979)

# Define female, black, and hispanic
new_data$female <- 1 * (new_data$SAMPLE_SEX_1979 == 2)
new_data$black <- 1 * (new_data$SAMPLE_RACE_78SCRN == 2)
new_data$hispanic <- 2 * (new_data$SAMPLE_RACE_78SCRN == 1)

# Find birth_year (will use it to calculate age later)
for (year in seq(2002, 2016, by=2)){
  new_data[[str_c("birth_year_", year)]] <- (1900 + new_data$`Q1-3_A~Y_1979`)
}

# Education questions
q_hb <- "Q3-10B_"
q_hd <- "Q3-10D_"

# Residence questions
q_msa <- "SMSARES_"

# Marital status and children
q_mar <- "MARSTAT-COL_"
q_nkid <- "NUMKID"
q_nch <- "NUMCH"

# Weeks worked question
q_wks <- "WKSWK-PCY_"

# For 2002, need to find pmax from 1998 onwards 
new_data$educ_2002 <- pmax(new_data[[str_c(q_hb, 1988)]], new_data[[str_c(q_hb, 1989)]],
                       new_data[[str_c(q_hb, 1990)]], new_data[[str_c(q_hb, 1991)]],
                       new_data[[str_c(q_hb, 1992)]], new_data[[str_c(q_hb, 1993)]],
                       new_data[[str_c(q_hb, 1994)]], new_data[[str_c(q_hb, 1996)]],
                       new_data[[str_c(q_hb, 1998)]], new_data[[str_c(q_hb, 2000)]],
                       new_data[[str_c(q_hb, 2002)]], na.rm=TRUE)

# For 2004 onwards, just take pmax of this year and last year
# (Variable used changes in 2008)
for (year in seq(2004, 2006, by=2)){
  new_data[[str_c("educ_", year)]] <- 
    pmax(new_data[[str_c(q_hb, year)]], new_data[[str_c("educ_", year - 2)]], na.rm=TRUE)
}

for (year in seq(2008, 2016, by=2)){
  new_data[[str_c("educ_", year)]] <- 
    pmax(new_data[[str_c(q_hd, year)]], new_data[[str_c("educ_", year - 2)]], na.rm=TRUE)
}

# Starting in 2014, work backwards and fill in NA with future degree 
# (assume the haven"t gained education) 
# This especially helps determine no degree, as this was not an option prior
# to 2008
for (year in seq(2014, 2002, by = -2)){
  new_data[[str_c("educ_", year)]] <- ifelse(
    !is.na(new_data[[str_c("educ_", year)]]), new_data[[str_c("educ_", year)]],
    new_data[[str_c("educ_", year + 2)]]
  )
}


# Mark each person"s msa status, marital status, and weeks worked
for (year in seq(2002, 2016, by=2)){
  new_data[[str_c("msa_", year)]] <- new_data[[str_c(q_msa, year)]]
  new_data[[str_c("marital_status_", year)]] <- new_data[[str_c(q_mar, year)]]
  new_data[[str_c("wks_work_prev_", year)]] <- new_data[[str_c(q_wks, year)]]
}

# Mark each persons tot_child and hh_child
for (year in seq(2, 16, by=2)){
  l <- "_20"
  if (year < 10){
    l <- "_200"
  }
  new_data[[str_c("tot_child", l, year)]] <- new_data[[str_c(q_nkid, year, l, year)]]
  
  new_data[[str_c("hh_child", l, year)]] <- new_data[[str_c(q_nch, year, l, year)]]
}

# Switch from wide to long

vary <- c("birth_year", "educ", "msa", "region", "marital_status", "tot_child", "hh_child",
          "wks_work_prev")

constant <- c("case_id", "sample_id", "female", "black", "hispanic")

# Variables we keep
keep <- str_c("^(", str_c(vary, collapse = "|"), "|",
               str_c(constant, collapse = "|"), ")")

# Transform data from wide to long
long <- new_data %>% 
  dplyr::select(matches(keep)) %>% 
  gather(matches(str_c("^(", str_c(vary, collapse="|"), ")")), key=key, value=val) %>%
  extract(key, into=c("variable", "key"), regex="(\\D+)(_20..)") %>%
  filter(!is.na(variable), !is.na(key)) %>%
  spread(key=variable, value=val) %>% 
  rename(year = key, region = REGION) %>%
  mutate(year = as.numeric(substring(year, 2))) %>%
  # Keep only observations with at least one job specific bit of data
  filter_at(vars(-case_id, -sample_id, -female, -black, -hispanic, -year),
            any_vars(!is.na(.))) %>% 
  # Create variables desired
  mutate(
    # Create education buckets
    less_hs = 1 * (educ == 0),
    hs = 1 * (educ == 1),
    aa = 1 * (educ == 2),
    ba = 1 * (educ %in% c(3, 4)),
    plus_ba = 1 * ((educ > 4) & (educ < 8)),
    educ_other = 1 * (educ == 8)
  ) %>% 
  # For missing data, fill in with previous answers. If no previous answers, fill
  # in with next answers
  group_by(case_id) %>%
  fill(msa, region, marital_status, tot_child, hh_child, .direction = "downup") %>%  
  ungroup()

# Save the data
write_csv(long, str_c(clean_folder, "demographics_clean.csv"))
