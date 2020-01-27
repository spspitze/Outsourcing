rm(list = ls())

# Set working directory
setwd('C:/Users/spspi/Dropbox/Documents/Outsourcing/NLSY 79 Data/Demographics')

new_data <- read.table('demographics.dat', sep=' ')
names(new_data) <- c('R0000100',
  'R0000500',
  'R0173600',
  'R0214700',
  'R0214800',
  'R2509800',
  'R2909200',
  'R3111200',
  'R3511200',
  'R3711200',
  'R4138900',
  'R4527600',
  'R5222900',
  'R5822800',
  'R6541400',
  'R7104600',
  'R7704100',
  'R7706200',
  'R7706300',
  'R7811500',
  'R8496500',
  'R8498601',
  'R8498700',
  'T0015400',
  'T0988300',
  'T0990401',
  'T0990500',
  'T1214300',
  'T1215400',
  'T2210300',
  'T2212200',
  'T2212300',
  'T2272800',
  'T2273900',
  'T3108200',
  'T3110100',
  'T3110200',
  'T3212900',
  'T3214000',
  'T4112700',
  'T4114600',
  'T4114700',
  'T4201100',
  'T4202200',
  'T5023100',
  'T5025900',
  'T5026000',
  'T5177200',
  'T5771000',
  'T5774000',
  'T5774100')


# Handle missing values

  new_data[new_data == -1] = NA  # Refused 
  new_data[new_data == -2] = NA  # Dont know 
  new_data[new_data == -3] = NA  # Invalid missing 
  new_data[new_data == -4] = NA  # Valid missing 
  new_data[new_data == -5] = NA  # Non-interview 


# If there are values not categorized they will be represented as NA


qnames = function(data) {
  names(data) <- c("CASEID_1979",
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
    "URBAN-RURAL_2002",
    "SMSARES_2002",
    "Q3-10B_2004",
    "REGION_2004",
    "URBAN-RURAL_REV_2004",
    "SMSARES_2004",
    "Q3-10B_2006",
    "REGION_2006",
    "URBAN-RURAL_REV_2006",
    "SMSARES_2006",
    "Q3-4_2008",
    "Q3-10B_2008",
    "REGION_2008",
    "URBAN-RURAL_2008",
    "SMSARES_2008",
    "Q3-4_2010",
    "Q3-10B_2010",
    "REGION_2010",
    "URBAN-RURAL_2010",
    "SMSARES_2010",
    "Q3-4_2012",
    "Q3-10B_2012",
    "REGION_2012",
    "URBAN-RURAL_2012",
    "SMSARES_2012",
    "Q3-4_2014",
    "Q3-10B_2014",
    "REGION_2014",
    "URBAN-RURAL_2014",
    "SMSARES_2014",
    "Q3-10B_2016",
    "REGION_2016",
    "URBAN-RURAL_2016",
    "SMSARES_2016")
  return(data)
}


#********************************************************************************************************

# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
new_data <- qnames(new_data)

#************************************************************************************************************

## !!!!!!!!!!!!!!!!

# My code

library(tidyr)
library(dplyr)

# Get variable names in a standard form so can convert from wide to long
wide <- data.frame(
  'case_id' = new_data$CASEID_1979,
  'samp_id' = new_data$SAMPLE_ID_1979)

# Define female, black, and hispanic
wide$female <- 1 * (new_data$SAMPLE_SEX_1979 == 2)
wide$black <- 1 * (new_data$SAMPLE_RACE_78SCRN == 2)
wide$hispanic <- 2 * (new_data$SAMPLE_RACE_78SCRN == 1)

# Find age, age_2 and age_3
for (year in seq(2002, 2016, by=2)){
  wide[[paste0('age_', year)]] <- year - 1900 - new_data$`Q1-3_A~Y`
}

# Define education as highest degree completed up to year
hd <- 'Q3-10B_'
hg <- 'Q3-4_'

# For 2002, need to find pmax from 1998 onwards
wide$educ_2002 <- pmax(new_data[[paste0(hd, 1988)]], new_data[[paste0(hd, 1989)]],
                       new_data[[paste0(hd, 1990)]], new_data[[paste0(hd, 1991)]],
                       new_data[[paste0(hd, 1992)]], new_data[[paste0(hd, 1993)]],
                       new_data[[paste0(hd, 1994)]], new_data[[paste0(hd, 1996)]],
                       new_data[[paste0(hd, 1998)]], new_data[[paste0(hd, 2000)]],
                       new_data[[paste0(hd, 2002)]], na.rm=TRUE)

# For 2004 onwards, just take pmax of this year and last year
for (year in seq(2004, 2016, by=2)){
  wide[[paste0('educ_', year)]] <- pmax(new_data[[paste0(hd, year)]],
                                        wide[[paste0('educ_', year - 2)]], na.rm=TRUE)
}

# For highest grade completed, use max report from 2008-2014
wide$h_grade <- pmax(new_data[[paste0(hg, 2008)]], new_data[[paste0(hg, 2010)]],
                     new_data[[paste0(hg, 2012)]], new_data[[paste0(hg, 2014)]],
                     na.rm=TRUE)


# Divide into less than hs diploma, hs diploma, associates degree, college degree,
# post-secondary
for (year in seq(2002, 2016, by=2)){
  wide[[paste0('less_hs_', year)]] <- ifelse(!is.na(wide[[paste0('educ_', year)]]), 0,
                                              (1 * (wide$h_grade <= 12)
                                               * is.na(wide[[paste0('educ_', year)]])))
  wide[[paste0('hs_dip_', year)]] <- ifelse(is.na(wide[[paste0('educ_', year)]]), 0,
                                            1 * (wide[[paste0('educ_', year)]] == 1))
  wide[[paste0('aa_deg_', year)]] <- ifelse(is.na(wide[[paste0('educ_', year)]]), 0,
                                            1 * (wide[[paste0('educ_', year)]] == 2))
  wide[[paste0('ba_deg_', year)]] <- ifelse(is.na(wide[[paste0('educ_', year)]]), 0,
                                            1 * (wide[[paste0('educ_', year)]] == 3 
                                                 | wide[[paste0('educ_', year)]] == 4))
  wide[[paste0('plus_deg_', year)]] <- ifelse(is.na(wide[[paste0('educ_', year)]]), 0,
                                              1 * (wide[[paste0('educ_', year)]] > 4))
  
}

# Get msa, urban_rural, and region each year
msa <- 'SMSARES_'
ur <- 'URBAN-RURAL_'
urr <- 'URBAN-RURAL_REV_'
reg <- 'REGION_'
for (year in seq(2002, 2016, by=2)){
  wide[[paste0('msa_', year)]] <- 1 * (new_data[[paste0(msa, year)]] == 1 
                                       | new_data[[paste0(msa, year)]] == 2)
  wide[[paste0('msa_cc_', year)]] <- 1 * (new_data[[paste0(msa, year)]] == 4)
  wide[[paste0('region_', year)]] <- new_data[[paste0(reg, year)]]
  
  if (year %in% c(2004, 2006)){
    wide[[paste0('urban_', year)]] <- 1 * (new_data[[paste0(urr, year)]] == 1 
                                           | wide[[paste0('msa_', year)]] == 1
                                           | wide[[paste0('msa_cc_', year)]] == 1)
  } else{
    wide[[paste0('urban_', year)]] <- 1 * (new_data[[paste0(ur, year)]] == 1 
                                           | wide[[paste0('msa_', year)]] == 1
                                           | wide[[paste0('msa_cc_', year)]] == 1)
  }
}

# Now switch data from wide to long

# Variables that will vary over time
vary <- c('age_', 'age_2_', 'age_3_', 'less_hs_', 'hs_dip_', 'aa_deg_',
          'ba_deg_', 'plus_deg_', 'msa_', 'msa_cc_', 'region_', 'urban_')

# Variables that will stay constant over time/job
constant <- c('case_id', 'samp_id', 'female', 'black', 'hispanic')

long <- wide %>%
  select(one_of(constant), matches(paste0("^(", paste(vary, collapse="|"), ")"))) %>%
  gather(matches(paste0("^(", paste(vary, collapse="|"), ")")),key=key,value=val) %>%
  extract(key, into=c("variable","key"),regex="(\\D+)(_20..)") %>%
  filter(!is.na(variable),!is.na(key)) %>%
  spread(key=variable,value=val) %>%
  mutate(key = substring(key, 2))  

long <- long %>% 
  rename(year = key) %>% 
  mutate(year = as.numeric(year), age_2 = age^2, age_3 = age^3, hispanic=hispanic/2) 

write.csv2(long, 'cleaned_demographics.csv', row.names=FALSE)
