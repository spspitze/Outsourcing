rm(list = ls())

# Set working directory
setwd('C:/Users/spspi/Dropbox/Documents/Outsourcing/NLSY 79 Data/Married Kids Weeks')

library(dplyr)

new_data <- read.table('married_kids_weeks.dat', sep=' ')
names(new_data) <- c('R0000100',
  'R8496600',
  'R8497400',
  'R8504200',
  'R8504300',
  'T0988400',
  'T0989200',
  'T0995900',
  'T0996000',
  'T2210400',
  'T2211000',
  'T2217700',
  'T2217800',
  'T3108300',
  'T3108900',
  'T3115700',
  'T3115800',
  'T4112800',
  'T4113400',
  'T4120200',
  'T4120300',
  'T5023200',
  'T5024800',
  'T5031400',
  'T5031500',
  'T5771100',
  'T5772800',
  'T5779600',
  'T5779700')


# Handle missing values

  new_data[new_data == -1] = NA  # Refused 
  new_data[new_data == -2] = NA  # Dont know 
  new_data[new_data == -3] = NA  # Invalid missing 
  new_data[new_data == -4] = NA  # Valid missing 
  new_data[new_data == -5] = NA  # Non-interview 


# Use qnames rather than rnums
  
# Note, I hand dropped year from NUMKID and NUMCH before running code

qnames = function(data) {
  names(data) <- c("CASEID_1979",
    "MARSTAT-COL_2004",
    "WKSWK-PCY_2004",
    "NUMKID_2004",
    "NUMCH_2004",
    "MARSTAT-COL_2006",
    "WKSWK-PCY_2006",
    "NUMKID_2006",
    "NUMCH_2006",
    "MARSTAT-COL_2008",
    "WKSWK-PCY_2008",
    "NUMKID_2008",
    "NUMCH_2008",
    "MARSTAT-COL_2010",
    "WKSWK-PCY_2010",
    "NUMKID_2010",
    "NUMCH_2010",
    "MARSTAT-COL_2012",
    "WKSWK-PCY_2012",
    "NUMKID_2012",
    "NUMCH_2012",
    "MARSTAT-COL_2014",
    "WKSWK-PCY_2014",
    "NUMKID_2014",
    "NUMCH_2014",
    "MARSTAT-COL_2016",
    "WKSWK-PCY_2016",
    "NUMKID_2016",
    "NUMCH_2016")
  return(data)
}


# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
new_data <- qnames(new_data)

# Note, I hand dropped abbreviated year from NUMKID and NUMCH before running code

# Clean data for each year, put in long format for easy matches
vary <- c('MARSTAT-COL_', 'WKSWK-PCY_', 'NUMKID_', 'NUMCH_')
constant <- c('CASEID_1979')


long <- new_data %>%
  select(one_of(constant), matches(paste0('^(', paste(vary, collapse='|'), ')'))) %>%
  gather(matches(paste0('^(', paste(vary, collapse='|'), ')')), key=key, value=val) %>%
  extract(key, into=c('variable', 'key'), regex='(\\D+)(20..)') %>%
  filter(!is.na(variable), !is.na(key)) %>%
  spread(key=variable, value=val) %>% 
  mutate(case_id = CASEID_1979,
         year = as.numeric(key), 
         single = as.numeric(`MARSTAT-COL_` == 1),
         married = as.numeric(`MARSTAT-COL_` == 2),
         other = as.numeric(`MARSTAT-COL_` == 3),
         tot_children = NUMKID_,
         hh_children = NUMCH_,
         weeks_worked_prev = `WKSWK-PCY_`
         ) %>% 
  select(case_id, year, single, married, other, tot_children, hh_children,
         weeks_worked_prev)
  
write.csv2(long, 'cleaned_m_k_w.csv', row.names=FALSE)