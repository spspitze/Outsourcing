rm(list = ls())

# Set working directory
setwd('C:/Users/spspi/Dropbox/Documents/Outsourcing/NLSY 79 Data/OnJobsVariables')

new_data <- read.table('onjobsvariables.dat', sep=' ')
names(new_data) <- c(
  'CASEID_1979',
  'Q6-8E_1A.01.01_2004',
  'Q6-8E_1A.01.02_2004',
  'Q6-8E_1A.02.01_2004',
  'Q6-8E_1A.02.02_2004',
  'Q6-8E_1A.03.01_2004',
  'Q6-8E_1A.04.01_2004',
  'Q6-27I.01_2002',
  'Q6-27I.02_2002',
  'Q6-27I.03_2002',
  'Q6-27I.04_2002',
  'Q6-27I.05_2002',
  'Q6-27I.06_2002',
  'Q6-27I.07_2002',
  'Q6-27I.01_2004',
  'Q6-27I.02_2004',
  'Q6-27I.03_2004',
  'Q6-27I.04_2004',
  'Q6-27I.05_2004',
  'Q6-27I.01_2006',
  'Q6-27I.02_2006',
  'Q6-27I.03_2006',
  'Q6-27I.04_2006',
  'Q6-27I.05_2006',
  'Q6-27I.01_2008',
  'Q6-27I.02_2008',
  'Q6-27I.03_2008',
  'Q6-27I.04_2008',
  'Q6-27I.05_2008',
  'Q6-27I.01_2010',
  'Q6-27I.02_2010',
  'Q6-27I.03_2010',
  'Q6-27I.04_2010',
  'Q6-27I.05_2010',
  'Q6-27I.01_2012',
  'Q6-27I.02_2012',
  'Q6-27I.03_2012',
  'Q6-27I.04_2012',
  'Q6-27I.05_2012'
)

# Handle missing values

new_data[new_data == -1] = NA  # Refused 
new_data[new_data == -2] = NA  # Dont know 
new_data[new_data == -3] = NA  # Invalid missing 
new_data[new_data == -4] = NA  # Valid missing 
new_data[new_data == -5] = NA  # Non-interview 

## !!!!!!!!!!!!!!!!

# My code

library(tidyr)
library(dplyr)