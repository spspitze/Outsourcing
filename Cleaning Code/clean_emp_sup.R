# This file takes raw data from emp_sup_raw (Employer Supplement)
# It cleans it then saves it as emp_sup_clean
rm(list = ls())

library(magrittr)
library(multiplex)
library(tidyverse)

# Folders of interest
raw_folder <- "../Raw Data/"
clean_folder <- "../Cleaned Data/"

new_data <- read_table2(str_c(raw_folder, "emp_sup_raw.dat"), 
                        col_types = cols(.default = col_double())) 

names(new_data) <- c(
  "CASEID_1979",
  "QES-84D.01_2002",
  "QES-84D.02_2002",
  "QES-84D.03_2002",
  "QES-84D.04_2002",
  "QES-84D.05_2002",
  "QES-84E.01_2002",
  "QES-84E.02_2002",
  "QES-84E.03_2002",
  "QES-84E.04_2002",
  "QES-84E.05_2002",
  "QES-84F.01_2002",
  "QES-84F.02_2002",
  "QES-84F.03_2002",
  "QES-84F.04_2002",
  "QES-84F.05_2002",
  "QES-84G.01_2002",
  "QES-84G.02_2002",
  "QES-84G.03_2002",
  "QES-84G.04_2002",
  "QES-84G.05_2002",
  "QES-84H.01_2002",
  "QES-84H.02_2002",
  "QES-84H.03_2002",
  "QES-84H.04_2002",
  "QES-84H.05_2002",
  "QES-84I.01_2002",
  "QES-84I.02_2002",
  "QES-84I.03_2002",
  "QES-84I.04_2002",
  "QES-84I.05_2002",
  "QES-84J.01_2002",
  "QES-84J.02_2002",
  "QES-84J.03_2002",
  "QES-84J.04_2002",
  "QES-84J.05_2002",
  "QES-84K.01_2002",
  "QES-84K.02_2002",
  "QES-84K.03_2002",
  "QES-84K.04_2002",
  "QES-84K.05_2002",
  "QES-84L.01_2002",
  "QES-84L.02_2002",
  "QES-84L.03_2002",
  "QES-84L.04_2002",
  "QES-84L.05_2002",
  "QES-84M.01_2002",
  "QES-84M.02_2002",
  "QES-84M.03_2002",
  "QES-84M.04_2002",
  "QES-84M.05_2002",
  "QES-89.01_2002",
  "QES-89.02_2002",
  "QES-89.03_2002",
  "QES-89.04_2002",
  "QES-89.05_2002",
  "JOB_UID_EMPROSTER1_2002",
  "JOB_UID_EMPROSTER2_2002",
  "JOB_UID_EMPROSTER3_2002",
  "JOB_UID_EMPROSTER4_2002",
  "JOB_UID_EMPROSTER5_2002",
  "JOB_UID_EMPROSTER6_2002",
  "JOB_UID_EMPROSTER7_2002",
  "JOB_UID_EMPROSTER8_2002",
  "JOB_UID_EMPROSTER9_2002",
  "QES-84D.01_2004",
  "QES-84D.02_2004",
  "QES-84D.03_2004",
  "QES-84D.04_2004",
  "QES-84D.05_2004",
  "QES-84E.01_2004",
  "QES-84E.02_2004",
  "QES-84E.03_2004",
  "QES-84E.04_2004",
  "QES-84E.05_2004",
  "QES-84F.01_2004",
  "QES-84F.02_2004",
  "QES-84F.03_2004",
  "QES-84F.04_2004",
  "QES-84F.05_2004",
  "QES-84G.01_2004",
  "QES-84G.02_2004",
  "QES-84G.03_2004",
  "QES-84G.04_2004",
  "QES-84G.05_2004",
  "QES-84H.01_2004",
  "QES-84H.02_2004",
  "QES-84H.03_2004",
  "QES-84H.04_2004",
  "QES-84H.05_2004",
  "QES-84I.01_2004",
  "QES-84I.02_2004",
  "QES-84I.03_2004",
  "QES-84I.04_2004",
  "QES-84I.05_2004",
  "QES-84J.01_2004",
  "QES-84J.02_2004",
  "QES-84J.03_2004",
  "QES-84J.04_2004",
  "QES-84J.05_2004",
  "QES-84K.01_2004",
  "QES-84K.02_2004",
  "QES-84K.03_2004",
  "QES-84K.04_2004",
  "QES-84K.05_2004",
  "QES-84L.01_2004",
  "QES-84L.02_2004",
  "QES-84L.03_2004",
  "QES-84L.04_2004",
  "QES-84L.05_2004",
  "QES-84M.01_2004",
  "QES-84M.02_2004",
  "QES-84M.03_2004",
  "QES-84M.04_2004",
  "QES-84M.05_2004",
  "QES-89.01_2004",
  "QES-89.02_2004",
  "QES-89.03_2004",
  "QES-89.04_2004",
  "QES-89.05_2004",
  "JOB_UID_EMPROSTER1_2004",
  "JOB_UID_EMPROSTER2_2004",
  "JOB_UID_EMPROSTER3_2004",
  "JOB_UID_EMPROSTER4_2004",
  "JOB_UID_EMPROSTER5_2004",
  "JOB_UID_EMPROSTER6_2004",
  "JOB_UID_EMPROSTER7_2004",
  "JOB_UID_EMPROSTER8_2004",
  "JOB_UID_EMPROSTER9_2004",
  "JOB_UID_EMPROSTER10_2004",
  "JOB_UID_EMPROSTER11_2004",
  "JOB_UID_EMPROSTER12_2004",
  "QES-84D.01_2006",
  "QES-84D.02_2006",
  "QES-84D.03_2006",
  "QES-84D.04_2006",
  "QES-84D.05_2006",
  "QES-84E.01~000001_2006",
  "QES-84E.01~000002_2006",
  "QES-84E.01~000003_2006",
  "QES-84E.01~000004_2006",
  "QES-84E.01~000005_2006",
  "QES-84E.01~000006_2006",
  "QES-84E.01~000007_2006",
  "QES-84E.01~000008_2006",
  "QES-84E.01~000009_2006",
  "QES-84E.02~000001_2006",
  "QES-84E.02~000002_2006",
  "QES-84E.02~000003_2006",
  "QES-84E.02~000004_2006",
  "QES-84E.02~000005_2006",
  "QES-84E.02~000006_2006",
  "QES-84E.02~000007_2006",
  "QES-84E.02~000008_2006",
  "QES-84E.02~000009_2006",
  "QES-84E.03~000001_2006",
  "QES-84E.03~000002_2006",
  "QES-84E.03~000003_2006",
  "QES-84E.03~000004_2006",
  "QES-84E.03~000005_2006",
  "QES-84E.03~000006_2006",
  "QES-84E.03~000007_2006",
  "QES-84E.03~000008_2006",
  "QES-84E.03~000009_2006",
  "QES-84E.04~000001_2006",
  "QES-84E.04~000002_2006",
  "QES-84E.04~000003_2006",
  "QES-84E.04~000004_2006",
  "QES-84E.04~000005_2006",
  "QES-84E.04~000006_2006",
  "QES-84E.04~000007_2006",
  "QES-84E.04~000008_2006",
  "QES-84E.04~000009_2006",
  "QES-84E.05~000001_2006",
  "QES-84E.05~000002_2006",
  "QES-84E.05~000003_2006",
  "QES-84E.05~000004_2006",
  "QES-84E.05~000005_2006",
  "QES-84E.05~000006_2006",
  "QES-84E.05~000007_2006",
  "QES-84E.05~000008_2006",
  "QES-84E.05~000009_2006",
  "QES-89.01_2006",
  "QES-89.02_2006",
  "QES-89.03_2006",
  "QES-89.04_2006",
  "QES-89.05_2006",
  "JOB_UID_EMPROSTER1_2006",
  "JOB_UID_EMPROSTER2_2006",
  "JOB_UID_EMPROSTER3_2006",
  "JOB_UID_EMPROSTER4_2006",
  "JOB_UID_EMPROSTER5_2006",
  "JOB_UID_EMPROSTER6_2006",
  "JOB_UID_EMPROSTER7_2006",
  "JOB_UID_EMPROSTER8_2006",
  "JOB_UID_EMPROSTER9_2006",
  "JOB_UID_EMPROSTER10_2006",
  "JOB_UID_EMPROSTER11_2006",
  "QES-84D.01_2008",
  "QES-84D.02_2008",
  "QES-84D.03_2008",
  "QES-84D.04_2008",
  "QES-84D.05_2008",
  "QES-84E.01~000001_2008",
  "QES-84E.01~000002_2008",
  "QES-84E.01~000003_2008",
  "QES-84E.01~000004_2008",
  "QES-84E.01~000005_2008",
  "QES-84E.01~000006_2008",
  "QES-84E.01~000007_2008",
  "QES-84E.01~000008_2008",
  "QES-84E.01~000009_2008",
  "QES-84E.02~000001_2008",
  "QES-84E.02~000002_2008",
  "QES-84E.02~000003_2008",
  "QES-84E.02~000004_2008",
  "QES-84E.02~000005_2008",
  "QES-84E.02~000006_2008",
  "QES-84E.02~000007_2008",
  "QES-84E.02~000008_2008",
  "QES-84E.02~000009_2008",
  "QES-84E.03~000001_2008",
  "QES-84E.03~000002_2008",
  "QES-84E.03~000003_2008",
  "QES-84E.03~000004_2008",
  "QES-84E.03~000005_2008",
  "QES-84E.03~000006_2008",
  "QES-84E.03~000007_2008",
  "QES-84E.03~000008_2008",
  "QES-84E.03~000009_2008",
  "QES-84E.04~000001_2008",
  "QES-84E.04~000002_2008",
  "QES-84E.04~000003_2008",
  "QES-84E.04~000004_2008",
  "QES-84E.04~000005_2008",
  "QES-84E.04~000006_2008",
  "QES-84E.04~000007_2008",
  "QES-84E.04~000008_2008",
  "QES-84E.04~000009_2008",
  "QES-84E.05~000001_2008",
  "QES-84E.05~000002_2008",
  "QES-84E.05~000003_2008",
  "QES-84E.05~000004_2008",
  "QES-84E.05~000005_2008",
  "QES-84E.05~000006_2008",
  "QES-84E.05~000007_2008",
  "QES-84E.05~000008_2008",
  "QES-84E.05~000009_2008",
  "QES-89.01_2008",
  "QES-89.02_2008",
  "QES-89.03_2008",
  "QES-89.04_2008",
  "QES-89.05_2008",
  "JOB_UID_EMPROSTER1_2008",
  "JOB_UID_EMPROSTER2_2008",
  "JOB_UID_EMPROSTER3_2008",
  "JOB_UID_EMPROSTER4_2008",
  "JOB_UID_EMPROSTER5_2008",
  "JOB_UID_EMPROSTER6_2008",
  "JOB_UID_EMPROSTER7_2008",
  "JOB_UID_EMPROSTER8_2008",
  "JOB_UID_EMPROSTER9_2008",
  "JOB_UID_EMPROSTER10_2008",
  "JOB_UID_EMPROSTER11_2008",
  "QES-84D.01_2010",
  "QES-84D.02_2010",
  "QES-84D.03_2010",
  "QES-84D.04_2010",
  "QES-84D.05_2010",
  "QES-84E.01~000001_2010",
  "QES-84E.01~000002_2010",
  "QES-84E.01~000003_2010",
  "QES-84E.01~000004_2010",
  "QES-84E.01~000005_2010",
  "QES-84E.01~000006_2010",
  "QES-84E.01~000007_2010",
  "QES-84E.01~000008_2010",
  "QES-84E.01~000009_2010",
  "QES-84E.02~000001_2010",
  "QES-84E.02~000002_2010",
  "QES-84E.02~000003_2010",
  "QES-84E.02~000004_2010",
  "QES-84E.02~000005_2010",
  "QES-84E.02~000006_2010",
  "QES-84E.02~000007_2010",
  "QES-84E.02~000008_2010",
  "QES-84E.02~000009_2010",
  "QES-84E.03~000001_2010",
  "QES-84E.03~000002_2010",
  "QES-84E.03~000003_2010",
  "QES-84E.03~000004_2010",
  "QES-84E.03~000005_2010",
  "QES-84E.03~000006_2010",
  "QES-84E.03~000007_2010",
  "QES-84E.03~000008_2010",
  "QES-84E.03~000009_2010",
  "QES-84E.04~000001_2010",
  "QES-84E.04~000002_2010",
  "QES-84E.04~000003_2010",
  "QES-84E.04~000004_2010",
  "QES-84E.04~000005_2010",
  "QES-84E.04~000006_2010",
  "QES-84E.04~000007_2010",
  "QES-84E.04~000008_2010",
  "QES-84E.04~000009_2010",
  "QES-84E.05~000001_2010",
  "QES-84E.05~000002_2010",
  "QES-84E.05~000003_2010",
  "QES-84E.05~000004_2010",
  "QES-84E.05~000005_2010",
  "QES-84E.05~000006_2010",
  "QES-84E.05~000007_2010",
  "QES-84E.05~000008_2010",
  "QES-84E.05~000009_2010",
  "QES-89.01_2010",
  "QES-89.02_2010",
  "QES-89.03_2010",
  "QES-89.04_2010",
  "QES-89.05_2010",
  "JOB_UID_EMPROSTER1_2010",
  "JOB_UID_EMPROSTER2_2010",
  "JOB_UID_EMPROSTER3_2010",
  "JOB_UID_EMPROSTER4_2010",
  "JOB_UID_EMPROSTER5_2010",
  "JOB_UID_EMPROSTER6_2010",
  "JOB_UID_EMPROSTER7_2010",
  "JOB_UID_EMPROSTER8_2010",
  "JOB_UID_EMPROSTER9_2010",
  "QES-84D.01_2012",
  "QES-84D.02_2012",
  "QES-84D.03_2012",
  "QES-84D.04_2012",
  "QES-84D.05_2012",
  "QES-84E.01~000001_2012",
  "QES-84E.01~000002_2012",
  "QES-84E.01~000003_2012",
  "QES-84E.01~000004_2012",
  "QES-84E.01~000005_2012",
  "QES-84E.01~000006_2012",
  "QES-84E.01~000007_2012",
  "QES-84E.01~000008_2012",
  "QES-84E.01~000009_2012",
  "QES-84E.02~000001_2012",
  "QES-84E.02~000002_2012",
  "QES-84E.02~000003_2012",
  "QES-84E.02~000004_2012",
  "QES-84E.02~000005_2012",
  "QES-84E.02~000006_2012",
  "QES-84E.02~000007_2012",
  "QES-84E.02~000008_2012",
  "QES-84E.02~000009_2012",
  "QES-84E.03~000001_2012",
  "QES-84E.03~000002_2012",
  "QES-84E.03~000003_2012",
  "QES-84E.03~000004_2012",
  "QES-84E.03~000005_2012",
  "QES-84E.03~000006_2012",
  "QES-84E.03~000007_2012",
  "QES-84E.03~000008_2012",
  "QES-84E.03~000009_2012",
  "QES-84E.04~000001_2012",
  "QES-84E.04~000002_2012",
  "QES-84E.04~000003_2012",
  "QES-84E.04~000004_2012",
  "QES-84E.04~000005_2012",
  "QES-84E.04~000006_2012",
  "QES-84E.04~000007_2012",
  "QES-84E.04~000008_2012",
  "QES-84E.04~000009_2012",
  "QES-84E.05~000001_2012",
  "QES-84E.05~000002_2012",
  "QES-84E.05~000003_2012",
  "QES-84E.05~000004_2012",
  "QES-84E.05~000005_2012",
  "QES-84E.05~000006_2012",
  "QES-84E.05~000007_2012",
  "QES-84E.05~000008_2012",
  "QES-84E.05~000009_2012",
  "QES-89.01_2012",
  "QES-89.02_2012",
  "QES-89.03_2012",
  "QES-89.04_2012",
  "QES-89.05_2012",
  "JOB_UID_EMPROSTER1_2012",
  "JOB_UID_EMPROSTER2_2012",
  "JOB_UID_EMPROSTER3_2012",
  "JOB_UID_EMPROSTER4_2012",
  "JOB_UID_EMPROSTER5_2012",
  "JOB_UID_EMPROSTER6_2012",
  "JOB_UID_EMPROSTER7_2012",
  "JOB_UID_EMPROSTER8_2012",
  "JOB_UID_EMPROSTER9_2012",
  "QES-84D.01_2014",
  "QES-84D.02_2014",
  "QES-84D.03_2014",
  "QES-84D.04_2014",
  "QES-84D.05_2014",
  "QES-84E.01~000001_2014",
  "QES-84E.01~000002_2014",
  "QES-84E.01~000003_2014",
  "QES-84E.01~000004_2014",
  "QES-84E.01~000005_2014",
  "QES-84E.01~000006_2014",
  "QES-84E.01~000007_2014",
  "QES-84E.01~000008_2014",
  "QES-84E.01~000009_2014",
  "QES-84E.02~000001_2014",
  "QES-84E.02~000002_2014",
  "QES-84E.02~000003_2014",
  "QES-84E.02~000004_2014",
  "QES-84E.02~000005_2014",
  "QES-84E.02~000006_2014",
  "QES-84E.02~000007_2014",
  "QES-84E.02~000008_2014",
  "QES-84E.02~000009_2014",
  "QES-84E.03~000001_2014",
  "QES-84E.03~000002_2014",
  "QES-84E.03~000003_2014",
  "QES-84E.03~000004_2014",
  "QES-84E.03~000005_2014",
  "QES-84E.03~000006_2014",
  "QES-84E.03~000007_2014",
  "QES-84E.03~000008_2014",
  "QES-84E.03~000009_2014",
  "QES-84E.04~000001_2014",
  "QES-84E.04~000002_2014",
  "QES-84E.04~000003_2014",
  "QES-84E.04~000004_2014",
  "QES-84E.04~000005_2014",
  "QES-84E.04~000006_2014",
  "QES-84E.04~000007_2014",
  "QES-84E.04~000008_2014",
  "QES-84E.04~000009_2014",
  "QES-84E.05~000001_2014",
  "QES-84E.05~000002_2014",
  "QES-84E.05~000003_2014",
  "QES-84E.05~000004_2014",
  "QES-84E.05~000005_2014",
  "QES-84E.05~000006_2014",
  "QES-84E.05~000007_2014",
  "QES-84E.05~000008_2014",
  "QES-84E.05~000009_2014",
  "QES-89.01_2014",
  "QES-89.02_2014",
  "QES-89.03_2014",
  "QES-89.04_2014",
  "QES-89.05_2014",
  "JOB_UID_EMPROSTER1_2014",
  "JOB_UID_EMPROSTER2_2014",
  "JOB_UID_EMPROSTER3_2014",
  "JOB_UID_EMPROSTER4_2014",
  "JOB_UID_EMPROSTER5_2014",
  "JOB_UID_EMPROSTER6_2014",
  "JOB_UID_EMPROSTER7_2014",
  "JOB_UID_EMPROSTER8_2014",
  "JOB_UID_EMPROSTER9_2014",
  "JOB_UID_EMPROSTER10_2014",
  "JOB_UID_EMPROSTER11_2014",
  "JOB_UID_EMPROSTER12_2014",
  "JOB_UID_EMPROSTER13_2014",
  "JOB_UID_EMPROSTER14_2014",
  "QES-84D.01_2016",
  "QES-84D.02_2016",
  "QES-84D.03_2016",
  "QES-84D.04_2016",
  "QES-84D.05_2016",
  "QES-84E.01~000001_2016",
  "QES-84E.01~000002_2016",
  "QES-84E.01~000003_2016",
  "QES-84E.01~000004_2016",
  "QES-84E.01~000005_2016",
  "QES-84E.01~000006_2016",
  "QES-84E.01~000007_2016",
  "QES-84E.01~000008_2016",
  "QES-84E.01~000009_2016",
  "QES-84E.02~000001_2016",
  "QES-84E.02~000002_2016",
  "QES-84E.02~000003_2016",
  "QES-84E.02~000004_2016",
  "QES-84E.02~000005_2016",
  "QES-84E.02~000006_2016",
  "QES-84E.02~000007_2016",
  "QES-84E.02~000008_2016",
  "QES-84E.02~000009_2016",
  "QES-84E.03~000001_2016",
  "QES-84E.03~000002_2016",
  "QES-84E.03~000003_2016",
  "QES-84E.03~000004_2016",
  "QES-84E.03~000005_2016",
  "QES-84E.03~000006_2016",
  "QES-84E.03~000007_2016",
  "QES-84E.03~000008_2016",
  "QES-84E.03~000009_2016",
  "QES-84E.04~000001_2016",
  "QES-84E.04~000002_2016",
  "QES-84E.04~000003_2016",
  "QES-84E.04~000004_2016",
  "QES-84E.04~000005_2016",
  "QES-84E.04~000006_2016",
  "QES-84E.04~000007_2016",
  "QES-84E.04~000008_2016",
  "QES-84E.04~000009_2016",
  "QES-84E.05~000001_2016",
  "QES-84E.05~000002_2016",
  "QES-84E.05~000003_2016",
  "QES-84E.05~000004_2016",
  "QES-84E.05~000005_2016",
  "QES-84E.05~000006_2016",
  "QES-84E.05~000007_2016",
  "QES-84E.05~000008_2016",
  "QES-84E.05~000009_2016",
  "QES-89.01_2016",
  "QES-89.02_2016",
  "QES-89.03_2016",
  "QES-89.04_2016",
  "QES-89.05_2016",
  "JOB_UID_EMPROSTER1_2016",
  "JOB_UID_EMPROSTER2_2016",
  "JOB_UID_EMPROSTER3_2016",
  "JOB_UID_EMPROSTER4_2016",
  "JOB_UID_EMPROSTER5_2016",
  "JOB_UID_EMPROSTER6_2016",
  "JOB_UID_EMPROSTER7_2016",
  "JOB_UID_EMPROSTER8_2016")


# Handle missing values

new_data[new_data == -1] = NA  # Refused 
new_data[new_data == -2] = NA  # Dont know 
new_data[new_data == -3] = NA  # Invalid missing 
new_data[new_data == -4] = NA  # Valid missing 
new_data[new_data == -5] = NA  # Non-interview 

# Create a function that, given a variable name and data set, returns T
# if variable is in names, F else
check <- function(var, data){
  return(var %in% colnames(data))
}

# Create a function to take a variable name and data set. Return the varibale if
# filled. If NA, check if other"s in group that are not NA. If all are NA, return
# NA, otherwise return mean
fill_NA_mean <- function(vector){
  return(
    ifelse(
      !is.na(vector), vector,
      ifelse(all(is.na(vector)), NA, mean(vector, na.rm = T))
      )
    )
}


# Define Variables --------------------------------------------------------

# Here is a list of variable names that will be useful later
q_uid <- "JOB_UID_EMPROSTER"
q_ben <- "QES-84"
q_sat <- "QES-89.0"

ben_let <- c("E", "F", "G", "H", "I", "J", "K", "L", "M")
ben_name <- c("health", "life", "dental", "maternity", "retirement", "flex_sched",
         "profit_share", "train_school", "childcare")

# Rename case_id
new_data <- rename(new_data, case_id = CASEID_1979)

# Loop over years and jobs (only look at first 5 jobs, that's all we have data on)
for (job in 1:5){
  for (year in seq(2002, 2016, by=2)){
    
    # Get UID number
    if (check(str_c(q_uid, job, "_", year), new_data)){
      new_data %<>% 
        rename(!!str_c("emp_id_", job, "_", year) := !!str_c(q_uid, job, "_", year))
    }
    
    # For any benefits, keep only if == 0 (these respondents skiped the question below)
    new_data %<>% 
      mutate(!!str_c("any_benefits", job, year, sep = "_") := 
               ifelse(.[[str_c(q_ben, "D.0", job, "_", year)]] == 0, 0, NA)
      )
    
    # Job satisfaction
    if (check(str_c(q_sat, job, "_", year), new_data)){
      new_data %<>% 
        rename(!!str_c("job_sat", job, year, sep = "_") := 
                 !!str_c(q_sat, job, "_", year))
    }
  }
    
  # For benefits put as 0 if any_benefits == 0
  # Naming is different in 2002-2004 (uses name)
  for (ben in 1:9){
    for (year in c(2002, 2004)) {
      new_data %<>%
        mutate(!!str_c(ben_name[ben], job, year, sep = "_") :=
                 ifelse(
                   !is.na(.[[str_c("any_benefits", job, year, sep = "_")]]), 0,
                   .[[str_c(q_ben, ben_let[ben], ".0", job, "_", year)]]
                   )
               )
    }
    # In 2006-2016, use numbers instead
    for (year in seq(2006, 2016, by = 2)) {
      new_data %<>% 
        mutate(
          !!str_c(ben_name[ben], job, year, sep = "_") := 
            ifelse(
              !is.na(.[[str_c("any_benefits", job, year, sep = "_")]]), 0,
              .[[str_c(q_ben, "E.0", job, "~00000", ben, "_", year)]]
              )
          )
    }
  }
}

# Reshape and Clean  ------------------------------------------------------------

# Create list of variables to loop over jobs
vary <- c(ben_name, c("emp_id", "any_benefits", "job_sat"))

constant <- "case_id"

# Variables we keep
keep <- str_c("^(", str_c(vary, collapse = "|"), "|", constant, ")")

long <- new_data %>% 
  dplyr::select(matches(keep)) %>% 
  gather(matches(str_c("^(", str_c(vary, collapse="|"), ")")), key=key, value=val) %>%
  extract(key, into=c("variable", "key"), regex="(\\D+)(_._20..)") %>%
  filter(!is.na(variable), !is.na(key)) %>%
  spread(key=variable, value=val) %>%
  mutate(key = substring(key, 2)) %>%
  separate(key, sep="_", into=c("job", "int_year"), convert = T) %>% 
  # Keep only observations with at least one job specific bit of data
  filter_at(vars(-case_id, -job, -int_year), any_vars(!is.na(.))) %>% 
  filter(!is.na(emp_id)) 

vars_fill <- c(ben_name, c("any_benefits", "job_sat"))

long %<>% 
  # Calculate if recieved any benefits. 
  mutate(
    any_benefits = pmax(health, life, dental, maternity, retirement, flex_sched,
                        profit_share, train_school, childcare, na.rm = T)
  ) %>% 
  # For missing data, If other job years have data, take averages
  group_by(case_id, emp_id) %>% 
  mutate_at(vars_fill, fill_NA_mean) %>% 
  ungroup() 

# # Data from 2014 and 2016 interviews in on jobs are less reliable. Drop these years
# long %<>% filter(int_year < 2014) 

# Save the data
write_csv(long, str_c(clean_folder, "emp_sup_clean.csv"))
