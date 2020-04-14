rm(list = ls())


# library(clubSandwich)
# library(plm)
# library(lmtest)
library(DescTools)
library(estimatr)
library(openxlsx)
library(BSDA)
library(ggplot2)
library(srvyr)
library(tidyr)
library(dplyr)

# Set working directory
setwd('C:/Users/spspi/Dropbox/Documents/Outsourcing/NLSY 79 Data/1 Yearly Report')

# Place to save tables
tables <- 'C:/Users/spspi/Dropbox/Documents/Outsourcing/Tables/'
figures <- 'C:/Users/spspi/Dropbox/Documents/Outsourcing/Figures/NLSY 79/'

new_data <- read.table('yearly_report.dat', sep=' ')
names(new_data) <- c('CASEID_1979',
 'Q6-8H_A5A.01.01_2002',
 'Q6-8H_A5A.02.01_2002',
 'Q6-8H_A5A.03.01_2002',
 'Q6-8H_A5A.04.01_2002',
 'Q6-8I.01_2002',
 'Q6-8I.02_2002',
 'Q6-8I.03_2002',
 'Q6-8I.04_2002',
 'Q6-8I.05_2002',
 'Q6-16E_1A.01.01_2002',
 'Q6-16E_1A.02.01_2002',
 'Q6-16E_1A.03.01_2002',
 'Q6-16E_1A.04.01_2002',
 'Q6-16E_1A.05.01_2002',
 'Q6-16H_A5A.01.01_2002',
 'Q6-16H_A5A.02.01_2002',
 'Q6-16H_A5A.03.01_2002',
 'Q6-16I.01_2002',
 'Q6-16I.02_2002',
 'Q6-16I.03_2002',
 'Q6-16I.04_2002',
 'Q6-16I.05_2002',
 'Q6-27D_1A.01.01_2002',
 'Q6-27D_1A.02.01_2002',
 'Q6-27D_1A.03.01_2002',
 'Q6-27D_1A.04.01_2002',
 'Q6-27D_1A.05.01_2002',
 'Q6-27D_1A.06.01_2002',
 'Q6-27D_1A.07.01_2002',
 'Q6-27E_A5A.01.01_2002',
 'Q6-27E_A5A.02.01_2002',
 'Q6-27E_A5A.03.01_2002',
 'Q6-27E_A5A.04.01_2002',
 'Q6-27E_A5A.05.01_2002',
 'Q6-27E_A5A.06.01_2002',
 'Q6-27E_A5A.07.01_2002',
 'Q6-8H_A5A.01.01_2004',
 'Q6-8H_A5A.02.01_2004',
 'Q6-8H_A5A.03.01_2004',
 'Q6-8H_A5A.04.01_2004',
 'Q6-8I.01_2004',
 'Q6-8I.02_2004',
 'Q6-8I.03_2004',
 'Q6-8I.04_2004',
 'Q6-8I.05_2004',
 'Q6-9.01~M_2004',
 'Q6-9.01~Y_2004',
 'Q6-9.02~M_2004',
 'Q6-9.02~Y_2004',
 'Q6-9.03~M_2004',
 'Q6-9.03~Y_2004',
 'Q6-9.04~M_2004',
 'Q6-9.04~Y_2004',
 'Q6-9.05~M_2004',
 'Q6-9.05~Y_2004',
 'Q6-16E_1A.01.01_2004',
 'Q6-16E_1A.02.01_2004',
 'Q6-16E_1A.03.01_2004',
 'Q6-16H_A5A.01.01_2004',
 'Q6-16H_A5A.02.01_2004',
 'Q6-16I.01_2004',
 'Q6-16I.02_2004',
 'Q6-16I.03_2004',
 'Q6-17.01~M_2004',
 'Q6-17.01~Y_2004',
 'Q6-17.02~M_2004',
 'Q6-17.02~Y_2004',
 'Q6-17.03~M_2004',
 'Q6-17.03~Y_2004',
 'Q6-27D_1A.01.01_2004',
 'Q6-27D_1A.02.01_2004',
 'Q6-27D_1A.03.01_2004',
 'Q6-27D_1A.04.01_2004',
 'Q6-27D_1A.05.01_2004',
 'Q6-27E_A5A.01.01_2004',
 'Q6-27E_A5A.02.01_2004',
 'Q6-27E_A5A.03.01_2004',
 'Q6-27E_A5A.04.01_2004',
 'Q6-27E_A5A.05.01_2004',
 'Q6-27K.01~M_2004',
 'Q6-27K.01~Y_2004',
 'Q6-27K.02~M_2004',
 'Q6-27K.02~Y_2004',
 'Q6-27K.03~M_2004',
 'Q6-27K.03~Y_2004',
 'Q6-27K.04~M_2004',
 'Q6-27K.04~Y_2004',
 'Q6-27K.05~M_2004',
 'Q6-27K.05~Y_2004',
 'ONJS-8800_2004',
 'QES-25J.01_2004',
 'QES-25J.02_2004',
 'QES-25J.03_2004',
 'QES-25J.04_2004',
 'QES-25J.05_2004',
 'QES-84E.01_2004',
 'QES-84E.02_2004',
 'QES-84E.03_2004',
 'QES-84E.04_2004',
 'QES-84E.05_2004',
 'QES-84F.01_2004',
 'QES-84F.02_2004',
 'QES-84F.03_2004',
 'QES-84F.04_2004',
 'QES-84F.05_2004',
 'QES-84G.01_2004',
 'QES-84G.02_2004',
 'QES-84G.03_2004',
 'QES-84G.04_2004',
 'QES-84G.05_2004',
 'QES-84H.01_2004',
 'QES-84H.02_2004',
 'QES-84H.03_2004',
 'QES-84H.04_2004',
 'QES-84H.05_2004',
 'QES-84I.01_2004',
 'QES-84I.02_2004',
 'QES-84I.03_2004',
 'QES-84I.04_2004',
 'QES-84I.05_2004',
 'QES-84J.01_2004',
 'QES-84J.02_2004',
 'QES-84J.03_2004',
 'QES-84J.04_2004',
 'QES-84J.05_2004',
 'QES-84K.01_2004',
 'QES-84K.02_2004',
 'QES-84K.03_2004',
 'QES-84K.04_2004',
 'QES-84K.05_2004',
 'QES-84L.01_2004',
 'QES-84L.02_2004',
 'QES-84L.03_2004',
 'QES-84L.04_2004',
 'QES-84L.05_2004',
 'QES-84M.01_2004',
 'QES-84M.02_2004',
 'QES-84M.03_2004',
 'QES-84M.04_2004',
 'QES-84M.05_2004',
 'QES-84N.01_2004',
 'QES-84N.02_2004',
 'QES-84N.03_2004',
 'QES-84N.04_2004',
 'QES-84N.05_2004',
 'QES-84NA.01_2004',
 'QES-84NA.02_2004',
 'QES-84NA.03_2004',
 'QES-84NA.04_2004',
 'QES-84NA.05_2004',
 'QES-84O.01_2004',
 'QES-84O.02_2004',
 'QES-84O.03_2004',
 'QES-84O.04_2004',
 'QES-84O.05_2004',
 'QES-89.01_2004',
 'QES-89.02_2004',
 'QES-89.03_2004',
 'QES-89.04_2004',
 'QES-89.05_2004',
 'JOB_UID_EMPROSTER1_2004',
 'JOB_UID_EMPROSTER2_2004',
 'JOB_UID_EMPROSTER3_2004',
 'JOB_UID_EMPROSTER4_2004',
 'JOB_UID_EMPROSTER5_2004',
 'JOB_UID_EMPROSTER6_2004',
 'JOB_UID_EMPROSTER7_2004',
 'JOB_UID_EMPROSTER8_2004',
 'JOB_UID_EMPROSTER9_2004',
 'JOB_UID_EMPROSTER10_2004',
 'JOB_UID_EMPROSTER11_2004',
 'JOB_UID_EMPROSTER12_2004',
 'Q6-8E_1A.01.01_2006',
 'Q6-8E_1A.02.01_2006',
 'Q6-8E_1A.03.01_2006',
 'Q6-8H_A5A.01.01_2006',
 'Q6-8H_A5A.02.01_2006',
 'Q6-8H_A5A.03.01_2006',
 'Q6-8H_A5A.04.01_2006',
 'Q6-8I.01_2006',
 'Q6-8I.02_2006',
 'Q6-8I.03_2006',
 'Q6-8I.04_2006',
 'Q6-9.01~M_2006',
 'Q6-9.01~Y_2006',
 'Q6-9.02~M_2006',
 'Q6-9.02~Y_2006',
 'Q6-9.03~M_2006',
 'Q6-9.03~Y_2006',
 'Q6-9.04~M_2006',
 'Q6-9.04~Y_2006',
 'Q6-16E_1A.01.01_2006',
 'Q6-16E_1A.02.01_2006',
 'Q6-16E_1A.03.01_2006',
 'Q6-16E_1A.04.01_2006',
 'Q6-16H_A5A.01.01_2006',
 'Q6-16H_A5A.02.01_2006',
 'Q6-16I.01_2006',
 'Q6-16I.02_2006',
 'Q6-16I.03_2006',
 'Q6-16I.04_2006',
 'Q6-17.01~M_2006',
 'Q6-17.01~Y_2006',
 'Q6-17.02~M_2006',
 'Q6-17.02~Y_2006',
 'Q6-17.03~M_2006',
 'Q6-17.03~Y_2006',
 'Q6-17.04~M_2006',
 'Q6-17.04~Y_2006',
 'Q6-27D_1A.01.01_2006',
 'Q6-27D_1A.02.01_2006',
 'Q6-27D_1A.03.01_2006',
 'Q6-27D_1A.04.01_2006',
 'Q6-27D_1A.05.01_2006',
 'Q6-27E_A5A.01.01_2006',
 'Q6-27E_A5A.02.01_2006',
 'Q6-27E_A5A.03.01_2006',
 'Q6-27E_A5A.04.01_2006',
 'Q6-27E_A5A.05.01_2006',
 'Q6-27K.01~M_2006',
 'Q6-27K.01~Y_2006',
 'Q6-27K.02~M_2006',
 'Q6-27K.02~Y_2006',
 'Q6-27K.03~M_2006',
 'Q6-27K.03~Y_2006',
 'Q6-27K.04~M_2006',
 'Q6-27K.04~Y_2006',
 'Q6-27K.05~M_2006',
 'Q6-27K.05~Y_2006',
 'ONJS-8800_2006',
 'QES-25J.01_2006',
 'QES-25J.02_2006',
 'QES-25J.03_2006',
 'QES-25J.04_2006',
 'QES-25J.05_2006',
 'QES-84E.01~000001_2006',
 'QES-84E.01~000002_2006',
 'QES-84E.01~000003_2006',
 'QES-84E.01~000004_2006',
 'QES-84E.01~000005_2006',
 'QES-84E.01~000006_2006',
 'QES-84E.01~000007_2006',
 'QES-84E.01~000008_2006',
 'QES-84E.01~000009_2006',
 'QES-84E.02~000001_2006',
 'QES-84E.02~000002_2006',
 'QES-84E.02~000003_2006',
 'QES-84E.02~000004_2006',
 'QES-84E.02~000005_2006',
 'QES-84E.02~000006_2006',
 'QES-84E.02~000007_2006',
 'QES-84E.02~000008_2006',
 'QES-84E.02~000009_2006',
 'QES-84E.03~000001_2006',
 'QES-84E.03~000002_2006',
 'QES-84E.03~000003_2006',
 'QES-84E.03~000004_2006',
 'QES-84E.03~000005_2006',
 'QES-84E.03~000006_2006',
 'QES-84E.03~000007_2006',
 'QES-84E.03~000008_2006',
 'QES-84E.03~000009_2006',
 'QES-84E.04~000001_2006',
 'QES-84E.04~000002_2006',
 'QES-84E.04~000003_2006',
 'QES-84E.04~000004_2006',
 'QES-84E.04~000005_2006',
 'QES-84E.04~000006_2006',
 'QES-84E.04~000007_2006',
 'QES-84E.04~000008_2006',
 'QES-84E.04~000009_2006',
 'QES-84E.05~000001_2006',
 'QES-84E.05~000002_2006',
 'QES-84E.05~000003_2006',
 'QES-84E.05~000004_2006',
 'QES-84E.05~000005_2006',
 'QES-84E.05~000006_2006',
 'QES-84E.05~000007_2006',
 'QES-84E.05~000008_2006',
 'QES-84E.05~000009_2006',
 'QES-84N.01_2006',
 'QES-84N.02_2006',
 'QES-84N.03_2006',
 'QES-84N.04_2006',
 'QES-84N.05_2006',
 'QES-84NA.01_2006',
 'QES-84NA.02_2006',
 'QES-84NA.03_2006',
 'QES-84NA.04_2006',
 'QES-84NA.05_2006',
 'QES-84O.01_2006',
 'QES-84O.02_2006',
 'QES-84O.03_2006',
 'QES-84O.04_2006',
 'QES-84O.05_2006',
 'QES-89.01_2006',
 'QES-89.02_2006',
 'QES-89.03_2006',
 'QES-89.04_2006',
 'QES-89.05_2006',
 'JOB_UID_EMPROSTER1_2006',
 'JOB_UID_EMPROSTER2_2006',
 'JOB_UID_EMPROSTER3_2006',
 'JOB_UID_EMPROSTER4_2006',
 'JOB_UID_EMPROSTER5_2006',
 'JOB_UID_EMPROSTER6_2006',
 'JOB_UID_EMPROSTER7_2006',
 'JOB_UID_EMPROSTER8_2006',
 'JOB_UID_EMPROSTER9_2006',
 'JOB_UID_EMPROSTER10_2006',
 'JOB_UID_EMPROSTER11_2006',
 'Q6-8E_1A.01.01_2008',
 'Q6-8E_1A.02.01_2008',
 'Q6-8E_1A.03.01_2008',
 'Q6-8E_1A.04.01_2008',
 'Q6-8E_1A.05.01_2008',
 'Q6-8H_A5A.01.01_2008',
 'Q6-8H_A5A.02.01_2008',
 'Q6-8H_A5A.04.01_2008',
 'Q6-8H_A5A.05.01_2008',
 'Q6-8I.01_2008',
 'Q6-8I.02_2008',
 'Q6-8I.03_2008',
 'Q6-8I.04_2008',
 'Q6-8I.05_2008',
 'Q6-9.01~M_2008',
 'Q6-9.01~Y_2008',
 'Q6-9.02~M_2008',
 'Q6-9.02~Y_2008',
 'Q6-9.03~M_2008',
 'Q6-9.03~Y_2008',
 'Q6-9.04~M_2008',
 'Q6-9.04~Y_2008',
 'Q6-16E_1A.01.01_2008',
 'Q6-16E_1A.02.01_2008',
 'Q6-16E_1A.03.01_2008',
 'Q6-16E_1A.04.01_2008',
 'Q6-16H_A5A.01.01_2008',
 'Q6-16H_A5A.02.01_2008',
 'Q6-16I.01_2008',
 'Q6-16I.02_2008',
 'Q6-16I.03_2008',
 'Q6-16I.04_2008',
 'Q6-17.01~M_2008',
 'Q6-17.01~Y_2008',
 'Q6-17.02~M_2008',
 'Q6-17.02~Y_2008',
 'Q6-17.03~M_2008',
 'Q6-17.03~Y_2008',
 'Q6-17.04~M_2008',
 'Q6-17.04~Y_2008',
 'Q6-27D_1A.01.01_2008',
 'Q6-27D_1A.02.01_2008',
 'Q6-27D_1A.03.01_2008',
 'Q6-27D_1A.04.01_2008',
 'Q6-27D_1A.05.01_2008',
 'Q6-27E_A5A.01.01_2008',
 'Q6-27E_A5A.02.01_2008',
 'Q6-27E_A5A.03.01_2008',
 'Q6-27E_A5A.04.01_2008',
 'Q6-27E_A5A.05.01_2008',
 'Q6-27K.01~M_2008',
 'Q6-27K.01~Y_2008',
 'Q6-27K.02~M_2008',
 'Q6-27K.02~Y_2008',
 'Q6-27K.03~M_2008',
 'Q6-27K.03~Y_2008',
 'Q6-27K.04~M_2008',
 'Q6-27K.04~Y_2008',
 'Q6-27K.05~M_2008',
 'Q6-27K.05~Y_2008',
 'ONJS-8800_2008',
 'QES-25J.01_2008',
 'QES-25J.02_2008',
 'QES-25J.03_2008',
 'QES-25J.04_2008',
 'QES-25J.05_2008',
 'QES-84E.01~000001_2008',
 'QES-84E.01~000002_2008',
 'QES-84E.01~000003_2008',
 'QES-84E.01~000004_2008',
 'QES-84E.01~000005_2008',
 'QES-84E.01~000006_2008',
 'QES-84E.01~000007_2008',
 'QES-84E.01~000008_2008',
 'QES-84E.01~000009_2008',
 'QES-84E.02~000001_2008',
 'QES-84E.02~000002_2008',
 'QES-84E.02~000003_2008',
 'QES-84E.02~000004_2008',
 'QES-84E.02~000005_2008',
 'QES-84E.02~000006_2008',
 'QES-84E.02~000007_2008',
 'QES-84E.02~000008_2008',
 'QES-84E.02~000009_2008',
 'QES-84E.03~000001_2008',
 'QES-84E.03~000002_2008',
 'QES-84E.03~000003_2008',
 'QES-84E.03~000004_2008',
 'QES-84E.03~000005_2008',
 'QES-84E.03~000006_2008',
 'QES-84E.03~000007_2008',
 'QES-84E.03~000008_2008',
 'QES-84E.03~000009_2008',
 'QES-84E.04~000001_2008',
 'QES-84E.04~000002_2008',
 'QES-84E.04~000003_2008',
 'QES-84E.04~000004_2008',
 'QES-84E.04~000005_2008',
 'QES-84E.04~000006_2008',
 'QES-84E.04~000007_2008',
 'QES-84E.04~000008_2008',
 'QES-84E.04~000009_2008',
 'QES-84E.05~000001_2008',
 'QES-84E.05~000002_2008',
 'QES-84E.05~000003_2008',
 'QES-84E.05~000004_2008',
 'QES-84E.05~000005_2008',
 'QES-84E.05~000006_2008',
 'QES-84E.05~000007_2008',
 'QES-84E.05~000008_2008',
 'QES-84E.05~000009_2008',
 'QES-84N.01_2008',
 'QES-84N.02_2008',
 'QES-84N.03_2008',
 'QES-84N.04_2008',
 'QES-84N.05_2008',
 'QES-84NA.01_2008',
 'QES-84NA.02_2008',
 'QES-84NA.03_2008',
 'QES-84NA.04_2008',
 'QES-84O.01_2008',
 'QES-84O.02_2008',
 'QES-84O.03_2008',
 'QES-84O.04_2008',
 'QES-84O.05_2008',
 'QES-89.01_2008',
 'QES-89.02_2008',
 'QES-89.03_2008',
 'QES-89.04_2008',
 'QES-89.05_2008',
 'JOB_UID_EMPROSTER1_2008',
 'JOB_UID_EMPROSTER2_2008',
 'JOB_UID_EMPROSTER3_2008',
 'JOB_UID_EMPROSTER4_2008',
 'JOB_UID_EMPROSTER5_2008',
 'JOB_UID_EMPROSTER6_2008',
 'JOB_UID_EMPROSTER7_2008',
 'JOB_UID_EMPROSTER8_2008',
 'JOB_UID_EMPROSTER9_2008',
 'JOB_UID_EMPROSTER10_2008',
 'JOB_UID_EMPROSTER11_2008',
 'Q6-8E_1A.01.01_2010',
 'Q6-8E_1A.02.01_2010',
 'Q6-8E_1A.03.01_2010',
 'ONJS-8800_2010',
 'QES-25J.01_2010',
 'QES-25J.02_2010',
 'QES-25J.03_2010',
 'QES-25J.04_2010',
 'QES-25J.05_2010',
 'QES-84E.01~000001_2010',
 'QES-84E.01~000002_2010',
 'QES-84E.01~000003_2010',
 'QES-84E.01~000004_2010',
 'QES-84E.01~000005_2010',
 'QES-84E.01~000006_2010',
 'QES-84E.01~000007_2010',
 'QES-84E.01~000008_2010',
 'QES-84E.01~000009_2010',
 'QES-84E.02~000001_2010',
 'QES-84E.02~000002_2010',
 'QES-84E.02~000003_2010',
 'QES-84E.02~000004_2010',
 'QES-84E.02~000005_2010',
 'QES-84E.02~000006_2010',
 'QES-84E.02~000007_2010',
 'QES-84E.02~000008_2010',
 'QES-84E.02~000009_2010',
 'QES-84E.03~000001_2010',
 'QES-84E.03~000002_2010',
 'QES-84E.03~000003_2010',
 'QES-84E.03~000004_2010',
 'QES-84E.03~000005_2010',
 'QES-84E.03~000006_2010',
 'QES-84E.03~000007_2010',
 'QES-84E.03~000008_2010',
 'QES-84E.03~000009_2010',
 'QES-84E.04~000001_2010',
 'QES-84E.04~000002_2010',
 'QES-84E.04~000003_2010',
 'QES-84E.04~000004_2010',
 'QES-84E.04~000005_2010',
 'QES-84E.04~000006_2010',
 'QES-84E.04~000007_2010',
 'QES-84E.04~000008_2010',
 'QES-84E.04~000009_2010',
 'QES-84E.05~000001_2010',
 'QES-84E.05~000002_2010',
 'QES-84E.05~000003_2010',
 'QES-84E.05~000004_2010',
 'QES-84E.05~000005_2010',
 'QES-84E.05~000006_2010',
 'QES-84E.05~000007_2010',
 'QES-84E.05~000008_2010',
 'QES-84E.05~000009_2010',
 'QES-84N.01_2010',
 'QES-84N.02_2010',
 'QES-84N.03_2010',
 'QES-84N.04_2010',
 'QES-84N.05_2010',
 'QES-84NA.01_2010',
 'QES-84NA.02_2010',
 'QES-84NA.03_2010',
 'QES-84NA.04_2010',
 'QES-84O.01_2010',
 'QES-84O.02_2010',
 'QES-84O.03_2010',
 'QES-84O.04_2010',
 'QES-84O.05_2010',
 'QES-89.01_2010',
 'QES-89.02_2010',
 'QES-89.03_2010',
 'QES-89.04_2010',
 'QES-89.05_2010',
 'Q6-8H_A5A.01.01_2010',
 'Q6-8H_A5A.02.01_2010',
 'Q6-8H_A5A.03.01_2010',
 'Q6-8I.01_2010',
 'Q6-8I.02_2010',
 'Q6-8I.03_2010',
 'Q6-8I.04_2010',
 'Q6-9.01~M_2010',
 'Q6-9.01~Y_2010',
 'Q6-9.02~M_2010',
 'Q6-9.02~Y_2010',
 'Q6-9.03~M_2010',
 'Q6-9.03~Y_2010',
 'Q6-9.04~M_2010',
 'Q6-9.04~Y_2010',
 'Q6-16E_1A.01.01_2010',
 'Q6-16E_1A.02.01_2010',
 'Q6-16E_1A.03.01_2010',
 'Q6-16E_1A.04.01_2010',
 'Q6-16H_A5A.01.01_2010',
 'Q6-16H_A5A.02.01_2010',
 'Q6-16I.01_2010',
 'Q6-16I.02_2010',
 'Q6-16I.03_2010',
 'Q6-16I.04_2010',
 'Q6-17.01~M_2010',
 'Q6-17.01~Y_2010',
 'Q6-17.02~M_2010',
 'Q6-17.02~Y_2010',
 'Q6-17.03~M_2010',
 'Q6-17.03~Y_2010',
 'Q6-17.04~M_2010',
 'Q6-17.04~Y_2010',
 'Q6-27D_1A.01.01_2010',
 'Q6-27D_1A.02.01_2010',
 'Q6-27D_1A.03.01_2010',
 'Q6-27D_1A.04.01_2010',
 'Q6-27D_1A.05.01_2010',
 'Q6-27E_A5A.01.01_2010',
 'Q6-27E_A5A.02.01_2010',
 'Q6-27E_A5A.03.01_2010',
 'Q6-27E_A5A.04.01_2010',
 'Q6-27E_A5A.05.01_2010',
 'Q6-27K.01~M_2010',
 'Q6-27K.01~Y_2010',
 'Q6-27K.02~M_2010',
 'Q6-27K.02~Y_2010',
 'Q6-27K.03~M_2010',
 'Q6-27K.03~Y_2010',
 'Q6-27K.04~M_2010',
 'Q6-27K.04~Y_2010',
 'Q6-27K.05~M_2010',
 'Q6-27K.05~Y_2010',
 'JOB_UID_EMPROSTER1_2010',
 'JOB_UID_EMPROSTER2_2010',
 'JOB_UID_EMPROSTER3_2010',
 'JOB_UID_EMPROSTER4_2010',
 'JOB_UID_EMPROSTER5_2010',
 'JOB_UID_EMPROSTER6_2010',
 'JOB_UID_EMPROSTER7_2010',
 'JOB_UID_EMPROSTER8_2010',
 'JOB_UID_EMPROSTER9_2010',
 'Q6-8E_1A.01.01_2012',
 'Q6-8E_1A.02.01_2012',
 'Q6-8E_1A.03.01_2012',
 'Q6-8E_1A.04.01_2012',
 'Q6-8E_1A.05.01_2012',
 'Q6-8H_A5A.01.01_2012',
 'Q6-8H_A5A.02.01_2012',
 'Q6-8H_A5A.03.01_2012',
 'Q6-8H_A5A.04.01_2012',
 'Q6-8H_A5A.05.01_2012',
 'Q6-8I.01_2012',
 'Q6-8I.02_2012',
 'Q6-8I.03_2012',
 'Q6-8I.04_2012',
 'Q6-8I.05_2012',
 'Q6-9.01~M_2012',
 'Q6-9.01~Y_2012',
 'Q6-9.02~M_2012',
 'Q6-9.02~Y_2012',
 'Q6-9.03~M_2012',
 'Q6-9.03~Y_2012',
 'Q6-9.04~M_2012',
 'Q6-9.04~Y_2012',
 'Q6-17.01~M_2012',
 'Q6-17.01~Y_2012',
 'Q6-17.02~M_2012',
 'Q6-17.02~Y_2012',
 'Q6-17.03~M_2012',
 'Q6-17.03~Y_2012',
 'Q6-17.04~M_2012',
 'Q6-17.04~Y_2012',
 'Q6-17.05~M_2012',
 'Q6-17.05~Y_2012',
 'Q6-16E_1A.01.01_2012',
 'Q6-16E_1A.02.01_2012',
 'Q6-16E_1A.03.01_2012',
 'Q6-16H_A5A.01.01_2012',
 'Q6-27D_1A.01.01_2012',
 'Q6-27D_1A.02.01_2012',
 'Q6-27D_1A.03.01_2012',
 'Q6-27D_1A.04.01_2012',
 'Q6-27D_1A.05.01_2012',
 'Q6-27E_A5A.01.01_2012',
 'Q6-27E_A5A.02.01_2012',
 'Q6-27E_A5A.03.01_2012',
 'Q6-27E_A5A.04.01_2012',
 'Q6-27E_A5A.05.01_2012',
 'Q6-27K.01~M_2012',
 'Q6-27K.01~Y_2012',
 'Q6-27K.02~M_2012',
 'Q6-27K.02~Y_2012',
 'Q6-27K.03~M_2012',
 'Q6-27K.03~Y_2012',
 'Q6-27K.04~M_2012',
 'Q6-27K.04~Y_2012',
 'Q6-27K.05~M_2012',
 'Q6-27K.05~Y_2012',
 'ONJS-8800_2012',
 'QES-25J.01_2012',
 'QES-25J.02_2012',
 'QES-25J.03_2012',
 'QES-25J.04_2012',
 'QES-25J.05_2012',
 'QES-84E.01~000001_2012',
 'QES-84E.01~000002_2012',
 'QES-84E.01~000003_2012',
 'QES-84E.01~000004_2012',
 'QES-84E.01~000005_2012',
 'QES-84E.01~000006_2012',
 'QES-84E.01~000007_2012',
 'QES-84E.01~000008_2012',
 'QES-84E.01~000009_2012',
 'QES-84E.02~000001_2012',
 'QES-84E.02~000002_2012',
 'QES-84E.02~000003_2012',
 'QES-84E.02~000004_2012',
 'QES-84E.02~000005_2012',
 'QES-84E.02~000006_2012',
 'QES-84E.02~000007_2012',
 'QES-84E.02~000008_2012',
 'QES-84E.02~000009_2012',
 'QES-84E.03~000001_2012',
 'QES-84E.03~000002_2012',
 'QES-84E.03~000003_2012',
 'QES-84E.03~000004_2012',
 'QES-84E.03~000005_2012',
 'QES-84E.03~000006_2012',
 'QES-84E.03~000007_2012',
 'QES-84E.03~000008_2012',
 'QES-84E.03~000009_2012',
 'QES-84E.04~000001_2012',
 'QES-84E.04~000002_2012',
 'QES-84E.04~000003_2012',
 'QES-84E.04~000004_2012',
 'QES-84E.04~000005_2012',
 'QES-84E.04~000006_2012',
 'QES-84E.04~000007_2012',
 'QES-84E.04~000008_2012',
 'QES-84E.04~000009_2012',
 'QES-84E.05~000001_2012',
 'QES-84E.05~000002_2012',
 'QES-84E.05~000003_2012',
 'QES-84E.05~000004_2012',
 'QES-84E.05~000005_2012',
 'QES-84E.05~000006_2012',
 'QES-84E.05~000007_2012',
 'QES-84E.05~000008_2012',
 'QES-84E.05~000009_2012',
 'QES-84N.01_2012',
 'QES-84N.02_2012',
 'QES-84N.03_2012',
 'QES-84N.04_2012',
 'QES-84N.05_2012',
 'QES-84NA.01_2012',
 'QES-84NA.02_2012',
 'QES-84NA.03_2012',
 'QES-84NA.04_2012',
 'QES-84NA.05_2012',
 'QES-84O.01_2012',
 'QES-84O.02_2012',
 'QES-84O.03_2012',
 'QES-84O.04_2012',
 'QES-84O.05_2012',
 'QES-89.01_2012',
 'QES-89.02_2012',
 'QES-89.03_2012',
 'QES-89.04_2012',
 'QES-89.05_2012',
 'JOB_UID_EMPROSTER1_2012',
 'JOB_UID_EMPROSTER2_2012',
 'JOB_UID_EMPROSTER3_2012',
 'JOB_UID_EMPROSTER4_2012',
 'JOB_UID_EMPROSTER5_2012',
 'JOB_UID_EMPROSTER6_2012',
 'JOB_UID_EMPROSTER7_2012',
 'JOB_UID_EMPROSTER8_2012',
 'JOB_UID_EMPROSTER9_2012',
 'Q6-9.01~M_2014',
 'Q6-9.01~Y_2014',
 'Q6-9.02~M_2014',
 'Q6-9.02~Y_2014',
 'Q6-9.03~M_2014',
 'Q6-9.03~Y_2014',
 'Q6-9.04~M_2014',
 'Q6-9.04~Y_2014',
 'Q6-8E_1A.01.01_2014',
 'Q6-8E_1A.02.01_2014',
 'Q6-8E_1A.03.01_2014',
 'Q6-8E_1A.04.01_2014',
 'Q6-8H_A5A.01.01_2014',
 'Q6-8H_A5A.02.01_2014',
 'Q6-8H_A5A.03.01_2014',
 'Q6-8H_A5A.04.01_2014',
 'Q6-8H_A5A.05.01_2014',
 'ONJS-8800_2014',
 'QES-25J.01_2014',
 'QES-25J.02_2014',
 'QES-25J.03_2014',
 'QES-25J.04_2014',
 'QES-25J.05_2014',
 'QES-84E.01~000001_2014',
 'QES-84E.01~000002_2014',
 'QES-84E.01~000003_2014',
 'QES-84E.01~000004_2014',
 'QES-84E.01~000005_2014',
 'QES-84E.01~000006_2014',
 'QES-84E.01~000007_2014',
 'QES-84E.01~000008_2014',
 'QES-84E.01~000009_2014',
 'QES-84E.02~000001_2014',
 'QES-84E.02~000002_2014',
 'QES-84E.02~000003_2014',
 'QES-84E.02~000004_2014',
 'QES-84E.02~000005_2014',
 'QES-84E.02~000006_2014',
 'QES-84E.02~000007_2014',
 'QES-84E.02~000008_2014',
 'QES-84E.02~000009_2014',
 'QES-84E.03~000001_2014',
 'QES-84E.03~000002_2014',
 'QES-84E.03~000003_2014',
 'QES-84E.03~000004_2014',
 'QES-84E.03~000005_2014',
 'QES-84E.03~000006_2014',
 'QES-84E.03~000007_2014',
 'QES-84E.03~000008_2014',
 'QES-84E.03~000009_2014',
 'QES-84E.04~000001_2014',
 'QES-84E.04~000002_2014',
 'QES-84E.04~000003_2014',
 'QES-84E.04~000004_2014',
 'QES-84E.04~000005_2014',
 'QES-84E.04~000006_2014',
 'QES-84E.04~000007_2014',
 'QES-84E.04~000008_2014',
 'QES-84E.04~000009_2014',
 'QES-84E.05~000001_2014',
 'QES-84E.05~000002_2014',
 'QES-84E.05~000003_2014',
 'QES-84E.05~000004_2014',
 'QES-84E.05~000005_2014',
 'QES-84E.05~000006_2014',
 'QES-84E.05~000007_2014',
 'QES-84E.05~000008_2014',
 'QES-84E.05~000009_2014',
 'QES-84N.01_2014',
 'QES-84N.02_2014',
 'QES-84N.03_2014',
 'QES-84N.04_2014',
 'QES-84N.05_2014',
 'QES-84NA.01_2014',
 'QES-84NA.02_2014',
 'QES-84NA.03_2014',
 'QES-84NA.05_2014',
 'QES-84O.01_2014',
 'QES-84O.02_2014',
 'QES-84O.03_2014',
 'QES-84O.04_2014',
 'QES-84O.05_2014',
 'QES-89.01_2014',
 'QES-89.02_2014',
 'QES-89.03_2014',
 'QES-89.04_2014',
 'QES-89.05_2014',
 'JOB_UID_EMPROSTER1_2014',
 'JOB_UID_EMPROSTER2_2014',
 'JOB_UID_EMPROSTER3_2014',
 'JOB_UID_EMPROSTER4_2014',
 'JOB_UID_EMPROSTER5_2014',
 'JOB_UID_EMPROSTER6_2014',
 'JOB_UID_EMPROSTER7_2014',
 'JOB_UID_EMPROSTER8_2014',
 'JOB_UID_EMPROSTER9_2014',
 'JOB_UID_EMPROSTER10_2014',
 'JOB_UID_EMPROSTER11_2014',
 'JOB_UID_EMPROSTER12_2014',
 'JOB_UID_EMPROSTER13_2014',
 'JOB_UID_EMPROSTER14_2014',
 'Q6-8.01_2016',
 'Q6-8.02_2016',
 'Q6-8.03_2016',
 'Q6-8.04_2016',
 'Q6-9.01~M_2016',
 'Q6-9.01~Y_2016',
 'Q6-9.02~M_2016',
 'Q6-9.02~Y_2016',
 'Q6-9.03~M_2016',
 'Q6-9.03~Y_2016',
 'Q6-9.04~M_2016',
 'Q6-9.04~Y_2016',
 'Q6-8E_1A.01.01_2016',
 'Q6-8E_1A.02.01_2016',
 'Q6-8E_1A.03.01_2016',
 'Q6-8E_1A.04.01_2016',
 'Q6-8E_1A.05.01_2016',
 'Q6-8H_A5A.01.01_2016',
 'Q6-8H_A5A.02.01_2016',
 'Q6-8H_A5A.03.01_2016',
 'Q6-8H_A5A.04.01_2016',
 'Q6-8H_A5A.05.01_2016',
 'ONJS-8800_2016',
 'QES-25J.01_2016',
 'QES-25J.02_2016',
 'QES-25J.03_2016',
 'QES-25J.04_2016',
 'QES-25J.05_2016',
 'QES-84E.01~000001_2016',
 'QES-84E.01~000002_2016',
 'QES-84E.01~000003_2016',
 'QES-84E.01~000004_2016',
 'QES-84E.01~000005_2016',
 'QES-84E.01~000006_2016',
 'QES-84E.01~000007_2016',
 'QES-84E.01~000008_2016',
 'QES-84E.01~000009_2016',
 'QES-84E.02~000001_2016',
 'QES-84E.02~000002_2016',
 'QES-84E.02~000003_2016',
 'QES-84E.02~000004_2016',
 'QES-84E.02~000005_2016',
 'QES-84E.02~000006_2016',
 'QES-84E.02~000007_2016',
 'QES-84E.02~000008_2016',
 'QES-84E.02~000009_2016',
 'QES-84E.03~000001_2016',
 'QES-84E.03~000002_2016',
 'QES-84E.03~000003_2016',
 'QES-84E.03~000004_2016',
 'QES-84E.03~000005_2016',
 'QES-84E.03~000006_2016',
 'QES-84E.03~000007_2016',
 'QES-84E.03~000008_2016',
 'QES-84E.03~000009_2016',
 'QES-84E.04~000001_2016',
 'QES-84E.04~000002_2016',
 'QES-84E.04~000003_2016',
 'QES-84E.04~000004_2016',
 'QES-84E.04~000005_2016',
 'QES-84E.04~000006_2016',
 'QES-84E.04~000007_2016',
 'QES-84E.04~000008_2016',
 'QES-84E.04~000009_2016',
 'QES-84E.05~000001_2016',
 'QES-84E.05~000002_2016',
 'QES-84E.05~000003_2016',
 'QES-84E.05~000004_2016',
 'QES-84E.05~000005_2016',
 'QES-84E.05~000006_2016',
 'QES-84E.05~000007_2016',
 'QES-84E.05~000008_2016',
 'QES-84E.05~000009_2016',
 'QES-84NA.01_2016',
 'QES-84NA.02_2016',
 'QES-84NA.03_2016',
 'QES-84NA.04_2016',
 'QES-84NA.05_2016',
 'QES-84NB.01_2016',
 'QES-84NB.02_2016',
 'QES-84NB.03_2016',
 'QES-84NB.04_2016',
 'QES-84NB.05_2016',
 'QES-84O.01_2016',
 'QES-84O.02_2016',
 'QES-84O.03_2016',
 'QES-84O.04_2016',
 'QES-84O.05_2016',
 'QES-89.01_2016',
 'QES-89.02_2016',
 'QES-89.03_2016',
 'QES-89.04_2016',
 'QES-89.05_2016',
 'JOB_UID_EMPROSTER1_2016',
 'JOB_UID_EMPROSTER2_2016',
 'JOB_UID_EMPROSTER3_2016',
 'JOB_UID_EMPROSTER4_2016',
 'JOB_UID_EMPROSTER5_2016',
 'JOB_UID_EMPROSTER6_2016',
 'JOB_UID_EMPROSTER7_2016',
 'JOB_UID_EMPROSTER8_2016')

# Merge with data from OnJobsVariables, which (right now)
# Is from the new extracts on NLSY79 website

new_data_2 <- read.table('../OnJobsVariables/onjobsvariables.dat', sep=' ')
names(new_data_2) <- c(
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

new_data <- inner_join(new_data, new_data_2, by = c('CASEID_1979'))

# Handle missing values

  new_data[new_data == -1] = NA  # Refused 
  new_data[new_data == -2] = NA  # Dont know 
  new_data[new_data == -3] = NA  # Invalid missing 
  new_data[new_data == -4] = NA  # Valid missing 
  new_data[new_data == -5] = NA  # Non-interview 


# If there are values not categorized they will be represented as NA


###########################################################################

# My code

# Create a function that, given a variable name, data set, and index,
# returns T if variable is in data set and is not NA for that index,
# false otherwise
check <- function(var, data, ind){
  c <- FALSE
  if (var %in% colnames(data)){
    if (!is.na(data[[var]][ind])){
      c <- T
    } 
  }
  return(c)
}

# Get variable names in a standard form so can convert from wide to long
wide <- data.frame('case_id' = new_data$CASEID_1979)

# Rename variables for each year and each job number
uidn <- 'JOB_UID_EMPROSTER'
q84 <- 'QES-84'
q89 <- 'QES-89.0'
q25 <- 'QES-25J.0'
qd <- 'Q6-8H_A5A.0'
qp <- 'Q6-16H_A5A.0'
qn <- 'Q6-27E_A5A.0'
qld <- 'Q6-8E_1A.0'
qlp <- 'Q6-16E_1A.0'
qln <- 'Q6-27D_1A.0'
qcd <- 'Q6-8I.0'
qcd2 <- 'Q6-8.0' # For 2016
qcp <- 'Q6-16I.0'
qcn <- 'Q6-27I.0'
qdd <- 'Q6-9.0'
qdp <- 'Q6-17.0'
qdn <- 'Q6-27K.0'
y <- '~Y_'
m <- '~M_'
onjs <- 'ONJS-8800_'
ben_name <- c('E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M')
ben <- c('health_', 'life_', 'dental_', 'mat_', 'retirement_', 'flex_',
         'profit_', 'train_', 'childcare_')
to_name <- c('N.0', 'NA.0', 'O.0')
to_ben <- c('sick_', 'sick_vac_', 'vac_')


# Three categories of jobs in On Jobs, jobs held at date of last interview (dli), jobs
# held prior to last interview (pli) and new jobs (nj). In On Jobs, each of these
# has it's own varibale name for any given question. These categories do not exist
# in employer supplement. To match better with ES, create one variable name, numbering
# 1-5 for dli, 6-10 for pli, and 11-15 for nj.

for (year in seq(2004, 2016, by=2)){
  for (job in seq(1, 5, by=1)){
    # From ES
    # First, rename all uid names (keep only first 5 to match other data)
    if (paste0(uidn, job, '_', year) %in% colnames(new_data)){
      wide[[paste0('emp_id_', job,'_', year)]] <- new_data[[paste0(uidn, job, '_', year)]]
    }
    
    # Rename all q89 as job satisfaction
    if (paste0(q89, job, '_', year) %in% colnames(new_data)){
      wide[[paste0('job_sat_', job,'_', year)]] <- new_data[[paste0(q89, job, '_', year)]]
    }
    
    # Report if non-traditional job (which includes contracted out)
    wide[[paste0('non_trad_', job,'_', year)]] <- ifelse(
      new_data[[paste0(q25, job, '_', year)]] == 2, 1, 0)
    
    # Create rank, current, contracted, num_cur, and m_flag for later
    new_data[[paste0('rank_', job,'_', year)]] <- NA
    new_data[[paste0('rank_', 5 + job,'_', year)]] <- NA
    new_data[[paste0('rank_', 10 + job,'_', year)]] <- NA
    new_data[[paste0('current_', job,'_', year)]] <- NA
    wide[[paste0('contracted_', job,'_', year)]] <- NA
    wide[[paste0('current_', job,'_', year)]] <- NA
    wide[[paste0('num_cur_', job,'_', year)]] <- NA
    wide[[paste0('m_flag_', job,'_', year)]] <- 0
    
    # Report time off
    for (ind in seq(1, 3, by=1)){
      if (paste0(q84, to_name[ind], job, '_', year) %in% colnames(new_data)){
        wide[[paste0(to_ben[ind], job,'_', year)]] <- new_data[[paste0(q84, to_name[ind], job, '_', year)]]
      }
    }
    
    
    # From OJ
    
    # Outside of 2004, ldw data for dli 5 not available, neither is pli 4 for 2004
    # or pli 5 for 2004-2010. Set these as NA
    if (year > 2004){
      new_data[[paste0('ldw_', 5,'_', year)]] <- NA
    } else {
      new_data[[paste0('ldw_', 9,'_', year)]] <- NA
    }
    if (year < 2012){
      new_data[[paste0('ldw_', 10,'_', year)]] <- NA
    }
    
    # Combine whether job started loop, if job is currently worked (must impute 2014), and
    # month and year last worked dli/pli/new job into 1 number in new_data
    # For pli and new jobs, change their job number to 5 + job# and 10 + job#
    # If loop is missing, report looped as NA
    # If month or year is missing, report using only one
    # Also create contracted as one variable (create based on if loop started)
    
    # dli
    if (paste0(qld, job, '.01_', year) %in% colnames(new_data)){
      new_data[[paste0('looped_', job,'_', year)]] <- new_data[[paste0(qld, job, '.01_', year)]]
    } else {
      new_data[[paste0('looped_', job,'_', year)]] <- NA
    }
    
    if (paste0(qcd, job, '_', year) %in% colnames(new_data)){
      new_data[[paste0('current_', job,'_', year)]] <- new_data[[paste0(qcd, job, '_', year)]]
    }
    
    if (paste0(qdd, job, m, year) %in% colnames(new_data)){
      new_data[[paste0('ldw_', job,'_', year)]] <- (
        new_data[[paste0(qdd, job, m, year)]] + 12 * new_data[[paste0(qdd, job, y, year)]])
      
      # If missing month or year, just use the other one
      new_data[[paste0('ldw_', job,'_', year)]] <- ifelse(
        is.na(new_data[[paste0('ldw_', job,'_', year)]]), 
        new_data[[paste0(qdd, job, m, year)]],
        new_data[[paste0('ldw_', job,'_', year)]])
      
      new_data[[paste0('ldw_', job,'_', year)]] <- ifelse(
        is.na(new_data[[paste0('ldw_', job,'_', year)]]), 
        new_data[[paste0(qdd, job, y, year)]],
        new_data[[paste0('ldw_', job,'_', year)]])
      
      # If missing everything but looped == 1, set = 1
      new_data[[paste0('ldw_', job,'_', year)]] <- ifelse(
        (is.na(new_data[[paste0('ldw_', job,'_', year)]]) & 
           new_data[[paste0('looped_', job,'_', year)]] == 1), 
        1, new_data[[paste0('ldw_', job,'_', year)]])
    }
    
    # Set current job's ldw to big number for later
    if (paste0('current_', job,'_', year) %in% colnames(new_data)){
      new_data[[paste0('ldw_', job,'_', year)]] <- ifelse(
        new_data[[paste0('current_', job,'_', year)]] == 1, 
        1e10, new_data[[paste0('ldw_', job,'_', year)]])
    }
    
    # For contracted, assume that if loop started but contracted is NA,
    # then contracted = 0 (because the respondent was never asked this question,
    # pressumable because their job was a different type, ie temp work)
    if (paste0(qd, job, '.01_', year) %in% colnames(new_data)){
      new_data[[paste0('contracted_', job,'_', year)]] <- pmax(
        new_data[[paste0(qd, job, '.01_', year)]],
        1 - new_data[[paste0('looped_', job,'_', year)]], na.rm=T)
    }
    
    # For match flag, set 0 if looped = 1 (will later change to 1 if matched)
    # else 0
    new_data[[paste0('m_flag_', job,'_', year)]] <- ifelse( 
      new_data[[paste0('looped_', job,'_', year)]] == 1, 0, NA)
    
    # pli 
    if (paste0(qlp, job, '.01_', year) %in% colnames(new_data)){
      new_data[[paste0('looped_', job + 5,'_', year)]] <- new_data[[paste0(qlp, job, '.01_', year)]]
    } else {
      new_data[[paste0('looped_', job + 5,'_', year)]] <- NA
    }
    
    if (paste0(qcp, job, '_', year) %in% colnames(new_data)){
      new_data[[paste0('current_', job + 5,'_', year)]] <- new_data[[paste0(qcp, job, '_', year)]]
    }
    
    if (paste0(qdp, job, m, year) %in% colnames(new_data)){
      new_data[[paste0('ldw_', job + 5,'_', year)]] <- (
        new_data[[paste0(qdp, job, m, year)]] + 12 * new_data[[paste0(qdp, job, y, year)]])
      
      # If missing month or year, just use the other one
      new_data[[paste0('ldw_', job + 5,'_', year)]] <- ifelse(
        is.na(new_data[[paste0('ldw_', job + 5,'_', year)]]), 
        new_data[[paste0(qdp, job, m, year)]],
        new_data[[paste0('ldw_', job + 5,'_', year)]])
      
      new_data[[paste0('ldw_', job + 5,'_', year)]] <- ifelse(
        is.na(new_data[[paste0('ldw_', job + 5,'_', year)]]), 
        new_data[[paste0(qdp, job, y, year)]],
        new_data[[paste0('ldw_', job + 5,'_', year)]])
      
      # If missing everything but looped == 1, set = 1
      new_data[[paste0('ldw_', job + 5,'_', year)]] <- ifelse(
        (is.na(new_data[[paste0('ldw_', job + 5,'_', year)]]) & 
           new_data[[paste0('looped_', job + 5,'_', year)]] == 1),
        1, new_data[[paste0('ldw_', job + 5,'_', year)]])
    }
    
    # Set current job's ldw to big number for later
    if (paste0('current_', job + 5,'_', year) %in% colnames(new_data)){
      new_data[[paste0('ldw_', job + 5,'_', year)]] <- ifelse(
        new_data[[paste0('current_', job + 5,'_', year)]] == 1, 
        1e10, new_data[[paste0('ldw_', job + 5,'_', year)]])
    }
    
    # For contracted, assume that if loop started but contracted is NA,
    # then contracted = 0 (because the respondent was never asked this question,
    # pressumable because their job was a different type, ie temp work)
    if (paste0(qp, job, '.01_', year) %in% colnames(new_data)){
      new_data[[paste0('contracted_', job + 5,'_', year)]] <- pmax(
        new_data[[paste0(qp, job, '.01_', year)]],
        1 - new_data[[paste0('looped_', job + 5,'_', year)]], na.rm=T)
    }
    
    # For match flag, set 0 if looped = 1 (will later change to 1 if matched)
    # else 0
    new_data[[paste0('m_flag_', job + 5,'_', year)]] <- ifelse( 
      new_data[[paste0('looped_', job + 5,'_', year)]] == 1, 0, NA)
    
    # new job
    if (paste0(qln, job, '.01_', year) %in% colnames(new_data)){
      new_data[[paste0('looped_', job + 10,'_', year)]] <- new_data[[paste0(qln, job, '.01_', year)]]
    } else {
      new_data[[paste0('looped_', job + 10,'_', year)]] <- NA
    }
    
    if (paste0(qcn, job, '_', year) %in% colnames(new_data)){
      new_data[[paste0('current_', job + 10,'_', year)]] <- new_data[[paste0(qcn, job, '_', year)]]
    }
    
    if (paste0(qdn, job, m, year) %in% colnames(new_data)){
      new_data[[paste0('ldw_', job + 10,'_', year)]] <- (
        new_data[[paste0(qdn, job, m, year)]] + 12 * new_data[[paste0(qdn, job, y, year)]])
      
      # If missing month or year, just use the other one
      new_data[[paste0('ldw_', job + 10,'_', year)]] <- ifelse(
        is.na(new_data[[paste0('ldw_', job + 10,'_', year)]]), 
        new_data[[paste0(qdn, job, m, year)]],
        new_data[[paste0('ldw_', job + 10,'_', year)]])
      
      new_data[[paste0('ldw_', job + 10,'_', year)]] <- ifelse(
        is.na(new_data[[paste0('ldw_', job + 10,'_', year)]]), 
        new_data[[paste0(qdn, job, y, year)]],
        new_data[[paste0('ldw_', job + 10,'_', year)]])
      
      # If missing everything but looped == 1, set = 1
      new_data[[paste0('ldw_', job + 10,'_', year)]] <- ifelse(
        (is.na(new_data[[paste0('ldw_', job + 10,'_', year)]]) & 
           new_data[[paste0('looped_', job + 10,'_', year)]] == 1),
        1, new_data[[paste0('ldw_', job + 10,'_', year)]])
    }
    
    # Set current job's ldw to big number for later
    if (paste0('current_', job + 10,'_', year) %in% colnames(new_data)){
      new_data[[paste0('ldw_', job + 10,'_', year)]] <- ifelse(
        new_data[[paste0('current_', job + 10,'_', year)]] == 1, 
        1e10, new_data[[paste0('ldw_', job + 10,'_', year)]])
    }
    
    # For contracted, assume that if loop started but contracted is NA,
    # then contracted = 0 (because the respondent was never asked this question,
    # pressumable because their job was a different type, ie temp work)
    if (paste0(qn, job, '.01_', year) %in% colnames(new_data)){
      new_data[[paste0('contracted_', job + 10,'_', year)]] <- pmax(
        new_data[[paste0(qn, job, '.01_', year)]],
        1 - new_data[[paste0('looped_', job + 10,'_', year)]], na.rm=T)
    }
    
    # For match flag, set 0 if looped = 1 (will later change to 1 if matched)
    # else 0
    new_data[[paste0('m_flag_', job + 10,'_', year)]] <- ifelse( 
      new_data[[paste0('looped_', job + 10,'_', year)]] == 1, 0, NA)
  }
}

# For 2012, can't find variable for if pli is current job, for now
# assume they are if looped == 1 and last date worked is NA
# For 2014, can't find variable for if still currently working job.
# For now, assume they are if looped == 1 and date last worked is NA
# 2016 had a different variable name for currently working (but job 5 missing)
# For 2016, sick days given by 'NB'
for (job in seq(1, 5, by=1)){
  new_data[[paste0('current_', job + 5, '_', 2012)]] <- ifelse(
    (new_data[[paste0('looped_', job + 5,'_', 2012)]] == 1 
     & is.na(new_data[[paste0('ldw_', job + 5,'_', 2012)]])), 1, 0
  )
  # Set these workers ldw to 1e10 for later
  new_data[[paste0('ldw_', job + 5,'_', 2012)]] <- ifelse(
    new_data[[paste0('current_', job + 5, '_', 2012)]] == 1, 1e10,
    new_data[[paste0('ldw_', job + 5,'_', 2012)]]
  )
  
  new_data[[paste0('current_', job, '_', 2014)]] <- ifelse(
    (new_data[[paste0('looped_', job,'_', 2014)]] == 1 
     & is.na(new_data[[paste0('ldw_', job,'_', 2014)]])), 1, 0
  )
  # Set these workers ldw to 1e10 for later
  new_data[[paste0('ldw_', job,'_', 2014)]] <- ifelse(
    new_data[[paste0('current_', job, '_', 2014)]] == 1, 1e10,
    new_data[[paste0('ldw_', job,'_', 2014)]]
  )
    
  if (job < 5){
    new_data[[paste0('current_', job,'_', 2016)]] <- new_data[[paste0(qcd2, job, '_', 2016)]]
    # Set these workers ldw to 1e10 for later
    new_data[[paste0('ldw_', job,'_', 2016)]] <- ifelse(
      new_data[[paste0('current_', job, '_', 2016)]] == 1, 1e10,
      new_data[[paste0('ldw_', job,'_', 2016)]]
    )
  }
  
  wide[[paste0('sick_', job,'_2016')]] <- new_data[[paste0(q84, 'NB.0', job, '_2016')]]
}


# For benefits, naming different in 2004
for (job in seq(1, 5, by=1)){
  for (ind in seq(1, 9, by=1)){
    
    # For 2004, use letter names
    wide[[paste0(ben[ind], job,'_', 2004)]] <- new_data[[paste0(q84, ben_name[ind], '.0', job, '_', 2004)]]
    
    # For 2006-2016, use numbers
    for (year in seq(2006, 2016, by=2)){
      wide[[paste0(ben[ind], job,'_', year)]] <- new_data[[paste0(q84, 'E.0', job, '~00000', ind, '_', year)]]
    }
  }
}

# Create an ever contracted variable (need pmax of 0 to deal with -Inf)
wide$ever_con <- pmax(0, apply(
  new_data[grep(paste0('contracted_.*_....'), names(new_data))], 1, max, na.rm=T))


# Need to match contracted data with rest of variables.
# To do this
# 1. Rank jobs by highest ldw (remember current jobs given big #)
# 2. If multiple current jobs, use ONJS to figure out which is #1. The rest
#     are ordered by dli -> pli -> nj and number
# Create con_flag if contracted but QES-25J != 2 
# (non-traditional in ES (don't know how consistent people are))

for (year in seq(2004, 2016, by=2)){
  # Record # of current jobs
  new_data[[paste0('num_cur_', year)]] <- apply(
    new_data[grep(paste0('current_.*_', year), names(new_data))], 1, sum, na.rm=T) 
  
  # Find last job(s) worked. Becasue of how ldw_ for current is defined,
  # this includes current job(s) if currently working
  new_data[[paste0('max_ldw_', year)]] <- apply(
    new_data[grep(paste0('ldw_.*_', year), names(new_data))], 1, max, na.rm=T)
  # Change -Inf to NA
  new_data[[paste0('max_ldw_', year)]] <- ifelse(
    new_data[[paste0('max_ldw_', year)]] > 0, new_data[[paste0('max_ldw_', year)]], NA)
  
  for (job in seq(1, 15, by=1)){
    if (paste0('ldw_', job, '_', year) %in% colnames(new_data)){
      new_data[[paste0('last_', job, '_', year)]] <- ifelse(
        new_data[[paste0('ldw_', job, '_', year)]] 
        == new_data[[paste0('max_ldw_', year)]], 1, 0)
      
      new_data[[paste0('last_', job, '_', year)]] <- ifelse(
        !is.na(new_data[[paste0('last_', job, '_', year)]]),
        new_data[[paste0('last_', job, '_', year)]], 0)
    }
  }
  
  for (per in seq(1, NROW(new_data), by=1)){
    count <- 1
    found <- 0
    for (job in seq(1, 15, by=1)){
      # If has ldw
      if (check(paste0('ldw_', job, '_', year), new_data, per)){
        # If worked multiple jobs, figure out which is #1
        if (check(paste0(onjs, year), new_data, per)){
          # For last jobs, find which is 1, list rest in order
          if (new_data[[paste0('last_', job, '_', year)]][per] == 1){
            if (count == new_data[[paste0(onjs, year)]][per] & found == 0){
              new_data[[paste0('rank_', job, '_', year)]][per] <- 1
              found <- 1
            } else {
              count <- count + 1
              new_data[[paste0('rank_', job, '_', year)]][per] <- count
            }
          }
        } else{ 
          # For jobs prior to last, find ranking by finding jobs worked after,
          # listed in order with ties
          rank <- 1
          for (comp in seq(1, 15, by=1)){
            # Check if comparison has ldw
            if (check(paste0('ldw_', comp, '_', year), new_data, per)){
              # Find those with later ldw or same ldw but listed earlier
              rank <- rank + 1 * (
                (new_data[[paste0('ldw_', comp, '_', year)]][per]
                 > new_data[[paste0('ldw_', job, '_', year)]][per]) |
                ((new_data[[paste0('ldw_', comp, '_', year)]][per]
                  == new_data[[paste0('ldw_', job, '_', year)]][per]) & (comp < job))
                )
            }
          }
          new_data[[paste0('rank_', job, '_', year)]][per] <- rank
        }
      }
    }
    for (job in seq(1, 5, by=1)){ # ES side
      for (match in seq(1, 15, by=1)){ # OJ side
        if (check(paste0('rank_', match, '_', year), new_data, per)){
          if (new_data[[paste0('rank_', match, '_', year)]][per] == job){ # Matched
            # Verify matched with m_flag
            new_data[[paste0('m_flag_', match,'_', year)]][per] <- 1
            wide[[paste0('m_flag_', job,'_', year)]][per] <- 1
            
            # Record if current job and num_cur
            if (check(paste0('current_', match, '_', year), new_data, per)){
              wide[[paste0('current_', job, '_', year)]][per] <- new_data[[paste0('current_', match, '_', year)]][per]
            }
            if (check(paste0('num_cur_', year), new_data, per)){
              wide[[paste0('num_cur_', job, '_', year)]][per] <- new_data[[paste0('num_cur_', year)]][per]
            }
            
            # Record if job contracted out
            if (check(paste0('contracted_', match, '_', year), new_data, per)){
              wide[[paste0('contracted_', job, '_', year)]][per] <- new_data[[paste0('contracted_', match, '_', year)]][per]
            }
          }
        }
      }
    }
  }
  # For these listed jobs, flag if contracted but not listed as non-traditional
  for (job in seq(1, 5, by=1)){
    wide[[paste0('con_flag_', job, '_', year)]] <- ifelse(
      (wide[[paste0('contracted_', job, '_', year)]] == 1) &
        (wide[[paste0('non_trad_', job,'_', year)]] != 1), 1, 0)
  }
}

# Now switch data from wide to long

# Variables that will vary over time/job
vary <- c('emp_id_', 'contracted_', 'job_sat_', 'health_', 'life_',
          'dental_', 'mat_', 'retirement_', 'flex_', 'profit_', 'train_', 'childcare_',
          'sick_', 'sick_vac_', 'vac_', 'non_trad_', 'con_flag_', 'current_', 'num_cur_',
          'm_flag')

# Variables that will stay constant over time/job
constant <- c('case_id', 'ever_con')

long <- wide %>%
  select(one_of(constant), matches(paste0('^(', paste(vary, collapse='|'), ')'))) %>%
  gather(matches(paste0('^(', paste(vary, collapse='|'), ')')), key=key, value=val) %>%
  extract(key, into=c('variable', 'key'), regex='(\\D+)(_._20..)') %>%
  filter(!is.na(variable), !is.na(key)) %>%
  spread(key=variable, value=val) %>%
  mutate(key = substring(key, 2))%>%
  separate(key, sep='_', into=c('job', 'year')) %>%
  drop_na(emp_id) %>%
  mutate(year = as.numeric(year), 
         job = as.numeric(job),
         # Combine sick and vac if sic_vac missing
         sick_vac = ifelse(is.na(sick_vac), pmax(sick, vac, sick + vac, na.rm=T), 
                           sick_vac),
         # Any benefits
         benefits = (pmax(health, life, dental, mat, retirement,
                          flex, profit, train, childcare, na.rm=T)),
         
         # If emp_id is from job pre-2002 and contracted is NA, set to 0
         # Do this because most of these jobs were never asked, assumed
         # not to be contract
         contracted = ifelse(is.na(contracted) & (emp_id < 2002 * 1e4), 0, contracted)
  ) %>%
  group_by(case_id) %>% 
  mutate(first = ifelse(year == min(year) & job == 1, 1, 0),
         ever_con_2 = ifelse(max(contracted, na.rm=T) == 1, 1, 0),
         ever_con_flag = ifelse(first == 1, ever_con - ever_con_2, 0)) %>%
  group_by(emp_id, add = T) %>% 
  mutate(contracted = ifelse(is.na(contracted), max(contracted, na.rm=T), contracted),
         # If this returns -Inf, set as NA
         contracted = ifelse(contracted < -1, NA, contracted),
         # Was this first or last day of job?
         start_job = ifelse(year == min(year), 1, 0),
         end_job = ifelse(year == max(year), 1, 0)) %>%
  ungroup() 


# Now merge in other data sets. Start with job history from Job History Stata
hist <- read.csv2('../Job History Stata/cleaned_job_hist.csv', sep = ',', header = T) 

hist[hist == -1] = NA  # Refused 
hist[hist == -2] = NA  # Dont know 
hist[hist == -3] = NA  # Invalid missing 
hist[hist == -4] = NA  # Valid missing 
hist[hist == -5] = NA  # Non-interview 

long <- inner_join(long, hist, by = c('case_id', 'emp_id', 'year')) 


# For each job, set industry to mode industry of job tenure (not occ, because 
# this might actually change)
long <- long %>% 
  group_by(case_id, emp_id) %>% 
  mutate(ind = Mode(ind, na.rm = T)[1]) %>% 
  ungroup()

# Merge in demographics
demo <- read.csv2('../Demographics/cleaned_demographics.csv', sep = ';', header = T)

long <- left_join(long, demo, by = c('case_id', 'year')) %>% 
  mutate(region = factor(region))

# Merge in marital status, number of children total and in household,
# and number of weeks worked last year (first year of each survey period)
m_k_w <- read.csv2('../Married Kids Weeks/cleaned_m_k_w.csv',
                   sep = ';', header = T)

long <- left_join(long, m_k_w, by = c('case_id', 'year'))

# Use FRED CPIAUCSL average of past 2 years to get real wages and earnings (in 2004 dollars)
cpi <- read.csv2('CPIAUCSL.csv', sep = ',', header = T)

cpi$CPI <- as.numeric(levels(cpi$CPI))[cpi$CPI]

long$log_real_wage <- NA
long$log_week_earn <- NA

for (year in seq(2004, 2016, by=2)){
  long <- long %>% 
    mutate(
      log_real_wage = ifelse(
        long$year == year, 
        ifelse(!is.na(hrly_wage) & hrly_wage != 0,
        log(hrly_wage * cpi$CPI[14] / cpi$CPI[year - 2002] / 100), NA), 
        log_real_wage),
      log_week_earn = ifelse(
        year == year,  
        ifelse(!is.na(hrly_wage) & hrly_wage != 0,
        log(hours_week * hrly_wage * cpi$CPI[14] / cpi$CPI[year - 2002] / 100), NA),
        log_week_earn)
    )
}

# If real wage below 3.3 (minimum wage in 2004 was 5.15, which is about 6.60 in 2016) 
# or above 400 set wage and earn to NA
# If hours_week is <0 or >80, set to NA
# Compare log real wage and log weekly earnings to occ average (based on gender)
# Creat part_time == 1 if hours_week < 35
# Create tenure_1, tenure_2 and tenure_3 (divide by 100 for regressions)
# Create an occ_cat of high level occupation categories to cluster data
# see https://usa.ipums.org/usa/volii/occ2000.shtml
# Create an ind_cat of high level industry categories to cluster data
# see https://usa.ipums.org/usa/volii/ind2000.shtml
# Create occ_ind for occ_cat overlap with ind_cat to cluster data
long <- long %>% 
  mutate(
    log_real_wage = ifelse(log_real_wage >= log(3.3) & log_real_wage <= log(400) &
                             hours_week > 0 & hours_week <= 80, log_real_wage, NA),
    log_week_earn = ifelse(log_real_wage >= log(3.3) & log_real_wage <= log(400) &
                             hours_week > 0 & hours_week <= 80, log_week_earn, NA),
    hours_week = ifelse(hours_week > 0 & hours_week <= 80, hours_week, NA),
    part_time = ifelse(hours_week < 35, 1, 0),
    tenure_1 = tenure / 100,
    tenure_2 = (tenure / 100) ^ 2,
    tenure_3 = (tenure / 100) ^ 3,
    occ_cat = 1 * (occ > 0) + 1 * (occ >= 3600) + 1 * (occ >= 4700) + 1 * (occ >= 6000) +
      1 * (occ >= 6200) + 1 * (occ >= 7700) + 1 * (occ >= 9800),
    ind_cat = 1 * (ind > 0) + 1 * (ind >= 370) + 1 * (ind >= 690) + 1 * (ind >= 770) +
      1 * (ind >= 1070) + 1 * (ind >= 4000) + 1 * (ind >= 4600) + 1 * (ind >= 6000) +
      1 * (ind >= 6400) + 1 * (ind >= 6800) + 1 * (ind >= 7200) + 1 * (ind >= 7800) +
      1 * (ind >= 8500) + 1 * (ind >= 8700) + 1 * (ind >= 9300) + 1 * (ind >= 9600), 
    occ_ind = occ_cat + ind_cat * 16
  ) %>%
  group_by(female, occ) %>% 
  mutate( 
    l_r_w_m_occ = ifelse(!is.na(log_real_wage),
                         log_real_wage - mean(log_real_wage, na.rm = T), NA),
    l_w_e_m_occ = ifelse(!is.na(log_week_earn),
                         log_week_earn - mean(log_week_earn, na.rm = T), NA)
  ) %>% 
  ungroup()

# Add weights. Weight currently is number of people represented (*100)
# Change to fraction of people represented by dividing by sum

weights <- read.table('customweight_nlsy79_5deeade7bfdfa.dat', sep = '')
colnames(weights) <- c('case_id', 'weight')
weights <- mutate(weights, weight = weight / sum(weight, na.rm=T))


long <- long %>% 
  left_join(weights, by = c('case_id'))

# Martin wants me to look into job transitions into and out of outsourcing
# Previousy created start_job and end_job for each job, use these to find job 
# before and after

# Make two temp matricies of case_id and emp_id shifted up and down to help do this
long <- long %>% 
  arrange(case_id, year, -job)

temp_shift_u <- rbind(data.frame(long$case_id, long$emp_id)[-1,], c(NA, NA))
temp_shift_d <- rbind(c(NA, NA), data.frame(long$case_id, long$emp_id)[-NROW(long),])

long <- long %>% 
  mutate(previous_job = ifelse(start_job == 1 & temp_shift_d$long.case_id == case_id,
                               temp_shift_d$long.emp_id, NA),
         next_job = ifelse(end_job == 1 & temp_shift_u$long.case_id == case_id,
                           temp_shift_u$long.emp_id, NA))

############################################################

# Create a function that finds difference of means and reports
# * if 10%, ** if 5%, and *** if 1% different
t_test <- function(data, var, obs, row_1, row_2){
  test <- tsum.test(mean.x=data[[var]][row_1],
                    s.x=data[[paste0(var, '_se')]][row_1] * sqrt(data[[obs]][row_1]),
                    n.x=data[[obs]][row_1],
                    mean.y=data[[var]][row_2],
                    s.y=data[[paste0(var, '_se')]][row_2] * sqrt(data[[obs]][row_2]),
                    n.y=data[[obs]][row_2])
  p <- test$p.value
  if (p < .01){
    stars <- '\\textsuperscript{***}'
  } else if (p < .05){
    stars <- '\\textsuperscript{**}'
  } else if (p < .1){
    stars <- '\\textsuperscript{*}'
  } else{
    stars <- ''
  }
  return(stars)
}

# Create a function that finds difference of proportions and reports
# * if 10%, ** if 5%, and *** if 1% different
p_test <- function(data, var, obs, row_1, row_2){
  test <- prop.test(x = c(data[[var]][row_1] * data[[obs]][row_1], 
                          data[[var]][row_2] * data[[obs]][row_2]),
                    n = c(data[[obs]][row_1], data[[obs]][row_2]), correct = FALSE)
  p <- test$p.value
  if (p < .01){
    stars <- '\\textsuperscript{***}'
  } else if (p < .05){
    stars <- '\\textsuperscript{**}'
  } else if (p < .1){
    stars <- '\\textsuperscript{*}'
  } else{
    stars <- ''
  }
  return(stars)
}

# Create a function that finds proportion test and reports
# * if 10%, ** if 5%, and *** if 1% different
p_test_1 <- function(data, var, obs, row){
  test <- prop.test(x = data[[var]][row] * data[[obs]][row],
                    n = data[[obs]][row], correct = FALSE)
  p <- test$p.value
  if (p < .01){
    stars <- '\\textsuperscript{***}'
  } else if (p < .05){
    stars <- '\\textsuperscript{**}'
  } else if (p < .1){
    stars <- '\\textsuperscript{*}'
  } else{
    stars <- ''
  }
  return(stars)
}

# In this section, create tables with summary statistics

# First, look at people who ever contract vs those who never do (ever_con == 1/0)
# Divide by men and women and weight based on population
# Look only at first observation for demographics
sum_demo <- long %>% 
  filter(first == 1) %>% 
  as_survey_design(ids = case_id, strata=samp_id, weights=weight) %>% 
  mutate(ever_con = factor(ever_con)) %>%
  group_by(female, ever_con) %>% 
  summarise(
    proportion = survey_mean(),
    black_per = survey_mean(black, na.rm = T),
    hispanic_per = survey_mean(hispanic, na.rm = T),
    less_hs_per = survey_mean(less_hs, na.rm = T),
    hs_dip_per = survey_mean(hs_dip, na.rm = T),
    aa_deg_per = survey_mean(aa_deg, na.rm = T),
    ba_deg_per = survey_mean(ba_deg, na.rm = T),
    single_per = survey_mean(single, na.rm = T),
    married_per = survey_mean(married, na.rm = T),
    tot_children_mean = survey_mean(tot_children, na.rm = T),
    hh_children_mean = survey_mean(hh_children, na.rm = T),
    n = unweighted(n())
    ) %>% 
  arrange(female, desc(ever_con)) 

# Create table in Latex
vars_d <- c('black', 'hispanic', 'less_hs', 'hs_dip', 'aa_deg', 'ba_deg', 
            'single', 'married')

vars_c <- c('tot', 'hh')

center_d <- rbind('Black', '', 'Hispanic', '', 'No HS Diploma', '', 'HS Diploma',
                  '',"AA Degree",'',"BA Degree +", '', 'Single', '', 'Married', '',
                  'Total Number', 'of Children', 'Children in', 'Household',
                  'Observations')


for (i in seq(1, NROW(sum_demo), by = 1)){
  col_i <- c()
  for (var in vars_d){
    var_n = paste0(var, '_per')
    se_n = paste0(var, '_per_se')
    if (i %% 2 > 0){
      col_i <- rbind(col_i,
                     paste0(' & ', format(round(sum_demo[[var_n]][i], 3), nsmall=3)),
                     paste0(' & (', format(round(sum_demo[[se_n]][i], 3), nsmall=3), ')'))
        }
    if (i %% 2 == 0){
      stars <- p_test(sum_demo, var_n, 'n', i, i - 1)
      col_i <- rbind(col_i,
                     paste0(' & ', format(round(sum_demo[[var_n]][i], 3), nsmall=3),
                            stars),
                     paste0(' & (', format(round(sum_demo[[se_n]][i], 3), nsmall=3), ')'))
    }
  }
  # Don't forget about total/hh children, which are means
  for (var in vars_c){
    var_n = paste0(var, '_children_mean')
    se_n = paste0(var, '_children_mean_se')
    if (i %% 2 > 0){
      col_i <- rbind(col_i,
                     paste0(' & ', format(round(sum_demo[[var_n]][i], 3), nsmall=3)),
                     paste0(' & (', format(round(sum_demo[[se_n]][i], 3), nsmall=3), ')'))
    }
    if (i %% 2 == 0){
      stars <- t_test(sum_demo, var_n, 'n', i, i - 1)
      col_i <- rbind(col_i,
                     paste0(' & ', format(round(sum_demo[[var_n]][i], 3), nsmall=3),
                            stars),
                     paste0(' & (', format(round(sum_demo[[se_n]][i], 3), nsmall=3), ')'))
    }
  }
  col_i <- rbind(col_i, 
                 paste0(' & {', format(sum_demo$n[i], big.mark = ',', trim = T), '}'))
  center_d <- cbind(center_d, col_i)
}

center_d <- cbind(center_d, 
                  rbind('\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                        '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                        '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                        '\\\\', '\\\\[2pt]', '\\\\'))

# Do weird stuff to create LaTeX output
j_folder <- paste0(tables, 'Junk/')
file_1 <- paste0(j_folder, 'center_d.txt')
file_2 <- paste0(j_folder, 'center_d_2.txt')
write.table(center_d, file_1, quote=T, col.names=F, row.names=F)
center_d <- read.table(file_1, sep = '')
write.table(center_d, file_2, quote=F, col.names=F, row.names=F, sep = '')
center_d <- readChar(file_2, nchars = 1e6)


top_d <- "\\documentclass[12pt]{article}
\\usepackage[margin=1in]{geometry}
\\usepackage{siunitx}
\\usepackage{booktabs}
\\begin{document}
\\begin{table}
\\centering
\\sisetup{
table-number-alignment = center,
table-figures-integer = 3,
table-figures-decimal = 3,
input-symbols=(),
table-space-text-post = \\textsuperscript{***},
table-align-text-post = false,
group-digits          = false
}
\\begin{tabular}{lSSSS}
\\toprule
& \\multicolumn{2}{c}{{Men}} & \\multicolumn{2}{c}{{Women}} \\\\
Variable & {Contracted} & {Never} & {Contracted} & {Never} \\\\ \\midrule
"

bot_d <- "\\bottomrule
\\end{tabular}
\\caption{Demographic statistics for men and women for those who work at least
one outsourced job versus those who never do. Observations are at the person level
and summary statistics are weighted.
Stars represent significant difference from ever outsourced at the .10 level *,
.05 level **, and .01 level ***.}
\\label{demo}
\\end{table}
\\end{document}"

write.table(paste0(top_d, center_d, bot_d),
            paste0(tables, 'NLSY79 Demographics/NLSY79 Demographics.tex'),
            quote=F, col.names=F, row.names=F, sep="")


# Now look at job characteristics for contracting jobs vs non-contracting jobs
sum_jobs_y <- long %>% 
  filter(!is.na(contracted)) %>% 
  as_survey_design(ids = case_id, strata=samp_id, weights=weight) %>% 
  mutate(contracted = factor(contracted)) %>% 
  group_by(female, contracted) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_wage_mean = survey_mean(log_real_wage, na.rm = T),
    log_week_earn_mean = survey_mean(log_week_earn, na.rm = T),
    l_r_w_m_occ_mean = survey_mean(l_r_w_m_occ, na.rm = T),
    l_w_e_m_occ_mean = survey_mean(l_w_e_m_occ, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    tenure_mean = survey_mean(tenure, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    weeks_worked_prev_mean = survey_mean(weeks_worked_prev, na.rm = T),
    sick_mean = survey_mean(sick, na.rm = T),
    vac_mean = survey_mean(vac, na.rm = T),
    sick_vac_mean = survey_mean(sick_vac, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    benefits_per = survey_mean(benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    retirement_per = survey_mean(retirement, na.rm = T),
    childcare_per = survey_mean(childcare, na.rm = T),
    dental_per = survey_mean(dental, na.rm = T),
    flex_per = survey_mean(flex, na.rm = T),
    life_per = survey_mean(life, na.rm = T),
    mat_per = survey_mean(mat, na.rm = T),
    profit_per = survey_mean(profit, na.rm = T),
    msa_per = survey_mean(msa, na.rm = T),
    msa_cc_per = survey_mean(msa_cc, na.rm = T),
    urban_per = survey_mean(urban, na.rm = T),
    single_per = survey_mean(single, na.rm = T),
    married_per = survey_mean(married, na.rm = T),
    tot_children_mean = survey_mean(tot_children, na.rm = T),
    hh_children_mean = survey_mean(hh_children, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(female, desc(contracted))


# This table is similar to the one above, but only looks at those who 
# were ever contracted out
sum_ever_jobs_y <- long %>% 
  filter(!is.na(contracted), ever_con == 1) %>% 
  as_survey_design(ids = case_id, weights=weight) %>% 
  mutate(contracted = factor(contracted)) %>% 
  group_by(female, contracted) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_wage_mean = survey_mean(log_real_wage, na.rm = T),
    log_week_earn_mean = survey_mean(log_week_earn, na.rm = T),
    l_r_w_m_occ_mean = survey_mean(l_r_w_m_occ, na.rm = T),
    l_w_e_m_occ_mean = survey_mean(l_w_e_m_occ, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    tenure_mean = survey_mean(tenure, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    weeks_worked_prev_mean = survey_mean(weeks_worked_prev, na.rm = T),
    sick_mean = survey_mean(sick, na.rm = T),
    vac_mean = survey_mean(vac, na.rm = T),
    sick_vac_mean = survey_mean(sick_vac, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    benefits_per = survey_mean(benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    retirement_per = survey_mean(retirement, na.rm = T),
    childcare_per = survey_mean(childcare, na.rm = T),
    dental_per = survey_mean(dental, na.rm = T),
    flex_per = survey_mean(flex, na.rm = T),
    life_per = survey_mean(life, na.rm = T),
    mat_per = survey_mean(mat, na.rm = T),
    profit_per = survey_mean(profit, na.rm = T),
    msa_per = survey_mean(msa, na.rm = T),
    msa_cc_per = survey_mean(msa_cc, na.rm = T),
    urban_per = survey_mean(urban, na.rm = T),
    single_per = survey_mean(single, na.rm = T),
    married_per = survey_mean(married, na.rm = T),
    tot_children_mean = survey_mean(tot_children, na.rm = T),
    hh_children_mean = survey_mean(hh_children, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(female, desc(contracted))

# Do the same as above except do summary at person-job level rather than
# person-job-year level

# Now look at job characteristics for contracting jobs vs non-contracting jobs
# Create long_jobs which groups data by jobs, potentially useful below
long_jobs <- long %>% 
  group_by(case_id, emp_id) %>% 
  summarise(
    female = mean(female, na.rm = T),
    contracted = ifelse(max(contracted, na.rm = T) >= 0, max(contracted, na.rm = T), NA),
    ever_con = max(ever_con, na.rm = T),
    samp_id = mean(samp_id, na.rm = T),
    weight = mean(weight, na.rm = T),
    log_real_wage = mean(log_real_wage, na.rm = T),
    log_week_earn = mean(log_week_earn, na.rm = T),
    hours_week = mean(hours_week, na.rm = T),
    part_time = mean(part_time, na.rm = T),
    tenure = mean(tenure, na.rm = T),
    job_sat = mean(job_sat, na.rm = T),
    weeks_worked_prev = mean(weeks_worked_prev, na.rm = T),
    sick = mean(sick, na.rm = T),
    vac = mean(vac, na.rm = T),
    sick_vac = mean(sick_vac, na.rm = T),
    union = mean(union, na.rm = T),
    benefits = mean(benefits, na.rm = T),
    health = mean(health, na.rm = T),
    retirement = mean(retirement, na.rm = T),
    childcare = mean(childcare, na.rm = T),
    dental = mean(dental, na.rm = T),
    flex = mean(flex, na.rm = T),
    life = mean(life, na.rm = T),
    mat = mean(mat, na.rm = T),
    profit = mean(profit, na.rm = T),
    msa = mean(msa, na.rm = T),
    msa_cc = mean(msa_cc, na.rm = T),
    urban = mean(urban, na.rm = T),
    single = mean(single, na.rm = T),
    married = mean(married, na.rm = T),
    tot_children = mean(tot_children, na.rm = T),
    hh_children = mean(hh_children, na.rm = T),
    occ = Mode(occ, na.rm = T)[1],
    ind = Mode(ind, na.rm = T)[1],
    previous_job = ifelse(max(previous_job, na.rm = T) >= 0,
                          max(previous_job, na.rm = T), NA),
    next_job = ifelse(max(next_job, na.rm = T) >= 0,
                      max(next_job, na.rm = T), NA)
  ) 

sum_jobs_j <- long_jobs %>%  
  filter(!is.na(contracted)) %>% 
  as_survey_design(ids = case_id, strata=samp_id, weights=weight) %>% 
  mutate(contracted = factor(contracted)) %>% 
  group_by(female, contracted) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_wage_mean = survey_mean(log_real_wage, na.rm = T),
    log_week_earn_mean = survey_mean(log_week_earn, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    tenure_mean = survey_mean(tenure, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    weeks_worked_prev_mean = survey_mean(weeks_worked_prev, na.rm = T),
    sick_mean = survey_mean(sick, na.rm = T),
    vac_mean = survey_mean(vac, na.rm = T),
    sick_vac_mean = survey_mean(sick_vac, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    benefits_per = survey_mean(benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    retirement_per = survey_mean(retirement, na.rm = T),
    childcare_per = survey_mean(childcare, na.rm = T),
    dental_per = survey_mean(dental, na.rm = T),
    flex_per = survey_mean(flex, na.rm = T),
    life_per = survey_mean(life, na.rm = T),
    mat_per = survey_mean(mat, na.rm = T),
    profit_per = survey_mean(profit, na.rm = T),
    msa_per = survey_mean(msa, na.rm = T),
    msa_cc_per = survey_mean(msa_cc, na.rm = T),
    urban_per = survey_mean(urban, na.rm = T),
    single_per = survey_mean(single, na.rm = T),
    married_per = survey_mean(married, na.rm = T),
    tot_children_mean = survey_mean(tot_children, na.rm = T),
    hh_children_mean = survey_mean(hh_children, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(female, desc(contracted))


# This table is similar to the one above, but only looks at those who 
# were ever contracted out
sum_ever_jobs_j <- long_jobs %>% 
  filter(!is.na(contracted), ever_con == 1) %>% 
  as_survey_design(ids = case_id, weights=weight) %>% 
  mutate(contracted = factor(contracted)) %>% 
  group_by(female, contracted) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_wage_mean = survey_mean(log_real_wage, na.rm = T),
    log_week_earn_mean = survey_mean(log_week_earn, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    tenure_mean = survey_mean(tenure, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    weeks_worked_prev_mean = survey_mean(weeks_worked_prev, na.rm = T),
    sick_mean = survey_mean(sick, na.rm = T),
    vac_mean = survey_mean(vac, na.rm = T),
    sick_vac_mean = survey_mean(sick_vac, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    benefits_per = survey_mean(benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    retirement_per = survey_mean(retirement, na.rm = T),
    childcare_per = survey_mean(childcare, na.rm = T),
    dental_per = survey_mean(dental, na.rm = T),
    flex_per = survey_mean(flex, na.rm = T),
    life_per = survey_mean(life, na.rm = T),
    mat_per = survey_mean(mat, na.rm = T),
    profit_per = survey_mean(profit, na.rm = T),
    msa_per = survey_mean(msa, na.rm = T),
    msa_cc_per = survey_mean(msa_cc, na.rm = T),
    urban_per = survey_mean(urban, na.rm = T),
    single_per = survey_mean(single, na.rm = T),
    married_per = survey_mean(married, na.rm = T),
    tot_children_mean = survey_mean(tot_children, na.rm = T),
    hh_children_mean = survey_mean(hh_children, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(female, desc(contracted))

# Create tables for both types

top_j <- "\\documentclass[12pt]{article}
\\usepackage[margin=.5in]{geometry}
\\usepackage{siunitx}
\\usepackage{booktabs}
\\begin{document}
\\begin{table}
\\footnotesize
\\centering
\\sisetup{
table-number-alignment = center,
table-figures-integer = 3,
table-figures-decimal = 3,
input-symbols=(),
table-space-text-post = \\textsuperscript{***},
table-align-text-post = false,
group-digits          = false
}
\\begin{tabular}{lSSSSSS}
\\toprule
& \\multicolumn{3}{c}{{Men}} & \\multicolumn{3}{c}{{Women}} \\\\
& {Contracted} & {Non-Contracted} & {Non-Contracted} & {Contracted} & {Non-Contracted} & {Non-Contracted} \\\\ 
Variable &  & {(All)} & {(Ever Contracted)} &  & {(All)} & {(Ever Contracted)} \\\\ \\midrule
"

bot_y <- "\\bottomrule
\\end{tabular}
\\caption{Job statistics for men and women. The three categories are outsourced 
jobs, all non-outsourced jobs, and non-outsourced jobs for people who ever had
an outsourced job. Observations are at the person-job-year level and summary 
statistics are weighted at the person level. All dollar figures are in 2004 dollars. 
Stars represent significant difference from outsourced jobs at the .10 level *, 
.05 level **, and .01 level ***.}
\\label{jobs_y}
\\end{table}
\\end{document}"


bot_j <- "\\bottomrule
\\end{tabular}
\\caption{Job statistics for men and women. The three categories are outsourced 
jobs, all non-outsourced jobs, and non-outsourced jobs for people who ever had
an outsourced job. Observations are at the person-job level (jobs with mulitple 
years of observations have average characteristics of all years) and summary 
statistics are weighted at the person level. All dollar figures are in 2004 dollars. 
Stars represent significant difference from outsourced jobs at the .10 level *, 
.05 level **, and .01 level ***.}
\\label{jobs_j}
\\end{table}
\\end{document}"

# Create table in Latex
vars_j <- c('log_real_wage', 'log_week_earn', 'hours_week', 'part_time', 'tenure', 
            'union', 'job_sat', 'weeks_worked_prev', 'benefits', 'health',
            'retirement', 'childcare', 'dental', 'flex', 'life', 'mat', 'profit',
            'msa', 'single', 'married', 'tot_children', 'hh_children')

# Divide variables by mean or percent (they are different below)
vars_m_j <- c('log_real_wage', 'log_week_earn', 'hours_week', 'tenure', 'job_sat',
              'weeks_worked_prev', 'tot_children', 'hh_children')

vars_p_j <- c('part_time', 'union', 'benefits', 'health', 'retirement', 'childcare',
              'dental', 'flex', 'life', 'mat', 'profit', 'msa', 'single', 'married')


for (loop in c(1, 2)){
  # Set up parameters
  if (loop == 1){
    sum_jobs <- sum_jobs_y
    sum_ever_jobs <- sum_ever_jobs_y
    bot_v <- bot_y
    name <- 'Year'
  } else{
    sum_jobs <- sum_jobs_j
    sum_ever_jobs <- sum_ever_jobs_j
    bot_v <- bot_j
    name <- 'Jobs'
    
  }
  
  center_j <- rbind('Log Real', 'Hourly Wage', 'Log Weekly', 'Earnings', 'Hours Worked', 
                  'Weekly', 'Part Time', '', 'Tenure', '(Weeks)', 'Union', '',
                  "Job Satisfaction", '(Lower Better)', 'Weeks Worked', 'Previous Year',
                  'Any Benefits', '',
                  'Health', 'Insurance', 'Retirement', 'Plan', "Subsidized", 'Childcare', 
                  'Dental', 'Benefits', 'Flexible', 'Schedule', 'Life', 'Insurance', 
                  'Maternity', 'Leave', 'Profit', 'Sharing', 'MSA', '', 'Single', '',
                  'Married', '', 'Total Number', 'of Children', 'Children in', 
                  'Household', 'Observations')
  
  
  for (i in seq(1, NROW(sum_jobs) + 2, by = 1)){
    col_i <- c()
    if (i %% 3 > 0){
      data <- sum_jobs
      j <- i - (i > 3)
    } else{
      data <- sum_ever_jobs
      j <- i - (i > 0) - (i > 3)
    }
    for (var in vars_j){
      if (var %in% vars_p_j){
        var_n = paste0(var, '_per')
        se_n = paste0(var, '_per_se')
        if (j %% 2 == 0){
          stars <- p_test(data, var_n, 'n', j, j - 1)
        }
      } else{
        var_n = paste0(var, '_mean')
        se_n = paste0(var, '_mean_se')
        if (j %% 2 == 0){
          stars <- t_test(data, var_n, 'n', j, j - 1)
        }
      }
      if (j %% 2 > 0){
        col_i <- rbind(col_i,
                       paste0(' & ', format(round(data[[var_n]][j], 3), nsmall=3)),
                       paste0(' & (', format(round(data[[se_n]][j], 3), nsmall=3), ')'))
      }
      if (j %% 2 == 0){
        col_i <- rbind(col_i,
                       paste0(' & ', format(round(data[[var_n]][j], 3), nsmall=3),
                              stars),
                       paste0(' & (', format(round(data[[se_n]][j], 3), nsmall=3), ')'))
      }
    }
    col_i <- rbind(col_i, 
                   paste0(' & {', format(data$n[j], big.mark = ',', trim = T), '}'))
    center_j <- cbind(center_j, col_i)
  }
  
  center_j <- cbind(center_j, 
                    rbind('\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                          '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                          '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                          '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                          '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                          '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                          '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                          '\\\\', '\\\\[2pt]', '\\\\'))
  
  # Do weird stuff to create LaTeX output
  j_folder <- paste0(tables, 'Junk/')
  file_3 <- paste0(j_folder, 'center_j.txt')
  file_4 <- paste0(j_folder, 'center_j_2.txt')
  write.table(center_j, file_3, quote=T, col.names=F, row.names=F)
  center_j <- read.table(file_3, sep = '')
  write.table(center_j, file_4, quote=F, col.names=F, row.names=F, sep = '')
  center_j <- readChar(file_4, nchars = 1e6)
  
  
  write.table(paste0(top_j, center_j, bot_v),
              paste0(tables, 'NLSY79 Jobs/NLSY79 Jobs ', name, '.tex'),
              quote=F, col.names=F, row.names=F, sep="")
}


###########################################################################


  
# Now summarise each job and look at the traights of the job before and
# after
# Merge to get previous and next jobs
job_transitions <- long_jobs %>% 
  left_join(long_jobs, 
            by = c("case_id" = "case_id", "previous_job" = "emp_id"),
            suffix = (c("" , "_prev"))) %>% 
  left_join(long_jobs, 
            by = c("case_id" = "case_id", "next_job" = "emp_id"),
            suffix = (c("" , "_next")))

# Sumarise job transitions for contracted and not contracted
# (I also rearrange dataset to make it look like past ones,
# this makes making tables easier)
constant <- c('female', 'contracted', 'n')

job_transitions_sum <- job_transitions %>% 
  filter(!is.na(contracted)) %>% 
  as_survey_design(ids = case_id, weights=weight) %>% 
  mutate(contracted = factor(contracted)) %>% 
  group_by(female, contracted) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_wage_mean = survey_mean(log_real_wage, na.rm = T),
    log_week_earn_mean = survey_mean(log_week_earn, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    benefits_per = survey_mean(benefits, na.rm = T),
    contracted_prev_per = survey_mean(contracted_prev, na.rm = T),
    log_real_wage_prev_mean = survey_mean(log_real_wage_prev, na.rm = T),
    log_week_earn_prev_mean = survey_mean(log_week_earn_prev, na.rm = T),
    hours_week_prev_mean = survey_mean(hours_week_prev, na.rm = T),
    part_time_prev_per = survey_mean(part_time_prev, na.rm = T),
    job_sat_prev_mean = survey_mean(job_sat_prev, na.rm = T),
    union_prev_per = survey_mean(union_prev, na.rm = T),
    benefits_prev_per = survey_mean(benefits_prev, na.rm = T),
    same_occ_prev_per = survey_mean(occ == occ_prev, na.rm = T),
    same_ind_prev_per = survey_mean(ind == ind_prev, na.rm = T),
    contracted_next_per = survey_mean(contracted_next, na.rm = T),
    log_real_wage_next_mean = survey_mean(log_real_wage_next, na.rm = T),
    log_week_earn_next_mean = survey_mean(log_week_earn_next, na.rm = T),
    hours_week_next_mean = survey_mean(hours_week_next, na.rm = T),
    part_time_next_per = survey_mean(part_time_next, na.rm = T),
    job_sat_next_mean = survey_mean(job_sat_next, na.rm = T),
    union_next_per = survey_mean(union_next, na.rm = T),
    benefits_next_per = survey_mean(benefits_next, na.rm = T),
    same_occ_next_per = survey_mean(occ == occ_next, na.rm = T),
    same_ind_next_per = survey_mean(ind == ind_next, na.rm = T),
    n = unweighted(n())
  ) %>% 
  select(
    female, contracted, n,
    log_real_wage_mean_curr = log_real_wage_mean,
    log_real_wage_mean_se_curr = log_real_wage_mean_se,
    log_week_earn_mean_curr = log_week_earn_mean,
    log_week_earn_mean_se_curr = log_week_earn_mean_se,
    hours_week_mean_curr = hours_week_mean,
    hours_week_mean_se_curr = hours_week_mean_se,  
    part_time_per_curr = part_time_per,
    part_time_per_se_curr = part_time_per_se,  
    job_sat_mean_curr = job_sat_mean,  
    job_sat_mean_se_curr = job_sat_mean_se,
    union_per_curr = union_per,
    union_per_se_curr = union_per_se,
    benefits_per_curr = benefits_per,
    benefits_per_se_curr = benefits_per_se,
    log_real_wage_mean_prev = log_real_wage_prev_mean,
    log_real_wage_mean_se_prev = log_real_wage_prev_mean_se,
    log_week_earn_mean_prev = log_week_earn_prev_mean,
    log_week_earn_mean_se_prev = log_week_earn_prev_mean_se,
    hours_week_mean_prev = hours_week_prev_mean,
    hours_week_mean_se_prev = hours_week_prev_mean_se,   
    part_time_per_prev = part_time_prev_per,
    part_time_per_se_prev = part_time_prev_per_se,
    job_sat_mean_prev = job_sat_prev_mean,  
    job_sat_mean_se_prev = job_sat_prev_mean_se,
    union_per_prev = union_prev_per,
    union_per_se_prev = union_prev_per_se,
    benefits_per_prev = benefits_prev_per,
    benefits_per_se_prev = benefits_prev_per_se,
    contracted_per_prev = contracted_prev_per,
    contracted_per_se_prev = contracted_prev_per_se,
    same_occ_per_prev = same_occ_prev_per,
    same_occ_per_se_prev = same_occ_prev_per_se,
    same_ind_per_prev = same_ind_prev_per,
    same_ind_per_se_prev = same_ind_prev_per_se,
    log_real_wage_mean_next = log_real_wage_next_mean,
    log_real_wage_mean_se_next = log_real_wage_next_mean_se,
    log_week_earn_mean_next = log_week_earn_next_mean,
    log_week_earn_mean_se_next = log_week_earn_next_mean_se, 
    part_time_per_next = part_time_next_per,
    part_time_per_se_next = part_time_next_per_se,
    hours_week_mean_next = hours_week_next_mean,
    hours_week_mean_se_next = hours_week_next_mean_se,  
    job_sat_mean_next = job_sat_next_mean,  
    job_sat_mean_se_next = job_sat_next_mean_se,
    union_per_next = union_next_per,
    union_per_se_next = union_next_per_se,
    benefits_per_next = benefits_next_per,
    benefits_per_se_next = benefits_next_per_se,
    contracted_per_next = contracted_next_per,
    contracted_per_se_next = contracted_next_per_se,
    same_occ_per_next = same_occ_next_per,
    same_occ_per_se_next = same_occ_next_per_se,
    same_ind_per_next = same_ind_next_per,
    same_ind_per_se_next = same_ind_next_per_se
  ) %>% 
  gather(key=key, value=val, -constant) %>% 
  mutate(
    var = substring(key, 1, nchar(key) - 5),
    time = substring(key, nchar(key) - 3)) %>%
  select(-key) %>% 
  filter(!is.na(var), !is.na(time)) %>%
  spread(key=var, value=val) %>%
  arrange(female, desc(contracted)) 
  
top_t <- "\\documentclass[12pt]{article}
\\usepackage[margin=.5in]{geometry}
\\usepackage{siunitx}
\\usepackage{booktabs}
\\begin{document}
\\begin{table}
\\footnotesize
\\centering
\\sisetup{
table-number-alignment = center,
table-figures-integer = 3,
table-figures-decimal = 3,
input-symbols=(),
table-space-text-post = \\textsuperscript{***},
table-align-text-post = false,
group-digits          = false
}
\\begin{tabular}{lSSSSSS}
\\toprule
& \\multicolumn{6}{c}{{Men}} \\\\
& \\multicolumn{3}{c}{{Contracted Currently}} & \\multicolumn{3}{c}{{Non-Contracted Currently}} \\\\
& {Previous} & {Current} & {Next} & {Previous} & {Current} & {Next} \\\\  \\midrule
"

top_w_t <- " \\midrule
& \\multicolumn{6}{c}{{Women}} \\\\
& \\multicolumn{3}{c}{{Contracted Currently}} & \\multicolumn{3}{c}{{Non-Contracted Currently}} \\\\
& {Previous} & {Current} & {Next} & {Previous} & {Current} & {Next} \\\\  \\midrule
"

bot_t <- "\\bottomrule
\\end{tabular}
\\caption{Job statistics for men and women at current, previous, and next job for workers
who are currently contracted out compared to those who are not. Observations are at the 
person-job level (jobs with mulitple years of observations have average characteristics
of all years) and summary statistics are weighted at the person level. 
All dollar figures are in 2004 dollars. Stars represent significant difference from 
current job (except for contracted, same occupation, and same industry, which represent
significant difference from 0) at the .10 level *, .05 level **, and .01 level ***.}
\\label{jobs_t}
\\end{table}
\\end{document}"

# Create table in Latex
vars_t <- c('log_real_wage', 'log_week_earn', 'hours_week', 'part_time',
            'union', 'job_sat', 'benefits')

# Divide variables by mean or percent (they are different below)
vars_m_t <- c('log_real_wage', 'log_week_earn', 'hours_week', 'job_sat')

vars_p_t <- c('part_time', 'union', 'benefits')

# Also check if new job has same occ/ind
vars_o_t <- c('occ', 'ind')


center_t <- rbind('Contracted', 'Out', 'Same', 'Occupation', 'Same', 'Industry',
                  'Log Real', 'Hourly Wage', 'Log Weekly', 'Earnings', 'Hours Worked', 
                  'Weekly', 'Part Time', '', 'Union', '',"Job Satisfaction",  
                  '(Lower Better)', 'Any Benefits', '', 'Observations')

for (sex in c(1, 7)){
  for (con in c(0, 3)){
    
    i <- sex + con
    i_n <- sex + con + 1
    i_p <- sex + con + 2
  
    # Start with contracted 
    temp <- cbind(
      rbind(
        paste0(' & ', format(round(job_transitions_sum$contracted_per[i_p], 3), nsmall=3),
               p_test_1(job_transitions_sum, 'contracted_per', 'n', i_p)),
        paste0(' & (', format(round(job_transitions_sum$contracted_per_se[i_p], 3), nsmall=3), ')')
        ),
      rbind( 
        paste0(' & ', format(round(1 - (con / 3), 3), nsmall=3)),
        paste0(' & {--} ')
        ),
      rbind(
        paste0(' & ', format(round(job_transitions_sum$contracted_per[i_n], 3), nsmall=3),
               p_test_1(job_transitions_sum, 'contracted_per', 'n', i_n)),
        paste0(' & (', format(round(job_transitions_sum$contracted_per_se[i_n], 3), nsmall=3), ')')
      )
    )
    
    # Now do same occ/ind
    for (var in vars_o_t){
      var_n <- paste0('same_', var, '_per')
      se_n <- paste0('same_', var, '_per_se')
      temp <- rbind(temp,
        cbind(
          rbind(
            paste0(' & ', format(round(job_transitions_sum[[var_n]][i_p], 3), nsmall=3),
                   p_test_1(job_transitions_sum, var_n, 'n', i_p)),
            paste0(' & (', format(round(job_transitions_sum[[se_n]][i_p], 3), nsmall=3), ')')
          ),
          rbind(paste0(' &  {--} '), paste0(' & ')),
          rbind(
            paste0(' & ', format(round(job_transitions_sum[[var_n]][i_n], 3), nsmall=3),
                   p_test_1(job_transitions_sum, var_n, 'n', i_n)),
            paste0(' & (', format(round(job_transitions_sum[[se_n]][i_n], 3), nsmall=3), ')')
          )
        )
      )
    }
    
    # Now for everything else
    for (var in vars_t){
      if (var %in% vars_p_t){
        var_n = paste0(var, '_per')
        se_n = paste0(var, '_per_se')
        p_star <- p_test(job_transitions_sum, var_n, 'n', i, i_p)
        n_star <- p_test(job_transitions_sum, var_n, 'n', i, i_n)
      } else{
        var_n = paste0(var, '_mean')
        se_n = paste0(var, '_mean_se')
        p_star <- t_test(job_transitions_sum, var_n, 'n', i, i_p)
        n_star <- t_test(job_transitions_sum, var_n, 'n', i, i_n)
      }
      temp <- rbind(temp,
        cbind(
          rbind(
            paste0(' & ', format(round(job_transitions_sum[[var_n]][i_p], 3), nsmall=3),
                   p_star),
            paste0(' & (', format(round(job_transitions_sum[[se_n]][i_p], 3), nsmall=3), ')')
          ),
          rbind( 
            paste0(' & ', format(round(job_transitions_sum[[var_n]][i], 3), nsmall=3)),
            paste0(' & (', format(round(job_transitions_sum[[se_n]][i], 3), nsmall=3), ')')
          ),
          rbind(
            paste0(' & ', format(round(job_transitions_sum[[var_n]][i_n], 3), nsmall=3),
                   n_star),
            paste0(' & (', format(round(job_transitions_sum[[se_n]][i_n], 3), nsmall=3), ')')
          )
        )
      )
    }
    
    temp <- rbind(
      temp,
      cbind(' & ', 
            paste0(' & {', format(job_transitions_sum$n[i], big.mark = ',', trim = T), '}'), ' & '))
  
    if (con == 0){
      temp_c <- cbind(center_t, temp)
    } else{
      temp_nc <- cbind(temp_c, temp)
      if (sex == 1){
        center_m_t <- cbind(
          temp_nc, 
          rbind('\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                '\\\\', '\\\\[2pt]', '\\\\'))
      } else{
        center_w_t <- cbind(
          temp_nc, 
          rbind('\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                '\\\\', '\\\\[2pt]', '\\\\'))
      }
    }
  
  }  
}

  
# Do weird stuff to create LaTeX output
t_folder <- paste0(tables, 'Junk/')
file_5_m <- paste0(t_folder, 'center_t_m.txt')
file_6_m <- paste0(t_folder, 'center_t_m_2.txt')
file_5_w <- paste0(t_folder, 'center_t_w.txt')
file_6_w <- paste0(t_folder, 'center_t_w_2.txt')
write.table(center_m_t, file_5_m, quote=T, col.names=F, row.names=F)
center_m_t <- read.table(file_5_m, sep = '')
write.table(center_m_t, file_6_m, quote=F, col.names=F, row.names=F, sep = '')
center_m_t <- readChar(file_6_m, nchars = 1e6)
write.table(center_w_t, file_5_w, quote=T, col.names=F, row.names=F)
center_w_t <- read.table(file_5_w, sep = '')
write.table(center_w_t, file_6_w, quote=F, col.names=F, row.names=F, sep = '')
center_w_t <- readChar(file_6_w, nchars = 1e6)


write.table(paste0(top_t, center_m_t, top_w_t, center_w_t, bot_t),
            paste0(tables, 'NLSY79 Job Transitions/NLSY79 Job Transitions.tex'),
            quote=F, col.names=F, row.names=F, sep="")



############################################################################

# Create an excel file with all industries and occupations that are outsourced
out_ind <- long %>% 
  filter(!is.na(contracted)) %>% 
  select(case_id, emp_id, ind, contracted) %>% 
  distinct() %>% 
  group_by(ind, contracted) %>% 
  summarise(number = n()) %>% 
  spread(contracted, number) %>% 
  rename(not = `0`, outsourced = `1`) %>% 
  drop_na(outsourced, ind) %>% 
  ungroup() %>% 
  mutate(per_out = round(outsourced / (outsourced + not) * 100, 0))

# For occ, also look if workers have wages above or below occ average
out_occ <- long %>% 
  filter(!is.na(contracted)) %>% 
  group_by(case_id, emp_id) %>% 
  mutate(
    l_r_w_m_above = ifelse(mean(l_r_w_m_occ, na.rm = T) > -1e10,
                         mean(l_r_w_m_occ, na.rm = T) > 0, NA)
  ) %>% 
  select(case_id, emp_id, occ, contracted, l_r_w_m_above) %>% 
  distinct() %>% 
  group_by(occ, contracted) %>% 
  summarise(
    number = n(),
    l_r = mean(l_r_w_m_above, na.rm = T)
  ) %>% 
  gather(variable, value, -(occ:contracted)) %>% 
  unite(temp, contracted, variable) %>% 
  spread(temp, value) %>% 
  rename(not = `0_number`, outsourced = `1_number`,
         not_per_above = `0_l_r`, outsourced_per_above = `1_l_r`) %>% 
  drop_na(outsourced, occ, outsourced_per_above) %>% 
  mutate(
    not = ifelse(is.na(not), 0, not),
    per_out = round(outsourced / (outsourced + not) * 100, 0),
    per_above_occ = round(outsourced_per_above * 100)
  ) %>% 
  ungroup() %>% 
  select(occ, not, outsourced, per_out, per_above_occ)

l <- list("Ind" = out_ind, "Occ" = out_occ)
write.xlsx(l, file="../Ind_Occ/Ind_Occ.xlsx")

# Write a table with some results
out_ind_sum <- out_ind %>% 
  summarise(
    obs = n(),
    per_out_ind = mean(per_out),
    per_out_total = 100 * sum(outsourced) / (sum(outsourced) + sum(not)),
    over_ten_per = mean(per_out >= 10),
    max_out = max(per_out))

out_occ_sum <- out_occ %>% 
  summarise(
    obs = n(),
    per_out_occ = mean(per_out),
    per_out_total = 100 * sum(outsourced) / (sum(outsourced) + sum(not)),
    per_above_occ_per = mean(per_above_occ),
    over_ten_per = mean(per_out >= 10),
    max_out = max(per_out))


################################################################################

# # Using Ind_Occ_List, see how well the ind/occ method
# # of Abraham (1990) at citers compares to ours

# From Dube and Kaplan (2010), look at janitors and security guards
janitor <- long %>%
  filter(occ == 4220, start_job == 1) %>%
  mutate(
    contracted_2 = 1 * (ind == 7690),
    con_1_v_2 = contracted - contracted_2
  )

sg <- long %>%
  filter(occ == 3920, start_job == 1) %>%
  mutate(
    contracted_2 = 1 * (ind == 7680),
    con_1_v_2 = contracted - contracted_2
  )
# 
# # Ind 6170 Truck Transportaion and Occ 9130 Driver/Sales Workers and Truck Drivers
# truck <- long %>% 
#   filter(occ == 9130, start_job == 1) %>% 
#   mutate(
#     contracted_2 = 1 * (ind == 6170),
#     con_1_v_2 = contracted - contracted_2
#   )
# 
# # maids
# maids <- long %>% 
#   filter(occ == 4230, start_job == 1) %>% 
#   mutate(
#     contracted_2 = 1 * (ind == 7690),
#     con_1_v_2 = contracted - contracted_2
#   )
# 
# # Management analysts
# ma <- long %>% 
#   filter(occ == 710, start_job == 1) %>% 
#   mutate(
#     contracted_2 = 1 * (ind == 7390),
#     con_1_v_2 = contracted - contracted_2
#   )
# 
# # Customer Service Representatives
# csr <- long %>% 
#   filter(occ == 5240, start_job == 1) %>% 
#   mutate(
#     contracted_2 = 1 * (ind == 7590),
#     con_1_v_2 = contracted - contracted_2
#   )

# Compare contracted vs not for janitors and security guards based on
# self-reported contracted and Dube and Kaplan's measure. See
# If they are significantly different

# Janitors
# Dube Kaplan
janitor_dk_sum <- janitor %>%  
  filter(!is.na(contracted), !is.na(contracted_2)) %>% 
  as_survey_design(ids = case_id, weights=weight) %>% 
  mutate(contracted = factor(contracted_2)) %>% 
  group_by(contracted) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_wage_mean = survey_mean(log_real_wage, na.rm = T),
    log_week_earn_mean = survey_mean(log_week_earn, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    benefits_per = survey_mean(benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    female_per = survey_mean(female, na.rm = T),
    black_per = survey_mean(black, na.rm = T),
    hispanic_per = survey_mean(hispanic, na.rm = T),
    less_hs_per = survey_mean(less_hs, na.rm = T),
    hs_dip_per = survey_mean(hs_dip, na.rm = T),
    aa_deg_per = survey_mean(aa_deg, na.rm = T),
    ba_deg_per = survey_mean(ba_deg, na.rm = T),
    age_mean = survey_mean(age, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(desc(contracted)) 

# Self reported (add DK to end for table)
janitor_sr_sum <- janitor %>% 
  filter(!is.na(contracted), !is.na(contracted_2)) %>% 
  as_survey_design(ids = case_id, weights=weight) %>% 
  mutate(contracted = factor(contracted)) %>% 
  group_by(contracted) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_wage_mean = survey_mean(log_real_wage, na.rm = T),
    log_week_earn_mean = survey_mean(log_week_earn, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    benefits_per = survey_mean(benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    female_per = survey_mean(female, na.rm = T),
    black_per = survey_mean(black, na.rm = T),
    hispanic_per = survey_mean(hispanic, na.rm = T),
    less_hs_per = survey_mean(less_hs, na.rm = T),
    hs_dip_per = survey_mean(hs_dip, na.rm = T),
    aa_deg_per = survey_mean(aa_deg, na.rm = T),
    ba_deg_per = survey_mean(ba_deg, na.rm = T),
    age_mean = survey_mean(age, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(desc(contracted)) %>% 
  bind_rows(janitor_dk_sum)

# Security Guards
# Dube Kaplan
sg_dk_sum <- sg %>%  
  filter(!is.na(contracted), !is.na(contracted_2)) %>% 
  as_survey_design(ids = case_id, weights=weight) %>% 
  mutate(contracted = factor(contracted_2)) %>% 
  group_by(contracted) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_wage_mean = survey_mean(log_real_wage, na.rm = T),
    log_week_earn_mean = survey_mean(log_week_earn, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    benefits_per = survey_mean(benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    female_per = survey_mean(female, na.rm = T),
    black_per = survey_mean(black, na.rm = T),
    hispanic_per = survey_mean(hispanic, na.rm = T),
    less_hs_per = survey_mean(less_hs, na.rm = T),
    hs_dip_per = survey_mean(hs_dip, na.rm = T),
    aa_deg_per = survey_mean(aa_deg, na.rm = T),
    ba_deg_per = survey_mean(ba_deg, na.rm = T),
    age_mean = survey_mean(age, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(desc(contracted)) 

# Self reported (add DK to end for table)
sg_sr_sum <- sg %>% 
  filter(!is.na(contracted), !is.na(contracted_2)) %>% 
  as_survey_design(ids = case_id, weights=weight) %>% 
  mutate(contracted = factor(contracted)) %>% 
  group_by(contracted) %>% 
  summarise(
    proportion = survey_mean(),
    log_real_wage_mean = survey_mean(log_real_wage, na.rm = T),
    log_week_earn_mean = survey_mean(log_week_earn, na.rm = T),
    hours_week_mean = survey_mean(hours_week, na.rm = T),
    part_time_per = survey_mean(part_time, na.rm = T),
    job_sat_mean = survey_mean(job_sat, na.rm = T),
    union_per = survey_mean(union, na.rm = T),
    benefits_per = survey_mean(benefits, na.rm = T),
    health_per = survey_mean(health, na.rm = T),
    female_per = survey_mean(female, na.rm = T),
    black_per = survey_mean(black, na.rm = T),
    hispanic_per = survey_mean(hispanic, na.rm = T),
    less_hs_per = survey_mean(less_hs, na.rm = T),
    hs_dip_per = survey_mean(hs_dip, na.rm = T),
    aa_deg_per = survey_mean(aa_deg, na.rm = T),
    ba_deg_per = survey_mean(ba_deg, na.rm = T),
    age_mean = survey_mean(age, na.rm = T),
    n = unweighted(n())
  ) %>% 
  arrange(desc(contracted)) %>% 
  bind_rows(sg_dk_sum)

top_js <- "\\documentclass[12pt]{article}
\\usepackage[margin=1in]{geometry}
\\usepackage{siunitx}
\\usepackage{booktabs}
\\begin{document}
\\begin{table}
\\centering
\\sisetup{
table-number-alignment = center,
table-figures-integer = 3,
table-figures-decimal = 3,
input-symbols=(),
table-space-text-post = \\textsuperscript{***},
table-align-text-post = false,
group-digits          = false
}
\\begin{tabular}{lSSSS}
\\toprule
& \\multicolumn{2}{c}{{Self-Reported}} & \\multicolumn{2}{c}{{Industry-Occupation}} \\\\
Variable & {Contracted} & {Not-Contracted} & {Contracted} & {Not-Contracted} \\\\ \\midrule
"

bot_jan <- "\\bottomrule
\\end{tabular}
\\caption{Summary statistics for janitors (occupation 4220) that are contracted vs
not contracted out. In the left two columns, contracted is self-reported by the worker.
In the right two, it is infered if the worker is in industry 7690. Observations are at the
worker-job-year level and summary statistics are weighted. 
Stars represent significant difference from contracted (the second and fourth column)
or from self-reported contracted (the third column) at the .10 level *,
.05 level **, and .01 level ***.}
\\label{demo}
\\end{table}
\\end{document}"

bot_sg <- "\\bottomrule
\\end{tabular}
\\caption{Summary statistics for security guards (occupation 3920) that are contracted
vs not contracted out. In the left two columns, contracted is self-reported by the worker.
In the right two, it is infered if the worker is in industry 7680. Observations are at the
worker-job-year level and summary statistics are weighted. 
Stars represent significant difference from contracted (the second and fourth column)
or from self-reported contracted (the third column) at the .10 level *,
.05 level **, and .01 level ***.}
\\label{demo}
\\end{table}
\\end{document}"

# Create table in Latex
vars_js <- c('log_real_wage', 'log_week_earn', 'benefits', 'health', 'hours_week', 
             'part_time', 'union', 'job_sat', 'less_hs', 'hs_dip', 'aa_deg', 'ba_deg',
             'black', 'hispanic', 'female', 'age')

vars_js_p <- c('part_time', 'black', 'hispanic', 'less_hs', 'hs_dip', 'aa_deg', 'ba_deg', 
                'female', 'benefits', 'health', 'union')

vars_js_m <- c('log_real_wage', 'log_week_earn', 'hours_week', 'job_sat', 'age')

for (count in c(1, 2)){
  
  if (count == 1){
    data_sum <- janitor_sr_sum
    bot_js <- bot_jan
    occ <- 'Janitors'
  } else {
    data_sum <- sg_sr_sum
    bot_js <- bot_sg
    occ <- 'Security Guards' 
  }
  
  center_js <- rbind("Log Hourly", "Wage", "Log Weekly", "Earnings", "Any Benefits", "",
                     "Health Insurance", "", "Hours Worked", "per Week", 'Part Time', '',
                     "Union", "", "Job Satisfaction", "(Lower Better)", 'No HS Diploma',
                     '', 'HS Diploma', '',"AA Degree",'',"BA Degree +", '', 'Black', '',
                     'Hispanic', '', "Female", "", "Age", "", 'Observations')
  
  for (i in seq(1, 4, by = 1)){
    j <- 1 + 2 * (i == 4) 
    col_i <- c()
    for (var in vars_js){
      if (var %in% vars_js_p){
        var_n = paste0(var, '_per')
        se_n = paste0(var, '_per_se')
        if (i != j){
          stars <- p_test(data_sum, var_n, 'n', i, j)
        } else {
          stars <- ""
        }
      } else{
        var_n = paste0(var, '_mean')
        se_n = paste0(var, '_mean_se')
        if (i != j){
          stars <- t_test(data_sum, var_n, 'n', i, j)
        } else {
          stars <- ""
        }
          
      }
      col_i <- rbind(col_i,
                     paste0(' & ', format(round(data_sum[[var_n]][i], 3), nsmall=3),
                            stars),
                     paste0(' & (', format(round(data_sum[[se_n]][i], 3), nsmall=3), ')'))
    }
    col_i <- rbind(col_i, 
                   paste0(' & {', format(data_sum$n[i], big.mark = ',', trim = T), '}'))
    center_js <- cbind(center_js, col_i)
  }
  
  center_js <- cbind(center_js, 
                     rbind('\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                           '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                           '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                           '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                           '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', 
                           '\\\\', '\\\\[2pt]', '\\\\'))
  
  # Do weird stuff to create LaTeX output
  j_folder <- paste0(tables, 'Junk/')
  file_7 <- paste0(j_folder, 'center_jan.txt')
  file_8 <- paste0(j_folder, 'center_jan_2.txt')
  write.table(center_js, file_7, quote=T, col.names=F, row.names=F)
  center_js <- read.table(file_7, sep = '')
  write.table(center_js, file_8, quote=F, col.names=F, row.names=F, sep = '')
  center_js <- readChar(file_8, nchars = 1e6)
  
  
  write.table(paste0(top_js, center_js, bot_js),
              paste0(tables, 'NLSY79 ', occ,  '/NLSY79 ', occ, '.tex'),
              quote=F, col.names=F, row.names=F, sep="")
  
}

###########################################################################

# Run regressions

# Create a function that takes regression p-values and reports
# * if 10%, ** if 5%, and *** if 1% significant
p_stars <- function(p){
  if (p < .01){
    stars <- '\\textsuperscript{***}'
  } else if (p < .05){
    stars <- '\\textsuperscript{**}'
  } else if (p < .1){
    stars <- '\\textsuperscript{*}'
  } else{
    stars <- ''
  }
  return(stars)
}

# Create a loop that takes various desired outcomes, runs series of regressions,
# and creates table in Latex
# For men and women
# 1. Basic Controls (age, age_2, age_3, black, hispanic, education,
# union, region, msa/cc, marital stutus, children, hours week / part time, year)
# 2. Add tenure
# 3. Add Ind factors
# 4. Add 2+3
# 5. Individual FE + some controls (age, age_2, age_3, union, region, msa/cc,
# marital stutus, children, hours week / part time, year)
# 6. Add tenure
# 7. Add Ind factors
# 8. Add 6+7

top_r <- "\\documentclass[12pt]{article}
\\usepackage[margin=1in]{geometry}
\\usepackage{siunitx}
\\usepackage{booktabs}
\\begin{document}
\\begin{table}
\\centering
\\sisetup{
table-number-alignment = center,
table-figures-integer = 3,
table-figures-decimal = 3,
input-symbols=(),
table-space-text-post = \\textsuperscript{***},
table-align-text-post = false,
group-digits          = false
}
\\begin{tabular}{lSccccS}
\\toprule
Model & {Contracted} & Tenure  & Occupation  & Individual & Obs & {$R^2$}  \\\\
&  & Quadratic & FE &  FE & &   \\\\\\midrule
"

var_r <- c('log_real_wage', 'log_week_earn', 'job_sat', 'benefits', 'health')

var_names <- c('log real wages', 'log real weekly earnings', 
              'job satisfaction (lower is better)', 'receiving any employment benefits',
              'receiving health insurance through employer')

sex_names <- c('men', 'women')

controls <- c('age', 'age_2', 'age_3', 'msa', 'msa_cc',
              'union', 'single', 'married', 'tot_children', 'hh_children')

hours <- c('hours_week', 'part_time')

ols_controls <- c('black', 'hispanic', 'hs_dip', 'aa_deg', 'ba_deg', 'plus_deg')

tenure <- c('tenure_1', 'tenure_2', 'tenure_3')


for (sex in c(0, 1)){
  for (ind in seq(1, 5)){
    var <- var_r[ind]
    var_name <- var_names[ind]
    sex_name <- sex_names[sex + 1]

    center_r <- rbind('OLS', 'Basic', '', 'Tenure Quadratic', '', 'Occ FE', '',
                      'Tenure + Occ', '', 'FE', 'Basic', '', 'Tenure Quadratic', '',
                      'Occ FE', '', 'Tenure + Occ', '')
    
    c_r <- cbind('& ', '& ', '& ', '& ', '& ', '& ')

    for (reg_ind in seq(1, 8)){

      ind_vars <- paste('contracted', paste(controls, collapse = '+'), sep='+')
      
      hours_text <- "" 
      if (var_r != 'log_week_earn'){
        ind_vars <- paste(ind_vars, paste(hours, collapse = '+'), sep='+')
        hours_text <- " hours worked per week, part-time status,"
      }

      if (reg_ind <= 4){
        ife_ind <- 'No'
        ind_vars <- paste(ind_vars, paste(ols_controls, collapse = '+'), sep='+')
      } else{
        ife_ind <- 'Yes'
      }

      if (reg_ind %in% c(2, 4, 6, 8)){
        ten_ind <- 'Yes'
        ind_vars <- paste(ind_vars, paste(tenure, collapse = '+'), sep='+')
      } else{
        ten_ind <- 'No'
      }

      if (reg_ind %in% c(3, 4, 7, 8)){
        oi_ind <- 'Yes'
      } else{
        oi_ind <- 'No'
      }
      
      if (reg_ind == 5){
        c_r <- rbind(c_r, cbind('& ', '& ', '& ', '& ', '& ', '& '))
      }

      eq_t <- formula(paste(var, ind_vars, sep = '~'))

      # Have to do this because eq for fixed effects not behaving
      if (reg_ind %in% c(1, 2)){
        temp <- lm_robust(eq_t, data = long, subset = (female == sex),
                          weights = weight,
                          fixed_effects = ~ year + region,
                          clusters = occ_cat, se_type = "stata", try_cholesky = T)
      } else if (reg_ind %in% c(3, 4)){
        temp <- lm_robust(eq_t, data = long, subset = (female == sex),
                          weights = weight,
                          fixed_effects = ~ year + region + occ,
                          clusters = occ_cat, se_type = "stata", try_cholesky = T)
      } else if (reg_ind %in% c(5, 6)){
        temp <- lm_robust(eq_t, data = long, subset = (female == sex),
                          weights = weight,
                          fixed_effects = ~ year + region + case_id,
                          clusters = occ_cat, se_type = "stata", try_cholesky = T)
      }else if (reg_ind %in% c(7, 8)){
        temp <- lm_robust(eq_t, data = long, subset = (female == sex),
                          weights = weight,
                          fixed_effects = ~ year + region + case_id + occ,
                          clusters = occ_cat, se_type = "stata", try_cholesky = T)
      }
      stars <- p_stars(temp$p.value['contracted'])

      c_r <- rbind(c_r,
        cbind(
          paste0(' & ', format(round(temp$coefficients['contracted'], 3), nsmall=3),
                            stars),
          paste0(' & ', ten_ind),
          paste0(' & ', oi_ind),
          paste0(' & ', ife_ind),
          paste0(' & ', format(temp$N, big.mark = ',', trim = T)),
          paste0(' & ', format(round(temp$r.squared, 2), nsmall=2))
        ),
        cbind(
          paste0(' & (', format(round(temp$std.error['contracted'], 3), nsmall=3), ')'),
          ' & ', ' & ', ' & ', ' & ', ' & '
        )
        )
    }

    center_r <- cbind(center_r, c_r,
                       rbind('\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]',
                             '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt] \\midrule',
                             '\\\\[2pt]', '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]', 
                             '\\\\', '\\\\[2pt]', '\\\\', '\\\\[2pt]'))

    # Do weird stuff to create LaTeX output
    r_folder <- paste0(tables, 'Junk/')
    file_9 <- paste0(r_folder, 'center_r.txt')
    file_10 <- paste0(r_folder, 'center_r_2.txt')
    write.table(center_r, file_9, quote=T, col.names=F, row.names=F)
    center_r <- read.table(file_9, sep = '')
    write.table(center_r, file_10, quote=F, col.names=F, row.names=F, sep = '')
    center_r <- readChar(file_10, nchars = 1e6)

    bot_r <- paste0(
"\\bottomrule
\\end{tabular}
\\caption{Regressions of contracted out on ", var_name, " for ", sex_name,
". All regressions
include controls for a quadratic in age, union status,", hours_text,
" dummies for region, whether in an MSA or central city,
marital status, number of children total and in household, and dummies for
observation year. The first four columns run OLS and also contain controls for
race and education. The last four columns use worker fixed effects.
All observations are at the person-job-year level and all standard errors are
clustered by occupation category.
Stars represent significant at the .10 level *, .05 level **, and .01 level ***.}
\\label{reg_" , var, "_", sex_name, "}
\\end{table}
\\end{document}"
    )


    write.table(paste0(top_r, center_r, bot_r),
                paste0(tables, 'NLSY79 Regressions/NLSY79 ', var, "_", sex_name, '.tex'),
                quote=F, col.names=F, row.names=F, sep="")

  }
}


##########################################################################

# # Use ggplot 2 to graph some things
# Graph some variables by year for contracted vs not for men and women
# Both all workers and ever contracted

# Also plot log_real_wage/log_week_earn residuals 
# from full FE regression about (but without outsourcing)
# See how these compare for contracted vs non-contracted

var_g <- c('log_real_wage', 'log_week_earn', 'hours_week', 'tenure', 
           'weeks_worked_prev')

var_names <- c('Log Real Hourly Wage', 'Log Real Weekly Earnings', 
               'Hours Worked per Week', 'Weeks of Tenure', 
               'Weeks Worked Previous Year')

# sex same as above
sex_names <- c("Men's ", "Women's ")
sex_save <- c('men', 'women')

for (sex in c(0, 1)){
  for (i in seq(1, length(var_g))){
    
    temp <- long %>%
      filter(female == sex, !is.na(contracted), !is.na(.[[var_g[i]]])) %>%
      ggplot(., aes(.[[var_g[i]]], fill = factor(.$contracted))) +
      geom_density(alpha = 0.3) +
      labs(title=paste0(sex_names[sex + 1], var_names[i], ' for All Workers'), 
           x = var_names[i], y = "Density") +
      scale_fill_discrete(name = "Contracted", breaks = c(0, 1),
                          labels = c("Not Contracted", "Contracted")) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(paste0(figures, var_g[i], '_', sex_save[sex + 1], '_', 'all.pdf'))
    
    temp <- long %>%
      filter(female == sex, !is.na(contracted), ever_con == 1, !is.na(.[[var_g[i]]])) %>%
      ggplot(., aes(.[[var_g[i]]], fill = factor(.$contracted))) +
      geom_density(alpha = 0.3) +
      labs(title=paste0(sex_names[sex + 1], var_names[i], ' for Ever Contracted'), 
           x = var_names[i], y = "Density") +
      scale_fill_discrete(name = "Contracted", breaks = c(0, 1),
                          labels = c("Not Contracted", "Contracted")) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(paste0(figures, var_g[i], '_', sex_save[sex + 1], '_', 'ever.pdf'))
  }
  
  for (i in c(1, 2)){
    
    ind_vars <- paste(controls, collapse = '+')
    ind_vars <- paste(ind_vars, paste(tenure, collapse = '+'), sep='+')
    if (var_r[i] != 'log_week_earn'){
      ind_vars <- paste(ind_vars, paste(hours, collapse = '+'), sep='+')
    }
    
    eq_t <- formula(paste(var_g[i], ind_vars, sep = '~'))
    
    reg <- lm_robust(eq_t, data = long, subset = (female == sex) & !is.na(contracted),
                     weights = weight,
                     fixed_effects = ~ year + region + case_id + occ,
                     clusters = occ_cat, se_type = "stata", try_cholesky = T)
    
    temp_df <- long %>% 
      filter(
        female == sex, !is.na(.[[var_g[i]]]), !is.na(occ), !is.na(region), 
        !is.na(msa), !is.na(msa_cc), !is.na(union), !is.na(single),
        !is.na(tenure_1), !is.na(hh_children), !is.na(contracted), !is.na(hours_week)
        ) %>% 
      mutate(resid = .[[var_g[i]]] - reg$fitted.values) %>% 
      filter(resid >= - 3 * sd(resid) & resid <= 3 * sd(resid))
    
    print(paste0(sex_names[sex + 1], var_names[i]))
    print(table(temp_df$resid[long$contracted == 0 & long$ever_con == 1] < 0))
    
    temp <- temp_df %>%
      ggplot(., aes(.$resid, fill = factor(.$contracted))) +
      geom_density(alpha = 0.3) +
      labs(title=paste0(sex_names[sex + 1], var_names[i], ' Residuals for All Workers'), 
           x = paste0(var_names[i], ' Residuals'), y = "Density") +
      scale_fill_discrete(name = "Contracted", breaks = c(0, 1),
                          labels = c("Not Contracted", "Contracted")) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(paste0(figures, 'resid_', var_g[i], '_', sex_save[sex + 1], '_', 'all.pdf'))
    
    temp <- temp_df %>%
      filter(ever_con == 1) %>%
      ggplot(., aes(.$resid, fill = factor(.$contracted))) +
      geom_density(alpha = 0.3) +
      labs(title=paste0(sex_names[sex + 1], var_names[i],
                        ' Residuals for Ever Contracted'), 
           x = paste0(var_names[i], ' Residuals'), y = "Density") +
      scale_fill_discrete(name = "Contracted", breaks = c(0, 1),
                          labels = c("Not Contracted", "Contracted")) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(paste0(figures, 'resid_', var_g[i], '_', sex_save[sex + 1], '_', 'ever.pdf'))
    
  }
}

###########################################################################

# How good is the match?
# From ES, can look at m_flag or look at contracted != NA. Later is probably
# prefered because it imputes contracted based on other job answers/ 
# is what I actually care about
es_1_miss <- sum(long$m_flag == 0, na.rm = T)
es_1_match <- sum(long$m_flag == 1, na.rm = T)
es_1_quality <- es_1_match / (es_1_match + es_1_miss) * 100

es_2_miss <- sum(is.na(long$contracted) == T)
es_2_match <- sum(is.na(long$contracted) == F)
es_2_quality <- es_2_match / (es_2_match + es_2_miss) * 100

# From OJ, look at m_flag == 0 and m_flag == 0 & contracted == 1
oj_miss <- sum(apply(
  new_data[grep('m_flag.*_....', names(new_data))] == 0, 1, sum, na.rm = T))
oj_match <- sum(apply(
  new_data[grep('m_flag.*_....', names(new_data))] == 1, 1, sum, na.rm = T))
oj_quality <- oj_match / (oj_match + oj_miss) * 100

oj_c_miss <- 0
oj_c_match <- 0
for (year in seq(2004, 2016, by=2)){
  for (job in seq(1, 15)){
    oj_c_miss <- oj_c_miss + sum(
      new_data[[paste0('m_flag_', job, '_', year)]][new_data[[paste0('contracted_', job, '_', year)]] == 1] == 0, na.rm = T)
    oj_c_match <- oj_c_match + sum(
      new_data[[paste0('m_flag_', job, '_', year)]][new_data[[paste0('contracted_', job, '_', year)]] == 1] == 1, na.rm = T)
  }
}
oj_c_quality <- oj_c_match / (oj_c_match + oj_c_miss) * 100

# Create Table of match quality

top_q <- "\\documentclass[12pt]{article}
\\usepackage[margin=1in]{geometry}
\\usepackage{siunitx}
\\usepackage{booktabs}
\\begin{document}
\\begin{table}
\\centering
\\begin{tabular}{lrr}
\\toprule
Variable &  Match Number & Match Percent \\\\ \\midrule
"

var_names <- c('oj', 'oj_c', 'es_1', 'es_2')

var_labels <- c('Jobs in On Jobs', 'Contracted Jobs in On Jobs ', 
                'Jobs in Employer Supplement', 
                'Contracted Known in Employer Supplement')

center_q <- ""

for (i in seq(1, length(var_names))){
  center_q <- paste(
    center_q, 
    paste(
      var_labels[i], 
      format(get(paste0(var_names[i], '_match')), big.mark = ',', trim = T),
      format(round(get(paste0(var_names[i], '_quality')), 2), nsmall = 2), sep = ' & '
    ), '\\\\ \n'
    )
}

bot_q <- "\\bottomrule
\\end{tabular}
\\caption{Number of matches and quality of match from the On Jobs section for 
all jobs and contracted jobs and for Employer Supplement. All matches are 
at the person-year-job level. For Employer Supplement,
contracted status may be learned from past matches, so the job may not be matched
this period and still be useful in analysis.
}
\\label{quality}
\\end{table}
\\end{document}"

write.table(paste0(top_q, center_q, bot_q),
            paste0(tables, 'NLSY79 Match Quality/NLSY79 Match Quality.tex'),
            quote=F, col.names=F, row.names=F, sep="")

