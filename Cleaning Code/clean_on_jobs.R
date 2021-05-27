# This file takes raw data from errata and on_jobs_raw.
# It cleans it then saves it as on_jobs_clean
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
  "Q6-8F.01.01_2002",
  "Q6-8F.02.01_2002",
  "Q6-8F.03.01_2002",
  "Q6-8F.04.01_2002",
  "Q6-8F.05.01_2002",
  "Q6-8H_A1.01.01_2002",
  "Q6-8H_A1.02.01_2002",
  "Q6-8H_A1.03.01_2002",
  "Q6-8H_A1.04.01_2002",
  "Q6-8H_A1.05.01_2002",
  "Q6-8H_A2.01.01_2002",
  "Q6-8H_A2.01.02_2002",
  "Q6-8H_A2.02.01_2002",
  "Q6-8H_A2.02.02_2002",
  "Q6-8H_A2.03.01_2002",
  "Q6-8H_A2.03.02_2002",
  "Q6-8H_A2.04.01_2002",
  "Q6-8H_A3.01.01_2002",
  "Q6-8H_A3.01.02_2002",
  "Q6-8H_A3.02.01_2002",
  "Q6-8H_A3.02.02_2002",
  "Q6-8H_A3.03.01_2002",
  "Q6-8H_A3.03.02_2002",
  "Q6-8H_A3.04.01_2002",
  "Q6-8H_A4A.01.01_2002",
  "Q6-8H_A4A.02.01_2002",
  "Q6-8H_A4A.03.01_2002",
  "Q6-8H_A4A.04.01_2002",
  "Q6-8H_A4B.01.02_2002",
  "Q6-8H_A4B.02.02_2002",
  "Q6-8H_A4B.03.02_2002",
  "Q6-8H_A5A.01.01_2002",
  "Q6-8H_A5A.02.01_2002",
  "Q6-8H_A5A.03.01_2002",
  "Q6-8H_A5A.04.01_2002",
  "Q6-8H_A5B.01.02_2002",
  "Q6-8H_A5B.02.02_2002",
  "Q6-8H_A5B.03.02_2002",
  "Q6-8I.01_2002",
  "Q6-8I.02_2002",
  "Q6-8I.03_2002",
  "Q6-8I.04_2002",
  "Q6-8I.05_2002",
  "Q6-16E_1A.01.01_2002",
  "Q6-16E_1A.02.01_2002",
  "Q6-16E_1A.03.01_2002",
  "Q6-16E_1A.04.01_2002",
  "Q6-16E_1A.05.01_2002",
  "Q6-16F.01.01_2002",
  "Q6-16F.02.01_2002",
  "Q6-16F.03.01_2002",
  "Q6-16F.04.01_2002",
  "Q6-16F.05.01_2002",
  "Q6-16H_A1.01.01_2002",
  "Q6-16H_A1.02.01_2002",
  "Q6-16H_A1.03.01_2002",
  "Q6-16H_A1.05.01_2002",
  "Q6-16H_A2.01.01_2002",
  "Q6-16H_A2.01.02_2002",
  "Q6-16H_A2.02.01_2002",
  "Q6-16H_A2.03.01_2002",
  "Q6-16H_A2.05.01_2002",
  "Q6-16H_A3.01.01_2002",
  "Q6-16H_A3.01.02_2002",
  "Q6-16H_A3.02.01_2002",
  "Q6-16H_A3.03.01_2002",
  "Q6-16H_A3.05.01_2002",
  "Q6-16H_A4A.01.01_2002",
  "Q6-16H_A4A.02.01_2002",
  "Q6-16H_A4A.03.01_2002",
  "Q6-16H_A4A.05.01_2002",
  "Q6-16H_A4B.01.02_2002",
  "Q6-16H_A5A.01.01_2002",
  "Q6-16H_A5A.02.01_2002",
  "Q6-16H_A5A.03.01_2002",
  "Q6-16I.01_2002",
  "Q6-16I.02_2002",
  "Q6-16I.03_2002",
  "Q6-16I.04_2002",
  "Q6-16I.05_2002",
  "Q6-27D_1A.01.01_2002",
  "Q6-27D_1A.02.01_2002",
  "Q6-27D_1A.03.01_2002",
  "Q6-27D_1A.04.01_2002",
  "Q6-27D_1A.05.01_2002",
  "Q6-27D_1A.06.01_2002",
  "Q6-27D_1A.07.01_2002",
  "Q6-27E_A1.01.01_2002",
  "Q6-27E_A1.02.01_2002",
  "Q6-27E_A1.03.01_2002",
  "Q6-27E_A1.04.01_2002",
  "Q6-27E_A1.05.01_2002",
  "Q6-27E_A1.06.01_2002",
  "Q6-27E_A1.07.01_2002",
  "Q6-27E_A2.01.01_2002",
  "Q6-27E_A2.01.02_2002",
  "Q6-27E_A2.02.01_2002",
  "Q6-27E_A2.02.02_2002",
  "Q6-27E_A2.03.01_2002",
  "Q6-27E_A2.04.01_2002",
  "Q6-27E_A2.04.02_2002",
  "Q6-27E_A2.05.01_2002",
  "Q6-27E_A2.06.01_2002",
  "Q6-27E_A2.07.01_2002",
  "Q6-27E_A3.01.01_2002",
  "Q6-27E_A3.01.02_2002",
  "Q6-27E_A3.02.01_2002",
  "Q6-27E_A3.02.02_2002",
  "Q6-27E_A3.03.01_2002",
  "Q6-27E_A3.04.01_2002",
  "Q6-27E_A3.05.01_2002",
  "Q6-27E_A3.06.01_2002",
  "Q6-27E_A3.07.01_2002",
  "Q6-27E_A4A.01.01_2002",
  "Q6-27E_A4A.02.01_2002",
  "Q6-27E_A4A.03.01_2002",
  "Q6-27E_A4A.04.01_2002",
  "Q6-27E_A4A.05.01_2002",
  "Q6-27E_A4A.06.01_2002",
  "Q6-27E_A4A.07.01_2002",
  "Q6-27E_A4B.01.02_2002",
  "Q6-27E_A4B.02.02_2002",
  "Q6-27E_A5A.01.01_2002",
  "Q6-27E_A5A.02.01_2002",
  "Q6-27E_A5A.03.01_2002",
  "Q6-27E_A5A.04.01_2002",
  "Q6-27E_A5A.05.01_2002",
  "Q6-27E_A5A.06.01_2002",
  "Q6-27E_A5A.07.01_2002",
  "Q6-27E_A5B.01.02_2002",
  "Q6-27E_A5B.02.02_2002",
  "ONJS-8800_2002",
  "Q6-8F.01.01_2004",
  "Q6-8F.02.01_2004",
  "Q6-8F.03.01_2004",
  "Q6-8F.04.01_2004",
  "Q6-8H_A1.01.01_2004",
  "Q6-8H_A1.02.01_2004",
  "Q6-8H_A1.03.01_2004",
  "Q6-8H_A1.04.01_2004",
  "Q6-8H_A2.01.01_2004",
  "Q6-8H_A2.01.02_2004",
  "Q6-8H_A2.02.01_2004",
  "Q6-8H_A2.02.02_2004",
  "Q6-8H_A2.03.01_2004",
  "Q6-8H_A2.04.01_2004",
  "Q6-8H_A3.01.01_2004",
  "Q6-8H_A3.01.02_2004",
  "Q6-8H_A3.02.01_2004",
  "Q6-8H_A3.02.02_2004",
  "Q6-8H_A3.03.01_2004",
  "Q6-8H_A3.04.01_2004",
  "Q6-8H_A4A.01.01_2004",
  "Q6-8H_A4A.02.01_2004",
  "Q6-8H_A4A.03.01_2004",
  "Q6-8H_A4A.04.01_2004",
  "Q6-8H_A4B.01.02_2004",
  "Q6-8H_A4B.02.02_2004",
  "Q6-8H_A5.01.01_2004",
  "Q6-8H_A5.01.02_2004",
  "Q6-8H_A5.02.01_2004",
  "Q6-8H_A5.02.02_2004",
  "Q6-8H_A5.03.01_2004",
  "Q6-8H_A5.04.01_2004",
  "Q6-8H_A5A.01.01_2004",
  "Q6-8H_A5A.02.01_2004",
  "Q6-8H_A5A.03.01_2004",
  "Q6-8H_A5A.04.01_2004",
  "Q6-8H_A5B.01.02_2004",
  "Q6-8H_A5B.02.02_2004",
  "Q6-8I.01_2004",
  "Q6-8I.02_2004",
  "Q6-8I.03_2004",
  "Q6-8I.04_2004",
  "Q6-8I.05_2004",
  "Q6-9.01~M_2004",
  "Q6-9.01~Y_2004",
  "Q6-9.02~M_2004",
  "Q6-9.02~Y_2004",
  "Q6-9.03~M_2004",
  "Q6-9.03~Y_2004",
  "Q6-9.04~M_2004",
  "Q6-9.04~Y_2004",
  "Q6-9.05~M_2004",
  "Q6-9.05~Y_2004",
  "PDLI-15.01~M_2004",
  "PDLI-15.01~Y_2004",
  "PDLI-15.02~M_2004",
  "PDLI-15.02~Y_2004",
  "PDLI-15.03~M_2004",
  "PDLI-15.03~Y_2004",
  "Q6-16E_1A.01.01_2004",
  "Q6-16E_1A.02.01_2004",
  "Q6-16E_1A.03.01_2004",
  "Q6-16F.01.01_2004",
  "Q6-16F.02.01_2004",
  "Q6-16F.03.01_2004",
  "Q6-16H_A1.01.01_2004",
  "Q6-16H_A1.02.01_2004",
  "Q6-16H_A2.01.01_2004",
  "Q6-16H_A2.01.02_2004",
  "Q6-16H_A2.02.01_2004",
  "Q6-16H_A3.01.01_2004",
  "Q6-16H_A3.02.01_2004",
  "Q6-16H_A4A.01.01_2004",
  "Q6-16H_A4A.02.01_2004",
  "Q6-16H_A5A.01.01_2004",
  "Q6-16H_A5A.02.01_2004",
  "Q6-16I.01_2004",
  "Q6-16I.02_2004",
  "Q6-16I.03_2004",
  "Q6-17.01~M_2004",
  "Q6-17.01~Y_2004",
  "Q6-17.02~M_2004",
  "Q6-17.02~Y_2004",
  "Q6-17.03~M_2004",
  "Q6-17.03~Y_2004",
  "Q6-27D_1A.01.01_2004",
  "Q6-27D_1A.02.01_2004",
  "Q6-27D_1A.03.01_2004",
  "Q6-27D_1A.04.01_2004",
  "Q6-27D_1A.05.01_2004",
  "Q6-27E_A1.01.01_2004",
  "Q6-27E_A1.02.01_2004",
  "Q6-27E_A1.03.01_2004",
  "Q6-27E_A1.04.01_2004",
  "Q6-27E_A1.05.01_2004",
  "Q6-27E_A2.01.01_2004",
  "Q6-27E_A2.01.02_2004",
  "Q6-27E_A2.02.01_2004",
  "Q6-27E_A2.02.02_2004",
  "Q6-27E_A2.03.01_2004",
  "Q6-27E_A2.04.01_2004",
  "Q6-27E_A2.05.01_2004",
  "Q6-27E_A3.01.01_2004",
  "Q6-27E_A3.01.02_2004",
  "Q6-27E_A3.02.01_2004",
  "Q6-27E_A3.02.02_2004",
  "Q6-27E_A3.03.01_2004",
  "Q6-27E_A3.04.01_2004",
  "Q6-27E_A3.05.01_2004",
  "Q6-27E_A4A.01.01_2004",
  "Q6-27E_A4A.02.01_2004",
  "Q6-27E_A4A.03.01_2004",
  "Q6-27E_A4A.04.01_2004",
  "Q6-27E_A4A.05.01_2004",
  "Q6-27E_A4B.01.02_2004",
  "Q6-27E_A4B.02.02_2004",
  "Q6-27E_A5A.01.01_2004",
  "Q6-27E_A5A.02.01_2004",
  "Q6-27E_A5A.03.01_2004",
  "Q6-27E_A5A.04.01_2004",
  "Q6-27E_A5A.05.01_2004",
  "Q6-27E_A5B.01.02_2004",
  "Q6-27E_A5B.02.02_2004",
  "Q6-27A.01~M_2004",
  "Q6-27A.01~Y_2004",
  "Q6-27A.02~M_2004",
  "Q6-27A.02~Y_2004",
  "Q6-27A.03~M_2004",
  "Q6-27A.03~Y_2004",
  "Q6-27A.04~M_2004",
  "Q6-27A.04~Y_2004",
  "Q6-27A.05~M_2004",
  "Q6-27A.05~Y_2004",
  "Q6-27K.01~M_2004",
  "Q6-27K.01~Y_2004",
  "Q6-27K.02~M_2004",
  "Q6-27K.02~Y_2004",
  "Q6-27K.03~M_2004",
  "Q6-27K.03~Y_2004",
  "Q6-27K.04~M_2004",
  "Q6-27K.04~Y_2004",
  "Q6-27K.05~M_2004",
  "Q6-27K.05~Y_2004",
  "ONJS-8800_2004",
  "Q6-8E_1A.01.01_2006",
  "Q6-8E_1A.02.01_2006",
  "Q6-8E_1A.03.01_2006",
  "Q6-8E_1A.04.01_2006",
  "Q6-8F.01.01_2006",
  "Q6-8F.02.01_2006",
  "Q6-8F.03.01_2006",
  "Q6-8F.04.01_2006",
  "Q6-8H_A1.01.01_2006",
  "Q6-8H_A1.02.01_2006",
  "Q6-8H_A1.03.01_2006",
  "Q6-8H_A1.04.01_2006",
  "Q6-8H_A2.01.01_2006",
  "Q6-8H_A2.01.02_2006",
  "Q6-8H_A2.02.01_2006",
  "Q6-8H_A2.02.02_2006",
  "Q6-8H_A2.03.01_2006",
  "Q6-8H_A2.03.02_2006",
  "Q6-8H_A2.04.01_2006",
  "Q6-8H_A3.01.01_2006",
  "Q6-8H_A3.01.02_2006",
  "Q6-8H_A3.02.01_2006",
  "Q6-8H_A3.02.02_2006",
  "Q6-8H_A3.03.01_2006",
  "Q6-8H_A3.04.01_2006",
  "Q6-8H_A4A.01.01_2006",
  "Q6-8H_A4A.02.01_2006",
  "Q6-8H_A4A.03.01_2006",
  "Q6-8H_A4A.04.01_2006",
  "Q6-8H_A4B.01.02_2006",
  "Q6-8H_A4B.02.02_2006",
  "Q6-8H_A5A.01.01_2006",
  "Q6-8H_A5A.02.01_2006",
  "Q6-8H_A5A.03.01_2006",
  "Q6-8H_A5A.04.01_2006",
  "Q6-8H_A5B.01.02_2006",
  "Q6-8I.01_2006",
  "Q6-8I.02_2006",
  "Q6-8I.03_2006",
  "Q6-8I.04_2006",
  "Q6-9.01~M_2006",
  "Q6-9.01~Y_2006",
  "Q6-9.02~M_2006",
  "Q6-9.02~Y_2006",
  "Q6-9.03~M_2006",
  "Q6-9.03~Y_2006",
  "Q6-9.04~M_2006",
  "Q6-9.04~Y_2006",
  "PDLI-15.01~M_2006",
  "PDLI-15.01~Y_2006",
  "PDLI-15.02~M_2006",
  "PDLI-15.02~Y_2006",
  "PDLI-15.03~M_2006",
  "PDLI-15.03~Y_2006",
  "PDLI-15.04~M_2006",
  "PDLI-15.04~Y_2006",
  "Q6-16E_1A.01.01_2006",
  "Q6-16E_1A.02.01_2006",
  "Q6-16E_1A.03.01_2006",
  "Q6-16E_1A.04.01_2006",
  "Q6-16F.01.01_2006",
  "Q6-16F.02.01_2006",
  "Q6-16F.03.01_2006",
  "Q6-16F.04.01_2006",
  "Q6-16H_A1.01.01_2006",
  "Q6-16H_A1.02.01_2006",
  "Q6-16H_A2.01.01_2006",
  "Q6-16H_A2.01.02_2006",
  "Q6-16H_A2.02.01_2006",
  "Q6-16H_A3.01.01_2006",
  "Q6-16H_A3.01.02_2006",
  "Q6-16H_A3.02.01_2006",
  "Q6-16H_A4A.01.01_2006",
  "Q6-16H_A4A.02.01_2006",
  "Q6-16H_A4B.01.02_2006",
  "Q6-16H_A5A.01.01_2006",
  "Q6-16H_A5A.02.01_2006",
  "Q6-16H_A5B.01.02_2006",
  "Q6-16I.01_2006",
  "Q6-16I.02_2006",
  "Q6-16I.03_2006",
  "Q6-16I.04_2006",
  "Q6-17.01~M_2006",
  "Q6-17.01~Y_2006",
  "Q6-17.02~M_2006",
  "Q6-17.02~Y_2006",
  "Q6-17.03~M_2006",
  "Q6-17.03~Y_2006",
  "Q6-17.04~M_2006",
  "Q6-17.04~Y_2006",
  "Q6-27D_1A.01.01_2006",
  "Q6-27D_1A.02.01_2006",
  "Q6-27D_1A.03.01_2006",
  "Q6-27D_1A.04.01_2006",
  "Q6-27D_1A.05.01_2006",
  "Q6-27E_A1.01.01_2006",
  "Q6-27E_A1.02.01_2006",
  "Q6-27E_A1.03.01_2006",
  "Q6-27E_A1.04.01_2006",
  "Q6-27E_A1.05.01_2006",
  "Q6-27E_A2.01.01_2006",
  "Q6-27E_A2.01.02_2006",
  "Q6-27E_A2.02.01_2006",
  "Q6-27E_A2.02.02_2006",
  "Q6-27E_A2.03.01_2006",
  "Q6-27E_A2.03.02_2006",
  "Q6-27E_A2.04.01_2006",
  "Q6-27E_A2.04.02_2006",
  "Q6-27E_A2.05.01_2006",
  "Q6-27E_A3.01.01_2006",
  "Q6-27E_A3.01.02_2006",
  "Q6-27E_A3.02.01_2006",
  "Q6-27E_A3.02.02_2006",
  "Q6-27E_A3.03.01_2006",
  "Q6-27E_A3.03.02_2006",
  "Q6-27E_A3.04.01_2006",
  "Q6-27E_A3.05.01_2006",
  "Q6-27E_A4A.01.01_2006",
  "Q6-27E_A4A.02.01_2006",
  "Q6-27E_A4A.03.01_2006",
  "Q6-27E_A4A.04.01_2006",
  "Q6-27E_A4A.05.01_2006",
  "Q6-27E_A4B.01.02_2006",
  "Q6-27E_A4B.02.02_2006",
  "Q6-27E_A4B.03.02_2006",
  "Q6-27E_A5A.01.01_2006",
  "Q6-27E_A5A.02.01_2006",
  "Q6-27E_A5A.03.01_2006",
  "Q6-27E_A5A.04.01_2006",
  "Q6-27E_A5A.05.01_2006",
  "Q6-27E_A5B.01.02_2006",
  "Q6-27E_A5B.02.02_2006",
  "Q6-27E_A5B.03.02_2006",
  "Q6-27A.01~M_2006",
  "Q6-27A.01~Y_2006",
  "Q6-27A.02~M_2006",
  "Q6-27A.02~Y_2006",
  "Q6-27A.03~M_2006",
  "Q6-27A.03~Y_2006",
  "Q6-27A.04~M_2006",
  "Q6-27A.04~Y_2006",
  "Q6-27A.05~M_2006",
  "Q6-27A.05~Y_2006",
  "Q6-27K.01~M_2006",
  "Q6-27K.01~Y_2006",
  "Q6-27K.02~M_2006",
  "Q6-27K.02~Y_2006",
  "Q6-27K.03~M_2006",
  "Q6-27K.03~Y_2006",
  "Q6-27K.04~M_2006",
  "Q6-27K.04~Y_2006",
  "Q6-27K.05~M_2006",
  "Q6-27K.05~Y_2006",
  "ONJS-8800_2006",
  "Q6-8E_1A.01.01_2008",
  "Q6-8E_1A.02.01_2008",
  "Q6-8E_1A.03.01_2008",
  "Q6-8E_1A.04.01_2008",
  "Q6-8E_1A.05.01_2008",
  "Q6-8F.01.01_2008",
  "Q6-8F.02.01_2008",
  "Q6-8F.03.01_2008",
  "Q6-8F.04.01_2008",
  "Q6-8F.05.01_2008",
  "Q6-8H_A1.01.01_2008",
  "Q6-8H_A1.02.01_2008",
  "Q6-8H_A1.03.01_2008",
  "Q6-8H_A1.04.01_2008",
  "Q6-8H_A1.05.01_2008",
  "Q6-8H_A2.01.01_2008",
  "Q6-8H_A2.01.02_2008",
  "Q6-8H_A2.02.01_2008",
  "Q6-8H_A2.02.02_2008",
  "Q6-8H_A2.03.01_2008",
  "Q6-8H_A2.04.01_2008",
  "Q6-8H_A2.05.01_2008",
  "Q6-8H_A3.01.01_2008",
  "Q6-8H_A3.01.02_2008",
  "Q6-8H_A3.02.01_2008",
  "Q6-8H_A3.02.02_2008",
  "Q6-8H_A3.03.01_2008",
  "Q6-8H_A3.04.01_2008",
  "Q6-8H_A3.05.01_2008",
  "Q6-8H_A4A.01.01_2008",
  "Q6-8H_A4A.02.01_2008",
  "Q6-8H_A4A.04.01_2008",
  "Q6-8H_A4A.05.01_2008",
  "Q6-8H_A4B.01.02_2008",
  "Q6-8H_A5A.01.01_2008",
  "Q6-8H_A5A.02.01_2008",
  "Q6-8H_A5A.04.01_2008",
  "Q6-8H_A5A.05.01_2008",
  "Q6-8H_A5B.01.02_2008",
  "Q6-8I.01_2008",
  "Q6-8I.02_2008",
  "Q6-8I.03_2008",
  "Q6-8I.04_2008",
  "Q6-8I.05_2008",
  "Q6-9.01~M_2008",
  "Q6-9.01~Y_2008",
  "Q6-9.02~M_2008",
  "Q6-9.02~Y_2008",
  "Q6-9.03~M_2008",
  "Q6-9.03~Y_2008",
  "Q6-9.04~M_2008",
  "Q6-9.04~Y_2008",
  "PDLI-15.01~M_2008",
  "PDLI-15.01~Y_2008",
  "PDLI-15.02~M_2008",
  "PDLI-15.02~Y_2008",
  "PDLI-15.03~M_2008",
  "PDLI-15.03~Y_2008",
  "PDLI-15.04~M_2008",
  "PDLI-15.04~Y_2008",
  "Q6-16E_1A.01.01_2008",
  "Q6-16E_1A.02.01_2008",
  "Q6-16E_1A.03.01_2008",
  "Q6-16E_1A.04.01_2008",
  "Q6-16F.01.01_2008",
  "Q6-16F.02.01_2008",
  "Q6-16F.03.01_2008",
  "Q6-16F.04.01_2008",
  "Q6-16H_A1.01.01_2008",
  "Q6-16H_A1.02.01_2008",
  "Q6-16H_A2.01.01_2008",
  "Q6-16H_A2.01.02_2008",
  "Q6-16H_A2.02.01_2008",
  "Q6-16H_A3.01.01_2008",
  "Q6-16H_A3.02.01_2008",
  "Q6-16H_A4A.01.01_2008",
  "Q6-16H_A4A.02.01_2008",
  "Q6-16H_A5A.01.01_2008",
  "Q6-16H_A5A.02.01_2008",
  "Q6-16I.01_2008",
  "Q6-16I.02_2008",
  "Q6-16I.03_2008",
  "Q6-16I.04_2008",
  "Q6-17.01~M_2008",
  "Q6-17.01~Y_2008",
  "Q6-17.02~M_2008",
  "Q6-17.02~Y_2008",
  "Q6-17.03~M_2008",
  "Q6-17.03~Y_2008",
  "Q6-17.04~M_2008",
  "Q6-17.04~Y_2008",
  "Q6-27D_1A.01.01_2008",
  "Q6-27D_1A.02.01_2008",
  "Q6-27D_1A.03.01_2008",
  "Q6-27D_1A.04.01_2008",
  "Q6-27D_1A.05.01_2008",
  "Q6-27E_A1.01.01_2008",
  "Q6-27E_A1.02.01_2008",
  "Q6-27E_A1.03.01_2008",
  "Q6-27E_A1.04.01_2008",
  "Q6-27E_A1.05.01_2008",
  "Q6-27E_A2.01.01_2008",
  "Q6-27E_A2.01.02_2008",
  "Q6-27E_A2.02.01_2008",
  "Q6-27E_A2.03.01_2008",
  "Q6-27E_A2.03.02_2008",
  "Q6-27E_A2.04.01_2008",
  "Q6-27E_A2.05.01_2008",
  "Q6-27E_A3.01.01_2008",
  "Q6-27E_A3.01.02_2008",
  "Q6-27E_A3.02.01_2008",
  "Q6-27E_A3.03.01_2008",
  "Q6-27E_A3.04.01_2008",
  "Q6-27E_A3.05.01_2008",
  "Q6-27E_A4A.01.01_2008",
  "Q6-27E_A4A.02.01_2008",
  "Q6-27E_A4A.03.01_2008",
  "Q6-27E_A4A.04.01_2008",
  "Q6-27E_A4A.05.01_2008",
  "Q6-27E_A4B.01.02_2008",
  "Q6-27E_A5A.01.01_2008",
  "Q6-27E_A5A.02.01_2008",
  "Q6-27E_A5A.03.01_2008",
  "Q6-27E_A5A.04.01_2008",
  "Q6-27E_A5A.05.01_2008",
  "Q6-27E_A5B.01.02_2008",
  "Q6-27A.01~M_2008",
  "Q6-27A.01~Y_2008",
  "Q6-27A.02~M_2008",
  "Q6-27A.02~Y_2008",
  "Q6-27A.03~M_2008",
  "Q6-27A.03~Y_2008",
  "Q6-27A.04~M_2008",
  "Q6-27A.04~Y_2008",
  "Q6-27A.05~M_2008",
  "Q6-27A.05~Y_2008",
  "Q6-27K.01~M_2008",
  "Q6-27K.01~Y_2008",
  "Q6-27K.02~M_2008",
  "Q6-27K.02~Y_2008",
  "Q6-27K.03~M_2008",
  "Q6-27K.03~Y_2008",
  "Q6-27K.04~M_2008",
  "Q6-27K.04~Y_2008",
  "Q6-27K.05~M_2008",
  "Q6-27K.05~Y_2008",
  "ONJS-8800_2008",
  "Q6-8E_1A.01.01_2010",
  "Q6-8E_1A.02.01_2010",
  "Q6-8E_1A.03.01_2010",
  "Q6-8F.01.01_2010",
  "Q6-8F.02.01_2010",
  "Q6-8F.03.01_2010",
  "ONJS-8800_2010",
  "Q6-8H_A1.01.01_2010",
  "Q6-8H_A1.02.01_2010",
  "Q6-8H_A1.03.01_2010",
  "Q6-8H_A2.01.01_2010",
  "Q6-8H_A2.02.01_2010",
  "Q6-8H_A2.02.02_2010",
  "Q6-8H_A2.03.01_2010",
  "Q6-8H_A3.01.01_2010",
  "Q6-8H_A3.02.01_2010",
  "Q6-8H_A3.03.01_2010",
  "Q6-8H_A4A.01.01_2010",
  "Q6-8H_A4A.02.01_2010",
  "Q6-8H_A4A.03.01_2010",
  "Q6-8H_A5A.01.01_2010",
  "Q6-8H_A5A.02.01_2010",
  "Q6-8H_A5A.03.01_2010",
  "Q6-8I.01_2010",
  "Q6-8I.02_2010",
  "Q6-8I.03_2010",
  "Q6-8I.04_2010",
  "Q6-9.01~M_2010",
  "Q6-9.01~Y_2010",
  "Q6-9.02~M_2010",
  "Q6-9.02~Y_2010",
  "Q6-9.03~M_2010",
  "Q6-9.03~Y_2010",
  "Q6-9.04~M_2010",
  "Q6-9.04~Y_2010",
  "Q6-16E_1A.01.01_2010",
  "Q6-16E_1A.02.01_2010",
  "Q6-16E_1A.03.01_2010",
  "Q6-16E_1A.04.01_2010",
  "Q6-16F.01.01_2010",
  "Q6-16F.02.01_2010",
  "Q6-16F.03.01_2010",
  "Q6-16F.04.01_2010",
  "Q6-16H_A1.01.01_2010",
  "Q6-16H_A1.02.01_2010",
  "Q6-16H_A2.01.01_2010",
  "Q6-16H_A2.02.01_2010",
  "Q6-16H_A3.01.01_2010",
  "Q6-16H_A3.02.01_2010",
  "Q6-16H_A4A.01.01_2010",
  "Q6-16H_A4A.02.01_2010",
  "Q6-16H_A5A.01.01_2010",
  "Q6-16H_A5A.02.01_2010",
  "Q6-16I.01_2010",
  "Q6-16I.02_2010",
  "Q6-16I.03_2010",
  "Q6-16I.04_2010",
  "Q6-17.01~M_2010",
  "Q6-17.01~Y_2010",
  "Q6-17.02~M_2010",
  "Q6-17.02~Y_2010",
  "Q6-17.03~M_2010",
  "Q6-17.03~Y_2010",
  "Q6-17.04~M_2010",
  "Q6-17.04~Y_2010",
  "Q6-27D_1A.01.01_2010",
  "Q6-27D_1A.02.01_2010",
  "Q6-27D_1A.03.01_2010",
  "Q6-27D_1A.04.01_2010",
  "Q6-27D_1A.05.01_2010",
  "Q6-27E_A1.01.01_2010",
  "Q6-27E_A1.02.01_2010",
  "Q6-27E_A1.03.01_2010",
  "Q6-27E_A1.04.01_2010",
  "Q6-27E_A1.05.01_2010",
  "Q6-27E_A2.01.01_2010",
  "Q6-27E_A2.01.02_2010",
  "Q6-27E_A2.02.01_2010",
  "Q6-27E_A2.02.02_2010",
  "Q6-27E_A2.03.01_2010",
  "Q6-27E_A2.03.02_2010",
  "Q6-27E_A2.04.01_2010",
  "Q6-27E_A2.05.01_2010",
  "Q6-27E_A3.01.01_2010",
  "Q6-27E_A3.01.02_2010",
  "Q6-27E_A3.02.01_2010",
  "Q6-27E_A3.03.01_2010",
  "Q6-27E_A3.03.02_2010",
  "Q6-27E_A3.04.01_2010",
  "Q6-27E_A3.05.01_2010",
  "Q6-27E_A4A.01.01_2010",
  "Q6-27E_A4A.02.01_2010",
  "Q6-27E_A4A.03.01_2010",
  "Q6-27E_A4A.04.01_2010",
  "Q6-27E_A4A.05.01_2010",
  "Q6-27E_A4B.01.02_2010",
  "Q6-27E_A4B.03.02_2010",
  "Q6-27E_A5A.01.01_2010",
  "Q6-27E_A5A.02.01_2010",
  "Q6-27E_A5A.03.01_2010",
  "Q6-27E_A5A.04.01_2010",
  "Q6-27E_A5A.05.01_2010",
  "Q6-27E_A5B.01.02_2010",
  "Q6-27E_A5B.03.02_2010",
  "Q6-27A.01~M_2010",
  "Q6-27A.01~Y_2010",
  "Q6-27A.02~M_2010",
  "Q6-27A.02~Y_2010",
  "Q6-27A.03~M_2010",
  "Q6-27A.03~Y_2010",
  "Q6-27A.04~M_2010",
  "Q6-27A.04~Y_2010",
  "Q6-27A.05~M_2010",
  "Q6-27A.05~Y_2010",
  "Q6-27K.01~M_2010",
  "Q6-27K.01~Y_2010",
  "Q6-27K.02~M_2010",
  "Q6-27K.02~Y_2010",
  "Q6-27K.03~M_2010",
  "Q6-27K.03~Y_2010",
  "Q6-27K.04~M_2010",
  "Q6-27K.04~Y_2010",
  "Q6-27K.05~M_2010",
  "Q6-27K.05~Y_2010",
  "Q6-8E_1A.01.01_2012",
  "Q6-8E_1A.02.01_2012",
  "Q6-8E_1A.03.01_2012",
  "Q6-8E_1A.04.01_2012",
  "Q6-8E_1A.05.01_2012",
  "Q6-8H_A1.01.01_2012",
  "Q6-8H_A1.02.01_2012",
  "Q6-8H_A1.03.01_2012",
  "Q6-8H_A1.04.01_2012",
  "Q6-8H_A1.05.01_2012",
  "Q6-8H_A2.01.01_2012",
  "Q6-8H_A2.02.01_2012",
  "Q6-8H_A2.03.01_2012",
  "Q6-8H_A2.04.01_2012",
  "Q6-8H_A2.05.01_2012",
  "Q6-8H_A3.01.01_2012",
  "Q6-8H_A3.02.01_2012",
  "Q6-8H_A3.03.01_2012",
  "Q6-8H_A3.04.01_2012",
  "Q6-8H_A3.05.01_2012",
  "Q6-8H_A4A.01.01_2012",
  "Q6-8H_A4A.02.01_2012",
  "Q6-8H_A4A.03.01_2012",
  "Q6-8H_A4A.04.01_2012",
  "Q6-8H_A4A.05.01_2012",
  "Q6-8H_A5A.01.01_2012",
  "Q6-8H_A5A.02.01_2012",
  "Q6-8H_A5A.03.01_2012",
  "Q6-8H_A5A.04.01_2012",
  "Q6-8H_A5A.05.01_2012",
  "Q6-8I.01_2012",
  "Q6-8I.02_2012",
  "Q6-8I.03_2012",
  "Q6-8I.04_2012",
  "Q6-8I.05_2012",
  "Q6-9.01~M_2012",
  "Q6-9.01~Y_2012",
  "Q6-9.02~M_2012",
  "Q6-9.02~Y_2012",
  "Q6-9.03~M_2012",
  "Q6-9.03~Y_2012",
  "Q6-9.04~M_2012",
  "Q6-9.04~Y_2012",
  "Q6-17.01~M_2012",
  "Q6-17.01~Y_2012",
  "Q6-17.02~M_2012",
  "Q6-17.02~Y_2012",
  "Q6-17.03~M_2012",
  "Q6-17.03~Y_2012",
  "Q6-17.04~M_2012",
  "Q6-17.04~Y_2012",
  "Q6-17.05~M_2012",
  "Q6-17.05~Y_2012",
  "Q6-16E_1A.01.01_2012",
  "Q6-16E_1A.02.01_2012",
  "Q6-16E_1A.03.01_2012",
  "Q6-16F.01.01_2012",
  "Q6-16F.02.01_2012",
  "Q6-16F.03.01_2012",
  "Q6-16H_A1.01.01_2012",
  "Q6-16H_A1.02.01_2012",
  "Q6-16H_A2.01.01_2012",
  "Q6-16H_A2.01.02_2012",
  "Q6-16H_A2.02.01_2012",
  "Q6-16H_A3.01.01_2012",
  "Q6-16H_A3.02.01_2012",
  "Q6-16H_A4A.01.01_2012",
  "Q6-16H_A4A.02.01_2012",
  "Q6-16H_A5A.01.01_2012",
  "Q6-27D_1A.01.01_2012",
  "Q6-27D_1A.02.01_2012",
  "Q6-27D_1A.03.01_2012",
  "Q6-27D_1A.04.01_2012",
  "Q6-27D_1A.05.01_2012",
  "Q6-27E_A1.01.01_2012",
  "Q6-27E_A1.02.01_2012",
  "Q6-27E_A1.03.01_2012",
  "Q6-27E_A1.04.01_2012",
  "Q6-27E_A1.05.01_2012",
  "Q6-27E_A2.01.01_2012",
  "Q6-27E_A2.01.02_2012",
  "Q6-27E_A2.02.01_2012",
  "Q6-27E_A2.02.02_2012",
  "Q6-27E_A2.03.01_2012",
  "Q6-27E_A2.03.02_2012",
  "Q6-27E_A2.04.01_2012",
  "Q6-27E_A2.05.01_2012",
  "Q6-27E_A3.01.01_2012",
  "Q6-27E_A3.01.02_2012",
  "Q6-27E_A3.02.01_2012",
  "Q6-27E_A3.02.02_2012",
  "Q6-27E_A3.03.01_2012",
  "Q6-27E_A3.03.02_2012",
  "Q6-27E_A3.04.01_2012",
  "Q6-27E_A3.05.01_2012",
  "Q6-27E_A4A.01.01_2012",
  "Q6-27E_A4A.02.01_2012",
  "Q6-27E_A4A.03.01_2012",
  "Q6-27E_A4A.04.01_2012",
  "Q6-27E_A4A.05.01_2012",
  "Q6-27E_A4B.01.02_2012",
  "Q6-27E_A4B.02.02_2012",
  "Q6-27E_A4B.03.02_2012",
  "Q6-27E_A5A.01.01_2012",
  "Q6-27E_A5A.02.01_2012",
  "Q6-27E_A5A.03.01_2012",
  "Q6-27E_A5A.04.01_2012",
  "Q6-27E_A5A.05.01_2012",
  "Q6-27E_A5B.01.02_2012",
  "Q6-27E_A5B.02.02_2012",
  "Q6-27E_A5B.03.02_2012",
  "Q6-27A_CHK1.01_2012",
  "Q6-27A_CHK1.02_2012",
  "Q6-27A_CHK1.03_2012",
  "Q6-27A_CHK1.04_2012",
  "Q6-27A_CHK1.05_2012",
  "Q6-27K.01~M_2012",
  "Q6-27K.01~Y_2012",
  "Q6-27K.02~M_2012",
  "Q6-27K.02~Y_2012",
  "Q6-27K.03~M_2012",
  "Q6-27K.03~Y_2012",
  "Q6-27K.04~M_2012",
  "Q6-27K.04~Y_2012",
  "Q6-27K.05~M_2012",
  "Q6-27K.05~Y_2012",
  "ONJS-8800_2012",
  "Q6-9.01~M_2014",
  "Q6-9.01~Y_2014",
  "Q6-9.02~M_2014",
  "Q6-9.02~Y_2014",
  "Q6-9.03~M_2014",
  "Q6-9.03~Y_2014",
  "Q6-9.04~M_2014",
  "Q6-9.04~Y_2014",
  "Q6-8E_1A.01.01_2014",
  "Q6-8E_1A.02.01_2014",
  "Q6-8E_1A.03.01_2014",
  "Q6-8E_1A.04.01_2014",
  "Q6-8H_A1.01.01_2014",
  "Q6-8H_A1.02.01_2014",
  "Q6-8H_A1.03.01_2014",
  "Q6-8H_A1.04.01_2014",
  "Q6-8H_A1.05.01_2014",
  "Q6-8H_A2.01.01_2014",
  "Q6-8H_A2.01.02_2014",
  "Q6-8H_A2.02.01_2014",
  "Q6-8H_A2.02.02_2014",
  "Q6-8H_A2.03.01_2014",
  "Q6-8H_A2.03.02_2014",
  "Q6-8H_A2.04.01_2014",
  "Q6-8H_A2.04.02_2014",
  "Q6-8H_A2.05.01_2014",
  "Q6-8H_A3.01.01_2014",
  "Q6-8H_A3.01.02_2014",
  "Q6-8H_A3.02.01_2014",
  "Q6-8H_A3.02.02_2014",
  "Q6-8H_A3.03.01_2014",
  "Q6-8H_A3.03.02_2014",
  "Q6-8H_A3.04.01_2014",
  "Q6-8H_A3.04.02_2014",
  "Q6-8H_A3.05.01_2014",
  "Q6-8H_A4A.01.01_2014",
  "Q6-8H_A4A.02.01_2014",
  "Q6-8H_A4A.03.01_2014",
  "Q6-8H_A4A.04.01_2014",
  "Q6-8H_A4A.05.01_2014",
  "Q6-8H_A4B.01.02_2014",
  "Q6-8H_A4B.02.02_2014",
  "Q6-8H_A4B.03.02_2014",
  "Q6-8H_A4B.04.02_2014",
  "Q6-8H_A5A.01.01_2014",
  "Q6-8H_A5A.02.01_2014",
  "Q6-8H_A5A.03.01_2014",
  "Q6-8H_A5A.04.01_2014",
  "Q6-8H_A5A.05.01_2014",
  "Q6-8H_A5B.01.02_2014",
  "Q6-8H_A5B.02.02_2014",
  "Q6-8H_A5B.03.02_2014",
  "Q6-8H_A5B.04.02_2014",
  "ONJS-8800_2014",
  "Q6-8.01_2016",
  "Q6-8.02_2016",
  "Q6-8.03_2016",
  "Q6-8.04_2016",
  "Q6-9.01~M_2016",
  "Q6-9.01~Y_2016",
  "Q6-9.02~M_2016",
  "Q6-9.02~Y_2016",
  "Q6-9.03~M_2016",
  "Q6-9.03~Y_2016",
  "Q6-9.04~M_2016",
  "Q6-9.04~Y_2016",
  "Q6-8E_1A.01.01_2016",
  "Q6-8E_1A.02.01_2016",
  "Q6-8E_1A.03.01_2016",
  "Q6-8E_1A.04.01_2016",
  "Q6-8E_1A.05.01_2016",
  "Q6-8H_A1.01.01_2016",
  "Q6-8H_A1.02.01_2016",
  "Q6-8H_A1.03.01_2016",
  "Q6-8H_A1.04.01_2016",
  "Q6-8H_A1.05.01_2016",
  "Q6-8H_A2.01.01_2016",
  "Q6-8H_A2.01.02_2016",
  "Q6-8H_A2.02.01_2016",
  "Q6-8H_A2.02.02_2016",
  "Q6-8H_A2.03.01_2016",
  "Q6-8H_A2.03.02_2016",
  "Q6-8H_A2.04.01_2016",
  "Q6-8H_A2.05.01_2016",
  "Q6-8H_A3.01.01_2016",
  "Q6-8H_A3.01.02_2016",
  "Q6-8H_A3.02.01_2016",
  "Q6-8H_A3.02.02_2016",
  "Q6-8H_A3.03.01_2016",
  "Q6-8H_A3.03.02_2016",
  "Q6-8H_A3.04.01_2016",
  "Q6-8H_A3.05.01_2016",
  "Q6-8H_A4A.01.01_2016",
  "Q6-8H_A4A.02.01_2016",
  "Q6-8H_A4A.03.01_2016",
  "Q6-8H_A4A.04.01_2016",
  "Q6-8H_A4A.05.01_2016",
  "Q6-8H_A4B.01.02_2016",
  "Q6-8H_A4B.02.02_2016",
  "Q6-8H_A4B.03.02_2016",
  "Q6-8H_A5A.01.01_2016",
  "Q6-8H_A5A.02.01_2016",
  "Q6-8H_A5A.03.01_2016",
  "Q6-8H_A5A.04.01_2016",
  "Q6-8H_A5A.05.01_2016",
  "Q6-8H_A5B.01.02_2016",
  "Q6-8H_A5B.02.02_2016",
  "Q6-8H_A5B.03.02_2016",
  "ONJS-8800_2016",
  "Q6-27A.01~M_2002",
  "Q6-27A.01~Y_2002",
  "Q6-27A.02~M_2002",
  "Q6-27A.02~Y_2002",
  "Q6-27A.03~M_2002",
  "Q6-27A.03~Y_2002",
  "Q6-27A.04~M_2002",
  "Q6-27A.04~Y_2002",
  "Q6-27A.05~M_2002",
  "Q6-27A.05~Y_2002",
  "Q6-27A.06~M_2002",
  "Q6-27A.06~Y_2002",
  "Q6-27A.07~M_2002",
  "Q6-27A.07~Y_2002",
  "Q6-27I.01_2002",
  "Q6-27I.02_2002",
  "Q6-27I.03_2002",
  "Q6-27I.04_2002",
  "Q6-27I.05_2002",
  "Q6-27I.06_2002",
  "Q6-27I.07_2002",
  "Q6-27K.01~M_2002",
  "Q6-27K.01~Y_2002",
  "Q6-27K.02~M_2002",
  "Q6-27K.02~Y_2002",
  "Q6-27K.03~M_2002",
  "Q6-27K.03~Y_2002",
  "Q6-27K.04~M_2002",
  "Q6-27K.04~Y_2002",
  "Q6-27K.05~M_2002",
  "Q6-27K.05~Y_2002",
  "Q6-27K.06~M_2002",
  "Q6-27K.06~Y_2002",
  "Q6-27I.01_2004",
  "Q6-27I.02_2004",
  "Q6-27I.03_2004",
  "Q6-27I.04_2004",
  "Q6-27I.05_2004",
  "Q6-27I.01_2006",
  "Q6-27I.02_2006",
  "Q6-27I.03_2006",
  "Q6-27I.04_2006",
  "Q6-27I.05_2006",
  "Q6-27I.01_2008",
  "Q6-27I.02_2008",
  "Q6-27I.03_2008",
  "Q6-27I.04_2008",
  "Q6-27I.05_2008",
  "Q6-27I.01_2010",
  "Q6-27I.02_2010",
  "Q6-27I.03_2010",
  "Q6-27I.04_2010",
  "Q6-27I.05_2010",
  "Q6-27I.01_2012",
  "Q6-27I.02_2012",
  "Q6-27I.03_2012",
  "Q6-27I.04_2012",
  "Q6-27I.05_2012")

new_data <- read_table2(str_c(raw_folder, "on_jobs_raw.dat"), 
                        col_names = col_names,
                        col_types = cols(.default = col_double()))

# This is data downloaded from errata
col_names_2 <- c(
  "CASEID_1979",
  "Q6-8E_1A.01.01_2002",
  "Q6-8E_1A.01.02_2002",
  "Q6-8E_1A.02.01_2002",
  "Q6-8E_1A.02.02_2002",
  "Q6-8E_1A.03.01_2002",
  "Q6-8E_1A.03.02_2002",
  "Q6-8E_1A.04.01_2002",
  "Q6-8E_1A.05.01_2002",
  "Q6-15.01~M_2002",
  "Q6-15.01~Y_2002",
  "Q6-15.02~M_2002",
  "Q6-15.02~Y_2002",
  "Q6-15.03~M_2002",
  "Q6-15.03~Y_2002",
  "Q6-15.04~M_2002",
  "Q6-15.04~Y_2002",
  "Q6-15.05~M_2002",
  "Q6-15.05~Y_2002",
  "Q6-9.01~M_2002",
  "Q6-9.01~Y_2002",
  "Q6-9.02~M_2002",
  "Q6-9.02~Y_2002",
  "Q6-9.03~M_2002",
  "Q6-9.03~Y_2002",
  "Q6-9.04~M_2002",
  "Q6-9.04~Y_2002",
  "Q6-17.01~M_2002",
  "Q6-17.01~Y_2002",
  "Q6-17.02~M_2002",
  "Q6-17.02~Y_2002",
  "Q6-17.03~M_2002",
  "Q6-17.03~Y_2002",
  "Q6-17.04~M_2002",
  "Q6-17.04~Y_2002",
  "Q6-17.05~M_2002",
  "Q6-17.05~Y_2002",
  "Q6-8E_1A.01.01_2004",
  "Q6-8E_1A.01.02_2004",
  "Q6-8E_1A.02.01_2004",
  "Q6-8E_1A.02.02_2004",
  "Q6-8E_1A.03.01_2004",
  "Q6-8E_1A.04.01_2004",
  "NEWEMP_STARTDATE.01~M_2012",
  "NEWEMP_STARTDATE.01~Y_2012",
  "NEWEMP_STARTDATE.02~M_2012",
  "NEWEMP_STARTDATE.02~Y_2012",
  "NEWEMP_STARTDATE.03~M_2012",
  "NEWEMP_STARTDATE.03~Y_2012",
  "NEWEMP_STARTDATE.04~M_2012",
  "NEWEMP_STARTDATE.04~Y_2012",
  "NEWEMP_STARTDATE.05~M_2012",
  "NEWEMP_STARTDATE.05~Y_2012",
  "NEWEMP_STARTDATE.06~M_2012",
  "NEWEMP_STARTDATE.06~Y_2012",
  "NEWEMP_STARTDATE.07~M_2012",
  "NEWEMP_STARTDATE.07~Y_2012",
  "NEWEMP_STARTDATE.08~M_2012",
  "NEWEMP_STARTDATE.08~Y_2012",
  "NEWEMP_CURFLAG.01_2012",
  "NEWEMP_CURFLAG.02_2012",
  "NEWEMP_CURFLAG.03_2012",
  "NEWEMP_CURFLAG.04_2012",
  "NEWEMP_CURFLAG.05_2012",
  "NEWEMP_CURFLAG.06_2012",
  "NEWEMP_CURFLAG.07_2012",
  "NEWEMP_CURFLAG.08_2012"
)

new_data_2 <- read_table2(str_c(raw_folder, "errata.dat"),  
                          col_names = col_names_2,
                          col_types = cols(.default = col_double()))

# Many jobs are being dropped in 2014-2016 because too much missing data,
# but it looks like these jobs should exist. These variables from looped
# should help determine if a job exists
col_names_3 <- c(
  "CASEID_1979",
  "Q6-8_JOBVER_1A.01_2014",
  "Q6-8_JOBVER_1A.02_2014",
  "Q6-8_JOBVER_1A.03_2014",
  "Q6-8_JOBVER_1A.04_2014",
  "Q6-8_JOBVER_1B.01_2014",
  "Q6-8_JOBVER_1B.02_2014",
  "Q6-8_JOBVER_1B.03_2014",
  "Q6-8_JOBVER_1C.01_2014",
  "Q6-8_JOBVER_1C.02_2014",
  "Q6-8_JOBVER_1C.03_2014",
  "Q6-8_JOBVER_1A.01_2016",
  "Q6-8_JOBVER_1A.02_2016",
  "Q6-8_JOBVER_1A.03_2016",
  "Q6-8_JOBVER_1A.04_2016",
  "Q6-8_JOBVER_1B.01_2016",
  "Q6-8_JOBVER_1B.02_2016",
  "Q6-8_JOBVER_1C.01_2016",
  "Q6-8_JOBVER_1C.02_2016",
  "Q6-8_JOBVER_1C.03_2016"
)

new_data_3 <- read_table2(str_c(raw_folder, "looped.dat"), 
                          col_names = col_names_3,
                          col_types = cols(.default = col_double()))

# Handle missing values

new_data[new_data == -1] = NA  # Refused 
new_data[new_data == -2] = NA  # Dont know 
new_data[new_data == -3] = NA  # Invalid missing 
new_data[new_data == -4] = NA  # Valid missing 
new_data[new_data == -5] = NA  # Non-interview 

new_data_2[new_data_2 == -1] = NA  # Refused 
new_data_2[new_data_2 == -2] = NA  # Dont know 
new_data_2[new_data_2 == -3] = NA  # Invalid missing 
new_data_2[new_data_2 == -4] = NA  # Valid missing 
new_data_2[new_data_2 == -5] = NA  # Non-interview 

new_data_3[new_data_3 == -1] = NA  # Refused 
new_data_3[new_data_3 == -2] = NA  # Dont know 
new_data_3[new_data_3 == -3] = NA  # Invalid missing 
new_data_3[new_data_3 == -4] = NA  # Valid missing 
new_data_3[new_data_3 == -5] = NA  # Non-interview 

# Join these data sets based on CASEID_1979
new_data <- inner_join(new_data, new_data_2, by="CASEID_1979")
new_data <- inner_join(new_data, new_data_3, by="CASEID_1979")

# Create a function that renames date variables.
# It takes an old variable name, job number, year, and job type and 
# returns a new name
rename_oj <- 
  function(data, old_name, job_n, year, type, new_name,
           fill = "") {
    add <- 0 + 5 * (type == "p") + 10 * (type == "n")
    var <- str_c(old_name, job_n, fill, "_", year)
    num <- sprintf("%02d", job_n + add)
    new_var <- str_c(new_name, num, year, sep = "_")
    if (check_exists(var, data)) {
      data <- rename(data, !!new_var := !!var)
    } else {
      data
    }
  }

# Define Variables -------------------------------------------------------

# Here is a list of variable names that will be useful later
# Note use d for dli, p for pli, n for nj
type_list <- c("d", "p", "n")

# Date began job
q_start_p <- "PDLI-15.0"
q_start_n <- "Q6-27A.0"
q_start <- c("", q_start_p, q_start_n)

# Question about currently working job
q_cur_d <- "Q6-8I.0"
q_cur_p <- "Q6-16I.0"
q_cur_n <- "Q6-27I.0"
q_cur <- c(q_cur_d, q_cur_p, q_cur_n)

# Question about date stopped working job
q_end_d <- "Q6-9.0"
q_end_p <- "Q6-17.0"
q_end_n <- "Q6-27K.0"
q_end <- c(q_end_d, q_end_p, q_end_n)

# Job worked most
q_most <- "ONJS-8800_"

# Began question Loop
q_loop_d <- "Q6-8E_1A.0"
q_loop_p <- "Q6-16E_1A.0"
q_loop_n <- "Q6-27D_1A.0"
q_loop <- c(q_loop_d, q_loop_p, q_loop_n)

# Job preassigned traditional
q_trad_d <- "Q6-8F.0"
q_trad_p <- "Q6-16F.0"
q_trad <- c(q_trad_d, q_trad_p, "")

# Questions about job type
q_type_d <- "Q6-8H_A"
q_type_p <- "Q6-16H_A"
q_type_n <- "Q6-27E_A"
q_type <- c(q_type_d, q_type_p, q_type_n)

# Temp variables for 2014-2016
q_temp <- "Q6-8_JOBVER_1"
temp_end <- c("A", "B", "C")

# Year and Month
y <- "~Y"
m <- "~M"
ym <- c(y, m)

# Rename Variables --------------------------------------------------------

# rename CASEID_1979 to case_id
new_data <- rename(new_data, case_id = CASEID_1979)

# Clean and relabel all data below. To make things a bit easier
# later on, will only look at first 5 jobs within each 
# category of d/p/n
# Will also change job numbers:
# d jobs will keep their number
# p jobs will be number + 5
# n jobs will be number + 10
# This makes it easier because I only need 1 name for variables

# List date variables together
q_date <- list(q_start, q_end)
q_date_new <- c("month_start_job", "month_end_job")
ym_new <- c("_y", "_m")

# List job type variables together
q_type_new <- c("self_emp", "indep_con", "temp_work",
                "on_call", "outsourced")

# All non-date/type variables
q_misc <- list(q_cur, q_loop, q_trad)
q_misc_new <- c("current_job", "looped", "pre_trad")
misc_fill <- c("", ".01", ".01")

for (job_n in 1:5) {
  for (year in seq(2002, 2016, by=2)) {
    for (type_n in 1:3) {
      
      # Rename date variables
      for (date_n in 1:2) {
        for (var_n in seq_along(q_date_new)) {
          old_name <- q_date[[var_n]][type_n]
          type <- type_list[type_n]
          date <- ym[date_n]
          new_name <- str_c(q_date_new[var_n], ym_new[date_n])
          new_data <- 
            rename_oj(new_data, old_name, job_n, year, type,
                      new_name, fill = date)
        }
      }
      
      # Rename type variables
      for (round_n in 1:2) {
        for (var_n in seq_along(q_type_new)) {
          AB <- ""
          if (var_n > 3){
            if (round_n == 1){
              AB <- "A"
            } else {
              AB <- "B"
            }
          }
          old_name <- str_c(q_type[type_n], var_n, AB, ".0")
          type <- type_list[type_n]
          new_name <- str_c(q_type_new[var_n], "_", round_n)
          f <- str_c(".0", round_n)
          new_data <-
            rename_oj(new_data, old_name, job_n, year, type,
                      new_name, fill = f)
        }
      }
      
      # Rename miscellaneous variables
      for (var_n in seq_along(q_misc_new)) {
        old_name <- q_misc[[var_n]][type_n]
        type <- type_list[type_n]
        new_data <- 
          rename_oj(new_data, old_name, job_n, year, type,
                    q_misc_new[var_n], fill = misc_fill[var_n])
      }
      
      # Report job worked most each year 
      # (data is each year, create variable for each job)
      add <- 0 + 5 * (type_n == 2) + 10 * (type_n == 3)
      num <- sprintf("%02d", job_n + add)
      old_var <- str_c(q_most, year)
      new_var <- str_c("most_job_", num, "_", year)
      new_data <- mutate(new_data, !!new_var := new_data[[old_var]])
    }
  }
  
  # Handling exceptions: some years/types have different names
  
  # 2002: month_start_job p had a different name
  for (date_n in 1:2) {
    date <- ym[date_n]
    new_name <- str_c("month_start_job", ym_new[date_n])
    new_data <- 
      rename_oj(new_data, "Q6-15.0", job_n, 2002, "p", 
                new_name, fill = date)
  }
  
  # 2012: month_start_job n had a different name
  for (date_n in 1:2) {
    date <- ym[date_n]
    new_name <- str_c("month_start_job", ym_new[date_n])
    new_data <-
      rename_oj(new_data, "NEWEMP_STARTDATE.0", job_n, 2012,
                "n", new_name, fill = date)
  }
  
  # 2014-2016: lots of missing data leads to too many drops. 
  # These variables should exist for viable jobs, keep them as 
  # temp_a, temp_b, and temp_c when droping missing values
  # then drop variables
  for (year in c(2014, 2016)) {
    for (temp_n in 1:3) {
      old_name <- str_c(q_temp, temp_end[temp_n], ".0")
      new_name <- str_c("temp_", temp_end[temp_n])
      new_data <-
        rename_oj(new_data, old_name, job_n, year, "d",
                  new_name, fill = "")
    }
  }
  
  # 2016: current_job d had a different name
  new_data <- 
    rename_oj(new_data, "Q6-8.0", job_n, 2016, "d",
              "current_job", fill = "")
}


# Reshape and Clean -------------------------------------------------------

# Create list of variables to loop over jobs
vary <- c("month_start_job", "month_end_job", "current_job",
          "most_job", "looped", "pre_trad", "self_emp", "indep_con",
          "temp_work", "on_call", "outsourced",
          "temp_A", "temp_B", "temp_C")

constant <- "case_id"

# Variables we keep
keep <- str_c("^(", str_c(vary, collapse = "|"), "|", constant, ")")

# Transform data from wide to long
long <- new_data |> 
  dplyr::select(matches(keep)) |> 
  gather(matches(str_c("^(", str_c(vary, collapse="|"), ")")),
         key=key, value=val) |>
  extract(key, into=c("variable", "key"), 
          regex="(.+)(_[01]?._20..)$") |>
  filter(!is.na(variable), !is.na(key)) |>
  spread(key=variable, value=val) |>
  mutate(key = substring(key, 2)) |>
  separate(key, sep="_", into=c("job", "int_year"), convert = TRUE) |> 
  # Keep only observations with at least one job specific bit of data
  filter_at(vars(-case_id, -job, -int_year, -most_job), 
            any_vars(!is.na(.))) |> 
  dplyr::select(-temp_A, -temp_B, -temp_C)

# Fill in missing job type data
# 1. For job types, 1 vs 2 are first and second time though the loop.
# set type = 2 if available, 1 otherwise
find_type <- function(data, var) {
  var_1 <- str_c(var, "_1")
  var_2 <- str_c(var, "_2")
  ifelse(!is.na(data[[var_2]]), data[[var_2]], data[[var_1]])
}

# Self-emp has no 2, so remove it from the list and rename it self_emp
long <- rename(long, self_emp = self_emp_1)

for (var in q_type_new[-1]) {
  long[[var]] <- find_type(long, var)
}

# 2. Default job type is traditional. Set as traditional 
# if pre_trad == 1 or if no other job type == 1 and 
# job went through loop.
# Appears looped job 5 2014 is missing. Set == 1 if !is.na(self_emp) 
long <- long |>
  select(-ends_with("1"), -ends_with("2")) |> 
  mutate(
    looped = ifelse(is.na(looped) & !is.na(self_emp), 1, looped),
    traditional = 
      ifelse(pre_trad %in% 1, 1,
        looped - pmax.int(self_emp, temp_work, on_call, outsourced,
                      na.rm = T)
        ),
    # If NA for all job types, assume traditional
    traditional = ifelse(is.na(traditional), looped, traditional),
    # If pre_trad is na, set to 0
    pre_trad = ifelse(!is.na(pre_trad), pre_trad, 0)
    )


# 3. If answered a job type question positively, assume all other
# types are 0
# If report multiple job types, prioritize indep_con, then
# outsourced, then temp, then self_emp, then on_call, then traditional
# If person ever outsourced, set ever_out_oj == 1
not_type <- function(type, comp = 1) {
  ifelse(is.na(type), 1 - comp, type)
} 

avoid_double <- function(type, comp) {
  ifelse(type == 1, type - comp, type)
}

avoid_doubles <- function(data, vec) {
  len <- length(vec)
  for (i in len:2) {
    for (j in (i - 1):1) {
      data[[vec[i]]] <- avoid_double(data[[vec[i]]], data[[vec[j]]])
    }
  }
  data
}

type_vec <- c("indep_con", "outsourced", "temp_work", "self_emp", 
               "on_call",  "traditional")

long <- long |> 
  mutate(
    looped = ifelse(
      !is.na(looped), looped,
      pmax(traditional, self_emp, indep_con,
           temp_work, on_call, outsourced, na.rm = T))
    ) |> 
  mutate_at(type_vec, not_type, comp = long$looped) |>
  avoid_doubles(type_vec) |> 
  # check_exists to make sure avoid_doubles works
  mutate(
    num_types = rowSums(cbind(traditional, self_emp, indep_con,
                              temp_work, on_call, outsourced),
                        na.rm = T)
    ) |> 
  group_by(case_id) |> 
  mutate(
    ever_out_oj = ifelse(all(is.na(outsourced)), 0, 
                         max(outsourced, na.rm = T))
    ) |> 
  ungroup()

# 4. Turn month start/end job into dates using lubridate
# Set dates before 1979 as NA
long <- long |>
  mutate(
    month_start_job = ymd(str_c(month_start_job_y, 
                                month_start_job_m, 1, sep="-")),
    month_end_job = ymd(str_c(month_end_job_y, 
                              month_end_job_m, 1, sep="-")),
    month_start_job = as_date(ifelse(
      month_start_job < ymd("1970-01-01"), NA,
      month_start_job)),
    month_end_job = as_date(ifelse(month_end_job < ymd("1970-01-01"),
                                   NA, month_end_job))
    ) |> 
  select(-ends_with("_m"), -ends_with("_y"))


# 5. Want to rank jobs by date last worked, with current jobs ranked
# first
# To do this
# 1. Group by case_id and year
# (If !is.na(month_end_job) and is.na(current_job), set 
# current_job == 0; also add up all current jobs)
# 2. Create rank_date = large number if current_job = 1, 
# month_end_job else (including if current_job NA)
# 3. Create rank using min_rank descending on rank_date
# 4. Group jobs with same rank together. Generate row_number. 
# For current jobs
# a. If most_job = row number, this stays rank 1
# b. If most_job > row_number, rank becomes 1 + rank_temp
# c. If most_job < row_number, rank becomes rank_temp
# Otherwise if row_number > 1, rank = rank + row_number - 1
# If most_job > tot_current, then subtract 1 from rank of current 
# Lastly, drop all unneeded variables
long <- long |>
  group_by(case_id, int_year) |> 
  mutate(
    current_job = ifelse(is.na(current_job) & !is.na(month_end_job),
                         0, current_job),
    tot_current = sum(current_job, na.rm = T),
    rank_date = ifelse(
      is.na(current_job), month_end_job, ifelse(current_job == 1,
                                                1e6, month_end_job)),
    rank_date = ifelse(is.na(rank_date), 0, rank_date),
    rank = min_rank(desc(rank_date))
  ) |> 
  group_by(rank, .add = T) |> 
  arrange(case_id, int_year, job) |> 
  mutate(row_num = row_number(rank)) |> 
  ungroup() |> 
  mutate(
    rank = ifelse(
      (current_job == 1 & !is.na(most_job)) %in% T,
      ifelse(most_job == row_num, 1,
             ifelse(most_job > row_num & most_job <= tot_current,
                    row_num + 1, row_num)),
      ifelse(row_num > 1, rank + row_num - 1, rank)
    )
  ) |> 
  select(-most_job, -num_types, -current_job, -tot_current,
         -rank_date, -row_num)

# # Data from 2014 and 2016 interviews in on jobs are less reliable.
# Drop these years
# long <- filter(long, int_year < 2014) 

# Save the data
write_csv(long, str_c(clean_folder, "on_jobs_clean.csv"))