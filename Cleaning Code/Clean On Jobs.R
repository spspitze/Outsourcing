# This file takes raw data from errata and on_jobs_raw.
# It cleans it then saves it as on_jobs_clean
rm(list = ls())

library(lubridate)
library(multiplex)
library(tidyverse)

# Folders of interest
raw_folder <- "../Raw Data/"
clean_folder <- "../Cleaned Data/"

new_data <- read_table2(str_c(raw_folder, "on_jobs_raw.dat"))
names(new_data) <- c(
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


# This is data downloaded from errata
new_data_2 <- read_table2(str_c(raw_folder, "errata.dat"))
names(new_data_2) <- c(
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

# Handle missing values

new_data[new_data == -1] = NA  # Refused 
new_data[new_data == -2] = NA  # Dont know 
new_data[new_data == -3] = NA  # Invalid missing 
new_data[new_data == -4] = NA  # Valid missing 
new_data[new_data == -5] = NA  # Non-interview 

# Handle missing values

new_data_2[new_data_2 == -1] = NA  # Refused 
new_data_2[new_data_2 == -2] = NA  # Dont know 
new_data_2[new_data_2 == -3] = NA  # Invalid missing 
new_data_2[new_data_2 == -4] = NA  # Valid missing 
new_data_2[new_data_2 == -5] = NA  # Non-interview 

# Join these data sets based on CASEID_1979
new_data <- inner_join(new_data, new_data_2, by="CASEID_1979")

# Create a function that, given a variable name and data set, returns T
# if variable is in names, F else
check <- function(var, data){
  return(var %in% colnames(data))
}

#************************************************************************************************************

# Start cleaning data

# Here is a list of variable names that will be useful later
# Note use d for dli, p for pli, n for nj

# Date began job
q_start_p <- "PDLI-15.0"
q_start_n <- "Q6-27A.0"

# Question about currently working job
q_cur_d <- "Q6-8I.0"
q_cur_p <- "Q6-16I.0"
q_cur_n <- "Q6-27I.0"

# Question about date stopped working job
q_end_d <- "Q6-9.0"
q_end_p <- "Q6-17.0"
q_end_n <- "Q6-27K.0"

# Job worked most
q_most <- "ONJS-8800_"

# Began question Loop
q_loop_d <- "Q6-8E_1A.0"
q_loop_p <- "Q6-16E_1A.0"
q_loop_n <- "Q6-27D_1A.0"

# Job preassigned traditional
q_trad_d <- "Q6-8F.0"
q_trad_p <- "Q6-16F.0"

# Questions about job type
q_type_d <- "Q6-8H_A"
q_type_p <- "Q6-16H_A"
q_type_n <- "Q6-27E_A"

# Year and Month
y <- "~Y_"
m <- "~M_"

# rename CASEID_1979 to case_id
new_data <- rename(new_data, case_id = CASEID_1979)

# Clean and relabel all data below. To make things a bit easier later on,
# will only look at first 5 jobs within each category of d/p/n
# Will also change job numbers:
# d jobs will keep their number
# p jobs will be number + 5
# n jobs will be number + 10
# This makes it easier because I only need 1 name for variables

# Loop over years and jobs
for (year in seq(2002, 2016, by=2)){
  for (job in seq(1, 5)){
    if (check(str_c(q_start_p, job, m, year), new_data)){
      new_data[[str_c("month_start_job_", job + 5, "_", year)]] <- 
        ifelse(
          !is.na(new_data[[str_c(q_start_p, job, m, year)]]) &
            !is.na(new_data[[str_c(q_start_p, job, y, year)]]),
          make_date(new_data[[str_c(q_start_p, job, y, year)]],
                    new_data[[str_c(q_start_p, job, m, year)]]),
          ifelse(
            !is.na(new_data[[str_c(q_start_p, job, m, year)]]),
            make_date(0, new_data[[str_c(q_start_p, job, m, year)]]),
            make_date(new_data[[str_c(q_start_p, job, y, year)]])
            )
          )
        }
    # Began job
    if (check(str_c(q_start_n, job, m, year), new_data)){
      new_data[[str_c("month_start_job_", job + 10, "_", year)]] <- 
        ifelse(
          !is.na(new_data[[str_c(q_start_n, job, m, year)]]) &
            !is.na(new_data[[str_c(q_start_n, job, y, year)]]),
          make_date(new_data[[str_c(q_start_n, job, y, year)]],
            new_data[[str_c(q_start_n, job, m, year)]]),
          ifelse(
            !is.na(new_data[[str_c(q_start_n, job, m, year)]]),
            make_date(0, new_data[[str_c(q_start_n, job, m, year)]]),
            make_date(new_data[[str_c(q_start_n, job, y, year)]])
          )
        )
    }
    
    # In 2002, began p had different name
    if (check(str_c("Q6-15.0", job, m, year), new_data)){
      new_data[[str_c("month_start_job_", job + 5, "_", year)]] <- 
        ifelse(
          !is.na(new_data[[str_c("Q6-15.0", job, m, year)]]) &
            !is.na(new_data[[str_c("Q6-15.0", job, y, year)]]),
          make_date(new_data[[str_c("Q6-15.0", job, y, year)]],
            new_data[[str_c("Q6-15.0", job, m, year)]]),
          ifelse(
            !is.na(new_data[[str_c("Q6-15.0", job, m, year)]]),
            make_date(0, new_data[[str_c("Q6-15.0", job, m, year)]]),
            make_date(new_data[[str_c("Q6-15.0", job, y, year)]])
          )
        )
    }
    
    # In 2012, began n had different name
    if (check(str_c("NEWEMP_STARTDATE.", job, m, year), new_data)){
      new_data[[str_c("month_start_job_", job + 10, "_", year)]] <- 
        ifelse(
          !is.na(new_data[[str_c("NEWEMP_STARTDATE.", job, m, year)]]) &
            !is.na(new_data[[str_c("NEWEMP_STARTDATE.", job, y, year)]]),
          make_date(new_data[[str_c("NEWEMP_STARTDATE.", job, y, year)]],
            new_data[[str_c("NEWEMP_STARTDATE.", job, m, year)]]),
          ifelse(
            !is.na(new_data[[str_c("NEWEMP_STARTDATE.", job, m, year)]]),
            make_date(0, new_data[[str_c("NEWEMP_STARTDATE.", job, m, year)]]),
            make_date(new_data[[str_c("NEWEMP_STARTDATE.", job, y, year)]])
          )
        )
    }
    
    # Ended job
    if (check(str_c(q_end_d, job, m, year), new_data)){
      new_data[[str_c("month_end_job_", job, "_", year)]] <- 
        ifelse(
          !is.na(new_data[[str_c(q_end_d, job, m, year)]]) &
            !is.na(new_data[[str_c(q_end_d, job, y, year)]]),
          make_date(new_data[[str_c(q_end_d, job, y, year)]],
            new_data[[str_c(q_end_d, job, m, year)]]),
          ifelse(
            !is.na(new_data[[str_c(q_end_d, job, m, year)]]),
            make_date(0, new_data[[str_c(q_end_d, job, m, year)]]),
            make_date(new_data[[str_c(q_end_d, job, y, year)]])
          )
        )
    }
    
    if (check(str_c(q_end_p, job, m, year), new_data)){
      new_data[[str_c("month_end_job_", job + 5, "_", year)]] <- 
        ifelse(
          !is.na(new_data[[str_c(q_end_p, job, m, year)]]) &
            !is.na(new_data[[str_c(q_end_p, job, y, year)]]),
          make_date(new_data[[str_c(q_end_p, job, y, year)]],
            new_data[[str_c(q_end_p, job, m, year)]]),
          ifelse(
            !is.na(new_data[[str_c(q_end_p, job, m, year)]]),
            make_date(0, new_data[[str_c(q_end_p, job, m, year)]]),
            make_date(new_data[[str_c(q_end_p, job, y, year)]])
          )
        )
    }
    
    if (check(str_c(q_end_n, job, m, year), new_data)){
      new_data[[str_c("month_end_job_", job + 10, "_", year)]] <- 
        ifelse(
          !is.na(new_data[[str_c(q_end_n, job, m, year)]]) &
            !is.na(new_data[[str_c(q_end_n, job, y, year)]]),
          make_date(new_data[[str_c(q_end_n, job, y, year)]],
            new_data[[str_c(q_end_n, job, m, year)]]),
          ifelse(
            !is.na(new_data[[str_c(q_end_n, job, m, year)]]),
            make_date(0, new_data[[str_c(q_end_n, job, m, year)]]),
            make_date(new_data[[str_c(q_end_n, job, y, year)]])
          )
        )
    }
    
    # Mark if currently working job
    if (check(str_c(q_cur_d, job, "_", year), new_data)){
      new_data[[str_c("current_job_", job, "_", year)]] <- 
        new_data[[str_c(q_cur_d, job, "_", year)]]
    }
    
    if (check(str_c(q_cur_p, job, "_", year), new_data)){
      new_data[[str_c("current_job_", job + 5, "_", year)]] <- 
        new_data[[str_c(q_cur_p, job, "_", year)]]
    }
    
    if (check(str_c(q_cur_n, job, "_", year), new_data)){
      new_data[[str_c("current_job_", job + 10, "_", year)]] <- 
        new_data[[str_c(q_cur_n, job, "_", year)]]
    }
    
    # 2016 n has different label
    if (check(str_c("NEWEMP_CURFLAG", job, "_", year), new_data)){
      new_data[[str_c("current_job_", job + 10, "_", year)]] <- 
        new_data[[str_c("NEWEMP_CURFLAG", job, "_", year)]]
    }
    
    # 2016 d has different label
    if (check(str_c("Q6-8", job, "_", year), new_data)){
      new_data[[str_c("current_job_", job, "_", year)]] <- 
        new_data[[str_c("Q6-8", job, "_", year)]]
    }
    
    # For each job, mark current job worked most. This is asked at the year
    # level, not job level
    if (check(str_c(q_most, year), new_data)){
      new_data[[str_c("most_job_", job, "_", year)]] <- 
        new_data[[str_c(q_most, year)]]
      
      new_data[[str_c("most_job_", job + 5, "_", year)]] <- 
        new_data[[str_c(q_most, year)]]
      
      new_data[[str_c("most_job_", job + 10, "_", year)]] <- 
        new_data[[str_c(q_most, year)]]
    }
    
    # Now we loop through the type of job, labeling began loop, traditional,
    # self-employed, independent contractor, temp worker, on-call worker,
    # then outsourced (contracted out) worker.
    # Some of these types have a .01 and .02 (ic and tw) and some
    # have A and B (o-c and o) if the person answered the question in a second loop.
    # If there is a second answer, I use it, otherwise I take the first answer.
    
    # Start with if loop began
    if (check(str_c(q_loop_d, job, ".01_", year), new_data)){
      new_data[[str_c("looped_", job, "_", year)]] <- 
        new_data[[str_c(q_loop_d, job, ".01_", year)]]
    }
    
    if (check(str_c(q_loop_p, job, ".01_", year), new_data)){
      new_data[[str_c("looped_", job + 5, "_", year)]] <- 
        new_data[[str_c(q_loop_p, job, ".01_", year)]]
    }
    
    if (check(str_c(q_loop_n, job, ".01_", year), new_data)){
      new_data[[str_c("looped_", job + 10, "_", year)]] <- 
        new_data[[str_c(q_loop_n, job, ".01_", year)]]
    }
    
    
    # See if preassigned traditional (Only keep if preassigned. Else
    # don"t know if trational or not, so set = NA)
    if (check(str_c(q_trad_d, job, ".01_", year), new_data)){
      new_data[[str_c("traditional_", job, "_", year)]] <- 
        ifelse(new_data[[str_c(q_trad_d, job, ".01_", year)]] == 1, 1, NA)
    }
    
    if (check(str_c(q_trad_p, job, ".01_", year), new_data)){
      new_data[[str_c("traditional_", job + 5, "_", year)]] <- 
        ifelse(new_data[[str_c(q_trad_p, job, ".01_", year)]] == 1, 1, NA)
    }
    
    # Self-employed
    if (check(str_c(q_type_d, "1.0",  job, ".01_", year), new_data)){
      new_data[[str_c("self_emp_", job, "_", year)]] <- 
          new_data[[str_c(q_type_d, "1.0",  job, ".01_", year)]]
    }
    
    if (check(str_c(q_type_p, "1.0",  job, ".01_", year), new_data)){
      new_data[[str_c("self_emp_", job + 5, "_", year)]] <- 
          new_data[[str_c(q_type_p, "1.0",  job, ".01_", year)]]
    }
    
    if (check(str_c(q_type_n, "1.0",  job, ".01_", year), new_data)){
      new_data[[str_c("self_emp_", job + 10, "_", year)]] <- 
          new_data[[str_c(q_type_n, "1.0",  job, ".01_", year)]]
    }
    
    # Independent contractors
    if (check(str_c(q_type_d, "2.0",  job, ".01_", year), new_data)){
      # If went through loop twice, make second answer the one reported
      if (check(str_c(q_type_d, "2.0",  job, ".02_", year), new_data)){
        new_data[[str_c(q_type_d, "2.0",  job, ".01_", year)]] <- 
          ifelse(
            !is.na(new_data[[str_c(q_type_d, "2.0",  job, ".02_", year)]]),
            new_data[[str_c(q_type_d, "2.0",  job, ".02_", year)]],
            new_data[[str_c(q_type_d, "2.0",  job, ".01_", year)]]
          )
      }
      new_data[[str_c("indep_con_", job, "_", year)]] <- 
          new_data[[str_c(q_type_d, "2.0",  job, ".01_", year)]]
    }
    
    if (check(str_c(q_type_p, "2.0",  job, ".01_", year), new_data)){
      # If went through loop twice, make second answer the one reported
      if (check(str_c(q_type_p, "2.0",  job, ".02_", year), new_data)){
        new_data[[str_c(q_type_p, "2.0",  job, ".01_", year)]] <- 
          ifelse(
            !is.na(new_data[[str_c(q_type_p, "2.0",  job, ".02_", year)]]),
            new_data[[str_c(q_type_p, "2.0",  job, ".02_", year)]],
            new_data[[str_c(q_type_p, "2.0",  job, ".01_", year)]]
          )
      }
      new_data[[str_c("indep_con_", job + 5, "_", year)]] <- 
          new_data[[str_c(q_type_p, "2.0",  job, ".01_", year)]]
    }
    
    if (check(str_c(q_type_n, "2.0",  job, ".01_", year), new_data)){
      # If went through loop twice, make second answer the one reported
      if (check(str_c(q_type_n, "2.0",  job, ".02_", year), new_data)){
        new_data[[str_c(q_type_n, "2.0",  job, ".01_", year)]] <- 
          ifelse(
            !is.na(new_data[[str_c(q_type_n, "2.0",  job, ".02_", year)]]),
            new_data[[str_c(q_type_n, "2.0",  job, ".02_", year)]],
            new_data[[str_c(q_type_n, "2.0",  job, ".01_", year)]]
          )
      }
      new_data[[str_c("indep_con_", job + 10, "_", year)]] <- 
          new_data[[str_c(q_type_n, "2.0",  job, ".01_", year)]]
    }
    
    # Temp worker
    if (check(str_c(q_type_d, "3.0",  job, ".01_", year), new_data)){
      # If went through loop twice, make second answer the one reported
      if (check(str_c(q_type_d, "3.0",  job, ".02_", year), new_data)){
        new_data[[str_c(q_type_d, "3.0",  job, ".01_", year)]] <- 
          ifelse(
            !is.na(new_data[[str_c(q_type_d, "3.0",  job, ".02_", year)]]),
            new_data[[str_c(q_type_d, "3.0",  job, ".02_", year)]],
            new_data[[str_c(q_type_d, "3.0",  job, ".01_", year)]]
          )
      }
      new_data[[str_c("temp_work_", job, "_", year)]] <- 
          new_data[[str_c(q_type_d, "3.0",  job, ".01_", year)]]
    }
    
    if (check(str_c(q_type_p, "3.0",  job, ".01_", year), new_data)){
      # If went through loop twice, make second answer the one reported
      if (check(str_c(q_type_p, "3.0",  job, ".02_", year), new_data)){
        new_data[[str_c(q_type_p, "3.0",  job, ".01_", year)]] <- 
          ifelse(
            !is.na(new_data[[str_c(q_type_p, "3.0",  job, ".02_", year)]]),
            new_data[[str_c(q_type_p, "3.0",  job, ".02_", year)]],
            new_data[[str_c(q_type_p, "3.0",  job, ".01_", year)]]
          )
      }
      new_data[[str_c("temp_work_", job + 5, "_", year)]] <- 
          new_data[[str_c(q_type_p, "3.0",  job, ".01_", year)]]
    }
    
    if (check(str_c(q_type_n, "3.0",  job, ".01_", year), new_data)){
      # If went through loop twice, make second answer the one reported
      if (check(str_c(q_type_n, "3.0",  job, ".02_", year), new_data)){
        new_data[[str_c(q_type_n, "3.0",  job, ".01_", year)]] <- 
          ifelse(
            !is.na(new_data[[str_c(q_type_n, "3.0",  job, ".02_", year)]]),
            new_data[[str_c(q_type_n, "3.0",  job, ".02_", year)]],
            new_data[[str_c(q_type_n, "3.0",  job, ".01_", year)]]
          )
      }
      new_data[[str_c("temp_work_", job + 10, "_", year)]] <- 
          new_data[[str_c(q_type_n, "3.0",  job, ".01_", year)]]
    }
    
    # On-call workers
    if (check(str_c(q_type_d, "4A.0",  job, ".01_", year), new_data)){
      # If went through loop twice, make second answer the one reported
      if (check(str_c(q_type_d, "4A.0",  job, ".02_", year), new_data)){
        new_data[[str_c(q_type_d, "4A.0",  job, ".01_", year)]] <- 
          ifelse(
            !is.na(new_data[[str_c(q_type_d, "4A.0",  job, ".02_", year)]]),
            new_data[[str_c(q_type_d, "4A.0",  job, ".02_", year)]],
            new_data[[str_c(q_type_d, "4A.0",  job, ".01_", year)]]
          )
      }
      new_data[[str_c("on_call_", job, "_", year)]] <- 
          new_data[[str_c(q_type_d, "4A.0",  job, ".01_", year)]]
    }
    
    if (check(str_c(q_type_p, "4A.0",  job, ".01_", year), new_data)){
      # If went through loop twice, make second answer the one reported
      if (check(str_c(q_type_p, "4A.0",  job, ".02_", year), new_data)){
        new_data[[str_c(q_type_p, "4A.0",  job, ".01_", year)]] <- 
          ifelse(
            !is.na(new_data[[str_c(q_type_p, "4A.0",  job, ".02_", year)]]),
            new_data[[str_c(q_type_p, "4A.0",  job, ".02_", year)]],
            new_data[[str_c(q_type_p, "4A.0",  job, ".01_", year)]]
          )
      }
      new_data[[str_c("on_call_", job + 5, "_", year)]] <- 
          new_data[[str_c(q_type_p, "4A.0",  job, ".01_", year)]]
    }
    
    if (check(str_c(q_type_n, "4A.0",  job, ".01_", year), new_data)){
      # If went through loop twice, make second answer the one reported
      if (check(str_c(q_type_n, "4A.0",  job, ".02_", year), new_data)){
        new_data[[str_c(q_type_n, "4A.0",  job, ".01_", year)]] <- 
          ifelse(
            !is.na(new_data[[str_c(q_type_n, "4A.0",  job, ".02_", year)]]),
            new_data[[str_c(q_type_n, "4A.0",  job, ".02_", year)]],
            new_data[[str_c(q_type_n, "4A.0",  job, ".01_", year)]]
          )
      }
      new_data[[str_c("on_call_", job + 10, "_", year)]] <- 
          new_data[[str_c(q_type_n, "4A.0",  job, ".01_", year)]]
    }
    
    # Outsourced Workers
    if (check(str_c(q_type_d, "5A.0",  job, ".01_", year), new_data)){
      # If went through loop twice, make second answer the one reported
      if (check(str_c(q_type_d, "5A.0",  job, ".02_", year), new_data)){
        new_data[[str_c(q_type_d, "5A.0",  job, ".01_", year)]] <- 
          ifelse(
            !is.na(new_data[[str_c(q_type_d, "5A.0",  job, ".02_", year)]]),
            new_data[[str_c(q_type_d, "5A.0",  job, ".02_", year)]],
            new_data[[str_c(q_type_d, "5A.0",  job, ".01_", year)]]
          )
      }
      new_data[[str_c("outsourced_", job, "_", year)]] <- 
          new_data[[str_c(q_type_d, "5A.0",  job, ".01_", year)]]
    }
    
    if (check(str_c(q_type_p, "5A.0",  job, ".01_", year), new_data)){
      # If went through loop twice, make second answer the one reported
      if (check(str_c(q_type_p, "5A.0",  job, ".02_", year), new_data)){
        new_data[[str_c(q_type_p, "5A.0",  job, ".01_", year)]] <- 
          ifelse(
            !is.na(new_data[[str_c(q_type_p, "5A.0",  job, ".02_", year)]]),
            new_data[[str_c(q_type_p, "5A.0",  job, ".02_", year)]],
            new_data[[str_c(q_type_p, "5A.0",  job, ".01_", year)]]
          )
      }
      new_data[[str_c("outsourced_", job + 5, "_", year)]] <- 
          new_data[[str_c(q_type_p, "5A.0",  job, ".01_", year)]]
    }
    
    if (check(str_c(q_type_n, "5A.0",  job, ".01_", year), new_data)){
      # If went through loop twice, make second answer the one reported
      if (check(str_c(q_type_n, "5A.0",  job, ".02_", year), new_data)){
        new_data[[str_c(q_type_n, "5A.0",  job, ".01_", year)]] <- 
          ifelse(
            !is.na(new_data[[str_c(q_type_n, "5A.0",  job, ".02_", year)]]),
            new_data[[str_c(q_type_n, "5A.0",  job, ".02_", year)]],
            new_data[[str_c(q_type_n, "5A.0",  job, ".01_", year)]]
          )
      }
      new_data[[str_c("outsourced_", job + 10, "_", year)]] <- 
          new_data[[str_c(q_type_n, "5A.0",  job, ".01_", year)]]
    }
  }
}

# Create list of variables to loop over jobs
vary <- c("month_start_job", "month_end_job", "current_job", "most_job", "looped", 
          "traditional", "self_emp", "indep_con", "temp_work", "on_call", "outsourced")

constant <- "case_id"

# Variables we keep
keep <- str_c("^(", str_c(vary, collapse = "|"), "|", constant, ")")


# Transform data from wide to long
long <- new_data %>% 
  dplyr::select(matches(keep)) %>% 
  gather(matches(str_c("^(", str_c(vary, collapse="|"), ")")), key=key, value=val) %>%
  extract(key, into=c("variable", "key"), regex="(\\D+)(_1?._20..)") %>%
  filter(!is.na(variable), !is.na(key)) %>%
  spread(key=variable, value=val) %>%
  mutate(key = substring(key, 2)) %>%
  separate(key, sep="_", into=c("job", "year"), convert = T) %>% 
  # Keep only observations with at least one job specific bit of data
  filter_at(vars(-case_id, -job, -year, -most_job), any_vars(!is.na(.))) %>% 
  # Fill in missing job type data
  # 1. When surveyed about job type (traditional, self-employed, etc), respondents
  # were not asked the remaining type questions if they answered yes. Replace these
  # NA"s with 0. (Exception is self-employed and independent contractor/ temp worker.)
  # (Also some jobs have NA"s before affirmitive answers. Set these as 0 too)
  # 2. Never formally asked if traditional. Call job traditional if looped
  # and no other job type (Missing looped variable for some jobs (ex job 5 2014)
  # If any job types reported, set looped = 1)
  mutate(
    self_emp = ifelse(
      !is.na(self_emp), self_emp, 
      ifelse(traditional == 1 %in% T, 0, self_emp)
      ),
    
    indep_con = ifelse(
      !is.na(indep_con), indep_con, 
      ifelse(pmax(traditional, self_emp, na.rm = T) == 1, 0, indep_con)
      ),
    
    temp_work = ifelse(
      !is.na(temp_work), temp_work, 
      ifelse(pmax(traditional, self_emp, indep_con, na.rm = T) == 1,
             0, temp_work)
    ),
    
    on_call = ifelse(
      !is.na(on_call), on_call, 
      ifelse(pmax(traditional, self_emp, indep_con, temp_work, na.rm = T) == 1,
             0, on_call)
    ),
    
    outsourced = ifelse(
      !is.na(outsourced), outsourced, 
      ifelse(pmax(traditional, self_emp, indep_con,
                  temp_work, on_call, na.rm = T) == 1,
             0, outsourced)
    ),
    
    # If any of job types are filled, set looped as 1
    looped = ifelse(
      !is.na(looped), looped,
      ifelse(is.na(self_emp) & is.na(indep_con) & is.na(temp_work) &
               is.na(on_call) & is.na(outsourced), looped, 1)
    ),
    
    traditional = ifelse(
      is.na(traditional) & looped == 1,  
      ifelse(pmax(self_emp, indep_con, temp_work, on_call, outsourced, 
                  na.rm = T) == 1,
             0, 1), traditional
    ),
    
    # If current_job = NA and looped = 1, set current_job = 1
    current_job = ifelse((is.na(current_job) & (looped == 1)) %in% T, 1, current_job),
    
    # Some people report being both self_emp and indep_con/temp_work. Call these people
    # just indep_con/temp_work
    self_emp = 
      ifelse(self_emp == 1 %in% T & !is.na(indep_con), self_emp - indep_con, self_emp),
    
    self_emp = 
      ifelse(self_emp == 1 %in% T & !is.na(temp_work), self_emp - temp_work, self_emp),
    
    # Create type_check to see if agent reported one type. (Some self_emp report 2)
    type_check = select(
      ., traditional, self_emp, indep_con, temp_work, on_call, outsourced) %>% 
      apply(1, sum, na.rm = T),
    
    # If person has a job type (type_check = 1), set NA job types = 0
    self_emp = ifelse(is.na(self_emp) & type_check == 1, 0, self_emp),
    indep_con = ifelse(is.na(indep_con) & type_check == 1, 0, indep_con),
    on_call = ifelse(is.na(on_call) & type_check == 1, 0, on_call),
    outsourced = ifelse(is.na(outsourced) & type_check == 1, 0, outsourced),
    
    # Finally, set start/end months as dates
    month_start_job = as_date(month_start_job),
    month_end_job = as_date(month_end_job)
  ) %>% 
  # Want to rank jobs by date last worked, with current jobs ranked first
  # To do this
  # 1. Group by case_id and year
  # (If !is.na(month_end_job) and is.na(current_job), set current_job == 0; 
  # Also add up all current jobs)
  # 2. Create rank_date = large number if current_job = 1, month_end_job else 
  # (including if current_job NA)
  # 3. Create rank using min_rank descending on rank_date
  # 4. Group jobs with same rank together. Generate row_number. 
  # For current jobs
  # a. If most_job = row number, this stays rank 1
  # b. If most_job > row_number, rank becomes 1 + rank_temp
  # c. If most_job < row_number, rank becomes rank_temp
  # Otherwise if row_number > 1, rank = rank + row_number - 1
  # If most_job > tot_current, then subtract 1 from rank of current 
  group_by(case_id, year) %>% 
  mutate(
    current_job = ifelse(is.na(current_job) & !is.na(month_end_job), 0, current_job),
    tot_current = sum(current_job, na.rm = T),
    rank_date = ifelse(
      is.na(current_job), month_end_job, ifelse(current_job == 1, 1e6, month_end_job)),
    rank_date = ifelse(is.na(rank_date), 0, rank_date),
    rank = min_rank(desc(rank_date))
    ) %>% 
  group_by(rank, add = T) %>% 
  mutate(row_num = row_number(rank)) %>% 
  ungroup() %>% 
  mutate(
    rank = ifelse(
      (current_job == 1 & !is.na(most_job)) %in% T,
      ifelse(most_job == row_num, 1,
             ifelse(most_job > row_num & most_job <= tot_current, row_num + 1, row_num)),
      ifelse(row_num > 1, rank + row_num - 1, rank)
      )
  ) %>% 
  # Create ever_out for those ever outsourced
  group_by(case_id) %>% 
  mutate(ever_out = ifelse(all(is.na(outsourced)), 0, max(outsourced, na.rm = T))) %>%
  ungroup() %>% 
  # Drop most_job, type_check, rank_date, row_num, tot_current, and job
  select(-most_job, -type_check, -rank_date, -row_num, -tot_current, -job)

# Save the data
write_csv(long, str_c(clean_folder, "on_jobs_clean.csv"))
