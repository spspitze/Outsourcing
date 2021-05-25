# This file compares the matches from the NLSY (main specification)
# to those imputed by me (old specification, in folders labeled
# Old Version (Imputed Match)).

rm(list = ls())

library(srvyr)
library(data.table)
library(magrittr)
library(DescTools)
library(lubridate)
library(tidyverse)

# Folders of interest
raw_folder <- "../Raw Data/"
clean_folder <- "../Cleaned Data/"
clean_folder_imp <- "../Cleaned Data/Previous Version (Imputed Match)/"
table_folder <- "../Tables/NLSY 79 Version Comparison/"
figure_folder <- "../Figures/NLSY 79 Version Comparison/"
d_table_folder <- "../Drafts/Draft Tables/"
s_table_folder <- "../Slides/Slide Tables/"

# Create a default table_top
table_top <- "\\documentclass[12pt]{article}
\\usepackage[margin=.5in]{geometry}
\\usepackage{booktabs}
\\usepackage{graphicx}
\\begin{document}
\\begin{table}
\\centering
\\resizebox{\\textwidth}{!}{ \n"

# Create a default top for Draft tables (no resizebox, sometimes use it for Slides)
d_table_top <- "\\begin{table}[t!]
\\centering 
{ \n"

# Create a default top for Slide tables (but sometimes use it for Drafts)
s_table_top <- "\\begin{table}[h!]
\\centering 
\\resizebox{\\textwidth}{!}{ \n"

# Create a default bottom for Slide tables (no details)
s_bot <- "\\bottomrule
\\end{tabular}
}
\\end{table}"

# For saving graphs
# aspect_ratio <- 1.62
aspect_ratio <- 1.77
height <- 6
width <- height * aspect_ratio

# Compare matched ---------------------------------------------------------

# Compare the matched data sets (observations at person-job-year level)
# Before comparison, drop variables that include economy wide aggregates
# (like occupation outsourced percent)
# Current version
matched <- read_csv(str_c(clean_folder, "matched.csv"),
                    col_types = cols(
                      .default = col_double(),
                      week_start_job = col_date(format = ""),
                      week_end_job = col_date(format = "")
                    )) %>% 
  select(case_id:traditional)

# Previous version
matched_imp <- read_csv(str_c(clean_folder_imp, "matched.csv"),
                    col_types = cols(
                      .default = col_double(),
                      week_start_job = col_date(format = ""),
                      week_end_job = col_date(format = "")
                    ))%>% 
  select(case_id:traditional, match_quality)

# Which observations look different?
different <- anti_join(matched, matched_imp)
different_imp <- anti_join(matched_imp, matched)

# What percent of current matches were different or not in the imputation?
diff <- NROW(different) / NROW(matched)

# How does this breakdown change by year?
table(different$int_year)
table(different_imp$int_year)

# Observations by year
table(matched$int_year)
table(matched_imp$int_year)

# Compare matched_jobs ----------------------------------------------------

# Compared matched_jobs data sets (observations at person-job level)
matched_jobs <- read_csv(str_c(clean_folder, "matched_jobs.csv"),
                         col_types = cols(
                           .default = col_double(),
                           week_start_job = col_date(format = ""),
                           week_end_job = col_date(format = "")
                         )) %>% 
  select(case_id:week_end_job)

matched_jobs_imp <- read_csv(str_c(clean_folder_imp, "matched_jobs.csv"),
                         col_types = cols(
                           .default = col_double(),
                           week_start_job = col_date(format = ""),
                           week_end_job = col_date(format = "")
                         )) %>% 
  select(case_id:week_end_job)

# Which observations look different?
different_jobs <- anti_join(matched_jobs, matched_jobs_imp)
different_jobs_imp <- anti_join(matched_jobs_imp, matched_jobs)

# What percent of current matches were different or not in the imputation?
diff_jobs <- NROW(different_jobs) / NROW(matched_jobs)

# How does this breakdown change by year started/ended job
table(year(different_jobs$week_start_job))
table(year(different_jobs_imp$week_start_job))
table(year(different_jobs$week_end_job))
table(year(different_jobs_imp$week_end_job))

# Observations by year started/ended job
table(year(matched_jobs$week_start_job))
table(year(matched_jobs_imp$week_start_job))
table(year(matched_jobs$week_end_job))
table(year(matched_jobs_imp$week_end_job))
