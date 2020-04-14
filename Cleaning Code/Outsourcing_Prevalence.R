# This file takes data from timeline and creates a measure
# of outsourcing in the economy and in each occupation.
# It then merges this data back into timeline as well
# as matched and matched_jobs

rm(list = ls())

library(openxlsx)
library(data.table)
library(BSDA)
library(srvyr)
library(DescTools)
library(lubridate)
library(tidyverse)

# Folders of interest
clean_folder <- "../Cleaned Data/"
table_folder <- "../Tables/"

# Dataset is large, need to read as data table
timeline <- fread(str_c(clean_folder, "timeline_clean.csv"))

timeline[, week := ymd(week)]

# Results noisy at end, so drop after max week, around end of 2016
week_max <- round_date(ymd("2016-09-11"), "week")

# How common is outsourcing? Take the average of all person-job-weeks
outsourcing_prevalence <- timeline %>% 
  filter(!is.na(outsourced), week <= week_max) %>% 
  as_survey_design(ids = case_id, weights=weight) %>%
  summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci"))

# How common is outsourcing in each occupation? Take the average of all 
# person-job-weeks. Also see if real weekly wage is above occupation
# average (by gender).
outsourcing_occ <- timeline %>% 
  filter(!is.na(outsourced), week <= week_max, !is.na(occ)) %>% 
  as_survey_design(ids = case_id, weights=weight) %>%
  group_by(female, occ) %>% 
  mutate(
    wage_above_occ = ifelse(outsourced == 1 %in% T,
      log_real_wkly_wage > mean(log_real_wkly_wage, na.rm = T), NA)
  ) %>% 
  group_by(occ) %>% 
  summarise(outsourced_per = survey_mean(outsourced * 100),
            outsourced_wage_above_per = survey_mean(wage_above_occ * 100, na.rm = T),
            week_obs = unweighted(n()),
            outsourced_week_obs = unweighted(sum(outsourced))) %>% 
  mutate(
    outsourcing_occ = outsourced_per >= 2 * outsourcing_prevalence$outsourced_per
  )

outsourcing_ind <- timeline %>% 
  filter(!is.na(outsourced), week <= week_max, !is.na(ind)) %>% 
  as_survey_design(ids = case_id, weights=weight) %>%
  group_by(ind) %>% 
  summarise(outsourced_per = survey_mean(outsourced * 100),
            week_obs = unweighted(n()),
            outsourced_week_obs = unweighted(sum(outsourced)))

# Create an excel file with all industries and occupations that are outsourced
l <- list("Ind" = outsourcing_ind %>%
            select(ind, week_obs, outsourced_week_obs, outsourced_per), 
          "Occ" = outsourcing_occ %>% 
            select(occ, week_obs, outsourced_week_obs, outsourced_per, 
                   outsourced_wage_above_per))

write.xlsx(l, file=str_c(table_folder, "Ind_Occ.xlsx"))

# Also make a table with relevant info (outsourced %, number of occupations, etc)
outsourcing_occ_ss <- outsourcing_occ %>% 
  summarise(
    occupations = n(),
    occupations_any = sum(outsourced_per > 0),
    occupations_out = sum(outsourcing_occ)
  )

table <- str_c(
  "\\documentclass[12pt]{article}
  \\usepackage[margin=.5in]{geometry}
  \\usepackage{booktabs}
  \\begin{document}
  \\begin{table}
  \\centering 
  \\begin{tabular}{lr}
  \\toprule
  Variable & Value \\\\ \\midrule
  Occupations & ", outsourcing_occ_ss$occupations, "\\\\\n",
  "Occupations with any Outsourcing & ", outsourcing_occ_ss$occupations_any, "\\\\\n",
  "Occupations with 2$\\times$ Average Outsourcing & ",
  outsourcing_occ_ss$occupations_out, "\\\\\n",
  "Average Outsourcing & ", round(outsourcing_prevalence$outsourced_per, 3), "\\\\\n",
  "\\bottomrule
  \\end{tabular}
  \\caption{Outsourcing prevalence among workers and occupations}
  \\label{outsourcing_occ}
  \\end{table}
  \\end{document}"
)

write.table(table,
            str_c(table_folder, "NLSY79 Occupation Info/Occupation Outsourcing.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Match occupation data with main datasets, matched, matched_jobs, 
# and timeline
matched <- fread(str_c(clean_folder, "matched_clean.csv"))
matched_jobs <- fread(str_c(clean_folder, "matched_jobs_clean.csv"))
outsourcing_occ <- outsourcing_occ %>% 
  select(occ, outsourced_per, outsourcing_occ, outsourced_wage_above_per) %>% 
  data.table()

setkey(matched, occ)
setkey(matched_jobs, occ)
setkey(timeline, occ)
setkey(outsourcing_occ, occ)

matched <- merge(matched, outsourcing_occ, all.x = T)

matched[order(case_id, year), ever_out_occ := any(outsourcing_occ %in% T),
  by = "case_id"] %>% 
  setcolorder(c("case_id", "year", "emp_id", "hours_week")) 

matched_jobs <- merge(matched_jobs, outsourcing_occ, all.x = T)

matched_jobs[order(case_id, emp_id), ever_out_occ := any(outsourcing_occ %in% T),
  by = "case_id"] %>% 
  setcolorder(c("case_id", "emp_id", "hours_week")) 

timeline <- merge(timeline, outsourcing_occ, all.x = T)

timeline[order(case_id, week), ever_out_occ := any(outsourcing_occ %in% T),
  by = "case_id"] %>% 
  setcolorder(c("case_id", "week", "unemployed", "unemployed_duration", "previous_job_2",
                "year", "hours_week")) 

# Save datasets with an _occ on the end
fwrite(matched, str_c(clean_folder, "matched_clean_occ.csv"), row.names = FALSE)
fwrite(matched_jobs, str_c(clean_folder, "matched_jobs_clean_occ.csv"), row.names = FALSE)
fwrite(timeline, str_c(clean_folder, "timeline_clean_occ.csv"), row.names = FALSE)