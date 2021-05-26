<<<<<<< HEAD
# Run some timeline analysis (mainly graphs) including women

rm(list = ls())

# library(lpirfs)
library(weights)
library(magrittr)
library(data.table)
library(estimatr)
library(data.table)
library(openxlsx)
library(BSDA)
library(srvyr)
library(DescTools)
library(lubridate)
library(tidyverse)

# Folders of interest
clean_folder <- "../Cleaned Data/"
table_folder <- "../Tables/"
figure_folder <- "../Figures/NLSY 79 Timeline/"
d_table_folder <- "../Drafts/Draft Tables/"
s_table_folder <- "../Slides/Slide Tables/"

# For saving graphs
# aspect_ratio <- 1.62
aspect_ratio <- 1.77
height <- 6
width <- height * aspect_ratio

# Get men's timeline
timeline <- read_csv(str_c(clean_folder, "matched_timeline.csv"),
                     col_types = cols(
                       .default = col_double(),
                       week = col_date(format = ""),
                       week_start_job = col_date(format = ""),
                       week_end_job = col_date(format = "")
                     ))

# # Create month and year from week in timeline (drop first observation
# # that looks like it's in 2000)
# timeline %<>% 
#   filter(year(week) > 2000) %>% 
#   mutate(month = floor_date(week, unit = "month"),
#          year = floor_date(week, unit = "year"))

# Bring in women's timeline
timeline_w <- read_csv(str_c(clean_folder, "matched_timeline_women.csv"),
                     col_types = cols(
                       .default = col_double(),
                       week = col_date(format = ""),
                       week_start_job = col_date(format = ""),
                       week_end_job = col_date(format = "")
                     ))

# # Create month and year from week in timeline (drop first observation
# # that looks like it's in 2000)
# timeline_w %<>%
#   filter(year(week) > 2000) %>%
#   mutate(month = floor_date(week, unit = "month"),
#          year = floor_date(week, unit = "year"))

timeline <- bind_rows(timeline, timeline_w)

rm(timeline_w)

# # If I want a quick run through
# timeline <- timeline %>% filter(week < ymd("2003-09-11"))

# Create a function that takes regression p-values and reports
# * if 10%, ** if 5%, and *** if 1% significant
p_stars <- function(p){
  if (p < .01){
    stars <- "\\textsuperscript{***}"
  } else if (p < .05){
    stars <- "\\textsuperscript{**}"
  } else if (p < .1){
    stars <- "\\textsuperscript{*}"
  } else{
    stars <- ""
  }
  return(stars)
}

# Create a function that finds difference of means or proportions and reports
# * if 10%, ** if 5%, and *** if 1% different
# means uses cond, proportion uses var_2
test <- function(data, var, weight, type, cond = T, cond_y = T, divider = NULL) {
  if (type == "mean") {
    data_x <- data[cond,]
    data_y <- data[cond_y,]
    outcome <- wtd.t.test(x=data_x[[var]], y=data_y[[var]],
                          weight=data_x[[weight]],
                          weighty=data_y[[weight]],
                          samedata = FALSE)
    p <- as.numeric(outcome$coefficients["p.value"])
  } else if (type == "prop") {
    # (round outcome to make sure it's 0/1 (useful when we sometimes time averages))
    outcome <- wtd.chi.sq(round(data[[var]]), data[[divider]], weight = data[[weight]])
    p <- outcome[["p.value"]]
  } else {
    return(warning("Not a valid test"))
  }
  
  p_stars(p)
}

# Create a function that makes a formula given dependent variable 
# and list of independent variables
create_formula <- function(y, x_list) {
  vars <- x_list %>% 
    map(str_c, collapse = "+") %>% 
    str_c(collapse = "+")
  
  eq <- formula(str_c(y, vars, sep = "~"))
}

# Create a function to properly format inputs
format_it <- function(var, r = 2, s = 2) {
  format(round(var, r), nsmall = s, scientific = F)
}

# Create a function to put values in tables
format_val <- function(var, star = "", r = 2, s = 2) {
  str_c(" & ", format_it(var, r, s), star, " ")
}

# Create a function to put standard errors in tables
format_se <- function(var, r = 2, s = 2) {
  str_c(" & (", format_it(var, r, s), ") ")
}

# Create a function to put N obs in tables
format_n <- function(n) {
  str_c(" & {", format(n, big.mark = ",", trim = T), "} ")
}

# Create a function to format percents
format_per <- function(var, r = 2, s = 2) {
  str_c(" & ", format(round(var * 100, r), nsmall = s), " ")
}

# Create a function to sum rows of a variable, droppin NAs
r_sum <- function(...) {
  rowSums(cbind(...), na.rm = T)
}

# Create a function that takes an lm_robust model and returns residuals
# Drop residuals == 0 (these are ones with single-use fixed effects)
lm_residuals <- function(model) {
  resids <- model.frame(model)[[model$outcome]] - model$fitted.values
  return(resids[resids != 0])
}

# Create a function that takes an lm_robust model and calculates
# "effective N" or values actually used to estimate coefficients
# (drops single FE)
lm_N <- function(model) {
  length(lm_residuals(model))
}

# I often want to run code on whole sample and on ever_ho_occ. 
# Pair these dfs in a list
split_data <- function(df) {
  list(df, filter(df, ever_ho_occ == 1))
}

# Define our default table top
table_top <- "\\documentclass[12pt]{article}
\\usepackage[margin=.5in]{geometry}
\\usepackage{siunitx}
\\usepackage{booktabs}
\\usepackage{graphicx}
\\begin{document}
\\begin{table}
\\centering 
\\resizebox{\\textwidth}{!}{ \n"

# If using siunitx, include this too
siunitx <- "\\sisetup{
table-number-alignment = center,
table-figures-integer = 3,
table-figures-decimal = 3,
input-symbols=()-,
table-space-text-post = \\textsuperscript{***},
table-align-text-post = false,
group-digits          = false
}"

# Create a default top for Draft tables (no resizebox)
d_table_top <- "\\begin{table}[h!]
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

# Plot Timelines ----------------------------------------------------------

# Plot time series by Weeks and Age, and Weeks for an age range
# 1. Percent of workers outsourced 
# 2. Percent of workers in HO occupations 
# 3. Percent of workers in HO occupations  who are outsourced 
# 4. Number of workers in all job types
# 5. Number of workers in PBS industries
# 6. Percent of workers outsourced by education, < ba and >= ba

vars_time <- c("week", "age")
vars_save <- c("Week", "Age")
vars_label <- c("Year", "Age")

# Create a function scale which takes index i and returns an
# x_scale_date for week, month and year and an 
# x_scale_continuous for age
scale <- function(i) {
  if (i == 2){
    scale_x_continuous(breaks = seq(min(timeline$age, na.rm = T),
                                    max(timeline$age, na.rm = T), by = 2))
  } else {
    scale_x_date(date_breaks = "2 years", date_labels = "%Y")
  }
}

# For Outsourced by week, show CWS and Katz and Krueger (2019) outsourcing levels
# on time graphs
# Get data from https://www.rsfjournal.org/content/rsfjss/5/5/132.full.pdf
# Table 1
date_2001 <- round_date(ymd("2001-02-15"), "week")
date_2005 <- round_date(ymd("2005-02-15"), "week")
date_2015 <- round_date(ymd("2015-10-30"), "week")
date_2017 <- round_date(ymd("2017-05-15"), "week")

# Make confidence intervals a bit more transparent
alpha_ci <- 0.7

for (i in seq_along(vars_time)) {
# To only do age
# for (i in c(2)) {
  
  # 1. Percent of workers outsourced 
  # (do this separately for age, so we can add CWS/KK info)
  if (i == 2) {
    temp <- timeline %>% 
      filter(!is.na(outsourced), !is.na(.[[vars_time[i]]])) %>% 
      as_survey_design(ids = case_id, weights=weight) %>% 
      group_by_at(vars_time[i]) %>% 
      summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
      ggplot() +
      geom_line(aes_string(x = vars_time[i], y = "outsourced_per"), color = "red",
                n = 4) +
      geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp"),
                linetype="dashed", color = "red", n = 4, alpha = alpha_ci) +
      geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low"), 
                linetype="dashed", color = "red", n = 4, alpha = alpha_ci) +
      labs(x = vars_label[i], y = "Percent Outsourced") +
      scale(i) +
      theme_light(base_size = 16) 
    
    ggsave(str_c(figure_folder, "Outsourced ", vars_save[i], " Men + Women.pdf"),
           height = height, width = width)
  } else {
    temp <- timeline %>% 
      filter(!is.na(outsourced), !is.na(.[[vars_time[i]]])) %>% 
      as_survey_design(ids = case_id, weights=weight) %>% 
      group_by_at(vars_time[i]) %>% 
      summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
      ggplot() +
      geom_line(aes_string(x = vars_time[i], y = "outsourced_per"), color = "red",
                n = 4) +
      geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp"),
                linetype="dashed", color = "red", n = 4, alpha = alpha_ci) +
      geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low"), 
                linetype="dashed", color = "red", n = 4, alpha = alpha_ci) +
      geom_point(aes(x = date_2001, y = 1.1), size = 2, color = "black") +
      geom_text(aes(x = date_2001, y = 1.2, hjust = 0,
                    label = "CWS 2001"), size = 6) +
      geom_point(aes(x = date_2005, y = 1.4), size = 2, color = "black") +
      geom_text(aes(x = date_2005, y = 1.5, hjust = 1,
                    label = "CWS 2005"), size = 6) +
      geom_point(aes(x = date_2015, y = 2.5), size = 2, color = "black") +
      geom_text(aes(x = date_2015, y = 2.425, hjust = 0,
                    label = "KK (2019)"), size = 6) +
      geom_point(aes(x = date_2017, y = 1.4), size = 2, color = "black") +
      geom_text(aes(x = date_2017, y = 1.325, hjust = 1,
                    label = "CWS 2017"), size = 6) +
      labs(x = vars_label[i], y = "Percent Outsourced") +
      scale(i) +
      theme_light(base_size = 16) 
    
    ggsave(str_c(figure_folder, "Outsourced ", vars_save[i], " Men + Women.pdf"),
           height = height, width = width)
  }
  
  # 2. Percent of workers in HO occupations 
  temp <- timeline %>% 
    filter(!is.na(ho_occ), !is.na(.[[vars_time[i]]])) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(ever_ho_occ_per = survey_mean(ever_ho_occ * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "ever_ho_occ_per"), color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "ever_ho_occ_per_upp"),
              linetype="dashed", color = "red", alpha = alpha_ci) +
    geom_line(aes_string(x = vars_time[i], y = "ever_ho_occ_per_low"), 
              linetype="dashed", color = "red", alpha = alpha_ci) +
    labs(x = vars_label[i], y = "% in HO Occupations") +
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "HO Occupation ", vars_save[i], " Men + Women.pdf"),
         height = height, width = width)
  
  # 3. Percent of workers ever in HO occupations who are outsourced 
  temp <- timeline %>% 
    filter(ever_ho_occ == 1, !is.na(outsourced), !is.na(.[[vars_time[i]]])) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per"), color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp"),
              linetype="dashed", color = "red", alpha = alpha_ci) +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low"), 
              linetype="dashed", color = "red", alpha = alpha_ci) +
    labs(x = vars_label[i], y = "% Outsourced Ever in HO Occupation") +
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "Outsourced Ever HO Occupations ",
               vars_save[i], ".pdf"),
         height = height, width = width)
  
  ggsave(str_c(figure_folder, "Not Working ", vars_save[i], " Men + Women.pdf"),
         height = height, width = width)
  
  # Next one is memory intensive, remove temp
  rm(temp)
  
  # 4. Number of workers in all job types
  breaks <- c("outsourced_per", "indep_con_per", "temp_work_per", "on_call_per")
  labels <- c("Outsourced", "Independent Contractor", "Temp Worker", "On-Call Worker")
  colors <- c( "red", "dark green", "orange", "purple")
  
  temp <- timeline %>% 
    filter(!is.na(outsourced), !is.na(indep_con), 
           !is.na(temp_work), !is.na(on_call), 
           !is.na(.[[vars_time[i]]])) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(
      outsourced_per = survey_mean(outsourced * 100),
      indep_con_per = survey_mean(indep_con * 100),
      temp_work_per = survey_mean(temp_work * 100),
      on_call_per = survey_mean(on_call * 100)) %>%
    select(-ends_with("_se")) %>% 
    gather(key = "var", value = "value", 2:5) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "value", color = "var"))  +
    scale_color_manual(name = "Job Type", breaks = breaks, labels = labels,
                       values = colors) +
    labs(x = vars_label[i], y = "Percent Workers") +
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "All Types ", vars_save[i], " Men + Women.pdf"),
         height = height, width = width)
  
  # 5. What percent of workers are in PBS industries?
  temp <- timeline %>% 
    filter(!is.na(pbs), !is.na(.[[vars_time[i]]])) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(pbs_per = survey_mean(pbs * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "pbs_per"), color = "red",
              n = 4) +
    geom_line(aes_string(x = vars_time[i], y = "pbs_per_upp"),
              linetype="dashed", color = "red", n = 4, alpha = alpha_ci) +
    geom_line(aes_string(x = vars_time[i], y = "pbs_per_low"), 
              linetype="dashed", color = "red", n = 4, alpha = alpha_ci) +
    labs(x = vars_label[i], y = "Percent PBS") +
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "PBS ", vars_save[i], " Men + Women.pdf"),
         height = height, width = width)
  
  # Next one is memory intensive, remove temp
  rm(temp)
  
  # By education < ba and >= ba (discard other educ)
  labels <- c("Less Than BA", "BA or More")
  colors <- c( "red", "blue")
  
  temp <- timeline %>% 
    filter(!is.na(outsourced), !is.na(.[[vars_time[i]]])) %>% 
    mutate(less_ba = less_hs + hs + aa,
           ba_more = ba + plus_ba) %>%
    filter(((less_ba == 1) | (ba_more == 1))) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>%
    group_by(ba_more, add = TRUE) %>% 
    summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per", 
                         color = "factor(ba_more)"), n = 4) +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp",
                         color = "factor(ba_more)"),
              linetype="dashed", n = 4, alpha = alpha_ci) +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low",
                         color = "factor(ba_more)"),
              linetype="dashed", n = 4, alpha = alpha_ci) +
    scale_color_manual(name = "Education", breaks = c(0, 1), labels = labels,
                       values = colors) +
    labs(x = vars_label[i], y = "Percent Outsourced") +
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "Outsourced ", vars_save[i], " Education Men + Women.pdf"),
         height = height, width = width)
  
  rm(temp)
}

# For Certain Age Groups, plot percent outsourced and percent outsourced in
# HO Occupations
age_mins <- c(43, 49)
age_maxes <- c(47, 53)

for (i in 1:length(age_mins)) {
  
  age_min <- age_mins[i]
  age_max <- age_maxes[i]
  
  # 1. Percent of workers outsourced
  temp <- timeline %>% 
    filter(!is.na(outsourced), !is.na(week), 
           age >= age_min, age <= age_max) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by(week) %>% 
    summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes(x = week, y = outsourced_per), color = "red", n = 4) +
    geom_line(aes(x = week, y = outsourced_per_upp),
              linetype="dashed", color = "red", n = 4, alpha = alpha_ci) +
    geom_line(aes(x = week, y = outsourced_per_low), 
              linetype="dashed", color = "red", n = 4, alpha = alpha_ci) +
    labs(x = "Year", y = "Percent Outsourced") +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "Outsourced Ages ", age_min, "-", age_max,
               " Men + Women.pdf"),
         height = height, width = width)
  
  # 2. Percent of workers ever in HO occupations who are outsourced 
  temp <- timeline %>% 
    filter(ever_ho_occ == 1, !is.na(outsourced), !is.na(week), 
           age >= age_min, age <= age_max) %>%  
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by(week) %>% 
    summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes(x = week, y = outsourced_per), color = "red",
              n = 4) +
    geom_line(aes(x = week, y = outsourced_per_upp),
              linetype="dashed", color = "red", n = 4, alpha = alpha_ci) +
    geom_line(aes(x = week, y = outsourced_per_low), 
              linetype="dashed", color = "red", n = 4, alpha = alpha_ci) +
    labs(x = "Year", y = "% Outsourced Ever in HO Occupation") +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "Outsourced Ever HO Occupations Ages ",
               age_min, "-", age_max, " Men + Women.pdf"),
         height = height, width = width)
}

# Before the next section, will be helpful to free up space
rm("temp", "breaks", "labels", "colors", "scale")

temp <- timeline %>% 
  filter(!is.na(outsourced)) %>% 
  mutate(
    year = floor_date(week, unit = "year"),
    birth_year = year(year) - age) %>% 
  filter(!is.na(birth_year)) %>% 
  as_survey_design(ids = case_id, weights = weight) %>% 
  group_by(year, birth_year) %>% 
  summarise(outsourced_per = survey_mean(outsourced * 100)) %>% 
  ggplot(aes(x = year, y = outsourced_per, color = factor(birth_year))) +
  geom_line() +
  labs(x = "Year", y = "Percent Outsourced", color = "Year Born") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_light(base_size = 16) 

ggsave(str_c(figure_folder, "Outsourced by Year Born Men + Women.pdf"),
       height = height, width = width)

# Try to free some space 
rm("temp")

# Katz and Krueger --------------------------------------------------------

# From Katz and Krueger (2019), look at workers by job types (Table 2)

# How common is each job type? Take the average of all person-job-weeks
type_prevalence <- timeline %>% 
  filter(!is.na(outsourced)) %>% 
  as_survey_design(ids = case_id, weights=weight) %>%
  summarise(
    outsourced = survey_mean(outsourced * 100),
    self_emp = survey_mean(self_emp * 100),
    indep_con = survey_mean(indep_con * 100),
    temp_work = survey_mean(temp_work * 100),
    on_call = survey_mean(on_call * 100),
    traditional = survey_mean(traditional * 100)
  )

vars <- c("outsourced", "indep_con", "temp_work", "on_call", 
          "self_emp", "traditional")

names <- c("Contracted Out", "Independent Contractor", "Temp Worker",
           "On-Call Worker", "Self-Employed", "Traditional Employee" )

table <- "\\begin{tabular}{lSSS}
\\toprule
Job Type & {NLSY} & {CWS 2005} & {Katz and Krueger (2019)} \\\\ \\midrule
"

# Import values from CWS and KK 
# https://www.rsfjournal.org/content/rsfjss/5/5/132.full.pdf
# Table 1
cws <- c(format_it(1.4), format_it(7.0), format_it(0.9), format_it(1.7),
         format_it(10.8), "{--}")
kk <- c(format_it(2.5), format_it(7.2), format_it(1.7), format_it(2.4),
        format_it(9.2), "{--}")


for (i in seq_along(vars)) {
  
  se <- str_c(vars[i], "_se")
  
  table %<>% str_c(
    names[i], format_val(type_prevalence[[vars[i]]]),
    " & ", cws[i], " & ", kk[i], " \\\\ \n",
    format_se(type_prevalence[[se]]), " & & \\\\[2pt] \n"
  )
}

bot <- "\\bottomrule
\\end{tabular}
}
\\caption{Percent of weekly job-person observations in each job type for men and women
in the NLSY.
Observations weighted at the person level. Other values are from 
\\citet{katz2019b} Table 1 using data from the Contingent Worker Survey (CWS) 2005 and 
the RAND American Life Panel using alternative weight 2,
which reweights to match the CPS in self-employment 
and multiple job holders. Both of these samples include men and women age 18 and older.
The NLSY separates self-employment as its own job type while both CWS and KK do not,
so I could not determine how many workers are in traditional jobs for these sources.}
\\label{per_job_types}
\\end{table}"

write.table(str_c(table_top, siunitx, table, bot, "\n \\end{document}"),
            str_c(table_folder, "NLSY79 Job Types/Job Type Percentages Men + Women.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save to Drafts and Slides
write.table(str_c(d_table_top, table, bot),
            str_c(d_table_folder, "Job Type Percentages Men + Women.tex"),
            quote=F, col.names=F, row.names=F, sep="")

write.table(str_c(s_table_top, table, s_bot),
            str_c(s_table_folder, "Job Type Percentages Men + Women.tex"),
=======
# Run some timeline analysis (mainly graphs) including women

rm(list = ls())

# library(lpirfs)
library(weights)
library(magrittr)
library(data.table)
library(estimatr)
library(data.table)
library(openxlsx)
library(BSDA)
library(srvyr)
library(DescTools)
library(lubridate)
library(tidyverse)

# Folders of interest
clean_folder <- "../Cleaned Data/"
table_folder <- "../Tables/"
figure_folder <- "../Figures/NLSY 79 Timeline/"
d_table_folder <- "../Drafts/Draft Tables/"
s_table_folder <- "../Slides/Slide Tables/"

# For saving graphs
aspect_ratio <- 1.62
height <- 7
width <- height * aspect_ratio

# Get men's timeline
timeline <- read_csv(str_c(clean_folder, "matched_timeline.csv"),
                     col_types = cols(
                       .default = col_double(),
                       week = col_date(format = ""),
                       week_start_job = col_date(format = ""),
                       week_end_job = col_date(format = "")
                     ))

# # Create month and year from week in timeline (drop first observation
# # that looks like it's in 2000)
# timeline %<>% 
#   filter(year(week) > 2000) %>% 
#   mutate(month = floor_date(week, unit = "month"),
#          year = floor_date(week, unit = "year"))

# Bring in women's timeline
timeline_w <- read_csv(str_c(clean_folder, "matched_timeline_women.csv"),
                     col_types = cols(
                       .default = col_double(),
                       week = col_date(format = ""),
                       week_start_job = col_date(format = ""),
                       week_end_job = col_date(format = "")
                     ))

# # Create month and year from week in timeline (drop first observation
# # that looks like it's in 2000)
# timeline_w %<>%
#   filter(year(week) > 2000) %>%
#   mutate(month = floor_date(week, unit = "month"),
#          year = floor_date(week, unit = "year"))

timeline <- bind_rows(timeline, timeline_w)

rm(timeline_w)

# # If I want a quick run through
# timeline <- timeline %>% filter(week < ymd("2003-09-11"))

# Create a function that takes regression p-values and reports
# * if 10%, ** if 5%, and *** if 1% significant
p_stars <- function(p){
  if (p < .01){
    stars <- "\\textsuperscript{***}"
  } else if (p < .05){
    stars <- "\\textsuperscript{**}"
  } else if (p < .1){
    stars <- "\\textsuperscript{*}"
  } else{
    stars <- ""
  }
  return(stars)
}

# Create a function that finds difference of means or proportions and reports
# * if 10%, ** if 5%, and *** if 1% different
# means uses cond, proportion uses var_2
test <- function(data, var, weight, type, cond = T, cond_y = T, divider = NULL) {
  if (type == "mean") {
    data_x <- data[cond,]
    data_y <- data[cond_y,]
    outcome <- wtd.t.test(x=data_x[[var]], y=data_y[[var]],
                          weight=data_x[[weight]],
                          weighty=data_y[[weight]],
                          samedata = FALSE)
    p <- as.numeric(outcome$coefficients["p.value"])
  } else if (type == "prop") {
    # (round outcome to make sure it's 0/1 (useful when we sometimes time averages))
    outcome <- wtd.chi.sq(round(data[[var]]), data[[divider]], weight = data[[weight]])
    p <- outcome[["p.value"]]
  } else {
    return(warning("Not a valid test"))
  }
  
  p_stars(p)
}

# Create a function that makes a formula given dependent variable 
# and list of independent variables
create_formula <- function(y, x_list) {
  vars <- x_list %>% 
    map(str_c, collapse = "+") %>% 
    str_c(collapse = "+")
  
  eq <- formula(str_c(y, vars, sep = "~"))
}

# Create a function to properly format inputs
format_it <- function(var, r = 2, s = 2) {
  format(round(var, r), nsmall = s, scientific = F)
}

# Create a function to put values in tables
format_val <- function(var, star = "", r = 2, s = 2) {
  str_c(" & ", format_it(var, r, s), star, " ")
}

# Create a function to put standard errors in tables
format_se <- function(var, r = 2, s = 2) {
  str_c(" & (", format_it(var, r, s), ") ")
}

# Create a function to put N obs in tables
format_n <- function(n) {
  str_c(" & {", format(n, big.mark = ",", trim = T), "} ")
}

# Create a function to format percents
format_per <- function(var, r = 2, s = 2) {
  str_c(" & ", format(round(var * 100, r), nsmall = s), " ")
}

# Create a function to sum rows of a variable, droppin NAs
r_sum <- function(...) {
  rowSums(cbind(...), na.rm = T)
}

# Create a function that takes an lm_robust model and returns residuals
# Drop residuals == 0 (these are ones with single-use fixed effects)
lm_residuals <- function(model) {
  resids <- model.frame(model)[[model$outcome]] - model$fitted.values
  return(resids[resids != 0])
}

# Create a function that takes an lm_robust model and calculates
# "effective N" or values actually used to estimate coefficients
# (drops single FE)
lm_N <- function(model) {
  length(lm_residuals(model))
}

# I often want to run code on whole sample and on ever_ho_occ. 
# Pair these dfs in a list
split_data <- function(df) {
  list(df, filter(df, ever_ho_occ == 1))
}

# Define our default table top
table_top <- "\\documentclass[12pt]{article}
\\usepackage[margin=.5in]{geometry}
\\usepackage{siunitx}
\\usepackage{booktabs}
\\usepackage{graphicx}
\\begin{document}
\\begin{table}
\\centering 
\\resizebox{\\textwidth}{!}{ \n"

# If using siunitx, include this too
siunitx <- "\\sisetup{
table-number-alignment = center,
table-figures-integer = 3,
table-figures-decimal = 3,
input-symbols=()-,
table-space-text-post = \\textsuperscript{***},
table-align-text-post = false,
group-digits          = false
}"

# Create a default top for Draft tables (no resizebox)
d_table_top <- "\\begin{table}[h!]
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

# Plot Timelines ----------------------------------------------------------

# Plot time series by Weeks and Age, and Weeks for an age range
# 1. Percent of workers outsourced 
# 2. Percent of workers in HO occupations 
# 3. Percent of workers in HO occupations  who are outsourced 
# 4. Number of workers in all job types
# 5. Number of workers in PBS industries

vars_time <- c("week", "age")
vars_save <- c("Week", "Age")
vars_label <- c("Year", "Age")

# Create a function scale which takes index i and returns an
# x_scale_date for week, month and year and an 
# x_scale_continuous for age
scale <- function(i) {
  if (i == 2){
    scale_x_continuous(breaks = seq(min(timeline$age, na.rm = T),
                                    max(timeline$age, na.rm = T), by = 2))
  } else {
    scale_x_date(date_breaks = "2 years", date_labels = "%Y")
  }
}

# For Outsourced by week, show CWS and Katz and Krueger (2019) outsourcing levels
# on time graphs
# Get data from https://www.rsfjournal.org/content/rsfjss/5/5/132.full.pdf
# Table 1
date_2001 <- round_date(ymd("2001-02-15"), "week")
date_2005 <- round_date(ymd("2005-02-15"), "week")
date_2015 <- round_date(ymd("2015-10-30"), "week")
date_2017 <- round_date(ymd("2017-05-15"), "week")

for (i in seq_along(vars_time)) {
  
  # 1. Percent of workers outsourced 
  # (do this separately for age, so we can add CWS/KK info)
  if (i == 2) {
    temp <- timeline %>% 
      filter(!is.na(outsourced), !is.na(.[[vars_time[i]]])) %>% 
      as_survey_design(ids = case_id, weights=weight) %>% 
      group_by_at(vars_time[i]) %>% 
      summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
      ggplot() +
      geom_line(aes_string(x = vars_time[i], y = "outsourced_per"), color = "red",
                n = 4) +
      geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp"),
                linetype="dashed", color = "red", n = 4) +
      geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low"), 
                linetype="dashed", color = "red", n = 4) +
      labs(x = vars_label[i], y = "Percent Outsourced") +
      scale(i) +
      theme_light(base_size = 16) 
    
    ggsave(str_c(figure_folder, "Outsourced ", vars_save[i], " Men + Women.pdf"),
           height = height, width = width)
  } else {
    temp <- timeline %>% 
      filter(!is.na(outsourced), !is.na(.[[vars_time[i]]])) %>% 
      as_survey_design(ids = case_id, weights=weight) %>% 
      group_by_at(vars_time[i]) %>% 
      summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
      ggplot() +
      geom_line(aes_string(x = vars_time[i], y = "outsourced_per"), color = "red",
                n = 4) +
      geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp"),
                linetype="dashed", color = "red", n = 4) +
      geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low"), 
                linetype="dashed", color = "red", n = 4) +
      geom_point(aes(x = date_2001, y = 1.1), size = 2, color = "black") +
      geom_text(aes(x = date_2001, y = 1.2, hjust = 0,
                    label = "CWS 2001"), size = 6) +
      geom_point(aes(x = date_2005, y = 1.4), size = 2, color = "black") +
      geom_text(aes(x = date_2005, y = 1.5, hjust = 1,
                    label = "CWS 2005"), size = 6) +
      geom_point(aes(x = date_2015, y = 2.5), size = 2, color = "black") +
      geom_text(aes(x = date_2015, y = 2.6, hjust = 1,
                    label = "Katz and Krueger (2019)"), size = 6) +
      geom_point(aes(x = date_2017, y = 1.4), size = 2, color = "black") +
      geom_text(aes(x = date_2017, y = 1.325, hjust = 1,
                    label = "CWS 2017"), size = 6) +
      labs(x = vars_label[i], y = "Percent Outsourced") +
      scale(i) +
      theme_light(base_size = 16) 
    
    ggsave(str_c(figure_folder, "Outsourced ", vars_save[i], " Men + Women.pdf"),
           height = height, width = width)
  }
  
  # 2. Percent of workers in HO occupations 
  temp <- timeline %>% 
    filter(!is.na(ho_occ), !is.na(.[[vars_time[i]]])) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(ever_ho_occ_per = survey_mean(ever_ho_occ * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "ever_ho_occ_per"), color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "ever_ho_occ_per_upp"),
              linetype="dashed", color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "ever_ho_occ_per_low"), 
              linetype="dashed", color = "red") +
    labs(x = vars_label[i], y = "% in HO Occupations") +
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "HO Occupation ", vars_save[i], " Men + Women.pdf"),
         height = height, width = width)
  
  # 3. Percent of workers ever in HO occupations who are outsourced 
  temp <- timeline %>% 
    filter(ever_ho_occ == 1, !is.na(outsourced), !is.na(.[[vars_time[i]]])) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per"), color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp"),
              linetype="dashed", color = "red") +
    geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low"), 
              linetype="dashed", color = "red") +
    labs(x = vars_label[i], y = "% Outsourced Ever in HO Occupation") +
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "Outsourced Ever HO Occupations ",
               vars_save[i], ".pdf"),
         height = height, width = width)
  
  ggsave(str_c(figure_folder, "Not Working ", vars_save[i], ".pdf"),
         height = height, width = width)
  
  # 4. Number of workers in all job types
  breaks <- c("outsourced_per", "indep_con_per", "temp_work_per", "on_call_per")
  labels <- c("Outsourced", "Independent Contractor", "Temp Worker", "On-Call Worker")
  colors <- c( "red", "dark green", "orange", "purple")
  
  temp <- timeline %>% 
    filter(!is.na(outsourced), !is.na(indep_con), 
           !is.na(temp_work), !is.na(on_call), 
           !is.na(.[[vars_time[i]]])) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(
      outsourced_per = survey_mean(outsourced * 100),
      indep_con_per = survey_mean(indep_con * 100),
      temp_work_per = survey_mean(temp_work * 100),
      on_call_per = survey_mean(on_call * 100)) %>%
    select(-ends_with("_se")) %>% 
    gather(key = "var", value = "value", 2:5) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "value", color = "var"))  +
    scale_color_manual(name = "Job Type", breaks = breaks, labels = labels,
                       values = colors) +
    labs(x = vars_label[i], y = "Percent Workers") +
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "All Types ", vars_save[i], " Men + Women.pdf"),
         height = height, width = width)
  
  # 5. What percent of workers are in PBS industries?
  temp <- timeline %>% 
    filter(!is.na(pbs), !is.na(.[[vars_time[i]]])) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by_at(vars_time[i]) %>% 
    summarise(pbs_per = survey_mean(pbs * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes_string(x = vars_time[i], y = "pbs_per"), color = "red",
              n = 4) +
    geom_line(aes_string(x = vars_time[i], y = "pbs_per_upp"),
              linetype="dashed", color = "red", n = 4) +
    geom_line(aes_string(x = vars_time[i], y = "pbs_per_low"), 
              linetype="dashed", color = "red", n = 4) +
    labs(x = vars_label[i], y = "Percent PBS") +
    scale(i) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "PBS ", vars_save[i], " Men + Women.pdf"),
         height = height, width = width)
  
  rm(temp)
}

# For Certain Age Groups, plot percent outsourced and percent outsourced in
# HO Occupations
age_mins <- c(43, 49)
age_maxes <- c(47, 53)

for (i in 1:length(age_mins)) {
  
  age_min <- age_mins[i]
  age_max <- age_maxes[i]
  
  # 1. Percent of workers outsourced
  temp <- timeline %>% 
    filter(!is.na(outsourced), !is.na(week), 
           age >= age_min, age <= age_max) %>% 
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by(week) %>% 
    summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes(x = week, y = outsourced_per), color = "red", n = 4) +
    geom_line(aes(x = week, y = outsourced_per_upp),
              linetype="dashed", color = "red", n = 4) +
    geom_line(aes(x = week, y = outsourced_per_low), 
              linetype="dashed", color = "red", n = 4) +
    labs(x = "Year", y = "Percent Outsourced") +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "Outsourced Ages ", age_min, "-", age_max,
               " Men + Women.pdf"),
         height = height, width = width)
  
  # 2. Percent of workers ever in HO occupations who are outsourced 
  temp <- timeline %>% 
    filter(ever_ho_occ == 1, !is.na(outsourced), !is.na(week), 
           age >= age_min, age <= age_max) %>%  
    as_survey_design(ids = case_id, weights=weight) %>% 
    group_by(week) %>% 
    summarise(outsourced_per = survey_mean(outsourced * 100, vartype = "ci")) %>% 
    ggplot() +
    geom_line(aes(x = week, y = outsourced_per), color = "red",
              n = 4) +
    geom_line(aes(x = week, y = outsourced_per_upp),
              linetype="dashed", color = "red", n = 4) +
    geom_line(aes(x = week, y = outsourced_per_low), 
              linetype="dashed", color = "red", n = 4) +
    labs(x = "Year", y = "% Outsourced Ever in HO Occupation") +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "Outsourced Ever HO Occupations Ages ",
               age_min, "-", age_max, " Men + Women.pdf"),
         height = height, width = width)
}

# Before the next section, will be helpful to free up space
rm("temp", "breaks", "labels", "colors", "scale")

temp <- timeline %>% 
  filter(!is.na(outsourced)) %>% 
  mutate(
    year = floor_date(week, unit = "year"),
    birth_year = year(year) - age) %>% 
  filter(!is.na(birth_year)) %>% 
  as_survey_design(ids = case_id, weights = weight) %>% 
  group_by(year, birth_year) %>% 
  summarise(outsourced_per = survey_mean(outsourced * 100)) %>% 
  ggplot(aes(x = year, y = outsourced_per, color = factor(birth_year))) +
  geom_line() +
  labs(x = "Year", y = "Percent Outsourced", color = "Year Born") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_light(base_size = 16) 

ggsave(str_c(figure_folder, "Outsourced by Year Born Men + Women.pdf"),
       height = height, width = width)

# Try to free some space 
rm("temp")

# Katz and Krueger --------------------------------------------------------

# From Katz and Krueger (2019), look at workers by job types (Table 2)

# How common is each job type? Take the average of all person-job-weeks
type_prevalence <- timeline %>% 
  filter(!is.na(outsourced)) %>% 
  as_survey_design(ids = case_id, weights=weight) %>%
  summarise(
    outsourced = survey_mean(outsourced * 100),
    self_emp = survey_mean(self_emp * 100),
    indep_con = survey_mean(indep_con * 100),
    temp_work = survey_mean(temp_work * 100),
    on_call = survey_mean(on_call * 100),
    traditional = survey_mean(traditional * 100)
  )

vars <- c("outsourced", "indep_con", "temp_work", "on_call", 
          "self_emp", "traditional")

names <- c("Contracted Out", "Independent Contractor", "Temp Worker",
           "On-Call Worker", "Self-Employed", "Traditional Employee" )

table <- "\\begin{tabular}{lSSS}
\\toprule
Job Type & {NLSY} & {CWS 2005} & {Katz and Krueger (2019)} \\\\ \\midrule
"

# Import values from CWS and KK 
# https://www.rsfjournal.org/content/rsfjss/5/5/132.full.pdf
# Table 1
cws <- c(format_it(1.4), format_it(7.0), format_it(0.9), format_it(1.7),
         format_it(10.8), "{--}")
kk <- c(format_it(2.5), format_it(7.2), format_it(1.7), format_it(2.4),
        format_it(9.2), "{--}")


for (i in seq_along(vars)) {
  
  se <- str_c(vars[i], "_se")
  
  table %<>% str_c(
    names[i], format_val(type_prevalence[[vars[i]]]),
    " & ", cws[i], " & ", kk[i], " \\\\ \n",
    format_se(type_prevalence[[se]]), " & & \\\\[2pt] \n"
  )
}

bot <- "\\bottomrule
\\end{tabular}
}
\\caption{Percent of weekly job-person observations in each job type for men and women
in the NLSY.
Observations weighted at the person level. Other values are from 
\\citet{katz2019b} Table 1 using data from the Contingent Worker Survey (CWS) 2005 and 
the RAND American Life Panel using alternative weight 2,
which reweights to match the CPS in self-employment 
and multiple job holders. Both of these samples include men and women age 18 and older.
The NLSY separates self-employment as its own job type while both CWS and KK do not,
so I could not determine how many workers are in traditional jobs for these sources.}
\\label{per_job_types}
\\end{table}"

write.table(str_c(table_top, siunitx, table, bot, "\n \\end{document}"),
            str_c(table_folder, "NLSY79 Job Types/Job Type Percentages Men + Women.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save to Drafts and Slides
write.table(str_c(d_table_top, table, bot),
            str_c(d_table_folder, "Job Type Percentages Men + Women.tex"),
            quote=F, col.names=F, row.names=F, sep="")

write.table(str_c(s_table_top, table, s_bot),
            str_c(s_table_folder, "Job Type Percentages Men + Women.tex"),
>>>>>>> 20b1e8c9af1a8f4e257353001e60ec8e1da197c2
            quote=F, col.names=F, row.names=F, sep="")