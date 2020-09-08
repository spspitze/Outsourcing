# This file contains IPUMS CPS data from cps_raw_codebook and cps_00005.dat.gz
# This is monthly data from 2001-2016 for all men. 
# Focus on occupation prevalance by outsourcing prevalance 
# (according to NLSY 79 data in occ_timeline).
# To compare cps occ2010 to nlsy (which uses 2000 census codes from 2001-2016),
# use the occ2010 sheet of integrated_ind_occ_crosswalks

rm(list = ls())

library(weights)
library(BSDA)
library(diagis)
library(estimatr)
library(readxl)
library(srvyr)
library(magrittr)
library(DescTools)
library(data.table)
library(lubridate)
library(ipumsr)
library(tidyverse)

raw_folder <- "../Raw Data/"
clean_folder <- "../Cleaned Data/"
table_folder <- "../Tables/"
figure_folder <- "../Figures/CPS/"
d_table_folder <- "../Drafts/Draft Tables/"
s_table_folder <- "../Slides/Slide Tables/"

# For saving graphs
aspect_ratio <- 1.62
height <- 7
width <- height * aspect_ratio

# Read the data using cps's custom formulas
ddi <- read_ipums_ddi(str_c(raw_folder, "cps_raw_codebook.xml"))
cps <- read_ipums_micro(ddi)

rm("ddi")

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
    p <- outcome$coefficients["p.value"]
  } else if (type == "prop") {
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

# Two different weights, so need to run weighted.mean separately
# Create function weighted_mean to run weighted.mean on
# variables as matricies (seems to be needed)
weighted_mean <- function(x, w, cond = T, na.rm = T) {
  weighted.mean(x = as.matrix(x[cond]), w = as.matrix(w[cond]), na.rm = na.rm)
}

# Do the same for weighted_se from diagis:: (na.rm not working for some reason)
weighted_sterr <- function(x, w, cond = T) {
  weighted_se(x = as.matrix(x[cond]), w = as.matrix(w[cond]))
}

# Define our default table top
table_top <- "\\documentclass[12pt]{article}
\\usepackage[margin=.5in]{geometry}
\\usepackage{siunitx}
\\usepackage{booktabs}
\\begin{document}
\\begin{table}
\\footnotesize
\\centering \n"

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

# Clean CPS data ----------------------------------------------------------

# Clean cps data, meging with cpi data and a crosswalk for 2000 occ codes
# Summarize each month by occupation

# Change Date to year, report cpi of each year 
# Get CPI from raw_folder
cpi <- read_csv(str_c(raw_folder, "CPIAUCSL.csv"),
                col_types = cols(
                  DATE = col_date(format = ""),
                  CPIAUCSL = col_double()
                ))

cpi %<>% 
  rename(year = DATE, cpi = CPIAUCSL) %>% 
  mutate(year = as.numeric(year(year)))

# Use cpi from year 2016 as base
base_cpi <- cpi$cpi[cpi$year == 2016]

# Start by marking clear NA's, and clarifying variable categories
names(cps) %<>% tolower
cps %<>%
  # Merge with cpi data for real wages 
  # (note, different attributes cause warning)
  left_join(cpi, by = "year") %>% 
  mutate(
    # Call black if black or mixed race
    black = 1 * (race %in% c(200, 801, 805, 806, 807, 810, 811, 814, 816)),
    # Call hispanic if mentioned being hispanic
    hispanic = 1 * (hispan > 0 & hispan < 900),
    # Define as married if marst is 1 or 2 and sinle if 6
    married = 1 * (marst %in% c(1, 2)),
    single = 1 * (marst == 6),
    # Define working if empstat is 1, 10, or 12 and unemployed if 21 or 22
    working = 1 * (empstat %in% c(1, 10, 12)),
    unemployed = 1 * (empstat %in% c(21, 22)),
    # Define part_time if wkstat is 12, 20-41
    part_time = 1 * (wkstat == 12 | (wkstat >= 20 & wkstat <= 22)),
    # For education groups
    less_hs = 1 * (educ <= 72),
    hs = 1 * (educ %in% c(73, 81)),
    aa = 1 * (educ %in% c(91, 92)),
    ba = 1 * (educ == 111),
    plus_ba = 1 * (educ > 111),
    # For union
    union = 1 * (union == 2),
    # For occ2010 and ind1990, replace various NA's
    occ_2010 = ifelse(occ2010 != 9920, occ2010, NA),
    ind_1990 = ifelse(ind1990 != 0, ind1990, NA),
    # Find log_real_wkly/hrly_wage, drop NA's and below 3 /hr or 60/wk
    log_real_hrly_wage = 
      ifelse(hourwage != 999.99 & hourwage >= 3, log(hourwage / cpi * base_cpi), NA),
    log_real_wkly_wage = 
      ifelse(earnweek != 9999.99 & earnweek >= 60, log(earnweek / cpi * base_cpi), NA),
    
    # Find birth_year if want to compare to NLSY cohort
    birth_year = year - age,
    nlsy79 = 1 * (birth_year >= 1957 & birth_year <= 1964)
  ) %>% 
  # Keep only variables needed
  select(year, month, wtfinl, cpsidp, age, earnwt,
         union, black:nlsy79)

# Read the crosswalk between occ_2010 and 2000 
# (multiply 2000 by 10 to match with NLSY)
# Keep only needed variables, convert to numeric, and drop NA
crosswalk <- read_excel(str_c(raw_folder, "integrated_ind_occ_crosswalks.xlsx"),
                        sheet = "occ2010", na = "#") %>% 
  select(occ_2010 = OCC2010, occ_2000 = `2000`) %>% 
  filter(!is.na(occ_2010), !is.na(occ_2000)) %>% 
  mutate(
    occ_2010 = as.numeric(occ_2010),
    occ_2000 = as.numeric(occ_2000) * 10
  ) %>% 
  # Some occ_2010's match to multiple occ_2000. Keep only the lowest 2000 match
  group_by(occ_2010) %>% 
  filter(occ_2000 == min(occ_2000)) %>% 
  ungroup()

# Merge with cps. Keep only occ_2000 as occ
cps %<>% 
  left_join(crosswalk, by = "occ_2010")  %>% 
  mutate(occ = occ_2000)
# %>% 
#   select(-occ_2010, -occ_2000)

# Get cleaned occ_timeline, which will be my measure of outsourcing over time
occ_timeline_m <- read_csv(str_c(clean_folder, "occ_timeline_m.csv"),
                         col_types = cols(
                           .default = col_double(),
                           month = col_date(format = "")
                         )) %>% 
  mutate(
    year = year(month),
    month = month(month)
  ) %>% 
  rename(black = black_per, hispanic = hispanic_per, union = union_per)

# Get year/month data from occ_timeline_m and keep needed variables
merger <- select(occ_timeline_m, year, month, occ, outsourced_per,
                 indep_con_per, self_emp_per, temp_work_per, 
                 on_call_per, ho_occ)

# Group cps by year, month, occ and summarise variables of interest.
# Look only at working. 
# Weights are either earnwt or wtfinal
occ_cps <- cps %>% 
  filter(working == 1, !is.na(occ)) %>% 
  group_by(year, month, occ) %>% 
  summarise(
    workers = sum(wtfinl),
    log_real_wkly_wage = weighted_mean(log_real_wkly_wage, earnwt),
    union = weighted_mean(union, earnwt),
    part_time = weighted_mean(part_time, wtfinl),
    black = weighted_mean(black, wtfinl),
    hispanic = weighted_mean(hispanic, wtfinl),
    age = weighted_mean(age, wtfinl),
  ) %>% 
  # What percent of workers are in this occupation?
  group_by(year, month) %>% 
  mutate(workers_per = workers / sum(workers) * 100) %>% 
  group_by(occ) %>% 
  mutate(average_size = mean(workers_per)) %>% 
  # inner join outsourcing info from merge
  inner_join(merger, by = c("year", "month", "occ")) 

# Do something similar for the NLSY79 cohort
occ_cps_nlsy <- cps %>% 
  filter(working == 1, !is.na(occ), nlsy79 == 1) %>% 
  group_by(year, month, occ) %>% 
  summarise(
    workers = sum(wtfinl),
    log_real_wkly_wage = weighted_mean(log_real_wkly_wage, earnwt),
    union = weighted_mean(union, earnwt),
    part_time = weighted_mean(part_time, wtfinl),
    black = weighted_mean(black, wtfinl),
    hispanic = weighted_mean(hispanic, wtfinl),
    age = weighted_mean(age, wtfinl),
  ) %>% 
  # What percent of workers are in this occupation?
  group_by(year, month) %>% 
  mutate(workers_per = workers / sum(workers) * 100) %>% 
  group_by(occ) %>% 
  mutate(average_size = mean(workers_per)) %>% 
  # inner join outsourcing info from merge
  inner_join(merger, by = c("year", "month", "occ")) 

# Analyze Correlations And Regression -------------------------------------

# What is the correlation between occuation outsourced_per and workers_per?
# Look at both occ_cps and occ_timeline. Compare results in tables,
# plot only occ_cps (plot occ_timeline in timeline_analysis)
data <- list(occ_cps, occ_timeline_m, occ_cps_nlsy)
titles <- c("CPS", "NLSY 79", "CPS (NLSY 79 Cohort)")

table <- str_c(
  table_top, siunitx, 
  "
  \\begin{tabular}{lSSSS}
  \\toprule
  Group & {Unweighted Correlation} & {t-stat} & {Weighted Correlation} & {t-stat}  \\\\
  \\midrule\n"
)

# Also run regressions to control for other factors and save output in table
# How do these correlations change with controls? Run regressions
controls <- c("outsourced_per", "self_emp_per", "indep_con_per",
              "temp_work_per", "on_call_per", "black", "hispanic",
              "union", "age")

fixed_effects <- c("I(year * month)", "occ")
fe <- create_formula("", fixed_effects)

eq <- create_formula("workers_per", controls)


top <- "\\begin{tabular}{lSSS}
\\toprule
Data Set & {Outsourced Percent} & {$R^2$} & {Observations}   \\\\  \\midrule
  "

center <- ""

for (i in 1:3){
  
  occ_corr <- data[[i]] %>% 
    group_by(occ) %>%
    summarise(
      corr_emp = cor(outsourced_per, workers_per, use = "na.or.complete"),
      average_size = mean(average_size),
      ho_occ = mean(ho_occ)
    ) %>% 
    filter(!is.na(corr_emp))
  
  # Look at correlation weighted by average occ size
  occ_corr_w <- occ_corr %>% 
    as_survey_design(ids = occ, weights = average_size) %>% 
    summarise(
      corr_emp = unweighted(mean(corr_emp)),
      corr_emp_se = unweighted(sd(corr_emp) / sqrt(n())),
      w_corr_emp = survey_mean(corr_emp, vartype = "se")
    )
  
  # Look at correlation weighted by average occ size for ho vs not occs
  occ_corr_w_g <- occ_corr %>% 
    as_survey_design(ids = occ, weights = average_size) %>% 
    group_by(ho_occ) %>% 
    summarise(
      corr_emp = unweighted(mean(corr_emp)),
      corr_emp_se = unweighted(sd(corr_emp) / sqrt(n())),
      w_corr_emp = survey_mean(corr_emp, vartype = "se")
    ) %>% 
    arrange(ho_occ)
  
  # Create graphs of distributions and create a table with each t.test
  
  corr_all <- occ_corr_w$corr_emp
  corr_ho <- occ_corr_w_g$corr_emp[2]
  corr_lo <- occ_corr_w_g$corr_emp[1]
  t_all <- occ_corr_w$corr_emp / occ_corr_w$corr_emp_se
  t_ho <- occ_corr_w_g$corr_emp[2] / occ_corr_w_g$corr_emp_se[2]
  t_lo <- occ_corr_w_g$corr_emp[1] / occ_corr_w_g$corr_emp_se[1]
  
  w_corr_all <- occ_corr_w$w_corr_emp
  w_corr_ho <- occ_corr_w_g$w_corr_emp[2]
  w_corr_lo <- occ_corr_w_g$w_corr_emp[1]
  w_t_all <- occ_corr_w$w_corr_emp / occ_corr_w$w_corr_emp_se
  w_t_ho <- occ_corr_w_g$w_corr_emp[2] / occ_corr_w_g$w_corr_emp_se[2]
  w_t_lo <- occ_corr_w_g$w_corr_emp[1] / occ_corr_w_g$w_corr_emp_se[1]
  
  if (i == 1) {
    # Plot histogram of correlations
    temp <- occ_corr %>% 
      ggplot() +
      geom_histogram(aes(corr_emp, fill = factor(ho_occ)),
                     alpha = 0.4, bins = 20) +
      geom_vline(aes(xintercept = corr_all), size=1) +
      geom_vline(aes(xintercept = corr_ho), color = "red", size=1) +
      geom_vline(aes(xintercept = corr_lo), color = "blue", size=1) +
      labs(x = "Correlation", y = "Count") +
      scale_fill_manual(name = "Occupation\nOutsourcing", breaks = c(0, 1),
                        values = c("blue", "red"),
                        labels = list(expression("Low (" < " 3.4%)"), 
                                      expression("High (" >= " 3.4%)"))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_light(base_size = 16) 
    
    ggsave(str_c(figure_folder, "Employment Correlation.pdf"),
           height = height, width = width)
    
    # What about correlation weighted by occupation size?
    temp <- occ_corr %>% 
      ggplot() +
      geom_point(aes(x = corr_emp, y = average_size,
                            color = factor(ho_occ)), alpha = 0.4) +
      geom_vline(aes(xintercept = w_corr_all), size=1) +
      geom_vline(aes(xintercept = w_corr_ho), color = "red", size=1) +
      geom_vline(aes(xintercept = w_corr_lo), color = "blue", size=1)  +
      labs(x = "Correlation", y = "Average Occupation Worker Share")+
      scale_color_manual(name = "Occupation\nOutsourcing", breaks = c(0, 1),
                         values = c("blue", "red"),
                         labels = list(expression("Low (" < " 3.4%)"), 
                                       expression("High (" >= " 3.4%)"))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_light(base_size = 16) 
    
    ggsave(str_c(figure_folder, "Employment Weighted Correlation.pdf"),
           height = height, width = width)
    }
  
  table %<>% str_c(
    titles[i], " & & & & \\\\ \n ",
    "All ",
    format_val(corr_all), format_val(t_all),
    format_val(w_corr_all), format_val(w_t_all), " \\\\ \n",
    "High Outsourcing ",
    format_val(corr_ho), format_val(t_ho), 
    format_val(w_corr_ho), format_val(w_t_ho), " \\\\ \n",
    "Low Outsourcing ",
    format_val(corr_lo), format_val(t_lo), 
    format_val(w_corr_lo), format_val(w_t_lo), " \\\\"
    )
  
  if (i != 3) {
    table %<>% str_c(" \\midrule \n")
  }
  
  
  temp <- lm_robust(eq, data = data[[i]],
                    fixed_effects = !!fe, clusters = occ, 
                    se_type = "stata", try_cholesky = T)
  
  center %<>% str_c(
    titles[i],
    format_val(temp$coefficients["outsourced_per"], r = 5, s = 5, 
               star = p_stars(temp$p.value["outsourced_per"])),
    format_val(temp$r.squared), format_n(lm_N(temp)), "\\\\ \n",
    format_se(temp$std.error["outsourced_per"], r = 5, s = 5), " & & \\\\ \n"
  )
}

table %<>% str_c(
  "
  \\bottomrule
  \\end{tabular}
  }
  \\caption{Mean correlation between an occupation's percent of all jobs and
    percent of that occupation that is outsourced in NLSY and CPS from 2001-2016.
    High outsourcing occupations are occupations that are outsourced at more 
    than twice the average rate, low outsourcing occupations are all others. 
    Unweighted correlations treat all occupations the same, weighted weight by 
    average weekly observations.}
    \\label{cps_occ_corr}
    \\end{table}
    \\end{document}
  "               
)

write.table(table,
            str_c(table_folder, "CPS/Occupation Correlation.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Create a table with regression results
bot <- "
\\bottomrule
\\end{tabular}
}
\\caption{Occupation level regressions of percent outsourced each month
(as measured in the NLSY) on percent of workers in an occupation. Data sets used are the
CPS, the NLSY, and the CPS with only workers born between 1957-1964 
(the same cohort as the NLSY). Each regression contains controls for percent 
in other alternative job types (ie. independent contractor, temp workers; also from NLSY),
percent Black, Hispanic, and union member, average age, and occupation and
month fixed effects. Data runs from January 2001 to October 2016.
Regressions use robust standard errors clustered at the occupation level. 
  Regressions use robust standard errors clustered at the occupation level. Stars represent
  significant difference from 0 at the .10 level *, .05 level **, and .01 level ***.}
\\label{occ_corr_reg_cps}
\\end{table}" 

write.table(str_c(table_top, siunitx, top, center, bot, "\n \\end{document}"),
            str_c(table_folder, "CPS/Occupation Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save in Drafts and Slides
write.table(str_c(d_table_top, top, center, bot),
            str_c(d_table_folder, "CPS Occupation Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

write.table(str_c(s_table_top, top, center, s_bot),
            str_c(s_table_folder, "CPS Occupation Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Robustness Check --------------------------------------------------------

# The occ_timeline_m above used timeline matched to matched_jobs
# This section matches instead to matched, so job
# characteristics can change each interview

# Use occ_timeline_robust_m instead
occ_timeline_r_m <- read_csv(str_c(clean_folder, "occ_timeline_robust_m.csv"),
                           col_types = cols(
                             .default = col_double(),
                             month = col_date(format = "")
                           )) %>% 
  mutate(
    year = year(month),
    month = month(month)
  ) %>% 
  rename(black = black_per, hispanic = hispanic_per, union = union_per)

# Get year/month data from occ_timeline_m and keep needed variables
merger_r <- select(occ_timeline_r_m, year, month, occ, outsourced_per,
                 indep_con_per, self_emp_per, temp_work_per, 
                 on_call_per)

# occ_cps and occ_cps_nlsy have already been created, just merge 
# in relevant variables
occ_cps_r <- inner_join(occ_cps, merger_r, by = c("year", "month", "occ"),
                      suffix = c("_orig", "")) 

occ_cps_nlsy_r <- inner_join(occ_cps_nlsy, merger_r, by = c("year", "month", "occ"),
                        suffix = c("_orig", "")) 

# Only rerun regressions
data_r <- list(occ_cps_r, occ_timeline_r_m, occ_cps_nlsy_r)
titles <- c("CPS", "NLSY 79", "CPS (NLSY 79 Cohort)")

controls <- c("outsourced_per", "self_emp_per", "indep_con_per",
              "temp_work_per", "on_call_per", "black", "hispanic",
              "union", "age")

fixed_effects <- c("I(year * month)", "occ")
fe <- create_formula("", fixed_effects)

eq <- create_formula("workers_per", controls)

center <- ""

for (i in 1:3){
  
  temp <- lm_robust(eq, data = data_r[[i]],
                    fixed_effects = !!fe, clusters = occ, 
                    se_type = "stata", try_cholesky = T)
  
  center %<>% str_c(
    titles[i],
    format_val(temp$coefficients["outsourced_per"], r = 5, s = 5, 
               star = p_stars(temp$p.value["outsourced_per"])),
    format_val(temp$r.squared), format_n(lm_N(temp)), "\\\\ \n",
    format_se(temp$std.error["outsourced_per"], r = 5, s = 5), " & & \\\\ \n"
  )
}


# Create a table with regression results
bot <- "
\\bottomrule
\\end{tabular}
}
\\caption{Occupation level regressions of percent outsourced each month
(as measured in the NLSY) on percent of workers in an occupation. Data sets used are the
CPS, the NLSY, and the CPS with only workers born between 1957-1964 
(the same cohort as the NLSY). Each regression contains controls for percent 
in other alternative job types (ie. independent contractor, temp workers; also from NLSY),
percent Black, Hispanic, and union member, average age, and occupation and
month fixed effects. Data runs from January 2001 to October 2016.
Regressions use robust standard errors clustered at the occupation level. 
  Regressions use robust standard errors clustered at the occupation level. Stars represent
  significant difference from 0 at the .10 level *, .05 level **, and .01 level ***.}
\\label{occ_corr_reg_robust_cps}
\\end{table}"

write.table(str_c(table_top, siunitx, top, center, bot, "\n \\end{document}"),
            str_c(table_folder, "CPS/Occupation Regressions Robust.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save to Drafts and Slides
write.table(str_c(d_table_top, top, center, bot),
            str_c(d_table_folder, "CPS Occupation Regressions Robust.tex"),
            quote=F, col.names=F, row.names=F, sep="")

write.table(str_c(s_table_top, top, center, s_bot),
            str_c(s_table_folder, "CPS Occupation Regressions Robust.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# HO Occupation Summary Statistics ----------------------------------------

# Look at summary statistics for HO Occupation Jobs/People vs not
# Merge ho_occ info into full cps data
cps %<>% data.table()

ho_occ <- occ_timeline_m %>% 
  filter(!is.na(ho_occ)) %>% 
  group_by(occ) %>% 
  summarise(ho_occ = mean(ho_occ)) %>% 
  data.table()

setkey(cps, occ)
setkey(ho_occ, occ)

cps <- merge(cps, ho_occ, all.x = T)

rm(ho_occ, crosswalk, occ_timeline_m, cpi)

# Create two datasets, one for all workers and one for NLSY cohort
ho_ss <- list(cps, filter(cps, nlsy79 == 1))

comp <- list(cps, filter(cps, nlsy79 == 1))

for (i in 1:2) {
  ho_ss[[i]] %<>% 
    filter(!is.na(ho_occ)) %>% 
    group_by(ho_occ) %>% 
    summarise(
      log_real_hrly_wage_se = weighted_sterr(log_real_hrly_wage, earnwt, 
                                             !is.na(log_real_hrly_wage)),
      log_real_hrly_wage = weighted_mean(log_real_hrly_wage, earnwt),
      log_real_wkly_wage_se = weighted_sterr(log_real_wkly_wage, earnwt, 
                                             !is.na(log_real_wkly_wage)),
      log_real_wkly_wage = weighted_mean(log_real_wkly_wage, earnwt),
      union_se = weighted_sterr(union, earnwt),
      union = weighted_mean(union, earnwt),
      part_time_se = weighted_sterr(part_time, wtfinl),
      part_time = weighted_mean(part_time, wtfinl),
      black_se = weighted_sterr(black, wtfinl),
      black = weighted_mean(black, wtfinl),
      hispanic_se = weighted_sterr(hispanic, wtfinl),
      hispanic = weighted_mean(hispanic, wtfinl),
      age_se = weighted_sterr(age, wtfinl),
      age = weighted_mean(age, wtfinl),
      less_hs_se = weighted_sterr(less_hs, wtfinl),
      less_hs = weighted_mean(less_hs, wtfinl),
      hs_se = weighted_sterr(hs, wtfinl),
      hs = weighted_mean(hs, wtfinl),
      aa_se = weighted_sterr(aa, wtfinl),
      aa = weighted_mean(aa, wtfinl),
      ba_se = weighted_sterr(ba, wtfinl),
      ba = weighted_mean(ba, wtfinl),
      plus_ba_se = weighted_sterr(plus_ba, wtfinl),
      plus_ba = weighted_mean(plus_ba, wtfinl),
      single_se = weighted_sterr(single, wtfinl),
      single = weighted_mean(single, wtfinl),
      married_se = weighted_sterr(married, wtfinl),
      married = weighted_mean(married, wtfinl),
      n = n()
    ) %>% 
    arrange(desc(ho_occ))
  
  comp[[i]] %<>%
    filter(!is.na(ho_occ))
}


vars_sum <- c(
  "log_real_hrly_wage", "log_real_wkly_wage",
  "part_time", "union", "age", "black", "hispanic",
  "less_hs", "hs", "aa", "ba", "plus_ba", "single", "married"
  )

# Create a Latex table
top <- "\\begin{tabular}{lSS}
\\toprule
& {High Outsourcing} & {Others} \\\\  \\midrule
"

# Proportion varables
vars_p <- c("part_time", "union", "black", "hispanic",
            "less_hs", "hs", "aa", "ba", "plus_ba", "single", "married")

# Variables using earnwt
vars_earnwt <- c("log_real_hrly_wage", "log_real_wkly_wage", "union")

saves <- c("", "NLSY Cohort ")
descriptions <- c("for all male", "for NLSY 79 cohort (aged 14-22 in 1979) male")
labels <- c("", "_nlsy")

for (j in 1:2) {
  
  save <- saves[j]
  desc <- descriptions[j]
  label <- labels[j]
  
  center <- rbind("Log Real", "Hourly Wage", "Log Real", 
                  "Weekly Wage", "Part-Time", "", "Union", "",
                  "Age", "", "Percent", "Black", "Percent", "Hispanic", 
                  "Less", "High School", "High School", "",
                  "Associates", "Degree", "Bachelor's", "Degree", "Plus", "Degree",
                  "Single", "", "Married", "", "Observations")
  
  cond <- comp[[j]]$ho_occ == 1
  
  for (i in 1:2) {
    
    c_i <- c()
    for (var in vars_sum) {
      
      se <- str_c(var, "_se")
      weights <- if (var %in% vars_earnwt) "earnwt" else "wtfinl"
      mode <- if (var %in% vars_p) "prop" else "mean"
      stars <- if (i == 2) test(comp[[j]], var, weights, mode, cond, !cond, "ho_occ") else ""
      
      c_i %<>% rbind(
        format_val(ho_ss[[j]][[var]][i], star = stars),
        format_se(ho_ss[[j]][[se]][i], r = 4, s = 4)
      )
    }
    
    c_i %<>% rbind(format_n(ho_ss[[j]]$n[i]))
    
    center %<>% cbind(c_i)
  }
  
  center %<>% cbind(
    rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\[2pt]"))
  
  # For Slides, keep only certain variables
  s_center <- center[c(11:23, 28), ]
  
  # Do weird stuff to create LaTeX output
  j_folder <- str_c(table_folder, "Junk/")
  file_1 <- str_c(j_folder, "center.txt")
  write.table(center, file_1, quote=T, col.names=F, row.names=F)
  center <- read.table(file_1, sep = "")
  write.table(center, file_1, quote=F, col.names=F, row.names=F, sep = "")
  center <- readChar(file_1, nchars = 1e6)
  
  bot <- str_c("
  \\bottomrule
  \\end{tabular}
  }
  \\caption{Summary statistics ", desc, " workers divided by high outsourcing
  (HO) occupations (all occupations with outsourcing more than 4.4\\% in the NLSY))
  vs not from 2001-2016. Statstics are weighted at the person level.
  Stars represent significant difference from HO occupations at the .10 level *, 
  .05 level **, and .01 level ***.}
  \\label{ho_occ_cps", label, "}
  \\end{table}"
  )
  
  write.table(str_c(table_top, siunitx, top, center, bot, "\n \\end{document}"),
              str_c(table_folder, "CPS/HO Occupation ",
                    save, "Summary Statistics.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save in Drafts 
  write.table(str_c(d_table_top, top, center, bot),
              str_c(d_table_folder, "CPS HO Occupation ",
                    save, "Summary Statistics.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save in Slides (no resize)
  # Do weird stuff to create LaTeX output
  j_folder <- str_c(table_folder, "Junk/")
  file_1 <- str_c(j_folder, "center.txt")
  write.table(s_center, file_1, quote=T, col.names=F, row.names=F)
  s_center <- read.table(file_1, sep = "")
  write.table(s_center, file_1, quote=F, col.names=F, row.names=F, sep = "")
  s_center <- readChar(file_1, nchars = 1e6)
  
  write.table(str_c(d_table_top, "\n \\footnotesize \n", top, s_center, s_bot),
              str_c(s_table_folder, "CPS HO Occupation ",
                    save, "Summary Statistics.tex"),
              quote=F, col.names=F, row.names=F, sep="")
}

# Wage Distribution ----------------------------------------------------

# Plot the wage distribution overall and for each year for ho_occs vs non ho_occs
# Also run a regression using a quartic in age, race, marital status,
# education, and year/month fes; weighted by earnwt
controls <- c("age", "I(age^2)", "I(age^3)", "I(age^4)", "black", "hispanic",
              "union", "married", "single", "hs", "aa", "ba", "plus_ba")
fe <- create_formula("", list("year", "month"))

vars <- c("log_real_hrly_wage", "log_real_wkly_wage")
var_names <- c("Log Real Hourly Wage", "Log Real Weekly Wage")
save_names <- c("LRH Wage", "LRW Wage")

for (i in seq_along(vars)) {
  
  var <- vars[i]
  var_name <- var_names[i]
  save_name <- save_names[i]
  
  temp_max <- max(cps[[var]], na.rm = T)
  
  temp <- cps %>% 
    filter(!is.na(ho_occ), !is.na(.[[var]])) %>% 
    ggplot(aes_string(var, fill = "factor(ho_occ)")) +
    geom_density(alpha = 0.2, bounds = c(0, temp_max)) +
    labs(x = var_name, y = "Density") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(name = "HO Occ", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = c("No", "Yes")) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, save_name, " Overall Distribution by HO Occ.pdf"),
         height = height, width = width)
  
  rm(temp)
  
  temp <- cps %>% 
    filter(!is.na(ho_occ), !is.na(.[[var]])) %>% 
    ggplot(aes_string(var, fill = "factor(ho_occ)")) +
    geom_density(alpha = 0.2, bounds = c(0, temp_max)) +
    labs(x = var_name, y = "Density") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(name = "HO Occ", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = c("No", "Yes")) +
    theme_light(base_size = 16) +
    facet_wrap(~ year)
  
  ggsave(str_c(figure_folder, save_name, " Yearly Distribution by HO Occ.pdf"),
         height = height, width = width)
  
  rm(temp)
  
  eq <- create_formula(var, controls)
  
  reg <- lm_robust(eq, data = cps, fixed_effects = !!fe,
                   weights = earnwt, se_type = "stata", try_cholesky = T)
  
  temp <- cps %>% 
    filter(!is.na(.[[var]]), !is.na(age), 
           !is.na(black), !is.na(hispanic),
           !is.na(union), !is.na(married), !is.na(hs)) %>% 
    mutate(residual = lm_residuals(reg)) %>% 
    filter(!is.na(ho_occ)) %>% 
    ggplot(aes(residual, fill = factor(ho_occ))) +
    geom_density(alpha = 0.2) +
    labs(x = str_c("Residual ", var_name), y = "Density") +
    scale_fill_manual(name = "HO Occ", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = c("No", "Yes")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, save_name, " Residuals by HO Occ.pdf"),
         height = height, width = width)
  
  rm(temp, reg)
}