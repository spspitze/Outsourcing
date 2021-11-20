# This file contains IPUMS CPS data from cps_raw_codebook 
# and cps_00011.dat.gz
# This is monthly data from Jan 2001- Oct 2016 for all workers. 
# Focus on occupation prevalence by outsourcing prevalence 
# (according to NLSY 79 data in occ_timeline).
# To compare cps occ2010 to nlsy (which uses 2000 census codes 
# from 2001-2016),
# use the occ2010 sheet of integrated_ind_occ_crosswalks
# Cross walk comes from https://usa.ipums.org/usa/volii/occ_ind.shtml
# Is the link "Crosswalks" in bullet reading
# Crosswalks for OCC1950, OCC1990 or OCC2010 to the contemporary 
# OCC codes and for IND1950 or IND1990 to the contemporary IND codes.

rm(list = ls())

library(outsourcing)
library(zeallot)
library(dineq)
library(readxl)
library(estimatr)
library(srvyr)
library(data.table)
library(lubridate)
library(ipumsr)
library(tidyverse)

# Folders of interest
folders <- name_folders("CPS")
c(raw_folder, clean_folder, table_folder, figure_folder,
  d_table_folder, s_table_folder) %<-% folders 

# For saving graphs
c(height, width) %<-% fig_size()

# Bottom of slide tables never changes, so make it once
s_bot <- make_bot(slide = TRUE)

# Read the data using cps's custom formulas
ddi <- read_ipums_ddi(str_c(raw_folder, "cps_raw_codebook.xml"))
cps <- read_ipums_micro(ddi)

rm("ddi")

# Clean CPS data ----------------------------------------------------------

# Clean cps data, meging with cpi data and a crosswalk for 
# 2000 occ codes. Summarize each month by occupation

# Change Date to year, report cpi of each year 
# Get CPI from raw_folder
cpi <- read_csv(str_c(raw_folder, "CPIAUCSL.csv"),
                col_types = cols(
                  DATE = col_date(format = ""),
                  CPIAUCSL = col_double()
                ))

cpi <- cpi |>
  rename(year = DATE, cpi = CPIAUCSL) |> 
  mutate(year = as.numeric(year(year)))

# Use cpi from year 2016 as base
base_cpi <- cpi$cpi[cpi$year == 2016]

# Start by marking clear NA's, and clarifying variable categories
names(cps) <- tolower(names(cps))
cps <- cps |>
  # Merge with cpi data for real wages 
  # (note, different attributes cause warning)
  left_join(cpi, by = "year") |> 
  mutate(
    # Call female if sex == 2
    female = 1 * (sex == 2),
    # Call black if black or mixed race
    black = 1 * (race %in% c(200, 801, 805, 806, 807, 
                             810, 811, 814, 816)),
    # Call hispanic if mentioned being hispanic
    hispanic = 1 * (hispan > 0 & hispan < 900),
    # Define as married if marst is 1 or 2 and sinle if 6
    married = 1 * (marst %in% c(1, 2)),
    single = 1 * (marst == 6),
    # Define working if empstat is 1, 10, or 12 and unemployed 
    # if 21 or 22
    working = 1 * (empstat %in% c(1, 10, 12)),
    unemployed = 1 * (empstat %in% c(21, 22)),
    # Define part_time if wkstat is 12, 20-42
    part_time = 1 * (wkstat == 12 | (wkstat >= 20 & wkstat <= 42)),
    # For education groups
    less_hs = 1 * (educ <= 72),
    hs = 1 * (educ %in% c(73, 81)),
    aa = 1 * (educ %in% c(91, 92)),
    ba = 1 * (educ == 111),
    plus_ba = 1 * (educ > 111),
    high_ed = ba + plus_ba,
    # For union
    union = 1 * (union == 2),
    # For occ2010 and ind1990, replace various NA's
    occ_2010 = ifelse(occ2010 != 9920, occ2010, NA),
    ind_1990 = ifelse(ind1990 != 0, ind1990, NA),
    # Mark Industries in professional business services (in 1990)
    pbs_1990 = 1 * (ind_1990 >= 721 & ind_1990 <= 760),
    # Flag imputed wages. earnings, or hours.
    # Impute wages when earnings and hours exist
    hourwage = ifelse(qhourwag == 0, hourwage, NA),
    earnweek = ifelse(qearnwee == 0, earnweek, NA),
    uhrswork1 = ifelse(quhrswork1 == 0, uhrswork1, NA),
    # In theory, could impute hrly wages with weekly earnings
    # divided by usual hours. In practice, this gives too high
    # wages, so won't.
    # hourwage = ifelse(is.na(hourwage) & !is.na(earnweek) & 
    # !is.na(uhrswork1),
    #                   earnweek / uhrswork1, hourwage),
    # Keep values where real hourwage exists and 3 <= x <= 500, 
    # earnweek exists and real wage is greater than 50 and
    # and uhrswork1 exists and 0 < x <= 80
    wage_keep = 
      (!is.na(hourwage) &
         hourwage / cpi * base_cpi >= 3 & 
         hourwage / cpi * base_cpi <= 500),
    week_keep = (earnweek != 9999.99 & !is.na(earnweek) 
                 & earnweek / cpi * base_cpi >= 50),
    hour_keep = (uhrswork1 > 0 & uhrswork1 <= 80 & !is.na(uhrswork1)),
    # Make wages real and take logs
    log_real_hrly_wage = 
      ifelse(wage_keep, log(hourwage / cpi * base_cpi), NA),
    log_real_wkly_wage = 
      ifelse(week_keep, log(earnweek / cpi * base_cpi), NA),
    hours_week = ifelse(hour_keep, uhrswork1, NA),
    # Find birth_year if want to compare to NLSY cohort
    birth_year = year - age,
    nlsy79 = 1 * (birth_year >= 1957 & birth_year <= 1964),
    # Create unique year*month measure
    year_month = 12 * (year - 2001) + month
  ) |> 
  # Keep only variables needed
  select(year, month, year_month, wtfinl, cpsidp, age, earnwt,
         union, female:pbs_1990, log_real_hrly_wage:nlsy79)

# Cross walk comes from https://usa.ipums.org/usa/volii/occ_ind.shtml
# Is the link "Crosswalks" in bullet reading
# Crosswalks for OCC1950, OCC1990 or OCC2010 to the contemporary 
# OCC codes and for IND1950 or IND1990 to the contemporary IND codes.

# Read the crosswalk between occ_2010 and 2000 
# (multiply 2000 by 10 to match with NLSY)
# Keep only needed variables, convert to numeric, and drop NA
crosswalk <- read_excel(str_c(raw_folder, 
                              "integrated_ind_occ_crosswalks.xlsx"),
                        sheet = "occ2010", na = "#") |> 
  select(occ_2010 = OCC2010, occ_2000 = `2000`) |> 
  filter(!is.na(occ_2010), !is.na(occ_2000)) |> 
  mutate(
    occ_2010 = as.numeric(occ_2010),
    occ_2000 = as.numeric(occ_2000) * 10
  ) |> 
  # Some occ_2010's match to multiple occ_2000. 
  # Keep only the lowest 2000 match
  group_by(occ_2010) |> 
  filter(occ_2000 == min(occ_2000)) |> 
  ungroup()

# Merge with cps. Keep only occ_2000 as occ
cps <- cps |>
  left_join(crosswalk, by = "occ_2010")  |> 
  mutate(occ = occ_2000) |>
  select(-occ_2010, -occ_2000)

# Do the same for ind_1990 and ind_2000
crosswalk <- 
  read_excel(str_c(raw_folder, "integrated_ind_occ_crosswalks.xlsx"),
             sheet = "ind1990", na = "#") |> 
  select(ind_1990 = IND1990, ind_2000 = `2000`) |> 
  filter(!is.na(ind_1990), !is.na(ind_2000)) |> 
  mutate(
    ind_1990 = as.numeric(ind_1990),
    ind_2000 = as.numeric(ind_2000) * 10
  ) |> 
  # Some ind_1990's match to multiple ind_2000.
  # Keep only the lowest 2000 match
  group_by(ind_1990) |> 
  filter(ind_2000 == min(ind_2000)) |> 
  ungroup()

# Merge with cps. Keep only ind_2000 as ind. 
# Create pbs for pbs industries according to 2000 measure
cps <- cps |>
  left_join(crosswalk, by = "ind_1990")  |> 
  mutate(
    ind = ind_2000,
    pbs = 1 * (ind >= 7270 & ind <= 7790)) |>
  select(-ind_1990, -ind_2000)

# Get cleaned occ_timeline, which will be my measure of
# outsourcing over time
occ_timeline_m <- read_csv(str_c(clean_folder, "occ_timeline_m.csv"),
                         col_types = cols(
                           .default = col_double(),
                           month = col_date(format = "")
                         )) |> 
  mutate(
    year = year(month),
    month = month(month),
    year_month = 12 * (year - 2001) + month
  ) |> 
  rename(female = female_per, black = black_per, 
         hispanic = hispanic_per, union = union_per)

# Get year/month data from occ_timeline_m and keep needed variables
merger <- occ_timeline_m |>
  select(year, month, year_month, occ, outsourced_per, 
         indep_con_per, self_emp_per, temp_work_per, 
         on_call_per, ho_occ)

# Group cps by year, month, occ and summarise variables of interest.
# Look only at working.
# Weights are either earnwt or wtfinal
occ_cps <- cps |>
  filter(working == 1, !is.na(occ)) |>
  group_by(year_month, occ) |>
  summarise(
    workers = sum(wtfinl),
    # log_real_wkly_wage = weighted_mean(log_real_wkly_wage, earnwt),
    union = weighted_mean(union, earnwt),
    # part_time = weighted_mean(part_time, wtfinl),
    female = weighted_mean(female, wtfinl),
    black = weighted_mean(black, wtfinl),
    hispanic = weighted_mean(hispanic, wtfinl),
    age = weighted_mean(age, wtfinl)
  ) |>
  # What percent of workers are in this occupation?
  group_by(year_month) |>
  mutate(workers_per = workers / sum(workers) * 100) |>
  group_by(occ) |>
  mutate(average_size = mean(workers_per)) |>
  # inner join outsourcing info from merge
  inner_join(merger, by = c("year_month", "occ"))

# Do something similar for the NLSY79 cohort
occ_cps_nlsy <- cps |>
  filter(working == 1, !is.na(occ), nlsy79 == 1) |>
  group_by(year_month, occ) |>
  summarise(
    workers = sum(wtfinl),
    # log_real_wkly_wage = weighted_mean(log_real_wkly_wage, earnwt),
    union = weighted_mean(union, earnwt),
    # part_time = weighted_mean(part_time, wtfinl),
    female = weighted_mean(female, wtfinl),
    black = weighted_mean(black, wtfinl),
    hispanic = weighted_mean(hispanic, wtfinl),
    age = weighted_mean(age, wtfinl)
  ) |>
  # What percent of workers are in this occupation?
  group_by(year_month) |>
  mutate(workers_per = workers / sum(workers) * 100) |>
  group_by(occ) |>
  mutate(average_size = mean(workers_per)) |>
  # inner join outsourcing info from merge
  inner_join(merger, by = c("year_month", "occ"))

# Analyze Correlations And Regression -------------------------------------

# What is the correlation between occuation outsourced_per 
# and workers_per?
# Look at both occ_cps and occ_timeline. Compare results in tables,
# plot only occ_cps (plot occ_timeline in timeline_analysis)
data <- list(occ_timeline_m, occ_cps, occ_cps_nlsy)
titles <- c("NLSY 79", "CPS", "CPS (NLSY 79 Cohort)")

table <- "
  \\begin{tabular}{lSSSS}
  \\toprule
  Group & {Unweighted Correlation} & {t-stat} & {Weighted Correlation} & {t-stat}  \\\\
  \\midrule\n"

# Also run regressions to control for other factors and save 
# output in table.
# How do these correlations change with controls? Run regressions
controls <- c("outsourced_per", "self_emp_per", "indep_con_per",
              "temp_work_per", "on_call_per", "female", "black", "hispanic",
              "union", "age")

fixed_effects <- c("year_month", "occ")
fe <- create_formula("", fixed_effects)

eq <- create_formula("workers_per", controls)

top <- "\\begin{tabular}{lSSS}
\\toprule
 & {NLSY 79} & {CPS} & {CPS (NLSY 79 Cohort)}   \\\\  \\midrule
  "

center <- c("Outsourced Percent", "", "$R^2$", "Observations")

for (i in 1:3){

  occ_corr <- data[[i]] |>
    group_by(occ) |>
    summarise(
      corr_emp = cor(outsourced_per, workers_per, 
                     use = "na.or.complete"),
      average_size = mean(average_size),
      ho_occ = mean(ho_occ)
    ) |>
    filter(!is.na(corr_emp))

  # Look at correlation weighted by average occ size
  occ_corr_w <- occ_corr |>
    as_survey_design(ids = occ, weights = average_size) |>
    summarise(
      corr_emp = unweighted(mean(corr_emp)),
      corr_emp_se = unweighted(sd(corr_emp) / sqrt(n())),
      w_corr_emp = survey_mean(unlist(corr_emp), vartype = "se")
    )

  # Look at correlation weighted by average occ size for ho vs not occs
  occ_corr_w_g <- occ_corr |>
    as_survey_design(ids = occ, weights = average_size) |>
    group_by(ho_occ) |>
    summarise(
      corr_emp = unweighted(mean(corr_emp)),
      corr_emp_se = unweighted(sd(corr_emp) / sqrt(n())),
      w_corr_emp = survey_mean(unlist(corr_emp), vartype = "se")
    ) |>
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
    temp <- occ_corr |>
      ggplot() +
      geom_histogram(aes(corr_emp, fill = factor(ho_occ)),
                     alpha = 0.4, bins = 20) +
      geom_vline(aes(xintercept = corr_all), size=1) +
      geom_vline(aes(xintercept = corr_ho), color = "red", size=1) +
      geom_vline(aes(xintercept = corr_lo), color = "blue", size=1) +
      labs(x = "Correlation", y = "Count") +
      scale_fill_manual(name = "Occupation\nOutsourcing",
                        breaks = c(0, 1), values = c("blue", "red"),
                        labels = 
                          list(expression("Low (" < " 3.4%)"),
                               expression("High (" >= " 3.4%)"))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_light(base_size = 16)

    ggsave(str_c(figure_folder, "Employment Correlation.pdf"),
           height = height, width = width)

    # What about correlation weighted by occupation size?
    temp <- occ_corr |>
      ggplot() +
      geom_point(aes(x = corr_emp, y = average_size,
                            color = factor(ho_occ)), alpha = 0.4) +
      geom_vline(aes(xintercept = w_corr_all), size=1) +
      geom_vline(aes(xintercept = w_corr_ho), color = "red", size=1) +
      geom_vline(aes(xintercept = w_corr_lo), color = "blue", size=1)  +
      labs(x = "Correlation", y = "Average Occupation Worker Share") +
      scale_color_manual(name = "Occupation\nOutsourcing",
                         breaks = c(0, 1), values = c("blue", "red"),
                         labels = 
                           list(expression("Low (" < " 3.4%)"),
                                expression("High (" >= " 3.4%)"))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_light(base_size = 16)

    ggsave(str_c(figure_folder, "Employment Weighted Correlation.pdf"),
           height = height, width = width)
    }

  table <- str_c(table, 
    titles[i], " & & & & \\\\ \n ",
    "All ",
    format_val(corr_all), format_val(t_all),
    format_val(w_corr_all), format_val(w_t_all), " \\\\ \n",
    "High-Outsourcing ",
    format_val(corr_ho), format_val(t_ho),
    format_val(w_corr_ho), format_val(w_t_ho), " \\\\ \n",
    "Low-Outsourcing ",
    format_val(corr_lo), format_val(t_lo),
    format_val(w_corr_lo), format_val(w_t_lo), " \\\\"
    )

  if (i != 3) {
    table <- str_c(table, " \\midrule \n")
  }

  temp <- lm_robust(eq, data = data[[i]],
                    fixed_effects = !!fe, clusters = occ,
                    se_type = "stata", try_cholesky = T)

  center <- cbind(
    center, 
    rbind(format_val(temp$coefficients["outsourced_per"], r = 5, s = 5,
                     star = p_stars(temp$p.value["outsourced_per"])),
          format_se(temp$std.error["outsourced_per"], r = 5, s = 5),
          format_val(temp$r.squared), format_n(lm_N(temp))
          )
  )
}

center <- center |>
  add_endline() |>
  center_to_latex()

name <- "Occupation Level Correlations With Outsourcing CPS"
label <- "cps_occ_corr"
note <- "
Mean correlation between an occupation's percent of all jobs and
percent of that occupation that is outsourced in NLSY and CPS 
from 2001 to 2016. High-outsourcing occupations are occupations 
that are outsourced at more than twice the average rate, 
low outsourcing occupations are all others. Unweighted correlations
treat all occupations the same, weighted weight by
average weekly observations."

header <- make_header("", name, label)
bot <- make_bot(note)

write.table(str_c(header, table, bot),
            str_c(table_folder, "CPS/Occupation Correlation.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Create a table with regression results

name <- "Effects of Outsourcing Level within Occupation on Worker Share"
label <- "occ_reg_cps"
note <- "
Occupation level regressions of percent outsourced each month
(as measured in the NLSY) on percent of workers in an occupation.
Data sets used are the CPS, the NLSY, and the CPS with only workers
born between 1957-1964 (the same cohort as the NLSY). 
Each regression contains controls for percent in other alternative
job types (ie. independent contractor, temp workers; also from NLSY),
percent female, Black, Hispanic, and union member, average age, and 
occupation and month fixed effects. Data runs from January 2001
to October 2016. Regressions use robust standard errors clustered 
at the occupation level. Stars represent significant difference 
from 0 at the .10 level (*), .05 level (**), and .01 level (***)."

header <- make_header("", name, label)
d_header <- make_header("d", name, label)
s_header <- make_header("s")

bot <- make_bot(note)

write.table(str_c(header, top, center, bot, "\n \\end{document}"),
            str_c(table_folder, "CPS/Occupation Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save in Drafts and Slides
write.table(str_c(d_header, top, center, bot),
            str_c(d_table_folder, "CPS Occupation Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

write.table(str_c(s_header, top, center, s_bot),
            str_c(s_table_folder, "CPS Occupation Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Robustness Check --------------------------------------------------------

# The occ_timeline_m above used timeline matched to matched_jobs
# This section matches instead to matched, so job
# characteristics can change each interview

# Use occ_timeline_robust_m instead
occ_timeline_r_m <- read_csv(str_c(clean_folder, 
                                   "occ_timeline_robust_m.csv"),
                           col_types = cols(
                             .default = col_double(),
                             month = col_date(format = "")
                           )) |>
  mutate(
    year = year(month),
    month = month(month),
    year_month = 12 * (year - 2001) + month
  ) |>
  rename(female = female_per, black = black_per,
         hispanic = hispanic_per, union = union_per)

# Get year/month data from occ_timeline_m and keep needed variables
merger_r <- occ_timeline_r_m |>
  select(year, month, year_month, occ, outsourced_per,
         indep_con_per, self_emp_per, temp_work_per, on_call_per)

# occ_cps and occ_cps_nlsy have already been created, just merge
# in relevant variables
occ_cps_r <- inner_join(occ_cps, merger_r, by = c("year_month", "occ"),
                        suffix = c("_orig", ""))

occ_cps_nlsy_r <- inner_join(occ_cps_nlsy, merger_r, 
                             by = c("year_month", "occ"),
                             suffix = c("_orig", ""))

# Only rerun regressions
data_r <- list(occ_timeline_r_m, occ_cps_r, occ_cps_nlsy_r)
titles <- c("NLSY 79", "CPS", "CPS (NLSY 79 Cohort)")

controls <- c("outsourced_per", "self_emp_per", "indep_con_per",
              "temp_work_per", "on_call_per", "female", "black", "hispanic",
              "union", "age")

fixed_effects <- c("year_month", "occ")
fe <- create_formula("", fixed_effects)

eq <- create_formula("workers_per", controls)

center <- c("Outsourced Percent", "", "$R^2$", "Observations")

for (i in 1:3){

  temp <- lm_robust(eq, data = data_r[[i]],
                    fixed_effects = !!fe, clusters = occ,
                    se_type = "stata", try_cholesky = T)
  
  center <- cbind(
    center, 
    rbind(format_val(temp$coefficients["outsourced_per"], r = 5, s = 5,
                     star = p_stars(temp$p.value["outsourced_per"])),
          format_se(temp$std.error["outsourced_per"], r = 5, s = 5),
          format_val(temp$r.squared), format_n(lm_N(temp))
    )
  )
}

center <- center |>
  add_endline() |>
  center_to_latex()

# Create a table with regression results
name <- "Effects of Outsourcing Level within Occupation on Worker Share"
label <- "occ_reg_cps_robust"
note <- "
Occupation level regressions of percent outsourced each month
(as measured in the NLSY) on percent of workers in an occupation.
Data sets used are the CPS, the NLSY, and the CPS with only workers
born between 1957-1964 (the same cohort as the NLSY). 
Each regression contains controls for percent in other alternative
job types (ie. independent contractor, temp workers; also from NLSY),
percent female, Black, Hispanic, and union member, average age, and 
occupation and month fixed effects. Data runs from January 2001
to October 2016. Regressions use robust standard errors clustered 
at the occupation level. Stars represent significant difference 
from 0 at the .10 level (*), .05 level (**), and .01 level (***)."

header <- make_header("", name, label)
d_header <- make_header("d", name, label)
s_header <- make_header("s")

bot <- make_bot(note)

write.table(str_c(header, top, center, bot, "\n \\end{document}"),
            str_c(table_folder, 
                  "CPS/Occupation Regressions Robust.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save to Drafts and Slides
write.table(str_c(d_header, top, center, bot),
            str_c(d_table_folder, 
                  "CPS Occupation Regressions Robust.tex"),
            quote=F, col.names=F, row.names=F, sep="")

write.table(str_c(s_header, top, center, s_bot),
            str_c(s_table_folder, 
                  "CPS Occupation Regressions Robust.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# PBS Regressions ---------------------------------------------------------

# Run similar regressions to above us PBS instead of outsourced

# Get NLSY data from pbs_timeline_m
pbs_timeline_m <- read_csv(str_c(clean_folder, "pbs_timeline_m.csv"),
                           col_types = cols(
                             .default = col_double(),
                             month = col_date(format = "")
                           )) |>
  mutate(
    year = year(month),
    month = month(month),
    year_month = 12 * (year - 2001) + month
  )

occ_pbs <- cps |>
  mutate(pbs = pbs_1990) |>
  filter(working == 1, !is.na(occ), !is.na(pbs)) |>
  group_by(year_month, occ, pbs) |>
  summarise(
    n = sum(wtfinl),
    n_female = sum(female * wtfinl),
    n_black = sum(black * wtfinl),
    n_hispanic = sum(hispanic * wtfinl),
    n_union = sum(union * wtfinl, na.rm = T),
    n_union_defined = sum((!is.na(union) %in% T) * wtfinl),
    tot_age = sum(age * wtfinl),
    tot_age_2 = sum(age^2 * wtfinl)
  ) |>
  pivot_wider(names_from = c(pbs),
              values_from = n:tot_age_2) |>
  ungroup() |>
  mutate(
    workers = r_sum(n_0, n_1),
    pbs_per = ifelse(!is.na(n_1), n_1 / workers * 100, 0),
    female_per = (
      r_sum(n_female_0, n_female_1) / workers * 100),
    black_per = (
      r_sum(n_black_0, n_black_1) / workers * 100),
    hispanic_per = (
      r_sum(n_hispanic_0, n_hispanic_1) / workers * 100),
    union_per = (r_sum(n_union_0, n_union_1) /
                   r_sum(n_union_defined_0, n_union_defined_1)
                 * 100),
    age = (r_sum(tot_age_0, tot_age_1) / workers),
    age_2 = (r_sum(tot_age_2_0, tot_age_2_1) / workers)
  ) |>
  group_by(year_month) |>
  mutate(workers_per = workers / sum(workers) * 100)

data_pbs <- list(pbs_timeline_m, occ_pbs)

titles <- c("NLSY 79" , "CPS")

controls <- c("pbs_per", "female_per", "black_per", "hispanic_per",
              "union_per", "age")

fixed_effects <- c("year_month", "occ")
fe <- create_formula("", fixed_effects)

eq <- create_formula("workers_per", controls)

top <- "\\begin{tabular}{lSS}
\\toprule
 & {NLSY 79} & {CPS}  \\\\  \\midrule
  "

center <- c("PBS Percent", "", "$R^2$", "Observations")

for (i in 1:2){

  temp <- lm_robust(eq, data = data_pbs[[i]],
                    fixed_effects = !!fe, clusters = occ,
                    se_type = "stata", try_cholesky = T)

  center <- cbind(
    center, 
    rbind(format_val(temp$coefficients["pbs_per"], r = 5, s = 5,
                     star = p_stars(temp$p.value["pbs_per"])),
          format_se(temp$std.error["pbs_per"], r = 5, s = 5),
          format_val(temp$r.squared), format_n(lm_N(temp))
    )
  )
}

center <- center |>
  add_endline() |>
  center_to_latex()

name <- "Effects of PBS Level within Occupation on Worker Share"
label <- "occ_pbs_reg_cps"
note <- "
Occupation level regressions of percent in Professional Business 
Service (PBS) industries within an occupation on percent of 
workers in an occupation. PBS Industries in the CPS have Census 
1990 Industry Codes between 721 and 760. PBS Industries in the 
NLSY 79 have Census 2000 Industry Codes between 7270 and 7790.
Each regression contains controls for percent female, Black, Hispanic,
and union member, average age, and occupation and month fixed effects.
Regressions use robust standard errors clustered at the 
occupation level. Stars represent significant difference from 0
at the .10 level (*), .05 level (**), and .01 level (***)."

header <- make_header("", name, label)
d_header <- make_header("d", name, label)
s_header <- make_header("s")

bot <- make_bot(note)

write.table(str_c(header, top, center, bot, "\n \\end{document}"),
            str_c(table_folder, "CPS/PBS Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save to Drafts and Slides
write.table(str_c(d_header, top, center, bot),
            str_c(d_table_folder, "CPS PBS Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

write.table(str_c(s_header, top, center, s_bot),
            str_c(s_table_folder, "CPS PBS Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# HO Occupation Summary Statistics ----------------------------------------

# Look at summary statistics for HO Occupation Jobs/People vs not
# Merge ho_occ info into full cps data
cps <- data.table(cps)

ho_occ <- occ_timeline_m |>
  filter(!is.na(ho_occ)) |>
  as_survey_design(ids = occ, weights = workers) |> 
  group_by(occ) |>
  summarise(
    ho_occ = unweighted(max(ho_occ)),
    outsourced_per = survey_mean(outsourced_per, na.rm = TRUE)
    ) |>
  data.table()

setkey(cps, occ)
setkey(ho_occ, occ)

cps <- merge(cps, ho_occ, all.x = T)

rm(ho_occ, crosswalk, occ_timeline_m, cpi)

# Download matched_jobs for summary statistics to compare
# matched_jobs <- read_csv(str_c(clean_folder, "matched_jobs.csv"),
#                          col_types = cols(
#                            .default = col_double(),
#                            week_start_job = col_date(format = ""),
#                            week_end_job = col_date(format = "")
#                          )) |>
#   # Create married/single if certain marital status
#   mutate(
#     single = 1 * (marital_status == 1),
#     married = 1 * (marital_status == 2)
#   ) |>
#   filter(!is.na(ho_occ))


# Download timeline data. To compare to CPS, keep
# only the first week of every month.
timeline <- 
  read_csv(str_c(clean_folder, "matched_timeline.csv"),
           col_types = cols(
             .default = col_double(),
             week = col_date(format = ""),
             week_start_job = col_date(format = ""),
             week_end_job = col_date(format = "")
             )) |>
  mutate(month = floor_date(week, unit = "month")) |>
  group_by(case_id, month) |>
  mutate(rank = rank(week)) |>
  filter(rank == 1, !is.na(ho_occ)) |>
  # Create married/single if certain marital status
  mutate(
    single = 1 * (marital_status == 1),
    married = 1 * (marital_status == 2)
    ) |>
  select(-rank, -month)

# Create three datasets
# 1. NLSY workers
# 2. All CPS workers
# 3. All CPS, NLSY cohort
# ho_ss <- list(matched_jobs, cps, filter(cps, nlsy79 == 1))
# 
# comp <- list(matched_jobs, cps, filter(cps, nlsy79 == 1))

ho_ss <- list(timeline, cps, filter(cps, nlsy79 == 1))

comp <- list(timeline, cps, filter(cps, nlsy79 == 1))

# Clean NLSY
vars_sum <- c("age", "female", "black", "hispanic",
              "less_hs", "hs", "aa", "ba", "plus_ba",
              "single", "married", "log_real_wkly_wage",
              "part_time", "union", "n")

# ho_ss[[1]] <- ho_ss[[1]] |>
#   as_survey_design(ids = case_id, weights = weight) |> 
#   group_by(ho_occ) |>
#   mutate(n = n()) |> 
#   summarise_at(vars_sum, survey_mean, na.rm = T) |> 
#   arrange(desc(ho_occ))

ho_ss[[1]] <- ho_ss[[1]] |>
  as_survey_design(ids = case_id, weights = weight) |> 
  group_by(ho_occ) |>
  mutate(n = n()) |> 
  summarise_at(vars_sum, survey_mean, na.rm = T) |> 
  arrange(desc(ho_occ))

# Clean CPS datasets
for (i in 2:3) {
  ho_ss[[i]] <- ho_ss[[i]] |>
    filter(!is.na(ho_occ)) |>
    group_by(ho_occ) |>
    summarise(
      log_real_hrly_wage_se = 
        weighted_se(log_real_hrly_wage, earnwt,
                    !is.na(log_real_hrly_wage)),
      log_real_hrly_wage = weighted_mean(log_real_hrly_wage, earnwt),
      log_real_wkly_wage_se = 
        weighted_se(log_real_wkly_wage, earnwt,
                    !is.na(log_real_wkly_wage)),
      log_real_wkly_wage = weighted_mean(log_real_wkly_wage, earnwt),
      union_se = weighted_se(union, earnwt),
      union = weighted_mean(union, earnwt),
      part_time_se = weighted_se(part_time, wtfinl),
      part_time = weighted_mean(part_time, wtfinl),
      female_se = weighted_se(female, wtfinl),
      female = weighted_mean(female, wtfinl),
      black_se = weighted_se(black, wtfinl),
      black = weighted_mean(black, wtfinl),
      hispanic_se = weighted_se(hispanic, wtfinl),
      hispanic = weighted_mean(hispanic, wtfinl),
      age_se = weighted_se(age, wtfinl),
      age = weighted_mean(age, wtfinl),
      less_hs_se = weighted_se(less_hs, wtfinl),
      less_hs = weighted_mean(less_hs, wtfinl),
      hs_se = weighted_se(hs, wtfinl),
      hs = weighted_mean(hs, wtfinl),
      aa_se = weighted_se(aa, wtfinl),
      aa = weighted_mean(aa, wtfinl),
      ba_se = weighted_se(ba, wtfinl),
      ba = weighted_mean(ba, wtfinl),
      plus_ba_se = weighted_se(plus_ba, wtfinl),
      plus_ba = weighted_mean(plus_ba, wtfinl),
      single_se = weighted_se(single, wtfinl),
      single = weighted_mean(single, wtfinl),
      married_se = weighted_se(married, wtfinl),
      married = weighted_mean(married, wtfinl),
      n = n()
    ) |>
    arrange(desc(ho_occ))

  comp[[i]] <- filter(comp[[i]], !is.na(ho_occ))
}

# Create a Latex table
top <- "\\begin{tabular}{lSS}
\\toprule
& {HO Occupation} & {Other Occupation} \\\\  \\midrule
"

# For Drafts and Slides, combine all three tables
c_top <- "\\begin{tabular}{lSSSSSS}
\\toprule
& \\multicolumn{2}{c}{{NLSY}} & \\multicolumn{2}{c}{{CPS}} & \\multicolumn{2}{c}{{CPS (NLSY Cohort)}} \\\\
& {HO Occ} & {Other Occ} & {HO Occ} & {Other Occ} & {HO Occ} & {Other Occ} \\\\  \\midrule
"

# Proportion varables
vars_p <- c("part_time", "union", "female", "black", "hispanic",
            "less_hs", "hs", "aa", "ba", "plus_ba", 
            "single", "married")

# Variables using earnwt (in CPS)
vars_earnwt <- c("log_real_hrly_wage", "log_real_wkly_wage", "union")

saves <- c("", "NLSY Cohort ")
descriptions <- c("for all workers", 
                  "for NLSY 79 cohort (born between 1957-1964) workers")
labels <- c("", "_nlsy")

n_center <- rbind("Age", "", "Female", "", "Black", "", "Hispanic", "",
                  "Less", "High School", "High School", "",
                  "Associate's", "Degree", "Bachelor's", 
                  "Degree", "Postgraduate", "Degree",
                  "Single", "", "Married", "", "Log Real",
                  "Weekly Wage", "Part-Time", "", "Union", "",
                  "Observations")

c_center <- c(n_center)

for (j in 1:3) {

  if (j > 1) {
    j_1 <- j - 1
    save <- saves[j_1]
    desc <- descriptions[j_1]
    lab <- labels[j_1]
  }

  cond <- comp[[j]]$ho_occ == 1

  for (i in 1:2) {

    c_i <- c()
    for (var in vars_sum[-length(vars_sum)]) {

      se <- str_c(var, "_se")
      # Name of weight depends on dataset/variable
      if (j == 1) {
        weights <- "weight"
      } else {
        weights <- if (var %in% vars_earnwt) "earnwt" else "wtfinl"
      }
      mode <- if (var %in% vars_p) "prop" else "mean"
      stars <- if (i == 2) diff_test(comp[[j]], var, weights, 
                                mode, cond, !cond, "ho_occ") else ""

      c_i <- rbind(c_i,
        format_val(ho_ss[[j]][[var]][i], r = 3, s = 3, star = stars),
        format_se(ho_ss[[j]][[se]][i], r = 4, s = 4)
      )
    }

    c_i <- rbind(c_i, format_n(ho_ss[[j]]$n[i]))

    center <- cbind(n_center, c_i)
    c_center <- cbind(c_center, c_i)
  }

  center <- center |>
    add_endline() |>
    center_to_latex()
  
  # Save CPS Tables
  if (j > 1) {
    name <- "Summary Statistics of Workers in HO Occupations"
    label <- str_c("ho_occ_cps", lab)
    note <- str_c(
      "Summary statistics from the CPS ", desc, 
      " workers divided by high-outsourcing (HO) occupations 
  (all occupations with outsourcing more than \\HO\\% in the NLSY))
  vs not from January 2001 to October 2016. Statstics are weighted 
  at the person level. Stars represent significant difference from 
  HO occupations at the .10 level (*), .05 level (**), and .01 level (***).
  ")
    
    header <- make_header("", name, label)
    bot <- make_bot(note)
    
    write.table(str_c(header, top, center, bot, "\n \\end{document}"),
                str_c(table_folder, "CPS/HO Occupation ",
                      save, "Summary Statistics.tex"),
                quote=F, col.names=F, row.names=F, sep="")
  }
}

c_center <- add_endline(c_center)

# For Slides, keep only certain variables
s_center <- c_center[c(1:18, 23:29), ]

c_center <- center_to_latex(c_center)
s_center <- center_to_latex(s_center)

name <- "Summary Statistics of Workers in HO Occupations: NLSY vs CPS"
label <- "ho_occ_cps"
note <- "
Summary statistics for workers in the NLSY and CPS.
NLSY data uses job data from the first week of every
month from January 2001 to October 2016. CPS data comes from 
the monthly January 2001 to October 2016 surveys
for all employed workers age 18--65 and for those born between 
1957--1964 (NLSY cohort). Workers are divided by if they work
in a high-outsourcing (HO) occupation (all occupations with 
outsourcing more than \\HO\\% in the NLSY). Statstics are 
weighted at the person level. Stars represent significant 
difference from HO occupations within own sample
at the .10 level (*), .05 level (**), and .01 level (***)."

header <- make_header("", name, label, colsep = 0.75)
d_header <- make_header("d", name, label, colsep = 0.75)
s_header <- make_header("s", size = "\\scriptsize", colsep = 1.5)

bot <- make_bot(note)

# Save for own use
write.table(
  str_c(header, c_top, c_center, bot, "\n \\end{document}"),
  str_c(table_folder, "CPS/HO Occupation Summary Statistics All.tex"),
  quote=F, col.names=F, row.names=F, sep="")

# Save in Drafts
write.table(str_c(d_header, c_top, c_center, bot),
            str_c(d_table_folder, 
                  "CPS HO Occupation Summary Statistics.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save in Slides 
write.table(str_c(s_header, c_top, s_center, s_bot),
            str_c(s_table_folder, 
                  "CPS HO Occupation Summary Statistics.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Make space
rm(data, data_pbs, data_r, ho_ss, matched_jobs,
   merger, merger_r, occ_corr, occ_corr_w, occ_corr_w_g, occ_cps,
   occ_cps_nlsy, occ_cps_nlsy_r, occ_cps_r, occ_pbs,
   occ_timeline_r_m, pbs_timeline_m, comp, cond, timeline)

# Wage Distribution ----------------------------------------------------

# Plot the wage distribution overall and for each year for 
# ho_occs vs non ho_occs.
# Rrun a regression using a quartic in age, race, marital status,
# education, and year/month fes; weighted by earnwt
# Run rif regressions to see the effect of more workers in ho_occupations
# or occupations with more outsourcing affects wage distribution
# Do this for all workers, and workers by low and high education
controls <- c("age", "I(age^2)", "I(age^3)", "I(age^4)", 
              "female", "black", "hispanic", "union",
              "married", "single",
              "hs", "aa", "ba", "plus_ba")

fe <- create_formula("", "year_month")

vars <- c("log_real_wkly_wage", "log_real_hrly_wage")
var_names <- c("Log Real Weekly Wage", "Log Real Hourly Wage")
save_names <- c("LRW Wage", "LRH Wage")

# For RIF
quantiles <- seq(0.75, 0.975, by = 0.025)

# Have all, low, and high education
wd_data <- list(cps, 
                filter(cps, high_ed == 0),
                filter(cps, high_ed == 1))

wd_saves <- c("All", "Low Education", "High Education")

for (j in seq_along(wd_data)) {
  
  data <- wd_data[[j]]
  wd_save <- wd_saves[j]
  
  for (i in seq_along(vars)) {
    
    var <- vars[i]
    var_sym <- sym(var)
    var_name <- var_names[i]
    save_name <- save_names[i]
    
    temp_max <- max(cps[[var]], na.rm = T)

    temp <- data |>
      filter(!is.na(ho_occ), !is.na(!!var_sym)) |>
      ggplot(aes_string(var, fill = "factor(ho_occ)")) +
      geom_density_bounds(
        alpha = 0.2, bounds = c(0, temp_max), color = "black",
        position = "identity") +
      labs(x = var_name, y = "Density") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      scale_fill_manual(name = "HO Occ", breaks = c(0, 1),
                        values = c("blue", "red"),
                        labels = c("No", "Yes")) +
      theme_light(base_size = 16)

    ggsave(str_c(figure_folder, save_name,
                 " Overall Distribution by HO Occ ", wd_save, ".pdf"),
           height = height, width = width)

    rm(temp)

    temp <- data |>
      filter(!is.na(ho_occ), !is.na(!!var_sym)) |>
      ggplot(aes_string(var, fill = "factor(ho_occ)")) +
      geom_density_bounds(
        alpha = 0.2, bounds = c(0, temp_max), color = "black",
        position = "identity") +
      labs(x = var_name, y = "Density") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      scale_fill_manual(name = "HO Occ", breaks = c(0, 1),
                        values = c("blue", "red"),
                        labels = c("No", "Yes")) +
      theme_light(base_size = 16) +
      facet_wrap(~ year)

    ggsave(str_c(figure_folder, save_name,
                 " Yearly Distribution by HO Occ ", wd_save, ".pdf"),
           height = height, width = width)

    rm(temp)
  
    # These are residual wages, but RIF regressions
    # might be more what I want
    # eq <- create_formula(var, controls)
    # 
    # reg <- lm_robust(eq, data = data, fixed_effects = !!fe,
    #                  weights = earnwt, se_type = "stata",
    #                  try_cholesky = TRUE)
    # 
    # temp <- data |>
    #   filter(!is.na(!!var_sym), !is.na(age), !is.na(female),
    #          !is.na(black), !is.na(hispanic),
    #          !is.na(union), !is.na(married), !is.na(hs)) |>
    #   mutate(residual = lm_residuals(reg)) |>
    #   filter(!is.na(ho_occ)) |>
    #   ggplot(aes(residual, fill = factor(ho_occ))) +
    #   geom_density(alpha = 0.2) +
    #   labs(x = str_c("Residual ", var_name), y = "Density") +
    #   scale_fill_manual(name = "HO Occ", breaks = c(0, 1),
    #                     values = c("blue", "red"),
    #                     labels = c("No", "Yes")) +
    #   scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    #   theme_light(base_size = 16)
    # 
    # ggsave(str_c(figure_folder, save_name, " Residuals by HO Occ ",
    #              wd_save, ".pdf"),
    #        height = height, width = width)
    # 
    # rm(temp, reg)
    
    # Get more sophisticated. Use RIF regressions on various 
    # quartiles of the regression and plot how ho_occ and
    # outsourced_per affect lrw wages
    # Note: program can't handle fixed effects, 
    # so include everything for now
    data_rif <- data |> 
      filter(!is.na(!!var_sym), !is.na(ho_occ), 
             !is.na(age), !is.na(female), 
             !is.na(black), !is.na(hispanic),
             !is.na(union), !is.na(married), !is.na(hs)) |> 
      select(log_real_hrly_wage, log_real_wkly_wage,
             ho_occ, outsourced_per, age, female, black,
             hispanic, union, married, single, hs, aa, ba,
             plus_ba, year_month, earnwt) 
     
    # If data is too big and we need to subset
    set.seed(1)
    data_rif <- sample_n(data_rif, min(500000, NROW(data_rif)))
    
    # rm(data)
    gc()
    
    eq_rif <- 
      create_formula(var, c("ho_occ", controls, "factor(year_month)"))
    
    reg_rif <- rifr(eq_rif, data_rif, weights = "earnwt", 
                    method = "quantile", quantile = quantiles,
                    kernel = "gaussian")
    
    # Get point estimates and ci. Turn into a df to plot
    coeffs <- reg_rif$Coef["ho_occ", ]
    up_ci <- coeffs + reg_rif$SE["ho_occ", ] * 1.96
    low_ci <- coeffs - reg_rif$SE["ho_occ", ] * 1.96
    
    df_rif <- tibble(
      quantiles, coeffs, up_ci, low_ci
    )
    
    temp <- df_rif |> 
      ggplot(aes(x=quantiles, y=coeffs)) +
      geom_hline(yintercept=0, linetype="dashed", 
                 color = "red", size=0.5) +
      geom_point() +
      geom_errorbar(aes(ymin=low_ci, ymax=up_ci), width=.02) +
      labs(x = "Wage Quantile", y = "Effect of HO Occupation") +
      xlim(0.7, 1) +
      theme_light(base_size = 16)
    
    ggsave(str_c(figure_folder, save_name, " RIF HO Occ ",
                 wd_save, ".pdf"),
           height = height, width = width)
    
    rm(temp, reg_rif, df_rif, coeffs, up_ci, low_ci)
    
    gc()
    
    # Try something similar with the outsourcing percent of worker's 
    # occupations
    # eq_rif <- 
    #   create_formula(var, c("outsourced_per", controls, "factor(year_month)"))
    # 
    # reg_rif <- rifr(eq_rif, data_rif, weights = "earnwt", 
    #                 method = "quantile", quantile = quantiles,
    #                 kernel = "gaussian")
    # 
    # # Get point estimates and ci. Turn into a df to plot
    # coeffs <- reg_rif$Coef["outsourced_per", ]
    # up_ci <- coeffs + reg_rif$SE["outsourced_per", ] * 1.96
    # low_ci <- coeffs - reg_rif$SE["outsourced_per", ] * 1.96
    # 
    # df_rif <- tibble(
    #   quantiles, coeffs, up_ci, low_ci
    # )
    # 
    # temp <- df_rif |> 
    #   ggplot(aes(x=quantiles, y=coeffs)) +
    #   geom_hline(yintercept=0, linetype="dashed", 
    #              color = "red", size=0.5) +
    #   geom_point() +
    #   geom_errorbar(aes(ymin=low_ci, ymax=up_ci), width=.02) +
    #   labs(x = "Quantile", y = "Effect of Outsourcing Percent") +
    #   xlim(0, 1) +
    #   theme_light(base_size = 16)
    # 
    # ggsave(str_c(figure_folder, save_name, " RIF Outsourcing Percent ", 
    #              wd_save, ".pdf"),
    #        height = height, width = width)
    
    rm(temp, data_rif, reg_rif, df_rif, coeffs, up_ci, low_ci)
  }
}
