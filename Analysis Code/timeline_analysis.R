# This file takes data from matched_timeline
# to compute summary statistics, run regressions, and make graphs

rm(list = ls())

# library(lpirfs)
library(outsourcing)
library(zeallot)
library(data.table)
library(estimatr)
library(data.table)
library(srvyr)
library(lubridate)
library(tidyverse)

# Folders of interest
folders <- name_folders("NLSY 79 Timeline")
c(raw_folder, clean_folder, table_folder, figure_folder,
  d_table_folder, s_table_folder) %<-% folders 

# For saving graphs
c(height, width) %<-% fig_size()

# Bottom of slide tables never changes, so make it once
s_bot <- make_bot(slide = TRUE)

# # Dataset is large, need to read as data table
# timeline <- fread(str_c(clean_folder, "matched_timeline.csv"), 
#                   colClasses = list(
#                     double = c(1, 3:52, 55:58),
#                     Date = c("week", "week_start_job", "week_end_job")
#                   ))

# fread having trouble with dates, so use read_csv
timeline <- read_csv(str_c(clean_folder, "matched_timeline.csv"),
                     col_types = cols(
                       .default = col_double(),
                       week = col_date(format = ""),
                       week_start_job = col_date(format = ""),
                       week_end_job = col_date(format = "")
                     )) |> 
  data.table()

# Create month and year from week in timeline (drop first observation
# that looks like it's in 2000)
timeline[year(week) > 2000, 
         `:=`(month = floor_date(week, unit = "month"),
              year = floor_date(week, unit = "year"))]

# # If I want a quick run through
# timeline <- timeline[week < ymd("2003-09-11")]

# Some figures calculated are useful for calibrating the model.
# Use these to update data_moments
data_moments <- read_csv(str_c(clean_folder, "data_moments.csv"),
                             col_types = cols(
                               variable = col_character(),
                               value = col_double()
                             ))

# Create a function to update data_moments given variable
# with correct name
update_parameters <- function(name, val) {
  data_moments$value[data_moments$variable == name] <- val
  data_moments
}

# # Create g_r for Great Recession between December 2007 to June 2009
# timeline <- timeline |>
#   mutate(g_r = 1 * (week >= ymd("2007-12-01")) 
#          & (week <= ymd("2009-06-30")))

# Plot Timelines ----------------------------------------------------------

# (Less Important now that we do this with Men + Women)

# Plot time series by Weeks, Months, Year, Age, and Weeks 
# for an age range
# 1. Percent of workers outsourced
# 2. Percent of workers in HO occupations
# 3. Percent of workers in HO occupations  who are outsourced
# 4. Log weekly wages of outsourced v traditional workers 
# in HO occupations
# 5. Number of workers unemployed based on ever HO occupations
# 6. Number of workers not working based on ever HO occupations
# 7. Number of workers in all job types
# 8. Number of workers in PBS industries

# vars_time <- c("week", "month", "year", "age")
# vars_save <- c("Week", "Month", "Year", "Age")
# vars_label <- c("Year", "Year", "Year", "Age")
# 
# # Create a function scale which takes index i and returns an
# # x_scale_date for week, month and year and an
# # x_scale_continuous for age
# scale <- function(i) {
#   if (i == 4){
#     scale_x_continuous(breaks = seq(min(timeline$age, na.rm = T),
#                                     max(timeline$age, na.rm = T),
#                                     by = 2))
#   } else {
#     scale_x_date(date_breaks = "2 years", date_labels = "%Y")
#   }
# }
# 
# # For Outsourced by week, show CWS and Katz and Krueger 
# # (2019) outsourcing levels on time graphs
# # Get data from https://www.rsfjournal.org/content/rsfjss/5/5/132.full.pdf
# # Table 1
# date_2001 <- round_date(ymd("2001-02-15"), "week")
# date_2005 <- round_date(ymd("2005-02-15"), "week")
# date_2015 <- round_date(ymd("2015-10-30"), "week")
# date_2017 <- round_date(ymd("2017-05-15"), "week")
# 
# # Make confidence intervals a bit more transparent
# alpha_ci <- 0.7
# 
# for (i in seq_along(vars_time)) {
#   
#   var_sym <- sym(vars_time[i])
# 
#   # 1. Percent of workers outsourced
#   # (do this separately for age, so we can add CWS/KK info)
#   if (i == 4) {
#     temp <- timeline |>
#       filter(!is.na(outsourced), !is.na(!!var_sym)) |>
#       as_survey_design(ids = case_id, weights=weight) |>
#       group_by_at(vars_time[i]) |>
#       summarise(outsourced_per = 
#                   survey_mean(outsourced * 100, vartype = "ci")) |>
#       ggplot() +
#       geom_line(aes_string(x = vars_time[i], y = "outsourced_per"), 
#                 color = "red", n = 4) +
#       geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp"),
#                 linetype="dashed", color = "red", n = 4,
#                 alpha = alpha_ci) +
#       geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low"),
#                 linetype="dashed", color = "red", 
#                 n = 4, alpha = alpha_ci) +
#       labs(x = vars_label[i], y = "Percent Outsourced") +
#       scale(i) +
#       theme_light(base_size = 16)
# 
#     ggsave(str_c(figure_folder, "Outsourced ", vars_save[i], ".pdf"),
#            height = height, width = width)
#   } else {
#     temp <- timeline |>
#       filter(!is.na(outsourced), !is.na(!!var_sym)) |>
#       as_survey_design(ids = case_id, weights=weight) |>
#       group_by_at(vars_time[i]) |>
#       summarise(outsourced_per = 
#                   survey_mean(outsourced * 100, vartype = "ci")) |>
#       ggplot() +
#       geom_line(aes_string(x = vars_time[i], y = "outsourced_per"),
#                 color = "red", n = 4) +
#       geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp"),
#                 linetype="dashed", color = "red", 
#                 n = 4, alpha = alpha_ci) +
#       geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low"),
#                 linetype="dashed", color = "red",
#                 n = 4, alpha = alpha_ci) +
#       geom_point(aes(x = date_2001, y = 1.1), 
#                  size = 2, color = "black") +
#       geom_text(aes(x = date_2001, y = 1.025, hjust = 0,
#                     label = "CWS 2001"), size = 6) +
#       geom_point(aes(x = date_2005, y = 1.4), 
#                  size = 2, color = "black") +
#       geom_text(aes(x = date_2005, y = 1.325, hjust = 0,
#                     label = "CWS 2005"), size = 6) +
#       geom_point(aes(x = date_2015, y = 2.5), 
#                  size = 2, color = "black") +
#       geom_text(aes(x = date_2015, y = 2.425, hjust = 1,
#                     label = "Katz and Krueger (2019)"), size = 6) +
#       geom_point(aes(x = date_2017, y = 1.4), 
#                  size = 2, color = "black") +
#       geom_text(aes(x = date_2017, y = 1.325, hjust = 1,
#                     label = "CWS 2017"), size = 6) +
#       labs(x = vars_label[i], y = "Percent Outsourced") +
#       scale(i) +
#       theme_light(base_size = 16)
# 
#     ggsave(str_c(figure_folder, "Outsourced ", vars_save[i], ".pdf"),
#            height = height, width = width)
#   }
# 
#   # 2. Percent of workers in HO occupations
#   temp <- timeline |>
#     filter(!is.na(ho_occ), !is.na(!!var_sym)) |>
#     as_survey_design(ids = case_id, weights=weight) |>
#     group_by_at(vars_time[i]) |>
#     summarise(ever_ho_occ_per = 
#                 survey_mean(ever_ho_occ * 100, vartype = "ci")) |>
#     ggplot() +
#     geom_line(aes_string(x = vars_time[i], y = "ever_ho_occ_per"), 
#               n = 4, color = "red") +
#     geom_line(aes_string(x = vars_time[i], y = "ever_ho_occ_per_upp"),
#               linetype="dashed", color = "red", 
#               n = 4, alpha = alpha_ci) +
#     geom_line(aes_string(x = vars_time[i], y = "ever_ho_occ_per_low"),
#               linetype="dashed", color = "red", 
#               n = 4, alpha = alpha_ci) +
#     labs(x = vars_label[i], y = "% in HO Occupations") +
#     scale(i) +
#     theme_light(base_size = 16)
# 
#   ggsave(str_c(figure_folder, "HO Occupation ", vars_save[i], ".pdf"),
#          height = height, width = width)
# 
#   # 3. Percent of workers ever in HO occupations who are outsourced
#   temp <- timeline |>
#     filter(ever_ho_occ == 1, !is.na(outsourced), !is.na(!!var_sym)) |>
#     as_survey_design(ids = case_id, weights=weight) |>
#     group_by_at(vars_time[i]) |>
#     summarise(outsourced_per = 
#                 survey_mean(outsourced * 100, vartype = "ci")) |>
#     ggplot() +
#     geom_line(aes_string(x = vars_time[i], y = "outsourced_per"),
#               n = 4, color = "red") +
#     geom_line(aes_string(x = vars_time[i], y = "outsourced_per_upp"),
#               linetype="dashed", color = "red", 
#               n = 4, alpha = alpha_ci) +
#     geom_line(aes_string(x = vars_time[i], y = "outsourced_per_low"),
#               linetype="dashed", color = "red", 
#               n = 4, alpha = alpha_ci) +
#     labs(x = vars_label[i], y = "% Outsourced Ever in HO Occupation") +
#     scale(i) +
#     theme_light(base_size = 16)
# 
#   ggsave(str_c(figure_folder, "Outsourced Ever HO Occupations ",
#                vars_save[i], ".pdf"),
#          height = height, width = width)
# 
#   # 4. Log weekly wages of outsourced v traditional workers ever in HO occupations
#   temp <- timeline |>
#     filter(!is.na(log_real_wkly_wage), ever_ho_occ == 1, 
#            !is.na(!!var_sym)) |>
#     as_survey_design(ids = case_id, weights=weight) |>
#     group_by_at(vars_time[i]) |>
#     group_by(outsourced, .add = T) |>
#     summarise(lrww_mean =
#                 survey_mean(log_real_wkly_wage, vartype = "ci")) |>
#     ggplot() +
#     geom_line(aes_string(x = vars_time[i], y = "lrww_mean",
#                          color = "factor(outsourced)"), n = 4) +
#     geom_line(aes_string(x = vars_time[i], y = "lrww_mean_upp",
#                          color = "factor(outsourced)"),
#               linetype="dashed", n = 4, alpha = alpha_ci) +
#     geom_line(aes_string(x = vars_time[i], y = "lrww_mean_low",
#                          color = "factor(outsourced)"),
#               linetype="dashed", n = 4, alpha = alpha_ci) +
#     labs(x = vars_label[i], y = "Log Real Weekly Wage") +
#     scale_color_manual(name = "Outsourced", breaks = c(0, 1),
#                        values = c("blue", "red"),
#                        labels = c("Not Outsourced", "Outsourced")) +
#     scale(i) +
#     theme_light(base_size = 16)
# 
#   ggsave(str_c(figure_folder, "Ever HO Occupation Weekly Wages ",
#                vars_save[i], ".pdf"),
#          height = height, width = width)
# 
#   # 5. Number of workers unemployed based on ever HO occupations
#   temp <- timeline |>
#     filter(!is.na(!!var_sym)) |>
#     as_survey_design(ids = case_id, weights=weight) |>
#     # Code is not allowing me to group by both at once for some reason
#     group_by_at(vars_time[i]) |>
#     group_by(ever_ho_occ, .add = T) |>
#     summarise(unemployed_per = 
#                 survey_mean(unemployed, vartype = "ci")) |>
#     ggplot() +
#     geom_line(aes_string(x = vars_time[i], y = "unemployed_per",
#                          color = "factor(ever_ho_occ)"),
#               n = 4) +
#     geom_line(aes_string(x = vars_time[i], y = "unemployed_per_upp",
#                          color = "factor(ever_ho_occ)"),
#               linetype="dashed", n = 4, alpha = alpha_ci) +
#     geom_line(aes_string(x = vars_time[i], y = "unemployed_per_low",
#                          color = "factor(ever_ho_occ)"),
#               linetype="dashed", n = 4, alpha = alpha_ci) +
#     labs(x = vars_label[i], y = "Unemployment Rate") +
#     scale_color_manual(name = "Ever HO Occ", breaks = c(0, 1),
#                        values = c("blue", "red"),
#                        labels = c("Never", "Ever")) +
#     scale(i) +
#     theme_light(base_size = 16)
# 
#   ggsave(str_c(figure_folder, "Unemployed ", vars_save[i], ".pdf"),
#          height = height, width = width)
# 
#   # 6. Number of workers not working based on ever HO occupations
#   temp <- timeline |>
#     filter(!is.na(!!var_sym)) |>
#     as_survey_design(ids = case_id, weights = weight) |>
#     # Code is not allowing me to group by both at once for some reason
#     group_by_at(vars_time[i]) |>
#     group_by(ever_ho_occ, .add = T) |>
#     summarise(not_working_per = survey_mean((1 - working) * 100, vartype = "ci")) |>
#     ggplot() +
#     geom_line(aes_string(x = vars_time[i], y = "not_working_per",
#                          color = "factor(ever_ho_occ)"), n = 4) +
#     geom_line(aes_string(x = vars_time[i], y = "not_working_per_upp",
#                          color = "factor(ever_ho_occ)"),
#               linetype="dashed", n = 4, alpha = alpha_ci) +
#     geom_line(aes_string(x = vars_time[i], y = "not_working_per_low",
#                          color = "factor(ever_ho_occ)"),
#               linetype="dashed", n = 4, alpha = alpha_ci) +
#     labs(x = vars_label[i], y = "Percent Not Working") +
#     scale_color_manual(name = "Ever HO Occ", breaks = c(0, 1),
#                        values = c("blue", "red"),
#                        labels = c("Never", "Ever")) +
#     scale(i) +
#     theme_light(base_size = 16)
# 
#   ggsave(str_c(figure_folder, "Not Working ", vars_save[i], ".pdf"),
#          height = height, width = width)
# 
#   # 7. Number of workers in all job types
#   breaks <- c("outsourced_per", "indep_con_per", "temp_work_per",
#               "on_call_per")
#   labels <- c("Outsourced", "Independent Contractor", "Temp Worker",
#               "On-Call Worker")
#   colors <- c( "red", "dark green", "orange", "purple")
# 
#   temp <- timeline |>
#     filter(!is.na(outsourced), !is.na(indep_con),
#            !is.na(temp_work), !is.na(on_call),
#            !is.na(!!var_sym)) |>
#     as_survey_design(ids = case_id, weights=weight) |>
#     group_by_at(vars_time[i]) |>
#     summarise(
#       outsourced_per = survey_mean(outsourced * 100),
#       indep_con_per = survey_mean(indep_con * 100),
#       temp_work_per = survey_mean(temp_work * 100),
#       on_call_per = survey_mean(on_call * 100)) |>
#     select(-ends_with("_se")) |>
#     gather(key = "var", value = "value", 2:5) |>
#     ggplot() +
#     geom_line(aes_string(x = vars_time[i], y = "value",
#                          color = "var"))  +
#     scale_color_manual(name = "Job Type", breaks = breaks,
#                        labels = labels, values = colors) +
#     labs(x = vars_label[i], y = "Percent Workers") +
#     scale(i) +
#     theme_light(base_size = 16)
# 
#   ggsave(str_c(figure_folder, "All Types ", vars_save[i], ".pdf"),
#          height = height, width = width)
# 
#   # 8. What percent of workers are in PBS industries?
#   temp <- timeline |>
#     filter(!is.na(pbs), !is.na(!!var_sym)) |>
#     as_survey_design(ids = case_id, weights=weight) |>
#     group_by_at(vars_time[i]) |>
#     summarise(pbs_per = survey_mean(pbs * 100, vartype = "ci")) |>
#     ggplot() +
#     geom_line(aes_string(x = vars_time[i], y = "pbs_per"),
#               color = "red", n = 4) +
#     geom_line(aes_string(x = vars_time[i], y = "pbs_per_upp"),
#               linetype="dashed", color = "red", 
#               n = 4, alpha = alpha_ci) +
#     geom_line(aes_string(x = vars_time[i], y = "pbs_per_low"),
#               linetype="dashed", color = "red", 
#               n = 4, alpha = alpha_ci) +
#     labs(x = vars_label[i], y = "Percent PBS") +
#     scale(i) +
#     theme_light(base_size = 16)
# 
#   ggsave(str_c(figure_folder, "PBS ", vars_save[i], ".pdf"),
#          height = height, width = width)
# }
# 
# # For Certain Age Groups, plot percent outsourced and percent outsourced in
# # HO Occupations
# age_mins <- c(43, 49)
# age_maxes <- c(47, 53)
# 
# for (i in 1:length(age_mins)) {
# 
#   age_min <- age_mins[i]
#   age_max <- age_maxes[i]
# 
#   # 1. Percent of workers outsourced
#   temp <- timeline |>
#     filter(!is.na(outsourced), !is.na(week),
#            age >= age_min, age <= age_max) |>
#     as_survey_design(ids = case_id, weights=weight) |>
#     group_by(week) |>
#     summarise(outsourced_per = 
#                 survey_mean(outsourced * 100, vartype = "ci")) |>
#     ggplot() +
#     geom_line(aes(x = week, y = outsourced_per), 
#               color = "red", n = 4) +
#     geom_line(aes(x = week, y = outsourced_per_upp),
#               linetype="dashed", color = "red", 
#               n = 4, alpha = alpha_ci) +
#     geom_line(aes(x = week, y = outsourced_per_low),
#               linetype="dashed", color = "red", 
#               n = 4, alpha = alpha_ci) +
#     labs(x = "Year", y = "Percent Outsourced") +
#     scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
#     theme_light(base_size = 16)
# 
#   ggsave(str_c(figure_folder, "Outsourced Ages ", 
#                age_min, "-", age_max, ".pdf"),
#          height = height, width = width)
# 
#   # 2. Percent of workers ever in HO occupations who are outsourced
#   temp <- timeline |>
#     filter(ever_ho_occ == 1, !is.na(outsourced), !is.na(week),
#            age >= age_min, age <= age_max) |>
#     as_survey_design(ids = case_id, weights=weight) |>
#     group_by(week) |>
#     summarise(outsourced_per = 
#                 survey_mean(outsourced * 100, vartype = "ci")) |>
#     ggplot() +
#     geom_line(aes(x = week, y = outsourced_per), color = "red",
#               n = 4) +
#     geom_line(aes(x = week, y = outsourced_per_upp),
#               linetype="dashed", color = "red", 
#               n = 4, alpha = alpha_ci) +
#     geom_line(aes(x = week, y = outsourced_per_low),
#               linetype="dashed", color = "red", 
#               n = 4, alpha = alpha_ci) +
#     labs(x = "Year", y = "% Outsourced Ever in HO Occupation") +
#     scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
#     theme_light(base_size = 16)
# 
#   ggsave(str_c(figure_folder, "Outsourced Ever HO Occupations Ages ",
#                age_min, "-", age_max, ".pdf"),
#          height = height, width = width)
# }
# 
# temp <- timeline |>
#   mutate(birth_year = year(year) - age) |>
#   filter(!is.na(outsourced), !is.na(birth_year)) |>
#   as_survey_design(ids = case_id, weights = weight) |>
#   group_by(year, birth_year) |>
#   summarise(outsourced_per = survey_mean(outsourced * 100)) |>
#   ggplot(aes(x = year, y = outsourced_per, 
#              color = factor(birth_year))) +
#   geom_line() +
#   labs(x = "Year", y = "Percent Outsourced", color = "Year Born") +
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
#   theme_light(base_size = 16)
# 
# ggsave(str_c(figure_folder, "Outsourced by Year Born.pdf"),
#        height = height, width = width)
# 
# # Plot weekly percent outsourced for only new jobs (started after 2001)
# # to get around pre-assignment of traditional
# temp <- timeline |>
#   filter(!is.na(outsourced), !is.na(week), 
#          week_start_job >= ymd("2001-01-01")) |>
#   as_survey_design(ids = case_id, weights=weight) |>
#   group_by(week) |>
#   summarise(outsourced_per = 
#               survey_mean(outsourced * 100, vartype = "ci")) |>
#   ggplot() +
#   geom_line(aes(x = week, y = outsourced_per), color = "red",
#             n = 4) +
#   geom_line(aes(x = week, y = outsourced_per_upp),
#             linetype="dashed", color = "red", 
#             n = 4, alpha = alpha_ci) +
#   geom_line(aes(x = week, y = outsourced_per_low),
#             linetype="dashed", color = "red", 
#             n = 4, alpha = alpha_ci) +
#   geom_point(aes(x = date_2001, y = 1.1), 
#              size = 2, color = "black") +
#   geom_text(aes(x = date_2001, y = 1.025, hjust = 0,
#                 label = "CWS 2001"), size = 6) +
#   geom_point(aes(x = date_2005, y = 1.4), 
#              size = 2, color = "black") +
#   geom_text(aes(x = date_2005, y = 1.325, hjust = 0,
#                 label = "CWS 2005"), size = 6) +
#   geom_point(aes(x = date_2015, y = 2.5), 
#              size = 2, color = "black") +
#   geom_text(aes(x = date_2015, y = 2.425, hjust = 1,
#                 label = "Katz and Krueger (2019)"), size = 6) +
#   geom_point(aes(x = date_2017, y = 1.4), 
#              size = 2, color = "black") +
#   geom_text(aes(x = date_2017, y = 1.325, hjust = 1,
#                 label = "CWS 2017"), size = 6) +
#   labs(x = "Year", y = "Percent Outsourced") +
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
#   theme_light(base_size = 16)
# 
# ggsave(str_c(figure_folder, "Outsourced Week New Jobs.pdf"),
#        height = height, width = width)
# 
# # Try to free some space to run all at once
# rm("temp", "breaks", "labels", "colors", "scale")

# Weekly Characteristics -------------------------------------------

# Create a table with average weekly charactersitcs
# for each week by ever_ho_occ

weekly <- timeline |>
  # Create new_job = 1 if w_tenure = 1 else NA
  mutate(new_job_o = ifelse(w_tenure == 1, 1, NA),
         new_outsourced = new_job_o * outsourced) |>
  as_survey_design(id = case_id, weight = weight) |>
  group_by(week, ever_ho_occ) |>
  summarise(
    not_working = survey_mean(1 - working, na.rm = T),
    unemployed = survey_mean(unemployed, na.rm = T),
    outsourced = survey_mean(outsourced, na.rm = T),
    new_outsourced = survey_mean(new_outsourced, na.rm = T),
    new_job = survey_mean(w_tenure == 1, na.rm = T),
    weight_w = unweighted(sum(weight) / 1000)
  )

weekly_ss <- weekly |>
  as_survey_design(id = week, weight = weight_w) |>
  group_by(ever_ho_occ) |>
  summarise(
    not_working = survey_mean(not_working, na.rm = T),
    unemployed = survey_mean(unemployed, na.rm = T),
    outsourced = survey_mean(outsourced, na.rm = T),
    new_outsourced = survey_mean(new_outsourced, na.rm = T),
    n = unweighted(n())
  ) |>
  arrange(desc(ever_ho_occ))

# Plot in Latex
table <- "
\\begin{tabular}{lSS}
\\toprule
& {Ever HO Occ} & {Never HO Occ}   \\\\  \\midrule \n
"

vars <- c("unemployed", "not_working", "outsourced", "new_outsourced")
var_names <- c("Unemployed", "Not Employed", "Outsourced",
               "New Jobs Outsourced")

for (i in seq_along(vars)) {
  var <- vars[i]
  se <- str_c(var, "se", sep = "_")
  cond <- weekly$ever_ho_occ == 1
  cond_y <- weekly$ever_ho_occ == 0
  stars <- diff_test(weekly, var, "weight_w", "mean", cond, 
                cond_y, "ever_ho_occ")
  table <- str_c(table,
    var_names[i],
    format_val(weekly_ss[[var]][1], r = 4, s = 4),
    format_val(weekly_ss[[var]][2], star = stars, r = 4, s = 4),
    "\\\\ \n",
    format_se(weekly_ss[[se]][1], r = 4, s = 4),
    format_se(weekly_ss[[se]][2], r = 4, s = 4), "\\\\ \n")
}

table <- str_c(table,
  "Observations", format_n(weekly_ss$n[1]), format_n(weekly_ss$n[2]),
  "\\\\ \n")

name <- "Labor Force Status of Ever HO Occupation Workers"
label <- "lf"

note <- "Average weekly unemployment and disemployment rates based on
if worker is ever employed in a high outsourcing occupation (HO Occ).
Stars represent
significant difference from ever HO Occupation at the .10 level *,
.05 level **, and .01 level ***."

# Save just for my own use
header <- make_header("", name, label)
bot <- make_bot(note)

write.table(str_c(header, table, bot),
            str_c(table_folder,
                  "NLSY79 HO Occupations/Unemployment.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Worker Flows ------------------------------------------------------------

# For ever_ho_occ,
# Calculate UE_hired, UE_outsourced, EU_hired, EU_outsourced,
# EE_hired_hired, EE_outsourced_hired, EE_hired_outsourced,
# and EE_outsourced_outsourced
# Record these in a table and in data moments
worker_flows <- timeline |>
  filter(ever_ho_occ == 1) |>
  # Drop people who never work
  # group_by(case_id) |>
  # mutate(periods_worked = sum(working),
  #        periods_observed = n()) |>
  # filter(periods_worked > 0, periods_observed > 400) |>
  # First, look at individual leads of unemployment or new jobs
  group_by(case_id) |>
  arrange(week) |>
  mutate(
    unemployed_lead = lead(unemployed),
    working_lead = lead(working),
    change_job = (lead(emp_id) != emp_id),
    lead_na = is.na(working_lead),
    outsourced_lead = lead(outsourced),
    traditional_lead = lead(traditional)
  ) |>
  ungroup() |>
  # Create 3 groups: unemployed; working_hired (traditional); 
  # and working_outsourced
  mutate(
    group = (unemployed 
             + 2 * ((traditional == 1) %in% T) 
             + 3 * ((outsourced == 1) %in% T))) |>
  filter(!is.na(group), group > 0) |>
  as_survey_design(id = case_id, weight = weight) |>
  group_by(group) |>
  summarise(
    total_weight = unweighted(sum(weight)),
    unemployed_lead = survey_mean(unemployed_lead, na.rm = T),
    working_lead = survey_mean(working_lead, na.rm = T),
    change_job = survey_mean(change_job, na.rm = T),
    lead_na = survey_mean(lead_na),
    outsourced_lead = survey_mean(outsourced_lead, na.rm = T),
    traditional_lead = survey_mean(traditional_lead, na.rm = T),
    n = unweighted(n())
  ) |> 
  mutate(population = total_weight / sum(total_weight))

# Testing something out below
# worker_flows_f <- timeline |>
#   filter(ever_ho_occ == 1) |>
#   # Drop people who never work
#   group_by(case_id) |>
#   mutate(periods_worked = sum(working),
#          periods_observed = n()) |>
#   filter(periods_worked > 0) |>
#   # First, look at individual leads of unemployment or new jobs
#   group_by(case_id) |>
#   arrange(week) |>
#   mutate(
#     unemployed_lead = lead(unemployed),
#     working_lead = lead(working),
#     change_job = (lead(emp_id) != emp_id),
#     lead_na = is.na(working_lead),
#     outsourced_lead = lead(outsourced),
#     traditional_lead = lead(traditional)
#   ) |>
#   ungroup() |>
#   # Create 3 groups: unemployed; working_hired (traditional); and working_outsourced
#   mutate(
#     group = (unemployed
#              + 2 * ((traditional == 1) %in% T)
#              + 3 * ((outsourced == 1) %in% T))) |>
#   filter(!is.na(group), group > 0) |>
#   group_by(group) |>
#   summarise(
#     unemployed_lead = mean(unemployed_lead, na.rm = T),
#     working_lead = mean(working_lead, na.rm = T),
#     lead_na = mean(lead_na),
#     change_job = mean(change_job, na.rm = T),
#     outsourced_lead = mean(outsourced_lead, na.rm = T),
#     traditional_lead = mean(traditional_lead, na.rm = T),
#     n = n()
#   ) |>
#   mutate(pop = n / sum(n))

# Calculate:
# 1. Unemployment by looking at percent unemployed
# 2. Percent of positions outsourced zeta
# by looking at percent of workers outsourced
# 3. Percent of vacancies from outsourcers pi
# by looking at percent of unemployed who become outsourced
u <- worker_flows$population[1]
zeta <- worker_flows$population[3] / (1 - u)
pi <- (worker_flows$outsourced_lead[1]
       / (worker_flows$outsourced_lead[1] 
          + worker_flows$traditional_lead[1]))


exist_lead_u <- 1 - worker_flows$lead_na[1] 
exist_lead_hired <- 1 - worker_flows$lead_na[2] 
exist_lead_outsourced <- 1 - worker_flows$lead_na[3] 

# Calculate various flows
UE <- worker_flows$working_lead[1] / exist_lead_u

# EU_hired <- 1 - (worker_flows$working_lead[2] 
#                  - worker_flows$lead_na[2])
# EU_outsourced <- 1 - (worker_flows$working_lead[3] 
#                       - worker_flows$lead_na[3])

EU_hired <- (1 - worker_flows$working_lead[2]) / exist_lead_hired
EU_outsourced <- 
  (1 - worker_flows$working_lead[3]) / exist_lead_outsourced

# EU_hired <- worker_flows$unemployed_lead[2]
# EU_outsourced <- worker_flows$unemployed_lead[3]

n <- (1 - zeta) * EU_hired + zeta * EU_outsourced
d <- n + UE
u_t <- n / d

EE_hired <- worker_flows$change_job[2] / exist_lead_hired
EE_outsourced <- worker_flows$change_job[3] / exist_lead_outsourced

leave_hired <- EU_hired + EE_hired
leave_outsourced <- EU_outsourced + EU_outsourced
MT_est_hired <- log(.5) / log(1 - leave_hired)
MT_est_outsourced <- log(.5) / log(1 - leave_outsourced)
# (1 - leave_hired)^1000
# (1 - leave_outsourced)^1000

JJ_hired <- EE_hired / leave_hired
JJ_outsourced <- EE_outsourced / leave_outsourced

# Update model parameters
data_moments <- update_parameters("u", u)
data_moments <- update_parameters("zeta", zeta)
data_moments <- update_parameters("pi", pi)
data_moments <- update_parameters("UE", UE)
data_moments <- update_parameters("EU_hired", EU_hired)
data_moments <- update_parameters("EU_outsourced", EU_outsourced)
data_moments <- update_parameters("EE_hired", EE_hired)
data_moments <- update_parameters("EE_outsourced", EE_outsourced)

write_csv(data_moments, str_c(clean_folder, "data_moments.csv"))

# Max Tenure Distribution -------------------------------------------------------

# Look at highest tenure of each job for outsourced vs not 
# overall and in outsourcing occupations
tenure_summary <- split_data(timeline)

for (i in 1:2) {
  tenure_summary[[i]] <- tenure_summary[[i]] |>
    group_by(case_id) |>
    arrange(week) |>
    mutate(
      unemployed_lead = lead(unemployed),
      working_lead = lead(working),
      change_job = (lead(emp_id) != emp_id),
      lead_na = is.na(working_lead),
      outsourced_lead = lead(outsourced) 
    ) |> 
    filter(!is.na(max_tenure), (traditional == 1 | outsourced == 1),
           w_tenure == max_tenure) |> 
    as_survey_design(ids = case_id, weights=weight) |>
    group_by(outsourced) |> 
    summarise(
      tenure_mean = survey_mean(max_tenure, vartype = "ci"),
      tenure_median = survey_median(max_tenure),
      unemployed_lead = survey_mean(unemployed_lead, na.rm = T),
      working_lead = survey_mean(working_lead, na.rm = T),
      change_job= survey_mean(change_job, na.rm = T),
      lead_na = survey_mean(lead_na, na.rm = T),
      obs = unweighted(n())
    ) |> 
    arrange(desc(outsourced))
}

save_ho <- c("", " HO Ocupations")
time_plot <- split_data(timeline)  

# Plot Max Tenure
for (ho in 1:2) {
  temp <- time_plot[[ho]] |>
    filter(!is.na(max_tenure), (traditional == 1 | outsourced == 1)) |> 
    group_by(case_id, emp_id) |> 
    filter(w_tenure == max_tenure) |>
    ggplot() +
    geom_density_bounds(
      aes(max_tenure, fill = factor(outsourced)), 
      alpha = 0.2, bounds = c(0, Inf), color = "black") +
    geom_vline(aes(xintercept = tenure_summary[[i]]$tenure_median[1]),
               color = "red", size=1) +
    geom_vline(aes(xintercept = tenure_summary[[i]]$tenure_median[2]),
               color = "blue", size=1) +
    labs(x = "Max Weeks Tenure", y = "Density") +
    scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = c("Traditional", "Outsourced")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_light(base_size = 16)
  
  ggsave(str_c(figure_folder, "Max Tenure", save_ho[ho], ".pdf"),
         height = height, width = width)
}

# Use tenure_summary[[2]] (ever_ho_occ) to update MedTenure_hired and 
# MedTenure_outsourced using median tenure for non-outsourced 
# and outsourced workers
MT_hired <- tenure_summary[[2]]$tenure_median[2]
MT_outsourced <- tenure_summary[[2]]$tenure_median[1]
leave_hired_2 <- 1 - .5 ^ (1 / MT_hired)
leave_outsourced_2 <- 1 - .5 ^ (1 / MT_outsourced)
JJ_hired_2 <- 
  (tenure_summary[[2]]$working_lead[2] 
   / (1 - tenure_summary[[2]]$lead_na[2]))
JJ_outsourced_2 <- (tenure_summary[[2]]$working_lead[1] 
                    / (1 - tenure_summary[[2]]$lead_na[1]))

# data_moments <- update_parameters("MedTenure_hired", MT_hired)
# data_moments <- update_parameters("MedTenure_outsourced",
#                                   MT_outsourced)
# 
# write_csv(data_moments, str_c(clean_folder, "data_moments.csv"))

# How do the model's implied job tenures look?
max_w <- 1500
weeks <- 0:max_w
keep_hired <- 1 - (1 - leave_hired) ^ weeks
keep_outsourced <- 1 - (1 - leave_outsourced) ^ weeks

# Turn keep into length pdf and adjust for population size
temp_plot <- time_plot[[2]] |>
  filter(!is.na(max_tenure), (traditional == 1 | outsourced == 1)) |> 
  group_by(case_id, emp_id) |> 
  filter(w_tenure == max_tenure)

pdf <- (keep_hired[2:length(weeks)] - keep_hired[1:(length(weeks) - 1)])
pdf_outsourced <- (keep_outsourced[2:length(weeks)] 
                   - keep_outsourced[1:(length(weeks) - 1)])

d <- "Trad"
d_o <- "Out"
sim <- str_c("EE + EU hired = ", round(leave_hired, 3))
sim_o <- str_c("EE + EU outsourced = ", round(leave_outsourced, 3))
temp <- ggplot() +
  geom_density_bounds(
    aes(x = temp_plot$tenure[temp_plot$traditional == 1],
        fill = d),
    fill = "blue", alpha = 0.3, bounds = c(0, Inf), color = "black") +
  geom_density_bounds(
    aes(x = temp_plot$tenure[temp_plot$outsourced == 1], 
        fill = d_o),
    fill = "red", alpha = 0.3, bounds = c(0, Inf), color = "black") +
  geom_point(aes(x = 1:max_w, y = pdf, color = sim), alpha = 1) +
  geom_point(aes(x = 1:max_w, y = pdf_outsourced, color = sim_o), 
             alpha = 1) +
  labs(x = "Weeks Tenure", y = "Count") +
  scale_fill_manual(name = "Data",
                    values = c("blue", "red"),
                    labels = c(d, d_o) ) +
  scale_color_manual((name = "Model"),
                     values = c("green4", "orange"),
                     labels = c(sim, sim_o)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_light(base_size = 16)

ggsave(str_c(figure_folder, "Max Tenure Data vs Model.pdf"),
       height = height, width = width)

# I can use worker flows (see previous section) or tenure + employment 
# after to measure many flows. How do these compare?

# Plot these in a table
table <- str_c("
\\begin{tabular}{lSS}
\\toprule
Variable & {Flows} & {Tenure + Next Period}  \\\\ \\midrule\n",
"$UE$", format_val(UE, r=4, s=4), "& {-}", "\\\\\n",
"$EU$ Hired", format_val(EU_hired, r=4, s=4), "& {-}" , "\\\\\n",
"$EU$ Outsourced", format_val(EU_outsourced, r=4, s=4), "& {-}",
"\\\\\n",
"$EE$ Hired", format_val(EE_hired, r=4, s=4), "& {-}", 
"\\\\\n",
"$EE$ Outsourced", format_val(EE_outsourced, r=4, s=4), "& {-}",
"\\\\\n",
"$EE+EU$ Hired", format_val(leave_hired, r=4, s=4), 
format_val(leave_hired_2, r=4, s=4), "\\\\\n",
"$EE+EU$ Outsourced", format_val(leave_outsourced, r=4, s=4), 
format_val(leave_outsourced_2, r=4, s=4), "\\\\\n",
"$\\frac{EE}{EE+EU}$ Hired", format_val(JJ_hired, r=2, s=2), 
format_val(JJ_hired_2, r=2, s=2), "\\\\\n",
"$\\frac{EE}{EE+EU}$ Outsourced", format_val(JJ_outsourced, r=2, s=2), 
format_val(JJ_outsourced_2, r=2, s=2), "\\\\\n",
"Median Tenure Hired", format_val(MT_est_hired, r=0, s=0), 
format_val(MT_hired, r=0, s=0), "\\\\\n",
"Median Tenure Outsourced", format_val(MT_est_outsourced, r=0, s=0), 
format_val(MT_outsourced, r=0, s=0), "\\\\\n")

# Save this just for my own use
name <- "Worker Flows"
label <- "timeline_flows"
note <- "Weekly worker flows using both overall flows and 
job tenure plus leading info from week after job ends. 
These theoretically should be the same."

header <- make_header("", name, label)
bot <- make_bot(note)

write.table(str_c(header, table, bot), 
            str_c(table_folder,
                         "NLSY79 Worker Flows/Worker Flows.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# Occupation Outsourcing vs Characteristics ----------------------------------------

# Group occupations by week and compare trend in employment to 
# trend in outsourcing

# Unweighted
occ_timeline <- timeline |>
  # filter(year(week) <= 2012) |> 
  filter(!is.na(occ), !is.na(outsourced)) |>
  group_by(week, occ, outsourced, self_emp, indep_con, 
           temp_work, on_call) |>
  summarise(
    ho_occ = mean(ho_occ),
    n = sum(weight),
    sd_log_real_hrly_wage = sd(log_real_hrly_wage, na.rm = T),
    sd_log_real_wkly_wage = sd(log_real_wkly_wage, na.rm = T),
    log_real_hrly_wage = mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage = mean(log_real_wkly_wage, na.rm = T),
    job_sat = mean(job_sat, na.rm = T),
    hours_week = mean(hours_week, na.rm = T),
    part_time = mean(part_time, na.rm = T),
    tenure = mean(tenure, na.rm = T),
    health = mean(health, na.rm = T),
    retirement = mean(retirement, na.rm = T),
    any_benefits = mean(any_benefits, na.rm = T),
    n_black = sum(black * weight),
    n_hispanic = sum(hispanic * weight),
    n_union = sum(union * weight, na.rm = T),
    n_union_defined = sum((!is.na(union) %in% T) * weight),
    tot_age = sum(age * weight),
    tot_age_2 = sum((age ^ 2) * weight)
  ) |>
  pivot_wider(names_from = c(outsourced, self_emp, indep_con,
                             temp_work, on_call),
              values_from = n:tot_age_2) |>
  ungroup() |> 
  # Rename pivot table so easier to reference 
  rename_at(vars(ends_with("_0_0_0_0_0")), 
            ~ str_replace(., "_0_0_0_0_0", "_0")) |> 
  rename_at(vars(ends_with("_1_0_0_0_0")),
            ~ str_replace(., "_1_0_0_0_0", "_1")) |> 
  rename_at(vars(ends_with("_0_1_0_0_0")), 
            ~ str_replace(., "_0_1_0_0_0", "_2")) |> 
  rename_at(vars(ends_with("_0_0_1_0_0")), 
            ~ str_replace(., "_0_0_1_0_0", "_3")) |> 
  rename_at(vars(ends_with("_0_0_0_1_0")), 
            ~ str_replace(., "_0_0_0_1_0", "_4")) |> 
  rename_at(vars(ends_with("_0_0_0_0_1")), 
            ~ str_replace(., "_0_0_0_0_1", "_5")) |> 
  mutate(
    workers = r_sum(n_0, n_1, n_2, n_3, n_4, n_5),
    outsourced_per = ifelse(!is.na(n_1), n_1 / workers * 100, 0),
    self_emp_per = ifelse(!is.na(n_2), n_2 / workers * 100, 0),
    indep_con_per = ifelse(!is.na(n_3), n_3 / workers * 100, 0),
    temp_work_per = ifelse(!is.na(n_4), n_4 / workers * 100, 0),
    on_call_per = ifelse(!is.na(n_5), n_5 / workers * 100, 0),
    black_per = (
      r_sum(n_black_0, n_black_1, n_black_2, n_black_3, 
            n_black_4, n_black_5)
      / workers * 100),
    hispanic_per = (
      r_sum(n_hispanic_0, n_hispanic_1, n_hispanic_2, 
            n_hispanic_3, n_hispanic_4, n_hispanic_5) 
      / workers * 100),
    union_per = (r_sum(n_union_0, n_union_1, n_union_2,
                       n_union_3, n_union_4, n_union_5) / 
                   r_sum(n_union_defined_0, n_union_defined_1, 
                         n_union_defined_2, n_union_defined_3, 
                         n_union_defined_4, n_union_defined_5)
                 * 100),
    age = (r_sum(tot_age_0, tot_age_1, tot_age_2, tot_age_3, 
                 tot_age_4, tot_age_5) 
           / workers),
    age_2 = (r_sum(tot_age_2_0, tot_age_2_1, tot_age_2_2,
                   tot_age_2_3, tot_age_2_4, tot_age_2_5) 
             / workers)
  ) |> 
  group_by(week) |> 
  mutate(workers_per = workers / sum(workers) * 100) |> 
  group_by(occ) |> 
  mutate(average_size = mean(workers_per)) |> 
  ungroup()

occ_corr <- occ_timeline |> 
  group_by(occ) |>
  summarise(
    corr_emp = cor(outsourced_per, workers_per, 
                   use = "na.or.complete"),
    corr_out_wages = 
      cor(outsourced_per[which(log_real_wkly_wage_1 > 0)],
          log_real_wkly_wage_1[which(log_real_wkly_wage_1 > 0)], 
          use = "na.or.complete"),
    corr_wages = cor(outsourced_per, log_real_wkly_wage_0, 
                     use = "na.or.complete"),
    ho_occ = mean(ho_occ),
    average_size = mean(average_size)
    ) |> 
  filter(!is.na(corr_emp))

# Look at correlation weighted by average occ size
occ_corr_w <- occ_corr |> 
  as_survey_design(ids = occ, weights = average_size) |> 
  summarise(
    corr_emp = unweighted(mean(corr_emp)),
    corr_emp_se = unweighted(sd(corr_emp) / sqrt(n())),
    w_corr_emp = survey_mean(unlist(corr_emp), vartype = "se"),
    
    corr_out_wages = unweighted(mean(corr_out_wages, na.rm = T)),
    corr_out_wages_se = 
      unweighted(sd(corr_out_wages, na.rm = T) /
                   sqrt(sum(!is.na(corr_out_wages)))),
    w_corr_out_wages = 
      survey_mean(unlist(corr_out_wages), na.rm = T, vartype = "se"),
    corr_wages = unweighted(mean(corr_wages, na.rm = T)),
    corr_wages_se = unweighted(sd(corr_wages, na.rm = T) /
                                 sqrt(sum(!is.na(corr_wages)))),
    w_corr_wages = 
      survey_mean(unlist(corr_wages), na.rm = T, vartype = "se")
    ) 

# Look at correlation weighted by average occ size for ho vs not occs
occ_corr_w_g <- occ_corr |> 
  as_survey_design(ids = occ, weights = average_size) |> 
  group_by(ho_occ) |> 
  summarise(
    corr_emp = unweighted(mean(corr_emp)),
    corr_emp_se = unweighted(sd(corr_emp) / sqrt(n())),
    w_corr_emp = 
      survey_mean(unlist(corr_emp), vartype = "se"),
    
    corr_out_wages = unweighted(mean(corr_out_wages, na.rm = T)),
    corr_out_wages_se = 
      unweighted(sd(corr_out_wages, na.rm = T) /
                   sqrt(sum(!is.na(corr_out_wages)))),
    w_corr_out_wages = 
      survey_mean(unlist(corr_out_wages), na.rm = T, vartype = "se"),
    
    corr_wages = unweighted(mean(corr_wages, na.rm = T)),
    corr_wages_se = unweighted(sd(corr_wages, na.rm = T) /
                                 sqrt(sum(!is.na(corr_wages)))),
    w_corr_wages = 
      survey_mean(unlist(corr_wages), na.rm = T, vartype = "se")
  ) |> 
  arrange(ho_occ)

# # # Weighted (not working)
# occ_timeline <- timeline |>
#   filter(!is.na(occ), !is.na(outsourced), occ < 100) |>
#   as_survey_design(ids = case_id, weights=weight) |>
#   group_by(week, occ, outsourced) |>
#   summarise(
#     ho_occ = unweighted(mean(ho_occ)),
#     # n = survey_count(),
#     log_real_wkly_wage = survey_mean(log_real_wkly_wage, na.rm = T),
#     hours_week = survey_mean(hours_week, na.rm = T),
#     part_time = survey_mean(part_time, na.rm = T),
#     tenure = survey_mean(tenure, na.rm = T),
#     health = survey_mean(health, na.rm = T),
#     retirement = survey_mean(retirement, na.rm = T),
#     any_benefits = survey_mean(any_benefits, na.rm = T),
#     n_female = survey_total(female),
#     n_black = survey_total(black),
#     n_hispanic = survey_total(hispanic),
#     n_union = survey_total(union * weight),
#     n_union_defined = survey_total(!is.na(union)),
#     tot_age = survey_total(age),
#     tot_age_2 = survey_total(age_2)
#   ) |>
#   pivot_wider(names_from = outsourced,
#               values_from = n:tot_age_2) |>
#   ungroup() |> 
#   mutate(
#     workers = r_sum(n_0, n_1),
#     outsourced_per = ifelse(!is.na(n_1), n_1 / workers * 100, 0),
#     black_per = r_sum(n_black_0, n_black_1) / workers * 100,
#     hispanic_per = r_sum(n_hispanic_0, n_hispanic_1) / workers * 100,
#     union_per = (r_sum(n_union_0, n_union_1) / 
#                    r_sum(n_union_defined_0, n_union_defined_1) * 100),
#     age = r_sum(n_0 * tot_age_0, n_1 * tot_age_1) / workers,
#     age_2 = r_sum(n_0 * tot_age_2_0, n_1 * tot_age_2_1) / workers 
#   ) |> 
#   group_by(week) |> 
#   mutate(workers_per = workers / sum(workers) * 100) |> 
#   group_by(occ) |> 
#   mutate(workers_mean = mean(workers)) |> 
#   ungroup()

# Create graphs of distributions and create a table with each t.test

vars_c <- c("emp", "wages", "out_wages")
var_names <- c("Employment", "Traditional Wages", "Outsourced Wages")
bins <- c(15, 15, 15)

table_c <- "
\\begin{tabular}{lSSSS}
\\toprule
Group & {Unweighted Correlation} & {t-stat} & {Weighted Correlation} & {t-stat}  \\\\
\\midrule\n"

for (i in 1:3) {
  corr <- str_c("corr_", vars_c[i])
  corr_sym <- sym(corr)
  corr_se <- str_c(corr, "_se")
  w_corr <- str_c("w_", corr)
  w_corr_se <- str_c("w_", corr_se)
  
  corr_all <- occ_corr_w[[corr]]
  corr_ho <- occ_corr_w_g[[corr]][2]
  corr_lo <- occ_corr_w_g[[corr]][1]
  t_all <- occ_corr_w[[corr]] / occ_corr_w[[corr_se]]
  t_ho <- occ_corr_w_g[[corr]][2] / occ_corr_w_g[[corr_se]][2]
  t_lo <- occ_corr_w_g[[corr]][1] / occ_corr_w_g[[corr_se]][1]
  
  w_corr_all <- occ_corr_w[[w_corr]]
  w_corr_ho <- occ_corr_w_g[[w_corr]][2]
  w_corr_lo <- occ_corr_w_g[[w_corr]][1]
  w_t_all <- occ_corr_w[[w_corr]] / occ_corr_w[[w_corr_se]]
  w_t_ho <- occ_corr_w_g[[w_corr]][2] / occ_corr_w_g[[w_corr_se]][2]
  w_t_lo <- occ_corr_w_g[[w_corr]][1] / occ_corr_w_g[[w_corr_se]][1]
  
  temp <- occ_corr |> 
    filter(!is.na(!!corr_sym)) |> 
    ggplot() +
    geom_histogram(aes_string(corr, fill = "factor(ho_occ)"),
                   alpha = 0.4, bins = bins[i]) +
    geom_vline(aes(xintercept = corr_all), size=1) +
    geom_vline(aes(xintercept = corr_ho), color = "red", size=1) +
    geom_vline(aes(xintercept = corr_lo), color = "blue", size=1) +
    labs(x = "Correlation", y = "Count") +
    scale_fill_manual(name = "Occupation\nOutsourcing", 
                      breaks = c(0, 1), values = c("blue", "red"),
                      labels = list(expression("Low (" < " 3.4%)"), 
                                    expression("High (" >= " 3.4%)"))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, var_names[i], " Correlation.pdf"),
         height = height, width = width)
  
  # What about correlation weighted by occupation size?
  temp <- occ_corr |> 
    filter(!is.na(!!corr_sym)) |> 
    ggplot() +
    geom_point(aes_string(x = corr, y = "average_size",
                          color = "factor(ho_occ)"), alpha = 0.4) +
    geom_vline(aes(xintercept = w_corr_all), size=1) +
    geom_vline(aes(xintercept = w_corr_ho), color = "red", size=1) +
    geom_vline(aes(xintercept = w_corr_lo), color = "blue", size=1)  +
    labs(x = "Correlation", y = "Average Occupation Worker Share") +
    scale_color_manual(name = "Occupation\nOutsourcing", 
                       breaks = c(0, 1), values = c("blue", "red"),
                      labels = list(expression("Low (" < " 3.4%)"), 
                                    expression("High (" >= " 3.4%)"))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, var_names[i], 
               " Weighted Correlation.pdf"),
         height = height, width = width)
  
  # Append table
  table_c <- str_c(table_c, 
    var_names[i], " & & & & \\\\ \\midrule\n",
    "All ",
    format_val(corr_all), format_val(t_all),
    format_val(w_corr_all), format_val(w_t_all), " \\\\ \n",
    "High Outsourcing ",
    format_val(corr_ho), format_val(t_ho), 
    format_val(w_corr_ho), format_val(w_t_ho), " \\\\ \n",
    "Low Outsourcing ",
    format_val(corr_lo), format_val(t_lo), 
    format_val(w_corr_lo), format_val(w_t_lo), " \\\\ \n")
  
  if (i %in% c(1, 2)) {
    table_c <- str_c(table_c, "\\midrule\n")
  } 
}

name <- "Occupation Level Correlations With Outsourcing"
label <- "occ_corr"
note <- "
Mean correlation between an occupation's percent of all jobs 
and either percent of that occupation that is outsourced or 
wage level of non-outsourced and outsourced workers at the
week level. High outsourcing occupations are occupations that
are outsourced at more than twice the average rate, 
low outsourcing occupations are all others. Unweighted
correlations treat all occupations the same, weighted weight by 
average weekly observations."

# Only save for own use
header <- make_header("", name, label)
bot <- make_bot(note)

write.table(str_c(header, table_c, bot),
            str_c(table_folder,
                  "NLSY79 Occupation Info/Occupation Correlation.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# How do these correlations change with controls? Run regressions
# Note, no workers_per regressions in these tables,
# do those with the CPS regressions. These
# are mainly for my own use
controls <- c("outsourced_per", "self_emp_per", "indep_con_per",
              "temp_work_per", "on_call_per", "black_per", 
              "hispanic_per", "union_per", "age")

fixed_effects <- c("week", "occ")
fe <- create_formula("", fixed_effects)

outcomes <- c("log_real_hrly_wage", "log_real_wkly_wage",
              "hours_week", "part_time", "any_benefits",
              "health")

# Create a table with regression results
top <- "\\begin{tabular}{lSSSSSS}
\\toprule
 & {Log Real} & {Log Real} & {Hours Worked} & {} & {Any} & {Health} \\\\
& {Hourly Wage} & {Weekly Wage} & {per Week} & {Part-Time} & {Benefits} & {Insurance} \\\\  \\midrule
"

center_r <- c("Outsourced Percent", "", "$R^2$", "Observations")

center <- list(center_r, center_r)
out_names <- c("Traditional", "Outsourced")
out_labels <- c("trad", "out")

# Save two tables, one for effects on traditional aggregates
# and one on effects on outsourced aggregates
for (out in 0:1){
  out_1 <- out + 1
  for (i in seq_along(outcomes)){
      
    outcome <- str_c(outcomes[i], "_", out)
    eq <- create_formula(outcome, controls)
    
    temp <- lm_robust(eq, data = occ_timeline, 
                      fixed_effects = !!fe,
                      clusters = occ, se_type = "stata", 
                      try_cholesky = TRUE)
    
    center[[out_1]] <- cbind(center[[out_1]],
      rbind(format_val(temp$coefficients["outsourced_per"], 
                       p_stars(temp$p.value["outsourced_per"]), 
                       r = 4, s = 4),
            format_se(temp$std.error["outsourced_per"], r = 4, s = 4),
            format_val(temp$r.squared), format_n(lm_N(temp)))
    )
  }
}

# Combine the two center tables into 1
center_t <- rbind(
  c("\\textbf{Traditional}", " & ",  " & ", " & ", " & ", " & ", " & "),
  center[[1]],
  c("\\textbf{Outsourced}", " & ", " & ", " & ", " & ", " & ", " & "),
  center[[2]]
)

center_t <- center_t |>
  add_endline(midrule = c(5)) |>
  center_to_latex()

name <- "Effects of Outsourcing Level within Occupation on Workers"
label <- "occ_reg_week"
note <- "
Occupation level regressions of percent outsourced 
within occupation each week on average job characteristics.
Sample consists of men in the NLSY, aggregated at the occupation
level. Each regression contains controls for percent 
in other alternative job types (ie. independent contractor, 
temp workers), percent Black, Hispanic, and union member, 
average age, and occupation and week fixed effects.
Regressions use robust standard errors clustered at the 
occupation level. Stars represent significant difference 
from 0 at the .10 level *, .05 level **, and .01 level ***."

# Save only for myself
header <- make_header("", name, label, colsep = 0.25)
bot <- make_bot(note)

write.table(
  str_c(header, top, center_t, bot, "\n \\end{document}"),
  str_c(table_folder,
        "NLSY79 Occupation Info/Occupation Regressions Week.tex"),
  quote=F, col.names=F, row.names=F, sep="")

# Occupation Characteristics vs Outsourcing at Yearly Level ---------------

# Try similar analysis, but aggregate to year level first. 
# Maybe will be easier to see changes in wages
# Unweighted
occ_timeline_y <- timeline |>
  # filter(year(week) <= 2012) |> 
  filter(!is.na(occ), !is.na(outsourced)) |>
  group_by(year, occ, outsourced, self_emp, 
           indep_con, temp_work, on_call) |>
  summarise(
    ho_occ = mean(ho_occ),
    n = sum(weight),
    sd_log_real_hrly_wage = sd(log_real_hrly_wage, na.rm = T),
    sd_log_real_wkly_wage = sd(log_real_wkly_wage, na.rm = T),
    log_real_hrly_wage = mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage = mean(log_real_wkly_wage, na.rm = T),
    job_sat = mean(job_sat, na.rm = T),
    hours_week = mean(hours_week, na.rm = T),
    part_time = mean(part_time, na.rm = T),
    tenure = mean(tenure, na.rm = T),
    health = mean(health, na.rm = T),
    retirement = mean(retirement, na.rm = T),
    any_benefits = mean(any_benefits, na.rm = T),
    n_black = sum(black * weight),
    n_hispanic = sum(hispanic * weight),
    n_union = sum(union * weight, na.rm = T),
    n_union_defined = sum((!is.na(union) %in% T) * weight),
    tot_age = sum(age * weight),
    tot_age_2 = sum((age ^ 2) * weight)
  ) |>
  pivot_wider(names_from = c(outsourced, self_emp, 
                             indep_con, temp_work, on_call),
              values_from = n:tot_age_2) |>
  ungroup() |> 
  # Rename pivot table so easier to reference 
  rename_at(vars(ends_with("_0_0_0_0_0")), 
            ~ str_replace(., "_0_0_0_0_0", "_0")) |> 
  rename_at(vars(ends_with("_1_0_0_0_0")), 
            ~ str_replace(., "_1_0_0_0_0", "_1")) |> 
  rename_at(vars(ends_with("_0_1_0_0_0")), 
            ~ str_replace(., "_0_1_0_0_0", "_2")) |> 
  rename_at(vars(ends_with("_0_0_1_0_0")), 
            ~ str_replace(., "_0_0_1_0_0", "_3")) |> 
  rename_at(vars(ends_with("_0_0_0_1_0")), 
            ~ str_replace(., "_0_0_0_1_0", "_4")) |> 
  rename_at(vars(ends_with("_0_0_0_0_1")), 
            ~ str_replace(., "_0_0_0_0_1", "_5")) |> 
  mutate(
    workers = r_sum(n_0, n_1, n_2, n_3, n_4, n_5),
    outsourced_per = ifelse(!is.na(n_1), n_1 / workers * 100, 0),
    self_emp_per = ifelse(!is.na(n_2), n_2 / workers * 100, 0),
    indep_con_per = ifelse(!is.na(n_3), n_3 / workers * 100, 0),
    temp_work_per = ifelse(!is.na(n_4), n_4 / workers * 100, 0),
    on_call_per = ifelse(!is.na(n_5), n_5 / workers * 100, 0),
    black_per = (
      r_sum(n_black_0, n_black_1, n_black_2, n_black_3, 
            n_black_4, n_black_5)
      / workers * 100),
    hispanic_per = (
      r_sum(n_hispanic_0, n_hispanic_1, n_hispanic_2, 
            n_hispanic_3, n_hispanic_4, n_hispanic_5) 
      / workers * 100),
    union_per = (r_sum(n_union_0, n_union_1, n_union_2, 
                       n_union_3, n_union_4, n_union_5) / 
                   r_sum(n_union_defined_0, n_union_defined_1,
                         n_union_defined_2, n_union_defined_3, 
                         n_union_defined_4, n_union_defined_5)
                 * 100),
    age = (r_sum(tot_age_0, tot_age_1, tot_age_2, tot_age_3, 
                 tot_age_4, tot_age_5) 
           / workers),
    age_2 = (r_sum(tot_age_2_0, tot_age_2_1, tot_age_2_2, 
                   tot_age_2_3, tot_age_2_4, tot_age_2_5) 
             / workers)
  ) |> 
  group_by(year) |> 
  mutate(workers_per = workers / sum(workers) * 100) |> 
  group_by(occ) |> 
  mutate(average_size = mean(workers_per)) |> 
  ungroup() |> 
  filter(!is.na(year))

# Recreate Regressions

# How do these correlations change with controls? Run regressions
controls <- c("outsourced_per", "self_emp_per", "indep_con_per",
              "temp_work_per", "on_call_per", "black_per", 
              "hispanic_per", "union_per", "age")

fixed_effects <- c("year", "occ")
fe <- create_formula("", fixed_effects)

outcomes <- c("log_real_hrly_wage", "log_real_wkly_wage",
              "hours_week", "part_time", "any_benefits",
              "health")

center <- list(center_r, center_r)

# Save two tables, one for effects on traditional aggregates
# and one on effects on outsourced aggregates
for (out in 0:1){
  out_1 <- out + 1
  for (i in seq_along(outcomes)){
    
    outcome <- str_c(outcomes[i], "_", out)
    eq <- create_formula(outcome, controls)
    
    temp <- lm_robust(eq, data = occ_timeline_y, 
                      fixed_effects = !!fe,
                      clusters = occ, se_type = "stata", 
                      try_cholesky = TRUE)
    
    center[[out_1]] <- cbind(
      center[[out_1]],
      rbind(format_val(temp$coefficients["outsourced_per"], 
                       p_stars(temp$p.value["outsourced_per"]), 
                       r = 4, s = 4),
            format_se(temp$std.error["outsourced_per"], r = 4, s = 4),
            format_val(temp$r.squared), format_n(lm_N(temp)))
    )
  }
}

# Combine the two center tables into 1
center_t <- rbind(
  c("\\textbf{Traditional}", " & ",  " & ", " & ", " & ", " & ", " & "),
  center[[1]],
  c("\\textbf{Outsourced}", " & ", " & ", " & ", " & ", " & ", " & "),
  center[[2]]
)

center_t <- center_t |>
  add_endline(midrule = c(5)) |>
  center_to_latex()

name <- "Effects of Outsourcing Level within Occupation on Workers"
label <- "occ_reg_year"
note <- "
Occupation level regressions of percent outsourced 
within occupation each year on average job characteristics.
Sample consists of men in the NLSY, aggregated at the occupation
level. Each regression contains controls for percent 
in other alternative job types (ie. independent contractor, 
temp workers), percent Black, Hispanic, and union member, 
average age, and occupation and week fixed effects.
Regressions use robust standard errors clustered at the 
occupation level. Stars represent significant difference 
from 0 at the .10 level *, .05 level **, and .01 level ***."

# Save only for myself
header <- make_header("", name, label, colsep = 0.25)
bot <- make_bot(note)

write.table(
  str_c(header, top, center_t, bot, "\n \\end{document}"),
  str_c(table_folder,
        "NLSY79 Occupation Info/Occupation Regressions Year.tex"),
  quote=F, col.names=F, row.names=F, sep="")


# Occupation Characteristics vs Outsourcing: Robustness -------------------

# Use timeline_robust and run regressions again. This 
# file matches to matched instead of matched_jobs, so variables are allowed to
# differ by interview within jobs. Also make a monthly dataset to
# do robustness with CPS
timeline_r <- read_csv(str_c(clean_folder, 
                             "matched_timeline_robust.csv"),
                     col_types = cols(
                       .default = col_double(),
                       week = col_date(format = ""),
                       week_start_job = col_date(format = ""),
                       week_end_job = col_date(format = "")
                     )) |> 
  data.table()

timeline_r[
  year(week) > 2000, `:=`(month = floor_date(week, unit = "month"),
                          year = floor_date(week, unit = "year"))]

# Unweighted
occ_timeline_r <- timeline_r |>
  # filter(year(week) <= 2012) |> 
  filter(!is.na(occ), !is.na(outsourced)) |>
  group_by(week, occ, outsourced, self_emp, 
           indep_con, temp_work, on_call) |>
  summarise(
    n = sum(weight),
    sd_log_real_hrly_wage = sd(log_real_hrly_wage, na.rm = T),
    sd_log_real_wkly_wage = sd(log_real_wkly_wage, na.rm = T),
    log_real_hrly_wage = mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage = mean(log_real_wkly_wage, na.rm = T),
    job_sat = mean(job_sat, na.rm = T),
    hours_week = mean(hours_week, na.rm = T),
    part_time = mean(part_time, na.rm = T),
    tenure = mean(tenure, na.rm = T),
    health = mean(health, na.rm = T),
    retirement = mean(retirement, na.rm = T),
    any_benefits = mean(any_benefits, na.rm = T),
    n_black = sum(black * weight),
    n_hispanic = sum(hispanic * weight),
    n_union = sum(union * weight, na.rm = T),
    n_union_defined = sum((!is.na(union) %in% T) * weight),
    tot_age = sum(age * weight),
    tot_age_2 = sum((age ^ 2) * weight)
  ) |>
  pivot_wider(names_from = c(outsourced, self_emp, 
                             indep_con, temp_work, on_call),
              values_from = n:tot_age_2) |>
  ungroup() |> 
  # Rename pivot table so easier to reference 
  rename_at(vars(ends_with("_0_0_0_0_0")), 
            ~ str_replace(., "_0_0_0_0_0", "_0")) |> 
  rename_at(vars(ends_with("_1_0_0_0_0")), 
            ~ str_replace(., "_1_0_0_0_0", "_1")) |> 
  rename_at(vars(ends_with("_0_1_0_0_0")), 
            ~ str_replace(., "_0_1_0_0_0", "_2")) |> 
  rename_at(vars(ends_with("_0_0_1_0_0")), 
            ~ str_replace(., "_0_0_1_0_0", "_3")) |> 
  rename_at(vars(ends_with("_0_0_0_1_0")), 
            ~ str_replace(., "_0_0_0_1_0", "_4")) |> 
  rename_at(vars(ends_with("_0_0_0_0_1")), 
            ~ str_replace(., "_0_0_0_0_1", "_5")) |> 
  mutate(
    workers = r_sum(n_0, n_1, n_2, n_3, n_4, n_5),
    outsourced_per = ifelse(!is.na(n_1), n_1 / workers * 100, 0),
    self_emp_per = ifelse(!is.na(n_2), n_2 / workers * 100, 0),
    indep_con_per = ifelse(!is.na(n_3), n_3 / workers * 100, 0),
    temp_work_per = ifelse(!is.na(n_4), n_4 / workers * 100, 0),
    on_call_per = ifelse(!is.na(n_5), n_5 / workers * 100, 0),
    black_per = (
      r_sum(n_black_0, n_black_1, n_black_2, n_black_3, 
            n_black_4, n_black_5)
      / workers * 100),
    hispanic_per = (
      r_sum(n_hispanic_0, n_hispanic_1, n_hispanic_2, 
            n_hispanic_3, n_hispanic_4, n_hispanic_5) 
      / workers * 100),
    union_per = (r_sum(n_union_0, n_union_1, n_union_2,
                       n_union_3, n_union_4, n_union_5) / 
                   r_sum(n_union_defined_0, n_union_defined_1, 
                         n_union_defined_2, n_union_defined_3,
                         n_union_defined_4, n_union_defined_5)
                 * 100),
    age = (r_sum(tot_age_0, tot_age_1, tot_age_2, 
                 tot_age_3, tot_age_4, tot_age_5) 
           / workers),
    age_2 = (r_sum(tot_age_2_0, tot_age_2_1, tot_age_2_2, 
                   tot_age_2_3, tot_age_2_4, tot_age_2_5) 
             / workers)
  ) |> 
  group_by(week) |> 
  mutate(workers_per = workers / sum(workers) * 100) |> 
  group_by(occ) |> 
  mutate(average_size = mean(workers_per)) |> 
  ungroup()

# Recreate Regressions

# How do these correlations change with controls? Run regressions
controls <- c("outsourced_per", "self_emp_per", "indep_con_per",
              "temp_work_per", "on_call_per", "black_per", 
              "hispanic_per", "union_per", "age")

fixed_effects <- c("week", "occ")
fe <- create_formula("", fixed_effects)

outcomes <- c("log_real_hrly_wage", "log_real_wkly_wage",
              "hours_week", "part_time", "any_benefits",
              "health")

center <- list(center_r, center_r)

# Save two tables, one for effects on traditional aggregates
# and one on effects on outsourced aggregates
for (out in 0:1){
  out_1 <- out + 1
  for (i in seq_along(outcomes)){
    
    outcome <- str_c(outcomes[i], "_", out)
    eq <- create_formula(outcome, controls)
    
    temp <- lm_robust(eq, data = occ_timeline_r, 
                      fixed_effects = !!fe,
                      clusters = occ, se_type = "stata", 
                      try_cholesky = TRUE)
    
    center[[out_1]] <- cbind(
      center[[out_1]],
      rbind(format_val(temp$coefficients["outsourced_per"], 
                       p_stars(temp$p.value["outsourced_per"]), 
                       r = 4, s = 4),
            format_se(temp$std.error["outsourced_per"], r = 4, s = 4),
            format_val(temp$r.squared), format_n(lm_N(temp)))
    )
  }
}

# Combine the two center tables into 1
center_t <- rbind(
  c("\\textbf{Traditional}", " & ",  " & ", " & ", " & ", " & ", " & "),
  center[[1]],
  c("\\textbf{Outsourced}", " & ", " & ", " & ", " & ", " & ", " & "),
  center[[2]]
)

center_t <- center_t |>
  add_endline(midrule = c(5)) |>
  center_to_latex()

name <- "Effects of Outsourcing Level within Occupation on Workers"
label <- "occ_reg_robust"
note <- "
Occupation level regressions of percent outsourced 
within occupation each week on average job characteristics.
Sample consists of men in the NLSY, aggregated at the occupation
level. Each regression contains controls for percent 
in other alternative job types (ie. independent contractor, 
temp workers), percent Black, Hispanic, and union member, 
average age, and occupation and week fixed effects.
Regressions use robust standard errors clustered at the 
occupation level. Stars represent significant difference 
from 0 at the .10 level *, .05 level **, and .01 level ***."

# Save only for myself
header <- make_header("", name, label, colsep = 0.25)
bot <- make_bot(note)

write.table(
  str_c(header, top, center_t, bot, "\n \\end{document}"),
  str_c(
    table_folder,
    "NLSY79 Occupation Info/Occupation Regressions Week Robust.tex"),
  quote=F, col.names=F, row.names=F, sep="")


# Also create a small table saving average workers_per of 
# each occupation for context
table <- str_c("\\begin{tabular}{lS}
\\toprule
Measure & {Workers Percent}  \\\\  \\midrule
Monthly", format_val(mean(occ_timeline$workers_per)), "\\\\
Yearly", format_val(mean(occ_timeline_y$workers_per)), "\\\\
Monthly Robust", format_val(mean(occ_timeline_r$workers_per)), 
"\\\\")

name <- "Average Outsourcing Percent of Occupations"
label <- "workers_per"
note <- "Help put regressions in context."

# Save only for myself
header <- make_header("", name, label)
bot <- make_bot(note)

write.table(str_c(header, table, bot, "\n \\end{document}"),
            str_c(table_folder,
                  "NLSY79 Occupation Info/Occupation Workers Per.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# PBS Occupation Characteristics vs Outsourcing (CPS Comparison) ---------------------------

# At monthly level, does percent of occupation in PBS industries
# effect workers? Save this dataset to compare with CPS
# Unweighted
pbs_timeline_m <- timeline |>
  # filter(year(week) <= 2012) |> 
  filter(!is.na(occ), !is.na(pbs)) |>
  group_by(month, occ, pbs) |>
  summarise(
    n = sum(weight),
    sd_log_real_hrly_wage = sd(log_real_hrly_wage, na.rm = T),
    sd_log_real_wkly_wage = sd(log_real_wkly_wage, na.rm = T),
    log_real_hrly_wage = mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage = mean(log_real_wkly_wage, na.rm = T),
    job_sat = mean(job_sat, na.rm = T),
    hours_week = mean(hours_week, na.rm = T),
    part_time = mean(part_time, na.rm = T),
    tenure = mean(tenure, na.rm = T),
    health = mean(health, na.rm = T),
    retirement = mean(retirement, na.rm = T),
    any_benefits = mean(any_benefits, na.rm = T),
    n_black = sum(black * weight),
    n_hispanic = sum(hispanic * weight),
    n_union = sum(union * weight, na.rm = T),
    n_union_defined = sum((!is.na(union) %in% T) * weight),
    tot_age = sum(age * weight),
    tot_age_2 = sum((age ^ 2) * weight)
  ) |>
  pivot_wider(names_from = c(pbs),
              values_from = n:tot_age_2) |>
  ungroup() |> 
  mutate(
    workers = r_sum(n_0, n_1),
    pbs_per = ifelse(!is.na(n_1), n_1 / workers * 100, 0),
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
  group_by(month) |> 
  mutate(workers_per = workers / sum(workers) * 100) |> 
  group_by(occ) |> 
  mutate(average_size = mean(workers_per)) |> 
  ungroup() |> 
  filter(!is.na(month))

# Save pbs_timeline_m in cleaned_data to compare with cps data
write_csv(pbs_timeline_m, str_c(clean_folder, "pbs_timeline_m.csv"))

# Recreate Regressions

# How do these correlations change with controls? Run regressions
controls <- c("pbs_per", "black_per", "hispanic_per",
              "union_per", "age")

fixed_effects <- c("month", "occ")
fe <- create_formula("", fixed_effects)

outcomes <- c("log_real_hrly_wage", "log_real_wkly_wage",
              "hours_week", "part_time", "any_benefits",
              "health")

center_pbs <- c("PBS Percent", "", "$R^2$", "Observations")

center <- list(center_pbs, center_pbs)
pbs_names <- c("Non-PBS", "PBS")
pbs_labels <- c("non_pbs", "pbs")

# Save two tables, one for effects on traditional aggregates
# and one on effects on outsourced aggregates
for (out in 0:1){
  out_1 <- out + 1
  for (i in seq_along(outcomes)){
    
    outcome <- str_c(outcomes[i], "_", out)
    eq <- create_formula(outcome, controls)
    
    temp <- lm_robust(eq, data = pbs_timeline_m, 
                      fixed_effects = !!fe,
                      clusters = occ, se_type = "stata", 
                      try_cholesky = TRUE)
    
    center[[out_1]] <- cbind(
      center[[out_1]],
      rbind(format_val(temp$coefficients["pbs_per"], 
                       p_stars(temp$p.value["pbs_per"]), 
                       r = 4, s = 4),
            format_se(temp$std.error["pbs_per"], r = 4, s = 4),
            format_val(temp$r.squared), format_n(lm_N(temp)))
    )
  }
}

# Combine the two center tables into 1
center_t <- rbind(
  c("\\textbf{Non-PBS}", " & ",  " & ", " & ", " & ", " & ", " & "),
  center[[1]],
  c("\\textbf{PBS}", " & ", " & ", " & ", " & ", " & ", " & "),
  center[[2]]
)

center_t <- center_t |>
  add_endline(midrule = c(5)) |>
  center_to_latex()

name <- "Effects of PBS Level within Occupation on Workers"
label <- "occ_pbs"
note <- "
Occupation level regressions of in percent in
Professional Business Service (PBS) industries 
within an occupation each month on average job characteristics.
PBS Industries have Census 2000 Industry Codes between 
7270 and 7790. Sample consists of men the NLSY, 
aggregated at the occupation level. 
Each regression contains controls for percent 
in other alternative job types (ie. independent contractor, 
temp workers), percent Black, Hispanic, and union member, 
average age, and occupation and week fixed effects.
Regressions use robust standard errors clustered at the 
occupation level. Stars represent significant difference 
from 0 at the .10 level *, .05 level **, and .01 level ***."

# Save only for myself
header <- make_header("", name, label, colsep = 0.25)
bot <- make_bot(note)

write.table(
  str_c(header, top, center_t, bot, "\n \\end{document}"),
  str_c(table_folder,
        "NLSY79 Occupation Info/Occupation PBS Regressions Month.tex"),
  quote=F, col.names=F, row.names=F, sep="")

# Occupation Characterstics at Monthly Level (CPS Comparison) -------------

# Create occ_timeline_m at monthly level to compare to CPS
occ_timeline_m <- timeline |>
  # filter(year(week) <= 2012) |> 
  filter(!is.na(occ), !is.na(outsourced)) |>
  group_by(month, occ, outsourced, self_emp, 
           indep_con, temp_work, on_call) |>
  summarise(
    ho_occ = mean(ho_occ),
    n = sum(weight),
    log_real_wkly_wage = mean(log_real_wkly_wage, na.rm = T),
    hours_week = mean(hours_week, na.rm = T),
    part_time = mean(part_time, na.rm = T),
    tenure = mean(tenure, na.rm = T),
    health = mean(health, na.rm = T),
    retirement = mean(retirement, na.rm = T),
    any_benefits = mean(any_benefits, na.rm = T),
    n_black = sum(black * weight),
    n_hispanic = sum(hispanic * weight),
    n_union = sum(union * weight, na.rm = T),
    n_union_defined = sum((!is.na(union) %in% T) * weight),
    tot_age = sum(age * weight),
    tot_age_2 = sum((age ^ 2) * weight)
  ) |>
  pivot_wider(names_from = c(outsourced, 
                             self_emp, indep_con, temp_work, on_call),
              values_from = n:tot_age_2) |>
  ungroup() |> 
  # Rename pivot table so easier to reference 
  rename_at(vars(ends_with("_0_0_0_0_0")), 
            ~ str_replace(., "_0_0_0_0_0", "_0")) |> 
  rename_at(vars(ends_with("_1_0_0_0_0")), 
            ~ str_replace(., "_1_0_0_0_0", "_1")) |> 
  rename_at(vars(ends_with("_0_1_0_0_0")), 
            ~ str_replace(., "_0_1_0_0_0", "_2")) |> 
  rename_at(vars(ends_with("_0_0_1_0_0")), 
            ~ str_replace(., "_0_0_1_0_0", "_3")) |> 
  rename_at(vars(ends_with("_0_0_0_1_0")), 
            ~ str_replace(., "_0_0_0_1_0", "_4")) |> 
  rename_at(vars(ends_with("_0_0_0_0_1")), 
            ~ str_replace(., "_0_0_0_0_1", "_5")) |> 
  mutate(
    workers = r_sum(n_0, n_1, n_2, n_3, n_4, n_5),
    outsourced_per = ifelse(!is.na(n_1), n_1 / workers * 100, 0),
    self_emp_per = ifelse(!is.na(n_2), n_2 / workers * 100, 0),
    indep_con_per = ifelse(!is.na(n_3), n_3 / workers * 100, 0),
    temp_work_per = ifelse(!is.na(n_4), n_4 / workers * 100, 0),
    on_call_per = ifelse(!is.na(n_5), n_5 / workers * 100, 0),
    black_per = (
      r_sum(n_black_0, n_black_1, n_black_2, n_black_3, 
            n_black_4, n_black_5)
      / workers * 100),
    hispanic_per = (
      r_sum(n_hispanic_0, n_hispanic_1, n_hispanic_2, 
            n_hispanic_3, n_hispanic_4, n_hispanic_5) 
      / workers * 100),
    union_per = (r_sum(n_union_0, n_union_1, n_union_2,
                       n_union_3, n_union_4, n_union_5) / 
                   r_sum(n_union_defined_0, n_union_defined_1,
                         n_union_defined_2, n_union_defined_3, 
                         n_union_defined_4, n_union_defined_5)
                 * 100),
    age = (r_sum(tot_age_0, tot_age_1, tot_age_2, 
                 tot_age_3, tot_age_4, tot_age_5) 
           / workers),
    age_2 = (r_sum(tot_age_2_0, tot_age_2_1, tot_age_2_2, 
                   tot_age_2_3, tot_age_2_4, tot_age_2_5) 
             / workers)
  ) |> 
  group_by(month) |> 
  mutate(workers_per = workers / sum(workers) * 100) |> 
  group_by(occ) |> 
  mutate(average_size = mean(workers_per)) |> 
  ungroup() |> 
  filter(!is.na(month))

# Save occ_timeline_week in cleaned_data to compare with cps data
write_csv(occ_timeline_m, str_c(clean_folder, "occ_timeline_m.csv"))

# Do the same for robust (see above)
occ_timeline_r_m <- timeline_r |>
  # filter(year(week) <= 2012) |> 
  filter(!is.na(occ), !is.na(outsourced)) |>
  group_by(month, occ, outsourced, self_emp, 
           indep_con, temp_work, on_call) |>
  summarise(
    n = sum(weight),
    log_real_wkly_wage = mean(log_real_wkly_wage, na.rm = T),
    hours_week = mean(hours_week, na.rm = T),
    part_time = mean(part_time, na.rm = T),
    tenure = mean(tenure, na.rm = T),
    health = mean(health, na.rm = T),
    retirement = mean(retirement, na.rm = T),
    any_benefits = mean(any_benefits, na.rm = T),
    n_black = sum(black * weight),
    n_hispanic = sum(hispanic * weight),
    n_union = sum(union * weight, na.rm = T),
    n_union_defined = sum((!is.na(union) %in% T) * weight),
    tot_age = sum(age * weight),
    tot_age_2 = sum((age ^ 2) * weight)
  ) |>
  pivot_wider(names_from = c(outsourced, self_emp, 
                             indep_con, temp_work, on_call),
              values_from = n:tot_age_2) |>
  ungroup() |> 
  # Rename pivot table so easier to reference 
  rename_at(vars(ends_with("_0_0_0_0_0")), 
            ~ str_replace(., "_0_0_0_0_0", "_0")) |> 
  rename_at(vars(ends_with("_1_0_0_0_0")), 
            ~ str_replace(., "_1_0_0_0_0", "_1")) |> 
  rename_at(vars(ends_with("_0_1_0_0_0")), 
            ~ str_replace(., "_0_1_0_0_0", "_2")) |> 
  rename_at(vars(ends_with("_0_0_1_0_0")), 
            ~ str_replace(., "_0_0_1_0_0", "_3")) |> 
  rename_at(vars(ends_with("_0_0_0_1_0")), 
            ~ str_replace(., "_0_0_0_1_0", "_4")) |> 
  rename_at(vars(ends_with("_0_0_0_0_1")), 
            ~ str_replace(., "_0_0_0_0_1", "_5")) |> 
  mutate(
    
    workers = r_sum(n_0, n_1, n_2, n_3, n_4, n_5),
    outsourced_per = ifelse(!is.na(n_1), n_1 / workers * 100, 0),
    self_emp_per = ifelse(!is.na(n_2), n_2 / workers * 100, 0),
    indep_con_per = ifelse(!is.na(n_3), n_3 / workers * 100, 0),
    temp_work_per = ifelse(!is.na(n_4), n_4 / workers * 100, 0),
    on_call_per = ifelse(!is.na(n_5), n_5 / workers * 100, 0),
    black_per = (
      r_sum(n_black_0, n_black_1, n_black_2, 
            n_black_3, n_black_4, n_black_5)
      / workers * 100),
    hispanic_per = (
      r_sum(n_hispanic_0, n_hispanic_1, n_hispanic_2, 
            n_hispanic_3, n_hispanic_4, n_hispanic_5) 
      / workers * 100),
    union_per = (r_sum(n_union_0, n_union_1, n_union_2,
                       n_union_3, n_union_4, n_union_5) / 
                   r_sum(n_union_defined_0, n_union_defined_1,
                         n_union_defined_2, n_union_defined_3,
                         n_union_defined_4, n_union_defined_5)
                 * 100),
    age = (r_sum(tot_age_0, tot_age_1, tot_age_2, 
                 tot_age_3, tot_age_4, tot_age_5) 
           / workers),
    age_2 = (r_sum(tot_age_2_0, tot_age_2_1, tot_age_2_2, 
                   tot_age_2_3, tot_age_2_4, tot_age_2_5) 
             / workers)
  ) |> 
  group_by(month) |> 
  mutate(workers_per = workers / sum(workers) * 100) |> 
  group_by(occ) |> 
  mutate(average_size = mean(workers_per)) |> 
  ungroup() |> 
  filter(!is.na(month))

# Save occ_timeline_r_m in cleaned_data to compare with cps data
write_csv(occ_timeline_r_m, 
          str_c(clean_folder, "occ_timeline_robust_m.csv"))

# Occupation Characterstics at Monthly Level Bartik Instruments -------------

# Create monthly Bartik instruments for share of workers outsourced
# within each occupation by dividing the US into the 4 regions
# (Northeast, Midwest, South, and West). 
# For instrument, need 
# 1. Leave-one-out occupation share of outsourcing each period
# 2. January 2001 (first month) share of occupation in region
occ_timeline_bartik <- timeline |>
  # filter(year(week) <= 2012) |> 
  filter(!is.na(occ), !is.na(outsourced)) |>
  group_by(region, month, occ, outsourced, self_emp, 
           indep_con, temp_work, on_call) |>
  summarise(
    ho_occ = mean(ho_occ),
    n = sum(weight),
    log_real_hrly_wage = mean(log_real_hrly_wage, na.rm = T),
    log_real_wkly_wage = mean(log_real_wkly_wage, na.rm = T),
    hours_week = mean(hours_week, na.rm = T),
    part_time = mean(part_time, na.rm = T),
    tenure = mean(tenure, na.rm = T),
    health = mean(health, na.rm = T),
    retirement = mean(retirement, na.rm = T),
    any_benefits = mean(any_benefits, na.rm = T),
    n_black = sum(black * weight),
    n_hispanic = sum(hispanic * weight),
    n_union = sum(union * weight, na.rm = T),
    n_union_defined = sum((!is.na(union) %in% T) * weight),
    tot_age = sum(age * weight),
    tot_age_2 = sum((age ^ 2) * weight)
  ) |>
  pivot_wider(names_from = c(outsourced, self_emp, 
                             indep_con, temp_work, on_call),
              values_from = n:tot_age_2) |>
  ungroup() |> 
  # Rename pivot table so easier to reference 
  rename_at(vars(ends_with("_0_0_0_0_0")), 
            ~ str_replace(., "_0_0_0_0_0", "_0")) |> 
  rename_at(vars(ends_with("_1_0_0_0_0")), 
            ~ str_replace(., "_1_0_0_0_0", "_1")) |> 
  rename_at(vars(ends_with("_0_1_0_0_0")), 
            ~ str_replace(., "_0_1_0_0_0", "_2")) |> 
  rename_at(vars(ends_with("_0_0_1_0_0")), 
            ~ str_replace(., "_0_0_1_0_0", "_3")) |> 
  rename_at(vars(ends_with("_0_0_0_1_0")), 
            ~ str_replace(., "_0_0_0_1_0", "_4")) |> 
  rename_at(vars(ends_with("_0_0_0_0_1")), 
            ~ str_replace(., "_0_0_0_0_1", "_5")) |> 
  mutate(
    # Set NA's in n_0-5 as 0 for ease below
    n_0 = ifelse(!is.na(n_0), n_0, 0),
    n_1 = ifelse(!is.na(n_1), n_1, 0),
    n_2 = ifelse(!is.na(n_2), n_2, 0),
    n_3 = ifelse(!is.na(n_3), n_3, 0),
    n_4 = ifelse(!is.na(n_4), n_4, 0),
    n_5 = ifelse(!is.na(n_5), n_5, 0),
    workers = r_sum(n_0, n_1, n_2, n_3, n_4, n_5),
    black_per = (
      r_sum(n_black_0, n_black_1, n_black_2, 
            n_black_3, n_black_4, n_black_5)
      / workers * 100),
    hispanic_per = (
      r_sum(n_hispanic_0, n_hispanic_1, n_hispanic_2,
            n_hispanic_3, n_hispanic_4, n_hispanic_5) 
      / workers * 100),
    union_per = (r_sum(n_union_0, n_union_1, n_union_2, 
                       n_union_3, n_union_4, n_union_5) / 
                   r_sum(n_union_defined_0, n_union_defined_1,
                         n_union_defined_2, n_union_defined_3, 
                         n_union_defined_4, n_union_defined_5)
                 * 100),
    age = (r_sum(tot_age_0, tot_age_1, tot_age_2,
                 tot_age_3, tot_age_4, tot_age_5) 
           / workers),
    age_2 = (r_sum(tot_age_2_0, tot_age_2_1, tot_age_2_2,
                   tot_age_2_3, tot_age_2_4, tot_age_2_5) 
             / workers)
  ) |> 
  filter(!is.na(month)) |> 
  group_by(month) |> 
  mutate(workers_per = workers / sum(workers) * 100) |> 
  group_by(occ) |> 
  mutate(average_size = mean(workers_per)) |> 
  # These are used to construct bartik_x and bartik_iv
  group_by(month, occ) |> 
  mutate(
    occ_share_national = workers / sum(workers),
    # Find worker type shares for whole US by summing over regions
    outsourced_per_national = sum(n_1) / sum(workers) * 100,
    self_emp_per_national = sum(n_2) / sum(workers) * 100,
    indep_con_per_national = sum(n_3) / sum(workers) * 100,
    temp_work_per_national = sum(n_4) / sum(workers) * 100,
    on_call_per_national = sum(n_5) / sum(workers) * 100,
    # Find leave one out by subtracting own contribution
    outsourced_per_loo = 
      ifelse(sum(workers) - workers > 0 & workers > 0,
             (sum(n_1) - n_1) / (sum(workers) - workers) * 100, NA),
    self_emp_per_loo = 
      ifelse(sum(workers) - workers > 0 & workers > 0,
             (sum(n_2) - n_2) / (sum(workers) - workers) * 100, NA),
    indep_con_per_loo = 
      ifelse(sum(workers) - workers > 0 & workers > 0,
             (sum(n_3) - n_3) / (sum(workers) - workers) * 100, NA),
    temp_work_per_loo = 
      ifelse(sum(workers) - workers > 0 & workers > 0,
             (sum(n_4) - n_4) / (sum(workers) - workers) * 100, NA),
    on_call_per_loo = 
      ifelse(sum(workers) - workers > 0 & workers > 0,
             (sum(n_5) - n_5) / (sum(workers) - workers) * 100, NA)
    ) |> 
  # Find each region, occ share in January 2001
  group_by(region, occ) |> 
  mutate(
    occ_share_national_init = 
      ifelse(month == ymd("2001-01-01"), occ_share_national, 0),
    occ_share_national_init = max(occ_share_national_init),
    # Create _delta variables with var - lag(var)
    workers_per_delta = workers_per - lag(workers_per),
    log_real_hrly_wage_0_delta = 
      log_real_hrly_wage_0 - lag(log_real_hrly_wage_0),
    log_real_wkly_wage_0_delta = 
      log_real_wkly_wage_0 - lag(log_real_wkly_wage_0),
    hours_week_0_delta = hours_week_0 - lag(hours_week_0),
    part_time_0_delta = part_time_0 - lag(part_time_0),
    tenure_0_delta = tenure_0 - lag(tenure_0),
    health_0_delta = health_0 - lag(health_0),
    retirement_0_delta = retirement_0 - lag(retirement_0),
    any_benefits_0_delta = any_benefits_0 - lag(any_benefits_0),
    log_real_hrly_wage_1_delta = 
      log_real_hrly_wage_1 - lag(log_real_hrly_wage_1),
    log_real_wkly_wage_1_delta = 
      log_real_wkly_wage_1 - lag(log_real_wkly_wage_1),
    hours_week_1_delta = hours_week_1 - lag(hours_week_1),
    part_time_1_delta = part_time_1 - lag(part_time_1),
    tenure_1_delta = tenure_1 - lag(tenure_1),
    health_1_delta = health_1 - lag(health_1),
    retirement_1_delta = retirement_1 - lag(retirement_1),
    any_benefits_1_delta = any_benefits_1 - lag(any_benefits_1)
    ) |> 
  ungroup() |> 
  # Create variables and bartik instruments
  mutate(
    outsourced_per = occ_share_national * outsourced_per_national,
    self_emp_per = occ_share_national * self_emp_per_national,
    indep_con_per = occ_share_national * indep_con_per_national,
    temp_work_per = occ_share_national * temp_work_per_national,
    on_call_per = occ_share_national * on_call_per_national,
    outsourced_per_bartik = 
      occ_share_national_init * outsourced_per_loo,
    self_emp_per_bartik = 
      occ_share_national_init * self_emp_per_loo,
    indep_con_per_bartik = 
      occ_share_national_init * indep_con_per_loo,
    temp_work_per_bartik = 
      occ_share_national_init * temp_work_per_loo,
    on_call_per_bartik = 
      occ_share_national_init * on_call_per_loo
  )
  
# Save occ_timeline_week in cleaned_data to compare with cps data
write_csv(occ_timeline_bartik, 
          str_c(clean_folder, "occ_timeline_bartik.csv"))

# Create a function that makes an iv formula given dependent variable 
# and list of independent variables and instruments
create_iv <- function(y, x_list, z_list) {
  vars <- x_list |> 
    map(str_c, collapse = "+") |> 
    str_c(collapse = "+")
  
  instruments <- z_list |> 
    map(str_c, collapse = "+") |> 
    str_c(collapse = "+")
  
  eq <- formula(str_c(y, "~", vars, "|", instruments))
}

# Run regressions with OLS and IV
ex_controls <- c("black_per", "hispanic_per", "union_per", "age",
                 "factor(region)", "factor(month)")
# , "factor(occ)")

job_types <- c("outsourced_per", "self_emp_per", "indep_con_per",
               "temp_work_per", "on_call_per")
job_types_iv <- str_c(job_types, "bartik", sep = "_")

controls <- c(job_types, ex_controls)
ivs <- c(job_types_iv, ex_controls)

fixed_effects <- c("month", "occ", "region")
fe <- create_formula("", fixed_effects)

# Drop part-time from outcomes but also look at employment
# share
outcomes <- c("log_real_hrly_wage", "log_real_wkly_wage",
              "hours_week", "any_benefits", "health")

top <- "\\begin{tabular}{lSSSSSS}
\\toprule
 & {Employment} & {Log Real} & {Log Real} & {Hours Worked} & {} & {Any} & {Health} \\\\
 & {Share} & {Hourly Wage} & {Weekly Wage} & {per Week} & {Part-Time} & {Benefits} & {Insurance} \\\\  \\midrule
"

# Create a table with regression results
center_iv <- c("OLS Outsourced", "", "OLS $R^2$",
               "IV Outsourced", "", "IV $R^2$", 
               "First-Stage F-stat", "Observations"
               )

center <- list(center_iv, center_iv)
iv_names <- c("Traditional", "Outsourced")
iv_labels <- c("trad", "out")

for (out in 0:1){
  out_1 <- out + 1
  # Include worker share in both tables
  eq <- create_formula("workers_per_delta", controls)
  eq_iv <- create_iv("workers_per_delta", controls, ivs)
  
  temp <- lm_robust(eq, data = occ_timeline_bartik,
                    clusters = occ, se_type = "stata",
                    try_cholesky = TRUE)
  
  temp_iv <- iv_robust(eq_iv, data = occ_timeline_bartik,
                       clusters = occ, se_type = "stata",
                       try_cholesky = TRUE,
                       diagnostics = TRUE)
  
  center[[out_1]] <- cbind(
    center[[out_1]],
    rbind(format_val(temp$coefficients["outsourced_per"], 
                     p_stars(temp$p.value["outsourced_per"]),
                     r = 5, s = 5),
          format_se(temp$std.error["outsourced_per"], r = 5, s = 5),
          format_val(temp$r.squared),
          format_val(temp_iv$coefficients["outsourced_per"], 
                     p_stars(temp_iv$p.value["outsourced_per"]),
                     r = 5, s = 5),
          format_se(temp_iv$std.error["outsourced_per"], r = 5, s = 5),
          format_val(temp_iv$r.squared),
          format_val(
            temp_iv$diagnostic_first_stage_fstatistic[
              "outsourced_per:value"]),
          format_n(lm_N(temp))
    )
  )
  
  for (i in seq_along(outcomes)){
    
    outcome <- str_c(outcomes[i], out, "delta", sep = "_")
    eq <- create_formula(outcome, controls)
    eq_iv <- create_iv(outcome, controls, ivs)
    
    temp <- lm_robust(eq, data = occ_timeline_bartik,
                      clusters = occ, se_type = "stata",
                      try_cholesky = TRUE)
    
    temp_iv <- iv_robust(eq_iv, data = occ_timeline_bartik,
                         clusters = occ, se_type = "stata",
                         try_cholesky = TRUE,
                         diagnostics = TRUE)
    
    center[[out_1]] <- cbind(
      center[[out_1]],
      rbind(format_val(temp$coefficients["outsourced_per"], 
                       p_stars(temp$p.value["outsourced_per"]), 
                       r = 5, s = 5),
            format_se(temp$std.error["outsourced_per"]),
            format_val(temp$r.squared),
            format_val(temp_iv$coefficients["outsourced_per"], 
                       p_stars(temp_iv$p.value["outsourced_per"]), 
                       r = 5, s = 5),
            format_se(temp_iv$std.error["outsourced_per"]), 
            format_val(temp_iv$r.squared),
            format_val(
              temp_iv$diagnostic_first_stage_fstatistic[
                "outsourced_per:value"]),
            format_n(lm_N(temp))
            )
      )
  }
}

# Combine the two center tables into 1
center_t <- rbind(
  c("\\textbf{Traditional}", " & ",  " & ", " & ", " & ", " & ", " & "),
  center[[1]],
  c("\\textbf{Outsourced}", " & ", " & ", " & ", " & ", " & ", " & "),
  center[[2]]
)

center_t <- center_t |>
  add_endline(midrule = c(9)) |>
  center_to_latex()

name <- "Effects of Outsourcing Level within Occupation on Workers"
label <- "occ_iv_reg"
note <- "
Occupation level regressions of percent outsourced 
within occupation each month on average job characteristics.
Sample consists of men in the NLSY, aggregated at the occupation
level. OLS uses percent outsourced in region, IV uses the Bartik
instrument of percent outsourced in every other region times 
this regions occupation share in January 2001. Regions are 
Northeast, Midwest, South, and West. 
Each regression contains controls for percent 
in other alternative job types (ie. independent contractor, 
temp workers), percent Black, Hispanic, and union member, 
average age, and occupation and week fixed effects.
Regressions use robust standard errors clustered at the 
occupation level. Stars represent significant difference 
from 0 at the .10 level *, .05 level **, and .01 level ***."

bot <- make_bot(note)

write.table(
  str_c(header, top, center_t, bot, "\n \\end{document}"),
  str_c(table_folder,
        "NLSY79 Occupation Info/Occupation IV Regressions.tex"),
  quote=F, col.names=F, row.names=F, sep="")

# Do the same for robust (see above)
occ_timeline_r_bartik <- timeline_r |>
  # filter(year(week) <= 2012) |> 
  filter(!is.na(occ), !is.na(outsourced)) |>
  group_by(region, month, occ, outsourced, self_emp, 
           indep_con, temp_work, on_call) |>
  summarise(
    n = sum(weight),
    log_real_wkly_wage = mean(log_real_wkly_wage, na.rm = T),
    hours_week = mean(hours_week, na.rm = T),
    part_time = mean(part_time, na.rm = T),
    tenure = mean(tenure, na.rm = T),
    health = mean(health, na.rm = T),
    retirement = mean(retirement, na.rm = T),
    any_benefits = mean(any_benefits, na.rm = T),
    n_black = sum(black * weight),
    n_hispanic = sum(hispanic * weight),
    n_union = sum(union * weight, na.rm = T),
    n_union_defined = sum((!is.na(union) %in% T) * weight),
    tot_age = sum(age * weight),
    tot_age_2 = sum((age ^ 2) * weight)
  ) |>
  pivot_wider(names_from = c(outsourced, self_emp, 
                             indep_con, temp_work, on_call),
              values_from = n:tot_age_2) |>
  ungroup() |> 
  # Rename pivot table so easier to reference 
  rename_at(vars(ends_with("_0_0_0_0_0")), 
            ~ str_replace(., "_0_0_0_0_0", "_0")) |> 
  rename_at(vars(ends_with("_1_0_0_0_0")), 
            ~ str_replace(., "_1_0_0_0_0", "_1")) |> 
  rename_at(vars(ends_with("_0_1_0_0_0")), 
            ~ str_replace(., "_0_1_0_0_0", "_2")) |> 
  rename_at(vars(ends_with("_0_0_1_0_0")), 
            ~ str_replace(., "_0_0_1_0_0", "_3")) |> 
  rename_at(vars(ends_with("_0_0_0_1_0")), 
            ~ str_replace(., "_0_0_0_1_0", "_4")) |> 
  rename_at(vars(ends_with("_0_0_0_0_1")), 
            ~ str_replace(., "_0_0_0_0_1", "_5")) |> 
  mutate(
    # Set NA's in n_0-5 as 0 for ease below
    n_0 = ifelse(!is.na(n_0), n_0, 0),
    n_1 = ifelse(!is.na(n_1), n_1, 0),
    n_2 = ifelse(!is.na(n_2), n_2, 0),
    n_3 = ifelse(!is.na(n_3), n_3, 0),
    n_4 = ifelse(!is.na(n_4), n_4, 0),
    n_5 = ifelse(!is.na(n_5), n_5, 0),
    workers = r_sum(n_0, n_1, n_2, n_3, n_4, n_5),
    black_per = (
      r_sum(n_black_0, n_black_1, n_black_2,
            n_black_3, n_black_4, n_black_5)
      / workers * 100),
    hispanic_per = (
      r_sum(n_hispanic_0, n_hispanic_1, n_hispanic_2, 
            n_hispanic_3, n_hispanic_4, n_hispanic_5) 
      / workers * 100),
    union_per = (r_sum(n_union_0, n_union_1, n_union_2, 
                       n_union_3, n_union_4, n_union_5) / 
                   r_sum(n_union_defined_0, n_union_defined_1, 
                         n_union_defined_2, n_union_defined_3, 
                         n_union_defined_4, n_union_defined_5)
                 * 100),
    age = (r_sum(tot_age_0, tot_age_1, tot_age_2, 
                 tot_age_3, tot_age_4, tot_age_5) 
           / workers),
    age_2 = (r_sum(tot_age_2_0, tot_age_2_1, tot_age_2_2,
                   tot_age_2_3, tot_age_2_4, tot_age_2_5) 
             / workers)
  ) |> 
  filter(!is.na(month)) |> 
  group_by(month) |> 
  mutate(workers_per = workers / sum(workers) * 100) |> 
  group_by(occ) |> 
  mutate(average_size = mean(workers_per)) |> 
  # These are used to construct bartik_x and bartik_iv
  group_by(month, occ) |> 
  mutate(
    occ_share_national = workers / sum(workers),
    # Find worker type shares for whole US by summing over regions
    outsourced_per_national = sum(n_1) / sum(workers) * 100,
    self_emp_per_national = sum(n_2) / sum(workers) * 100,
    indep_con_per_national = sum(n_3) / sum(workers) * 100,
    temp_work_per_national = sum(n_4) / sum(workers) * 100,
    on_call_per_national = sum(n_5) / sum(workers) * 100,
    # Find leave one out by subtracting own contribution
    outsourced_per_loo = 
      ifelse(sum(workers) - workers > 0,
             (sum(n_1) - n_1) / (sum(workers) - workers) * 100, 0),
    self_emp_per_loo = 
      ifelse(sum(workers) - workers > 0,
             (sum(n_2) - n_2) / (sum(workers) - workers) * 100, 0),
    indep_con_per_loo = 
      ifelse(sum(workers) - workers > 0,
             (sum(n_3) - n_3) / (sum(workers) - workers) * 100, 0),
    temp_work_per_loo = 
      ifelse(sum(workers) - workers > 0,
             (sum(n_4) - n_4) / (sum(workers) - workers) * 100, 0),
    on_call_per_loo = 
      ifelse(sum(workers) - workers > 0,
             (sum(n_5) - n_5) / (sum(workers) - workers) * 100, 0)
  ) |> 
  # Find each region, occ share in January 2001
  group_by(region, occ) |> 
  mutate(
    occ_share_national_init = 
      ifelse(month == ymd("2001-01-01"), occ_share_national, 0),
    occ_share_national_init = max(occ_share_national_init)
  ) |> 
  ungroup() |> 
  # Create variables and bartik instruments
  mutate(
    outsourced_per = occ_share_national * outsourced_per_national,
    self_emp_per = occ_share_national * self_emp_per_national,
    indep_con_per = occ_share_national * indep_con_per_national,
    temp_work_per = occ_share_national * temp_work_per_national,
    on_call_per = occ_share_national * on_call_per_national,
    outsourced_per_bartik = 
      occ_share_national_init * outsourced_per_loo,
    self_emp_per_bartik = 
      occ_share_national_init * self_emp_per_loo,
    indep_con_per_bartik = 
      occ_share_national_init * indep_con_per_loo,
    temp_work_per_bartik = 
      occ_share_national_init * temp_work_per_loo,
    on_call_per_bartik = 
      occ_share_national_init * on_call_per_loo
  )

# Save occ_timeline_r_m in cleaned_data to compare with cps data
write_csv(occ_timeline_r_bartik, 
          str_c(clean_folder, "occ_timeline_robust_bartik.csv"))

# Katz and Krueger --------------------------------------------------------

# From Katz and Krueger (2019), look at workers by job types (Table 2)
# (Less Important now that we do this with Men + Women)

# # How common is each job type? Take the average of all person-job-weeks
# type_prevalence <- timeline |> 
#   filter(!is.na(outsourced)) |> 
#   as_survey_design(ids = case_id, weights=weight) |>
#   summarise(
#     outsourced = survey_mean(outsourced * 100),
#     self_emp = survey_mean(self_emp * 100),
#     indep_con = survey_mean(indep_con * 100),
#     temp_work = survey_mean(temp_work * 100),
#     on_call = survey_mean(on_call * 100),
#     traditional = survey_mean(traditional * 100)
#     )
# 
# vars <- c("outsourced", "indep_con", "temp_work", "on_call", 
#           "self_emp", "traditional")
# 
# names <- c("Contracted Out", "Independent Contractor", "Temp Worker",
#            "On-Call Worker", "Self-Employed", "Traditional Employee" )
# 
# table <- "\\begin{tabular}{lSSS}
# \\toprule
# Job Type & {NLSY} & {CWS 2005} & {Katz and Krueger (2019)} \\\\ \\midrule
# "
# 
# # Import values from CWS and KK 
# # https://www.rsfjournal.org/content/rsfjss/5/5/132.full.pdf
# # Table 1
# cws <- c(format_it(1.4), format_it(7.0), format_it(0.9), format_it(1.7),
#          format_it(10.8), "{--}")
# kk <- c(format_it(2.5), format_it(7.2), format_it(1.7), format_it(2.4),
#         format_it(9.2), "{--}")
# 
# 
# for (i in seq_along(vars)) {
#   
#   se <- str_c(vars[i], "_se")
#   
#   table <- str_c(table,
#     names[i], format_val(type_prevalence[[vars[i]]]),
#     " & ", cws[i], " & ", kk[i], " \\\\ \n",
#     format_se(type_prevalence[[se]]), " & & \\\\[2pt] \n"
#   )
# }
# 
# bot <- "\\bottomrule
# \\end{tabular}
# }
# \\caption{Percent of weekly job-person observations in each job type for men in the NLSY.
# Observations weighted at the person level. Other values are from 
# \\citet{katz2019b} Table 1 using data from the Contingent Worker Survey (CWS) 2005 and 
# the RAND American Life Panel using alternative weight 2,
# which reweights to match the CPS in self-employment 
# and multiple job holders. Both of these samples include men and women age 18 and older.
# The NLSY separates self-employment as its own job type while both CWS and KK do not,
# so I could not determine how many workers are in traditional jobs for these sources.}
# \\label{per_job_types}
# \\end{table}"
# 
# write.table(str_c(table_top, siunitx, table, bot, "\n \\end{document}"),
#             str_c(table_folder, "NLSY79 Job Types/Job Type Percentages.tex"),
#             quote=F, col.names=F, row.names=F, sep="")
# 
# # Save to Drafts and Slides
# write.table(str_c(d_table_top, table, bot),
#             str_c(d_table_folder, "Job Type Percentages.tex"),
#             quote=F, col.names=F, row.names=F, sep="")
# 
# write.table(str_c(s_table_top, table, s_bot),
#             str_c(s_table_folder, "Job Type Percentages.tex"),
#             quote=F, col.names=F, row.names=F, sep="")

# Individual's Percent Outsourcing ----------------------------------------

# For workers ever outsourced, what percent of weeks are in outsourcing
# vs non-outsourcing jobs?

outsourcing_per <- timeline |> 
  filter(ever_out_oj == 1, !is.na(outsourced)) |> 
  group_by(case_id) |> 
  summarise(
    outsourcing_per = mean(outsourced) * 100,
    ever_ho_occ = mean(ever_ho_occ),
    weight = mean(weight)
  ) |> 
  ungroup() |> 
  mutate(weight = weight / sum(weight))

outsourcing_per_mean <- 
  weighted.mean(outsourcing_per$outsourcing_per, 
                outsourcing_per$weight)

temp <- outsourcing_per |> 
  ggplot() +
  geom_histogram(aes(outsourcing_per), bins = 40, 
                 alpha = 0.4, fill = "blue") +
  geom_vline(aes(xintercept = outsourcing_per_mean), size = 1) +
  geom_text(aes(x = outsourcing_per_mean + 1, y = 50, hjust = 0,
                label = str_c("Mean = ", 
                              round(outsourcing_per_mean, 2))),
            size = 6) +
  labs(x = "Percent of Weeks Worked Outsourced", y = "Density") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_light(base_size = 16) 
  
ggsave(str_c(figure_folder, "Percent Weeks Worked Outsourced.pdf"),
       height = height, width = width) 

# Log Weekly Wage Distribution by Year --------------------------------------------

# What is distribution of log wages each year for 
# outsourced vs traditional jobs
timeline_yearly <- timeline |> 
  filter(!is.na(outsourced), !is.na(year), 
         (outsourced == 1 | traditional == 1)) |>
  group_by(case_id, emp_id, year) |> 
  # Keep only last week observed each year
  filter(week == max(week))

ss_yearly <- timeline |> 
  filter(!is.na(log_real_wkly_wage)) |> 
  as_survey_design(id = case_id, weight = weight) |> 
  group_by(year, outsourced) |> 
  summarise(
    log_real_wkly_wage_mean = survey_mean(log_real_wkly_wage),
    log_reak_wkly_wage_sd = survey_sd(log_real_wkly_wage),
    obs = unweighted(n()),
    log_real_wkly_wage_skew = unweighted(Skew(log_real_wkly_wage))
  )

temp <- timeline_yearly |> 
  filter(!is.na(log_real_wkly_wage), year(year) <= 2016) |> 
  ggplot(aes(log_real_wkly_wage, fill = factor(outsourced))) +
  geom_density(alpha = 0.2) +
  labs(x = "Log Real Weekly Wages", y = "Density") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                    values = c("blue", "red"),
                    labels = c("Traditional", "Outsourced")) +
  theme_light(base_size = 16) +
  facet_wrap(~ year(year))

ggsave(str_c(figure_folder, "LRW Wage Distribution by Year.pdf"),
       height = height, width = width)

temp <- timeline_yearly |> 
  filter(!is.na(log_real_wkly_wage), year(year) <= 2016,
         ever_ho_occ == 1) |> 
  ggplot(aes(log_real_wkly_wage, fill = factor(outsourced))) +
  geom_density(alpha = 0.2) +
  labs(x = "Log Real Weekly Wages", y = "Density") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                    values = c("blue", "red"),
                    labels = c("Traditional", "Outsourced")) +
  theme_light(base_size = 16) +
  facet_wrap(~ year(year))

ggsave(str_c(figure_folder, 
             "LRW Wage Distribution by Year Ever HO.pdf"),
       height = height, width = width)
