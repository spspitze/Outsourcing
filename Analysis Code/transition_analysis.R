# This file takes data from matched_transition
# to compute summary statistics, run regressions, and make graphs

rm(list = ls())

# library(lpirfs)
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
figure_folder <- "../Figures/NLSY 79 Transition/"
d_table_folder <- "../Drafts/Draft Tables/"
s_table_folder <- "../Slides/Slide Tables/"

# For saving graphs
# aspect_ratio <- 1.62
aspect_ratio <- 1.77
height <- 6
width <- height * aspect_ratio

# Download the data
transition <- read.csv(str_c(clean_folder, "matched_transition.csv"))

# Some figures calculated are useful for calbirating the model.
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

# Note: this is an older version of the code. It's not as elegant
# but it is useful for transitions because it uses final figures
# instead of raw data, which is useful when comparing variables from different columns
# Create a function that finds difference of means or proportions and reports
# * if 10%, ** if 5%, and *** if 1% different
test <- function(data, var, obs, row_1, row_2, type) {
  if (type == "mean") {
    test <- tsum.test(mean.x=data[[var]][row_1],
                      s.x=data[[str_c(var, "_se")]][row_1] * sqrt(data[[obs]][row_1]),
                      n.x=data[[obs]][row_1],
                      mean.y=data[[var]][row_2],
                      s.y=data[[str_c(var, "_se")]][row_2] * sqrt(data[[obs]][row_2]),
                      n.y=data[[obs]][row_2])
  } else if (type == "prop") {
    # If value is 0, return ""
    if (data[[var]][row_1] == 0 | data[[var]][row_2] == 0){
      return("")
    }
    test <- prop.test(x = c(data[[var]][row_1] * data[[obs]][row_1], 
                            data[[var]][row_2] * data[[obs]][row_2]),
                      n = c(data[[obs]][row_1], data[[obs]][row_2]),
                      correct = FALSE)
  } else {
    return(warning("Not a valid test"))
  }
  
  p <- p_stars(test$p.value)
}

# Create a function that finds proportion test and reports
# * if 10%, ** if 5%, and *** if 1% different
p_test_1 <- function(data, var, obs, row){
  test <- prop.test(x = data[[var]][row] * data[[obs]][row],
                    n = data[[obs]][row], correct = FALSE)
  p_stars(test$p.value)
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

# Create a function that takes an lm_robust model and returns residuals
# This one keeps residuals == 0 for now
lm_residuals_2 <- function(model) {
  model.frame(model)[[model$outcome]] - model$fitted.values
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

# Create a function that appends all variable names with each of a vector
# of given suffixes
add_suffix <- function(var_vec, suffix_vec, sep = "_") {
  return_vec <- c()
  for (var in var_vec) {
    for (suffix in suffix_vec) {
      return_vec %<>% append(str_c(var, suffix, sep = sep))
    }
  }
  return(return_vec)
}

# Define our default table top
table_top <- "\\documentclass[12pt]{article}
\\usepackage[margin=.5in]{geometry}
\\usepackage{siunitx}
\\usepackage{booktabs}
\\usepackage{graphicx}
\\begin{document}
\\begin{table}
\\footnotesize
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

# Rename union_fill to make matching easier
# Trim top 1% of time to find job
# Create wj_prev/next that sets 1 week to NA
# Create jj_transition if weeks_job_prev = 1
# Create same occ/ind prev/next
transition %<>%
  mutate(
    u_f_curr = union_fill_curr, 
    u_f_prev = union_fill_prev,
    u_f_next = union_fill_next,
    weeks_job_prev = ifelse(
      weeks_job_prev < quantile(weeks_job_prev, .99, na.rm = T), 
      weeks_job_prev, NA),
    weeks_job_next = ifelse(
      weeks_job_next < quantile(weeks_job_next, .99, na.rm = T), 
      weeks_job_next, NA),
    wj_prev = ifelse(weeks_job_prev > 1, weeks_job_prev, NA),
    wj_next = ifelse(weeks_job_next > 1, weeks_job_next, NA),
    jj_prev = 1 * (weeks_job_prev == 1),
    jj_next = 1 * (weeks_job_next == 1),
    same_occ_prev = (occ_curr == occ_prev),
    same_ind_prev = (ind_curr == ind_prev),
    same_occ_next = (occ_curr == occ_next),
    same_ind_next = (ind_curr == ind_next)
  )

# Summary Statistics ------------------------------------------------------

vars_sum_all <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "part_time",
                  "union", "u_f", "job_sat", "any_benefits", "health", 
                  "retirement", "self_emp", "indep_con", "on_call",
                  "temp_work", "traditional")

vars_sum_np <- c("same_occ", "same_ind", "outsourced", "weeks_job", "wj", "jj")

vars_mean <- c(add_suffix(vars_sum_all, c("curr", "prev", "next")),
               add_suffix(vars_sum_np, c("prev", "next")),
               "n")

vars_match <- str_c(str_c(vars_sum_all, collapse = "|"),
                    str_c(vars_sum_np, collapse = "|"),
                    sep = "|")

constant <- c("outsourced_curr", "n", "n_se")

# Summary Statistics
transition_summary <- split_data(transition)

for (i in 1:2) { 
  transition_summary[[i]] %<>%
    filter(!is.na(outsourced_curr), (outsourced_curr == 1 | traditional_curr == 1),
           emp_id_curr >= 100) %>% 
    as_survey_design(ids = case_id, weights=weight) %>%
    group_by(outsourced_curr) %>%
    mutate(n = n()) %>% 
    summarise_at(vars_mean, survey_mean, na.rm = T) %>%
    gather(key=key, value=value, -constant) %>%
    mutate(
      var = str_subset(key, vars_match) %>% str_extract(vars_match),
      se = ifelse(str_detect(key, "_se"), "_se", ""),
      curr = str_detect(key, "_curr"),
      prev = str_detect(key, "_prev")) %>%
    unite(variable, var, se, sep = "") %>%
    select(-key) %>%
    spread(key=variable, value=value) %>% 
    select(-n_se) %>% 
    arrange(desc(outsourced_curr), desc(curr), desc(prev))
}

top <- "\\begin{tabular}{lSSSSSS}
\\toprule
& \\multicolumn{3}{c}{{Outsourced Currently}} & \\multicolumn{3}{c}{{Traditional Currently}} \\\\
& {Previous} & {Current} & {Next} & {Previous} & {Current} & {Next}  \\\\  \\midrule
"

# For Slides, use only previous and next
s_top <- "\\begin{tabular}{lSSSS}
\\toprule
& \\multicolumn{2}{c}{{Outsourced Currently}} & \\multicolumn{2}{c}{{Traditional Currently}} \\\\
& {Previous} & {Next} & {Previous} & {Next}  \\\\  \\midrule
"

# Create table in Latex
vars_t <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "part_time",
            "union", "job_sat", "health", "any_benefits")

# Divide variables by mean or percent (they are different below)
vars_t_m <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "job_sat")

vars_t_p <- c("part_time", "union", "health", "any_benefits")

# Also check if new job has same occ/ind
vars_t_o <- c("occ", "ind")

for (ho in 1:2) {
  if (ho == 1) {
    occ_name <- ""
    Occ_name <- ""
    occ_label <- ""
  } else {
    occ_name <- " in high outsourcing occupations"
    Occ_name <- " HO Occupations"
    occ_label <- "_ho"
  }
  
  center <- rbind("Outsourced", "", "Same", "Occupation", "Same", "Industry",
                  "Log Real", "Hourly Wage", "Log Real", "Weekly Earnings", 
                  "Hours Worked", "Weekly", "Part Time", "", "Union", "",
                  "Job Satisfaction", "(Lower Better)", "Health", "Insurance",
                  "Any Benefits", "", "Weeks To", "Find Job", "Weeks To", 
                  "Find Job ($>1$ week)", "Job-to-Job", "Transition", "Observations")
    
  for (out in c(1, 4)){
    
    i <- out
    i_p <- out + 1
    i_n <- out + 2
    
    # Start with outsourced
    temp <- cbind(
      rbind(
        format_val(transition_summary[[ho]]$outsourced[i_p], 
                   star = p_test_1(transition_summary[[ho]], "outsourced", "n", i_p)),
        format_se(transition_summary[[ho]]$outsourced_se[i_p])
      ),
      rbind(
        str_c(" & {", format_it(1 * (out == 1), 3, 0), "}"),
        str_c(" &  ")
      ),
      rbind(
        format_val(transition_summary[[ho]]$outsourced[i_n], 
                   star = p_test_1(transition_summary[[ho]], "outsourced", "n", i_n)),
        format_se(transition_summary[[ho]]$outsourced_se[i_n])
      )
    )
    
    # Now do same occ/ind
    for (var in vars_t_o){
      var_n <- str_c("same", var, sep = "_")
      se_n <- str_c("same", var, "se", sep = "_")
      temp <- rbind(temp,
                    cbind(
                      rbind(
                        format_val(transition_summary[[ho]][[var_n]][i_p]),
                        format_se(transition_summary[[ho]][[se_n]][i_p])
                      ),
                      rbind(str_c(" &  {--} "), str_c(" & ")),
                      rbind(
                        format_val(transition_summary[[ho]][[var_n]][i_n]),
                        format_se(transition_summary[[ho]][[se_n]][i_n])
                      )
                    )
      )
    }
    
    # Now for everything else
    for (var in vars_t){
      se <- str_c(var, "se", sep = "_")
      t <- if (var %in% vars_t_p) "prop" else "mean"
      p_star_p <- test(transition_summary[[ho]], var, "n", i, i_p, type = t)
      p_star_n <- test(transition_summary[[ho]], var, "n", i, i_n, type = t)
      temp %<>% rbind(
        cbind(
          rbind(
            format_val(transition_summary[[ho]][[var]][i_p], star = p_star_p),
            format_se(transition_summary[[ho]][[se]][i_p])
          ),
          rbind(
            format_val(transition_summary[[ho]][[var]][i]),
            format_se(transition_summary[[ho]][[se]][i])
          ),
          rbind(
            format_val(transition_summary[[ho]][[var]][i_n], star = p_star_n),
            format_se(transition_summary[[ho]][[se]][i_n])
          )
        )
      )
    }
    
    # Weeks to find job (raw and >1), jj transitions, and n observations
    temp %<>% rbind(
      cbind(rbind(
              format_val(transition_summary[[ho]]$weeks_job[i_p]),
              format_se(transition_summary[[ho]]$weeks_job_se[i_p], r = 2, s = 2),
              format_val(transition_summary[[ho]]$wj[i_p]),
              format_se(transition_summary[[ho]]$wj_se[i_p], r = 2, s = 2),
              format_val(transition_summary[[ho]]$jj[i_p]),
              format_se(transition_summary[[ho]]$jj_se[i_p], r = 2, s = 2),
              " & "),
            rbind(" & {--}", " & ", " & {--}", " & ", " & {--}", " & ",
                  format_n(transition_summary[[ho]]$n[i])),
            rbind(
              format_val(transition_summary[[ho]]$weeks_job[i_n]),
              format_se(transition_summary[[ho]]$weeks_job_se[i_n], r = 2, s = 2),
              format_val(transition_summary[[ho]]$wj[i_n]),
              format_se(transition_summary[[ho]]$wj_se[i_n], r = 2, s = 2),
              format_val(transition_summary[[ho]]$jj[i_n]),
              format_se(transition_summary[[ho]]$jj_se[i_n], r = 2, s = 2),
              " & ")
            )
      )
    
    if (out == 1){
      center %<>% cbind(temp)
    } else{
      center %<>% cbind(
        temp,
        rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
              "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
              "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
              "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
              "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\"))
    }
    
  }
  
  # Keep only certain rows/coumns for Slides
  s_center <- center[c(1:6, 23:28), c(1:2, 4:5, 7:8)]
  # Put observations back
  s_center %<>% rbind(
    cbind("Observations ",
      str_c("& \\multicolumn{2}{c}{{",transition_summary[[ho]]$n[1], "}}"),
          "", 
          str_c("& \\multicolumn{2}{c}{{",transition_summary[[ho]]$n[4], "}}"),
          "", "\\\\")
  ) 
  
  bot <- str_c(
    "\\bottomrule
\\end{tabular}
}
\\caption{Job summary statistics in the NLSY", occ_name,
    " at previous, current, and next job for workers
who are currently outsourced compared to those who are currently in traditional jobs.
Observations are at the person-job level and summary statistics are weighted at 
the person level. Stars represent significant difference from
current job (except for outsourced which represents significant difference from 0)
at the .10 level *, .05 level **, and .01 level ***.}
\\label{jobs_transitions", occ_label ,"}
\\end{table}"
  )
  
  # Do weird stuff to create LaTeX output
  t_folder <- str_c(table_folder, "Junk/")
  file_1 <- str_c(t_folder, "center.txt")
  write.table(center, file_1, quote=T, col.names=F, row.names=F)
  center <- read.table(file_1, sep = "")
  write.table(center, file_1, quote=F, col.names=F, row.names=F, sep = "")
  center <- readChar(file_1, nchars = 1e6)
  
  write.table(str_c(table_top, siunitx, top, center, bot, "\n \\end{document}"),
              str_c(table_folder,
                    "NLSY79 Job Transitions/Summary Statistics", Occ_name, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save to Drafts (Use s_table top to resize)
  write.table(str_c(s_table_top, top, center, bot),
              str_c(d_table_folder,
                    "Job Transition Summary Statistics", Occ_name, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save to Slides
  # Do weird stuff to create LaTeX output
  t_folder <- str_c(table_folder, "Junk/")
  file_1 <- str_c(t_folder, "center.txt")
  write.table(s_center, file_1, quote=T, col.names=F, row.names=F)
  s_center <- read.table(file_1, sep = "")
  write.table(s_center, file_1, quote=F, col.names=F, row.names=F, sep = "")
  s_center <- readChar(file_1, nchars = 1e6)
  
  write.table(str_c(s_table_top, s_top, s_center, s_bot),
              str_c(s_table_folder,
                    "Job Transition Summary Statistics", Occ_name, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
}

# Job Quality Before and After Transitions --------------------------------

# Look at job summary statisitcs before and after transitions by 
# outsourced to traditonal and tradtional to outsourced
vars_sum_all <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "part_time",
                  "union", "job_sat", "any_benefits", "health", "retirement")

vars_sum_np <- c("same_occ", "same_ind", "weeks_job", "wj", "jj")

vars_mean <- c(add_suffix(vars_sum_all, c("curr", "prev")),
               add_suffix(vars_sum_np, c("prev")),
               "n")

vars_match <- str_c(str_c(vars_sum_all, collapse = "|"),
                    str_c(vars_sum_np, collapse = "|"),
                    sep = "|")

types <- c("outsourced", "traditional", "self_emp", "indep_con", "temp_work", "on_call")

constant <- c("n", "n_se", add_suffix(types, c("curr", "prev")))

# Summary Statistics By Job Type before and after
transition_type <- split_data(transition)

for (i in 1:2) { 
  transition_type[[i]] %<>%
    filter(!is.na(outsourced_curr), !is.na(outsourced_prev), emp_id_curr >= 100) %>% 
    as_survey_design(ids = case_id, weights=weight) %>%
    group_by(outsourced_curr, traditional_curr, 
             indep_con_curr, temp_work_curr, self_emp_curr, on_call_curr,
             outsourced_prev, traditional_prev, 
             indep_con_prev, temp_work_prev, self_emp_prev, on_call_prev
             ) %>%
    mutate(n = n()) %>% 
    summarise_at(vars_mean, survey_mean, na.rm = T) %>% 
    gather(key=key, value=value, -constant) %>%
    mutate(
      var = str_subset(key, vars_match) %>% str_extract(vars_match),
      se = ifelse(str_detect(key, "_se"), "_se", ""),
      curr = str_detect(key, "_curr")) %>%
    unite(variable, var, se, sep = "") %>%
    select(-key) %>%
    spread(key=variable, value=value) %>% 
    mutate(
      curr_type = case_when(outsourced_curr == 1 ~ "outsourced",
                           traditional_curr == 1 ~ "traditional",
                           self_emp_curr == 1 ~ "self_emp",
                           indep_con_curr == 1 ~ "indep_con",
                           temp_work_curr == 1 ~ "temp_work",
                           on_call_curr == 1 ~ "on_call"),
      prev_type = case_when(outsourced_prev == 1 ~ "outsourced",
                           traditional_prev == 1 ~ "traditional",
                           self_emp_prev == 1 ~ "self_emp",
                           indep_con_prev == 1 ~ "indep_con",
                           temp_work_prev == 1 ~ "temp_work",
                           on_call_prev == 1 ~ "on_call"),
      # Label job type as T or O (or none) for graphing
      type = case_when((curr == TRUE) & (curr_type == "traditional") ~ "Traditional",
                       (curr == TRUE) & (curr_type == "outsourced") ~ "Outsourced",
                       (curr == FALSE) & (prev_type == "traditional") ~ "Traditional",
                       (curr == FALSE) & (prev_type == "outsourced") ~ "Outsourced"
      )
    ) %>% 
    select(-n_se, -ends_with("_curr"), -ends_with("_prev"))
}

# For outsourced and traditional jobs, see how previous and current
# jobs differ
var_label <- c("Log Real Hourly Wage", "Log Real Weekly Wage", 
               "Hours Worked Per Week", "Part-Time", "Union",
               "Job Satisfaction (Lower Better)", "Any Benefits",
               "Health Insurance", "Retrement Benefits")

var_save <- c("LRH Wage", "LRW Wage", "Hours", "Part Time", "Union",
              "Job Satisfaction", "Benefits", "Health", "Retirement")

ho_save <- c("", " Ever HO Occupation")

for (ho in c(1, 2)) {
  for (i in seq_along(vars_sum_all)) {
    temp <- transition_type[[ho]] %>% 
      filter(prev_type %in% c("outsourced", "traditional"), 
             curr_type %in% c("outsourced", "traditional")) %>% 
      ggplot(aes_string(x = "curr", y = vars_sum_all[i], 
                 color = "curr_type", shape = "prev_type", 
                 linetype = "prev_type", group = "n")) +
      # geom_point(size = 4) +
      geom_line(show.legend = FALSE) +
      # Include labels on all points 
      geom_text(aes(label = type, hjust = "outward"), size = 4, check_overlap = TRUE) +
      labs(x = "Job", y = var_label[i]) +
      scale_color_manual(name = "Current Job", breaks = c("traditional", "outsourced"),
                        values = c("blue", "red"),
                        labels = c("Traditional", "Outsourced")) +
      scale_x_discrete(breaks = c(FALSE, TRUE), 
                       labels = c("Previous Job", "Current Job")) +
      theme_light(base_size = 16) +
      theme(legend.position = "none")
    
    ggsave(str_c(figure_folder, "Job Transition ", var_save[i], ho_save[ho], ".pdf"),
           height = height, width = width)
    
    # For paper, make some tables custom so all labels can be seen
    # For LRW_Wage
    if (i == 2 & ho == 1) {
      temp <- transition_type[[ho]] %>% 
        filter(prev_type %in% c("outsourced", "traditional"), 
               curr_type %in% c("outsourced", "traditional")) %>% 
        ggplot(aes_string(x = "curr", y = vars_sum_all[i], 
                          color = "curr_type", shape = "prev_type", 
                          linetype = "prev_type", group = "n")) +
        geom_line(show.legend = FALSE) +
        # Include labels on all points 
        # Previous
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==F & transition_type[[ho]]$prev_type=="traditional" & transition_type[[ho]]$curr_type=="outsourced"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "red") +
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==F & transition_type[[ho]]$prev_type=="traditional" & transition_type[[ho]]$curr_type=="traditional"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==F & transition_type[[ho]]$prev_type=="outsourced" & transition_type[[ho]]$curr_type=="outsourced"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red") +
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==F & transition_type[[ho]]$prev_type=="outsourced" & transition_type[[ho]]$curr_type=="traditional"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "blue") +
        # Current
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==T & transition_type[[ho]]$prev_type=="traditional" & transition_type[[ho]]$curr_type=="outsourced"],
                      label = "Outsourced", hjust = "outward"), 
                  size = 4, color = "red") +
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==T & transition_type[[ho]]$prev_type=="traditional" & transition_type[[ho]]$curr_type=="traditional"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==T & transition_type[[ho]]$prev_type=="outsourced" & transition_type[[ho]]$curr_type=="outsourced"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red") +
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==T & transition_type[[ho]]$prev_type=="outsourced" & transition_type[[ho]]$curr_type=="traditional"],
                      label = "Traditional", hjust = "outward"),
                  size = 4, color = "blue") +
        labs(x = "Job", y = var_label[i]) +
        scale_color_manual(name = "Current Job", breaks = c("traditional", "outsourced"),
                           values = c("blue", "red"),
                           labels = c("Traditional", "Outsourced")) +
        scale_x_discrete(breaks = c(FALSE, TRUE), 
                         labels = c("Previous Job", "Current Job")) +
        theme_light(base_size = 16) +
        theme(legend.position = "none")
      
      ggsave(str_c(figure_folder, "Job Transition ", var_save[i], ho_save[ho], ".pdf"),
             height = height, width = width)
    }
    
    # For Health
    if (i == 8 & ho == 1) {
      temp <- transition_type[[ho]] %>% 
        filter(prev_type %in% c("outsourced", "traditional"), 
               curr_type %in% c("outsourced", "traditional")) %>% 
        ggplot(aes_string(x = "curr", y = vars_sum_all[i], 
                          color = "curr_type", shape = "prev_type", 
                          linetype = "prev_type", group = "n")) +
        geom_line(show.legend = FALSE) +
        # Include labels on all points 
        # Previous
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==F & transition_type[[ho]]$prev_type=="traditional" & transition_type[[ho]]$curr_type=="outsourced"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "red") +
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==F & transition_type[[ho]]$prev_type=="traditional" & transition_type[[ho]]$curr_type=="traditional"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==F & transition_type[[ho]]$prev_type=="outsourced" & transition_type[[ho]]$curr_type=="outsourced"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red") +
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==F & transition_type[[ho]]$prev_type=="outsourced" & transition_type[[ho]]$curr_type=="traditional"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "blue") +
        # Current
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==T & transition_type[[ho]]$prev_type=="traditional" & transition_type[[ho]]$curr_type=="outsourced"],
                      label = "Outsourced", hjust = "outward"), 
                  size = 4, color = "red") +
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==T & transition_type[[ho]]$prev_type=="traditional" & transition_type[[ho]]$curr_type=="traditional"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==T & transition_type[[ho]]$prev_type=="outsourced" & transition_type[[ho]]$curr_type=="outsourced"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red") +
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[vars_sum_all[i]]][transition_type[[ho]]$curr==T & transition_type[[ho]]$prev_type=="outsourced" & transition_type[[ho]]$curr_type=="traditional"],
                      label = "Traditional", hjust = "outward"),
                  size = 4, color = "blue") +
        labs(x = "Job", y = var_label[i]) +
        scale_color_manual(name = "Current Job", breaks = c("traditional", "outsourced"),
                           values = c("blue", "red"),
                           labels = c("Traditional", "Outsourced")) +
        scale_x_discrete(breaks = c(FALSE, TRUE), 
                         labels = c("Previous Job", "Current Job")) +
        theme_light(base_size = 16) +
        theme(legend.position = "none")
      
      ggsave(str_c(figure_folder, "Job Transition ", var_save[i], ho_save[ho], ".pdf"),
             height = height, width = width)
    }
  }
}

# For Job Transitions, how do they differ for jobs coming from/going to

# This function takes current and previous job type and selects the previous
# job row
select_transition <- function(data, pt, ct) {
  filter(data, curr == FALSE, prev_type == pt, curr_type == ct) 
}

top <- "\\begin{tabular}{lSSSS}
\\toprule
Current Job & \\multicolumn{2}{c}{{Outsourced}} &  \\multicolumn{2}{c}{{Traditional}} \\\\
Previous Job & {Outsourced} & {Traditional} & {Outsourced} & {Traditional} \\\\  \\midrule
"

t_types <- c("outsourced", "traditional")

for (ho in 1:2) {
  if (ho == 1) {
    occ_name <- ""
    Occ_name <- ""
    occ_label <- ""
  } else {
    occ_name <- " in high outsourcing occupations"
    Occ_name <- " HO Occupations"
    occ_label <- "_ho"
  }
  
  center <- rbind("Same", "Occupation", "Same", "Industry",
                  "Weeks To", "Find Job", "Weeks To", "Find Job ($>1$ week)",
                  "Job-to-Job", "Transition", "Observations")
  
  c_r <- c()
  for (ct in t_types) {
    for (pt in t_types) {
      c_c <- c()
      for (var in vars_sum_np){
        se <- str_c(var, "_se")
            c_c %<>% rbind(
              format_val(select_transition(transition_type[[ho]], pt, ct)[[var]]),
              format_se(select_transition(transition_type[[ho]], pt, ct)[[se]])
            )
      }
      # Add observations
      c_c %<>% rbind(format_n(select_transition(transition_type[[ho]], pt, ct)$n))
      c_r %<>% cbind(c_c)
    }
  }
  
  center %<>% cbind(
    c_r,
    rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]",
          "\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\"))
  
  bot <- str_c(
    "\\bottomrule
\\end{tabular}
}
\\caption{Job transition summary statistics in the NLSY", occ_name,
" between previous and current job. Transitions are grouped by workers
coming from and currently in outsourced or traditional jobs.
Observations are at the person-job level and summary statistics are weighted at 
the person level.}
\\label{jobs_types_transitions", occ_label ,"}
\\end{table}"
  )
  
  # Do weird stuff to create LaTeX output
  t_folder <- str_c(table_folder, "Junk/")
  file_1 <- str_c(t_folder, "center.txt")
  write.table(center, file_1, quote=T, col.names=F, row.names=F)
  center <- read.table(file_1, sep = "")
  write.table(center, file_1, quote=F, col.names=F, row.names=F, sep = "")
  center <- readChar(file_1, nchars = 1e6)
  
  write.table(str_c(table_top, siunitx, top, center, bot, "\n \\end{document}"),
              str_c(table_folder,
                    "NLSY79 Job Transitions/Type Transition Summary Statistics",
                    Occ_name, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save to Drafts (Use s_table top to resize) and Slides
  write.table(str_c(s_table_top, top, center, bot),
              str_c(d_table_folder,
                    "Job Type Transition Summary Statistics", Occ_name, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  write.table(str_c(s_table_top, top, center, s_bot),
              str_c(s_table_folder,
                    "Job Type Transition Summary Statistics", Occ_name, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
}

# For some outcomes, run regressions that don't know outsourced versus traditional
# How do residuals change before and after? Run regression on all jobs using 
# current job, then predict residuals for previous jobs
# Bug prevents using predict for lm_robust with fe, so regressions will be slow
# Run regressions for all workers, use residuals for both all and HO workers

vars_reg <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "part_time",
              "job_sat", "any_benefits", "health", "retirement")

var_label <- c("Log Real Hourly Wage", "Log Real Weekly Wage", 
               "Hours Worked Per Week", "Part-Time",
               "Job Satisfaction (Lower Better)", "Any Benefits",
               "Health Insurance", "Retrement Benefits")

var_save <- c("LRH Wage", "LRW Wage", "Hours", "Part Time",
              "Job Satisfaction", "Benefits", "Health", "Retirement")

ho_save <- c("", " Ever HO Occupation")

types <- c("self_emp", "indep_con", "temp_work", "on_call")

controls <- c("age", "I((age)^2)", "I((age)^3)", "I((age)^4)", "tot_child", 
              "hh_child", "factor(union_fill)")

hours <- c("hours_week", "part_time")

tenure <- c("I((tenure/100))", "I((tenure/100)^2)",
                 "I((tenure/100)^3)", "I((tenure/100)^4)")

fixed_effects <- c("factor(region)", "factor(marital_status)", "factor(msa)",
                   "factor(case_id)", "factor(occ)",
                   "factor(I(year(week_start_job)))", "factor(I(year(week_end_job)))")

# Need a dataset with just current and previous jobs by same 
# variable names (so prediction will work)
transition_curr <- transition %>% 
  select(case_id:last_week_curr, emp_id_prev) %>% 
  rename_at(.vars = vars(ends_with("_curr")),
            .funs = ~ sub("_curr", "", .))

transition_prev <- transition %>% 
  select(case_id, weight, sample_id, emp_id_prev:last_week_prev, emp_id_curr) %>% 
  rename_at(.vars = vars(ends_with("_prev")),
            .funs = ~ sub("_prev", "", .))

for (ind in seq_along(vars_reg)) {
  var <- vars_reg[ind]
  
  ind_vars <- c(types, controls, tenure)
  
  if (!(var %in% c("log_real_wkly_wage", "health", "any_benefits",
                   "hours_week", "part_time"))){
    ind_vars  %<>% c(hours)
    t_transition_curr <- filter(transition_curr, !is.na(transition_curr[[var]]), 
                                !is.na(hours_week), !is.na(part_time),
                                !is.na(region), !is.na(marital_status), !is.na(msa),
                                !is.na(occ),
                                !is.na(outsourced), emp_id >= 100,
                                !is.na(tot_child), !is.na(hh_child), !is.na(tenure))
    t_transition_prev <- filter(transition_prev, !is.na(transition_prev[[var]]),
                                !is.na(hours_week), !is.na(part_time),
                                !is.na(hours_week), !is.na(part_time),
                                !is.na(region), !is.na(marital_status), !is.na(msa),
                                !is.na(occ),
                                !is.na(outsourced), emp_id >= 100,
                                !is.na(tot_child), !is.na(hh_child), !is.na(tenure))
  } else {
    t_transition_curr <- filter(transition_curr, !is.na(transition_curr[[var]]),
                                !is.na(region), !is.na(marital_status), !is.na(msa),
                                !is.na(occ),
                                !is.na(outsourced), emp_id >= 100,
                                !is.na(tot_child), !is.na(hh_child), !is.na(tenure))
    t_transition_prev <- filter(transition_prev, !is.na(transition_prev[[var]]),
                                !is.na(hours_week), !is.na(part_time),
                                !is.na(region), !is.na(marital_status), !is.na(msa),
                                !is.na(occ),
                                !is.na(outsourced), emp_id >= 100, 
                                !is.na(tot_child), !is.na(hh_child), !is.na(tenure))
  }
  
  eq <- create_formula(var, list(ind_vars, fixed_effects))
  
  reg <- lm_robust(eq, data = t_transition_curr, weights = weight,
                        subset = !is.na(region) & !is.na(marital_status)
                        & !is.na(msa) & !is.na(occ) & !is.na(outsourced)
                        & (emp_id >= 100),
                        # fixed_effects = !!fe,
                        clusters = as_factor(sample_id),
                        se_type = "stata", try_cholesky = T)
  
  t_transition_curr %<>% 
    mutate(
      predict = predict(reg, newdata = t_transition_curr, na.action = "na.exclude"),
      residual = !!sym(var) - predict
    ) %>% 
    # filter abs(residuals) >= 1e-11 and currently outsourced/traditional
    filter(abs(residual) >= 1e-11, outsourced == 1 | traditional == 1)
  
  t_transition_prev %<>% 
    mutate(
      predict = predict(reg, newdata = t_transition_prev, na.action = "na.exclude"),
      residual = !!sym(var) - predict
    ) %>% 
    # filter abs(residuals) >= 1e-11 and currently outsourced/traditional
    filter(abs(residual) >= 1e-11, outsourced == 1 | traditional == 1)
  
  # Match datasets
  temp_data <- inner_join(t_transition_curr, t_transition_prev, 
                         by = c("case_id" = "case_id", "emp_id_prev" = "emp_id"),
                         suffix = c("_curr", "_prev"))
  
  temp_summary <- split_data(temp_data)
  vars_temp <- c("residual_curr", "residual_prev", "n")
  constant_temp <- c("n", "n_se", "outsourced_curr", "traditional_curr",
                     "outsourced_prev", "traditional_prev")
  
  for (ho in c(1, 2)) {
    temp_summary[[ho]] %<>%
      as_survey_design(ids = case_id, weights=weight_curr) %>%
      group_by(outsourced_curr, traditional_curr,
               outsourced_prev, traditional_prev) %>%
      mutate(n = n()) %>% 
      summarise_at(vars_temp, survey_mean, na.rm = T) %>% 
      gather(key=key, value=value, -constant_temp) %>% 
      mutate(
        var = ifelse(str_detect(key, "residual"), "residual", key),
        se = ifelse(str_detect(key, "_se"), "_se", ""),
        curr = str_detect(key, "_curr")) %>%
      unite(variable, var, se, sep = "") %>%
      select(-key) %>% 
      spread(key=variable, value=value) %>% 
      mutate(
        curr_type = case_when(outsourced_curr == 1 ~ "outsourced",
                              traditional_curr == 1 ~ "traditional"),
        prev_type = case_when(outsourced_prev == 1 ~ "outsourced",
                              traditional_prev == 1 ~ "traditional"),
        # Label job type as T or O (or none) for graphing
        type = case_when((curr == TRUE) & (curr_type == "traditional") ~ "Traditional",
                         (curr == TRUE) & (curr_type == "outsourced") ~ "Outsourced",
                         (curr == FALSE) & (prev_type == "traditional") ~ "Traditional",
                         (curr == FALSE) & (prev_type == "outsourced") ~ "Outsourced"
        )
      ) %>% 
      select(-n_se)
    
    temp <- temp_summary[[ho]] %>%  
      ggplot(aes(x = curr, y = residual, 
                 color = curr_type, linetype = prev_type, group = n)) +
      geom_line(show.legend = FALSE) +
      # Include labels on all points 
      geom_text(aes(label = type, hjust = "outward"), size = 4, check_overlap = TRUE) +
      labs(x = "Job", y = str_c(var_label[ind], " Residuals")) +
      scale_color_manual(name = "Current Job", breaks = c("traditional", "outsourced"),
                         values = c("blue", "red"),
                         labels = c("Traditional", "Outsourced")) +
      scale_x_discrete(breaks = c(FALSE, TRUE), 
                       labels = c("Previous Job", "Current Job")) +
      theme_light(base_size = 16) +
      theme(legend.position = "none")
    
    ggsave(str_c(figure_folder, "Job Transition ", var_save[ind], " Residuals", 
                 ho_save[ho], ".pdf"), height = height, width = width)
    
    # For paper, make some figures custom so all labels can be seen
    # For LRW_Wage
    if (ind == 2 & ho == 1) {
      temp <- temp_summary[[ho]] %>%  
        ggplot(aes(x = curr, y = residual, 
                   color = curr_type, linetype = prev_type, group = n)) +
        geom_line(show.legend = FALSE) +
        # Include labels on all points 
        # Previous
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$prev_type=="traditional" & temp_summary[[ho]]$curr_type=="outsourced"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "red") +
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$prev_type=="traditional" & temp_summary[[ho]]$curr_type=="traditional"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$prev_type=="outsourced" & temp_summary[[ho]]$curr_type=="outsourced"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red") +
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$prev_type=="outsourced" & temp_summary[[ho]]$curr_type=="traditional"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "blue") +
        # Current
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$prev_type=="traditional" & temp_summary[[ho]]$curr_type=="outsourced"],
                      label = "Outsourced", hjust = "outward"), 
                  size = 4, color = "red", nudge_y = .0015) +
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$prev_type=="traditional" & temp_summary[[ho]]$curr_type=="traditional"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$prev_type=="outsourced" & temp_summary[[ho]]$curr_type=="outsourced"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red", nudge_y = -.0015) +
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$prev_type=="outsourced" & temp_summary[[ho]]$curr_type=="traditional"],
                      label = "Traditional", hjust = "outward"),
                  size = 4, color = "blue") +
        labs(x = "Job", y = str_c(var_label[ind], " Residuals")) +
        scale_color_manual(name = "Current Job", breaks = c("traditional", "outsourced"),
                           values = c("blue", "red"),
                           labels = c("Traditional", "Outsourced")) +
        scale_x_discrete(breaks = c(FALSE, TRUE), 
                         labels = c("Previous Job", "Current Job")) +
        theme_light(base_size = 16) +
        theme(legend.position = "none")
      
      ggsave(str_c(figure_folder, "Job Transition ", var_save[ind], " Residuals", 
                   ho_save[ho], ".pdf"), height = height, width = width)
    }
    
    # For Health
    if (ind == 7 & ho == 1) {
      temp <- temp_summary[[ho]] %>%  
        ggplot(aes(x = curr, y = residual, 
                   color = curr_type, linetype = prev_type, group = n)) +
        geom_line(show.legend = FALSE) +
        # Include labels on all points 
        # Previous
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$prev_type=="traditional" & temp_summary[[ho]]$curr_type=="outsourced"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "red") +
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$prev_type=="traditional" & temp_summary[[ho]]$curr_type=="traditional"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$prev_type=="outsourced" & temp_summary[[ho]]$curr_type=="outsourced"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red") +
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$prev_type=="outsourced" & temp_summary[[ho]]$curr_type=="traditional"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "blue") +
        # Current
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$prev_type=="traditional" & temp_summary[[ho]]$curr_type=="outsourced"],
                      label = "Outsourced", hjust = "outward"), 
                  size = 4, color = "red") +
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$prev_type=="traditional" & temp_summary[[ho]]$curr_type=="traditional"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$prev_type=="outsourced" & temp_summary[[ho]]$curr_type=="outsourced"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red") +
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$prev_type=="outsourced" & temp_summary[[ho]]$curr_type=="traditional"],
                      label = "Traditional", hjust = "outward"),
                  size = 4, color = "blue") +
        labs(x = "Job", y = str_c(var_label[ind], " Residuals")) +
        scale_color_manual(name = "Current Job", breaks = c("traditional", "outsourced"),
                           values = c("blue", "red"),
                           labels = c("Traditional", "Outsourced")) +
        scale_x_discrete(breaks = c(FALSE, TRUE), 
                         labels = c("Previous Job", "Current Job")) +
        theme_light(base_size = 16) +
        theme(legend.position = "none")
      
      ggsave(str_c(figure_folder, "Job Transition ", var_save[ind], " Residuals", 
                   ho_save[ho], ".pdf"), height = height, width = width)
    }
  }
}

# Weeks to Job ------------------------------------------------------------

# Focus on weeks_job_prev and wj_prev using HO Occupations 
# (transition_summary[[2]])

# Plot mean 
table <- "\\begin{tabular}{lSSSS}
\\toprule
& \\multicolumn{2}{c}{{All Transitions}} &  \\multicolumn{2}{c}{{$>1$ Week}} \\\\
& {Outsourced} & {Traditional} & {Outsourced} & {Traditional} \\\\  \\midrule
"

table %<>% str_c(
  format_val(transition_summary[[2]]$weeks_job[2]),
  format_val(transition_summary[[2]]$weeks_job[5],
             test(transition_summary[[2]], "weeks_job", "n",
                    2, 5, type = "mean")),
  format_val(transition_summary[[2]]$wj[2]),
  format_val(transition_summary[[2]]$wj[5], 
             test(transition_summary[[2]], "wj", "n",
                    2, 5, type = "mean")), "\\\\ \n",
  format_se(transition_summary[[2]]$weeks_job_se[2]),
  format_se(transition_summary[[2]]$weeks_job_se[5]),
  format_se(transition_summary[[2]]$wj_se[2]),
  format_se(transition_summary[[2]]$wj_se[5]),
  "\\\\ \n")

bot <- "\\bottomrule
\\end{tabular}
}
\\caption{Mean weeks to find current job for workers in high outsourcing
occupations both overall and for periods longer than 1 week. 
Stars represent significant difference from outsourced jobs at 
the .10 level *, .05 level **, and .01 level ***.}
\\label{weeks_find_job}
\\end{table}"

write.table(str_c(table_top, siunitx, table, bot, "\n \\end{document}"), 
  str_c(table_folder, "NLSY79 Job Transitions/Weeks to Find Job HO Occupations.tex"),
  quote=F, col.names=F, row.names=F, sep="")

# Save in Drafts and Slides
write.table(str_c(d_table_top, table, bot), 
            str_c(d_table_folder, "Weeks to Find Job HO Occupations.tex"),
            quote=F, col.names=F, row.names=F, sep="")

write.table(str_c(s_table_top, table, s_bot), 
            str_c(s_table_folder, "Weeks to Find Job HO Occupations.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Plot figures 
var_g <- c("weeks_job_prev", "wj_prev", "weeks_job_next", "wj_next")
var_g_s <- c("weeks_job", "wj", "weeks_job", "wj")
var_names <- c("Weeks to Find Job", "Weeks to Find Job",
               "Weeks to Find Next Job", "Weeks to Find Next Job")
var_save <- c("Weeks to Job", "Weeks to Job G1", 
              "Weeks to Next Job", "Weeks to Next Job G1")

ho_save <- c("", " HO Occupations")
trans_plot <- split_data(transition)

for (ho in 1:2) {
  for (i in seq_along(var_g)) {
      
    # Change row searching for mean based on prev or next 
    n_o <- if (i <= 2) 2 else 3
    n_t <- if (i <= 2) 5 else 6
    
    temp <- trans_plot[[ho]] %>%
      filter(!is.na(.[[var_g[i]]]), !is.na(outsourced_curr),
             (outsourced_curr == 1 | traditional_curr == 1),
             .[[var_g[i]]] < quantile(.[[var_g[i]]], .99, na.rm = T)) %>% 
      ggplot(aes_string(var_g[i], fill = "factor(outsourced_curr)")) +
      geom_density(alpha = 0.2, bounds = c(i - 1, Inf)) +
      geom_vline(aes(xintercept = transition_summary[[ho]][[var_g_s[i]]][n_o]),
                 color = "red", size=1) +
      geom_vline(aes(xintercept = transition_summary[[ho]][[var_g_s[i]]][n_t]),
                 color = "blue", size=1) +
      labs(x = var_names[i], y = "Density") +
      scale_fill_manual(name = "Outsourced", breaks = c(0, 1),
                        values = c("blue", "red"),
                        labels = c("Traditional", "Outsourced")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_light(base_size = 16)
    
    ggsave(str_c(figure_folder, var_save[i], ho_save[ho], ".pdf"),
           height = height, width = width)
  }
}

# Update data_moments with median wj_prev (no job-job transitions)
# for all ever_ho_occ workers to any job
weeks_to_job_ss <- transition %>% 
  filter(!is.na(outsourced_curr), (outsourced_curr == 1 | traditional_curr == 1)) %>% 
  as_survey_design(ids = case_id, weights=weight) %>% 
  summarise(
    median_wj_prev = survey_median(wj_prev, na.rm = T),
    mean_wj_prev = survey_mean(wj_prev, na.rm = T))

# WBJ <- weeks_to_job_ss$median_wj_prev[1]
# UE_2 <- 1 - .5 ^(1 / WBJ)

# data_moments <- update_parameters("WeeksBetweenJobs", WBJ)
# 
# write_csv(data_moments, str_c(clean_folder, "data_moments.csv"))

# # Plot to see how reasonable this looks
# max_w <- 75
# plot <- trans_plot[[2]] %>%
#   filter(!is.na(wj_prev), !is.na(outsourced_curr),
#          wj_prev < max_w)
# 
# weeks <- 0:max_w
# not_found <- 1 - (1 - l) ^ weeks
# # Turn not found into found pdf and adjust for population size
# found_pdf <- (not_found[2:length(weeks)] - not_found[1:(length(weeks) - 1)])
# found_hist <- found_pdf * NROW(plot)
# 
# sim <- str_c("l = ", round(l, 3))
# temp <- ggplot() +
#   geom_density(aes(x = plot$weeks_job_prev, fill = "Data"),
#                  alpha = 0.2, bounds = c(0, max_w)) +
#   geom_point(aes(x = 1:max_w, y = found_pdf, color = sim), alpha = 1) +
#   labs(x = "Weeks to Find Job", y = "Count") +
#   scale_fill_manual(name = "Data", 
#                     values = c("blue"),
#                     labels = c("Data")) +
#   scale_color_manual(name = "Model",
#                      values = c("orange"),
#                      labels = c(sim) 
#                      ) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
#   theme_light(base_size = 16)
# 
# ggsave(str_c(figure_folder, "Weeks To Job Data vs Model.pdf"),
#        height = height, width = width)

# Separate Job-Job Transitions --------------------------------------------

# Try to estimate how many workers go from job-job transitions.
# For now, assume all 1 week job gaps are job-job transitions (marked as jj)



# Regression on Weeks to Jobs ---------------------------------------------

top <- "\\begin{tabular}{lSSSSSS}
\\toprule
&  \\multicolumn{3}{c}{{OLS}} & \\multicolumn{3}{c}{{FE}} \\\\
Variables & {Basic}  & {Job Info} & {Occ FE}   & {Basic} & {Job Info} & {Occ FE} \\\\\\midrule
"

# Create an s_top with just the final regression for both 
# weeks to job and weeks to job == 1
s_top <- "\\begin{tabular}{lSSS}
\\toprule
Variables & {Weeks to Job} & {Weeks to Job ($>1$)} & {Job-Job Transition} \\\\\\midrule
"

# Type of job (compare to traditional)
types <- c("outsourced", "self_emp", "indep_con", "temp_work", "on_call")

# Demographic Controls
dem_controls <- c("age_curr", "I(age_curr^2)", "I(age_curr^3)", "I(age_curr^4)",
                  "tot_child_curr", "hh_child_curr")

# Current job control
job_controls <- c("hours_week", "part_time", "u_f", "health", "retirement",
              "any_benefits","log_real_wkly_wage")

# Tenure controls from previous job (/100 to keep small)
tenure_controls <- c("I(tenure_prev / 100)", "I((tenure_prev / 100)^2)",
                     "I((tenure_prev / 100)^3)",
                     "I((tenure_prev / 100)^4)")

# OLS controls
ols_controls <- c("black", "hispanic", "hs_curr", "aa_curr", "ba_curr", "plus_ba_curr")

# Fixed Effects for all regresssions
fixed_effects <- c("region_curr", "marital_status_curr", "msa_curr", 
                   "I(year(week_start_job_curr))")

# Occupation fixed effects
occ_effects <- c("occ_curr", "occ_prev")

# Individual fixed effects
id_effects <- c("case_id")

vars_1 <- list(add_suffix(types, c("curr", "prev")), dem_controls)
vars_2 <- append(vars_1, 
                 list(add_suffix(job_controls, c("curr", "prev")), tenure_controls))
var_list <- list(vars_1, vars_2, vars_2)

job_t <- c("No", "Yes", "Yes")
occ_t <- c("No", "No", "Yes")
worker_t <- c("No", "Yes")

center <- rbind("Outsourced", "Current", "Outsourced", "Previous",
                  "Job Info", "Occupation FE", "Worker FE", "$R^2$", "Obs")

vars <- c("weeks_job_prev", "wj_prev", "I(weeks_job_prev == 1)")
var_save <- c("Weeks to Find Job", "Weeks to Find Job G1",
              "Job to Job Transition")
labels <- c("_weeks_between_jobs", "_weeks_between_jobs_g1", "_job_to_job")
descriptions <- c("weeks to find current job", "weeks to find current job (>1 week)",
                  "probability of job to job transition")

c_s <- c()

for (k in 1:length(vars)) {

  c_i <- c()
  var <- vars[k]
  save <- var_save[k]
  label <- labels[k]
  desc <- descriptions[k]
  
  for (j in 1:2){
    for (i in 1:3) {
      
      controls <- var_list[[i]]
      fes <- fixed_effects
      
      if (i == 3) {
        fes %<>% append(occ_effects)
      }
      
      if (j == 1) {
        controls %<>% append(ols_controls)
      } else {
        fes %<>% append(id_effects)
      }
      
      eq <- create_formula(var, controls)
      fe <- create_formula("", fes)
      
      temp <- lm_robust(
        eq, data = transition, 
        subset = (!is.na(marital_status_curr)
                  & !is.na(msa_curr) & !is.na(occ_curr) & !is.na(occ_prev)),
        weights = weight,
        fixed_effects = !!fe,
        clusters = as_factor(sample_id),
        se_type = "stata", try_cholesky = T)
      
      # Put regession results in Tables
      c_i %<>% cbind(
        rbind(format_val(temp$coefficients["outsourced_curr"],
                         p_stars(temp$p.value["outsourced_curr"])),
              format_se(temp$std.error["outsourced_curr"]),
              format_val(temp$coefficients["outsourced_prev"],
                         p_stars(temp$p.value["outsourced_prev"])),
              format_se(temp$std.error["outsourced_prev"]),
              str_c(" & {", job_t[i], "}"),
              str_c(" & {", occ_t[i], "}"),
              str_c(" & {", worker_t[j], "}"),
              format_val(temp$r.squared),
              format_n(lm_N(temp))
        )
      )
      
      # If full regression, save to c_s
      if (i == 3 & j == 2) {
        
        c_s %<>% cbind(
          rbind(format_val(temp$coefficients["outsourced_curr"],
                           p_stars(temp$p.value["outsourced_curr"])),
                format_se(temp$std.error["outsourced_curr"]),
                format_val(temp$coefficients["outsourced_prev"],
                           p_stars(temp$p.value["outsourced_prev"])),
                format_se(temp$std.error["outsourced_prev"]),
                str_c(" & {", job_t[i], "}"),
                str_c(" & {", occ_t[i], "}"),
                str_c(" & {", worker_t[j], "}"),
                format_val(temp$r.squared),
                format_n(lm_N(temp))
          )
        )
        
        # If weeks_job_prev, see how well prediction does
        if (var == "weeks_job_prev") {
          df <- tibble(est = temp$fitted.values, 
                       act = model.frame(temp)$weeks_job_prev) 
          
          temp_plot <- df %>% 
            filter(act < quantile(act, .99), act != est) %>% 
            ggplot() +
            geom_abline(slope = 1, intercept = 0, color = "blue") +
            geom_jitter(aes(est, act)) +
            labs(x = "Estimated Weeks to Find Job", y = "Actual Weeks to Find Job") +
            theme_light(base_size = 16) 
          
          ggsave(str_c(figure_folder, "Weeks to Find Job Actual v Estimated.pdf"),
                 height = height, width = width)
          
          temp_plot <- df %>% 
            filter(act < quantile(act, .99), act != est) %>% 
            ggplot() +
            geom_density(aes(est, fill = "red"), alpha = 0.2, show.legend = T) +
            geom_density(aes(act, fill = "blue"), alpha = 0.2, show.legend = T) +
            labs(x = "Weeks to Find Job", y = "Density") +
            scale_fill_manual(name = "Weeks to\nFind Job", breaks = c("blue", "red"),
                              values = c("blue", "red"),
                              labels = c("Actual", "Estimated")) +
            scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
            theme_light(base_size = 16) 
          
          ggsave(str_c(figure_folder, 
                       "Weeks to Find Job Actual v Estimated Distribution.pdf"),
                 height = height, width = width)
        }
      }
    }
  }

  t_center <- cbind(center, c_i, 
    rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\[2pt]",
          "\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]")
  )
  
  # Do weird stuff to create LaTeX output
  r_folder <- str_c(table_folder, "Junk/")
  file_1 <- str_c(r_folder, "center.txt")
  write.table(t_center, file_1, quote=T, col.names=F, row.names=F)
  t_center <- read.table(file_1, sep = "")
  write.table(t_center, file_1, quote=F, col.names=F, row.names=F, sep = "")
  t_center <- readChar(file_1, nchars = 1e6)
  
  bot <- str_c(
    "\\bottomrule
  \\end{tabular}
  }
  \\caption{Regressions of outsourced at current and previous job on 
  ", desc, " in the NLSY. All regressions
  include controls for current and previous job type (reported coefficients are 
  compared to traditional jobs), a quartic in age, dummies for year started current 
  job, dummies for region,
  whether in an MSA or central city, marital status, and number of children total
  and in household. The first three columns 
  run OLS and also contain controls for race and education. The last three columns
  use worker fixed effects. Regression with job info contain current and previous
  hours worked per week, part-time status, log real weekly wage, union status, and
  whether received health insurance, retirement benefits, or any benefits. They also
  contain a quartic of previous tenure. Regressions with occupation fixed effects
  contain fixed effects for current and previous occupation.
  All observations are at the person-job level and regressions are weighted
  at the person level. All standard errors are clustered by demographic sampling group.
  Stars represent significant at the .10 level *, .05 level **, and .01 level ***.}
  \\label{reg", label, "}
  \\end{table}"
  )
  
  write.table(str_c(table_top, siunitx, top, t_center, bot, "\n \\end{document}"),
              str_c(table_folder, 
                    "NLSY79 Job Transitions/", save, " Regressions.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save to Drafts (Use s_table_top to resize) 
  write.table(str_c(s_table_top, top, t_center, bot),
              str_c(d_table_folder, save, " Regressions.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
}

# Save last regressions of both vars to Drafts and Slides

s_center <- cbind(center, c_s, 
                  rbind("\\\\", "\\\\[2pt]", "\\\\", "\\\\[2pt]", "\\\\[2pt]",
                        "\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]", "\\\\[2pt]")
)

bot <- "\\bottomrule
  \\end{tabular}
  }
  \\caption{Regressions of outsourced at current and previous job on 
  weeks to find current job (both overall and conditional on taking more than
  one week) and probability of job to job transition in the NLSY. Each regression 
  contains current and previous job variables: job type (reported coefficients are 
  compared to traditional jobs), fixed effects for occupation, hours worked per week, 
  part-time status, log real weekly wage, union status, and
  whether received health insurance, retirement benefits, or any benefits. They also
  contain a quartic of previous tenure and a dummy for year current job began.
  They contain demographic variables:
  a quartic in age, dummies for region, whether in an MSA or central city,
  marital status, and number of children total and in household.
  All observations are at the person-job level and regressions are weighted
  at the person level. All standard errors are clustered by demographic sampling group.
  Stars represent significant at the .10 level *, .05 level **, and .01 level ***.}
  \\label{reg_transition_comp}
  \\end{table}"

# Do weird stuff to create LaTeX output
r_folder <- str_c(table_folder, "Junk/")
file_1 <- str_c(r_folder, "center.txt")
write.table(s_center, file_1, quote=T, col.names=F, row.names=F)
s_center <- read.table(file_1, sep = "")
write.table(s_center, file_1, quote=F, col.names=F, row.names=F, sep = "")
s_center <- readChar(file_1, nchars = 1e6)

write.table(str_c(d_table_top, s_top, s_center, bot),
            str_c(d_table_folder, "Job Transition Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Too long for slide, don't resize
write.table(str_c(d_table_top, s_top, s_center, s_bot),
            str_c(s_table_folder, "Job Transition Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Regressions of Past Job Type on Outcomes --------------------------------

# Run full regression (see matched anaylsis) but replace current job 
# type with previous job type

var_r <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week", "part_time",
           "job_sat", "any_benefits", "health")

types <- c("outsourced", "self_emp", "indep_con", "temp_work", "on_call")

controls <- c("age_curr", "I((age_curr)^2)", "I((age_curr)^3)", "I((age_curr)^4)",
              "tot_child_curr", "hh_child_curr", "factor(union_fill_curr)")

hours <- c("hours_week_curr", "part_time_curr")

tenure <- c("I((tenure_curr/100))", "I((tenure_curr/100)^2)", "I((tenure_curr/100)^3)",
            "I((tenure_curr/100)^4)")

fixed_effects <- c("region", "marital_status", "msa")

center <- rbind("Log Real", "Hourly Wages", "Log Real", "Weekly Wages",
                "Hours Worked", "Per Week", "Part-Time", "",
                "Job Satisfaction", "(Lower Better)", 
                "Any Benefits", "", "Health", "Insurance")

top <- "\\begin{tabular}{lSSS|SSS}
\\toprule
Outcome & {Outsourced Currently} & {$R^2$}  & {Observations} & {Outsourced Previously} 
& {$R^2$}  & {Observations} \\\\\\midrule \n"

# For Slides, just show previous regression
top_s <- "\\begin{tabular}{lSSS}
\\toprule
Outcome & {Outsourced Previously} & {$R^2$}  & {Observations} \\\\\\midrule \n"

c_curr <- c()
c_prev <- c()

for (ind in seq_along(var_r)) {
  var <- var_r[ind]
  
  ind_vars <- c(controls, tenure)
  fe_vars <- c(str_c(fixed_effects, "_curr"), "case_id", "occ_curr",
               "I(year(week_start_job_curr))", "I(year(week_end_job_curr))")
  
  hours_text <- "" 
  if (!(var %in% c("log_real_wkly_wage", "health", "any_benefits",
                   "hours_week", "part_time"))){
    ind_vars %<>% c(hours)
    hours_text <- " hours worked per week, part-time status,"
  }
  
  # Run equation using both current and previous job type
  ind_vars_curr <- c(str_c(types, "_curr"), ind_vars)
  ind_vars_prev <- c(str_c(types, "_prev"), ind_vars)
  
  eq_curr <- create_formula(str_c(var, "_curr"), ind_vars_curr)
  eq_prev <- create_formula(str_c(var, "_curr"), ind_vars_prev)
  fe <- create_formula("", fe_vars)
  
  # Run regression both for matched and matched_jobs
  temp_curr <- lm_robust(eq_curr, data = transition, weights = weight,
                         subset = !is.na(region_curr) & !is.na(marital_status_curr)
                         & !is.na(msa_curr) & !is.na(occ_curr) & !is.na(outsourced_curr)
                         & !is.na(outsourced_prev) & (emp_id_curr >= 100),
                         fixed_effects = !!fe,
                         clusters = as_factor(sample_id),
                         se_type = "stata", try_cholesky = T)
  
  stars <- p_stars(temp_curr$p.value["outsourced_curr"])
  
  c_curr %<>% rbind(
    cbind(format_val(temp_curr$coefficients["outsourced_curr"], r=3, s=3, star = stars),
          format_val(temp_curr$r.squared), format_n(lm_N(temp_curr))),
    cbind(format_se(temp_curr$std.error["outsourced_curr"], r=3, s=3),
          " & ", " & ")
  )
  
  temp_prev <- lm_robust(eq_prev, data = transition, weights = weight,
                         subset = !is.na(region_curr) & !is.na(marital_status_curr)
                         & !is.na(msa_curr) & !is.na(occ_curr) & !is.na(outsourced_curr)
                         & !is.na(outsourced_prev) & (emp_id_curr >= 100),
                         fixed_effects = !!fe,
                         clusters = as_factor(sample_id),
                         se_type = "stata", try_cholesky = T)
  
  stars <- p_stars(temp_prev$p.value["outsourced_prev"])
  
  c_prev %<>% rbind(
    cbind(format_val(temp_prev$coefficients["outsourced_prev"], r=3, s=3, star = stars),
          format_val(temp_prev$r.squared), format_n(lm_N(temp_prev)), "\\\\"),
    cbind(format_se(temp_prev$std.error["outsourced_prev"], r=3, s=3),
          " & ", " & ", "\\\\[2pt]")
  )
}

center_s <- cbind(center, c_prev)
center %<>% cbind(c_curr, c_prev)

bot <- "\\bottomrule
     \\end{tabular}
     }
     \\caption{Regressions of worker outsourcing status on job outcomes in the NLSY.
     All regressions include controls for job type (traditional job is default) 
     in current (left three columns) or previous (right three columns) job. 
     Additional controls are worker and occupation fixed effects, a quartic in age
     and tenure, dummies for year started and ended job, union status, dummies for region, 
     whether in an MSA or central city, marital status,
     and number of children total and in household. 
     Regressions for log real hourly wages and job satisfaction also
     include controls for hours worked per week and part-time status.
  All observations are at the person-job level, 
  where jobs observed more than once use average or modal characteristics. 
  All regressions are weighted at the person level and all standard errors are
  clustered by demographic sample.
  Stars represent significant at the .10 level *, .05 level **, and .01 level ***.}
  \\label{regs_curr_prev}
  \\end{table}"

# Do weird stuff to create LaTeX output
r_folder <- str_c(table_folder, "Junk/")
file_1 <- str_c(r_folder, "center.txt")
write.table(center, file_1, quote=T, col.names=F, row.names=F)
center <- read.table(file_1, sep = "")
write.table(center, file_1, quote=F, col.names=F, row.names=F, sep = "")
center <- readChar(file_1, nchars = 1e6)

# For Slides, just save Previous
r_folder <- str_c(table_folder, "Junk/")
file_1 <- str_c(r_folder, "center.txt")
write.table(center_s, file_1, quote=T, col.names=F, row.names=F)
center_s <- read.table(file_1, sep = "")
write.table(center_s, file_1, quote=F, col.names=F, row.names=F, sep = "")
center_s <- readChar(file_1, nchars = 1e6)

# Save to Folder and Drafts
write.table(str_c(table_top, siunitx, top, center, bot, "\n \\end{document}"), 
            str_c(table_folder, 
                  "NLSY79 Job Transitions/Current and Previous Outsourced Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

write.table(str_c(s_table_top, top, center, bot),
            str_c(d_table_folder, "Current and Previous Outsourced Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save just the previous regression to slides
write.table(str_c(s_table_top, top_s, center_s, s_bot),
            str_c(s_table_folder, "Previous Outsourced Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")