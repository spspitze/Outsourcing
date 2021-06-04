# This file takes data from matched_transition
# to compute summary statistics, run regressions, and make graphs

rm(list = ls())

library(outsourcing)
library(zeallot)
library(weights)
library(data.table)
library(estimatr)
library(data.table)
library(srvyr)
library(lubridate)
library(tidyverse)

# Folders of interest
folders <- name_folders("NLSY 79 Transition")
c(raw_folder, clean_folder, table_folder, figure_folder,
  d_table_folder, s_table_folder) %<-% folders 

# For saving graphs
c(height, width) %<-% fig_size()

# Bottom of slide tables never changes, so make it once
s_bot <- make_bot(slide = TRUE)

# Download the data
transition <- read.csv(str_c(clean_folder, "matched_transition.csv"))

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

# Create a function that appends all variable names with 
# each of a vector of given suffixes
add_suffix <- function(var_vec, suffix_vec, sep = "_") {
  return_vec <- c()
  for (var in var_vec) {
    for (suffix in suffix_vec) {
      return_vec <- append(return_vec, str_c(var, suffix, sep = sep))
    }
  }
  return(return_vec)
}

# Rename union_fill to make matching easier
# Create wj_prev/next that sets 1 week to NA
# Create jj_transition if weeks_job_prev = 1
# Create same occ/ind prev/next
transition <- transition |>
  rename(
    u_f_curr = union_fill_curr, 
    u_f_prev = union_fill_prev,
    u_f_next = union_fill_next
  ) |>
  mutate(
    wj_prev = ifelse(weeks_job_prev > 1, weeks_job_prev, NA),
    wj_next = ifelse(weeks_job_next > 1, weeks_job_next, NA),
    jj_prev = 1 * (weeks_job_prev == 1),
    jj_next = 1 * (weeks_job_next == 1),
    same_occ_prev = (occ_curr == occ_prev),
    same_ind_prev = (ind_curr == ind_prev),
    same_occ_next = (occ_curr == occ_next),
    same_ind_next = (ind_curr == ind_next)
  )

# Create transition_long, which switches data from
# wide to long format. Useful for summary statistics and 
# regression. 
# Include variables
# 1. Marking if current job is outsourced or traditional
# 2. If the transition between previous and current job
# was in and out of an outsourced or traditional job.
# 3. If transition from previous job to current job
# was a job-to-job transition (jj == 1)
vars_to_long <- c(
  "emp_id", "occ", "ind", "pbs", "tenure", "max_tenure",
  "week_start_job", "week_end_job", "log_real_hrly_wage", 
  "log_real_wkly_wage", "hours_week", "part_time", "union", "u_f", 
  "job_sat", "any_benefits", "health", "retirement", "self_emp",
  "same_occ", "same_ind", "weeks_job", "wj", "jj", "hh_child",
  "marital_status", "msa", "region", "tot_child", "less_hs",
  "hs", "aa", "ba", "plus_ba", "educ_other", "age",
  "outsourced_per", "ho_occ", "outsourced_wage_above_per",
  "first_week", "last_week", "outsourced", "traditional",
  "self_emp", "indep_con", "on_call", "temp_work")

transition_long <- transition |>
  mutate(
    curr_trad = (traditional_curr == 1),
    curr_out = (outsourced_curr == 1),
    trans_type = 
      case_when((traditional_prev == 1) 
                & (traditional_curr == 1) ~ "trad_trad",
                (traditional_prev == 1) 
                & (outsourced_curr == 1) ~ "trad_out",
                (outsourced_prev == 1) 
                & (traditional_curr == 1) ~ "out_trad",
                (outsourced_prev == 1) 
                & (outsourced_curr == 1) ~ "out_out",
                TRUE ~ "other"),
    prev_curr_jj = jj_prev
  ) |>
  pivot_longer(starts_with(vars_to_long), 
               names_to = c(".value", "job_time"),
               names_pattern = "(.*)_(.*)")

# Summary Statistics ------------------------------------------------------

vars_sum_all <- c("log_real_hrly_wage", "log_real_wkly_wage",
                  "hours_week", "part_time", "union", "u_f", 
                  "job_sat", "any_benefits", "health", 
                  "retirement", "self_emp", "indep_con", "on_call",
                  "temp_work", "traditional")

vars_sum_np <- c("same_occ", "same_ind", "outsourced", 
                 "weeks_job", "wj", "jj")

vars_mean <- c(vars_sum_all, vars_sum_np, "n")

# Summary Statistics
transition_summary <- split_data(transition_long)
comp <- split_data(transition_long)

for (i in 1:2) { 
  transition_summary[[i]] <- transition_summary[[i]] |>
    filter(!is.na(outsourced), 
           (curr_out == TRUE | curr_trad == TRUE),
           emp_id >= 100) |> 
    as_survey_design(ids = case_id, weights=weight) |>
    group_by(curr_out, job_time) |>
    mutate(n = n()) |> 
    summarise_at(vars_mean, survey_mean, na.rm = T) |> 
    select(-n_se) |> 
    arrange(desc(curr_out), job_time)
  
  comp[[i]] <- comp[[i]] |>
    filter(
      !is.na(outsourced), curr_trad == TRUE | curr_out == TRUE,
      emp_id >= 100)
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
vars_t <- c("log_real_hrly_wage", "log_real_wkly_wage", 
            "hours_week", "part_time", "union", "job_sat",
            "health", "any_benefits")

# Divide variables by mean or percent (they are different below)
vars_t_m <- c("log_real_hrly_wage", "log_real_wkly_wage",
              "hours_week", "job_sat")

vars_t_p <- c("part_time", "union", "health", "any_benefits")

center_r <- rbind("Outsourced", "", "Same", "Occupation", 
                  "Same", "Industry", "Log Real", "Hourly Wage",
                  "Log Real", "Weekly Earnings", "Hours Worked",
                  "Weekly", "Part-Time", "", "Union", "",
                  "Job Satisfaction", "(Lower Better)", "Health",
                  "Insurance", "Any Benefits", "", "Weeks To",
                  "Find Job", "Weeks To", "Find Job ($>1$ week)",
                  "Job-to-Job", "Transition", "Observations")

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
  
  center <- c(center_r)
  
  for (out in c(1, 4)){
    
    i <- out
    i_p <- out + 2
    i_n <- out + 1
    
    # Get comparison data set for testing
    jt <- if (out == 1) "curr_out" else "curr_trad"
    jt_sym <- sym(jt)
    # Compare previous to current
    comp_prev <- comp[[ho]] |>
      filter(!!jt_sym == TRUE, 
             job_time %in% c("curr", "prev"))
    cond_prev <- comp_prev$job_time == "prev"
    # Compare next to current
    comp_next <- comp[[ho]] |>
      filter(!!jt_sym == TRUE, 
             job_time %in% c("curr", "next"))
    cond_next <- comp_next$job_time == "next"
    
    # Start with outsourced
    p_prev <- 
      wtd.t.test(comp_prev$outsourced, 
                 weight = comp_prev$weight)$coefficients["p.value"]
    p_next <- 
      wtd.t.test(comp_next$outsourced, 
                 weight = comp_next$weight)$coefficients["p.value"]
    temp <- cbind(
      rbind(
        format_val(transition_summary[[ho]]$outsourced[i_p], 
                   star = p_stars(p_prev)),
        format_se(transition_summary[[ho]]$outsourced_se[i_p])
      ),
      rbind(
        str_c(" & {", format_it(1 * (out == 1), 3, 0), "}"),
        str_c(" &  ")
      ),
      rbind(
        format_val(transition_summary[[ho]]$outsourced[i_n], 
                   star = p_stars(p_next)),
        format_se(transition_summary[[ho]]$outsourced_se[i_n])
      )
    )
    
    # Now do same occ/ind
    for (var in vars_t_o){
      var_n <- str_c("same", var, sep = "_")
      se_n <- str_c("same", var, "se", sep = "_")
      temp <- rbind(
        temp,
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
      type <- if (var %in% vars_t_p) "prop" else "mean"
      p_star_p <- 
        diff_test(comp_prev, var, "weight", type, 
             cond_prev, !cond_prev, "job_time")
      p_star_n <- 
        diff_test(comp_next, var, "weight", type, 
             cond_next, !cond_next, "job_time")
      temp <- rbind(
        temp,
        cbind(
          rbind(
            format_val(transition_summary[[ho]][[var]][i_p], 
                       star = p_star_p),
            format_se(transition_summary[[ho]][[se]][i_p])
          ),
          rbind(
            format_val(transition_summary[[ho]][[var]][i]),
            format_se(transition_summary[[ho]][[se]][i])
          ),
          rbind(
            format_val(transition_summary[[ho]][[var]][i_n], 
                       star = p_star_n),
            format_se(transition_summary[[ho]][[se]][i_n])
          )
        )
      )
    }
    
    # Weeks to find job (raw and >1), jj transitions, 
    # and n observations
    temp <- rbind(
      temp,
      cbind(
        rbind(
          format_val(transition_summary[[ho]]$weeks_job[i_p]),
          format_se(transition_summary[[ho]]$weeks_job_se[i_p],
                    r = 2, s = 2),
          format_val(transition_summary[[ho]]$wj[i_p]),
          format_se(transition_summary[[ho]]$wj_se[i_p], 
                    r = 2, s = 2),
          format_val(transition_summary[[ho]]$jj[i_p]),
          format_se(transition_summary[[ho]]$jj_se[i_p], 
                    r = 2, s = 2),
          " & "),
        rbind(" & {--}", " & ", " & {--}", " & ", " & {--}", " & ",
              format_n(transition_summary[[ho]]$n[i])),
        rbind(
          format_val(transition_summary[[ho]]$weeks_job[i_n]),
          format_se(transition_summary[[ho]]$weeks_job_se[i_n], 
                    r = 2, s = 2),
          format_val(transition_summary[[ho]]$wj[i_n]),
          format_se(transition_summary[[ho]]$wj_se[i_n],
                    r = 2, s = 2),
          format_val(transition_summary[[ho]]$jj[i_n]),
          format_se(transition_summary[[ho]]$jj_se[i_n],
                    r = 2, s = 2),
          " & ")
        )
      )
    
    if (out == 1){
      center <- cbind(center, temp)
    } else{
      center <- center |>
        cbind(temp) |>
        add_endline()
    }
  }
  
  # Keep only certain rows/columns for Slides
  s_center <- center[c(1:6, 23:28), c(1:2, 4:5, 7:8)]
  # Put observations back
  s_center <- rbind(
    s_center,
    cbind(
      "Observations ",
      str_c("& \\multicolumn{2}{c}{{",
            transition_summary[[ho]]$n[1], "}}"),
      "", 
      str_c("& \\multicolumn{2}{c}{{",
            transition_summary[[ho]]$n[4], "}}"),
      "", "\\\\")
  ) 
  
  center <- center_to_latex(center)
  s_center <- center_to_latex(s_center)
  
  name <- "Summary Statistics for Previous, Current, and Next Jobs"
  label <- str_c("jobs_transitions", occ_label)
  note <- str_c(
  "Job summary statistics in the NLSY", occ_name,
  " at previous, current, and next job for workers who are currently 
  outsourced compared to those who are currently in traditional jobs.
  Observations are at the person-job level and summary statistics 
  are weighted at the person level. Stars represent significance
  difference from current job (except for outsourced which 
  represents significance difference from 0) 
  at the .10 level *, .05 level **, and .01 level ***.
  "
  )
  
  header <- make_header("", name, label)
  d_header <- make_header("d", name, label)
  s_header <- make_header("s")
  
  bot <- make_bot(note)
  
  write.table(
    str_c(header, top, center, bot, "\n \\end{document}"),
    str_c(table_folder,
          "NLSY79 Job Transitions/Summary Statistics", 
          Occ_name, ".tex"),
    quote=F, col.names=F, row.names=F, sep="")
  
  # Save to Drafts (Use s_table top to resize)
  write.table(
    str_c(d_header, top, center, bot),
    str_c(d_table_folder,
          "Job Transition Summary Statistics", Occ_name, ".tex"),
    quote=F, col.names=F, row.names=F, sep="")
  
  # Save to Slides
  write.table(
    str_c(s_header, s_top, s_center, s_bot),
    str_c(s_table_folder,
          "Job Transition Summary Statistics", Occ_name, ".tex"),
    quote=F, col.names=F, row.names=F, sep="")
}

# Job Quality Before and After Transitions -------------------------------

# Look at job summary statistics before and after transitions by 
# outsourced to traditional and traditional to outsourced

# Summary Statistics By Job Type before and after
# Study 3 data sets, one looking at all workers, 
# one looking at just ho workers, and one
# looking at all workers and dividing by jj transitions
# or not. These two functions will help with this
divide_jj <- function(data, cond) {
  if (cond == TRUE) {
    data <- data |>
      filter(!is.na(prev_curr_jj)) |>
      group_by(prev_curr_jj, .add = TRUE)
  }
  
  data
}

denote_jj <- function(data, cond) {
  if (cond == TRUE) {
    data <- data |>
      mutate(
        jj_trans_type = str_c(trans_type, prev_curr_jj, sep = "_")
      )
  }
  
  data
}

vars_sum_all <- c("log_real_hrly_wage", "log_real_wkly_wage",
                  "hours_week", "part_time", "union", "job_sat",
                  "any_benefits", "health", "retirement")

vars_sum_np <- c("same_occ", "same_ind", "weeks_job", "wj", "jj")

vars_mean <- c(vars_sum_all, vars_sum_np, "n")

transition_type <- c(split_data(transition_long), 
                     list(transition_long))

for (i in 1:3) { 
  transition_type[[i]] <- transition_type[[i]] |>
    filter(trans_type != "other", job_time != "next") |>
    as_survey_design(ids = case_id, weights=weight) |>
    group_by(trans_type, job_time) |>
    divide_jj(i == 3) |>
    mutate(n = n()) |> 
    summarise_at(vars_mean, survey_mean, na.rm = T) |>
    select(-n_se) |> 
    # Label job type as T or O for graphing
    mutate(
      curr = ifelse(job_time == "curr", TRUE, FALSE),
      type = 
        case_when(
          (curr == TRUE) 
          & (trans_type %in% c("trad_trad", "out_trad")) ~ 
            "Traditional",
          (curr == TRUE) 
          & (trans_type %in% c("trad_out", "out_out")) ~ 
            "Outsourced",
          (curr == FALSE) & 
            (trans_type %in% c("trad_trad", "trad_out")) ~ 
            "Traditional",
          (curr == FALSE) & 
            (trans_type %in% c("out_out", "out_trad")) ~ 
            "Outsourced"
        ) 
    ) |>
    denote_jj(i == 3)
}

# For outsourced and traditional jobs, see how previous and current
# jobs differ
var_label <- c("Log Real Hourly Wage", "Log Real Weekly Wage", 
               "Hours Worked Per Week", "Part-Time", "Union",
               "Job Satisfaction (Lower Better)", "Any Benefits",
               "Health Insurance", "Retrement Benefits")

var_save <- c("LRH Wage", "LRW Wage", "Hours", "Part Time", "Union",
              "Job Satisfaction", "Benefits", "Health", "Retirement")

ho_save <- c("", " Ever HO Occupation", " by Transition Length")

for (i in seq_along(vars_sum_all)) {
  
  var <- vars_sum_all[i]
  var_sym <- sym(var)
  
  # HO 1/2 graphs look the same
  for (ho in c(1, 2)) {
    temp <- transition_type[[ho]] |> 
      ggplot(aes(x = curr, y = !!var_sym, 
                 color = trans_type, group = trans_type)) +
      # geom_point(size = 4) +
      geom_line(show.legend = FALSE) +
      # Include labels on all points 
      geom_text(aes(label = type, hjust = "outward"), 
                size = 4, check_overlap = TRUE) +
      labs(x = "Job", y = var_label[i]) +
      scale_color_manual(name = "Transition Type", 
                         breaks = c("trad_trad", "trad_out",
                                    "out_trad", "out_out"),
                         values = c("blue", "dark green",
                                    "orange", "red"),
                         labels = c("Traditional-Traditional",
                                    "Traditional-Outsourced",
                                    "Outsourced-Traditional",
                                    "Outsourced-Outsourced")) +
      scale_x_discrete(breaks = c(FALSE, TRUE), 
                       labels = c("Previous Job", "Current Job")) +
      theme_light(base_size = 16) +
      theme(legend.position = "none")
    
    ggsave(str_c(figure_folder, "Job Transition ", 
                 var_save[i], ho_save[ho], ".pdf"),
           height = height, width = width)
    
    # For paper, make some tables custom so all labels can be seen
    # For LRW_Wage
    if (i == 2 & ho == 1) {
      temp <- transition_type[[ho]] |> 
        ggplot(aes(x = curr, y = !!var_sym, 
                   color = trans_type, group = trans_type)) + 
        geom_line(show.legend = FALSE) +
        # Include labels on all points 
        # Previous
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==F & transition_type[[ho]]$trans_type == "trad_out"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "dark green") +
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==F & transition_type[[ho]]$trans_type == "trad_trad"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==F & transition_type[[ho]]$trans_type == "out_out"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red") +
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==F & transition_type[[ho]]$trans_type == "out_trad"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "orange") +
        # Current
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==T & transition_type[[ho]]$trans_type == "trad_trad"],
                      label = "Outsourced", hjust = "outward"), 
                  size = 4, color = "dark green") +
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==T & transition_type[[ho]]$trans_type == "trad_out"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==T & transition_type[[ho]]$trans_type == "out_out"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red") +
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==T & transition_type[[ho]]$trans_type == "out_trad"],
                      label = "Traditional", hjust = "outward"),
                  size = 4, color = "orange") +
        labs(x = "Job", y = var_label[i]) +
        scale_color_manual(name = "Transition Type", 
                           breaks = c("trad_trad", "trad_out",
                                      "out_trad", "out_out"),
                           values = c("blue", "dark green",
                                      "orange", "red"),
                           labels = c("Traditional-Traditional",
                                      "Traditional-Outsourced",
                                      "Outsourced-Traditional",
                                      "Outsourced-Outsourced")) +
        scale_x_discrete(breaks = c(FALSE, TRUE), 
                         labels = c("Previous Job", "Current Job")) +
        theme_light(base_size = 16) +
        theme(legend.position = "none")
      
      ggsave(str_c(figure_folder, "Job Transition ", 
                   var_save[i], ho_save[ho], ".pdf"),
             height = height, width = width)
    }
    
    # For Health
    if (i == 8 & ho == 1) {
      temp <- transition_type[[ho]] |>
        ggplot(aes(x = curr, y = !!var_sym, 
                   color = trans_type, group = trans_type)) + 
        geom_line(show.legend = FALSE) +
        # Include labels on all points 
        # Previous
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==F & transition_type[[ho]]$trans_type == "trad_out"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "dark green") +
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==F & transition_type[[ho]]$trans_type == "trad_trad"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==F & transition_type[[ho]]$trans_type == "out_out"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red") +
        geom_text(aes(x = FALSE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==F & transition_type[[ho]]$trans_type == "out_trad"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "orange") +
        # Current
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==T & transition_type[[ho]]$trans_type == "trad_trad"],
                      label = "Outsourced", hjust = "outward"), 
                  size = 4, color = "dark green") +
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==T & transition_type[[ho]]$trans_type == "trad_out"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==T & transition_type[[ho]]$trans_type == "out_out"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red") +
        geom_text(aes(x = TRUE,
                      y = transition_type[[ho]][[var]][transition_type[[ho]]$curr==T & transition_type[[ho]]$trans_type == "out_trad"],
                      label = "Traditional", hjust = "outward"),
                  size = 4, color = "orange") +
        labs(x = "Job", y = var_label[i]) +
        scale_color_manual(name = "Transition Type", 
                           breaks = c("trad_trad", "trad_out",
                                      "out_trad", "out_out"),
                           values = c("blue", "dark green",
                                      "orange", "red"),
                           labels = c("Traditional-Traditional",
                                      "Traditional-Outsourced",
                                      "Outsourced-Traditional",
                                      "Outsourced-Outsourced")) +
        scale_x_discrete(breaks = c(FALSE, TRUE), 
                         labels = c("Previous Job", "Current Job")) +
        theme_light(base_size = 16) +
        theme(legend.position = "none")
      
      ggsave(str_c(figure_folder, "Job Transition ", var_save[i], ho_save[ho], ".pdf"),
             height = height, width = width)
    }
  }
  
  # HO 3 has different linestyle by job transition
  for (ho in c(3)) {
    temp <- transition_type[[ho]] |> 
      ggplot(aes(x = curr, y = !!var_sym, 
                 color = trans_type, 
                 linetype = factor(prev_curr_jj),
                 group = jj_trans_type)) +
      # geom_point(size = 4) +
      geom_line(show.legend = TRUE) +
      # Include labels on all points 
      geom_text(aes(label = type, hjust = "outward"), 
                size = 4, check_overlap = TRUE) +
      labs(x = "Job", y = var_label[i]) +
      scale_linetype_manual(
        name = "Transition Length",
        breaks = c(1, 0), values = c("solid", "dashed"),
        labels = c("Job-to-Job (1 Week)",
                   "Unemployment ($>$1 Week)")) +
      scale_color_manual(name = "Transition Type", 
                         breaks = c("trad_trad", "trad_out",
                                    "out_trad", "out_out"),
                         values = c("blue", "dark green",
                                    "orange", "red"),
                         labels = c("Traditional-Traditional",
                                    "Traditional-Outsourced",
                                    "Outsourced-Traditional",
                                    "Outsourced-Outsourced"),
                         guide = FALSE) +
      scale_x_discrete(breaks = c(FALSE, TRUE), 
                       labels = c("Previous Job", "Current Job")) +
      theme_light(base_size = 16) +
      theme(legend.position="bottom")
    
    ggsave(str_c(figure_folder, "Job Transition ", 
                 var_save[i], ho_save[ho], ".pdf"),
           height = height, width = width)
  }
}

# For Job Transitions, how do they differ for jobs coming from/going to

top <- "\\begin{tabular}{lSSSS}
\\toprule
Current Job & \\multicolumn{2}{c}{{Outsourced}} &  \\multicolumn{2}{c}{{Traditional}} \\\\
Previous Job & {Outsourced} & {Traditional} & {Outsourced} & {Traditional} \\\\  \\midrule
"

trans_types <- c("out_out", "out_trad", "trad_out", "trad_trad")

center_r <- rbind("Same", "Occupation", "Same", "Industry",
                  "Weeks To", "Find Job", "Weeks To",
                  "Find Job ($>1$ week)",
                  "Job-to-Job", "Transition", "Observations")

# Only look at first two summary tables
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
  
  c_r <- c()
  for (trans in trans_types) {
    
    temp_data <- transition_type[[ho]] |>
      filter(curr == FALSE, trans_type == trans) 
    
    # Use make_ss_col from my outsourcing package,
    # but never do testing, so many of these inputs will
    # not be used
    c_c <- make_ss_col(temp_data, 1, vars_sum_np, temp_data,
                       vars_sum_np, TRUE, FALSE, "out")
    c_r <- cbind(c_r, c_c)
  }
  
  center <- center_r |>
    cbind(c_r) |>
    add_endline() |>
    center_to_latex()
  
  name <- "Differences by Job Type Transitions"
  label <- str_c("jobs_types_transitions", occ_label)
  note <- str_c(
    "Job transition summary statistics in the NLSY", occ_name,
    " between previous and current job. Transitions are grouped 
    by workers coming from and currently in outsourced or 
    traditional jobs. Observations are at the person-job 
    level and summary statistics are weighted at the person level."
  )
  
  header <- make_header("", name, label)
  d_header <- make_header("d", name, label)
  s_header <- make_header("s")
  
  bot <- make_bot(note)
  
  write.table(
    str_c(header, top, center, bot, "\n \\end{document}"),
    str_c(
      table_folder,
      "NLSY79 Job Transitions/Type Transition Summary Statistics",
      Occ_name, ".tex"),
    quote=F, col.names=F, row.names=F, sep="")
  
  # Save to Drafts
  write.table(
    str_c(d_header, top, center, bot),
    str_c(d_table_folder,
          "Job Type Transition Summary Statistics", Occ_name, ".tex"),
    quote=F, col.names=F, row.names=F, sep="")
  
  # Save to Slides
  write.table(
    str_c(s_header, top, center, s_bot),
    str_c(s_table_folder,
          "Job Type Transition Summary Statistics", Occ_name, ".tex"),
    quote=F, col.names=F, row.names=F, sep="")
}

# For some outcomes, run regressions that don't know outsourced 
# versus traditional. How do residuals change before and after?
# Run regression on all jobs using current job, then predict 
# residuals for previous jobs. Bug prevents using predict for 
# lm_robust with fe, so regressions will be slow
# Run regressions for all workers, use residuals for
# all workers, HO workers, and all workers by job transition length.

vars_reg <- c("log_real_hrly_wage", "log_real_wkly_wage", 
              "hours_week", "part_time", "job_sat", 
              "any_benefits", "health", "retirement")

var_label <- c("Log Real Hourly Wage", "Log Real Weekly Wage", 
               "Hours Worked Per Week", "Part-Time",
               "Job Satisfaction (Lower Better)", "Any Benefits",
               "Health Insurance", "Retrement Benefits")

var_save <- c("LRH Wage", "LRW Wage", "Hours", "Part Time",
              "Job Satisfaction", "Benefits", "Health", "Retirement")

ho_save <- c("", " Ever HO Occupation", " by Transition Length")

types <- c("self_emp", "indep_con", "temp_work", "on_call")

controls <- c("age", "I((age)^2)", "I((age)^3)", "I((age)^4)",
              "factor(u_f)")

tenure <- c("I((tenure/100))", "I((tenure/100)^2)",
            "I((tenure/100)^3)", "I((tenure/100)^4)")

fixed_effects <- c("factor(region)", "factor(marital_status)",
                   "factor(msa)", "factor(case_id)", "factor(occ)",
                   "factor(I(year(week_start_job)))", 
                   "factor(I(year(week_end_job)))")

for (ind in seq_along(vars_reg)) {
  var <- vars_reg[ind]
  var_sym <- sym(var)
  
  ind_vars <- c(types, controls, tenure)
  eq <- create_formula(var, list(ind_vars, fixed_effects))
  
  reg <- lm_robust(eq, data = transition_long, weights = weight,
                   subset = (job_time == "curr") 
                   & !is.na(region) & !is.na(marital_status)
                   & !is.na(msa) & !is.na(occ) & !is.na(outsourced)
                   & !is.na(tenure) & !is.na(age) & !is.na(u_f)
                   & !is.na(week_start_job) 
                   & !is.na(week_end_job)
                   & (emp_id >= 100),
                   # fixed_effects = !!fe,
                   clusters = as_factor(sample_id),
                   se_type = "stata", try_cholesky = T)
  
  temp_data <- transition_long |>
    filter(trans_type != "other", job_time %in% c("curr", "prev"),
           !is.na(!!var_sym),
           !is.na(region), !is.na(marital_status), 
           !is.na(msa), !is.na(occ),
           !is.na(tenure), !is.na(age), !is.na(u_f),
           !is.na(week_start_job), !is.na(week_end_job),
           (emp_id >= 100))
  
  temp_data <- temp_data |>
    mutate(
      predict = predict(reg, newdata = temp_data),
      residual = !!var_sym - predict
    )
  
  vars_temp <- c("residuals", "n")
  temp_summary <- c(split_data(temp_data), list(temp_data))
  
  for (ho in 1:3) {
    temp_summary[[ho]] <- temp_summary[[ho]] |>
      as_survey_design(ids = case_id, weights=weight) |>
      group_by(trans_type, job_time) |>
      divide_jj(ho == 3) |>
      mutate(n = n()) |> 
      summarise_at(vars_temp, survey_mean, na.rm = T) |> 
      select(-n_se) |> 
      # Label job type as T or O for graphing
      mutate(
        curr = ifelse(job_time == "curr", TRUE, FALSE),
        type = 
          case_when(
            (curr == TRUE) 
            & (trans_type %in% c("trad_trad", "out_trad")) ~ 
              "Traditional",
            (curr == TRUE) 
            & (trans_type %in% c("trad_out", "out_out")) ~ 
              "Outsourced",
            (curr == FALSE) & 
              (trans_type %in% c("trad_trad", "trad_out")) ~ 
              "Traditional",
            (curr == FALSE) & 
              (trans_type %in% c("out_out", "out_trad")) ~ 
              "Outsourced"
        )
      ) |>
      denote_jj(ho == 3)
  }
  
  # Save first two data sets mostly the same
  for (ho in 1:2) {      
    temp <- temp_summary[[ho]] |>  
      ggplot(aes(x = curr, y = residual, 
                 color = trans_type, group = trans_type)) +
      geom_line(show.legend = FALSE) +
      # Include labels on all points 
      geom_text(aes(label = type, hjust = "outward"), size = 4, 
                check_overlap = TRUE) +
      labs(x = "Job", y = str_c(var_label[ind], " Residuals")) +
      scale_color_manual(name = "Transition Type", 
                         breaks = c("trad_trad", "trad_out",
                                    "out_trad", "out_out"),
                         values = c("blue", "dark green",
                                    "orange", "red"),
                         labels = c("Traditional-Traditional",
                                    "Traditional-Outsourced",
                                    "Outsourced-Traditional",
                                    "Outsourced-Outsourced")) +
      scale_x_discrete(breaks = c(FALSE, TRUE), 
                       labels = c("Previous Job", "Current Job")) +
      theme_light(base_size = 16) +
      theme(legend.position = "none")
    
    ggsave(str_c(figure_folder, "Job Transition ", var_save[ind], 
                 " Residuals", ho_save[ho], ".pdf"),
           height = height, width = width)
    
    # For paper, make some figures custom so all labels can be seen
    # For LRW_Wage
    if (ind == 2 & ho == 1) {
      temp <- temp_summary[[ho]] |>  
        ggplot(aes(x = curr, y = residual, 
                   color = trans_type, group = trans_type)) +
        geom_line(show.legend = FALSE) +
        # Include labels on all points 
        # Previous
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$trans_type == "trad_out"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "dark green") +
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$trans_type == "trad_trad"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$trans_type == "out_out"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red") +
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$trans_type == "out_trad"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "orange") +
        # Current
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$trans_type == "trad_out"],
                      label = "Outsourced", hjust = "outward"), 
                  size = 4, color = "dark green", nudge_y = .0015) +
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$trans_type == "trad_trad"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$trans_type == "out_out"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red", nudge_y = -.0015) +
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$trans_type == "out_trad"],
                      label = "Traditional", hjust = "outward"),
                  size = 4, color = "orange") +
        labs(x = "Job", y = str_c(var_label[ind], " Residuals")) +
        scale_color_manual(name = "Transition Type", 
                           breaks = c("trad_trad", "trad_out",
                                      "out_trad", "out_out"),
                           values = c("blue", "dark green",
                                      "orange", "red"),
                           labels = c("Traditional-Traditional",
                                      "Traditional-Outsourced",
                                      "Outsourced-Traditional",
                                      "Outsourced-Outsourced")) +
        scale_x_discrete(breaks = c(FALSE, TRUE), 
                         labels = c("Previous Job", "Current Job")) +
        theme_light(base_size = 16) +
        theme(legend.position = "none")
      
      ggsave(str_c(figure_folder, "Job Transition ", var_save[ind], " Residuals", 
                   ho_save[ho], ".pdf"), height = height, width = width)
    }
    
    # For Health
    if (ind == 7 & ho == 1) {
      temp <- temp_summary[[ho]] |>  
        ggplot(aes(x = curr, y = residual, 
                   color = trans_type, group = trans_type)) +
        geom_line(show.legend = FALSE) +
        # Include labels on all points 
        # Previous
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$trans_type == "trad_out"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "dark green") +
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$trans_type == "trad_trad"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$trans_type == "out_out"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red") +
        geom_text(aes(x = FALSE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==F & temp_summary[[ho]]$trans_type == "out_trad"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "orange") +
        # Current
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$trans_type == "trad_out"],
                      label = "Outsourced", hjust = "outward"), 
                  size = 4, color = "dark green") +
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$trans_type == "trad_trad"],
                      label = "Traditional", hjust = "outward"), 
                  size = 4, color = "blue") +
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$trans_type == "out_out"],
                      label = "Outsourced", hjust = "outward"),
                  size = 4, color = "red") +
        geom_text(aes(x = TRUE,
                      y = temp_summary[[ho]]$residual[temp_summary[[ho]]$curr==T & temp_summary[[ho]]$trans_type == "out_trad"],
                      label = "Traditional", hjust = "outward"),
                  size = 4, color = "orange") +
        labs(x = "Job", y = str_c(var_label[ind], " Residuals")) +
        scale_color_manual(name = "Transition Type", 
                           breaks = c("trad_trad", "trad_out",
                                      "out_trad", "out_out"),
                           values = c("blue", "dark green",
                                      "orange", "red"),
                           labels = c("Traditional-Traditional",
                                      "Traditional-Outsourced",
                                      "Outsourced-Traditional",
                                      "Outsourced-Outsourced")) +
        scale_x_discrete(breaks = c(FALSE, TRUE), 
                         labels = c("Previous Job", "Current Job")) +
        theme_light(base_size = 16) +
        theme(legend.position = "none")
      
      ggsave(str_c(figure_folder, "Job Transition ", var_save[ind], " Residuals", 
                   ho_save[ho], ".pdf"), height = height, width = width)
    }
  }
  
  # Save the third data set with transition length denoted by
  # linetype
  for (ho in c(3)) {
    temp <- temp_summary[[ho]] |>  
      ggplot(aes(x = curr, y = residual, 
                 color = trans_type, 
                 linetype = factor(prev_curr_jj),
                 group = jj_trans_type)) +
      geom_line(show.legend = TRUE) +
      # Include labels on all points 
      geom_text(aes(label = type, hjust = "outward"), size = 4, 
                check_overlap = TRUE) +
      labs(x = "Job", y = str_c(var_label[ind], " Residuals")) +
      scale_linetype_manual(
        name = "Transition Length",
        breaks = c(1, 0), values = c("solid", "dashed"),
        labels = c("Job-to-Job (1 Week)",
                   "Unemployment ($>$1 Week)")) +
      scale_color_manual(name = "Transition Type", 
                         breaks = c("trad_trad", "trad_out",
                                    "out_trad", "out_out"),
                         values = c("blue", "dark green",
                                    "orange", "red"),
                         labels = c("Traditional-Traditional",
                                    "Traditional-Outsourced",
                                    "Outsourced-Traditional",
                                    "Outsourced-Outsourced"),
                         guide = FALSE) +
      scale_x_discrete(breaks = c(FALSE, TRUE), 
                       labels = c("Previous Job", "Current Job")) +
      theme_light(base_size = 16) +
      theme(legend.position="bottom")
    
    ggsave(str_c(figure_folder, "Job Transition ", var_save[ind], 
                 " Residuals", ho_save[ho], ".pdf"),
           height = height, width = width)
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

comp_w <- filter(comp[[2]], job_time == "prev") 
cond_w <- comp_w$curr_trad == TRUE

table <- str_c(
  table,
  format_val(transition_summary[[2]]$weeks_job[3]),
  format_val(transition_summary[[2]]$weeks_job[6],
             star = diff_test(comp_w, "weeks_job", "weight", "mean",
                    cond_w, !cond_w)),
  format_val(transition_summary[[2]]$wj[3]),
  format_val(transition_summary[[2]]$wj[6], 
             star = diff_test(comp_w, "wj", "weight", "mean",
                          cond_w, !cond_w)), "\\\\ \n",
  format_se(transition_summary[[2]]$weeks_job_se[3]),
  format_se(transition_summary[[2]]$weeks_job_se[6]),
  format_se(transition_summary[[2]]$wj_se[2]),
  format_se(transition_summary[[2]]$wj_se[5]),
  "\\\\ \n")

name <- "Mean Weeks to Find Job by Current Job Type"
label <- "weeks_find_job"
note <- "
Mean weeks to find current job for workers in high outsourcing
occupations. Time between jobs is averaged both for all transitions 
and for transitions that last longer than 1 week. 
Stars represent significant difference from outsourced jobs at 
the .10 level *, .05 level **, and .01 level ***."

header <- make_header("", name, label)
bot <- make_bot(note)

# Save only for myself for now
write.table(str_c(header, table, bot, "\n \\end{document}"), 
  str_c(table_folder, 
        "NLSY79 Job Transitions/Weeks to Find Job HO Occupations.tex"),
  quote=F, col.names=F, row.names=F, sep="")

# # Save in Drafts and Slides
# write.table(str_c(d_header, table, bot),
#             str_c(d_table_folder, 
#                   "Weeks to Find Job HO Occupations.tex"),
#             quote=F, col.names=F, row.names=F, sep="")
# 
# write.table(str_c(s_header, table, s_bot),
#             str_c(s_table_folder, 
#                   "Weeks to Find Job HO Occupations.tex"),
#             quote=F, col.names=F, row.names=F, sep="")

# Plot figures 
var_g <- c("weeks_job", "wj")
var_time <- c("prev", "next")
var_save_t <- c("Current", "Next")
var_axis_g <- c("", " (>1 Week)")
var_save_g <- c("", " G1")

ho_save <- c("", " HO Occupations")
trans_plot <- split_data(transition_long)

for (ho in 1:2) {
  for (t in seq_along(var_time)) {
    
    time <- var_time[t]
    
    for (i in seq_along(var_g)) {
    
      var <- var_g[i]
      var_sym <- sym(var)
      axis <- str_c("Weeks to Find ", var_save_t[t], 
                    " Job", var_axis_g[i])
      save <- str_c("Weeks to Find ", var_save_t[t], 
                    " Job", var_save_g[i], ho_save[ho])
        
      # Change row searching for mean based on prev or next 
      n_o <- if (i <= 2) 3 else 2
      n_t <- if (i <= 2) 6 else 5
      
      # Drop the top 5\% of longest wait times
      temp <- trans_plot[[ho]] |>
        filter(!is.na(!!var_sym), 
               !!var_sym <= quantile(!!var_sym, .95, na.rm = TRUE),
               !is.na(outsourced),
               job_time == time,
               (curr_out == 1 | curr_trad == 1)) |>
        ggplot(aes(!!var_sym, fill = factor(curr_out))) +
        geom_density_bounds(alpha = 0.2, bounds = c(i, Inf),
                            color = "black") +
        geom_vline(
          aes(xintercept = transition_summary[[ho]][[var]][n_o]),
          color = "red", size=1) +
        geom_vline(
          aes(xintercept = transition_summary[[ho]][[var]][n_t]),
          color = "blue", size=1) +
        labs(x = axis, y = "Density") +
        scale_fill_manual(name = "Outsourced", 
                          breaks = c(FALSE, TRUE),
                          values = c("blue", "red"),
                          labels = c("Traditional", "Outsourced")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        theme_light(base_size = 16)
      
      ggsave(str_c(figure_folder, save, ".pdf"),
             height = height, width = width)
    }
  }
}

# Update data_moments with median wj_prev (no job-job transitions)
# for all ever_ho_occ workers to any job
# weeks_to_job_ss <- transition |> 
#   filter(!is.na(outsourced_curr), 
#          (outsourced_curr == 1 | traditional_curr == 1)) |> 
#   as_survey_design(ids = case_id, weights=weight) |> 
#   summarise(
#     median_wj_prev = survey_median(wj_prev, na.rm = T),
#     mean_wj_prev = survey_mean(wj_prev, na.rm = T))
# 
# WBJ <- weeks_to_job_ss$median_wj_prev[1]
# UE_2 <- 1 - .5 ^(1 / WBJ)
# 
# data_moments <- update_parameters("WeeksBetweenJobs", WBJ)
# 
# write_csv(data_moments, str_c(clean_folder, "data_moments.csv"))
# 
# # Plot to see how reasonable this looks
# max_w <- 75
# plot <- trans_plot[[2]] |>
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
#   geom_density_bounds(aes(x = plot$weeks_job_prev, fill = "Data"),
#                  alpha = 0.2, bounds = c(0, max_w), color = "black) +
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
# For now, assume all 1 week job gaps are job-job transitions 
# (marked as jj)


# Regression on Weeks to Jobs ---------------------------------------------

top <- "\\begin{tabular}{lSSSSSS}
\\toprule
&  \\multicolumn{3}{c}{{OLS}} & \\multicolumn{3}{c}{{FE}} \\\\
Variables & {Basic}  & {Occ FE} & {Job Info} & {Basic} & {Occ FE} & {Job Info}  \\\\\\midrule
"

# Create an s_top with just the occ fe (but not job characteristics)
# for both weeks to job and weeks to job == 1
s_top <- "\\begin{tabular}{lSSS}
\\toprule
Variables & {Weeks to Job} & {Weeks to Job ($>1$)} & {Job-Job Transition} \\\\\\midrule
"

# Type of job (compare to traditional)
types <- c("outsourced", "self_emp", "indep_con", 
           "temp_work", "on_call")

# Demographic Controls
dem_controls <- c("age_curr", "I(age_curr^2)", 
                  "I(age_curr^3)", "I(age_curr^4)")

# Current job control
job_controls <- c("hours_week", "part_time", "u_f",
                  "health", "retirement",
                  "any_benefits","log_real_wkly_wage")

# Tenure controls from previous job (/100 to keep small)
tenure_controls <- c(
  "I(tenure_prev / 100)", "I((tenure_prev / 100)^2)",
  "I((tenure_prev / 100)^3)", "I((tenure_prev / 100)^4)")

# OLS controls
ols_controls <- c("black", "hispanic", "hs_curr", 
                  "aa_curr", "ba_curr", "plus_ba_curr")

# Fixed Effects for all regressions
fixed_effects <- c("region_curr", "marital_status_curr", "msa_curr", 
                   "I(year(week_start_job_curr))")

# Occupation fixed effects
occ_effects <- c("occ_curr", "occ_prev")

# Individual fixed effects
id_effects <- c("case_id")

vars_1 <- list(add_suffix(types, c("curr", "prev")), dem_controls)
vars_2 <- 
  append(vars_1, 
         list(add_suffix(job_controls, c("curr", "prev")), 
              tenure_controls))
var_list <- list(vars_1, vars_1, vars_2)

job_t <- c("No", "No", "Yes")
occ_t <- c("No", "Yes", "Yes")
worker_t <- c("No", "Yes")

center <- rbind("Outsourced", "Current", "Outsourced", 
                "Previous", "Job Info", "Occupation FE", 
                "Worker FE", "$R^2$", "Obs")

vars <- c("weeks_job_prev", "wj_prev", "I(weeks_job_prev == 1)")
var_save <- c("Weeks to Find Job", "Weeks to Find Job G1",
              "Job to Job Transition")
labels <- c("_weeks_between_jobs", "_weeks_between_jobs_g1",
            "_job_to_job")
descriptions <- c("weeks to find current job", 
                  "weeks to find current job (>1 week)",
                  "probability of job to job transition")

c_s <- c()

for (k in 1:length(vars)) {

  c_i <- c()
  var <- vars[k]
  save <- var_save[k]
  lab <- labels[k]
  desc <- descriptions[k]
  
  for (j in 1:2){
    for (i in 1:3) {
      
      controls <- var_list[[i]]
      fes <- fixed_effects
      
      if (i > 1) {
        fes <- append(fes, occ_effects)
      }
      
      if (j == 1) {
        controls <- append(controls, ols_controls)
      } else {
        fes <- append(fes, id_effects)
      }
      
      eq <- create_formula(var, controls)
      fe <- create_formula("", fes)
      
      temp <- lm_robust(
        eq, data = transition, 
        subset = (!is.na(marital_status_curr)
                  & !is.na(msa_curr) & !is.na(occ_curr) 
                  & !is.na(occ_prev)),
        weights = weight,
        fixed_effects = !!fe,
        clusters = as_factor(sample_id),
        se_type = "stata", try_cholesky = T)
      
      # Put regression results in Tables
      c_i <- cbind(
        c_i,
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
      
      # If regression with worker + occ FE (but not
      # job controls) save to c_s
      if (i == 2 & j == 2) {
        
        c_s <- cbind(
          c_s,
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
          
          temp_plot <- df |> 
            filter(act != est) |> 
            ggplot() +
            geom_abline(slope = 1, intercept = 0, color = "blue") +
            geom_jitter(aes(est, act)) +
            labs(x = "Estimated Weeks to Find Job", 
                 y = "Actual Weeks to Find Job") +
            theme_light(base_size = 16) 
          
          ggsave(str_c(figure_folder, 
                       "Weeks to Find Job Actual v Estimated.pdf"),
                 height = height, width = width)
          
          temp_plot <- df |> 
            filter(act != est) |> 
            ggplot() +
            geom_density(aes(est, fill = "red"), 
                         alpha = 0.2, show.legend = T) +
            geom_density(aes(act, fill = "blue"), 
                         alpha = 0.2, show.legend = T) +
            labs(x = "Weeks to Find Job", y = "Density") +
            scale_fill_manual(name = "Weeks to\nFind Job", 
                              breaks = c("blue", "red"),
                              values = c("blue", "red"),
                              labels = c("Actual", "Estimated")) +
            scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
            theme_light(base_size = 16) 
          
          ggsave(
            str_c(
              figure_folder, 
              "Weeks to Find Job Actual v Estimated Distribution.pdf"),
            height = height, width = width)
        }
      }
    }
  }

  t_center <- center |>
    cbind(c_i) |>
    add_endline() |>
    center_to_latex()
  
  name <- "Weeks to Find Current Job Regressions"
  label <- str_c("reg", lab)
  note <- str_c(
    "Regressions of outsourced at current and previous job on ", 
    desc, " in the NLSY. All regressions include controls for
    current and previous job type (reported coefficients are 
    compared to traditional jobs), a quartic in age, dummies 
    for year started current job, dummies for region, whether 
    in an MSA or central city, and marital status. 
    The first three columns run OLS and also contain controls
    for race and education. The last three columns use worker 
    fixed effects. Regressions with occupation fixed effects
    contain fixed effects for current and previous occupation. 
    Regression with job info contain current 
    and previous hours worked per week, part-time status, 
    log real weekly wage, union status, and whether received 
    health insurance, retirement benefits, or any benefits. 
    They also contain a quartic of previous tenure. 
    All observations are at 
    the person-job level and regressions are weighted at the 
    person level. All standard errors are clustered by 
    demographic sampling group. Stars represent significance 
    at the .10 level *, .05 level **, and .01 level ***.
    "
  )
  
  # Only save all regressions for own use
  header <- make_header("", name, label)
  bot <- make_bot(note)
  
  write.table(str_c(header, top, t_center, bot, "\n \\end{document}"),
              str_c(table_folder, "NLSY79 Job Transitions/",
                    save, " Regressions.tex"),
              quote=F, col.names=F, row.names=F, sep="")
}

# Save last regressions of both vars to Drafts and Slides
s_center <- center |>
  cbind(c_s) |>
  add_endline() 

# Remove Job Quality line
s_center <- s_center[-5:-7, ]
  
s_center <- center_to_latex(s_center)

name <- "Weeks to Find Current Job Regressions"
label <- "reg_transition_comp"
note <- "
Regressions of outsourced at current and previous job on weeks to 
find current job (both overall and conditional on the transition 
taking more than one week) and probability of job to job transition
in the NLSY. Each regression contains current and previous job 
variables: job type (reported coefficients are compared to 
traditional jobs), and fixed effects for occupation. 
It also contains a dummy for year current job began. 
They contain the following
demographic variables: a quartic in age, dummies for region, 
whether in an MSA or central city, and marital status.
All observations are at the person-job level and regressions 
are weighted at the person level. All standard errors are 
clustered by demographic sampling group. Stars represent 
significance at the .10 level *, .05 level **, and .01 level ***.
"

# Save to Drafts and Slides
d_header <- make_header("d", name, label)
s_header <- make_header("s")

bot <- make_bot(note)

write.table(str_c(d_header, s_top, s_center, bot),
            str_c(d_table_folder, "Job Transition Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

write.table(str_c(s_header, s_top, s_center, s_bot),
            str_c(s_table_folder, "Job Transition Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Regressions of Past Job Type on Outcomes --------------------------------

# Run full regression (see matched analysis) but replace current job 
# type with previous job type

var_r <- c("log_real_hrly_wage", "log_real_wkly_wage",
           "hours_week", "part_time", "any_benefits",
           "health")

types <- c("outsourced", "self_emp", "indep_con", 
           "temp_work", "on_call")

controls <- c("age_curr", "I((age_curr)^2)", "I((age_curr)^3)",
              "I((age_curr)^4)", "factor(u_f_curr)")

tenure <- c("I((tenure_curr/100))", "I((tenure_curr/100)^2)",
            "I((tenure_curr/100)^3)", "I((tenure_curr/100)^4)")

fixed_effects <- c("region", "marital_status", "msa")

top <- "
\\begin{tabular}{lSSSSSS}
\\toprule
& {Log Real} &  {Log Real} & {Hours Worked} & {} & {Any} & {Health} \\\\
& {Hourly Wages} & {Weekly Wages}  & {Per Week}  & {Part-Time} & {Benefits} & {Insurance}  \\\\\\midrule
"

center_c <- c("Outsourced Currently", "", "$R^2$", "Observations")
center_p <- c("Outsourced Previously", "", "$R^2$", "Observations")

c_curr <- c()
c_prev <- c()

for (ind in seq_along(var_r)) {
  var <- var_r[ind]
  
  ind_vars <- c(controls, tenure)
  fe_vars <- c(str_c(fixed_effects, "_curr"), 
               "case_id", "occ_curr",
               "I(year(week_start_job_curr))", 
               "I(year(week_end_job_curr))")
  
  # Run equation using both current and previous job type
  ind_vars_curr <- c(str_c(types, "_curr"), ind_vars)
  ind_vars_prev <- c(str_c(types, "_prev"), ind_vars)
  
  eq_curr <- create_formula(str_c(var, "_curr"), ind_vars_curr)
  eq_prev <- create_formula(str_c(var, "_curr"), ind_vars_prev)
  fe <- create_formula("", fe_vars)
  
  # Run regression both for matched and matched_jobs
  temp_curr <- 
    lm_robust(eq_curr, data = transition, 
              weights = weight,
              subset = !is.na(region_curr) 
              & !is.na(marital_status_curr)
              & !is.na(msa_curr) & !is.na(occ_curr) 
              & !is.na(outsourced_curr)
              & !is.na(outsourced_prev) & (emp_id_curr >= 100),
              fixed_effects = !!fe,
              clusters = as_factor(sample_id),
              se_type = "stata", try_cholesky = T)
  
  stars <- p_stars(temp_curr$p.value["outsourced_curr"])
  
  c_curr <- cbind(
    c_curr,
    rbind(format_val(temp_curr$coefficients["outsourced_curr"],
                     r=3, s=3, star = stars),
          format_se(temp_curr$std.error["outsourced_curr"],
                    r=3, s=3),
          format_val(temp_curr$r.squared), 
          format_n(lm_N(temp_curr)))
  )
  
  temp_prev <- 
    lm_robust(eq_prev, data = transition, weights = weight,
              subset = !is.na(region_curr) 
              & !is.na(marital_status_curr)
              & !is.na(msa_curr) & !is.na(occ_curr) 
              & !is.na(outsourced_curr)
              & !is.na(outsourced_prev) & (emp_id_curr >= 100),
              fixed_effects = !!fe,
              clusters = as_factor(sample_id),
              se_type = "stata", try_cholesky = T)
  
  stars <- p_stars(temp_prev$p.value["outsourced_prev"])
  
  c_prev <- cbind(
    c_prev,
    rbind(format_val(temp_prev$coefficients["outsourced_prev"], 
                     r=3, s=3, star = stars),
          format_se(temp_prev$std.error["outsourced_prev"],
                    r=3, s=3),
          format_val(temp_prev$r.squared),
          format_n(lm_N(temp_prev)))
    )
}

center_c <- center_c |>
  cbind(c_curr)

center_p <- center_p |>
  cbind(c_prev)

center <- center_c |>
  rbind(center_p) |>
  add_endline(midrule = c(4)) |>
  center_to_latex()

name <- "Effect of Previous Job Type on Current Job Quality"
label <- "regs_curr_prev"
note <- "
Regressions of worker outsourcing status on job outcomes in the NLSY.
All regressions include controls for job type (traditional job 
is default) in current (top regressions) or previous 
(bottom regressions) job. Additional controls are worker and 
occupation fixed effects, a quartic in age and tenure, dummies for 
year started and ended job, union status, dummies for region,
whether in an MSA or central city, and marital status. 
All observations are at the person-job level, where jobs observed 
more than once use average or modal characteristics. 
All regressions are weighted at the person level and 
all standard errors are clustered by demographic sample.
Stars represent significance at the .10 level *, .05 level **, 
and .01 level ***.
"

header <- make_header("", name, label, colsep = 1.5)
d_header <- make_header("d", name, label, colsep = 1.5)
s_header <- make_header("s", colsep = 0.5)

bot <- make_bot(note)

# Save to Folder and Drafts
write.table(
  str_c(header, top, center, bot, "\n \\end{document}"), 
  str_c(table_folder, 
        "NLSY79 Job Transitions/Current and Previous Outsourced Regressions.tex"),
  quote=F, col.names=F, row.names=F, sep="")

# Drafts
write.table(str_c(d_header, top, center, bot),
            str_c(d_table_folder, 
                  "Current and Previous Outsourced Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Slides
write.table(str_c(s_header, top, center, s_bot),
            str_c(s_table_folder, 
                  "Current and Previous Outsourced Regressions.tex"),
            quote=F, col.names=F, row.names=F, sep="")
