# This file takes data from matched_clean and matched_jobs_clean
# to compute summary statistics

rm(list = ls())

library(outsourcing)
library(zeallot)
library(weights)
library(DescTools)
library(dineq)
library(estimatr)
library(data.table)
library(srvyr)
library(tidyverse)

# Folders of interest
folders <- name_folders("NLSY 79 Matched")
c(raw_folder, clean_folder, table_folder, figure_folder,
  d_table_folder, s_table_folder) %<-% folders 

# For saving graphs
c(height, width) %<-% fig_size()

# Bottom of slide tables never changes, so make it once
s_bot <- make_bot(slide = TRUE)

# Download data
matched <- read_csv(str_c(clean_folder, "matched.csv"),
  col_types = cols(
    .default = col_double(),
    week_start_job = col_date(format = ""),
    week_end_job = col_date(format = "")
  )) |> 
  # Create married/single if certain marital status
  mutate(
    single = 1 * (marital_status == 1),
    married = 1 * (marital_status == 2)
  ) |>
  # Separate workers into <ba and >ba (not educ_other)
  mutate(
    high_ed = ifelse(educ_other == 1, NA, ba + plus_ba)
  )

matched_jobs <- read_csv(str_c(clean_folder, "matched_jobs.csv"),
                         col_types = cols(
                           .default = col_double(),
                           week_start_job = col_date(format = ""),
                           week_end_job = col_date(format = "")
                         )) |> 
  # Create married/single if certain marital status
  mutate(
    single = 1 * (marital_status == 1),
    married = 1 * (marital_status == 2)
  ) |>
  # Separate workers into <ba and >ba (not educ_other)
  mutate(
    high_ed = ifelse(educ_other, NA, ba + plus_ba)
  )

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

splits <- c(split_data(matched), split_data(matched_jobs))

# There will be common table notes for access to benefits
# and imputing total compensation. Save those here
note_access <- "Benefits measure if worker reports access to
benefit through employer."

note_tot_comp <- "Total compensation is imputed using log real weekly
wages and access to health insurance and retirement plans. The value
of these benefits is calculated using data from the NCS. 
See Appendix~\\ref{ap:impute_benefits} for more details."

# Demographic Summary Statistics ------------------------------------------------------

# First, look at people who ever outsource vs those 
# who never do (ever_out_oj == 1/0)
# Overall and ever in HO Occupations
# And those ever in HO occupations vs not (ever_ho_occ == 1/0)
# Weight based on population
# Look only at first observation for demographics
vars <- c("female", "black", "hispanic", "less_hs", "hs", "aa", "ba",
          "plus_ba", "n")

vars_ho <- c(vars, "ever_out_oj")

vars <- c(vars, "ever_ho_occ")

sum_demo <- c(split_data(matched), list(matched))
# For running t/p tests
comp <- split_data(matched)

for (i in 1:2) {
  sum_demo[[i]] <- sum_demo[[i]] |>
    group_by(case_id) |> 
    filter(row_number() == min(row_number())) |> 
    as_survey_design(ids = case_id, weights = weight) |> 
    group_by(ever_out_oj) |>
    mutate(n = n()) |> 
    summarise_at(vars, survey_mean, na.rm = T) |> 
    arrange(desc(ever_out_oj)) 
  
  
  comp[[i]] <- comp[[i]] |>
    group_by(case_id) |> 
    filter(row_number() == min(row_number())) 
}

sum_demo[[3]] <- sum_demo[[3]] |>
  group_by(case_id) |> 
  filter(row_number() == min(row_number())) |> 
  as_survey_design(ids = case_id, weights = weight) |> 
  group_by(ever_ho_occ) |>
  mutate(n = n()) |> 
  summarise_at(vars_ho, survey_mean, na.rm = T) |> 
  arrange(desc(ever_ho_occ)) 

# Baseline data same as comp[[1]]
comp <- c(comp, list(comp[[1]]))

# Create table in Latex
vars_p <- c("female", "black", "hispanic", "less_hs", "hs", "aa", "ba",
            "plus_ba")

# Variable names
n_center <- rbind("Female", "", "Black", "", "Hispanic", "",
                  "Less High School", "",
                  "High School", "Diploma", "Associate's", "Degree",
                  "Bachelor's", "Degree", "Postgraduate", "Degree",
                  "Observations")

# Needed to space out table
space <- cbind(rbind(" & {1}", " & "),
               rbind(" & {0}", " & ")
)

for (ho in 1:3) {
  
  c_i <- c()
  center <- c(n_center)
  
  if (ho == 1) {
    description <- 
      "who work at least one outsourced job (in On Jobs survey)"
    name <- "Demographic Summary Statistics"
    lab <- "_ever_out"
    save <- ""
    col_name <- "Outsourced"
    divider <- "ever_out_oj"
  } else if (ho == 2) {
    description <- 
    " who ever work in a high-outsourcing occupation, comparing 
    those who work at least one outsourced job (in On Jobs survey)"
    name <- "Demographic Summary Statistics"
    lab <- "_ever_out_ever_ho"
    save <- " Ever Outsourced in Ever HO Occupation"
    col_name <- "Outsourced"
    divider <- "ever_out_oj"
  } else {
    description <- 
      " who work at least one job in a high-outsourcing occupation"
    name <- "Demographic Summary Statistics for Ever in a HO Occupation"
    lab <- "_ever_ho"
    save <- " Ever HO Occupation"
    col_name <- "HO Occupation"
    divider <- "ever_ho_occ"
  }
  
  cond <- comp[[ho]][[divider]] == 1
  
  for (i in 1:2) {
    col_i <- make_ss_col(sum_demo[[ho]], i, 
                         vars[c(-length(vars), -(length(vars)- 1))],
                         comp[[ho]], vars_p, cond, !cond, divider,
                         round = 3, get_stars = (i == 2)
                         )
    center <- cbind(center, col_i) 
    c_i <- cbind(c_i, col_i)
  }
  
  center <- add_endline(center)
  
  # If looking at ever_out_oj, also look at ever_ho_occ and 
  # add to combined
  if (ho == 1) {
    var <- "ever_ho_occ"
    se <- str_c(var, "_se")
    stars <- diff_test(comp[[ho]], var, "weight", "prop", cond, 
                  !cond, divider)
    add_out <- cbind(rbind("Percent Ever", "HO Occupation"),
                 rbind(format_val(sum_demo[[ho]][[var]][1]),
                       format_se(sum_demo[[ho]][[se]][1])),
                 rbind(format_val(sum_demo[[ho]][[var]][2],
                                  star = stars),
                       format_se(sum_demo[[ho]][[se]][2])),
                 space
    )
    c_center <- cbind(n_center, c_i)
  }
  
  # If looking at Ever HO Occupation, also look at percent ever
  # outsourced and add to combined
  if (ho == 3) {
    var <- "ever_out_oj"
    se <- str_c(var, "_se")
    stars <- diff_test(comp[[ho]], var, "weight", "prop", cond, 
                  !cond, divider)
    add_ho <- cbind(rbind("Percent Ever", "Outsourced"),
                 space,
                 rbind(format_val(sum_demo[[ho]][[var]][1]),
                       format_se(sum_demo[[ho]][[se]][1])),
                 rbind(format_val(sum_demo[[ho]][[var]][2],
                                  star = stars),
                       format_se(sum_demo[[ho]][[se]][2]))
            )
    c_center <- cbind(c_center, c_i)
    c_center <- rbind(add_out, add_ho, c_center)
    c_center <- add_endline(c_center) 
  }
  
  # Structure of table we are saving depends on if ho is 3 or not
  if (ho <= 2) {
    center <- center_to_latex(center)
    top <- str_c(
      "\\begin{tabular}{lSS}
\\toprule
Variable & {Ever ", col_name, "} & {Never ", col_name, "} \\\\ \\midrule
")
  } else {
    center <- center_to_latex(c_center)
    top <- str_c(
      "\\begin{tabular}{lSSSS}
\\toprule
Variable & {Ever Outsourced} & {Never Outsourced} & {Ever ",
      col_name, "} & {Never ", col_name, "} \\\\ \\midrule
")
  }
  
  # Save full table for own use + Drafts
  note <- str_c(
  "Demographic statistics for workers ", description, 
" versus those who never do.
Observations are at the person level for first survey post-2000 
and summary statistics are weighted.
Stars represent significance difference at the .10 level (*),
.05 level (**), and .01 level (***)."
  )
  label <- str_c("demo", lab)
  
  header <- make_header("", name, label)
  d_header <- make_header("d", name, label)
  
  bot <- make_bot(note)

  write.table(str_c(header, top, center, bot,
                    "\n \\end{document}"),
              str_c(table_folder, "NLSY79 Demographics/Demographics",
                    save, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  write.table(str_c(d_header, top, center, bot),
              str_c(d_table_folder, "NLSY79 Demographics",
                    save, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
}

# Save a basic version with just ever outsourced
b_center <- c_center[5:21, c(1:3, 6)]

# Create LaTeX output
b_center <- center_to_latex(b_center)

top <- "\\begin{tabular}{lSSSS}
\\toprule
& \\multicolumn{2}{c}{{Outsourced}} & \\multicolumn{2}{c}{{HO Occupation}} \\\\
Variable & {Ever} & {Never} & {Ever} & {Never} \\\\ \\midrule
"

b_top <- "\\begin{tabular}{lSS}
\\toprule
Variable & {Ever Outsourced} & {Never Outsourced} \\\\ \\midrule
"

name <- "Demographic Summary Statistics"
s_note <- "Demographic statistics for those who ever
work an outsourced jobs or in a high-outsourcing occupation
versus those that never do.
Observations are at the person level from an individual's first survey
post-2000 and summary statistics are weighted at
the person level. Stars represent significance difference at
the .10 level (*), .05 level (**), and .01 level (***)."
s_label <- "demo_comp_full"

b_note <- "Demographic statistics for those who ever
work in outsourced jobs versus those that never do.
Observations are weighted at the person level. 
Stars represent significance difference at 
the .10 level (*), .05 level (**), and .01 level (***)."
b_label <- "demo_comp"

d_s_header <- make_header("d", name, s_label)
d_b_header <- make_header("d", name, b_label)
# Note for slides, need to save full table as tiny
s_header <- make_header("s", size = "\\tiny")
# For basic comparing just ever to never ho occ, scriptsize is fine
s_b_header <- make_header("s", size = "\\scriptsize")

d_s_bot <- make_bot(s_note)
d_b_bot <- make_bot(b_note)

# Save for Drafts
write.table(str_c(d_s_header, top, c_center, d_s_bot),
            str_c(d_table_folder, "Demographics Comparison.tex"),
            quote=F, col.names=F, row.names=F, sep="")

write.table(str_c(d_b_header, b_top, b_center, d_b_bot),
            str_c(d_table_folder, "Demographics Comparison Basic.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save for Slides
write.table(str_c(s_header, top, c_center, s_bot),
            str_c(s_table_folder, "Demographics Comparison.tex"),
            quote=F, col.names=F, row.names=F, sep="")

write.table(str_c(s_b_header, b_top, b_center, s_bot),
            str_c(s_table_folder, "Demographics Comparison Basic.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Job Number Statistics --------------------------------------------------

life_jobs <- split_data(matched_jobs)

for (i in 1:2) {
  life_jobs[[i]] <- life_jobs[[i]] |>
    group_by(case_id) |> 
    mutate(
      n_jobs = n(),
      n_out_jobs = sum(outsourced)
    ) |> 
    filter(row_number() == min(row_number())) |> 
    as_survey_design(ids = case_id, weights = weight) |> 
    mutate(ever_out_oj = factor(ever_out_oj)) |>
    group_by(ever_out_oj) |> 
    summarise(
      n_jobs_mean = survey_mean(n_jobs),
      n_out_jobs_mean = survey_mean(n_out_jobs),
      n = unweighted(n()),
      n_only_out = unweighted(sum(n_out_jobs == n_jobs))
    ) |> 
    arrange(desc(ever_out_oj))
}

# # How many people only outsource in dataset?
# only_outsource <- matched_jobs |> 
#   group_by(case_id) |> 
#   mutate(per_out = mean(outsourced)) |> 
#   filter(per_out == 1)

# Record results in LateX
table <- str_c(make_header("", "Job Numbers", "job_numbers"),
"
\\begin{tabular}{lSS}
\\toprule
Variable & {Ever Outsourced} & {Never Outsourced} \\\\ \\midrule \n"
)

labels <- c("Whole Sample", "Ever HO Occupation")

for (ho in 1:2) {
  table <- str_c(table,
    labels[ho], " & & \\\\ \\midrule \n
    Number of Jobs", format_val(life_jobs[[ho]]$n_jobs_mean[1]),
                   format_val(life_jobs[[ho]]$n_jobs_mean[2]), 
    "\\\\ \n",
    format_se(life_jobs[[ho]]$n_jobs_mean_se[1]),
    format_se(life_jobs[[ho]]$n_jobs_mean_se[2]), "\\\\ \n 
    Number of Outsourced Jobs", 
    format_val(life_jobs[[ho]]$n_out_jobs_mean[1]),
    " & {--} \\\\ \n ", 
    format_se(life_jobs[[ho]]$n_out_jobs_mean_se[1]), " & \\\\ \n
    Number Only Outsourced", 
    format_n(life_jobs[[ho]]$n_only_out[1]), " & {--} \\\\ \n
    Observations", format_n(life_jobs[[ho]]$n[1]), 
    format_n(life_jobs[[ho]]$n[2]), 
    "\\\\"
  )
  if (ho == 1) {
    table <- str_c(table, "\\midrule \n")
  }
}

note <- "Job numbers in NLSY."
table <- str_c(table, make_bot(note))

write.table(table, str_c(table_folder, "NLSY79 Jobs/Job Numbers.tex"),
            quote=F, col.names=F, row.names=F, sep="")


# Jobs Summary Statistics -------------------------------------------------

# Summarize jobs at the year level using matched and job level using
# matched_jobs. Split by outsourced or not and by all job types

vars_sum <- 
  c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week",
    "part_time", "tenure", "union", "any_benefits",
    "health", "retirement", "childcare", "dental", "flex_sched",
    "life", "maternity", "profit_share", "train_school",
    "log_real_tot_comp", "job_sat", "n")

sum_jobs <- c(splits, splits)

for (i in 1:4) {
  sum_jobs[[i]] <- sum_jobs[[i]] |>
    as_survey_design(ids = case_id, weights = weight) |> 
    group_by(outsourced) |> 
    mutate(n = n()) |> 
    summarise_at(vars_sum, survey_mean, na.rm = T) |> 
    arrange(desc(outsourced))
  
  sum_jobs[[i + 4]] <- sum_jobs[[i + 4]] |>
    as_survey_design(ids = case_id, weights = weight) |> 
    group_by(temp_work, on_call, indep_con, self_emp, traditional,
             outsourced) |> 
    mutate(n = n()) |> 
    summarise_at(vars_sum, survey_mean, na.rm = T)
}

# Create Latex tables
top_j <- "\\begin{tabular}{lSS}
\\toprule
& {Outsourced} & {Non-Outsourced} \\\\  \\midrule
"

top_j_t <- "\\begin{tabular}{lSSSSSS}
\\toprule
Variable & {Outsourced} & {Traditional} & {Self-Emp.} & {Ind. Contractor} & {On-Call} & {Temp} \\\\ \\midrule 
"

top_j_o_t <- "\\begin{tabular}{lSS}
\\toprule
& {Outsourced} & {Traditional} \\\\  \\midrule
"

s_top <- "\\begin{tabular}{lSSSS}
\\toprule
Variable & {Outsourced} & {Traditional} & {Ind. Contractor} & {Temp} \\\\ \\midrule 
"

types <- c("outsourced", "traditional", "self_emp", 
           "indep_con", "on_call", "temp_work")

# Create table in Latex
# Use all vars except n
vars <- vars_sum[-length(vars_sum)]

# Divide variables by mean or prop (they are different below)
vars_p <- c("part_time", "union", "any_benefits", "health",
            "retirement", "childcare", "dental", "flex_sched",
            "life", "maternity", "profit_share", "train_school")

# Use table notes to make graph clearer, but not needed 
# for slides
center_r <- rbind(
  "Log Real", "Hourly Wage", "Log Real", "Weekly Wage",
  "Hours Worked", "Weekly", "Part Time", "", "Tenure", "(Weeks)",
  "Union", "", "Any Benefits\\tnote{a}", "", "Health", "Insurance\\tnote{a}",
  "Retirement", "Plan\\tnote{a}", "Subsidized", "Childcare\\tnote{a}",
  "Dental", "Insurance\\tnote{a}", "Flex", "Schedule\\tnote{a}",
  "Life", "Insurance\\tnote{a}", "Maternity", "Leave\\tnote{a}",
  "Profit", "Sharing\\tnote{a}", "Training\\tnote{a}", "",
  "Log Real", "Total Compensation\\tnote{b}", "Job Satisfaction",
  "(Lower Better)", "Observations")

center_r_s <- rbind(
  "Log Real", "Hourly Wage", "Log Real", "Weekly Wage",
  "Hours Worked", "Weekly", "Part Time", "", "Tenure", "(Weeks)",
  "Union", "", "Any Benefits", "", "Health", "Insurance", "Retirement",
  "Plan", "Subsidized", "Childcare", "Dental",
  "Insurance", "Flex", "Schedule", "Life", "Insurance", "Maternity",
  "Leave", "Profit", "Sharing", "Training", "",
  "Log Real", "Total Compensation", "Job Satisfaction", "(Lower Better)",
  "Observations")

dividers <- c()
sample <- c(
  "", " for workers who are ever in high-outsourcing industries")
observation <- c("person-job-year", "person-job")
explanation <- c(
  "", 
  ", where jobs observed more than once use average or modal characteristics"
  )
name <- "Job Summary Statistics"
labels <- c("_year", "_year_ho", "_job", "_job_ho")
folder <- c("Jobs", "Job Types")
save <- c("Year", "Year HO Occupations", "Job", "Job HO Occupations")

for (ty in 1:2) {
  for (ho in 1:2) {
    for (ob in 1:2) { 
    
    j <- 4 * (ty - 1) + 2 * (ob - 1) + ho 
    k <- 2 * (ob - 1) + ho 
    
    # What is the comparison dataset for test of differences?
    comp_r <- splits[[k]]
    
    center <- c()
    
    for (i in 1:NROW(sum_jobs[[j]])){
      # If ty == 2 (looking at all types), then for proportion tests,
      # need to get rid of all types but that type and outsourced
      type <- types[i]
      if (ty == 2) {
        cond <- comp_r$outsourced == 1
        cond_y <- comp_r[[type]] == 1
        comp <- comp_r[cond | cond_y, ]
        # For these, need to recreate cond and cond_y using comp
        cond <- comp$outsourced == 1
        cond_y <- comp[[type]] == 1
      } else {
        cond <- comp_r$outsourced == 1
        cond_y <- comp_r$outsourced == 0
        comp <- comp_r
      }
      col_i <- make_ss_col(sum_jobs[[j]], i, vars, comp, vars_p,
                           cond, cond_y, "outsourced", 
                           get_stars = (i > 1))
      center <- cbind(center, col_i)
    }
  
    center <- add_endline(center)
    
    # For Slides, (only if ty == 2) do not want self_emp or on_call
    # and only care about a few variables
    if (ty == 2) {
      
      s_center <- center_r_s |>
        cbind(center) 
      s_center <- s_center[c(3:4, 7:12, 15:18, 35:37), c(1:3, 5, 7:8)]
      s_center <- center_to_latex(s_center)
      
      # For Draft and Slides, (only if ty == 2) create tables with
      # outsourced and traditional.
      # For slides only care about a few variables
      center_o_t <- center_r |>
        cbind(center)
      center_o_t <- center_o_t[, c(1:3, 8)]
      center_o_t <- center_to_latex(center_o_t)
      
      s_center_o_t <- center_r_s |>
        cbind(center)
      s_center_o_t <- s_center_o_t[c(3:4, 7:12, 15:18, 35:37), c(1:3, 8)]
      s_center_o_t <- center_to_latex(s_center_o_t)
    }
  
    # Create LaTeX output
    center <- center_r |>
      cbind(center) |>
      center_to_latex()
  
    if (ty == 1) {
      
      top <- top_j
      
      label <- str_c("jobs", labels[k])
      note <- str_c(
      "Summary statistics of jobs in the NLSY 
      for each job type.",
      sample[ho], ". Observations are at the ", observation[ob],
      " level", explanation[ob], 
      ". All statistics are weighted at the person level. 
      Stars represent significant difference from outsourced 
      jobs at the .10 level (*), .05 level (**), and .01 level (***).
      \\item[a] ", note_access, "
      \\item[b] ", note_tot_comp)
      
      # These tables (just outsourced vs not) are saved for my own 
      # personal use
      header <- make_header("", name, label)
      
      bot <- make_bot(note)
    } else {
      
      top <- top_j_t
      
      label <- str_c("job_types", labels[k])
      note <- str_c(
        "Summary statistics of jobs in the NLSY for each job types",
        sample[ho], ". Observations are at the ",
        observation[ob], " level", explanation[ob],
        ". All statistics are weighted at the person level.
        Stars represent significant difference from outsourced
        jobs at the .10 level (*), .05 level (**), and .01 level (***).
        \\item[a] ", note_access, "
        \\item[b] ", note_tot_comp)
      
      # These graphs are too big, need to be smaller font and
      # less column separation
      header <- make_header("", name, label, 
                            size = "\\scriptsize", colsep = -1.0)
      d_header <- make_header("d", name, label, 
                              size = "\\scriptsize", colsep = -1.0)
      s_header <- make_header("s", size = "\\scriptsize")
      
      bot <- make_bot(note)
    
      label_o_t <- str_c("job_types", labels[k], "_o_t")
      note_o_t <- str_c(
        "Summary statistics of jobs in the NLSY for
        outsourced and traditional jobs", sample[ho], 
        ". Observations are at the ", observation[ob], 
        " level", explanation[ob], 
        ". All statistics are weighted at the person level.
        Stars represent significant difference from outsourced 
        jobs at the .10 level (*), .05 level (**), and .01 level (***).
        \\item[a] ", note_access, "
        \\item[b] ", note_tot_comp
        )
      
      d_header_o_t <- make_header("d", name, label_o_t)
      s_header_o_t <- make_header("s", size = "\\scriptsize")
      
      bot_o_t <- make_bot(note_o_t)
      
      # If ho == 1, save a version in Drafts and Slides
      if (ho == 1) {
        write.table(str_c(d_header, top, center, bot),
                    str_c(d_table_folder, "Job Summary Statistics ", 
                          save[k], ".tex"),
                    quote=F, col.names=F, row.names=F, sep="")
        
        write.table(str_c(s_header, s_top, s_center, s_bot),
                    str_c(s_table_folder, "Job Summary Statistics ",
                          save[k], ".tex"),
                    quote=F, col.names=F, row.names=F, sep="")
        
        # Just save outsourced and traditional
        if (ty == 2){
          write.table(str_c(d_header_o_t, top_j_o_t, center_o_t,
                            bot_o_t),
                      str_c(d_table_folder, "Job Summary Statistics ",
                            save[k], " Out + Trad.tex"),
                      quote=F, col.names=F, row.names=F, sep="")

          write.table(str_c(s_header_o_t, top_j_o_t, s_center_o_t,
                            s_bot),
                      str_c(s_table_folder, "Job Summary Statistics ",
                            save[k], " Out + Trad.tex"),
                      quote=F, col.names=F, row.names=F, sep="")
        }
      }
    }
    
    # Save Everything for my own records 
    write.table(str_c(header, top, center, bot, "\n \\end{document}"),
              str_c(table_folder, "NLSY79 ", folder[ty] ,"/Jobs ",
                    save[k], ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
    }
  }
}

# HO Occupation Job Characteristics --------------------------------------------

# How do HO Occupations compare to non-HO Occupations? Use matched_jobs
vars_sum <- c("log_real_hrly_wage", "log_real_wkly_wage", 
              "hours_week", "part_time", "tenure", "union", 
              "job_sat", "any_benefits", "health", "retirement",
              "childcare", "dental", "flex_sched", "life", 
              "maternity", "profit_share", "train_school",
              "log_real_tot_comp", "n")

ho_sum <- list(matched, matched_jobs)
comp <- list(matched, matched_jobs)

for (i in 1:2) {
  ho_sum[[i]] <- ho_sum[[i]] |>
    as_survey_design(ids = case_id, weights = weight) |> 
    group_by(ho_occ) |> 
    mutate(n = n()) |> 
    summarise_at(vars_sum, survey_mean, na.rm = T) |> 
    arrange(desc(ho_occ))
}

observation <- c("person-job-year", "person-job")
labels <- c("_year", "_job")
save <- c("Year", "Job")

# Create a Latex table (just for me)
top <- "
\\begin{tabular}{lSS}
\\toprule
& {High-Outsourcing} & {Others} \\\\  \\midrule
"

# Drop n from vars
vars <- vars_sum[-length(vars_sum)]

center_r <- rbind(
  "Log Real", "Hourly Wage", "Log Real", "Weekly Wage", 
  "Hours Worked", "Weekly", "Part Time", "", "Tenure", "(Weeks)",
  "Union", "", "Job Satisfaction", "(Lower Better)", 
  "Any Benefits", "", "Health", "Insurance", "Retirement",
  "Plan", "Subsidized", "Childcare", "Dental",
  "Insurance", "Flex", "Schedule", "Life", "Insurance", "Maternity",
  "Leave", "Profit", "Sharing", "Training", "", "Log Real",
  "Total Compensation" ,"Observations")

# Proportion variables
vars_p <- c("part_time", "union", "any_benefits", "health",
            "retirement", "childcare", "dental", "flex_sched",
            "life", "maternity", "profit_share", "train_school")

for (ob in 1:2) { 
  
  cond <- comp[[ob]]$ho_occ == 1
  cond_y <- comp[[ob]]$ho_occ == 0
  
  center <- c(center_r)
  
  for (i in 1:2){
    col_i <- make_ss_col(ho_sum[[ob]], i, vars, comp[[ob]], vars_p,
                         cond, cond_y, "ho_occ", get_stars = (i > 1))
    center <- cbind(center, col_i)
  }
  
  center <- center |> 
    add_endline() |>
    center_to_latex()
  
  name <- "HO Occupation Job Summary Statistics"
  label <- str_c("jobs", labels[ob])
  note <- str_c(
    "Summary statistics of jobs divided by high-outsourcing 
    ($\\geq$ 3.48\\%) vs other occupations. Observations are 
    at the ", observation[ob], " level and statistics are 
    weighted at the person level. Stars represent significance 
    difference from outsourced jobs at the .10 level (*), 
    .05 level (**), and .01 level (***)."
  )
  
  header <- make_header("", name, label)
  
  bot <- make_bot(note)
  
  write.table(str_c(top, center, bot),
              str_c(table_folder, "NLSY79 Jobs/Jobs ", save[ob],
                    " HO Occupation vs Not.tex"),
              quote=F, col.names=F, row.names=F, sep="")
}

# Summarize Union NA ------------------------------------------------------

# Note: always run this code after previous

# # Many people are missing union. Do these people/jobs look any different
# union_type <- c(split_data(matched), split_data(matched_jobs))
# for (i in 1:4) {
#   union_type[[i]] <- union_type[[i]] |>
#     filter(!is.na(union_fill)) |> 
#     as_survey_design(ids = case_id, weights = weight) |> 
#     group_by(union_fill, outsourced) |> 
#     mutate(n = n()) |> 
#     summarise_at(vars_sum, survey_mean, na.rm = T) |> 
#     arrange(desc(union_fill))
# }


# Within Job Wage Distribution --------------------------------------------

# Does the within job wage distribution (wage - starting wage) look
# different for outsourced vs traditional (for jobs with >=2 obs)

multi_year <- matched |> 
  filter(!is.na(log_real_wkly_wage), !is.na(log_real_hrly_wage),
         !is.na(log_real_tot_comp)) |> 
  group_by(case_id, emp_id) |> 
  mutate(
    ever_ho_occ = max(ever_ho_occ),
    rank = rank(int_year),
    lrww_min_start = 
      log_real_wkly_wage - log_real_wkly_wage[which(rank == 1)],
    lrtc_min_start = 
      log_real_tot_comp - log_real_tot_comp[which(rank == 1)],
    gain_health = health - health[which(rank == 1)]
    ) |> 
  filter(rank > 1, rank <= 7, lrww_min_start > -2.5, 
         lrww_min_start < 2.5)

temp <- multi_year |> 
  filter((outsourced == 1 | traditional == 1)) |> 
  ggplot() +
  geom_density(aes(lrww_min_start, fill = factor(outsourced)), 
               alpha = 0.2) +
  labs(x = "Log Real Weekly Wages", y = "Density") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                    values = c("blue", "red"),
                    labels = c("Traditional", "Outsourced")) +
  theme_light(base_size = 16) + 
  facet_wrap(~ rank)
  
ggsave(str_c(figure_folder, 
             "LRW Wage Compared to First Observation.pdf"),
       height = height, width = width)

temp <- multi_year |> 
  filter(ever_ho_occ == 1, (outsourced == 1 | traditional == 1)) |> 
  ggplot() +
  geom_density(aes(lrww_min_start, fill = factor(outsourced)), 
               alpha = 0.2) +
  labs(x = "Log Real Weekly Wages", y = "Density") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                    values = c("blue", "red"),
                    labels = c("Traditional", "Outsourced")) +
  theme_light(base_size = 16) + 
  facet_wrap(~ rank)

ggsave(str_c(figure_folder, 
             "LRW Wage Compared to First Observation Ever HO.pdf"),
       height = height, width = width)

temp <- multi_year |> 
  filter((outsourced == 1 | traditional == 1)) |> 
  ggplot() +
  geom_density(aes(lrtc_min_start, fill = factor(outsourced)), 
               alpha = 0.2) +
  labs(x = "Log Real Total Compensation", y = "Density") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                    values = c("blue", "red"),
                    labels = c("Traditional", "Outsourced")) +
  theme_light(base_size = 16) + 
  facet_wrap(~ rank)

ggsave(str_c(figure_folder, 
             "LR Tot Comp Compared to First Observation.pdf"),
       height = height, width = width)

# What about probability of starting access to health insurance over time?
temp <- multi_year |> 
  filter((outsourced == 1 | traditional == 1)) |> 
  ggplot(aes(gain_health, fill = factor(outsourced))) +
  geom_histogram(aes(y=.5*..density..), 
                 alpha=0.5, position="dodge",
                 binwidth = .5) + 
  labs(x = "Gained/Lost Health Insurance", y = "Density") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = c(-1, 0, 1)) +
  scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                    values = c("blue", "red"),
                    labels = c("Traditional", "Outsourced")) +
  theme_light(base_size = 16) + 
  facet_wrap(~ rank)

ggsave(str_c(figure_folder, 
             "Health Insurance Over Time.pdf"),
       height = height, width = width)

temp <- multi_year |> 
  filter(ever_ho_occ == 1, (outsourced == 1 | traditional == 1)) |> 
  ggplot(aes(gain_health, fill = factor(outsourced))) +
  geom_histogram(aes(y=.5*..density..), 
                 alpha=0.5, position="dodge",
                 binwidth = .5) + 
  labs(x = "Gained/Lost Health Insurance", y = "Density") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = c(-1, 0, 1)) +
  scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                    values = c("blue", "red"),
                    labels = c("Traditional", "Outsourced")) +
  theme_light(base_size = 16) + 
  facet_wrap(~ rank)

ggsave(str_c(figure_folder, 
             "Health Insurance Over Time Ever HO.pdf"),
       height = height, width = width)

# Dube and Kaplan ---------------------------------------------------------

# Compare outsourced vs not for janitors and security guards based on
# self-reported outsourced and Dube and Kaplan's measure (outsourced_2), 
# which uses those in certain industries as outsourced. 
# See if they are significantly different

janitor <- matched_jobs |>
  filter(occ == 4220) |>
  mutate(
    # outsourced_a = 1 - traditional - on_call,
    outsourced_2 = 1 * (ind == 7690),
    out_1_v_2 = outsourced - outsourced_2
  ) |> 
  filter(!is.na(outsourced_2)) 

sg <- matched_jobs |>
  filter(occ == 3920) |>
  mutate(
    # outsourced_a = 1 - traditional - on_call,
    outsourced_2 = 1 * (ind == 7680),
    out_1_v_2 = outsourced - outsourced_2
  ) |> 
  filter(!is.na(outsourced_2)) 

vars_sum_js <- c("log_real_hrly_wage", "log_real_wkly_wage", 
                 "hours_week", "part_time",
                 "any_benefits", "health", "union", "job_sat",
                 "less_hs", "hs", "aa", "ba", "plus_ba",
                 "female", "black", "hispanic", "age", "n")

sum_js <- list(janitor, janitor, sg, sg)
comp_js <- list(janitor, sg)

for (i in 1:4) {
  out<- if (i %% 2 == 1) "outsourced" else "outsourced_2"
  
  sum_js[[i]] <- sum_js[[i]] |>
    as_survey_design(ids = case_id, weights=weight) |> 
    group_by_at(out) |>
    mutate(n = n()) |> 
    summarise_at(vars_sum_js, survey_mean, na.rm = T) |> 
    arrange_at(desc(out))
}

top_js <- "\\begin{tabular}{lSSSS}
\\toprule
& \\multicolumn{2}{c}{{Self-Reported (This Paper)}} & 
\\multicolumn{2}{c}{{Industry-Occupation (Dube and Kaplan)}} \\\\
Variable & {Outsourced} & {Not Outsourced} & {Outsourced} & {Not Outsourced} \\\\ \\midrule
"

# Slides need a slightly tighter top
s_top <- "\\begin{tabular}{lSSSS}
\\toprule
& \\multicolumn{2}{c}{{Self-Reported (This Paper)}} & 
\\multicolumn{2}{c}{{Ind-Occ (Dube and Kaplan)}} \\\\
Variable & {Outsourced} & {Not Outsourced} & {Outsourced} & {Not Outsourced} \\\\ \\midrule
"

# Create table in Latex
vars_js <- vars_sum_js[-length(vars_sum_js)]

vars_js_p <- c("part_time", "female", "black", "hispanic", "less_hs",
               "hs", "aa", "ba", "plus_ba", "any_benefits", 
               "health", "union")

center_js_r <- rbind("Log Real", "Hourly Wage", "Log Real",
                     "Weekly Wage", "Hours Worked", "per Week",
                     "Part Time", "", "Any Benefits", "", 
                     "Health Insurance", "", "Union", "", 
                     "Job Satisfaction", "(Lower Better)", 
                     "No HS Diploma", "", "HS Diploma", "",
                     "AA Degree", "", "BA Degree", "",
                     "Postgraduate", "Degree", "Female", "",
                     "Black", "", "Hispanic", "", "Age", "",
                     "Observations")

# vars_js_m <- c("log_real_hrly_wage", "log_real_wkly_wage", 
#                "hours_week", "job_sat", "age")

desc_js <- c("janitors (occupation 4220)", 
             "security guards (occupation 3920)")
label_js <- c("janitor", "sg")
occ_js <- c("Janitors", "Security Guards")
ind_js <- c("services to buildings and dwellings (industry 7690)", 
            "protective services (industry 7680)") 

for (js in 1:2){
  
  center_js <- c(center_js_r)
  
  comp <- comp_js[[js]]
  
  for (def in 1:2) {
    
    j <- 2 * (js - 1) + def
    
    for (i in 1:2) {
      r <- i + 2 * (def - 1)
      # Different comparison groups
      if (r == 2){
        divider <- "outsourced"
        cond <- comp$outsourced == 1
        cond_y <- comp$outsourced == 0
      } else if (r == 4) {
        divider <- "outsourced_2"
        cond <- comp$outsourced_2 == 1
        cond_y <- comp$outsourced_2 == 0
      } else {
        cond <- T
        cond_y <- T
      }
      # This is not true for our Xi test (because there is overlap)
      # else if (r == 3) {
        # divider <- "outsourced"
        # cond <- comp_r$outsourced == 1
        # cond_y <- comp_r$outsourced_2 == 1
        # comp <- comp_r[cond | cond_y, ]
        # # Need to make these the same size as comp
        # cond <- comp$outsourced_a == 1
        # cond_y <- comp$outsourced_2 == 1
      # } 
      col_i <- make_ss_col(sum_js[[j]], i, vars_js,
                           comp, vars_js_p, cond, cond_y, divider,
                           get_stars = (i > 1))
      # col_i <- c()
      # for (var in vars_js){
      #   se <- str_c(var, "_se")
      #   mode <- if (var %in% vars_js_p) "prop" else "mean"
      #   stars <- if (i > 1) diff_test(
      #     comp, var, "weight", mode, cond, cond_y, divider
      #   ) else ""
      #   col_i <- rbind(col_i,
      #     format_val(sum_js[[j]][[var]][i], star = stars),
      #     format_se(sum_js[[j]][[se]][i])
      #   )
      # }
      # col_i <- rbind(col_i, format_n(sum_js[[j]]$n[i]))
      center_js <- cbind(center_js, col_i)
    }
  }
  
  center_js <- add_endline(center_js)
  
  # Save only some variables for Slides
  s_center <- center_js[c(3:4, 7:10, 17:20, 27:30, 33),]
  
  # Create LaTeX output
  center_js <- center_to_latex(center_js)
  s_center <- center_to_latex(s_center)
  
  name <- 
    str_c("Summary Statistic Comparison to Dube and Kaplan (2010) for ",
          occ_js[js])
  label <- label_js[js]
  note <- str_c(
    "Summary statistics for ", desc_js[js], " who are outsourced vs
not outsourced. In the left two columns, outsourced is 
self-reported by the worker as in the rest of this paper. 
In the right two, outsourced the worker is assumed outsourced
if their industry is ", ind_js[js], 
" following \\citet{dubekaplan2010}. Observations are at the
person-job level and summary statistics are weighted at the 
person level. Stars represent significance difference from 
outsourced of same determination method
at the .10 level (*), .05 level (**), and .01 level (***)."
  )
  
  header <- make_header("", name, label)
  d_header <- make_header("d", name, label)
  # Table needs to be scriptsize for Slides
  s_header <- make_header("s", size = "\\scriptsize")
  
  bot <- make_bot(note)
  
  write.table(str_c(header, top_js, center_js, bot, 
                    "\n \\end{document}"),
              str_c(table_folder, "NLSY79 Dube Kaplan/", 
                    occ_js[js], " Summary.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save a version to Drafts
  write.table(str_c(d_header, top_js, center_js, bot),
              str_c(d_table_folder, "Dube Kaplan ", occ_js[js],
                    " Summary.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save a version to Slides
  write.table(str_c(s_header, s_top, s_center, s_bot),
              str_c(s_table_folder, "Dube Kaplan ", occ_js[js],
                    " Summary.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
}

# What are the self-reported job types of DK's outsourced?
# Use data.frame's SQL like capabilities to make these tables
top <- "\\begin{tabular}{lrrr}
\\toprule
& \\multicolumn{3}{c} {Industry-Occupation (Dube and Kaplan)} \\\\
Self-Reported & Outsourced & Not Outsourced & Total \\\\ \\midrule
"

center_r <- c("Contracted-Out (This Paper)", "Independent Contractor",
            "Temp Worker", "On-Call Worker", "Self-Employed", 
            "Traditional Employee", "Total" )

occ_js <- c("janitors (occupation 4220)", 
            "security guards (occupation 3920)")
ind_js <- c("services to buildings and dwellings (industry 7690)", 
            "protective services (industry 7680)")
labels <- c("janitors", "sg")
save <- c("Janitors", "Security Guards")

types <- c("outsourced", "indep_con", "temp_work",
           "on_call", "self_emp", "traditional")

dt_js <- list(data.table(janitor), data.table(sg))

for (js in 1:2){
  
  col_i <- c()
  
  for (type in types){
    # Some observations may not occur, set to 0
    a <- tryCatch(dt_js[[js]][outsourced_2 == 1 
                              & dt_js[[js]][[type]] == 1, .N], 
                  error = function(e) {a <- 0})
    b <- tryCatch(dt_js[[js]][outsourced_2 == 0 
                              & dt_js[[js]][[type]] == 1, .N],
                  error = function(e) {b <- 0})
    b <- b * !is.na(b) + 0  
    col_i <- rbind(
      col_i, cbind(str_c(" & ", a), str_c(" & ", b), 
                   str_c(" & ", a + b)))
  }
  
  col_i <- rbind(col_i,
    cbind(str_c(" & ", dt_js[[js]][outsourced_2 == 1, .N]),
          str_c(" & ", dt_js[[js]][outsourced_2 == 0, .N]),
          str_c(" & ", NROW(dt_js[[js]]))))
  
  center <- center_r |>
    cbind(col_i) |>
    add_endline() |> 
    center_to_latex()
  
  name <- 
    str_c(
      "Job Type Classification Comparison to Dube and Kaplan 2010 for ",
      save[js])
  label <- str_c("dk_types_", labels[js])
  note <- str_c(
    "Counts of \\citet{dubekaplan2010}'s (DK) method of measuring 
    outsourcing versus NLSY self-reported job type for ", occ_js[js],
    ". For columns, following DK, workers are considered outsourced 
    if they are in ", ind_js[js], ". Rows show the worker's 
    self-reported job type. Observations are at the person-job level."
  )
  
  header <- make_header("", name, label)
  d_header <- make_header("d", name, label)
  s_header <- make_header("s")
  
  bot <- make_bot(note)
  
  write.table(str_c(header, top, center, bot, "\n \\end{document}"),
              str_c(table_folder, "NLSY79 Dube Kaplan/",
                    save[js], " Types.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save in Drafts
  write.table(str_c(d_header, top, center, bot),
              str_c(d_table_folder, "Dube Kaplan ", save[js],
                    " Outsourced.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save in Slides
  write.table(str_c(s_header, top, center, s_bot),
              str_c(s_table_folder, "Dube Kaplan ", save[js],
                    " Outsourced.tex"),
              quote=F, col.names=F, row.names=F, sep="")
}

# Some jobs were preassigned traditional (pre-trad). 
# How many are there of each type and how many of these 
# are outsourced according to DK?
table <- str_c(
"\\begin{tabular}{lcc}
\\toprule
Variable & Janitors & Security Guards \\\\ \\midrule
Traditional", format_val(sum(janitor$traditional), s = 0),
format_val(sum(sg$traditional), s = 0), "\\\\ \n",
"Traditional Outsourced (DK)", 
format_val(sum(janitor$outsourced_2), s = 0), 
format_val(sum(sg$outsourced_2), s = 0), "\\\\ \n",
"Pre-assigned Traditional", format_val(sum(janitor$pre_trad), s = 0), 
format_val(sum(sg$pre_trad), s = 0), "\\\\ \n",
"Pre-assigned Traditional Outsourced (DK)", 
format_val(sum(janitor$outsourced_2[janitor$pre_trad > 0]), s = 0), 
format_val(sum(sg$outsourced_2[sg$pre_trad > 0]), s = 0), "\\\\"
)

name <- "Pre-Assigned Traditional Outsourcing Status According to DK"
label <- "dk_pre_trad"
note <- "For janitors and security guards who were in traditional jobs,
how many were outsourced according to Dube and Kaplan (2010) (DK), how
many were pre-assigned traditional, and how many pre-assigned jobs 
were outsourced according to DK."

# Save this just for me
header <- make_header("", name, label)
bot <- make_bot(note)

write.table(str_c(header, table, bot, "\n \\end{document}"),
            str_c(table_folder,
                  "NLSY79 Dube Kaplan/Traditional Split.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# PBS Industries by Type --------------------------------------------------

# Break down workers by type again, but do this for both PBS and 
# non-PBS industries
m_j_dt <- data.table(matched_jobs)

top <- "\\begin{tabular}{lrrr}
\\toprule
Self-Reported & PBS & Not PBS & Total \\\\ \\midrule
"

types <- c("outsourced", "indep_con", "temp_work", "on_call",
           "self_emp", "traditional")

center <- c("Outsourced (Contracted-Out)", "Independent Contractor", 
            "Temp Worker", "On-Call Worker", "Self-Employed",
            "Traditional Employee", "Total" )

col_i <- c()

for (type in types){
  # Some observations may not occur, set to 0
  a <- tryCatch(m_j_dt[pbs == 1 & m_j_dt[[type]] == 1, .N], 
                error = function(e) {a <- 0})
  b <- tryCatch(m_j_dt[pbs == 0 & m_j_dt[[type]] == 1, .N],
                error = function(e) {b <- 0})
  b <- b * !is.na(b) + 0  
  col_i <- rbind(
    col_i, cbind(str_c(" & ", a), str_c(" & ", b), 
                 str_c(" & ", a + b)))
}

col_i <- rbind(
  col_i,
  cbind(str_c(" & ", m_j_dt[pbs == 1, .N]),
        str_c(" & ", m_j_dt[pbs == 0, .N]),
        str_c(" & ", NROW(m_j_dt))))

center <- center |>
  cbind(col_i) |>
  add_endline() |> 
  center_to_latex()

name <- "Job Types of Personal Business Service Workers"
label <- "pbs_types"
note <- "Job types of workers in Professional Business Service (PBS)
industries versus all other industries. PBS Industries have 
Census 2000 Industry Codes between 7270 and 7790."

header <- make_header("", name, label)
d_header <- make_header("d", name, label)
s_header <- make_header("s")

bot <- make_bot(note)

write.table(str_c(header, top, center, bot, "\n \\end{document}"),
            str_c(table_folder, "NLSY79 Job Types/PBS Types.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save in Drafts
write.table(str_c(d_header, top, center, bot),
            str_c(d_table_folder, "PBS Types.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save in Slides
write.table(str_c(s_header, top, center, s_bot),
            str_c(s_table_folder, "PBS Types.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Before next sections, remove some larger files to free memory.
rm("dt_js", "janitor", "life_jobs", "sg", "sum_demo",
   "sum_jobs", "sum_js")

# Regressions -------------------------------------------------------------

# Create a loop that takes various desired outcomes, 
# runs series of regressions, and creates table in Latex
# 1. Basic Controls (age:age quartic, black, hispanic, education,
# union_fill, region, msa/cc, marital status, children, hours week 
# / part time, year)
# 2. Add tenure quartic and occ fixed effects
# 3. Individual FE + some controls (age quartic, union_fill, 
# region, msa/cc, year/year started+ended job)
# 4. Add tenure quartic
# 5. Add occ factors
# 6. Add 4+5

# Also create a table for Drafts and Slides with 
# just 6 for some outcomes
var_r <- c("log_real_hrly_wage", "log_real_wkly_wage", "hours_week",
           "part_time", "job_sat", "any_benefits",
           "health", "retirement", "log_real_tot_comp")

var_names <- c("log real hourly wages", "log real weekly wages",
               "hours worked per week", 
               "part-time status ($<$35 hours per week)", 
               "job satisfaction (lower is better)",
               "access to any employment benefits",
               "access to health insurance",
               "access to retirement benefits",
               "log real total compensation")

save_names <- c("LRH Wages", "LRW Wages",  "Hours Week", "Part Time",
                "Job Satisfaction", "Benefits", "H Insurance",
                "Retirement", "LR Tot Comp")

title_names <- c("Log Real Hourly Wages", "Log Real Weekly Wages",
                 "Hours Worked per Week", "Part-time Status", 
                 "Job Satisfaction", "Employment Benefits",
                 "Health Insurance", "Retirement Benefits",
                 "Log Real Total Compensation")

types <- c("outsourced", "self_emp", "indep_con", "temp_work", 
           "on_call")

controls <- c("age", "I((age)^2)", "I((age)^3)", "I((age)^4)",
              "factor(union_fill)")

ols_controls <- c("female", "black", "hispanic", "less_hs", "hs", "aa", "ba",
                  "plus_ba")

tenure <- c("I((tenure/100))", "I((tenure/100)^2)", 
            "I((tenure/100)^3)", "I((tenure/100)^4)")

fixed_effects <- c("region", "marital_status", "msa")

labels <- c("lrhw", "lrww", "hours", "pt", "job_sat", 
            "benefits", "health", "lrtc")

dfs <- list(matched, matched_jobs)
samples <- c("person-job-year", "person-job")
s_labels <- c("_year", "_job")
s_saves <- c("Years", "Jobs")
s_explanations <- c(
  "", ", where jobs observed more than once use average 
  or modal characteristics"
)

top <- "
\\begin{tabular}{lSSSSSS}
\\toprule
& \\multicolumn{2}{c}{{OLS}} & \\multicolumn{4}{c}{{Worker Fixed Effects}} \\\\
& {Basic} & {Full}  & {Basic}  & {Tenure} & {Occ FE} & {Full}  \\\\\\midrule
"

center_r <- rbind("Outsourced", "", "Worker FE", "Tenure Quartic",
                  "Occupation FE", "$R^2$", "Observations")

# Save two tables, one with main results and one with robustness
# Slide tops do not have tablenotes
var_1 <- c("log_real_wkly_wage", "health", "retirement", "log_real_tot_comp")
top_1 <- "
\\begin{tabular}{lSSSS}
\\toprule
&  {Log Real} & {Health} & {Retirement} & {Log Real} \\\\
& {Weekly Wages} & {Insurance\\tnote{a}} & {Plan\\tnote{a}} & {Total Compensation\\tnote{b}}  \\\\\\midrule
"
top_1_s <- "
\\begin{tabular}{lSSSS}
\\toprule
&  {Log Real} & {Health} & {Retirement} & {Log Real} \\\\
& {Weekly Wages} & {Insurance} & {Plan} & {Total Compensation}  \\\\\\midrule
"

var_2 <- c("log_real_hrly_wage", "hours_week",
           "part_time", "any_benefits")
top_2 <- "
\\begin{tabular}{lSSSSSS}
\\toprule
& {Log Real} & {Hours Worked} & {} & {Any} \\\\
& {Hourly Wages} &  {Per Week}  & {Part-Time} & {Benefits\\tnote{a}}  \\\\\\midrule
"
top_2_s <- "
\\begin{tabular}{lSSSSSS}
\\toprule
& {Log Real} & {Hours Worked} & {} & {Any} \\\\
& {Hourly Wages} &  {Per Week}  & {Part-Time} & {Benefits}  \\\\\\midrule
"

center_d <- rbind("Outsourced", "", "$R^2$", "Observations")

name_1 <- "Quality of Outsourced Jobs"
name_2 <- "Quality of Outsourced Jobs: Alternative Measures"

for (loop in 1:2) {
  
  sample <- samples[loop]
  s_label <- s_labels[loop]
  s_save <- s_saves[loop]
  s_explanation <- s_explanations[loop]
  y <- if (loop == 1) "dummies for year" else "dummies for year
  started and ended job"
  
  # Reset Draft Table Centers
  c_1 <- c()
  c_2 <- c()
  
  for (ind in seq_along(var_r)) {
    
    center <- c(center_r)
    
    var <- var_r[ind]
    var_name <- var_names[ind]
    label <- labels[ind]
    save <- str_c(" ", save_names[ind])
    name <- str_c(name_1, title_names[ind], sep = ": ")
    
    for (reg_ind in 1:6){
      
      ind_vars <- c(types, controls)
      if (loop == 1) {
        fe_vars <- c(fixed_effects, "int_year")
      } else {
        fe_vars <- c(fixed_effects, "I(year(week_start_job))", 
                     "I(year(week_end_job))")
      }
      
      if (reg_ind <= 2){
        ife_ind <- "{No}"
        ind_vars <- c(ind_vars, ols_controls)
      } else{
        ife_ind <- "{Yes}"
        fe_vars <- c(fe_vars, "case_id")
      }
      
      if (reg_ind %in% c(2, 4, 6)){
        ten_ind <- "{Yes}"
        ind_vars <- c(ind_vars, tenure)
      } else{
        ten_ind <- "{No}"
      }
      
      if (reg_ind %in% c(2, 5, 6)){
        occ_ind <- "{Yes}"
        fe_vars <- c(fe_vars, "occ")
      } else{
        occ_ind <- "{No}"
      }
      
      eq <- create_formula(var, ind_vars)
      fe <- create_formula("", fe_vars)
      
      # Run regression both for matched and matched_jobs
      temp <- lm_robust(eq, data = dfs[[loop]], weights = weight,
                        subset = !is.na(region) & !is.na(msa) 
                        & !is.na(marital_status) & !is.na(occ),
                        fixed_effects = !!fe,
                        clusters = as_factor(sample_id),
                        se_type = "stata", try_cholesky = T)
      
      stars <- p_stars(temp$p.value["outsourced"])
      
      center <- cbind(
        center,
        rbind(format_val(temp$coefficients["outsourced"], 
                         r=3, s=3, star = stars),
              format_se(temp$std.error["outsourced"], r=3, s=3),
              str_c(" & ", ife_ind),
              str_c(" & ", ten_ind),
              str_c(" & ", occ_ind),
              format_val(temp$r.squared),
              format_n(lm_N(temp))
        )
      )
      
      # Save full regression in c_1 or c_2 depending on outcome
      if (reg_ind == 6) {
        if (var %in% var_1) {
          c_1 <- cbind(
            c_1,
            rbind(format_val(temp$coefficients["outsourced"],
                             r=3, s=3, star = stars),
                  format_se(temp$std.error["outsourced"], r=3, s=3),
                  format_val(temp$r.squared), format_n(lm_N(temp)))
          )
        }
        
        if (var %in% var_2) {
          c_2 <- cbind(
            c_2,
            rbind(format_val(temp$coefficients["outsourced"],
                             r=3, s=3, star = stars),
                  format_se(temp$std.error["outsourced"], r=3, s=3),
                  format_val(temp$r.squared), format_n(lm_N(temp)))
          )
        }
      }
    }
    
    center <- center |>
      add_endline() |>
      center_to_latex()
    
    # Save tables with all specifications for each individual
    # outcome only for myself
    
    header <- make_header("", name, str_c("reg_", label, s_label),
                          colsep = 1.5)
    
    note <- str_c(
      "Regressions of outsourced on ", var_name, ". Data comes
      from the NLSY. All regressions
      include controls for job type (traditional job is default),
      a quartic in age, ", y, ", union status, dummies for region,
      whether in an MSA or central city, and marital status. 
      The first two columns run OLS and also contain controls for
      race and education. The last four columns use worker 
      fixed effects. All observations are at the ", sample,
      " level and all standard errors are clustered by 
      demographic sample. Stars represent significance 
      at the .10 level (*), .05 level (**), and .01 level (***)."
    )
    
    bot <- make_bot(note)
    
    write.table(str_c(header, top, center, bot, "\n \\end{document}"),
                str_c(table_folder, "NLSY79 Regressions/",
                      s_save, save, ".tex"),
                quote=F, col.names=F, row.names=F, sep="")
    
  }
  
  # Save composite tables to Drafts and Slides
  center_1 <- center_d |>
    cbind(c_1) |>
    add_endline() |>
    center_to_latex()
  center_2 <- center_d |>
    cbind(c_2) |>
    add_endline() |>
    center_to_latex()
  
  # Draft needs smaller column separation
  d_header <- make_header("d", name_1, 
                          str_c("regs", s_label),
                          colsep = 1.5)
  s_header <- make_header("s", colsep = 0.5)
  
  note <- str_c(
    "Regressions of worker outsourcing status on job outcomes.
    Data comes from the NLSY. 
    All regressions include controls for job type 
    (traditional job is default), worker 
    and occupation fixed effects, a quartic in age, ", y,
    ", union status, dummies for region, whether in an MSA 
    or central city, and marital status. All observations are
    at the ", sample, " level,", s_explanation, ". 
    All regressions are weighted at the person level 
    and all standard errors are clustered by demographic sample.
    Stars represent significance at the .10 level (*), .05 level (**),
    and .01 level (***).
    \\item[a] ", note_access, "
    \\item[b] ", note_tot_comp
  )
  
  bot_1 <- make_bot(note)
  
  # Drafts
  write.table(str_c(d_header, top_1, center_1, bot_1),
              str_c(d_table_folder, "Job Regressions ",
                    s_save, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")

  # Slides
  write.table(str_c(s_header, top_1_s, center_1, s_bot),
              str_c(s_table_folder, "Job Regressions ",
                    s_save, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save 6
  
  # Wide tables needs smaller column separation
  d_header <- make_header("d", name_2, str_c("alt_regs", s_label),
                          colsep = 1.5)
  s_header <- make_header("s", colsep = 0.5)
  
  note <- str_c(
    "Regressions of worker outsourcing status on job outcomes.
    Data comes from the NLSY. 
    All regressions include controls for job type (traditional job 
    is default), worker and occupation fixed effects, a quartic
    in age and job tenure, ", y, 
    ", union status, dummies for region, whether in an MSA 
    or central city, and marital status. All observations are
    at the ", sample, " level,", s_explanation, ". 
    All regressions are weighted at the person level 
    and all standard errors are clustered by demographic sample.
    Stars represent significance at the .10 level (*), .05 level (**),
    and .01 level (***).
    \\item[a] ", note_access
  )
  
  bot_2 <- make_bot(note)
  
  write.table(str_c(d_header, top_2, center_2, bot_2),
              str_c(d_table_folder, "Job Regressions Alternative ",
                    s_save, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Also save to Slides
  write.table(str_c(s_header, top_2_s, center_2, s_bot),
              str_c(s_table_folder, "Job Regressions Alternative ",
                    s_save, ".tex"),
              quote=F, col.names=F, row.names=F, sep="")
}


# Type Regressions --------------------------------------------------------

# + getting correct coeffs.
# How much does type matter? Run full regression but add types 
# in groups
# 1. Just outsourced vs not
# 2. Separate self_emp and indep_con
# 3. Separate on_call and temp_work
keep_var <- function(model, var, keep) {
  if (keep == F) {
    c(" & {--}", " & ")
  } else {
    stars <- p_stars(model$p.value[var])
    val <- format_val(model$coefficients[var], r=3, s=3, star = stars)
    se <- format_se(model$std.error[var], r=3, s=3)
    c(val, se)
  }
}

top <- "\\begin{tabular}{lSSS}
\\toprule
& {Outsourced} & {Self-Employed} & {Full} \\\\\\midrule
"

name_r <- "Quality of Outsourced Jobs, Comparison Robustness"

# Controls in full regression
controls <- c("age", "I((age)^2)", "I((age)^3)", "I((age)^4)",
              "factor(union_fill)")

tenure <- c("I((tenure/100))", "I((tenure/100)^2)", 
            "I((tenure/100)^3)", "I((tenure/100)^4)")

ind_vars <- c(controls, tenure)

fixed_effects <- c("region", "marital_status", "msa", "case_id", "occ")

center_t <- rbind("Outsourced", "", "Self-Employed", "",
                  "Independent", "Contractor", "On-Call", "",
                  "Temp Worker", "", "$R^2$", "Observations")

for (loop in 1:2) {
  
  sample <- samples[loop]
  s_label <- s_labels[loop]
  s_save <- s_saves[loop]
  s_explanation <- s_explanations[loop]
  y <- if (loop == 1) " and year" else "and year started and ended job"
  
  for (ind in seq_along(var_r)) {
    
    center <- c(center_t)
    
    var <- var_r[ind]
    var_name <- var_names[ind]
    label <- labels[ind]
    save <- str_c(" ", save_names[ind])
    name <- str_c(name_r, title_names[ind], sep = ": ")
    
    for (reg_ind in c(1:3)){
      
      if (loop == 1) {
        fe_vars <- c(fixed_effects, "int_year")
      } else {
        fe_vars <- c(fixed_effects, "I(year(week_start_job))",
                     "I(year(week_end_job))")
      }
      
      r_types <- c("outsourced")
      r_types <- 
        if (reg_ind >= 2) c(r_types, "self_emp", "indep_con") else r_types
      r_types <- 
        if (reg_ind == 3) c(r_types, "on_call", "temp_work") else r_types
      
      ind_vars_t <- c(ind_vars, r_types)
      
      eq <- create_formula(var, ind_vars_t)
      fe <- create_formula("", fe_vars)
      
      # Run regression both for matched and matched_jobs
      temp <- lm_robust(eq, data = dfs[[loop]], weights = weight,
                        subset = !is.na(region) & !is.na(msa)
                        & !is.na(marital_status) & !is.na(occ),
                        fixed_effects = !!fe,
                        clusters = as_factor(sample_id),
                        se_type = "stata", try_cholesky = T)
      
      keep_se <- if (reg_ind >= 2) TRUE else FALSE
      keep_oc <- if (reg_ind == 3) TRUE else FALSE
      
      ou <- keep_var(temp, "outsourced", TRUE)
      se <- keep_var(temp, "self_emp", keep_se)
      ic <- keep_var(temp, "indep_con", keep_se)
      oc <- keep_var(temp, "on_call", keep_oc)
      tw <- keep_var(temp, "temp_work", keep_oc)
      
      center <- cbind(
        center,
        rbind(ou[1], ou[2],
              se[1], se[2],
              ic[1], ic[2],
              oc[1], oc[2],
              tw[1], tw[2],
              format_val(temp$r.squared),
              format_n(lm_N(temp))
        )
      )
    }
    
    center <- center |>
      add_endline() |>
      center_to_latex()
    
    header <- make_header("", name, 
                          str_c("type_reg_", label, s_label))
    d_header <- make_header("d", name, 
                            str_c("type_reg_", label, s_label))
    s_header <- make_header("s", size = "\\footnotesize")
    
    note <- str_c(
    "Regressions of job type on ", var_name, ". 
    Data comes from the NLSY. Missing type in final row 
    is traditional jobs. All regressions use worker and 
    occupation fixed effects and include a quartic in age 
    and job tenure", y, ", union status,
    dummies for region, whether in an MSA or central city, and
    marital status. All observations are at the ", sample,
    " level", s_explanation, ". All regressions are weighted at 
    the person level, and all standard errors are 
    clustered by demographic sample. Stars represent significance 
    at the .10 level (*), .05 level (**), and .01 level (***).
    ")
    
    bot <- make_bot(note)
    
    write.table(str_c(header, top, center, bot, "\n \\end{document}"),
                str_c(table_folder, "NLSY79 Regressions/",
                      s_save, save, " Types.tex"),
                quote=F, col.names=F, row.names=F, sep="")
    
    # If looking at log real tot comp or health insurance,
    # save in Drafts and Slides
    if (var %in% c("log_real_tot_comp", "health")) {
      write.table(str_c(d_header, top, center, bot),
                  str_c(d_table_folder, "Job Regressions", save,
                        " ", s_save, " Types.tex"),
                  quote=F, col.names=F, row.names=F, sep="")
      
      write.table(str_c(s_header, top, center, s_bot),
                  str_c(s_table_folder, "Job Regressions", save,
                        " ", s_save, " Types.tex"),
                  quote=F, col.names=F, row.names=F, sep="")
    }
    
    # Code sometimes very slow here.
    # Maybe deleting some files will help
    rm("temp", "eq", "fe", "center", "ou", "se", "ic", "oc", "tw")
    
  }
}

# Education Regressions ---------------------------------------------------

# What if we interact job status with education level?
# Use full regressions with worker fixed effects.
# Have dummies for each type * educ except traditional
# Also run regressions using only high vs low education

# Type of job (compare to traditional)
types <- c("outsourced", "self_emp", "indep_con", 
           "temp_work", "on_call")

# Education Level
educ_levels <- c("less_hs", "hs", "aa", "ba", "plus_ba",
                 "educ_other")

# Combine types and education levels
educ_type <- str_c("I(", rep(types, each=6),
                   " * ", rep(educ_levels, times=5), ")")

# If only looking at high vs low education
educ_hl <- 
  c(str_c("I(", types, " * ", rep("(1 - high_ed)", times=5), ")"),
    str_c("I(", types, " * ", rep("high_ed", times=5), ")"))

# Other Controls
controls <- c("age", "I((age)^2)", "I((age)^3)", "I((age)^4)",
              "factor(union_fill)")

tenure <- c("I((tenure/100))", "I((tenure/100)^2)", 
            "I((tenure/100)^3)", "I((tenure/100)^4)")

ind_vars <- c(controls, tenure, educ_type)
ind_vars_hl <- c(controls, tenure, educ_hl)

fixed_effects <- c("region", "marital_status", "msa", "case_id", "occ")

center_e <- c("Less High School $\\times$", "Outsourced",
              "High School $\\times$", "Outsourced",
              "Associate's Degree $\\times$", "Outsourced",
              "Bachelor's Degree $\\times$", "Outsourced",
              "Postgraduate Degree $\\times$", "Outsourced",
              "$R^2$", "Observations")

center_hl <- c("No Bachelor's $\\times$", "Outsourced",
               "Bachelor's $\\times$", "Outsourced",
               "$R^2$", "Observations")

name_e_1 <- "Quality of Outsourced Jobs by Education"
name_e_2 <- "Quality of Outsourced Jobs by Education: Alternative Measures"
name_hl_1 <- "Quality of Outsourced Jobs by Bachelor's Degree Attainment"
name_hl_2 <- "Quality of Outsourced Jobs by Bachelor's Degree Attainment: Alternative Measures"

for (loop in 1:2) {

  sample <- samples[loop]
  s_label <- s_labels[loop]
  s_save <- s_saves[loop]
  s_explanation <- s_explanations[loop]
  y <- if (loop == 1) " and year" else "and year started and ended job"
  c_1 <- c()
  c_2 <- c()
  c_1_hl <- c()
  c_2_hl <- c()

  for (ind in seq_along(var_r)) {

    var <- var_r[ind]
    var_name <- var_names[ind]
    label <- labels[ind]

    if (loop == 1) {
      fe_vars <- c(fixed_effects, "int_year")
    } else {
      fe_vars <- c(fixed_effects, "I(year(week_start_job))",
                   "I(year(week_end_job))")
    }

    eq <- create_formula(var, ind_vars)
    eq_hl <- create_formula(var, ind_vars_hl)
    fe <- create_formula("", fe_vars)

    # Run regression both for all education levels
    temp <- lm_robust(eq, data = dfs[[loop]], weights = weight,
                      subset = !is.na(region) & !is.na(msa)
                      & !is.na(marital_status) & !is.na(occ),
                      fixed_effects = !!fe,
                      clusters = as_factor(sample_id),
                      se_type = "stata", try_cholesky = T)
    
    # Run regressions for just low or high education
    temp_hl <- lm_robust(eq_hl, data = dfs[[loop]], weights = weight,
                         subset = !is.na(region) & !is.na(msa)
                         & !is.na(marital_status) & !is.na(occ)
                         & !is.na(high_ed),
                         fixed_effects = !!fe,
                         clusters = as_factor(sample_id),
                         se_type = "stata", try_cholesky = T)
    
    # Depending on variable, put in right table
    if (var %in% var_1) {
      c_1 <- cbind(
        c_1,
        rbind(
          format_val(temp$coefficients[educ_type[1]], r=3, s=3,
                     star=p_stars(temp$p.value[educ_type[1]])),
          format_se(temp$std.error[educ_type[1]], r=3, s=3),
          format_val(temp$coefficients[educ_type[2]], r=3, s=3,
                     star=p_stars(temp$p.value[educ_type[2]])),
          format_se(temp$std.error[educ_type[2]], r=3, s=3),
          format_val(temp$coefficients[educ_type[3]], r=3, s=3,
                     star=p_stars(temp$p.value[educ_type[3]])),
          format_se(temp$std.error[educ_type[3]], r=3, s=3),
          format_val(temp$coefficients[educ_type[4]], r=3, s=3,
                     star=p_stars(temp$p.value[educ_type[4]])),
          format_se(temp$std.error[educ_type[4]], r=3, s=3),
          format_val(temp$coefficients[educ_type[5]], r=3, s=3,
                     star=p_stars(temp$p.value[educ_type[5]])),
          format_se(temp$std.error[educ_type[5]], r=3, s=3),
          format_val(temp$r.squared), format_n(lm_N(temp)))
      )
      
      c_1_hl <- cbind(
        c_1_hl,
        rbind(
          format_val(temp_hl$coefficients[educ_hl[1]], r=3, s=3,
                     star=p_stars(temp_hl$p.value[educ_hl[1]])),
          format_se(temp_hl$std.error[educ_hl[1]], r=3, s=3),
          format_val(temp_hl$coefficients[educ_hl[6]], r=3, s=3,
                     star=p_stars(temp_hl$p.value[educ_hl[6]])),
          format_se(temp_hl$std.error[educ_hl[6]], r=3, s=3),
          format_val(temp_hl$r.squared), format_n(lm_N(temp_hl)))
      )
    }
    
    if (var %in% var_2) {
      c_2 <- cbind(
        c_2,
        rbind(
          format_val(temp$coefficients[educ_type[1]], r=3, s=3,
                     star=p_stars(temp$p.value[educ_type[1]])),
          format_se(temp$std.error[educ_type[1]], r=3, s=3),
          format_val(temp$coefficients[educ_type[2]], r=3, s=3,
                     star=p_stars(temp$p.value[educ_type[2]])),
          format_se(temp$std.error[educ_type[2]], r=3, s=3),
          format_val(temp$coefficients[educ_type[3]], r=3, s=3,
                     star=p_stars(temp$p.value[educ_type[3]])),
          format_se(temp$std.error[educ_type[3]], r=3, s=3),
          format_val(temp$coefficients[educ_type[4]], r=3, s=3,
                     star=p_stars(temp$p.value[educ_type[4]])),
          format_se(temp$std.error[educ_type[4]], r=3, s=3),
          format_val(temp$coefficients[educ_type[5]], r=3, s=3,
                     star=p_stars(temp$p.value[educ_type[5]])),
          format_se(temp$std.error[educ_type[5]], r=3, s=3),
          format_val(temp$r.squared), format_n(lm_N(temp)))
      )
      
      c_2_hl <- cbind(
        c_2_hl,
        rbind(
          format_val(temp_hl$coefficients[educ_hl[1]], r=3, s=3,
                     star=p_stars(temp_hl$p.value[educ_hl[1]])),
          format_se(temp_hl$std.error[educ_hl[1]], r=3, s=3),
          format_val(temp_hl$coefficients[educ_hl[6]], r=3, s=3,
                     star=p_stars(temp_hl$p.value[educ_hl[6]])),
          format_se(temp_hl$std.error[educ_hl[6]], r=3, s=3),
          format_val(temp_hl$r.squared), format_n(lm_N(temp_hl)))
      )
    }

    # Save regressions with all education levels
    center_1 <- center_e |>
      cbind(c_1) |>
      add_endline() |>
      center_to_latex()
    
    center_2 <- center_e |>
      cbind(c_2) |>
      add_endline() |>
      center_to_latex()
  
    label <- str_c("ed_regs", s_label)
    note <- str_c(
      "Regressions of worker outsourcing status by education level
      on job outcomes. Data comes from the NLSY.
      Regressions control for education dummies multiplied by
      job type (default is traditional). Regressions also include
      worker and occupation fixed effects, a quartic in age
      and job tenure, ", y, ", union status, dummies for region,
      whether in an MSA or central city, and marital status.
      All observations are at the ", sample, " level", s_explanation,
      ". All regressions are weighted at the person level, and all 
      standard errors are clustered by demographic sample.
      Stars represent significance at the .10 level (*), .05 level (**),
      and .01 level (***).
      \\item[a] ", note_access, "
      \\item[b] ", note_tot_comp
    )
    
    label_2 <- str_c("alt_ed_regs", s_label)
    note_2 <- str_c(
      "Regressions of worker outsourcing status by education level
      on job outcomes. Data comes from the NLSY.
      Regressions control for education dummies multiplied by
      job type (default is traditional). Regressions also include
      worker and occupation fixed effects, a quartic in age
      and job tenure, ", y, ", union status, dummies for region,
      whether in an MSA or central city, and marital status.
      All observations are at the ", sample, " level", s_explanation,
      ". All regressions are weighted at the person level, and all 
      standard errors are clustered by demographic sample.
      Stars represent significance at the .10 level (*), .05 level (**),
      and .01 level (***).
      \\item[a] ", note_access
    )
    
    header <- make_header("", name_e_1, label, colsep = 0.75)
    d_header <- make_header("d", name_e_1, label, colsep = 0.75)
    s_header <- make_header("s", size = "\\footnotesize")
    
    header_2 <- make_header("", name_e_2, label, colsep = 0.75)
    d_header_2 <- make_header("d", name_e_2, label, colsep = 0.75)
    
    bot_1 <- make_bot(note)
    bot_2 <- make_bot(note_2)
  
    write.table(str_c(header, top_1, center_1, bot_1, "\n \\end{document}"),
                str_c(table_folder, "NLSY79 Regressions/",
                      s_save, " Education All.tex"),
                quote=F, col.names=F, row.names=F, sep="")
    
    write.table(str_c(header_2, top_2, center_2, bot_2, "\n \\end{document}"),
                str_c(table_folder, "NLSY79 Regressions/",
                      s_save, " Education All Alternative.tex"),
                quote=F, col.names=F, row.names=F, sep="")
    
    # For Jobs, also save for Drafts and Slides
    if (loop == 2) {
      write.table(str_c(d_header, top_1, center_1, bot_1),
                  str_c(d_table_folder, s_save, 
                        " Education Regressions All.tex"),
                  quote=F, col.names=F, row.names=F, sep="")
      
      write.table(str_c(s_header, top_1_s, center_1, s_bot),
                  str_c(s_table_folder, s_save,
                        " Education Regressions All.tex"),
                  quote=F, col.names=F, row.names=F, sep="")
      
      write.table(str_c(d_header_2, top_2, center_2, bot_2),
                  str_c(d_table_folder, s_save, 
                        " Education Regressions All Alternative.tex"),
                  quote=F, col.names=F, row.names=F, sep="")
      
      write.table(str_c(s_header, top_2_s, center_2, s_bot),
                  str_c(s_table_folder, s_save,
                        " Education Regressions All Alternative.tex"),
                  quote=F, col.names=F, row.names=F, sep="")
    }
    
    # Save regressions with high/low education
    center_1_hl <- center_hl |>
      cbind(c_1_hl) |>
      add_endline() |>
      center_to_latex()
    
    center_2_hl <- center_hl |>
      cbind(c_2_hl) |>
      add_endline() |>
      center_to_latex()
    
    label <- str_c("hl_regs", s_label)
    note <- str_c(
      "Regressions of worker outsourcing status by bachelor's
      degree attainment on job outcomes. Data comes from the NLSY. 
    Regressions control for bachelor's attainment multiplied by
    job type (default is traditional). Regressions also include
    worker and occupation fixed effects, a quartic in age
    and job tenure, ", y, ", union status, dummies for region,
    whether in an MSA or central city, and marital status.
    All observations are at the ", sample, " level", s_explanation,
      ". All regressions are weighted at the person level, and all 
    standard errors are clustered by demographic sample.
    Stars represent significance at the .10 level (*), .05 level (**),
    and .01 level (***).
    \\item[a] ", note_access, "
    \\item[b] ", note_tot_comp
    )
    
    label_2 <- str_c("alt_ed_regs", s_label)
    note_2 <- str_c(
      "Regressions of worker outsourcing status by bachelor's
      degree attainment on job outcomes. Data comes from the NLSY. 
    Regressions control for bachelor's attainment multiplied by
    job type (default is traditional). Regressions also include
    worker and occupation fixed effects, a quartic in age
    and job tenure, ", y, ", union status, dummies for region,
    whether in an MSA or central city, and marital status.
    All observations are at the ", sample, " level", s_explanation,
      ". All regressions are weighted at the person level, and all 
    standard errors are clustered by demographic sample.
    Stars represent significance at the .10 level (*), .05 level (**),
    and .01 level (***).
    \\item[a] ", note_access
    )
    
    header <- make_header("", name_hl_1, label, colsep = 0.75)
    d_header <- make_header("d", name_hl_1, label, colsep = 0.75)
    s_header <- make_header("s", size = "\\small")
    
    header_2 <- make_header("", name_hl_2, label, colsep = 0.75)
    d_header_2 <- make_header("d", name_hl_2, label, colsep = 0.75)
    
    bot_1 <- make_bot(note)
    bot_2 <- make_bot(note_2)
    
    write.table(str_c(header, top_1, center_1_hl, bot_1, "\n \\end{document}"),
                str_c(table_folder, "NLSY79 Regressions/",
                      s_save, " Education HL.tex"),
                quote=F, col.names=F, row.names=F, sep="")
    
    write.table(str_c(header_2, top_2, center_2_hl, bot_2, "\n \\end{document}"),
                str_c(table_folder, "NLSY79 Regressions/",
                      s_save, " Education HL Alternative.tex"),
                quote=F, col.names=F, row.names=F, sep="")
    
    # For Jobs, also save for Drafts and Slides
    if (loop == 2) {
      write.table(str_c(d_header, top_1, center_1_hl, bot_1),
                  str_c(d_table_folder, s_save, 
                        " Education Regressions HL.tex"),
                  quote=F, col.names=F, row.names=F, sep="")
      
      write.table(str_c(s_header, top_1_s, center_1_hl, s_bot),
                  str_c(s_table_folder, s_save,
                        " Education Regressions HL.tex"),
                  quote=F, col.names=F, row.names=F, sep="")
      
      write.table(str_c(d_header_2, top_2, center_2_hl, bot_2),
                  str_c(d_table_folder, s_save, 
                        " Education Regressions HL Alternative.tex"),
                  quote=F, col.names=F, row.names=F, sep="")
      
      write.table(str_c(s_header, top_2_s, center_2_hl, s_bot),
                  str_c(s_table_folder, s_save,
                        " Education Regressions HL Alternative.tex"),
                  quote=F, col.names=F, row.names=F, sep="")
    }
  
  }
}

# Plots -------------------------------------------------------------------

# Use ggplot2 to graph some things
# Graph some variables by year for outsourced vs traditional
# Both all workers and ever_ho_occ

# Also plot log_real_wkly_wage/tot_comp residuals 
# from full FE regression about (but without outsourcing)
# See how these compare for outsourced vs traditional

# Create filter_ever which filters by ever_ho_occ if condition is TRUE
filter_ever <- function(df, condition) {
  if (condition) {
    filter(df, ever_ho_occ == 1)
  } else {
    df
  }
}

var_g <- c("log_real_hrly_wage", "log_real_wkly_wage", "log_real_tot_comp",
           "hours_week", "tenure")

var_names <- c("Log Real Hourly Wage", "Log Real Weekly Wage", 
               "Log Real Total Compensation",
               "Hours Worked per Week", "Weeks of Tenure")

var_saves <- c("LRH Wage", "LRW Wage", "LR Tot Comp", "Hours", "Tenure")

types <- c("self_emp", "indep_con", "temp_work", "on_call")

controls <- c("age", "I((age)^2)", "I((age)^3)", "I((age)^4)",
              "factor(union_fill)")

tenure <- c("I((tenure/100))", "I((tenure/100)^2)",
            "I((tenure/100)^3)", "I((tenure/100)^4)")

fixed_effects <- c("region", "marital_status", "msa", "occ", "case_id")

dfs <- c(split_data(matched), split_data(matched_jobs))
s_saves <- c("Years ", "Jobs ")
ho_saves <- c("", " Ever HO Occupation")

c_i <- c()

for (ob in 1:2) {
  
  c_i <- rbind(
    c_i,
    cbind(" & ", " & ", " & ", " & ", " & ", " & ", " & ", " & ")
  )
  
  for (i in seq_along(var_g)) {
    
    var_sym <- sym(var_g[i])
    # Do extra things for lrww and lrtc. Create a condition if these vars
    condition <- var_g[i] %in% c("log_real_wkly_wage", "log_real_tot_comp")
    lrtc <- var_g[i] == "log_real_tot_comp"
    
    # Will plot residuals from regressions below for 
    # weekly wages and total compensation. 
    # Run regressions with full data. 
    if (condition) {
      
      ind_vars <- c(controls, tenure, types)
      
      if (ob == 1) {
        fe_vars <- c(fixed_effects, "int_year")
      } else {
        fe_vars <- c(fixed_effects, "I(year(week_start_job))",
                     "I(year(week_end_job))")
      }
      
      eq <- create_formula(var_g[i], ind_vars)
      fe <- create_formula("~", fe_vars)
      
      k <- if (ob == 1) 1 else 3
      reg <- lm_robust(eq, data = dfs[[k]],
                       subset = !is.na(region) & !is.na(msa)
                       & !is.na(marital_status) & !is.na(occ),
                       weights = weight,
                       fixed_effects = !!fe,
                       clusters = sample_id, 
                       se_type = "stata", try_cholesky = T)
    }
    
    for (ho in 1:2) {
      
      j <- 2 * (ob - 1) + ho 
      
      # Plot raw figures
      temp <- dfs[[j]] |>
        filter(!is.na(!!var_sym), 
               (outsourced == 1 | traditional == 1)) |>
        ggplot(aes_string(var_g[i], fill = "factor(outsourced)")) +
        geom_density(alpha = 0.2) +
        labs(x = var_names[i], y = "Density") +
        scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                          values = c("blue", "red"),
                          labels = c("Traditional", "Outsourced")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        theme_light(base_size = 16) 
      
      ggsave(str_c(figure_folder, s_saves[ob],
                   var_saves[i], ho_saves[ho], ".pdf"),
             height = height, width = width)
    
      # If looking at lrww or lrtc, plot residuals too. 
      if (condition) {
        df <- dfs[[k]] |> 
          filter(!is.na(!!var_sym), !is.na(tenure), 
                 !is.na(union_fill), !is.na(region), 
                 !is.na(marital_status),
                 !is.na(msa), !is.na(occ)) |> 
          # mutate(residual = lm_residuals(reg, TRUE)) |> 
          lm_mutate_residuals(reg) |>
          filter_ever(ho == 2) |> 
          filter((outsourced == 1 | traditional == 1))
        
        # Observations with unique case_ids/occupations have
        # artificially 0 residuals
        # Keep only people/occupations with multiple observations
        temp <- df |> 
          filter(high_ed == 0) |>
          ggplot(aes(residual, fill = factor(outsourced))) +
          geom_density(alpha = 0.2) +
          labs(x = str_c("Residual ", var_names[i]), y = "Density") +
          scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                            values = c("blue", "red"),
                            labels = c("Traditional", "Outsourced")) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
          theme_light(base_size = 16) 
        
        ggsave(str_c(figure_folder, s_saves[ob], var_saves[i],
                     " Residuals", ho_saves[ho], " Low Ed.pdf"),
               height = height, width = width)
        
        temp <- df |> 
          filter(high_ed == 1) |>
          ggplot(aes(residual, fill = factor(outsourced))) +
          geom_density(alpha = 0.2) +
          labs(x = str_c("Residual ", var_names[i]), y = "Density") +
          scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                            values = c("blue", "red"),
                            labels = c("Traditional", "Outsourced")) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
          theme_light(base_size = 16) 
        
        ggsave(str_c(figure_folder, s_saves[ob], var_saves[i],
                     " Residuals", ho_saves[ho], " High Ed.pdf"),
               height = height, width = width)
          
        # If job - year ob == 2, total compensation lrtc == TRUE, 
        # and ho workers ho == 2
        if (ob == 2 & lrtc & ho == 2) {
          # Update with mean lrtc for both job types
          # divided by low and high education.
          # To do this, take overall mean total compensation
          # (for all types, including discarded ones)
          # Because this is what the regression was run on.
          # Then take average residual of each type and add together
          # Recreate this df just to find mean wage of entire 
          # regression sample
          df_mean <- dfs[[k]] |> 
            filter(!is.na(!!var_sym), !is.na(tenure), 
                   !is.na(union_fill), !is.na(region),
                   !is.na(marital_status),
                   !is.na(msa), !is.na(occ))
          
          # Frequently used conditions
          cond_t <- df$traditional == 1
          cond_o <- df$outsourced == 1
          cond_l <- (df$high_ed == 0) %in% TRUE
          cond_h <- (df$high_ed == 1) %in% TRUE
          
          # Get mean compensation by education level. 
          # This is the baseline that we will add back to 
          # residuals (worker fixed effects means we must 
          # separate these by education)
          w_bar_l <- 
            weighted.mean(
              df_mean$log_real_tot_comp[(df_mean$high_ed == 0) %in% TRUE],
              df_mean$weight[(df_mean$high_ed == 0) %in% TRUE], na.rm = T)
          w_bar_h <- 
            weighted.mean(
              df_mean$log_real_tot_comp[(df_mean$high_ed == 1) %in% TRUE],
              df_mean$weight[(df_mean$high_ed == 1) %in% TRUE], na.rm = T)
          
          m_resid_l <- weighted.mean(
            df$residual[cond_t & cond_l],
            df$weight[cond_t & cond_l], na.rm = T)
          
          m_resid_o_l <- weighted.mean(
            df$residual[cond_o & cond_l],
            df$weight[cond_o & cond_l], na.rm = T)
          
          m_resid_h <- weighted.mean(
            df$residual[cond_t & cond_h],
            df$weight[cond_t & cond_h], na.rm = T)
          
          m_resid_o_h <- weighted.mean(
            df$residual[cond_o & cond_h],
            df$weight[cond_o & cond_h], na.rm = T)
          
          w_bar_l <- w_bar_l + m_resid_l
          w_o_bar_l <- w_bar_l + m_resid_o_l
          
          w_bar_h <- w_bar_h + m_resid_h
          w_o_bar_h <- w_bar_h + m_resid_o_h
          
          # Also find sd of residuals for each type
          var_resid_l <- (
            sum(df$weight[cond_t & cond_l] 
                * (df$residual[cond_t & cond_l] - m_resid_l)^2) /
              ((length(df$weight[cond_t & cond_l] != 0) - 1) /
                 length(df$weight[cond_t & cond_l] != 0)
               * sum(df$weight[cond_t & cond_l]))) 
          w_sd_l <- sqrt(var_resid_l)
          
          var_resid_o_l <- (
            sum(df$weight[cond_o & cond_l] * 
                  (df$residual[cond_o & cond_l] - m_resid_o_l)^2) /
              ((length(df$weight[cond_o & cond_l] != 0) - 1) / 
                 length(df$weight[cond_o & cond_l] != 0)
               * sum(df$weight[cond_o & cond_l]))) 
          w_o_sd_l <- sqrt(var_resid_o_l)
          
          var_resid_h <- (
            sum(df$weight[cond_t & cond_h] * 
                  (df$residual[cond_t & cond_h] - m_resid_h)^2) /
              ((length(df$weight[cond_t & cond_h] != 0) - 1) /
                 length(df$weight[cond_t & cond_h] != 0)
               * sum(df$weight[cond_t & cond_h]))) 
          w_sd_h <- sqrt(var_resid_h)
          
          var_resid_o_h <- (
            sum(df$weight[cond_o & cond_h] * 
                  (df$residual[cond_o & cond_h] - m_resid_o_h)^2) /
              ((length(df$weight[cond_o & cond_h] != 0) - 1) / 
                 length(df$weight[cond_o & cond_h] != 0)
               * sum(df$weight[cond_o & cond_h]))) 
          w_o_sd_h <- sqrt(var_resid_o_h)
          
          # Test out using 25th and 75th quartiles
          quantiles_l <- wtd.quantile(df$residual[cond_t & cond_l], 
                                    na.rm = TRUE, 
                                    df$weight[cond_t & cond_l])
          w_25_l <- w_bar_l + quantiles_l[2]
          w_75_l <- w_bar_l + quantiles_l[4]
          
          quantiles_o_l <- wtd.quantile(df$residual[cond_o & cond_l], 
                                    na.rm = TRUE, 
                                    df$weight[cond_o & cond_l])
          w_o_25_l <- w_bar_l + quantiles_o_l[2]
          w_o_75_l <- w_bar_l + quantiles_o_l[4]
          
          quantiles_h <- wtd.quantile(df$residual[cond_t & cond_h], 
                                      na.rm = TRUE, 
                                      df$weight[cond_t & cond_h])
          w_25_h <- w_bar_h + quantiles_h[2]
          w_75_h <- w_bar_h + quantiles_h[4]
          
          quantiles_o_h <- wtd.quantile(df$residual[cond_o & cond_h], 
                                        na.rm = TRUE, 
                                        df$weight[cond_o & cond_h])
          w_o_25_h <- w_bar_h + quantiles_o_h[2]
          w_o_75_h <- w_bar_h + quantiles_o_h[4]
          
          data_moments <- update_parameters("w_bar_l", w_bar_l)
          data_moments <- update_parameters("w_o_bar_l", w_o_bar_l)
          data_moments <- update_parameters("w_sd_l", w_sd_l)
          data_moments <- update_parameters("w_o_sd_l", w_o_sd_l)
          data_moments <- update_parameters("w_25_l", w_25_l)
          data_moments <- update_parameters("w_o_25_l", w_o_25_l)
          data_moments <- update_parameters("w_75_l", w_75_l)
          data_moments <- update_parameters("w_o_75_l", w_o_75_l)
          
          data_moments <- update_parameters("w_bar_h", w_bar_h)
          data_moments <- update_parameters("w_o_bar_h", w_o_bar_h)
          data_moments <- update_parameters("w_sd_h", w_sd_h)
          data_moments <- update_parameters("w_o_sd_h", w_o_sd_h)
          data_moments <- update_parameters("w_25_h", w_25_h)
          data_moments <- update_parameters("w_o_25_h", w_o_25_h)
          data_moments <- update_parameters("w_75_h", w_75_h)
          data_moments <- update_parameters("w_o_75_h", w_o_75_h)
          
          # Plot ecdf for low and high education full-time workers
          temp <- df |> 
            filter(high_ed == 0) |>
            ggplot(aes(residual, color = factor(outsourced))) +
            stat_ecdf(geom = "line") +
            labs(x = str_c("Residual ", var_names[i]), y = "CDF") +
            scale_color_manual(name = "Job Type", breaks = c(0, 1),
                              values = c("blue", "red"),
                              labels = c("Traditional", "Outsourced")
                              ) +
            theme_light(base_size = 16) 
          
          ggsave(str_c(figure_folder, s_saves[ob], var_saves[i],
                       " Residual CDF", ho_saves[ho], " Low Ed.pdf"),
                 height = height, width = width)
            
          # Save this data set, rename x as wage and add w/w_o bar to 
          # revolve around mean wage overall. Rename y as cdf, 
          # group as outsourced discard rest
          ecdf_l <- ggplot_build(temp)$data[[1]] |> 
            filter(x > -15, x < 15) |> # filter out -Inf and Inf 
            rename(density = y) |> 
            mutate(wage = w_bar_l + x,
                   outsourced = group - 1) |> 
            select(outsourced, wage, density)
          
          temp <- df |> 
            filter(high_ed == 1) |>
            ggplot(aes(residual, color = factor(outsourced))) +
            stat_ecdf(geom = "line") +
            labs(x = str_c("Residual ", var_names[i]), y = "CDF") +
            scale_color_manual(name = "Job Type", breaks = c(0, 1),
                               values = c("blue", "red"),
                               labels = c("Traditional", "Outsourced")
            ) +
            theme_light(base_size = 16) 
          
          ggsave(str_c(figure_folder, s_saves[ob], var_saves[i],
                       " Residual CDF", ho_saves[ho], " High Ed.pdf"),
                 height = height, width = width)
          
          
          ecdf_h <- ggplot_build(temp)$data[[1]] |> 
            filter(x > -15, x < 15) |> # filter out -Inf and Inf 
            rename(density = y) |> 
            mutate(wage = w_bar_h + x,
                   outsourced = group - 1) |> 
            select(outsourced, wage, density)
          }
        }
      }
      
      # Also plot job_sat as a density histogram (only do this once)
      if (i == 1) {
        temp <- dfs[[j]] |>
          filter(!is.na(job_sat), 
                 (outsourced == 1 | traditional == 1)) |>
          ggplot(aes(round(job_sat, 0), fill = factor(outsourced))) +
          geom_histogram(aes(y=.5*..density..), 
                         alpha=0.5, position="dodge",
                         binwidth = .5) +
          labs(x = "Job Satisfaction", y = "Density") +
          scale_fill_manual(name = "Job Type", breaks = c(0, 1),
                            values = c("blue", "red"),
                            labels = c("Traditional", "Outsourced")) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
          theme_light(base_size = 16) 
        
        ggsave(str_c(figure_folder, s_saves[ob],
                     "Job Satisfaction", ho_saves[ho], ".pdf"),
               height = height, width = width)
      }
    }
  }

write_csv(data_moments, str_c(clean_folder, "data_moments.csv"))
write_csv(ecdf_l, str_c(clean_folder, "data_ecdf_l.csv"))
write_csv(ecdf_h, str_c(clean_folder, "data_ecdf_h.csv"))

# Remove things to save space
rm("add_ho", "add_out", "c_1", "c_1_hl", "c_2", "c_2_hl", "c_center",
   "c_i", "center_d", "center_js_r", "center_r", "center_r_s", "col_i",
   "comp", "comp_js", "comp_r", "data_moments", "df", "df_mean", "dfs",
   "ecdf_h", "ecdf_l", "ho_sum", "m_j_dt", "multi_year", "n_center",
   "reg", "space", "splits", "temp", "temp_hl")

# Wage Distribution HO Occs -----------------------------------------------

# Look at log real weekly wage in matched_jobs for traditional
# ho_occ vs not. Also run a full regression (minus occ fe) 
# and compare residuals. Do this for workers overall and with
# less than/more than a BA.

wd_data <- list(matched_jobs, 
                filter(matched_jobs, high_ed == 0),
                filter(matched_jobs, high_ed == 1))

wd_saves <- c("All", "Low Education", "High Education")

# For RIF
quantiles <- seq(0.75, 0.95, by = 0.05)

controls <- c("age", "I((age)^2)", "I((age)^3)", "I((age)^4)",
              "factor(union_fill)")

tenure <- c("I((tenure/100))", "I((tenure/100)^2)",
            "I((tenure/100)^3)", "I((tenure/100)^4)")

for (i in seq_along(wd_data)) {
  
  data <- wd_data[[i]]
  wd_save <- wd_saves[i]
  
  temp <- data |> 
    filter(!is.na(log_real_wkly_wage), !is.na(ho_occ)) |> 
    ggplot(aes(log_real_tot_comp, fill = factor(ho_occ))) +
    geom_density(alpha = 0.2) +
    labs(x = "Log Real Weekly Wage", y = "Density") +
    scale_fill_manual(name = "HO Occupation", breaks = c(0, 1),
                      values = c("blue", "red"),
                      labels = c("No", "Yes")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_light(base_size = 16) 
  
  ggsave(str_c(figure_folder, "Jobs LRW Wage HO Occ ",
               wd_save, ".pdf"),
         height = height, width = width)
  
  # # eq <- create_formula("log_real_wkly_wage", 
  # #                      c(controls, tenure, "outsourced", types))
  # eq <- create_formula("log_real_wkly_wage", c(controls, tenure))
  # fe <- create_formula("~", 
  #                      c("region", "marital_status", "msa", "case_id"))
  # 
  # reg <- lm_robust(eq, data,
  #                  subset = !is.na(region) & !is.na(msa) 
  #                  & !is.na(marital_status) & !is.na(ho_occ),
  #                  weights = weight,
  #                  fixed_effects = !!fe,
  #                  clusters = sample_id, 
  #                  se_type = "stata", try_cholesky = T)
  # 
  # df <- data |> 
  #   filter(!is.na(log_real_wkly_wage), !is.na(tenure), 
  #          !is.na(union_fill), !is.na(region), !is.na(marital_status),
  #          !is.na(msa), !is.na(ho_occ)) |>
  #   lm_mutate_residuals(reg) |> 
  #   filter(residual >= - 3.5 * sd(residual),
  #          residual <= 3.5 * sd(residual), traditional == 1)
  # 
  # df_sum <- df |> 
  #   as_survey_design(id = case_id, weight = weight) |> 
  #   group_by(ho_occ) |> 
  #   summarise(
  #     variance = survey_var(residual, vartype = "ci"),
  #     skew = unweighted(Skew(residual, conf.level = .95, 
  #                            ci.type = "basic")[1]),
  #     skew_low = unweighted(Skew(residual, conf.level = .95,
  #                                ci.type = "basic")[2]),
  #     skew_upp = unweighted(Skew(residual, conf.level = .95,
  #                                ci.type = "basic")[3]),
  #     obs = unweighted(n())
  #   ) |> 
  #   arrange(desc(ho_occ))
  # 
  # temp <- df |> 
  #   ggplot(aes(residual, fill = factor(ho_occ))) +
  #   geom_density(alpha = 0.2) +
  #   labs(x = str_c("Residual Log Real Weekly Wage"), y = "Density") +
  #   scale_fill_manual(name = "HO Occupation", breaks = c(0, 1),
  #                     values = c("blue", "red"),
  #                     labels = c("No", "Yes")) +
  #   scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  #   theme_light(base_size = 16) 
  # 
  # ggsave(str_c(figure_folder, "Jobs LRW Wage Residuals HO Occ ",
  #              wd_save, ".pdf"),
  #        height = height, width = width)
  
  # Get more sophisticated. Use RIF regressions on various
  # quartiles of the regression and plot how ho_occ affects lrw wages
  # Note: program can't handle fixed effects, so include everything
  # for now. Also can't subset, so need to ensure correct dataset upfront
  # (Note: this is very slow)
  data_rif <- data |>
    filter(!is.na(log_real_wkly_wage), !is.na(ho_occ), !is.na(region),
           !is.na(marital_status), !is.na(msa))

  eq_rif <- create_formula(
    "log_real_wkly_wage",
    c("ho_occ", controls, tenure, "factor(region)",
      "factor(marital_status)", "factor(msa)", "factor(case_id)")
    )

  reg_rif <- rifr(eq_rif, data_rif, weights = "weight",
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
    labs(x = "Quantile", y = "Effect of HO Occupation") +
    xlim(0.7, 1) +
    theme_light(base_size = 16)

  ggsave(str_c(figure_folder, "Jobs LRW Wage HO Occ RIF ",
               wd_save, ".pdf"),
         height = height, width = width)
}

# # Residual LR Tot Comp vs Max Tenure ----------------------------------------
# 
# # Plot Residual LRW Wages (without Tenure controls) vs Tenure
# # for outsourced and traditional
# types <- c("self_emp", "indep_con", "temp_work", "on_call")
# 
# controls <- c("age", "I((age)^2)", "I((age)^3)", "I((age)^4)",
#               "factor(union_fill)")
# 
# fixed_effects <- c("region", "marital_status", "msa", "occ", "case_id")
# 
# eq <- create_formula("log_real_tot_comp", c(types, controls))
# fe <- create_formula("~", fixed_effects)
# 
# reg <- lm_robust(eq, data = matched_jobs,
#                  subset = !is.na(region) & !is.na(msa)
#                  & !is.na(marital_status) & !is.na(occ),
#                  weights = weight,
#                  fixed_effects = !!fe,
#                  clusters = sample_id, 
#                  se_type = "stata", try_cholesky = T)
# 
# df <- matched_jobs |> 
#   filter(!is.na(log_real_wkly_wage),  
#          !is.na(union_fill), !is.na(region), !is.na(marital_status),
#          !is.na(msa), !is.na(occ), !is.na(outsourced)) |> 
#   lm_mutate_residuals(reg) |>
#   filter(ever_ho_occ == 1, (outsourced == 1 | traditional == 1))
# 
# temp <- df |>
#   ggplot(aes(x = residual, y = max_tenure, 
#              color = factor(outsourced))) +
#   geom_point(alpha = 0.25) +
#   geom_smooth(method = "lm", formula = y ~ x) +
#   labs(x = "Residual Log Real Total Compensation", y = "Max Tenure") +
#   scale_color_manual(name = "Job Type", breaks = c(0, 1),
#                      values = c("blue", "red"),
#                      labels = c("Traditional", "Outsourced")) +
#   theme_light(base_size = 16)
# 
# ggsave(
#   str_c(figure_folder,
#         "Jobs LR Tot Comp Residuals vs Max Tenure Ever HO Occupation.pdf"),
#   height = height, width = width)
