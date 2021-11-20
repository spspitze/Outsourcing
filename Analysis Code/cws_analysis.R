# This file analyzes cps_00009, which contains
# the 6 CWS supplements from 1995-2005 and 2017

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
folders <- name_folders("CWS")
c(raw_folder, clean_folder, table_folder, figure_folder,
  d_table_folder, s_table_folder) %<-% folders 

# For saving graphs
c(height, width) %<-% fig_size()

# Bottom of slide tables never changes, so make it once
s_bot <- make_bot(slide = TRUE)

# Read the data using cps's custom formulas
ddi <- read_ipums_ddi(str_c(raw_folder, "cws_raw_codebook.xml"))
cws <- read_ipums_micro(ddi)

rm("ddi")

# Clean CWS data ----------------------------------------------------------

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
names(cws) <- tolower(names(cws))
cws <- cws |>
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
    # Define as married if marst is 1 or 2 and single if 6
    married = 1 * (marst %in% c(1, 2)),
    single = 1 * (marst == 6),
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
    # For occ1990, replace various NA's, 
    # mark if janitor or security guard
    occ = ifelse(occ1990 < 991, occ1990, NA),
    janitor_occ = 1 * (occ == 453),
    sg_occ = 1 * (occ == 426),
    # For ind1990, replace various NA's, 
    # mark if janitor or security guard industry
    ind = ifelse(ind1990 < 998, ind1990, NA),
    janitor_ind = 1 * (ind == 722),
    sg_ind = 1 * (ind == 740),
    # Mark Industries in professional business services
    pbs = 1 * (ind >= 721 & ind <= 760),
    # Find log_real_wkly/hrly_wage, drop NA's and below 3 /hr or 60/wk
    log_real_hrly_wage = 
      ifelse(hourwage != 999.99 & hourwage >= 3, 
             log(hourwage / cpi * base_cpi), NA),
    log_real_wkly_wage = 
      ifelse(earnweek != 9999.99 & earnweek >= 60, 
             log(earnweek / cpi * base_cpi), NA),
    # Find birth_year if want to compare to NLSY cohort
    birth_year = year - age,
    nlsy79 = 1 * (birth_year >= 1957 & birth_year <= 1964),
    # Clean CWS specific variables
    temp_work = case_when(cwpdtag == 1 ~ 0, cwpdtag == 2 ~ 1, 
                          cwpdtag > 2 ~ NA_real_),
    on_call = case_when(cwoncall == 1 ~ 0,
                        (cwoncall >= 2) & (cwoncall <= 6) ~ 1, 
                        cwoncall > 6 ~ NA_real_),
    day_laborer = case_when(cwdaylab == 1 ~ 0, cwdaylab == 2 ~ 1,
                            cwdaylab > 2 ~ NA_real_),
    contract_work = case_when(cwcontract == 1 ~ 0, cwcontract == 2 ~ 1,
                              cwcontract > 2 ~ NA_real_),
    indep_con = case_when((cwcontractic == 1) | (cwseemp == 2) ~ 0,
                          (cwcontractic == 2) | (cwseemp == 1) ~ 1,
                          cwcontractic > 2 ~ NA_real_),
    self_emp = case_when(classwkr %in% c(13, 14) ~ 1,
                         classwkr < 99 ~ 0,
                         classwkr == 99 ~ NA_real_),
    # If another type, don't count as on_call or self_emp
    on_call = 
      ifelse(pmax(temp_work, day_laborer, contract_work,
                  indep_con, na.rm = T) == 1, 0, on_call),
    self_emp = 
      ifelse(pmax(temp_work, on_call, day_laborer, 
                  contract_work, indep_con, na.rm = T) == 1,
             0, self_emp),
    traditional = 
      (1 - pmax(temp_work, on_call, day_laborer, contract_work,
                indep_con, self_emp, na.rm = T)),
    mult_cust = case_when(cwconcust == 1 ~ 0, cwconcust == 2 ~ 1,
                          cwconcust > 2 ~ NA_real_),
    on_site = case_when(cwconsite == 1 ~ 0, cwconsite == 2 ~ 1,
                        cwconsite > 2 ~ NA_real_),
    health = case_when(cwhiemp == 1 ~ 0, cwhiemp == 2 ~ 1, 
                       cwhiemp > 2 ~ NA_real_),
    retirement = case_when(cwretempany == 1 ~ 0, 
                           cwretempany == 2 ~ 1,
                           cwretempany > 2 ~ NA_real_),
  ) |>
  # Keep only variables needed
  select(year, month, wtfinl, cpsidp, age, earnwt,
         union, female:retirement, cwsuppwt)


# Compare to DK -----------------------------------------------------------

janitor <- cws |> 
  filter(janitor_occ == 1) |> 
  mutate(outsourced_2 = 1 * (janitor_occ == janitor_ind))

sg <- cws |> 
  filter(sg_occ == 1) |> 
  mutate(outsourced_2 = 1 * (sg_occ == sg_ind))

# Compare outsourced_job summary statistics
vars_sum_js <- c("log_real_hrly_wage", "log_real_wkly_wage", 
                 "part_time", "health", "retirement", "union",
                 "less_hs", "hs", "aa", "ba", "plus_ba", "age", "female", 
                 "black", "hispanic", "n")

sum_js <- list(janitor, janitor, sg, sg)
comp_js <- list(janitor, sg)

for (i in 1:4) {
  out <- if (i %% 2 == 1) "contract_work" else "outsourced_2"
  out_sym <- sym(out)
  
  sum_js[[i]] <- sum_js[[i]] |>
    as_survey_design(ids = cpsidp, weights=cwsuppwt) |> 
    filter(!is.na(!!out_sym)) |>
    group_by(!!out_sym) |>
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

vars_js_p <- c("part_time", "black", "hispanic", "female",
               "less_hs", "hs", "aa", "ba", "plus_ba", 
               "retirement", "health", "union")

desc_js <- c("janitors (occupation 753)", "security guards (occupation 726)")
ind_js <- c("services to buildings and dwellings (industry 722)",
            "protective services (industry 744)")
label_js <- c("janitor", "sg")
occ_js <- c("Janitors", "Security Guards")

center_r <- rbind("Log Real", "Hourly Wage", "Log Real",
                  "Weekly Wage", "Part Time", "", 
                  "Health Insurance", "", "Retirement Plan", "",
                  "Union", "", "Less", "High School", "High School", "",
                  "Associate's", "Degree", "Bachelor's", 
                  "Degree", "Postgraduate", "Degree", "Age", "", "Female", "",
                  "Black", "", "Hispanic", "", "Observations")

for (js in 1:2){
  
  center_js <- c(center_r)
  comp <- comp_js[[js]]
  
  for (def in 1:2) {
    
    j <- 2 * (js - 1) + def
    
    for (i in 1:2) {
      r <- i + 2 * (def - 1)
      # Different comparison groups
      if (r == 2){
        divider <- "contract_work"
        cond <- comp$contract_work == 1
        cond_y <- comp$contract_work == 0
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
                           comp, vars_js_p, cond, cond_y, divider)

      center_js <- cbind(center_js, col_i)
    }
  }
  
  center_js <- center_js |>
    add_endline() |>
    center_to_latex()
  
  name <- 
    str_c("Summary Statistic Comparison to Dube and Kaplan 2010 for ",
                occ_js[js], ": CWS")
  label <- str_c(label_js[js], "_cws")
  note <- str_c(
    "Summary statistics for ", desc_js[js], " that are outsourced vs
    not outsourced from the Contingent Worker Supplement (CWS) 
    of the CPS. In the left two columns, outsourced is self-reported 
    by the worker. In the right two, the worker is assumed outsourced
    if their industry is ", ind_js[js], 
    " following \\citet{dubekaplan2010}. Observations are at the
    worker-job level and summary statistics are weighted at the 
    person level. Stars represent significant difference from 
    outsourced at the .10 level (*), .05 level (**), and .01 level (***)."
  )
  
  header <- make_header("", name, label)
  d_header <- make_header("d", name, label)
  bot_js <- make_bot(note)
  
  write.table(str_c(header, top_js, center_js, bot_js, 
                    "\n \\end{document}"),
              str_c(table_folder, "CWS/", occ_js[js], " Summary.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save a version to Drafts
  write.table(str_c(d_header, top_js, center_js, bot_js),
              str_c(d_table_folder, "Dube Kaplan ", occ_js[js],
                    " Summary CWS.tex"),
              quote=F, col.names=F, row.names=F, sep="")
}

# What are the self-reported job types of DK's outsourced?
# Use data.frame's SQL like capabilities to make these tables
top <- "\\begin{tabular}{lrrr}
\\toprule
& \\multicolumn{3}{c} {Industry-Occupation (Dube and Kaplan)} \\\\
Self-Reported & Outsourced & Not Outsourced & Total \\\\ \\midrule
"

occ_js <- c("janitors (occupation 753)",
            "security guards (occupation 726)")
ind_js <- c("722", "744")
labels <- c("janitors", "sg")
save <- c("Janitors", "Security Guards")

types <- c("contract_work", "indep_con", "temp_work",
           "on_call", "day_laborer", "self_emp", "traditional")

dt_js <- list(data.table(janitor), data.table(sg))

center_r <- c("Contracted-Out", "Independent Contractor", 
              "Temp Worker", "On-Call Worker", "Day Laborer",
              "Self-Employed", "Traditional Employee", "Total" )

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
      col_i, 
      cbind(str_c(" & ", a),
            str_c(" & ", b),
            str_c(" & ", a + b)))
  }
  
  col_i <- rbind(
    col_i,
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
      save[js], ": CWS")
  label <- str_c("dk_types_", labels[js], "_cws")
  note <- str_c(
    "Counts of \\citet{dubekaplan2010} (DK) method of measuring 
    outsourcing versus CWS self-reported job type for ", occ_js[js],
    " in the CWS. For columns, following DK, workers are consider 
    outsourced if they are in industry ", ind_js[js], ". 
    Rows show the worker's self-reported job type.
    "
  )
  
  header <- make_header("", name, label)
  d_header <- make_header("d", name, label)
  s_header <- make_header("s")
  
  bot <- make_bot(note)
  
  write.table(str_c(header, top, center, bot, "\n \\end{document}"),
              str_c(table_folder, "CWS/Dube Kaplan ", save[js],
                    " Types.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save in Drafts
  write.table(str_c(d_header, top, center, bot),
              str_c(d_table_folder, "Dube Kaplan CWS ", save[js],
                    " Outsourced.tex"),
              quote=F, col.names=F, row.names=F, sep="")
  
  # Save in Slides
  write.table(str_c(s_header, top, center, s_bot),
              str_c(s_table_folder, "Dube Kaplan CWS ", 
                    save[js], " Outsourced.tex"),
              quote=F, col.names=F, row.names=F, sep="")

}

# Break Down PBS and other Ind ----------------------------------------------------------

cws <- data.table(cws) 

top <- "\\begin{tabular}{lrrr}
\\toprule
Self-Reported & PBS & Not PBS & Total \\\\ \\midrule
"

types <- c("contract_work", "indep_con", "temp_work",
           "on_call", "day_laborer", "self_emp", "traditional")

center <- c("Contracted-Out (This Paper)", "Independent Contractor", "Temp Worker",
            "On-Call Worker", "Day Laborer", "Self-Employed",
            "Traditional Employee", "Total" )

col_i <- c()

for (type in types){
  # Some observations may not occur, set to 0
  a <- tryCatch(cws[pbs == 1 & cws[[type]] == 1, .N], 
                error = function(e) {a <- 0})
  b <- tryCatch(cws[pbs == 0 & cws[[type]] == 1, .N],
                error = function(e) {b <- 0})
  b <- b * !is.na(b) + 0  
  col_i <- rbind(
    col_i,
    cbind(str_c(" & ", a),
          str_c(" & ", b),
          str_c(" & ", a + b)))
}

col_i <- rbind(
  col_i,
  cbind(str_c(" & ", cws[pbs == 1, .N]),
        str_c(" & ", cws[pbs == 0, .N]),
        str_c(" & ", NROW(cws))))

center <- center |>
  cbind(col_i) |>
  add_endline() |>
  center_to_latex()

name <- "Job Types of Personal Business Service Workers: CWS"
label <- "pbs_types_cws"
note <- "
Job types of workers in Professional Business Service (PBS) industries
versus all other industries for all workers age 18-65 in all 6 rounds 
of the CWS. PBS industries have a Census 1990 Industry code between 
721 and 760."

header <- make_header("", name, label)
d_header <- make_header("d", name, label)
s_header <- make_header("s")

bot <- make_bot(note)

write.table(str_c(header, top, center, bot, "\n \\end{document}"),
            str_c(table_folder, "CWS/PBS Types.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save in Drafts
write.table(str_c(d_header, top, center, bot),
            str_c(d_table_folder, "PBS CWS Types.tex"),
            quote=F, col.names=F, row.names=F, sep="")

# Save in Slides
write.table(str_c(s_header, top, center, s_bot),
            str_c(s_table_folder, "PBS CWS Types.tex"),
            quote=F, col.names=F, row.names=F, sep="")
