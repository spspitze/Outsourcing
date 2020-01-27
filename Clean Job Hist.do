* This file is to clean job_hist (using stata because file is to big for R
* Studio). Need to convert wide job histories to long histories based on year
* and job number

clear

import delim "C:\Users\sps55207\Desktop\Outsourcing\job_hist"

* Variables used

local varlist employers_all_uid  employers_all_tenure ///
employers_all_hoursweek employers_all_ind employers_all_occ ///
employers_all_hrly_wage employers_all_union 

* For uid, the data is only listed once, otherwise each job is listed 
* yearly. Reshaping will be easier if uid is yearly too

forvalues year = 2002(2)2016{
	forvalues job = 1/9{
	gen employers_all_uid_`year'0`job' = employers_all_uid0`job'
	}
	
	forvalues job = 10/54{
	gen employers_all_uid_`year'`job' = employers_all_uid`job'
	}
}

* Make sure to drop plain uid
keep caseid employers_all_*_200* employers_all_*_201*

* Reshape data long
reshape long `varlist', i(caseid) j(year_job) string 

* Separate year
generate year = substr(year_job, 2, 4)

local varlist uid tenure hoursweek ind occ hrly_wage union

* Rename variables without "employers_all"
* replace <0 with .
foreach var in `varlist'{
rename employers_all_`var' `var'
}

* Drop year_job, if uid = -4 or missing, if ind or occ missing,
* or if all variables negative (missing)
drop year_job
drop if uid == -4 | missing(uid) | missing(occ) | missing(ind) | ///
(tenure < 0 & hoursweek < 0 & ind < 0 & occ < 0 & hrly_wage < 0)

* Rename and retype to match R data
rename uid emp_id
rename caseid case_id
rename hoursweek hours_week

export delimited using "C:\Users\sps55207\Desktop\Outsourcing\cleaned_job_hist.csv", nolabel replace
