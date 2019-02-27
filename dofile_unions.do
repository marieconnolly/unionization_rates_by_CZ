/********************************************************************************************************/
/* Title: Do file computing predicted unionization rates by Commuting Zone                            	*/
/* Authors: Marie Connolly and Martin Leblond-Letourneau												*/
/* Research Group on Human Capital																		*/
/* University of Quebec in Montreal																		*/
/* Software: Stata 14.2             																	*/
/* Date: May 14, 2018																					*/
/* Steps:      																							*/
/* 1: Using March Outgoing Rotation Groups CPS data for 2000 (morg00)									*/
/* 1.1: Make variables compatible with QWI. QWI uses NAICS codes for industry. CPS uses Census ind codes*/
/* 1.2: Generation all possible combinations of age/sex-state-industry									*/
/* 1.3: Estimate probability to be a union member or covered by a collective bargaining agreement		*/
/*																										*/
/* 2: Using the Quarterly Workforce Indicators (QWI)													*/
/* QWI files: one per state, county-level, 3-digit industry, All firms, by sex/age						*/
/* QWI files: variable emps (stable employment), 4 quarters, 2000 Q1 to 2000 Q4 when possible			*/
/* QWI files: if not possible, then the first 4 quarters available										*/
/* 2.1: Put together the state-level .csv files															*/
/* 2.2: Make variables compatible																		*/
/*																										*/
/* 3: Merge QWI and MORG																				*/
/*																										*/
/* 4: Get CZ from 2010 county using 2010 county to CZ crosswalk from Chetty et al 2014					*/
/*																										*/
/* 5: Collapse counties into CZ																			*/
/********************************************************************************************************/
clear all
cap log close
set more off

// Set the main directory below. The rest should follow
glo mainDir  "/Users/marieconnolly/Dropbox/Projets/small differences/JOLE Revisions/Data Archive"						// Main directory (edit to your path)

glo unionDir "${mainDir}/US data/Unionization rates"	// Where the union files are
glo inputDir "${unionDir}/input"						// Where the union files are
glo qwiDir "${inputDir}/QWI"							// Where the QWI data are
glo outputDir "${unionDir}/output"						// Where you want the output
glo name "unionization_by_CZ"							// Name for output

cap mkdir "${outputDir}"

cd "$unionDir"

log using "$outputDir/log_unionization_rates", text replace

di "=== SYSTEM DIAGNOSTICS ==="
di "Stata version: `c(stata_version)'"
di "Updated as of: `c(born_date)'"
di "Flavor:        `c(flavor)'"
di "Processors:    `c(processors)'"
di "OS:            `c(os)' `c(osdtl)'"
di "Machine type:  `c(machine_type)'"
di "Max matsize:   `c(max_matsize)'"
di "=========================="
  
* Import harmonized industry codes (NAICS vs Census codes)

//industry crosswalk comes from https://www.census.gov/topics/employment/industry-occupation/guidance/code-lists.html
//click on : 1990-2012 Census Industry Codes with Crosswalk 
//download : industry-crosswalk-90-00-02-07-12.xls

//most Census codes map to a single 3-digit NAICS codes, with a few exceptions
//final crosswalk used in this paper part of the data archive

import excel "$inputDir/Industry_Crosswalk.xlsx", sheet("Feuil1") firstrow clear
rename NAICSHarmonized naics
rename Census2000 indTrans
keep naics indTrans
drop if indTrans=="New2" | indTrans=="New3" |indTrans=="Old " | indTrans=="Old" 
destring _all , replace
drop if naics==. | indTrans==.
tempfile ind
save `ind'
clear

/* Step 1*/
* download morg00.dta from NBER archive https://www.nber.org/morg/annual/
use "$inputDir/morg00.dta",clear

*Only keep Employed-At Work and Employed-Absent individuals 
*Remove Unemployed or Not In Labor Force
keep if lfsr94<3

*Only keep Employees from the Private or Government sector
*Remove Self-Employed or Without Pay
keep if class94<=5

*Generate member variable: employee is either a union member or covered by a collective bargaining agreement
gen member =(unionmme==1 | unioncov==1)

/* Step 1.1*/
replace ind02=ind02/10
rename ind02 indTrans
replace indTrans=928 if (indTrans==959 | indTrans==967 | indTrans==968 | indTrans==969 | indTrans==977 | indTrans==978 | indTrans==979 | indTrans==987) 


*merge harmonized industry codes
merge m:1 indTrans using "`ind'"
drop _merge

*industries
levelsof(naics), local(levels)
foreach i of local levels {
gen ind_`i'=naics==`i'
}

*sex
gen male = (sex==1)
gen female = (sex==2)

*age: match with QWI age groups
gen agegrp=.
replace agegrp=1 if age>=14 & age<=18
replace agegrp=2 if age>=19 & age<=21
replace agegrp=3 if age>=22 & age<=24
replace agegrp=4 if age>=25 & age<=34
replace agegrp=5 if age>=35 & age<=44
replace agegrp=6 if age>=45 & age<=54
replace agegrp=7 if age>=55 & age<=64
replace agegrp=8 if age>=65 & age<=99
tab agegrp, gen(age_)
drop age agegrp

*state
levelsof(stfips), local(levels)
foreach i of local levels {
gen st_`i'=stfips==`i'
}

***1.2.****  
keep male female age_* st_* ind_* member weight earnwt stfips naics
gen morg = 1

**Loop to generate dummy observations with all possible combinations or sex/age/state/industry
global obs = _N+1

foreach v of varlist male female {
  
  foreach w of varlist age_* {
    
	foreach x of varlist st_* {
	
	  foreach y of varlist ind_* {
	  
        qui set obs $obs
	    foreach var of varlist _all {
	      qui replace `var' = 0 in $obs
	    }
        
		qui replace `v' = 1 in $obs
		qui replace `w' = 1 in $obs
		qui replace `x' = 1 in $obs
		qui replace `y' = 1 in $obs
				
		global obs = $obs+1
		}
	  }
	}
}

***1.3.*** 
probit member male female age* st_* ind_* [pw=weight] if morg==1 ,r
predict pmember, pr

**generate categorical variables to make merging with QWI easier
*sex
gen sex = 1 if male==1
replace sex = 2 if female==1

*age
gen agegroup = .
forvalues i = 1/8 {
replace agegroup=`i' if age_`i'==1
}

*industry    
replace naics = . if morg==0
levelsof(naics), local(levels)
foreach i of local levels {
replace naics=`i' if ind_`i'==1 & morg==0
}

*state   
replace stfips = . if morg==0
levelsof(stfips), local(levels)
foreach i of local levels {
replace stfips=`i' if st_`i'==1 & morg==0
}
rename stfips state
 
keep if morg==0 
keep pmember sex agegroup naics state

tempfile morg00_prop
save `morg00_prop'

***2. QWI***

cd "$qwiDir"
capture mkdir "QWI_temp"

***2.1: Put together the state-level .csv files***		
levelsof(state), local(levels)
foreach i of local levels {
	import delimited "QWI_`i'.csv", encoding(ISO-8859-1) clear
	keep geography sex agegrp industry emps semps
	save "QWI_temp/QWI_temp_`i'", replace
}

use "QWI_temp/QWI_temp_1", replace
foreach i of numlist 2 4 5 6 8 9 10 11 12 13 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 44 45 46 47 48 49 50 51 53 54 55 56 {
append using "QWI_temp/QWI_temp_`i'"
}

***2.2: Make variables compatible***

*impute random 1 or 2
*stable employment recode for flag==5 (Value suppressed because it does not meet US Census Bureau publication standards.)
*emps measures the number of stable jobs in a quarter for a given combination
*when semps==5, we know that the number of jobs is greater than 0, but the number of individuals is too small to release the actual number of jobs
*in this case, we impute 1 job 50% of the time and 2 jobs 50% of the time
*this represents less than 2% of the total number of stable jobs observed
set seed 1978 //For replication of Connolly, Corak and Haeck (JOLE), seed was not set. Values computed by CCH are thus slightly different.
gen random=runiform()
replace emps=1 if semps==5 & random<=0.5
replace emps=2 if semps==5 & random>0.5

*state
gen state=floor(geography/1000)

* var sex ok

*age
gen agegroup=.  
forvalues i=1/8 {
replace agegroup=`i' if agegrp=="A0`i'"
}

*industry
replace industry=23 if industry==236 | industry==237 | industry==238
replace industry=423 if industry==425
replace industry=523 if industry==525
replace industry=924 if industry==925
replace industry=926 if industry==927
rename industry naics

***3: Merge QWI and MORG***

merge m:1 sex agegroup state naics using `morg00_prop.dta'

keep if _merge==3
drop _merge
rename geography county_id 

***4: Get CZ from 2010 county using 2010 county to 1990 CZ crosswalk from Chetty et al 2014

replace county_id=2270 if county_id==2158 //change in fips code and county name from Wade Hampton 2158 to Kusilvak 2270
replace county_id=46113 if county_id==46102 //change in fips code and county name from Shannon County 46113 to Oglala Lakota county 46102

merge m:1 county_id using "$inputDir/county_2010.dta"
keep if _merge==3
drop _merge

***5: Collapse counties into CZ	***
collapse (mean) pmember [aw=emps], by(cz)

save "$outputDir/unionization_by_CZ.dta",replace
export delimited using "${outputDir}/${name}.csv", replace
log close
