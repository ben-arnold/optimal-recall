capture log close
set more off
clear mata
clear

log using "~/dropbox/articles/wsp-recall/programs/final/1-descriptive/3-wsp-fever-diagnostics.log", text replace


*---------------------------------------------
* Program      : 3-wsp-fever-diagnostics
* Programmer   : Ben Arnold
* Date         : 3 Mar 2011
* Description  :
/*

Summarize diarrhea characteristics for 
WSP datasets to inform recall calculations
and simulations.

*/
*---------------------------------------------

*---------------------------------------------
* Major updates / modifications to the code:
*
* 
*---------------------------------------------



set mem 500m



*---------------------------------------------
* Program to calculate summary stats
*---------------------------------------------

capture program drop feversum
program define feversum
version 9
	
*--------------------------------------------
* Summarize length for cases that terminated
* in the 7 d prior to the interview
*--------------------------------------------
	
di as res _n "Reported fever episode length"
tab feverlencat if agemth<24  & fever00==0 & fever07pp==1

di as res _n "Reported fever episode length (untruncated)"
tab feverlen if agemth<24 & fever00==0 & fever07pp==1

di as res _n "Mean Fever episode length"
sum feverlen if agemth<24 & feverlen<30 & fever00==0 & fever07pp==1, d




*--------------------------------------------
* Summarize point and period prevalence
* among children <=24 months
*--------------------------------------------

di as res "Point prevalence means"
sum fever00 - fever14 if agemth <24


di as res "Period prevalence means"
sum fever*pp if agemth <24


end feversum



*---------------------------------------------
* Calculate summary stats for each country
*---------------------------------------------

* INDIA HP
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="India HP"
feversum


* INDIA MP
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="India MP"
feversum


* INDONESIA
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="Indonesia"
feversum


* PERU
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="Peru"
feversum


* SENEGAL
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="Senegal"
feversum



log close
exit


