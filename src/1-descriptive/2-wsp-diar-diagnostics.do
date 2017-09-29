capture log close
set more off
clear mata
clear

log using "~/dropbox/articles/wsp-recall/programs/final/1-descriptive/2-wsp-diar-diagnostics.log", text replace


*---------------------------------------------
* Program      : 2-wsp-diar-diagnostics
* Programmer   : Ben Arnold
* Date         : 8 Jun 2010
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
* 15 July 2011 simplified to exclude processing
*---------------------------------------------



set mem 500m



*---------------------------------------------
* Program to calculate summary stats
*---------------------------------------------

capture program drop diarsum
program define diarsum
version 9

*--------------------------------------------
* Summarize length for cases that terminated
* in the 7 d prior to the interview
*--------------------------------------------


di as res _n "Reported diarrhea episode length"
tab diarlencat if agemth<24 & diar00==0 & diar07pp==1

di as res _n "Reported diarrhea episode length (untruncated)"
tab diarlen if agemth<24 & diar00==0 & diar07pp==1

di as res _n "Mean episode length"
sum diarlen if agemth<24 & diarlen<30 & diar00==0 & diar07pp==1, d


*--------------------------------------------
* Summarize point and period prevalence
* among children <=24 months
*--------------------------------------------

di as res "Point prevalence means"
sum diar00 - diar14 if agemth <24


di as res "Period prevalence means"
sum diar*pp if agemth <24


end diarsum



*---------------------------------------------
* Calculate summary stats for each country
*---------------------------------------------

* INDIA HP
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="India HP"
diarsum


* INDIA MP
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="India MP"
diarsum


* INDONESIA
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="Indonesia"
diarsum


* PERU
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="Peru"
diarsum


* SENEGAL
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="Senegal"
diarsum

log close
exit


