capture log close
set more off
clear mata
clear

log using "~/dropbox/articles/wsp-recall/programs/final/1-descriptive/1-wsp-cough-diagnostics.log", text replace


*---------------------------------------------
* Program      : 1-wsp-cough-diagnostics
* Programmer   : Ben Arnold
* Date         : 9 Mar 2011
* Description  :
/*

Summarize cough characteristics for 
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

capture program drop coughsum
program define coughsum
version 9

*--------------------------------------------
* Summarize length for cases that terminated
* in the 7 d prior to the interview
*--------------------------------------------

di as res _n "Reported cough episode length"
tab coughlencat if agemth<24 & cough00==0 & cough07pp==1

di as res _n "Reported cough episode length (untruncated)"
tab coughlen if agemth<24  & cough00==0 & cough07pp==1

di as res _n "Mean Cough episode length"
sum coughlen if agemth<24 & coughlen<30  & cough00==0 & cough07pp==1, d




*--------------------------------------------
* Summarize point and period prevalence
* among children <=24 months
*--------------------------------------------

di as res "Point prevalence means"
sum cough00 - cough14 if agemth <24


di as res "Period prevalence means"
sum cough*pp if agemth <24


end coughsum



*---------------------------------------------
* Calculate summary stats for each country
*---------------------------------------------

* INDIA HP
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="India HP"
coughsum


* INDIA MP
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="India MP"
coughsum


* INDONESIA
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="Indonesia"
coughsum


* PERU
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="Peru"
coughsum


* SENEGAL
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="Senegal"
coughsum



log close
exit


