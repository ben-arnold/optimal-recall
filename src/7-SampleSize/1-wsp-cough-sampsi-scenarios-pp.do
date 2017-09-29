capture log close
set more off
clear mata
clear

log using "~/dropbox/articles/wsp-recall/programs/final/7-samplesize/1-wsp-cough-sampsi-scenarios-pp.log", text replace


*---------------------------------------------
* Program      : 1-wsp-cough-sampsi-scenarios-pp.do
* Programmer   : Ben Arnold
* Date         : 17 May 2011
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
* Calculate summary stats for each country
*---------------------------------------------


tempname memhold
tempfile results
postfile `memhold' str30 data str2 day corr using `results'

* INDIA HP
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="India HP"
gen ID = child
keep ID cough00 - cough14
local dd "02 03 04 05 06 07 08 09 10 11 12 13 14"
foreach d of local dd {
	di as res _n "Correlation for days 01 - `d'"
	alpha cough01 - cough`d', std
	
	post `memhold' ("IndiaHP") ("`d'") (r(rho))

}


* INDIA MP
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="India MP"
gen ID = child
keep ID cough00 - cough14
local dd "02 03 04 05 06 07 08 09 10 11 12 13 14"
foreach d of local dd {
	di as res _n "Correlation for days 01 - `d'"
	alpha cough01 - cough`d', std
	
	post `memhold' ("IndiaMP") ("`d'") (r(rho))

}


* INDONESIA
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="Indonesia"
gen ID = child
keep ID cough00 - cough14

local dd "02 03 04 05 06 07 08 09 10 11 12 13 14"
foreach d of local dd {
	di as res _n "Correlation for days 01 - `d'"
	alpha cough01 - cough`d', std
	
	post `memhold' ("Indonesia") ("`d'") (r(rho))

}


* PERU
use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="Peru"
gen ID = child
keep ID cough00 - cough14

local dd "02 03 04 05 06 07 08 09 10 11 12 13 14"
foreach d of local dd {
	di as res _n "Correlation for days 01 - `d'"
	alpha cough01 - cough`d', std
	
	post `memhold' ("Peru") ("`d'") (r(rho))

}


use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear
keep if dataset=="Senegal"
gen ID = child
keep ID cough00 - cough14
local dd "02 03 04 05 06 07 08 09 10 11 12 13 14"
foreach d of local dd {
	di as res _n "Correlation for days 01 - `d'"
	alpha cough01 - cough`d', std
	
	post `memhold' ("Senegal") ("`d'") (r(rho))

}


*---------------------------------------------
* Retrieve results
*---------------------------------------------
postclose `memhold'
use `results', clear

reshape wide corr, i(data) j(day, str)

saveold "~/dropbox/articles/wsp-recall/tables/rawoutput/cough-daily-corr.dta", replace


log close
exit


