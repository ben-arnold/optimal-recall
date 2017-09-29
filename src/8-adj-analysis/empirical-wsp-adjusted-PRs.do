capture log close
set more off
clear all

*-------------------------------------------
* empirical-wsp-adjusted-PRs.do
*
* Calculate unadjusted and adjusted PRs
* store results
*
* The purpose is to compare the effect of
* adjusting for covariates on the bias and
* SE of the estimates
*-------------------------------------------

use "~/dropbox/manuscripts/wsp-recall/data/wsp-recall-data.dta", clear


*-------------------------------------------
* Run unadjusted and adjusted log-binomial
* models to estimate the PR
* store results
*-------------------------------------------
capture program drop adjregs
program define adjregs, rclass
version 12
args dataset

tempname memhold
tempfile results
postfile `memhold' str10(outcome exposure) recall pr pr_se pr_lb pr_ub apr apr_se apr_lb apr_ub using `results'


local outs "diar cough fever"
local exps "anemia haz2 waz2"
local recall "01 02 03 04 05 06 07 08 09 10 11 12 13 14"

foreach out of local outs {
	foreach exp of local exps {
		foreach rec of local recall {

			glm `out'`rec'pp `exp' if dataset=="`dataset'", cluster(cluster) family(binomial) link(log) eform robust iterate(25)
			local pr = exp(_b[`exp'])
			local pr_se = `pr'*_se[`exp']
			local pr_lb = `pr' - 1.96*`pr_se'
			local pr_ub = `pr' + 1.96*`pr_se'
			
			glm  `out'`rec'pp `exp' agemth female cbf elec soil biof JMPsan JMPH2O if dataset=="`dataset'", cluster(cluster) family(binomial) link(log) eform robust iterate(25)
			local apr = exp(_b[`exp'])
			local apr_se = `apr'*_se[`exp']
			local apr_lb = `apr' - 1.96*`apr_se'
			local apr_ub = `apr' + 1.96*`apr_se'
		
			post `memhold' ("`out'") ("`exp'") (`rec') (`pr') (`pr_se') (`pr_lb') (`pr_ub') (`apr') (`apr_se') (`apr_lb') (`apr_ub')
		

		}
	}
}
postclose `memhold'
use `results', clear

end adjregs

*-------------------------------------------
* Estimates by country
*-------------------------------------------

*-------------------------------------------
* India HP
*-------------------------------------------
preserve
adjregs "India HP"
gen dataset = "India HP"
tempfile ihp
save `ihp'
restore

*-------------------------------------------
* India MP
*-------------------------------------------
preserve
adjregs "India MP"
gen dataset = "India MP"
tempfile imp
save `imp'
restore

*-------------------------------------------
* Indonesia
*-------------------------------------------
preserve
adjregs "Indonesia"
gen dataset = "Indonesia"
tempfile ind
save `ind'
restore

*-------------------------------------------
* Peru
*-------------------------------------------
preserve
adjregs "Peru"
gen dataset = "Peru"
tempfile per
save `per'
restore

*-------------------------------------------
* Senegal
*-------------------------------------------
preserve
adjregs "Senegal"
gen dataset = "Senegal"
tempfile sen
save `sen'
restore

*-------------------------------------------
* Append results, calculate bias
*-------------------------------------------
use `ihp', clear
append using `imp'
append using `ind'
append using `per'
append using `sen'

gen _x = pr if recall==2
gen _y = apr if recall==2
bysort dataset outcome exposure: egen prtrue = max(_x)
bysort dataset outcome exposure: egen aprtrue = max(_y)
drop _x _y
gen pr_bias = pr-prtrue
gen apr_bias = apr-aprtrue
gen bias_diff = apr_bias-pr_bias

*-------------------------------------------
* output results
*-------------------------------------------
sort dataset outcome exposure recall
label data "unadj and adj PRs, created by empricial-wsp-adjusted-PRs.do"
saveold "~/dropbox/manuscripts/wsp-recall/tables/rawoutput/PRs-unadj-adj.dta", replace






