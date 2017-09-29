capture log close
set more off
clear mata
clear

log using "~/dropbox/articles/wsp-recall/programs/final/1-descriptive/0-wsp-optimrecall-summary-char.log", text replace


*---------------------------------------------
* Program      : 0-wsp-optimrecall-summary-char
* Programmer   : Ben Arnold
* Date         : 15 Aug 2011
* Description  :
/*

Summarize child and household characteristics
for Table 1 of the manuscript (not including
symptom summary statistics)

*/
*---------------------------------------------


*---------------------------------------------
* Calculate summary stats for each country
*---------------------------------------------

use "~/dropbox/articles/wsp-recall/data/wsp-recall-data.dta", clear


* INDIA HP
sum agemth female cbf anemia haz2 waz2 if dataset=="India HP"
sum elec soil biof JMPH2O JMPsan if dataset=="India HP"

* INDIA MP
sum agemth female cbf anemia haz2 waz2 if dataset=="India MP"
sum elec soil biof JMPH2O JMPsan if dataset=="India MP"

* INDONESIA
sum agemth female cbf anemia haz2 waz2 if dataset=="Indonesia"
sum elec soil biof JMPH2O JMPsan if dataset=="Indonesia"


* PERU
sum agemth female cbf anemia haz2 waz2 if dataset=="Peru"
sum elec soil biof JMPH2O JMPsan if dataset=="Peru"

* SENEGAL
sum agemth female cbf anemia haz2 waz2 if dataset=="Senegal"
sum elec soil biof JMPH2O JMPsan if dataset=="Senegal"





log close
exit


