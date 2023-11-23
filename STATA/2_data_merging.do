								* -------------
								* Data merging
								* -------------
							
clear
set more off						
cap log close
log using "/Users/raphaelboulat/Desktop/RA /Trade & conflicts/logs/1_data_merging_$sysdate.smcl", replace	

** Merge the two datasets

use "$datadir/2_trade", clear

merge m:m year partneriso using "/Users/raphaelboulat/Desktop/RA /Trade & conflicts/data/2_conflicts.dta"

save "$datadir/0_merge", replace

use "$datadir/0_merge", clear

/* As mentionned in the previous code, we need to create the variable incidence based on countries that were not merged. */

* We can first see what countries were not merged (from the conflict dataset). Once we saw that, we can double check in the conflict data if some years are indeed missing for the "un-merged" countries. 

tab partneriso if _merge==1 // tells us what countries have years missing

use "$datadir/2_conflicts", clear 

tab year if partneriso=="BWA" // Here, we can see that 4 years are indeed missing for BWA.

use "$datadir/0_merge", clear

drop if _merge==2

g incidence=0 if _merge==1 // we generate the variable incidence taking the value 0 if the variable was not merged and hence if there was no conflict this year for the country
replace incidence=1 if _merge==3 // The variable incidence takes the value 1 if a conflict occured this year in the country.

** I create the variable onset by hand because struggled to find an efficient way. 
* We find the information on which country and years to choose for the onset by using "tab partneriso if _merge==1". We hence find what countries have missing years, and the variable onset takes the value 0 if there was non conflict in the previous year in the country.
 
g onset=0 if partneriso=="BWA" & year==2000 
replace onset=0 if partneriso=="BWA" & year==2002
replace onset=0 if partneriso=="BWA" & year==2003 
replace onset=0 if partneriso=="BWA" & year==2013
replace onset=0 if partneriso=="COM"
replace onset=0 if partneriso=="CPV"
replace onset=0 if partneriso=="GAB" & year==2008
replace onset=0 if partneriso=="GNQ" & year==2000
replace onset=0 if partneriso=="LSO" & year==2006
replace onset=0 if partneriso=="LSO" & year==2014
replace onset=0 if partneriso=="MUS"
replace onset=0 if partneriso=="STP"
replace onset=0 if partneriso=="SYC"
replace onset=1 if onset!=0

save "$datadir/1_merge", replace

** end of file
log close


