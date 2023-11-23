
								* -------------
								* Data cleaning
								* -------------

clear
set more off						
cap log close
log using "/Users/raphaelboulat/Desktop/RA /Trade & conflicts/logs/1_data_cleaning_$sysdate.smcl", replace			

/* This do file cleans the trade and conflict data */

*************** TRADE ***************

use "$datadir/0_trade", clear

* We first re-label the variables for a better understanding

label variable reporteriso "ISO 3 code of the importer (reporter)"	
label variable partneriso "ISO 3 code of the exporter (partner)"
label variable commoditycode "Code indicating what commodity is used"
label variable tradevalueus "Total trade value of the flow in current USD($)" 
label variable netweight "Total weight of the exchange"

// Note: import data are better recorded than than export data, so we usually use import data while analyzing exports. Also, when we say in current USD, we are talking about the reference year.


* The value 0 can mean either that there has been no trade between these two countries or that the reporter did not report anything for this year. We hence check that with the following command:

egen total_value = sum(tradevalueus), by(year reporteriso) // creates a sum of trade values by year and reporter. If the sum is equal to 0, it means that the reporter did not report anything for the year. 

// We can see that there is no 0 in the total_value variable, meaning that no country decided not to report for one year  -> the 0's mean that no trade occured between the countries.

drop total_value  // we do not need the variable anymore

* Check if all transcations have a commodity code
br if commoditycode=="" 
// almost 100,000 observation don't have a commodity code --> delete because can't use them
drop if commoditycode==""

* We can now create an african-dummy (taking one if the exporter is an african country and 0 otherwise)
// Note: there's no distinction in the data between RDC and RC --> only data for Congo (COG)

g african_dummy = 1 if partneriso=="DZA"
replace african_dummy = 1 if partneriso=="AGO"
replace african_dummy = 1 if partneriso=="BEN"
replace african_dummy = 1 if partneriso=="BWA"
replace african_dummy = 1 if partneriso=="BFA"
replace african_dummy = 1 if partneriso=="BDI"
replace african_dummy = 1 if partneriso=="CMR"
replace african_dummy = 1 if partneriso=="CPV"
replace african_dummy = 1 if partneriso=="CAF"
replace african_dummy = 1 if partneriso=="TCD"
replace african_dummy = 1 if partneriso=="COM"
replace african_dummy = 1 if partneriso=="COG"
replace african_dummy = 1 if partneriso=="CIV"
replace african_dummy = 1 if partneriso=="DJI"
replace african_dummy = 1 if partneriso=="EGY"
replace african_dummy = 1 if partneriso=="GNQ"
replace african_dummy = 1 if partneriso=="ERI"
replace african_dummy = 1 if partneriso=="ETH"
replace african_dummy = 1 if partneriso=="ETF"
replace african_dummy = 1 if partneriso=="SWZ"
replace african_dummy = 1 if partneriso=="GAB"
replace african_dummy = 1 if partneriso=="GMB"
replace african_dummy = 1 if partneriso=="GHA"
replace african_dummy = 1 if partneriso=="GIN"
replace african_dummy = 1 if partneriso=="GNB"
replace african_dummy = 1 if partneriso=="KEN"
replace african_dummy = 1 if partneriso=="LSO"
replace african_dummy = 1 if partneriso=="LBR"
replace african_dummy = 1 if partneriso=="LBY"
replace african_dummy = 1 if partneriso=="MDG"
replace african_dummy = 1 if partneriso=="MWI"
replace african_dummy = 1 if partneriso=="MLI"
replace african_dummy = 1 if partneriso=="MRT"
replace african_dummy = 1 if partneriso=="MUS"
replace african_dummy = 1 if partneriso=="MAR"
replace african_dummy = 1 if partneriso=="MOZ"
replace african_dummy = 1 if partneriso=="NAM"
replace african_dummy = 1 if partneriso=="NER"
replace african_dummy = 1 if partneriso=="NGA"
replace african_dummy = 1 if partneriso=="PNG"
replace african_dummy = 1 if partneriso=="RWA"
replace african_dummy = 1 if partneriso=="STP"
replace african_dummy = 1 if partneriso=="SEN"
replace african_dummy = 1 if partneriso=="SYC"
replace african_dummy = 1 if partneriso=="SLE"
replace african_dummy = 1 if partneriso=="SOM"
replace african_dummy = 1 if partneriso=="ZAF"
replace african_dummy = 1 if partneriso=="SDN"
replace african_dummy = 1 if partneriso=="TZA"
replace african_dummy = 1 if partneriso=="TGO"
replace african_dummy = 1 if partneriso=="TUN"
replace african_dummy = 1 if partneriso=="UGA"
replace african_dummy = 1 if partneriso=="ZMB"
replace african_dummy = 1 if partneriso=="ZWE"
replace african_dummy = 0 if african_dummy ==.

* We want to keep the data only when african countries exported to the rest of the world (including africa). 

keep if african_dummy==1 // around 7.5 million observations remaining
drop african_dummy // we do not need the african dummy anymore

save "$datadir/1_trade", replace // this dataset contains only trade data for african countries
**************

use "$datadir/1_trade", clear // do that so that we don't have to run everything again

* First, we generate a new variable taking the log of tradevalueus
g log_tradevalue = log(tradevalueus) // creates an NA if the value is equal to zero, and a zero if the value is equal to 1. 

/* We generate variables to have 3 different aggregation level in total. The HS code with 2 digits is the most aggregated one and the HS code with 6 digits is the least aggregated one (our commoditycode variable). 
More info here: https://unstats.un.org/wiki/pages/viewpage.action?pageId=87426301 and https://wits.worldbank.org/trade/country-byhs6product.aspx?lang=en  */

g HS_two = substr(commoditycode, 1,2) // identifies the chapter the goods are classified in, e.g., 09 = Coffee, Tea, Maté and Spices
g HS_four = substr(commoditycode, 1,4) // identifies groupings within that chapter, e.g., 09.02 = Tea, whether or not flavoured

// commoditycode is even more specific, e.g., 09.02.10 Green tea (not fermented)

* Label, rename and and re-order 
rename commoditycode HS_six

label variable HS_two "Most aggregated commodity code"
label variable HS_four "1st level of disaggregation"
label variable HS_six "Least aggregated commodity code"

order year reporteriso partneriso HS_two HS_four HS_six 

keep if year>1996 // do that to match the conflict data

save "$datadir/2_trade", replace

* We keep only the sections of interest that are related to minerals. We also keep metals. The most important one seems to be number 26. 

use "$datadir/2_trade", clear

keep if HS_two== "25" | HS_two== "26" | HS_two=="27" | HS_two=="28" | HS_two=="71" | HS_two=="72" | HS_two=="73"  | HS_two=="74"  | HS_two=="75" | HS_two=="76" | HS_two=="77" | HS_two=="78" | HS_two=="79" | HS_two=="80"  | HS_two=="81" | HS_two=="83" 


* Select only the 17 metals of interest
keep if HS_four == "2510" |  HS_four == "2601" | HS_four == "2602" | HS_four == "2603" | HS_four == "2604" | HS_four == "2605" | HS_four == "2606" | HS_four == "2607" | HS_four == "2608" | HS_four == "2609" | HS_four == "2615" | HS_four == "2825" |  HS_four == "2701" | HS_four == "7102" | HS_four == "7106" | HS_four == "7108" | HS_four == "7110" 

save "$datadir/3_trade", replace

** We can also collapes the data to a certain level of aggregation

*use "$datadir/3_trade", clear
*collapse (sum) tradevalueus netweight, by(HS_four year partneriso reporteriso)
*collapse (sum) tradevalueus netweight, by(HS_two year partneriso reporteriso)

*************** CONFLICTS ***************

/* We downloaded data on conflicts in Africa from ACLED. Data is available between 1997 and 2023. We kept the event ID, the year, the disorder type, the event type, the actors involved, the ISO code, the region, the country, the location, the longitud/latitude and the number of fatalities. We need to clean this data and create an ISO3 code in order to merge it with the trade data. */

use "$datadir/0_conflicts", clear

rename YEAR year // we rename the variable name as it is in the trade data
rename COUNTRY country // easier to manipulate without capital letters
rename EVENT_ID_CNTY event_id
rename EVENT_TYPE event_type
rename DISORDER_TYPE disorder_type
rename ISO iso_code
rename REGION region
rename FATALITIES fatalities
rename LOCATION location
rename LATITUDE latitude
rename LONGITUDE longitude
rename ACTOR1 actor1
rename ACTOR2 actor2

keep if year < 2021 // we only keep these years in order to match the trade data

label variable event_id "An individual identifier by number and country acronym"
label variable iso "A numeric code for each individual country"
label variable fatalities "The number of reported fatalities which occurred during the event"

** We can now create the partner (exporter) ISO code to match the trade data. 

g partneriso = "DZA" if country=="Algeria" 
replace partneriso = "AGO" if country=="Angola"
replace partneriso = "BEN" if country=="Benin"
replace partneriso = "BWA" if country=="Botswana"
replace partneriso = "BFA" if country=="Burkina Faso"
replace partneriso = "BDI" if country=="Burundi"
replace partneriso = "CMR" if country=="Cameroon"
replace partneriso = "CPV" if country=="Cape Verde"
replace partneriso = "CAF" if country=="Central African Republic"
replace partneriso = "TCD" if country=="Chad"
replace partneriso = "COM" if country=="Comoros"
replace partneriso = "COG" if country=="Democratic Republic of Congo" | country== "Republic of Congo" 
replace partneriso = "DJI" if country=="Djibouti"
replace partneriso = "EGY" if country=="Egypt"
replace partneriso = "GNQ" if country=="Equatorial Guinea"
replace partneriso = "ERI" if country=="Eritrea"
replace partneriso = "ETH" if country=="Ethiopia"
replace partneriso = "GAB" if country=="Gabon"
replace partneriso = "GMB" if country=="Gambia"
replace partneriso = "GHA" if country=="Ghana"
replace partneriso = "GIN" if country=="Guinea"
replace partneriso = "GNB" if country=="Guinea-Bissau"
replace partneriso = "CIV" if country=="Ivory Coast"
replace partneriso = "KEN" if country=="Kenya"
replace partneriso = "LSO" if country=="Lesotho"
replace partneriso = "LBR" if country=="Liberia"
replace partneriso = "LBY" if country=="Libya"
replace partneriso = "MDG" if country=="Madagascar"
replace partneriso = "MWI" if country=="Malawi"
replace partneriso = "MLI" if country=="Mali"
replace partneriso = "MRT" if country=="Mauritania"
replace partneriso = "MUS" if country=="Mauritius"
replace partneriso = "MYT" if country=="Mayotte"
replace partneriso = "MAR" if country=="Morocco"
replace partneriso = "MOZ" if country=="Mozambique"
replace partneriso = "NAM" if country=="Namibia"
replace partneriso = "NER" if country=="Niger"
replace partneriso = "NGA" if country=="Nigeria"
replace partneriso = "REU" if country=="Reunion"
replace partneriso = "RWA" if country=="Rwanda"
replace partneriso = "STP" if country=="Sao Tome and Principe"
replace partneriso = "SEN" if country=="Senegal"
replace partneriso = "SYC" if country=="Seychelles"
replace partneriso = "SLE" if country=="Sierra Leone"
replace partneriso = "SOM" if country=="Somalia"
replace partneriso = "ZAF" if country=="South Africa"
replace partneriso = "SDN" if country=="Sudan" | country=="South Sudan"
replace partneriso = "TZA" if country=="Tanzania"
replace partneriso = "TGO" if country=="Togo"
replace partneriso = "TUN" if country=="Tunisia"
replace partneriso = "UGA" if country=="Uganda"
replace partneriso = "ZMB" if country=="Zambia"
replace partneriso = "ZWE" if country=="Zimbabwe"
replace partneriso = "SWZ" if country=="eSwatini"

// Note: in the trade data, RDC and ROC + Sudan and South Sudan are both aggregated in one variable, which is why we coded them together. We also do not have Papuasia New ginea in the conflict data. 

order year country region event_id partneriso iso_code location latitude longitude disorder_type event_type fatalities // re-order the dataset

save "$datadir/1_conflicts", replace 

** We now want to create the incidence, onset, and intensity variables

use "$datadir/1_conflicts", clear

g number_conflict=1 if year!=. // We first create a variable taking one for each observations, just to be able to count them afterwards
collapse (sum) number_conflict fatalities , by(year iso_code partneriso) // we collapse the "individual" conflicts by country and year, giving us the number of conflicts and fatalities by country each year. 

/* For the incidence (if the conflict occured or not) and the onset (if a conflict was already here the year before) variables, it's a bit more complicated because we obviously don't have data for countries with no conflict.

What we'll do is first merge with the trade data, and therefore see what year/countries were not successfully merged. 
*/



save "$datadir/2_conflicts", replace 

** end of file
log close

