					* -----------------------------------------
					* Minerals trade and conflicts: master file
					* -----------------------------------------

					
* Always run that first				
*----------------------------------------------------------------------------------------------	

clear
set more off
global sysdate=c(current_date)
global path "/Users/raphaelboulat/Desktop/RA /Trade & conflicts"
global datadir "${path}/data"
global codedir "${path}/code" 
global tabledir "${path}/tables"
cd "${path}"
*----------------------------------------------------------------------------------------------

*Data wrangling 
do "${codedir}/1_data_cleaning.do"
do "${codedir}/2_data_merging.do"

*Descriptive statistics
do "${codedir}/3_desc_stats.do"

*Regressions 
do "${codedir}/4_regressions.do"



