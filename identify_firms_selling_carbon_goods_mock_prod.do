/*******************************
* This code identify firms selling carbon-related goods using the mock prodcom data set

* I classify carbon-related goods using
	* 1.
	* 2.
********************************/

*------------------------------
* Set-up folders
*------------------------------

if "`c(username)'"=="jardang"{
	global folder "X:/Documents/JARDANG"
	
}

global raw_data "${folder}/carbon_policy_networks/data/raw"
global int_data "${folder}/carbon_policy_networks/data/intermediate"
global proc_data "${folder}/carbon_policy_networks/data/processed"
global output "${folder}/carbon_policy_networks/output"
global code "${folder}/carbon_policy_networks/code"

*------------------------------
* Import data with Klenow goods and adjust variable name to merge
*------------------------------

	tempfile cpa_temp
	use "${proc_data}/cpa_v21_codes_of_klenow_goods.dta", clear
	rename cpa_v21_code cpa_code
	save `cpa_temp'
	
*------------------------------
* Import mock prodcom data and merge
*------------------------------

	use "${raw_data}/NBB/prod.dta", clear
	gen cpa_code = substr(pc8, 1, 6)
	merge m:1 cpa_code using `cpa_temp', keepusing(*) nogen



