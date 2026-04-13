/*******************************************************************************
Creates data set at the installation-year level with information on
1. emissions
2. BvD id
3. acitivity and nace ids

GJ
*******************************************************************************/

*------------------------------
* Set-up folders
*------------------------------

if "`c(username)'"=="JARDANG"{
	global folder "X:/Documents/JARDANG" 
	
}

global raw_data "${folder}/carbon_policy_networks/data/raw"
global int_data "${folder}/carbon_policy_networks/data/intermediate"
global proc_data "${folder}/carbon_policy_networks/data/processed"

*------------------------------
* Read in installation info
*------------------------------

	import delimited "${raw_data}/EUTL/installation.csv", clear
	
	rename id installation_id
	
	keep installation_id nace_id activity_id
	
*------------------------------
* Read in account info
*------------------------------
	
preserve
	tempfile account
	
	import delimited "${raw_data}/EUTL/account.csv", clear
	
	save "`account'"
restore

*------------------------------
* Merge account info with installation info
*------------------------------

	merge 1:m installation_id using "`account'", gen(first_merge)
	
	drop if first_merge == 2 // those are obs for which installation_id is missing
	
	duplicates tag installation_id, g(dup)
	
	// two types of duplicates: installations of the type "esd" and installations which have both former (FOHA, type 120-0) and current (OHA, type 100-7) accounts
	
	drop if dup > 1 // this eliminates installations of type "esd", which we dont care about
	
	// among the installations which have both FOHA and OHA, is bvdid the same?
	bysort installation_id (bvdid): gen bvdid_consistent = bvdid[1] == bvdid[_N]
	// not for all of them
	
	drop if accounttype_id == "120-0"
	
*------------------------------
* Read in compliance info
*------------------------------
	
preserve

	tempfile emissions

	import delimited "${raw_data}/EUTL/compliance.csv", clear
	
	save "`emissions'"
restore

*------------------------------
* Merge compliance info with installation info
*------------------------------

	merge 1:m installation_id using "`emissions'", gen(second_merge)
	
*------------------------------
* Merge account info with installation info
*------------------------------

	drop if _merge == 2 // those are obs for which installation_id is missing
	
	drop if 

	drop _merge
