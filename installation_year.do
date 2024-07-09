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

global nbb_data "${folder}/NBB"
global eutl_data "${folder}/EUTL"
