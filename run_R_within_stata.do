********************* HEADER

** Run R code from within Stata

****************************

// Clear all data and variables
clear all

// Define folder path for Jardanovski
global jarda_folder "X:/Documents/JARDANG"
global code "${jarda_folder}/carbon_policy_networks/code"

// Define the folder path for Gert
global gert_folder // "FILE PATH FOR THE PRODCOM DATA SET"
* Obs: write file path with "/" (as oppoased to "\") and do not include a "/" in the end
* The structure of gert_folder file path should be the same as jarda_folder above

global prodcom_file // "NAME OF THE PRODCOM FILE.dta"

// Run the R script and pass the value of the macro

* 1. run code that saves prodcom file as RData
shell Rscript "${code}/prodcom_as_rdata.R" "${jarda_folder}" "${gert_folder}" "${prodcom_file}" 

* 2. run code that creates firm_product_month_prodcom.RData
shell Rscript "${code}/firm_product_month_prodcom.R" "${jarda_folder}" "${gert_folder}"

* 3. run code that creates descriptive statistics
shell Rscript "${code}/nace_month_prices.R" "${jarda_folder}" "${gert_folder}"





