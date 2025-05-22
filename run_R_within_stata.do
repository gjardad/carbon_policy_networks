********************* HEADER

** Run R code from within Stata

****************************

// Clear all data and variables
clear all

// Define folder path for Jardanovski
global jarda_folder "X:/Documents/JARDANG"
global code "${jarda_folder}/carbon_policy_networks/code"

// Define the folder path for PRODCOM
global gert_folder "X:/Documents/BIJNENG/Main/Main/Raw"
global prodcom_file "prodcom_12_23.dta"

// Dfine the folder path for output
global prodcom_output "E:/Documents/bijneng/Jardanovski/Results"

// Run the R script and pass the value of the macro

* 1. run code that saves prodcom file as RData
shell Rscript "${code}/prodcom_as_rdata.R" "${jarda_folder}" "${gert_folder}" "${prodcom_file}" "${prodcom_output}"

* 2. run code that creates firm_product_month_prodcom.RData
shell Rscript "${code}/firm_product_month_prodcom.R" "${jarda_folder}" "${gert_folder}"
"${prodcom_output}"

* 3. run code that creates descriptive statistics
shell Rscript "${code}/nace_month_prices.R" "${jarda_folder}" "${gert_folder}" "${prodcom_output}"





