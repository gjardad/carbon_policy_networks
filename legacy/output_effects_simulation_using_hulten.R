#### HEADER -------

## Output effects from carbon taxation rationalized as productivity shocks

# This code simulates output costs from carbon taxation following document 
# "output_effects_from_carbon_tax"

#####################

## Setup ------
rm(list = ls())

if(Sys.info()[["user"]] =="JARDANG"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Libraries ----


# Import data ------

load(paste0(proc_data,"/dlogpz_shocks_that_imply_hicp_energy_increases_in_diego_jmp.RData"))
load(paste0(proc_data,"/A_coeff_that_implies_output_decrease_in_diego_jmp.RData"))
load(paste0(proc_data, "/ordered_domar_weights_by_year.RData"))
load(paste0(proc_data, "/firm_year_obs_and_imputed_emissions_using_firm_size.RData"))
load(paste0(proc_data,"/firms_total_costs_by_year.RData"))
load(paste0(proc_data,"/vats_as_ordered_in_io_matrix.RData"))

# Compute output effects as in Hulten Theorem ------

  # choose targeting vector; this needs to be list of VATs of targeted firms
  targeted_vat_ids <- c()

  dlogY_list <- list()
  dlogY_list[[1]] <- 0
  dlogY_list[[2]] <- 0
  dlogY_list[[3]] <- 0
  # first three entries are zero because loop starts in 2008, not 2005
  
  i <- 0
  
  for(y in 2008:2022){
    
    i <- i + 1
    
    # find indices of targeted firms in io_matrix
    firms_ids <- vats_as_ordered_in_io_matrix[[i+3]]
    # i+3 because I'm starting loop at 2008 (since that's when series of imputed emissions start)
    # but other lists start in 2005
    
    targeted_indices_in_io_matrix <- match(targeted_vat_ids, firms_ids)
    targeted_indices_in_firm_year_balance_sheet <- which(firm_emissions$vat %in% targeted_vat_ids)
  
    # compute emission intensiveness
    firm_emissions <- firm_year_balance_sheet_and_emissions_using_firm_size %>% 
      filter(year == y) %>% 
      rename(vat = vat_ano) %>% 
      select(vat, emissions)
    
    emissions_unordered <- firm_emissions$emissions
    
    targeted_ordered_emissions <- rep(0, length(firms_ids))
    targeted_ordered_emissions[targeted_indices_in_io_matrix] <- emissions_unordered[targeted_indices_in_firm_year_balance_sheet]
    
    ordered_total_costs <- firms_total_costs_list[[i+3]]
    
    targeted_emission_intensiveness <- targeted_ordered_emissions/ordered_total_costs
    
    # recover dlogpz
    dlogpz <- dlogpz_list[[i+3]]
  
    # compute output effects
    ordered_domar_weights <- ordered_domar_weights_list[[i+3]]
    
    A_coeff <- A_list[[i+3]]
    
    Adlogpz <- A_coeff*dlogpz
    
    dlogY_list[[i+3]] <- Adlogpz * t(ordered_domar_weights) %*% targeted_emission_intensiveness
  
  }