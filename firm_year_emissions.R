#### HEADER -------

## This code creates data set at the firm-year level with info on
# 1. emissions
# 2. BvD id
# 3. NACE codes
# 4. country codes
# 5. number of allowances allocated for free
# 6. number of total allowances allocated

# Obs:

# 1. firm_id and bvd_id are not 1:1 matching. There are more distinct firm_ids than
# there are bvd_ids. This is because there are some firm_ids which contain "0" in front and some that
# don't, even in cases when the underlying identifier is the same
# (e.g. firm_ids 0419052173 and 419052173 are matched with the same bvd_id)
# also, there are multiple firm_ids for which bvd_id is missing (and therefore belongs to the category
# of firms for which firm_ids are different but bvd_id is the same, in particular bvd_id == "")

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

library(tidyverse)
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately

# Import data ------

load(paste0(proc_data,"/installation_year_emissions.RData"))

df_account <- read.csv(paste0(raw_data,"/EUTL/account.csv"))

# Clean data ------

  df_account <- df_account %>% 
    rename(account_id = id, account_type = accountType_id, bvd_id = bvdId,
           firm_id = companyRegistrationNumber) %>% 
    select(account_id, account_type, bvd_id, installation_id, firm_id) %>% 
    filter(account_type %in% c("100-7","120-0"))
  
  firm_year_emissions  <- installation_year_emissions %>% 
    left_join(df_account %>% select(bvd_id, installation_id),
              by = "installation_id") %>% 
    distinct() %>% 
    group_by(bvd_id, year) %>%
    summarise(
      allocated_free = sum(allocatedFree, na.rm = T),
      allocated_total = sum(allocatedTotal, na.rm = T),
      emissions = sum(verified, na.rm = TRUE),  # Total verified emissions
      bvd_id = first(bvd_id),  # Include bvd_id (constant within each group)
      country_id = first(country_id),  # Include country_id (constant within each group)
      # activity_id associated with the installation with the largest verified emissions
      activity_id = activity_id[which.max(verified)],
      # nace_id associated with the installation with the largest verified emissions
      nace_id_from_eutl = nace_id[which.max(verified)]
    ) %>%
    ungroup() %>% 
    filter(bvd_id != "")

# Save it -------
save(firm_year_emissions, file = paste0(proc_data,"/firm_year_emissions.RData"))
