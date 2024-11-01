#### HEADER -------

# Goal of the code is to understand differences between EUTL_Belgium provided by Gert
# and data from EUTL, from the original source

# Code produces data set with VAT and iseuets dummy for firms
# in EUTL for which bvd_id corresponds to a VAT in Belgium records

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

## Import data ------

df_belgium_euets <- read_dta(paste0(raw_data,"/NBB/EUTL_Belgium.dta")) %>% 
  rename(bvd_id = bvdid, firm_id = companyregistrationnumber)

df_eutl_account <- read.csv(paste0(raw_data,"/EUTL/account.csv"))

load(paste0(proc_data,"/installation_year_emissions.RData"))

# Clean data ------

# how many firms in df_belgium_euets?
length(unique(df_belgium_euets$bvd_id)) # 334 BvD ids
length(unique(df_belgium_euets$firm_id)) # 416 firm ids
length(unique(df_belgium_euets$vat_ano))

# how many firms if restrict attention to accounts 100-7, 120-0?
be_emitters <- df_belgium_euets %>% 
  filter(accounttype_id %in% c("100-7","120-0") & bvd_id != "")
length(unique(be_emitters$bvd_id)) # 292 BvD ids
length(unique(be_emitters$vat_ano)) # 292 vats

df_eutl_account <- df_eutl_account %>% 
  rename(account_id = id, account_type = accountType_id, bvd_id = bvdId,
         firm_id = companyRegistrationNumber) %>% 
  filter(account_type %in% c("100-7","120-0"))

be_eutl_account <- df_eutl_account %>% 
  filter(registry_id == "BE" & bvd_id != "")
length(unique(be_eutl_account$bvd_id)) # 292 BvD ids as well

# there are 292 accounts in EUTL for which bvd_id corresponds to a firm with a VAT in Belgium records

# how much of those can I get emissions for?
be_with_emissions <- installation_year_emissions %>% 
  left_join(be_eutl_account %>% select(bvd_id, installation_id, account_type), by = "installation_id") %>% 
  filter(!is.na(bvd_id))

length(unique(be_with_emissions$bvd_id)) # 281 BvD ids

# 11 firms for which we observe accounts but for which we don't have emissions data

# which are those firms?
firms_with_accounts <- unique(be_eutl_account$bvd_id)
firms_with_emissions <- unique(be_with_emissions$bvd_id)
firms_wout_emissions <- setdiff(firms_with_accounts, firms_with_emissions)

accounts_wout_emissions <- be_eutl_account %>% filter(bvd_id %in% firms_wout_emissions)
installations_wout_emissions <- unique(accounts_wout_emissions$installation_id) # 14 installations

installation_year_emissions %>% filter(installation_id %in% installations_wout_emissions) # 0 rows

df_installation <- read.csv(paste0(raw_data,"/EUTL/installation.csv"))
df_compliance <- read.csv(paste0(raw_data,"/EUTL/compliance.csv"))

df_installation %>% filter(id %in% installations_wout_emissions) # all 14 are here
df_compliance %>% filter(installation_id %in% installations_wout_emissions) # information on those 14 is very poor

# Conclusion: 292 VATs in Belgium treated by EUETS, but we only have emissions data for 281 of those. 
# This is true independently of whether we use Gert's crosswalk or directly from EUTL microdata


