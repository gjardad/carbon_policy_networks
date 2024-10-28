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

# Clean data ------

df_belgium_euets <- df_belgium_euets %>% 
  group_by(vat_ano) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  select(vat_ano, bvd_id, firm_id)

df_eutl_account_sliced <- df_eutl_account %>% 
  rename(account_id = id, account_type = accountType_id, bvd_id = bvdId,
         firm_id = companyRegistrationNumber) %>% 
  select(account_id, account_type, bvd_id, installation_id, firm_id) %>% 
  filter(account_type %in% c("100-7","120-0")) %>% 
  group_by(bvd_id) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  select(bvd_id, firm_id, installation_id)

eutl_belgian <- df_eutl_account_sliced %>% 
  left_join(df_belgium_euets, by = "bvd_id")

test <- eutl_belgian %>% filter(!is.na(vat_ano) & vat_ano != "")
length(unique(test$vat_ano))

test <- eutl_belgian %>% 
  select(vat_ano) %>% 
  distinct() %>% 
  filter(!is.na(vat_ano) & vat_ano!="") %>% 
  mutate(iseuets = 1)

# there are 292 accounts in EUTL for which bvd_id corresponds to a firm with a VAT in Belgium records

firm_year_emissions  <- installation_year_emissions %>% 
  left_join(df_account %>% select(companyRegistrationNumber, bvdId, installation_id),
            by = "installation_id") %>% 
  rename(firm_id = companyRegistrationNumber, bvd_id = bvdId) %>% 
  group_by(bvd_id, year) %>%
  summarise(
    emissions = sum(verified, na.rm = TRUE),  # Total verified emissions
    country_id = first(country_id)  # Include country_id (constant within each group)
  ) %>%
  ungroup()

be_emissions <- firm_year_emissions %>% 
  filter(!is.na(bvd_id) & bvd_id != "" & country_id == "BE")

length(unique(be_emissions$bvd_id))  
# there are 281 bvdis for which we have emissions data

# what's going on with the 11 bvds present in df_account but for which we don't have emissions data?

no_emissions <- setdiff(test$bvd_id, be_emissions$bvd_id)

hey <- df_account %>%
  filter(bvdId %in% no_emissions & accountType_id == "100-7") %>%
  select(installation_id)

# there are 14 installations for which account type is 100-7 and bvd_id is one of the 11 bvd_ids

load(paste0(proc_data,"/installation_year_emissions.RData"))

hello <- installation_year_emissions %>% 
  filter(installation_id %in% hey$installation_id)

# but none of those installations seem to be in the data set for installation_year_emissions. why?

df_installation <- read.csv(paste0(raw_data,"/EUTL/installation.csv"))

investigate <- df_installation %>% 
  filter(id %in% hey$installation_id) %>% 
  select(id, nace_id)

# it's still unclear to me....



df_installation <- read.csv(paste0(raw_data,"/EUTL/installation.csv")) %>% 
  rename(installation_id = id)
df_compliance <- read.csv(paste0(raw_data,"/EUTL/compliance.csv"))

from_the_source <- df_installation %>% 
  left_join(df_compliance, by = "installation_id")

hello_from_the_source <- df_compliance %>% 
  filter(installation_id %in% hey$installation_id)

#%>% 
  group_by(firm_id, year) %>%
  summarise(
    allocated_free = sum(allocatedFree, na.rm = T),
    allocated_total = sum(allocatedTotal, na.rm = T),
    emissions = sum(verified, na.rm = TRUE),  # Total verified emissions
    bvd_id = first(bvd_id),  # Include bvd_id (constant within each group)
    country_id = first(country_id)  # Include country_id (constant within each group)
    # obs: country_id and bvd_id are only inconsistent for two firms
  ) %>%
  ungroup() %>% 
  filter(firm_id != "")

load(paste0(proc_data,"/installation_year_emissions.RData"))



df_compare2 <- df_account %>% 
  left_join(df_belgium_euets, by = "firm_id")

test <- df_compare %>% filter(!is.na(vat_ano) & vat_ano != "")

test2 <- df_compare2 %>% filter(!is.na(vat_ano) & vat_ano != "")

test3 <- setdiff(test$firm_id.x, test2$firm_id)

#

