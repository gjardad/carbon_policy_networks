#### HEADER -------

## Creates table that compares agg. emissions and EUETS emissions by sector,
# using different procedures to group emissions into sectors

# OBS: In 2022 table there are differences between emissions in column "shares_crf"
# and emissions in column "crf" for sectors "C17-C18", "C23", and "C24-C25"
# In theory, this shouldn't happen since shares_crf is a weighted sum of crf emissions
# and when summing across all sectors mapped to a particular CRF category, the weight is 1.
# However, the difference arises because:
# 1. I excluded emissions from 1A1c because it's small compared to other categories and splitting
# 1A1c among the sectors that are mapped to it would require some work
# 2. in Appendix XII they don't list emissions from 2H1 separately, only emissions from 2H. This is not currently linked
# to sector C17,C18 even though it should. FIX THIS. But also, size of emissions in 2H1 is negligible.
# 3. similar reasoning for emissions from 2A4 for sector C23

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
library(dplyr)
library(readxl)

# Import data -----

load(paste0(proc_data, "/euets_emissions_by_activity.RData"))
load(paste0(proc_data, "/euets_emissions_by_nace_using_installation.RData"))
load(paste0(proc_data, "/euets_emissions_by_nace_using_firms.RData"))

load(paste0(proc_data, "/agg_emissions_by_nace_year_using_pefa.RData"))
load(paste0(proc_data, "/agg_emissions_by_nace_year_using_shares_crf.RData"))
agg_emissions_by_nace_year_eurostat <- read_csv(paste0(raw_data, "/Eurostat/eurostat_emissions_by_sector_belgium.csv")) %>% 
  filter(airpol == "CO2:Carbon dioxide") %>% 
  select(c(5,8,9)) %>% 
  rename(nace = 1, year = 2, emissions = 3) %>% 
  mutate(nace = str_extract(nace, "^[^:]+")) %>% 
  filter(!nace %in% c("A", "C", "E", "G", "G-U_X_H", "HH", "HH_HEAT", "HH_OTH",
                      "HH_TRA", "TOTAL", "TOTAL_HH"))

annex_xii <- read_excel(paste0(raw_data, "/NIR/BE_2024_Art14_AnnexXII_Consistency_with_ETS_280224.xlsx"))[15:73, -c(5)] %>% 
  rename(crf = 1, crf_emissions = 2, ets_emissions = 3, ratio = 4) %>% 
  mutate(crf_code = str_extract(crf, "^[^ ]+")) %>% 
  select(-crf) %>% 
  select(crf_code, crf_emissions, ets_emissions, ratio) %>% 
  filter(!is.na(crf_code), !crf_emissions %in% c("IE (2B10 other)", "NO"),
         crf_code != "Iron") %>% 
  mutate(crf_emissions = as.numeric(crf_emissions),
         ets_emissions = as.numeric(ets_emissions),
         ratio = as.numeric(ratio)) %>% 
  mutate(type = case_when(
    substr(crf_code, 1, 1) == "1" ~ "energy",
    substr(crf_code, 1, 1) == "2" ~ "ind_process",
    substr(crf_code, 1, 1) == "5" ~ "waste"
  ))

crf_to_nace_crosswalk <- read_excel(paste0(raw_data, "/NIR/Annex-I-(Correspondence-between-CRF-NFR-NACE-Rev.-2)-to-Manual-for-Air-Emissions-Accounts-(2015-edition).xlsx"),
                                    sheet = "Correspondence-CRF-NFR-NACE")

# Make NACE sectors comparable ------

agg_emissions_by_nace_year_eurostat <- agg_emissions_by_nace_year_eurostat %>% 
  mutate(nace = ifelse(nace == "D", "D35", nace))
  
# Build table for year 2022 -------

df_2022 <- agg_emissions_by_nace_year_eurostat %>% 
  filter(year == 2022) %>% 
  rename(eurostat = emissions) %>% # emissions in tonnes
  left_join(agg_emissions_by_nace_year_using_pefa, by = c("nace", "year")) %>% 
  rename(pefa = agg_emissions) %>% 
  left_join(agg_emissions_by_nace_year_using_shares_crf, by = c("nace", "year")) %>% 
  rename(shares_crf = agg_emissions) %>% 
  mutate(eurostat = eurostat/10^3) %>% # measured in ktonnes
  left_join(euets_emissions_by_nace_using_firms, by = c("nace", "year")) %>% 
  rename(euets = emissions) %>%
  #left_join(euets_emissions_by_nace_using_installation, by = c("nace", "year")) %>% 
  #rename(euets_installation = emissions) %>% 
  mutate(euets = euets/10^3) %>% 
  #       euets_installation = euets_installation/10^3) %>% # measured in ktonnes
  mutate(across(everything(), ~ replace_na(., 0))) %>% 
  mutate(nace = ifelse(nace == "D", "D35", nace))

# create rows C17-C18, C20-C21, C24-C25
for(row in c("C17-C18", "C20-C21", "C24-C25")){
  
  nace1 <- substr(row, 1, 3)
  nace2 <- substr(row, 5, 7)
  
  new_row <- df_2022 %>%
    filter(nace %in% c(nace1, nace2)) %>%
    group_by(year) %>%
    summarise(
      nace = row,
      eurostat = sum(eurostat),
      pefa = sum(pefa),
      shares_crf = sum(shares_crf),
      euets = sum(euets, na.rm = T),
      #euets_installation = sum(euets_installation, na.rm = T),
      .groups = 'drop'
    )
  
  df_2022 <- bind_rows(df_2022, new_row)
  
}

df_2022 <- df_2022 %>% 
  left_join(annex_xii_comparable, by = "nace") %>% 
  mutate(ratio = euets/shares_crf,
         #ratio_installation = euets_installation/shares_crf,
         ratio_crf = as.numeric(ratio_crf))

sector_vector <- c(
  "Mining and quarrying",
  "Food, beverages and tobacco products",
  "Textiles, wearing apparel, and leather",
  "Wood products",
  "",  # C17-C18 has an empty sector
  "Paper",
  "Printing",
  "Coke and refined petroleum",
  "",  # C20-C21 has an empty sector
  "Chemicals",
  "Basic pharmaceuticals",
  "Rubber and plastic products",
  "Non-metallic mineral products",
  "",  # C24-C25 has an empty sector
  "Basic metals (iron & steel)",
  "Fabricated metal products",
  "Electricity, gas, and steam supply"
)

nace_vector <- c(
  "B",
  "C10-C12",
  "C13-C15",
  "C16",
  "C17-C18",
  "C17",
  "C18",
  "C19",
  "C20-C21",
  "C20",
  "C21",
  "C22",
  "C23",
  "C24-C25",
  "C24",
  "C25",
  "D35"
)

col_order <- c("nace", "sector", "eurostat", "pefa", "shares_crf",
               "euets", "ratio", "crf", "euets_crf", "ratio_crf")

row_order <-  c("A01", "A02", "A03", "B", "C10-C12", "C13-C15", "C16",
                "C17-C18", "C17", "C18", "C19", "C20-C21", "C20", "C21",
                "C22", "C23", "C24-C25", "C24",  "C25", "C26", "C27", "C28",
                "C29", "C30", "C31_C32", "C33", "D35", "E36", "E37-E39", "F",
                "G45", "G46", "G47", "H", "H49", "H50", "H51", "H52", "H53",
                "I", "J", "J58", "J59_J60", "J61", "J62_J63", "K", "K64", "K65",
                "K66", "L", "L68A", "M", "M69_M70", "M71", "M72", "M73", "M74_M75",
                "N", "N77", "N78", "N79", "N80-N82", "O", "P", "Q", "Q86",
                "Q87_Q88", "R", "R90-R92", "R93", "S", "S94", "S95", "S96", "T", "U")
 
df_2022 <- df_2022 %>%
  mutate(across(eurostat:ratio, ~ formatC(., format = "f", digits = 2, big.mark = ","))) %>% 
  select(-c(year)) %>% 
  mutate(nace = factor(nace, levels = row_order)) %>%
  arrange(nace) %>% 
  filter(nace %in% nace_vector) %>% 
  mutate(sector = sector_vector) %>% 
  select(all_of(col_order))

df_2022<- df_2022 %>% 
  mutate(across(everything(), ~ case_when(
    is.na(.) | . == "NA" | . == "NaN" | . == "Inf" | . == " NA" | . == "-Inf" ~ "$-$",
    TRUE ~ as.character(.)
  )))

library(kableExtra)
latex_table_2022 <- kable(df_2022, format = "latex", booktabs = TRUE,
                          escape = FALSE,
                          caption = "Table Caption Here")

# Build table for other years -------
ready <- F
if(ready == T){

# Build table for year 2023 -------

# Build table for year 2012 -------

df_2012 <- agg_emissions_by_nace_year_eurostat %>% 
  filter(year == 2012) %>% 
  rename(eurostat = emissions) %>% # emissions in tonnes
  left_join(agg_emissions_by_nace_year_using_pefa, by = c("nace", "year")) %>% 
  rename(pefa = agg_emissions) %>% 
  left_join(agg_emissions_by_nace_year_using_shares_crf, by = c("nace", "year")) %>% 
  rename(shares_crf = agg_emissions) %>% 
  mutate(eurostat = eurostat/10^3) %>% # measured in ktonnes
  left_join(euets_emissions_by_nace_using_firms, by = c("nace", "year")) %>% 
  rename(euets = emissions) %>%
  #left_join(euets_emissions_by_nace_using_installation, by = c("nace", "year")) %>% 
  #rename(euets_installation = emissions) %>% 
  mutate(euets = euets/10^3) %>% 
  #       euets_installation = euets_installation/10^3) %>% # measured in ktonnes
  mutate(across(everything(), ~ replace_na(., 0))) %>% 
  mutate(nace = ifelse(nace == "D", "D35", nace))

# create rows C17-C18, C20-C21, C24-C25
for(row in c("C17-C18", "C20-C21", "C24-C25")){
  
  nace1 <- substr(row, 1, 3)
  nace2 <- substr(row, 5, 7)
  
  new_row <- df_2012 %>%
    filter(nace %in% c(nace1, nace2)) %>%
    group_by(year) %>%
    summarise(
      nace = row,
      eurostat = sum(eurostat),
      pefa = sum(pefa),
      shares_crf = sum(shares_crf),
      euets = sum(euets, na.rm = T),
      #euets_installation = sum(euets_installation, na.rm = T),
      .groups = 'drop'
    )
  
  df_2012 <- bind_rows(df_2012, new_row)
  
}

df_2012 <- df_2012 %>% 
  mutate(ratio = euets/shares_crf)

sector_vector <- c(
  "Mining and quarrying",
  "Food, beverages and tobacco products",
  "Textiles, wearing apparel, and leather",
  "Wood products",
  "",  # C17-C18 has an empty sector
  "Paper",
  "Printing",
  "Coke and refined petroleum",
  "",  # C20-C21 has an empty sector
  "Chemicals",
  "Basic pharmaceuticals",
  "Rubber and plastic products",
  "Non-metallic mineral products",
  "",  # C24-C25 has an empty sector
  "Basic metals (iron & steel)",
  "Fabricated metal products",
  "Electricity, gas, and steam supply"
)

nace_vector <- c(
  "B",
  "C10-C12",
  "C13-C15",
  "C16",
  "C17-C18",
  "C17",
  "C18",
  "C19",
  "C20-C21",
  "C20",
  "C21",
  "C22",
  "C23",
  "C24-C25",
  "C24",
  "C25",
  "D35"
)

col_order <- c("nace", "sector", "eurostat", "pefa", "shares_crf",
               "euets", "ratio")

row_order <-  c("A01", "A02", "A03", "B", "C10-C12", "C13-C15", "C16",
                "C17-C18", "C17", "C18", "C19", "C20-C21", "C20", "C21",
                "C22", "C23", "C24-C25", "C24",  "C25", "C26", "C27", "C28",
                "C29", "C30", "C31_C32", "C33", "D35", "E36", "E37-E39", "F",
                "G45", "G46", "G47", "H", "H49", "H50", "H51", "H52", "H53",
                "I", "J", "J58", "J59_J60", "J61", "J62_J63", "K", "K64", "K65",
                "K66", "L", "L68A", "M", "M69_M70", "M71", "M72", "M73", "M74_M75",
                "N", "N77", "N78", "N79", "N80-N82", "O", "P", "Q", "Q86",
                "Q87_Q88", "R", "R90-R92", "R93", "S", "S94", "S95", "S96", "T", "U")

df_2012 <- df_2012 %>%
  mutate(across(eurostat:ratio, ~ formatC(., format = "f", digits = 2, big.mark = ","))) %>% 
  select(-c(year)) %>% 
  mutate(nace = factor(nace, levels = row_order)) %>%
  arrange(nace) %>% 
  filter(nace %in% nace_vector) %>% 
  mutate(sector = sector_vector) %>% 
  select(all_of(col_order))

df_2012<- df_2012 %>% 
  mutate(across(everything(), ~ case_when(
    is.na(.) | . == "NA" | . == "NaN" | . == "Inf" | . == " NA" | . == "-Inf" ~ "$-$",
    TRUE ~ as.character(.)
  )))

library(kableExtra)
latex_table_2012 <- kable(df_2012, format = "latex", booktabs = TRUE,
                          escape = FALSE,
                          caption = "Table Caption Here")

# Build table for year 2008 -------

df_2008 <- agg_emissions_by_nace_year_eurostat %>% 
  filter(year == 2008) %>% 
  rename(eurostat = emissions) %>% # emissions in tonnes
  left_join(agg_emissions_by_nace_year_using_pefa, by = c("nace", "year")) %>% 
  rename(pefa = agg_emissions) %>% 
  left_join(agg_emissions_by_nace_year_using_shares_crf, by = c("nace", "year")) %>% 
  rename(shares_crf = agg_emissions) %>% 
  mutate(eurostat = eurostat/10^3) %>% # measured in ktonnes
  left_join(euets_emissions_by_nace_using_firms, by = c("nace", "year")) %>% 
  rename(euets = emissions) %>%
  #left_join(euets_emissions_by_nace_using_installation, by = c("nace", "year")) %>% 
  #rename(euets_installation = emissions) %>% 
  mutate(euets = euets/10^3) %>% 
  #       euets_installation = euets_installation/10^3) %>% # measured in ktonnes
  mutate(across(everything(), ~ replace_na(., 0))) %>% 
  mutate(nace = ifelse(nace == "D", "D35", nace))

# create rows C17-C18, C20-C21, C24-C25
for(row in c("C17-C18", "C20-C21", "C24-C25")){
  
  nace1 <- substr(row, 1, 3)
  nace2 <- substr(row, 5, 7)
  
  new_row <- df_2008 %>%
    filter(nace %in% c(nace1, nace2)) %>%
    group_by(year) %>%
    summarise(
      nace = row,
      eurostat = sum(eurostat),
      pefa = sum(pefa),
      shares_crf = sum(shares_crf),
      euets = sum(euets, na.rm = T),
      #euets_installation = sum(euets_installation, na.rm = T),
      .groups = 'drop'
    )
  
  df_2008 <- bind_rows(df_2008, new_row)
  
}

df_2008 <- df_2008 %>% 
  mutate(ratio = euets/shares_crf)

sector_vector <- c(
  "Mining and quarrying",
  "Food, beverages and tobacco products",
  "Textiles, wearing apparel, and leather",
  "Wood products",
  "",  # C17-C18 has an empty sector
  "Paper",
  "Printing",
  "Coke and refined petroleum",
  "",  # C20-C21 has an empty sector
  "Chemicals",
  "Basic pharmaceuticals",
  "Rubber and plastic products",
  "Non-metallic mineral products",
  "",  # C24-C25 has an empty sector
  "Basic metals (iron & steel)",
  "Fabricated metal products",
  "Electricity, gas, and steam supply"
)

nace_vector <- c(
  "B",
  "C10-C12",
  "C13-C15",
  "C16",
  "C17-C18",
  "C17",
  "C18",
  "C19",
  "C20-C21",
  "C20",
  "C21",
  "C22",
  "C23",
  "C24-C25",
  "C24",
  "C25",
  "D35"
)

col_order <- c("nace", "sector", "eurostat", "pefa", "shares_crf",
               "euets", "ratio")

row_order <-  c("A01", "A02", "A03", "B", "C10-C12", "C13-C15", "C16",
                "C17-C18", "C17", "C18", "C19", "C20-C21", "C20", "C21",
                "C22", "C23", "C24-C25", "C24",  "C25", "C26", "C27", "C28",
                "C29", "C30", "C31_C32", "C33", "D35", "E36", "E37-E39", "F",
                "G45", "G46", "G47", "H", "H49", "H50", "H51", "H52", "H53",
                "I", "J", "J58", "J59_J60", "J61", "J62_J63", "K", "K64", "K65",
                "K66", "L", "L68A", "M", "M69_M70", "M71", "M72", "M73", "M74_M75",
                "N", "N77", "N78", "N79", "N80-N82", "O", "P", "Q", "Q86",
                "Q87_Q88", "R", "R90-R92", "R93", "S", "S94", "S95", "S96", "T", "U")

df_2008 <- df_2008 %>%
  mutate(across(eurostat:ratio, ~ formatC(., format = "f", digits = 2, big.mark = ","))) %>% 
  select(-c(year)) %>% 
  mutate(nace = factor(nace, levels = row_order)) %>%
  arrange(nace) %>% 
  filter(nace %in% nace_vector) %>% 
  mutate(sector = sector_vector) %>% 
  select(all_of(col_order))

df_2008<- df_2008 %>% 
  mutate(across(everything(), ~ case_when(
    is.na(.) | . == "NA" | . == "NaN" | . == "Inf" | . == " NA" | . == "-Inf" ~ "$-$",
    TRUE ~ as.character(.)
  )))

library(kableExtra)
latex_table_2008 <- kable(df_2008, format = "latex", booktabs = TRUE,
                          escape = FALSE,
                          caption = "Table Caption Here")
  
}








