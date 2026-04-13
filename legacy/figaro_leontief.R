#### HEADER -------

## Build sector-country-level network-adj emission intensity
# usig FIGARO data set in 2010

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

  # Initialize an empty data frame to store results
  df_emissions <- data.frame()

  library(readxl)
  file_path <- paste0(raw_data,"/Eurostat/ghg_by_nace_country_2010.xlsx")

  # Get the names of all sheets
  sheet_names <- excel_sheets(file_path)
  sheet_names <- sheet_names[3:length(sheet_names)] # no data in first two

  # Loop through each sheet within data set
  for (sheet in sheet_names) {
  
  # Read the necessary cells from the sheet
  sector <- read_excel(file_path, sheet = sheet, range = "C7", col_names = FALSE)[[1]]
  countries <- read_excel(file_path, sheet = sheet, range = "A13:A44", col_names = FALSE)
  emissions <- read_excel(file_path, sheet = sheet, range = "B13:B44", col_names = FALSE)
  
  # Reshape the data and append to the main data frame
  temp_df <- data.frame(
    sector = sector,
    country = countries,
    ton_emissions = emissions
  )
  
  colnames(temp_df) <- c("sector", "country", "ton_emissions")
  
  # Append to the main dataset
  df_emissions <- rbind(df_emissions, temp_df)
}

  figaro <- read_csv(paste0(raw_data,"/Eurostat/figaro_ind_by_ind_2010.csv"))
  # entry ij is expenditure of sector j on input i in mi euros
  
  emission_price <- read_csv(paste0(raw_data,"/icap_price.csv")) %>% 
    select(1,3) %>% 
    slice(-1) %>% 
    rename(date = 1, price = 2) %>% 
    mutate(price = as.numeric(price))
  
# Emission costs ------
  
  # make sector labels comparable to figaro's
  df_emissions <- df_emissions %>% 
    mutate(sector = case_when(
      sector == "C10-C12" ~ "C10T12",
      sector == "C13-C15" ~ "C13T15",
      sector == "C31_C32" ~ "C31_32",
      sector == "D" ~ "D35",
      sector == "E37-E39" ~ "E37T39",
      sector == "J59_J60" ~ "J59_60",
      sector == "J62_J63" ~ "J62_63",
      sector == "M69_M70" ~ "M69_70",
      sector == "M74_M75" ~ "M74_75",
      sector == "N80-N82" ~ "N80T82",
      sector == "O" ~ "O84",
      sector == "P" ~ "P85",
      sector == "Q87_Q88" ~ "Q87_88",
      sector == "R90-R92" ~ "R90T92",
      TRUE ~ as.character(sector)
    ),
    ton_emissions = ifelse(ton_emissions == ":", 0, as.numeric(ton_emissions)))
  
  emissions_costs <- tibble(label = figaro$rowLabels[1:2944]) %>% 
    mutate(country = sub("_.*", "", label),
           sector = sub(".*?_","", label)) %>% 
    left_join(df_emissions, by = c('country', 'sector')) %>% 
    mutate(ton_emissions = ifelse(is.na(ton_emissions), 0, ton_emissions))

  # set non-EUETS-targeted sectors to zero
  euets <- c("D35", "C19", "B", "C23", "C24", "C20", "C17")
  noneuets <- setdiff(unique(emissions_costs$sector), euets)
  
  emissions_costs$ton_emissions[emissions_costs$sector %in% noneuets] <- 0
  
  # emission prices
  emission_price$date <- as.Date(emission_price$date)
  
  # Calculate the average price per year
  average_price_per_year <- emission_price %>%
    group_by(year = format(date, "%Y")) %>%
    summarize(average_price = mean(price, na.rm = TRUE))
  
  price_2010 <- average_price_per_year[average_price_per_year$year == "2010", "average_price"]
  
  emissions_costs$price_2010 <- rep(price_2010[1,1], length(emissions_costs[,1])) 
  
  emissions_costs <- emissions_costs %>% 
    mutate(emissions_costs = ton_emissions * as.numeric(price_2010)/10^6) # express in million of euros
  
# I-O and Leontief matrices -----
  
  # create the global Z matrix
  # I need to exclude the rows of value added (last 6 rows)
  # and the cols of row names (first col) and cols of final use (starts on col 2946)
  global_z <- figaro[c(1:2944,2948),2:2945]
  global_z <- as.matrix(apply(global_z, 2, as.numeric))
  
  # create total input cost per industry-country
  total_cost <- as.vector(as.tibble(global_z) %>%
                                 summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))))
  
  total_cost <- cbind(as.matrix(as.numeric(total_cost)),emissions_costs[,6]) %>% 
    rename(total_cost = 1) %>% 
    mutate(total_cost = total_cost + emissions_costs) %>% 
    select(total_cost)
  
  # create the diagonal matrix of total cost
  total_cost <- as.numeric(total_cost$total_cost)
  diag_total <- diag(total_cost)
  inv_diag_total <- ifelse(diag_total == 0, 0, 1/diag_total) #because some entries are zero
  
  # create the I-O coeff matrix
  Omega <- t(as.matrix(global_z[-2945,]) %*% inv_diag_total)
  # entry ij is expenditure share of sector i on input j in mi euros
  
  # create the Leontief-inverse matrix
  Psi <- solve((diag(nrow(Omega))-Omega))
  
# Network-adjusted emissions exposure ----
  
  emissions_cost_share <- emissions_costs[,6] / total_cost
  emissions_cost_share <- emissions_cost_share %>% 
    rename(emissions_cost_share = emissions_costs) %>% 
    mutate(emissions_cost_share = ifelse(is.na(emissions_cost_share), 0, emissions_cost_share))
  
  net_exposure <- as.data.frame(Psi %*% as.matrix(emissions_cost_share))
  
  net_exposure$country_sector <- emissions_costs$label
  
  colnames(net_exposure)[1] <- "net_exposure"

# save it -----
  save(net_exposure, file = paste0(proc_data,"/sector_net_exposure_emissions.RData"))
  
