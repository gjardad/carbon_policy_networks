#### HEADER -------

## Code produces graph that shows evolution of prices over time

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
library(readxl)

# Import data ------
  
  # Yearly prices
  
  file <- paste0(raw_data, "/Eurostat/ippi_by_nace_eu_year.xlsx")
  
  sheets <- excel_sheets(file)
  sheets <- sheets[-c(1:2)]
  
  df_prices <- c()
  
  for(sheet in sheets){
    
    sector <- read_excel(file, sheet = sheet, range = "C7", col_names = FALSE)[[1]]
    country <- read_excel(file, sheet = sheet, range = "A13", col_names = FALSE)[[1]]
    index <- read_excel(file, sheet = sheet, range = "C9", col_names = FALSE)[[1]]
    years <- seq(2000, 2022, by = 1)
    prices <- read_excel(file, sheet = sheet, range = "B13:AU13", col_names = FALSE)
    prices <- prices[, seq(1, ncol(prices), by = 2)]
    prices <- as.vector(unlist(prices))
    prices[prices == ":"] <- NA
    
    dataset <- data.frame(
      country = rep(country, each = length(years)),  # Repeat country for each year
      sector = rep(sector, times = length(prices)),  # Repeat sector for all prices
      year = rep(years, times = length(country)),    # Repeat years for each country
      index = rep(index, times = length(prices)),    # Repeat index for all prices
      price = prices                                 # Add the price values
    )
    
    dataset <- dataset %>%
      mutate(price = as.numeric(price))
    
    df_prices <- bind_rows(df_prices, dataset)
    
  }
  
  emission_price <- read_csv(paste0(raw_data,"/icap_price.csv")) %>% 
    select(1,3) %>% 
    slice(-1) %>% 
    rename(date = 1, price = 2) %>% 
    mutate(price = as.numeric(price))

# Clean data set -----
  
  euets <- c("B07", "B08", "D35", "C19", "C23", "C24", "C25", "C20", "C17")
  noneuets <- setdiff(unique(df_prices$sector), euets)
  
  # only I21 and include euets dummy (for some reason sector %in% euets is not working...)
  df_prices <- df_prices %>%
    mutate(
      euets = case_when(
        sector == "B07" ~ 1,
        sector == "B08" ~ 1,
        sector == "C17" ~ 1,
        sector == "C19" ~ 1,
        sector == "C20" ~ 1,
        sector == "C23" ~ 1,
        sector == "C24" ~ 1,
        sector == "C25" ~ 1,
        TRUE ~ 0                
      )) %>% 
    filter(index == "I21") %>% 
    select(-c(country,index))

  # create avg euets and avg non-euets prices
  avg_prices <- df_prices %>%
    group_by(euets, year) %>%  # Group by 'euets' (0 or 1)
    summarize(price = mean(price, na.rm = TRUE)) %>% 
    mutate(sector = ifelse(euets == 0, "Avg. non-EUETS", "Avg. EUETS"))
  
  # add emission prices and avg.
  emission_price_year <- emission_price %>%
    mutate(year = year(as.Date(date))) %>%  # Extract year from the date
    group_by(year) %>%                      # Group by the extracted year
    summarise(price = mean(as.numeric(price), na.rm = TRUE)) %>% 
    mutate(sector = "Carbon permits", euets = 0)
  
  df_prices <- bind_rows(df_prices, emission_price_year, avg_prices)
  
  # normalize to 100 in 2005
  df_prices <- df_prices %>%
    # Step 1: Get price in 2005 for each sector
    group_by(sector) %>%
    mutate(price_2005 = price[year == 2005][1]) %>%  # Ensure only 1 value is taken for price in 2005 (if there are multiple, take the first one)
    
    # Step 2: Normalize price by dividing by price in 2005
    mutate(
      normalized_price = (price / price_2005) * 100
    ) %>%
    # Drop unnecessary columns
    select(-price_2005) %>% 
    filter(year <= 2020)
  
# Graphs -------

library(ggplot2)
  
  # All data
  
  plot1 <- df_prices %>%
    ggplot(aes(x = year, y = normalized_price, group = sector)) +
    geom_line(aes(color = case_when(
      sector == "Carbon permits" ~ "red",
      euets == 1 ~ "lightblue",   # If euets is 1, color is light blue
      TRUE ~ "lightgray"          # Otherwise, color is light gray
    )), linewidth = 1) +  # Customize line width
    labs(title = "Normalized Price Over Time by Sector",
         x = "Year",
         y = "Normalized Price (Base Year 2005 = 100)",
         color = "Sector") +  # Color legend title
    theme_minimal() +  # Use minimal theme
    theme(
      legend.position = "none",  # Remove legend if not needed
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
    
    avg <- df_prices %>% 
      filter(sector %in% c("Carbon permits", "Avg. non-EUETS", "Avg. EUETS"))
  
  # Avg EUETS and Avg non-EUETS
  
  plot2 <- df_prices %>% 
    filter(sector %in% c("Carbon permits", "Avg. non-EUETS", "Avg. EUETS")) %>% 
    ggplot(aes(x = year, y = normalized_price, group = sector)) +
    geom_line(aes(color = case_when(
      sector == "Carbon permits" ~ "red",
      euets == 1 ~ "lightblue",   # If euets is 1, color is light blue
      TRUE ~ "lightgray"          # Otherwise, color is light gray
    )), linewidth = 1) +  # Customize line width
    labs(title = "Normalized Price Over Time by Sector",
         x = "Year",
         y = "Normalized Price (Base Year 2005 = 100)",
         color = "Sector") +  # Color legend title
    theme_minimal() +  # Use minimal theme
    theme(
      legend.position = "none",  # Remove legend if not needed
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  # Price growth
  
  growth_yoy <- df_prices %>% 
    # Step 1: Calculate percentage change for price and normalized_price within each sector
    group_by(sector) %>%
    arrange(sector, year) %>%
    mutate(
      price_pct_change = (price - lag(price)) / lag(price) * 100,  # Percentage change for price
    ) %>%
    ungroup()
  
  plot3 <- growth_yoy %>% 
    filter(sector %in% c("Carbon permits", "Avg. non-EUETS", "Avg. EUETS")) %>% 
    ggplot(aes(x = year, y = price_pct_change, group = sector)) +
    geom_line(aes(color = case_when(
      sector == "Carbon permits" ~ "red",
      euets == 1 ~ "lightblue",   # If euets is 1, color is light blue
      TRUE ~ "lightgray"          # Otherwise, color is light gray
    )), linewidth = 1) +  # Customize line width
    labs(title = "Normalized Price Over Time by Sector",
         x = "Year",
         y = "Normalized Price (Base Year 2005 = 100)",
         color = "Sector") +  # Color legend title
    theme_minimal() +  # Use minimal theme
    theme(
      legend.position = "none",  # Remove legend if not needed
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  
    
