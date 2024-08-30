#### HEADER -------

## This code creates data set with avg and end-of-year EUETS permit prices by year

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

df_price <- read.csv(paste0(raw_data,"/icap_price.csv"))
colnames(df_price) <- as.character(unlist(df_price[1, ]))
df_price <- df_price[-1,]

df_price_index <- read.csv(paste0(raw_data,"/ppi_euro19_monthly.csv")) %>% 
  rename(date = 1, ppi = 2)

# Create df ------

df_price_index$date <- as.Date(df_price_index$date)
df_price_index <- df_price_index %>% 
  mutate(date = format(date, "%Y-%m"))

df_price <- df_price %>%
  select(c(1,3))
colnames(df_price) <- c("date","price")

df_price$date <- as.Date(df_price$date)

df_price_month <- df_price %>%
  mutate(year_month = format(date, "%Y-%m")) %>% 
  select(c(2,3)) %>% 
  rename(date = 2) %>% 
  mutate(price = as.numeric(price)) %>% 
  group_by(date) %>% 
  summarize(monthly_avg = mean(price, na.rm = TRUE)) %>% 
  left_join(df_price_index, by = "date")

df_price_month$date <- as.Date(paste0(df_price_month$date, "-01"), format = "%Y-%m-%d")

df_price_month <- df_price_month %>% 
  filter(as.numeric(format(date, "%Y")) <= 2022)

df_price_month <- df_price_month %>%
  rename(nominal_price = monthly_avg) %>% 
  mutate(real_price = nominal_price/ppi*100)

df_price_year <- df_price_month %>% 
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>% 
  summarize(nominal_price = mean(nominal_price, na.rm = TRUE),
            real_price = mean(real_price, na.rm = TRUE))
  
# Save it ------
save(df_price_month, file = paste0(proc_data,"/euets_prices_month.RData"))  
save(df_price_year, file = paste0(proc_data,"/euets_prices_year.RData"))  


