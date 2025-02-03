########### HEADER


############################################

# Set-up -------

library(dplyr)

# Import data ------

  library(readxl)

  file <- "C:/Users/jota_/Downloads/sts_inpp_a__custom_13927145_spreadsheet.xlsx"

  sheets <- excel_sheets(file)
  sheets <- sheets[3:64]
  
  df_prices <- c()
  
  for(sheet in sheets){
    
    sector <- read_excel(file, sheet = sheet, range = "C7", col_names = FALSE)[[1]]
    country <- read_excel(file, sheet = sheet, range = "A13:A14", col_names = FALSE)[[1]]
    index <- read_excel(file, sheet = sheet, range = "C9", col_names = FALSE)[[1]]
    years <- seq(2000,2023,1)
    prices <- read_excel(file, sheet = sheet, range = "B13:AV14", col_names = FALSE)
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
    
    df_prices <- bind_rows(df_prices, dataset)
    
  }
  
  

  
# Clean data ---------