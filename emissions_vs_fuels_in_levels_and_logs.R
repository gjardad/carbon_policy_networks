#### HEADER -------

## This code computes the relationship between emssions and fuel consumption
# in levels and in logs

# Fuel consumption is defined as the amount purchased from fuel importers

#####################

## Setup ------
rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Libraries ------

library(ggplot2)

# Import data --------

  load(paste0(proc_data, "/amount_spent_on_fuel_by_firm_year.RData"))

# Generate plots ------

  # Prepare data
  df_plot <- amount_spent_on_fuel_by_firm_year %>%
    filter(emissions > 0,
           amount_spent_on_fuel > 0) %>%  # just in case
    mutate(
      year   = as.factor(year),
      nace5d = as.factor(nace5d),
      log_em = log(emissions),
      log_fuel = log(amount_spent_on_fuel))

  # Plot in levels
  p1 <- ggplot(df_plot, aes(x = amount_spent_on_fuel, y = emissions)) +
    geom_point(alpha = 0.3) +
    #scale_x_continuous(trans = "log10") +
    #scale_y_continuous(trans = "log10") +
    labs(title = "Levels: emissions vs fuel",
         x = "Fuel expenditure",
         y = "Emissions") +
    theme_minimal()

  # Plot in logs
  p2 <- ggplot(df_plot, aes(x = log_fuel, y = log_em)) +
    geom_point(alpha = 0.3) +
    #geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = "Logs: log(emissions) vs log(fuel)",
         x = "log(fuel)",
         y = "log(emissions)") +
    theme_minimal()
  
  p1
  p2
