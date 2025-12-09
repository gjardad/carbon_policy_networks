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

# Import and clean data --------

  load(paste0(proc_data, "/amount_spent_on_fuel_by_firm_year.RData"))
  
  load(paste0(proc_data, "/firm_year_belgian_euets.RData"))
  
  df <- amount_spent_on_fuel_by_firm_year %>%
    left_join(firm_year_belgian_euets %>% select(vat, year, revenue),
              by = c("vat_j_ano" = "vat", "year")
    ) %>%
    filter(emissions > 0,
           !is.na(amount_spent_on_fuel)) %>%
    mutate(
      year   = as.factor(year),
      nace5d = as.factor(nace5d)
    )

# Drop singleton firms -------

  #obs: there are some sectors with only one firm among the firms with emissions > 0
  # this means it's not possible to estimate the NACE fixed-effect in the LOFO regs
  # so we build training data excluding singleton firms
  
  sector_firm_counts <- df %>%
    group_by(nace5d) %>%
    summarise(n_firms = n_distinct(vat_j_ano), .groups = "drop")
  
  valid_sectors <- sector_firm_counts %>%
    filter(n_firms >= 2) %>%
    pull(nace5d)
  
  df_no_singletons <- df %>% filter(nace5d %in% valid_sectors)
  
  df_no_singletons <- df_no_singletons %>%
    mutate(year = factor(year),
           nace5d = factor(nace5d))

# Generate plots ------

  # Prepare data
  df_plot <- df_no_singletons %>%
    mutate(
      year   = as.factor(year),
      nace5d = as.factor(nace5d),
      log_em = log(emissions),
      log_fuel = log(amount_spent_on_fuel_excl_euets_importers))

  # Plot in levels
  p1 <- ggplot(df_plot, aes(x = amount_spent_on_fuel_excl_euets_importers, y = emissions)) +
    geom_point(alpha = 0.3) +
    labs(
      title = "",
      x = "Fuel Consumption",
      y = "Emissions"
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 16, margin = margin(t = 14)),
      axis.title.y = element_text(size = 16, margin = margin(r = 14)),
      axis.text    = element_text(size = 14)
    )
  
  # Plot in logs
  p2 <- ggplot(df_plot, aes(x = log_fuel, y = log_em)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(
      title = "",
      x = "Fuel Consumption",
      y = "Emissions"
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 16, margin = margin(t = 14)),
      axis.title.y = element_text(size = 16, margin = margin(r = 14)),
      axis.text    = element_text(size = 14)
    )
  
# Save it --------
ggsave(paste0(output, "/emissions_fuel_levels.png"), p1, width = 8, height = 5, dpi = 300)
ggsave(paste0(output, "/emissions_fuel_logs.png"), p2, width = 8, height = 5, dpi = 300)
  

