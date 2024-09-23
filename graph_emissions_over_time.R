#### HEADER -------

## This 

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

load(paste0(proc_data, "/firm_year_belgian_euets.RData"))

df_owindata <- read_csv(paste0(raw_data,"/annual-co2-emissions-per-country.csv"))

# Clean data set -----

firm_year_belgian_euets <- firm_year_belgian_euets %>% 
  mutate(nace2d = str_sub(nace5d, 1, 2)) %>% 
  filter(year >= 2005)

df_owindata <- df_owindata %>% 
  filter((Entity == "European Union (28)" | Entity == "Belgium") & Year >= 1990) %>% 
  rename(entity = Entity, year = Year, emissions = 4) %>% 
  mutate(entity = ifelse(entity == "European Union (28)", "Europe", entity)) %>% 
  select(-c(Code))

# Graphs -------

  library(ggplot2)

  # normalize emissions 2005 = 100
  belgian_euets_emissions <- firm_year_belgian_euets %>%
    group_by(year) %>%
    summarize(emissions = sum(emissions, na.rm = TRUE)) %>% 
    mutate(entity = "Belgian EUETS")
  
  df_total_emissions <- df_owindata %>% 
    bind_rows(belgian_euets_emissions) %>% 
    filter(year <= 2022) %>% 
    group_by(entity) %>% 
    mutate(emissions = emissions/emissions[year == 2005]*100) 
  
  plot1 <- ggplot(df_total_emissions, aes(x = year, y = emissions, group = entity)) +
    geom_line(aes(color = entity, linetype = entity), linewidth = 1.2) +   # Line color and type based on entity
    scale_color_manual(values = c("Belgium" = "#444444", 
                                  "Europe" = "gray", 
                                  "Belgian EUETS" = "#C0362C")) +     # Custom colors
    scale_linetype_manual(values = c("Belgium" = "solid", 
                                     "Europe" = "dashed", 
                                     "Belgian EUETS" = "solid")) +    # Custom linetypes
    # Manually place text annotations at specific coordinates
    geom_text(aes(x = 2015, y = 75, label = "Belgium"), color = "#444444", size = 4, hjust = 0) +
    geom_text(aes(x = 2017, y = 85, label = "Europe"), color = "gray", size = 4, hjust = 0) +
    geom_text(aes(x = 2012, y = 65, label = "Belgian EUETS"), color = "#C0362C", size = 4, hjust = 0) +
    geom_text(aes(x = 2001.5, y = 105, label = "EUETS"), color = "#0072B2", size = 4, hjust = 0) +
    # Add a vertical dotted blue line at the year 2005
    geom_vline(xintercept = 2005, linetype = "dotted", color = "#0072B2", size = 1) +
    # Set the axis labels and title
    labs(title = "",
         x = "",
         y = "CO2 emissions (2005 = 100)",
         color = NULL,  # Remove color legend title
         linetype = NULL) +  # Remove linetype legend title
    theme_minimal() +                 # Use a minimal theme
    theme(
      panel.grid.major = element_blank(),      # Remove major grid lines
      panel.grid.minor = element_blank(),      # Remove minor grid lines
      panel.background = element_rect(fill = "white", color = NA),  # Set background to white
      axis.line = element_line(color = "black"),  # Add solid x and y axis lines
      axis.line.x.bottom = element_line(color = "black"),  # Ensure bottom x-axis line
      axis.line.y.left = element_line(color = "black"),    # Ensure left y-axis line
      legend.position = "none",                 # Remove the legend
      axis.title.y = element_text(margin = margin(r = 15))
    )
  
  # EUETS emissions as a share of Belgian emissions
  df_belgian_emissions <- df_owindata %>% 
    filter(entity == "Belgium") %>% 
    left_join(belgian_euets_emissions %>% select(year, emissions), by = "year") %>%
    rename(emissions_euets = emissions.y, emissions_total = emissions.x) %>% 
    mutate(share_euets = emissions_euets/emissions_total,
           emissions_noneuets = emissions_total - emissions_euets) %>% 
    filter(year >= 2005)
  
  plot2 <- ggplot(df_belgian_emissions, aes(x = year, y = share_euets)) +
    geom_line(color = "#444444", linewidth = 1.2) +   # Line color and type based on entity
    # Set the axis labels and title
    labs(title = "",
         x = "",
         y = "",
         color = NULL,  # Remove color legend title
         linetype = NULL) +  # Remove linetype legend title
    theme_minimal() +                 # Use a minimal theme
    theme(
      panel.grid.major = element_blank(),      # Remove major grid lines
      panel.grid.minor = element_blank(),      # Remove minor grid lines
      panel.background = element_rect(fill = "white", color = NA),  # Set background to white
      axis.line = element_line(color = "black"),  # Add solid x and y axis lines
      axis.line.x.bottom = element_line(color = "black"),  # Ensure bottom x-axis line
      axis.line.y.left = element_line(color = "black"),    # Ensure left y-axis line
      legend.position = "none",                 # Remove the legend
      axis.title.y = element_text(margin = margin(r = 15))
    )
  
  # Belgian emissions split between EUETS and nonEUETS
  df_plot3 <- df_belgian_emissions %>% 
    select(c(year,emissions_total, emissions_euets, emissions_noneuets)) %>% 
    pivot_longer(cols = starts_with("emissions"),
                 names_to = "type",
                 values_to = "emissions") %>%
    mutate(type = case_when(
      type == "emissions_total" ~ "Total",
      type == "emissions_euets" ~ "EUETS",
      type == "emissions_noneuets" ~ "Non-EUETS"
    ),
    emissions = emissions*10^-6)
    
    
  plot3 <- ggplot(df_plot3, aes(x = year, y = emissions, group = type)) +
    geom_line(aes(color = type, linetype = type), linewidth = 1.2) +   # Line color and type based on entity
    scale_color_manual(values = c("Total" = "#444444", 
                                  "EUETS" = "gray", 
                                  "Non-EUETS" = "#C0362C")) +     # Custom colors
    scale_linetype_manual(values = c("Total" = "solid", 
                                     "EUETS" = "solid", 
                                     "Non-EUETS" = "solid")) +    # Custom linetypes
    # Manually place text annotations at specific coordinates
    geom_text(aes(x = 2017, y = 105, label = "Total"), color = "#444444", size = 4, hjust = 0) +
    geom_text(aes(x = 2017, y = 80, label = "EUETS"), color = "gray", size = 4, hjust = 0) +
    geom_text(aes(x = 2017, y = 35, label = "Non-EUETS"), color = "#C0362C", size = 4, hjust = 0) +
    #geom_text(aes(x = 2001.5, y = 105, label = "EUETS"), color = "#0072B2", size = 4, hjust = 0) +
    # Add a vertical dotted blue line at the year 2005
    #geom_vline(xintercept = 2005, linetype = "dotted", color = "#0072B2", size = 1) +
    # Set the axis labels and title
    labs(title = "",
         x = "",
         y = "mi tons of CO2",
         color = NULL,  # Remove color legend title
         linetype = NULL) +  # Remove linetype legend title
    theme_minimal() +                 # Use a minimal theme
    theme(
      panel.grid.major = element_blank(),      # Remove major grid lines
      panel.grid.minor = element_blank(),      # Remove minor grid lines
      panel.background = element_rect(fill = "white", color = NA),  # Set background to white
      axis.line = element_line(color = "black"),  # Add solid x and y axis lines
      axis.line.x.bottom = element_line(color = "black"),  # Ensure bottom x-axis line
      axis.line.y.left = element_line(color = "black"),    # Ensure left y-axis line
      legend.position = "none",                 # Remove the legend
      axis.title.y = element_text(margin = margin(r = 15))
    )

  # evolution of Belgian EUETS emissions by sector

  # evolution of number of firms
  n_firms <- firm_year_input_bundle_euets %>% 
    group_by(year) %>% 
    summarize(count = n_distinct(vat))
  
  ggplot(n_firms, aes(x = year, y = count)) +
    geom_line(color = "blue", size = 1) +  # Line plot
    geom_point(size = 2, color = "red") +  # Add points for each year
    labs(title = "", 
         x = "Year", 
         y = "Number of firms") +
    theme_minimal()
