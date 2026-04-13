#### HEADER -------

## Plot EUTL/Aggregate emissions over time by sector

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

# Import data -----

load(paste0(proc_data, "/euets_emissions_by_nace_using_installation.RData"))
load(paste0(proc_data, "/agg_emissions_by_nace_year_using_shares_crf.RData"))
load(paste0(proc_data, "/emissions_by_nace_group_annex_xii_2024.RData"))
nace_label <- read_csv(paste0(raw_data, "/list_nace_codes_brief.csv"))

# Create group of NACE sectors that map to Annex XII -------

nace_groups <- list(
  "C17-C18" = c("C17", "C18"),
  "C20-C21" = c("C20", "C21"),
  "C24-C25" = c("C24", "C25")
)

  # for agg emissions
  summed_rows <- bind_rows(
    lapply(names(nace_groups), function(group_name) {
      agg_emissions_by_nace_year_using_shares_crf %>%
        filter(nace %in% nace_groups[[group_name]]) %>%
        group_by(year) %>%
        summarise(nace = group_name, agg_emissions = sum(agg_emissions, na.rm = TRUE), .groups = "drop")
    })
  )
  
  # Bind new rows back to the original dataframe
  agg_emissions_by_nace_year_using_shares_crf <- bind_rows(agg_emissions_by_nace_year_using_shares_crf, summed_rows)

  # for EUETS emissions
  summed_rows <- bind_rows(
    lapply(names(nace_groups), function(group_name) {
      euets_emissions_by_nace_using_installation %>%
        filter(nace %in% nace_groups[[group_name]]) %>%
        group_by(year) %>%
        summarise(nace = group_name, emissions = sum(emissions, na.rm = TRUE), .groups = "drop")
    })
  )
  
  # Bind new rows back to the original dataframe
  euets_emissions_by_nace_using_installation <- bind_rows(euets_emissions_by_nace_using_installation, summed_rows)

# Create graph by sector ------
  
  df <- euets_emissions_by_nace_using_installation %>%
    filter(year >= 2008, year <= 2022) %>%  
    mutate(emissions = emissions/10^3) %>% 
    left_join(agg_emissions_by_nace_year_using_shares_crf, by = c("nace", "year")) %>%
    mutate(ratio = emissions / agg_emissions) %>% 
    left_join(emissions_by_nace_annex_xii_24 %>% select(nace, year, ratio_crf), by = c("nace", "year")) %>% 
    left_join(nace_label, by = "nace")
  
  nace_sectors <- unique(euets_emissions_by_nace_using_installation$nace)
  
  for(i in seq_along(nace_sectors)) {
    
    sector <- nace_sectors[i]
    
    df_plot <- df %>%
      filter(nace == sector)
    
    sector_name <- df_plot$label[[1]]
    
    scale_factor <- max(c(df_plot$emissions, df_plot$agg_emissions), na.rm = TRUE)
    
    plot <- ggplot(df_plot, aes(x = year)) +
      geom_line(aes(y = emissions, color = "EUETS", linetype = "EUETS"), size = 1) +
      geom_line(aes(y = agg_emissions, color = "Agg. Emissions", linetype = "Agg. Emissions"), size = 1) +
      geom_line(aes(y = ratio * scale_factor, color = "Ratio", linetype = "Ratio"), size = 1) +
      geom_hline(
        data = df_plot %>% filter(!is.na(ratio_crf)),
        aes(yintercept = ratio_crf * scale_factor, color = "Ratio in Nat. Inv.", linetype = "Ratio in Nat. Inv."),
        size = 1
      ) +
      scale_y_continuous(
        name = "Emissions",
        sec.axis = sec_axis(~ . / scale_factor, name = "Ratio (EUETS / Aggregate)")
      ) +
      scale_color_manual(values = c(
        "EUETS" = "black",
        "Agg. Emissions" = "gray40",
        "Ratio" = "black",
        "Ratio in Nat. Inv." = "red"
      )) +
      scale_linetype_manual(values = c(
        "EUETS" = "solid",
        "Agg. Emissions" = "solid",
        "Ratio" = "dotted",
        "Ratio in Nat. Inv." = "dashed"
      )) +
      labs(title = paste0("Sector ", sector, ": ", sector_name), x = NULL, color = NULL, linetype = NULL) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.y.left = element_text(margin = margin(r = 10)),
        axis.title.y.right = element_text(margin = margin(l = 10))
      )
    
    ggsave(paste0(output, "/euets_vs_agg_sector_", nace_sectors[i], ".png"), plot, width = 8, height = 6)
  }
  
# Create graph for all of stationary installations ------
  
  total_euets <- euets_emissions_by_nace_using_installation %>% 
    group_by(year) %>% 
    summarise(total_euets_emissions = sum(emissions, na.rm=T))
  
  total_emissions <- agg_emissions_by_nace_year_using_shares_crf %>%
    filter(!(str_starts(nace, "[IJKLMNQRS]") & str_length(nace) > 1)) %>% 
    group_by(year) %>% 
    summarise(total_stationary_emissions = sum(agg_emissions, na.rm=T)) %>% 
    left_join(total_euets, by = "year") %>% 
    mutate(total_euets_emissions = total_euets_emissions/10^3,
           ratio = total_euets_emissions/total_stationary_emissions)
  
  scale_factor <- max(c(total_emissions$total_euets_emissions, total_emissions$total_stationary_emissions), na.rm = TRUE)
  
  plot <- ggplot(total_emissions, aes(x = year)) +
    geom_line(aes(y = total_euets_emissions, color = "EUETS", linetype = "EUETS"), size = 1) +
    geom_line(aes(y = total_stationary_emissions, color = "Agg. Emissions", linetype = "Agg. Emissions"), size = 1) +
    geom_line(aes(y = ratio * scale_factor, color = "Ratio", linetype = "Ratio"), size = 1) +
    #geom_hline(
    #  data = df_plot %>% filter(!is.na(ratio_crf)),
    #  aes(yintercept = ratio_crf * scale_factor, color = "Ratio in Nat. Inv.", linetype = "Ratio in Nat. Inv."),
    #  size = 1
    #) +
    scale_y_continuous(
      name = "Emissions",
      sec.axis = sec_axis(~ . / scale_factor, name = "Ratio (EUETS / Aggregate)")
    ) +
    scale_color_manual(values = c(
      "EUETS" = "black",
      "Agg. Emissions" = "gray40",
      "Ratio" = "black"
      #"Ratio in Nat. Inv." = "red"
    )) +
    scale_linetype_manual(values = c(
      "EUETS" = "solid",
      "Agg. Emissions" = "solid",
      "Ratio" = "dotted"
     # "Ratio in Nat. Inv." = "dashed"
    )) +
    labs(title = "Share of stationary emissions covered by EUETS", x = NULL, color = NULL, linetype = NULL) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title.y.left = element_text(margin = margin(r = 10)),
      axis.title.y.right = element_text(margin = margin(l = 10))
    )
  
  ggsave(paste0(output, "/euets_coverage_on_aggregate.png"), plot, width = 8, height = 6)
  
  

