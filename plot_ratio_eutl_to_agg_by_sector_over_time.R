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

# Create graph ------
  
  df <- euets_emissions_by_nace_using_installation %>%
    filter(year >= 2008, year <= 2022) %>% 
    mutate(emissions = emissions/10^3) %>% 
    left_join(agg_emissions_by_nace_year_using_shares_crf, by = c("nace", "year")) %>%
    mutate(ratio = emissions / agg_emissions) %>% 
    left_join(emissions_by_nace_annex_xii_24 %>% select(nace, year, ratio_crf), by = c("nace", "year"))
  
  # plot C24
  nace_to_plot <- "C24"
  
  df_plot <- df %>%
    filter(nace == nace_to_plot)
  
  max_emissions <- max(df_plot$emissions, na.rm = TRUE)
  
  ggplot(df_plot, aes(x = year)) +
    geom_line(aes(y = emissions, color = "EUETS", linetype = "EUETS"), size = 1) +
    geom_line(aes(y = agg_emissions, color = "Agg. Emissions", linetype = "Agg. Emissions"), size = 1) +
    geom_line(aes(y = ratio * max_emissions, color = "Ratio", linetype = "Ratio"), size = 1) +
    scale_y_continuous(
      name = "Emissions",
      sec.axis = sec_axis(~ . / max_emissions, name = "Ratio (EUETS / Aggregate)")
    ) +
    scale_color_manual(values = c(
      "EUETS" = "black",
      "Agg. Emissions" = "gray40",
      "Ratio" = "black"
    )) +
    scale_linetype_manual(values = c(
      "EUETS" = "solid",
      "Agg. Emissions" = "solid",
      "Ratio" = "dotted"
    )) +
    labs(x = NULL, color = NULL, linetype = NULL) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_blank(),
      axis.title.y.left = element_text(margin = margin(r = 10)),
      axis.title.y.right = element_text(margin = margin(l = 10))
    )
  
  # plot C24-C25
  nace_to_plot <- "C24-C25"
  
  df_plot <- df %>%
    filter(nace == nace_to_plot)
  
  max_emissions <- max(df_plot$emissions, na.rm = TRUE)
  
  ggplot(df_plot, aes(x = year)) +
    geom_line(aes(y = emissions, color = "EUETS", linetype = "EUETS"), size = 1) +
    geom_line(aes(y = agg_emissions, color = "Agg. Emissions", linetype = "Agg. Emissions"), size = 1) +
    geom_line(aes(y = ratio * max_emissions, color = "Ratio", linetype = "Ratio"), size = 1) +
    # Add dashed red horizontal line with legend
    geom_hline(
      data = df_plot %>% filter(!is.na(ratio_crf)),
      aes(yintercept = ratio_crf * max_emissions, color = "Ratio in Nat. Inv.", linetype = "Ratio in Nat. Inv."),
      size = 1
    ) +
    scale_y_continuous(
      name = "Emissions",
      sec.axis = sec_axis(~ . / max_emissions, name = "Ratio (EUETS / Aggregate)")
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
    labs(x = NULL, color = NULL, linetype = NULL) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_blank(),
      axis.title.y.left = element_text(margin = margin(r = 10)),
      axis.title.y.right = element_text(margin = margin(l = 10))
    )
  
  # plot C20-C21
  nace_to_plot <- "C20-C21"
  
  df_plot <- df %>%
    filter(nace == nace_to_plot)
  
  max_emissions <- max(df_plot$emissions, na.rm = TRUE)
  
  ggplot(df_plot, aes(x = year)) +
    geom_line(aes(y = emissions, color = "EUETS", linetype = "EUETS"), size = 1) +
    geom_line(aes(y = agg_emissions, color = "Agg. Emissions", linetype = "Agg. Emissions"), size = 1) +
    geom_line(aes(y = ratio * max_emissions, color = "Ratio", linetype = "Ratio"), size = 1) +
    # Add dashed red horizontal line with legend
    geom_hline(
      data = df_plot %>% filter(!is.na(ratio_crf)),
      aes(yintercept = ratio_crf * max_emissions, color = "Ratio in Nat. Inv.", linetype = "Ratio in Nat. Inv."),
      size = 1
    ) +
    scale_y_continuous(
      name = "Emissions",
      sec.axis = sec_axis(~ . / max_emissions, name = "Ratio (EUETS / Aggregate)")
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
    labs(x = NULL, color = NULL, linetype = NULL) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_blank(),
      axis.title.y.left = element_text(margin = margin(r = 10)),
      axis.title.y.right = element_text(margin = margin(l = 10))
    )
  
  # plot C17-C18
  nace_to_plot <- "C17-C18"
  
  df_plot <- df %>%
    filter(nace == nace_to_plot)
  
  max_emissions <- max(df_plot$emissions, na.rm = TRUE)
  
  ggplot(df_plot, aes(x = year)) +
    geom_line(aes(y = emissions, color = "EUETS", linetype = "EUETS"), size = 1) +
    geom_line(aes(y = agg_emissions, color = "Agg. Emissions", linetype = "Agg. Emissions"), size = 1) +
    geom_line(aes(y = ratio * max_emissions, color = "Ratio", linetype = "Ratio"), size = 1) +
    # Add dashed red horizontal line with legend
    geom_hline(
      data = df_plot %>% filter(!is.na(ratio_crf)),
      aes(yintercept = ratio_crf * max_emissions, color = "Ratio in Nat. Inv.", linetype = "Ratio in Nat. Inv."),
      size = 1
    ) +
    scale_y_continuous(
      name = "Emissions",
      sec.axis = sec_axis(~ . / max_emissions, name = "Ratio (EUETS / Aggregate)")
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
    labs(x = NULL, color = NULL, linetype = NULL) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_blank(),
      axis.title.y.left = element_text(margin = margin(r = 10)),
      axis.title.y.right = element_text(margin = margin(l = 10))
    )
  
  # plot D35
  nace_to_plot <- "D35"
  
  df_plot <- df %>%
    filter(nace == nace_to_plot)
  
  max_emissions <- max(df_plot$emissions, na.rm = TRUE)
  
  ggplot(df_plot, aes(x = year)) +
    geom_line(aes(y = emissions, color = "EUETS", linetype = "EUETS"), size = 1) +
    geom_line(aes(y = agg_emissions, color = "Agg. Emissions", linetype = "Agg. Emissions"), size = 1) +
    geom_line(aes(y = ratio * max_emissions, color = "Ratio", linetype = "Ratio"), size = 1) +
    # Add dashed red horizontal line with legend
    geom_hline(
      data = df_plot %>% filter(!is.na(ratio_crf)),
      aes(yintercept = ratio_crf * max_emissions, color = "Ratio in Nat. Inv.", linetype = "Ratio in Nat. Inv."),
      size = 1
    ) +
    scale_y_continuous(
      name = "Emissions",
      sec.axis = sec_axis(~ . / max_emissions, name = "Ratio (EUETS / Aggregate)")
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
    labs(x = NULL, color = NULL, linetype = NULL) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_blank(),
      axis.title.y.left = element_text(margin = margin(r = 10)),
      axis.title.y.right = element_text(margin = margin(l = 10))
    )
  
  # plot B
  nace_to_plot <- "B"
  
  df_plot <- df %>%
    filter(nace == nace_to_plot)
  
  max_emissions <- max(df_plot$emissions, na.rm = TRUE)
  
  ggplot(df_plot, aes(x = year)) +
    geom_line(aes(y = emissions, color = "EUETS", linetype = "EUETS"), size = 1) +
    geom_line(aes(y = agg_emissions, color = "Agg. Emissions", linetype = "Agg. Emissions"), size = 1) +
    geom_line(aes(y = ratio * max_emissions, color = "Ratio", linetype = "Ratio"), size = 1) +
    # Add dashed red horizontal line with legend
    geom_hline(
      data = df_plot %>% filter(!is.na(ratio_crf)),
      aes(yintercept = ratio_crf * max_emissions, color = "Ratio in Nat. Inv.", linetype = "Ratio in Nat. Inv."),
      size = 1
    ) +
    scale_y_continuous(
      name = "Emissions",
      sec.axis = sec_axis(~ . / max_emissions, name = "Ratio (EUETS / Aggregate)")
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
    labs(x = NULL, color = NULL, linetype = NULL) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_blank(),
      axis.title.y.left = element_text(margin = margin(r = 10)),
      axis.title.y.right = element_text(margin = margin(l = 10))
    )
  
  

