#### HEADER -------

## This code gnerates plot that compares EUETS vs non-EUETS firms
# wrt to
# 1. share of firms consuming fossil fuels
# 2. share of fossil fuel consumption out of total input cost

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

# Libraries ---------

library(dplyr)
library(ggplot2)

# Import data -------

  load(paste0(proc_data, "/amount_spent_on_fuel_by_firm_year.RData"))
  
  load(paste0(proc_data, "/firm_year_belgian_euets.RData"))
  
  load(paste0(proc_data, "/firm_year_domestic_input_cost.RData"))
  
  load(paste0(proc_data, "/firm_year_total_imports.RData"))
  
## 1. Harmonise VAT ID and build firm-year data ------------------------------

  fuel <- amount_spent_on_fuel_by_firm_year %>%
    rename(
      vat = vat_j_ano,
      fuel_spend = amount_spent_on_fuel_excl_euets_importers
    )
  
  euets_flag <- firm_year_belgian_euets %>%
    mutate(euets = "EUETS") %>% 
    select(vat, year, euets)
  
  domestic_costs <- firm_year_domestic_input_cost %>%
    select(vat, year, input_cost)
  
  imports <- firm_year_total_imports %>%
    rename(vat = vat_ano) %>% 
    select(vat, year, total_imports)
  
  firm_year <- fuel %>%
    left_join(domestic_costs, by = c("vat", "year")) %>%
    left_join(imports,       by = c("vat", "year")) %>%
    left_join(euets_flag,    by = c("vat", "year")) %>%
    mutate(
      # non-matched firms are non-EUETS
      euets = if_else(is.na(euets), "non-EUETS", euets),
      # total costs = domestic input costs + imports
      total_costs = input_cost + total_imports,
      # firm-year fuel share of costs (set to NA if total_costs is 0 or missing)
      fuel_share_costs = if_else(
        !is.na(total_costs) & total_costs > 0,
        fuel_spend / total_costs,
        NA_real_
      ),
      # indicator for positive fuel consumption
      fuel_positive = fuel_spend > 0
    )
  
## 2. Compute the two quantities by EUETS status -----------------------------
  
  # (1) Share of firm-years with positive fuel consumption
  share_positive <- firm_year %>%
    group_by(euets) %>%
    summarise(value = mean(fuel_positive, na.rm = TRUE), .groups = "drop") %>%
    mutate(metric = "Fuel consumption > 0")
  
  # (2) Average fuel share of total costs (firm-year level, then mean by group)
  share_costs <- firm_year %>%
    group_by(euets) %>%
    summarise(value = mean(fuel_share_costs, na.rm = TRUE), .groups = "drop") %>%
    mutate(metric = "Fuel consumption / input costs")
  
  plot_data <- bind_rows(share_positive, share_costs) %>%
    mutate(
      metric = factor(metric,
                      levels = c(
                        "Fuel consumption > 0",
                        "Spacing spacer",                 # invisible spacer to create distance
                        "Fuel consumption / input costs"
                      )
      ),
      euets = factor(euets, levels = c("EUETS", "non-EUETS"))
    )
  
  # Remove the spacer from plotting (empty row)
  plot_data <- plot_data %>% filter(metric != "Spacing spacer")
  
## 3. Plot: two regions on x-axis, 0–1 on y-axis -----------------------------
  
  fuel_plot <- ggplot(plot_data, aes(x = euets, y = value, fill = euets)) +
    geom_col(width = 0.6) +
    geom_text(
      aes(label = sprintf("%.2f", value)),
      vjust = -0.3,
      size = 3
    ) +
    facet_grid(~ metric, switch = "x") +
    scale_y_continuous(
      limits = c(0, 1.05),
      breaks = seq(0, 1, by = 0.2)
    ) +
    scale_fill_manual(
      values = c("EUETS" = "lightblue", "non-EUETS" = "grey70"),
      name   = NULL
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      
      # ----- REMOVE X-AXIS TEXT BELOW BARS -----
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      
      # ----- GRID LINES: HORIZONTAL ONLY -----
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
      panel.grid.minor.y = element_blank(),
      
      # ----- REMOVE GAP BETWEEN PANELS -----
      panel.spacing.x = unit(0, "lines"),
      
      # ----- REDUCE DISTANCE BETWEEN X-AXIS & FACET LABELS -----
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.x = element_text(margin = margin(t = 2, b = 2)),  # reduce space
      
      plot.background = element_rect(fill = "white", color = NA),
      
      axis.text.y = element_text(size = 11)
    )
  
# Save it --------------
ggsave(paste0(output, "/fuel_consumption_by_euets_status.png"), fuel_plot, width = 8, height = 5, dpi = 300)
  
  
  