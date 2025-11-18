#### HEADER -------

## This code creates df with imported share by fuel-year in Belgium

#####################

# Setup ------
rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
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

# Import and clean data --------

  load(paste0(proc_data,"/energy_balance_by_fuel_year.RData"))

  energy_balance_wide <- energy_balance %>%
    mutate(
      # rename flow categories
      flow = case_when(
        str_starts(flow, "EXP:")       ~ "X",
        str_starts(flow, "IMP:")       ~ "I",
        str_starts(flow, "GIC:")       ~ "gic",
        str_starts(flow, "FC_IND_E:")  ~ "fc_ind_e",
        str_starts(flow, "TI_E:")      ~ "ti_e",
        TRUE                           ~ flow
      )
    ) %>%
    select(siec_code, year, flow, energy_balance_value) %>%
    
    # make it wide by siec_code × year
    pivot_wider(
      names_from  = flow,
      values_from = energy_balance_value
    ) %>%
    
    # compute domestic absorption share = (imports - exports) / gic
    mutate(
      absorption = I - X,  # domestic absorption = net imports
      absorption_share = if_else(
        !is.na(gic) & gic != 0,
        absorption / gic,  # how close net imports are to true consumption
        NA_real_
      )
    ) %>%
  filter(year >= 2005)
  
# Compute summary statistics --------

  summary_absorption <- energy_balance_wide %>%
    group_by(siec_code) %>%
    summarise(
      n_years       = n(),
      n_valid       = sum(!is.na(absorption_share)),
      
      mean_absorption_share = {
        x <- mean(absorption_share, na.rm = TRUE)
        if (is.nan(x)) NA_real_ else x
      },
      median_absorption_share = {
        x <- median(absorption_share, na.rm = TRUE)
        if (is.nan(x)) NA_real_ else x
      },
      sd_absorption_share = {
        x <- sd(absorption_share, na.rm = TRUE)
        if (is.nan(x)) NA_real_ else x
      },
      min_absorption_share = {
        x <- min(absorption_share, na.rm = TRUE)
        if (is.infinite(x)) NA_real_ else x
      },
      max_absorption_share = {
        x <- max(absorption_share, na.rm = TRUE)
        if (is.infinite(x)) NA_real_ else x
      },
      
      # share of years where proxy is clearly "too low" or "too high"
      share_years_below_0_5 = ifelse(
        n_valid > 0,
        sum(absorption_share < 0.5, na.rm = TRUE) / n_valid,
        NA_real_
      ),
      share_years_above_1_2 = ifelse(
        n_valid > 0,
        sum(absorption_share > 1.2, na.rm = TRUE) / n_valid,
        NA_real_
      ),
      
      # GIC levels (for weighting / relevance)
      gic_total = sum(gic, na.rm = TRUE),
      gic_mean  = {
        x <- mean(gic, na.rm = TRUE)
        if (is.nan(x)) NA_real_ else x
      },
      
      # GIC-weighted average absorption_share
      weighted_absorption_share = {
        x <- absorption_share
        w <- gic
        ok <- !is.na(x) & !is.na(w) & w > 0
        if (!any(ok)) {
          NA_real_
        } else {
          sum(x[ok] * w[ok]) / sum(w[ok])
        }
      },
      
      .groups = "drop"
    )
  
  # Inspect summary
  print(summary_absorption)
  
# Summary statistics for absorption share across time ------
  
  
  
# Generate graphs --------
  
  # Drop rows with no absorption_share for plotting
  eb_plot_data <- energy_balance_wide %>%
    filter(!is.na(absorption_share))
  
  
  ### Plot 1: Time series of absorption_share by fuel (faceted)
  p1 <- ggplot(eb_plot_data,
               aes(x = year, y = absorption_share, group = siec_code)) +
    geom_line() +
    facet_wrap(~ siec_code, scales = "free_y") +
    geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
    labs(
      title = "Domestic absorption as proxy for consumption by fuel",
      subtitle = "absorption_share = (Imports - Exports) / GIC",
      x = "Year",
      y = "Absorption share"
    ) +
    theme_minimal()
  
   print(p1)
  
  
  ### Plot 2: Distribution of absorption_share by fuel (boxplot)
  p2 <- eb_plot_data %>%
    ggplot(aes(
      x = fct_reorder(siec_code, absorption_share, .fun = median, na.rm = TRUE),
      y = absorption_share
    )) +
    geom_boxplot(outlier.alpha = 0.4) +
    coord_flip() +
    geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
    labs(
      title = "Distribution of absorption_share by fuel",
      subtitle = "(I - X) / GIC by SIEC code over time",
      x = "SIEC code",
      y = "Absorption share"
    ) +
    theme_minimal()
  
  # print(p2)
  
  
  ### Plot 3: Scatterplot of absorption vs GIC
  # If points lie close to the 45° line, absorption is a good proxy for consumption.
  p3 <- energy_balance_wide %>%
    filter(!is.na(absorption), !is.na(gic), gic > 0) %>%
    ggplot(aes(x = absorption, y = gic, color = siec_code)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Domestic absorption vs Gross inland consumption",
      subtitle = "Each point is a fuel-year; dashed line is equality",
      x = "Absorption = Imports - Exports (TJ)",
      y = "Gross inland consumption (TJ)",
      color = "SIEC code"
    ) +
    theme_minimal()
  
  # print(p3)
  
  
  ### Plot 4: Mean absorption_share by fuel (headline picture)
  p4 <- summary_absorption %>%
    filter(!is.na(mean_absorption_share)) %>%
    ggplot(aes(
      x = fct_reorder(siec_code, mean_absorption_share),
      y = mean_absorption_share
    )) +
    geom_col() +
    coord_flip() +
    geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
    labs(
      title = "Average quality of domestic absorption as proxy for consumption",
      subtitle = "mean((I - X) / GIC) by fuel, Belgium",
      x = "SIEC code",
      y = "Mean absorption_share"
    ) +
    theme_minimal()
  
  # print(p4)
  
  
  
