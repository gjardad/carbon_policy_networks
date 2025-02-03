#### HEADER -------

## Calculate covariance matrix of input intensity at the
# leve of the sector-input pair in the year 2005.

# That is, for each sector calculate the covariance of the input
# intensity of any given pair of inputs across firms, and then average
# this number across sectors.

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

load(paste0(proc_data,"/firm_year_nominal_domestic_input_bundle_2005.RData"))

load(paste0(proc_data,"/firm_year_belgian_euets.RData"))

# Within-sector covariance of electricity and all others dirty inputs ------

firm_year_nominal_domestic_input_bundle_2005 <- firm_year_nominal_domestic_input_bundle_2005 %>% 
    ungroup() %>% 
    select(-expenditure_share, -year, -output) %>% 
    group_by(vat_j_ano) %>% 
    mutate(total_expenditure = sum(expenditure, na.rm = TRUE),
           expenditure_share = expenditure/total_expenditure) %>% 
    ungroup()
  
  # expenditure share on 35.1 (electricity)
  electricity <- firm_year_nominal_domestic_input_bundle_2005 %>%
    filter(substr(nace5d_supplier,1,3) == "351") %>%
    group_by(vat_j_ano) %>% 
    summarise(exp_share_electricity = sum(expenditure_share, na.rm = TRUE)) %>% 
    ungroup()
  
  # expenditure share on dirty inputs
  pollutant_inputs <- as.data.frame(c(unique(firm_year_belgian_euets$nace5d))) %>% 
    rename(nace5d = 1) %>% 
    mutate(nace2d = substr(nace5d, 1, 2)) %>% 
    filter(!is.na(nace5d))
  
  dirty_inputs <- firm_year_nominal_domestic_input_bundle_2005 %>%
    filter(nace5d_supplier %in% pollutant_inputs$nace5d) %>%
    group_by(vat_j_ano) %>% 
    summarise(exp_share_dirty = sum(expenditure_share, na.rm = TRUE)) %>% 
    ungroup()
  
  firm_year_nominal_domestic_input_bundle_2005 <- firm_year_nominal_domestic_input_bundle_2005 %>% 
    left_join(electricity, by = "vat_j_ano") %>% 
    left_join(dirty_inputs, by = "vat_j_ano") %>%
    mutate(exp_share_dirty = ifelse(is.na(exp_share_dirty), 0, exp_share_dirty)) %>% 
    mutate(exp_share_electricity = ifelse(is.na(exp_share_electricity), 0, exp_share_electricity))
  
  df_cov <- firm_year_nominal_domestic_input_bundle_2005 %>% 
    select(nace5d_buyer, exp_share_electricity, exp_share_dirty) %>% 
    mutate(nace4d_buyer = substr(nace5d_buyer, 1, 4),
           nace3d_buyer = substr(nace5d_buyer, 1, 3))
  
  # Calculate covariance within nace5d_buyer
  covariance_nace5d <- df_cov %>%
    group_by(nace5d_buyer) %>%  # Group by nace5d_buyer
    summarise(covariance = cov(exp_share_electricity, exp_share_dirty, use = "pairwise.complete.obs"),
              sd_electricity = sd(exp_share_electricity, na.rm = TRUE),
              sd_dirty = sd(exp_share_dirty, na.rm = TRUE),
              correlation = covariance / (sd_electricity * sd_dirty),
              .groups = "drop")  # Ungroup after summarisation
  
  # number of nace5d sectors and number of firms per sector
  summary_df <- df_cov %>%
    group_by(nace5d_buyer) %>%
    summarise(count = n()) %>%
    ungroup()
  
  # Number of different values of nace_5dbuyer
  num_different_values <- n_distinct(df_cov$nace5d_buyer)
  
  # Avg number of firms per sector
  avg_num_firms_by_sector <- mean(summary_df$count, na.rm = T)
  med_num_firms_by_sector <- median(summary_df$count, na.rm = T)
  
  # Calculate covariance within nace4d_buyer
  covariance_nace4d <- df_cov %>%
    group_by(nace4d_buyer) %>%  # Group by nace4d_buyer
    summarise(covariance = cov(exp_share_electricity, exp_share_dirty, use = "pairwise.complete.obs"),
              sd_electricity = sd(exp_share_electricity, na.rm = TRUE),
              sd_dirty = sd(exp_share_dirty, na.rm = TRUE),
              correlation = covariance / (sd_electricity * sd_dirty),
              .groups = "drop")  # Ungroup after summarisation
  
  # Calculate covariance within nace3d_buyer
  covariance_nace3d <- df_cov %>%
    group_by(nace3d_buyer) %>%  # Group by nace4d_buyer
    summarise(covariance = cov(exp_share_electricity, exp_share_dirty, use = "pairwise.complete.obs"),
              sd_electricity = sd(exp_share_electricity, na.rm = TRUE),
              sd_dirty = sd(exp_share_dirty, na.rm = TRUE),
              correlation = covariance / (sd_electricity * sd_dirty),
              .groups = "drop")  # Ungroup after summarisation

# Graph covariance of electricty and dirty inputs across sectors ------

  library(ggplot2)
  
  plot_cov_nace3d <- ggplot(covariance_nace3d, aes(x = covariance)) +
    geom_histogram(binwidth = 0.001, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Histogram of Covariance Values Across Sectors",
         x = "Covariance",
         y = "Frequency") +
    theme_minimal()
  
  df_corr_3d <- covariance_nace3d %>% filter(!is.na(correlation))
  
  plot_corr_nace3d <- ggplot(df_corr_3d, aes(x = correlation)) +
    geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Histogram of Covariance Values Across Sectors",
         x = "Covariance",
         y = "Frequency") +
    theme_minimal()
  
  plot_cov_nace4d <- ggplot(covariance_nace4d, aes(x = covariance)) +
    geom_histogram(binwidth = 0.001, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Histogram of Covariance Values Across Sectors",
         x = "Covariance",
         y = "Frequency") +
    theme_minimal()
  
  df_corr_4d <- covariance_nace4d %>% filter(!is.na(correlation))
  
  plot_corr_nace4d <- ggplot(df_corr_4d, aes(x = correlation)) +
    geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Histogram of Covariance Values Across Sectors",
         x = "Covariance",
         y = "Frequency") +
    theme_minimal()
  
  plot_cov_nace5d <- ggplot(covariance_nace5d, aes(x = covariance)) +
    geom_histogram(binwidth = 0.001, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Histogram of Covariance Values Across Sectors",
         x = "Covariance",
         y = "Frequency") +
    theme_minimal()
  
  df_corr_5d <- covariance_nace5d %>% filter(!is.na(correlation))
  
  plot_corr_nace5d <- ggplot(df_corr_5d, aes(x = correlation)) +
    geom_histogram(data = subset(df_corr_5d, correlation < 0), 
                   aes(y = ..count..), 
                   binwidth = 0.05, 
                   fill = "blue", 
                   color = "black", 
                   alpha = 0.1) +  # Lower alpha for bars to the left of zero
    geom_histogram(data = subset(df_corr_5d, correlation >= 0), 
                   aes(y = ..count..), 
                   binwidth = 0.05, 
                   fill = "blue", 
                   color = "black", 
                   alpha = 0.7) +  # Higher alpha for bars to the right of zero
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),  # Remove gray grid lines
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "black", fill = NA)) +  # Optional border around the plot
    geom_vline(xintercept = 0, linetype = "dotted", color = "black")

  
  binwidth <- 0.05
  
  max_neg_xlim <- (floor((0 - (binwidth / 2)) / binwidth) * binwidth) + (binwidth / 2)
  
  median_value <- median(df_corr_5d$correlation, na.rm = TRUE)
  mean_value <- mean(df_corr_5d$correlation, na.rm = TRUE)
  
  # Create histogram with specified colors based on the threshold
  plot_corr_nace5d <- ggplot(df_corr_5d, aes(x = correlation)) +
    geom_histogram(data = subset(df_corr_5d, correlation < max_neg_xlim), 
                   aes(y = ..count..), 
                   binwidth = binwidth, 
                   fill = "blue",
                   alpha = 0.1) +  # Lower alpha for bars to the left of the threshold
    geom_histogram(data = subset(df_corr_5d, correlation > max_neg_xlim), 
                   aes(y = ..count..), 
                   binwidth = binwidth, 
                   fill = "blue",
                   alpha = 0.7) +  # Higher alpha for bars to the right of zero
    labs(title = "",
         x = "Correlation",
         y = "Frequency") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),  # Remove gray grid lines
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),  # Remove border around the plot
          axis.text = element_text(size = 16),  # Increase axis tick label size
          axis.title = element_text(size = 16),
          axis.title.x = element_text(margin = margin(t = 12)),  # Increase distance from x-axis label
          axis.title.y = element_text(margin = margin(r = 12))) +  # Increase axis title size
    
    # Add vertical line at the median
    geom_vline(xintercept = median(df_corr_5d$correlation, na.rm = TRUE), 
               linetype = "dotted", color = "black", size = 1.2) +
    
    # Add text label for the median
    annotate("text", 
             x = median_value + 0.25, 
             y =  64,  # Position above the highest bar
             label = paste("med =", round(median_value, 2)), 
             color = "black", 
             size = 5, 
             vjust = -0.5) + # Adjust the vertical position
  
  # Add text label for the median
  annotate("text", 
           x = median_value + 0.25, 
           y =  58,  # Position above the highest bar
           label = paste("avg =", round(mean_value, 2)), 
           color = "black", 
           size = 5, 
           vjust = -0.5)  # Adjust the vertical position
  
  file_name <- paste0(output, "/histogram_corr_energy_dirty_inputs_nace5d", Sys.Date(), ".png")
  ggsave(filename = file_name, plot = plot_corr_nace5d, width = 8, height = 6)
