#### HEADER -------

## This code generates histogram plot of value of sufficient statistic
# per euets firm, per yea

# sufficient statistic calculated assuming single elasticity of substitution
# across all inputs and firms
# and emissions = 0  for all non-euets firms

#####################

# Setup ------
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
library(Matrix)

# Import data ------

  load(paste0(proc_data,"/reallocation_term_of_targeted_interventions_by_year.RData"))

# Histogram plot of sufficient statistic as pct of total emissions

  hist_suff_stat_pct_total_emissions_list <- list()
  i <- 0
  
  for(y in 2005:2022){
    
    i <- i + 1
    
    df <- df_cov_list[[i]] %>% 
      select(vat, cov, total_emissions) %>% 
      mutate(pct_total_emissions = cov/total_emissions,
             log_pct_total_emissions = log(pct_total_emissions + 1)) %>% 
      filter(vat != "total", pct_total_emissions != 0) %>% 
      select(pct_total_emissions)
    
    binwidth <- 0.01
    
    max_value <- max(df$pct_total_emissions, na.rm = TRUE)
    min_value <- min(df$pct_total_emissions, na.rm = TRUE)
    
    if (max_value < 0.1) {
      breaks <- seq(0, 8, by = 0.02)  # Breaks for max_value less than 10
    } else {
      breaks <- seq(0, ceiling(max_value / 5) * 5, by = 5)  # Smallest multiple of 5 larger than max_value
    }
    
    # Create histogram
    plot <- ggplot(df, aes(x = pct_total_emissions)) +
      geom_histogram(data = df, 
                     aes(y = ..count..), 
                     binwidth = binwidth, 
                     fill = "blue",
                     alpha = 0.7) +  # Higher alpha for bars to the right of zero
      labs(title = "",
           x = "Change in agg. emissions due to reallocation (%)",
           y = "Frequency") +
      xlim(min_value, max_value) +
      scale_x_continuous(breaks = breaks) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),  # Remove gray grid lines
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),  # Remove border around the plot
            axis.text = element_text(size = 16),  # Increase axis tick label size
            axis.title = element_text(size = 16),
            axis.title.x = element_text(margin = margin(t = 12)),  # Increase distance from x-axis label
            axis.title.y = element_text(margin = margin(r = 12)))  # Increase axis title size
    
    hist_suff_stat_pct_total_emissions_list[[i]] <- plot
  }
  
  # save it
  save(hist_suff_stat_pct_total_emissions_list, file = paste0(output,"/histograms_for_suff_statistic_as_pct_total_emissions.RData"))
  
  
# Histogram plot of sufficient statistic as pct of EUETS sectors emissions

# Histogram plot of sufficient statistic as pct of EUETS firms emissions

# Bar plot of total value of sufficient statistic as pct of total, EUETS sectors, and EUETS firms emissions

  hist_total_suff_stat_list <- list()
  i <- 0
  
  for(y in 2005:2022){
    
    i <- i + 1
    
    df <- df_cov_list[[i]] %>% 
      filter(vat == "total") %>% 
      mutate(pct_total_emissions = cov/total_emissions,
             pct_euets_sectors = cov/euets_sectors_emissions,
             pct_euets_emissions = cov/euets_emissions) %>% 
      select(starts_with("pct"))
    
    df_long <- df %>%
      pivot_longer(cols = everything(), names_to = "Emissions", values_to = "Value") %>% 
      mutate(Emissions = case_when(
        Emissions == "pct_total_emissions" ~ "Agg. emissions",
        Emissions == "pct_euets_sectors" ~ "EUETS sectors",
        Emissions == "pct_euets_emissions" ~ "EUETS firms",
        TRUE ~ Emissions
      ))
    
    df_long$alpha <- ifelse(df_long$Emissions == "Agg. emissions", 0.3,
                            ifelse(df_long$Emissions == "EUETS sectors", 0.5, 0.7))
    
    df_long$Emissions <- factor(df_long$Emissions, levels = c("Agg. emissions", "EUETS sectors", "EUETS firms"))
    
    # Set the desired y-axis upper limit
    y_upper_limit <- max(df_long$Value) + 0.05 # Set an upper limit with a buffer
    
    # Create the plot
    plot <- ggplot(df_long, aes(x = Emissions, y = Value, fill = Emissions, alpha = alpha)) +
      geom_bar(stat = "identity", show.legend = FALSE) +  # Use the alpha mapping
      labs(title = "",
           x = "",
           y = "Change in emissions (%)") +
      scale_fill_manual(values = c("Agg. emissions" = "blue", 
                                   "EUETS sectors" = "blue", 
                                   "EUETS firms" = "blue")) +
      scale_alpha_continuous(range = c(0.3, 0.7)) +  # Set the alpha levels for the bars
      geom_text(aes(label =  round(Value, 2), vjust = -0.5, size = 5)) +  # Add rounded text labels on top of bars
      scale_y_continuous(limits = c(0, y_upper_limit)) +  # Set the y-axis limits
      theme_minimal() +
      theme(panel.grid.major = element_blank(),  # Remove gray grid lines
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),  # Remove border around the plot
            axis.text = element_text(size = 16),  # Increase axis tick label size
            axis.title = element_text(size = 16),
            axis.title.x = element_text(margin = margin(t = 12)),  # Increase distance from x-axis label
            axis.title.y = element_text(margin = margin(r = 12)),
            legend.position = "none")
    
    hist_total_suff_stat_list[[i]] <- plot
  }
  
  # save it
  save(hist_total_suff_stat_list, file = paste0(output,"/histograms_total_suff_statistic.RData"))
  