#### HEADER -------

## This code creates graphs that shows measures of dispersion over time

# 1. emissions productivity (inverse of emission intensity);
# 2. labor productivity
# 3. capital productivity

# for all firms in Belgium treated by the EUETS

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
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately

# Import data ------

load(paste0(proc_data, "/dispersion_belgium.RData"))

# Clean and reshape data -----

df_avg <- dispersion %>%
  group_by(year) %>%
  summarise(across(starts_with("dispersion"), \(x) mean(x, na.rm = TRUE))) %>% 
  pivot_longer(cols = starts_with("dispersion"), names_to = "measure", values_to = "value") %>% 
  filter(year <= 2022)

# Graph --------

library(ggplot2)

plot1 <- ggplot(df_avg, aes(x = year, y = value, color = measure, linetype = measure)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("dispersion_emissions" = "black", 
                                "dispersion_capital" = "gray", 
                                "dispersion_labor" = "black"),
                     labels = c("dispersion_emissions" = expression(CO[2]), 
                                "dispersion_capital" = "Capital", 
                                "dispersion_labor" = "Labor")) +
  scale_linetype_manual(values = c("dispersion_emissions" = "dashed", 
                                   "dispersion_capital" = "solid", 
                                   "dispersion_labor" = "solid"),
                        labels = c("dispersion_emissions" = expression(CO[2]), 
                                   "dispersion_capital" = "Capital", 
                                   "dispersion_labor" = "Labor")) +
  labs(title = "",
       x = "",
       y = "Average difference in log points 90th - 10th percentile",
       color = NULL,  # Remove the title of the color legend
       linetype = NULL) +  # Remove the title of the linetype legend
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.4),  # Position the legend inside the plot
    legend.background = element_rect(fill = "white", color = "black"),  # White box with black lines
    legend.box.margin = margin(6, 6, 6, 6),  # Add some padding inside the legend box
    axis.title.y = element_text(margin = margin(r = 15))  # Increase the distance between y title and y axis
  )  +
  expand_limits(y = 0)

# Save it --------
ggsave(filename = paste0(output,"/belgium_euets_dispersion.png"), plot = plot1, width = 8, height = 6, dpi = 300)


