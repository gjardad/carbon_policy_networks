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

load(paste0(proc_data, "/dispersion_nace_belgium.RData"))
load(paste0(proc_data, "/dispersion_activity_belgium.RData"))

# Clean and reshape data -----

avg_dispersion_nace <- dispersion_nace %>%
  group_by(year) %>%
  summarise(across(starts_with("dispersion"), \(x) mean(x, na.rm = TRUE))) %>% 
  pivot_longer(cols = starts_with("dispersion"), names_to = "measure", values_to = "value") %>% 
  filter(year <= 2022)

avg_dispersion_activity <- dispersion_activity %>%
  group_by(year) %>%
  summarise(across(starts_with("dispersion"), \(x) mean(x, na.rm = TRUE))) %>% 
  pivot_longer(cols = starts_with("dispersion"), names_to = "measure", values_to = "value") %>% 
  filter(year <= 2022)

# Graph --------

library(ggplot2)

  # NACE graphs

  avg_dispersion_nace$measure <- factor(avg_dispersion_nace$measure, 
                         levels = c("dispersion_shortage", "dispersion_emissions", "dispersion_capital", "dispersion_labor"))

  # plot all four measures for NACE
  plot1 <- ggplot(avg_dispersion_nace, aes(x = year, y = value, color = measure, linetype = measure)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = c("dispersion_emissions" = "#444444", 
                                  "dispersion_capital" = "#E69F00", 
                                  "dispersion_labor" = "#0072B2",
                                  "dispersion_shortage" = "#C0362C"),
                       labels = c("dispersion_emissions" = expression(CO[2]), 
                                  "dispersion_capital" = "Capital", 
                                  "dispersion_labor" = "Labor",
                                  "dispersion_shortage" = "Allowance shortage")) +
    scale_linetype_manual(values = c("dispersion_emissions" = "solid", 
                                     "dispersion_capital" = "solid", 
                                     "dispersion_labor" = "solid",
                                     "dispersion_shortage" = "solid"),
                          labels = c("dispersion_emissions" = expression(CO[2]), 
                                     "dispersion_capital" = "Capital", 
                                     "dispersion_labor" = "Labor",
                                     "dispersion_shortage" = "Allowance shortage")) +
    labs(title = "",
         x = "",
         y = "Average difference in log points 90th - 10th percentile",
         color = NULL,  # Remove the title of the color legend
         linetype = NULL) +  # Remove the title of the linetype legend
    theme_minimal() +
    theme(
      legend.position = c(0.3, 0.9),  # Position the legend inside the plot
      legend.background = element_rect(fill = "white", color = "black"),  # White box with black lines
      legend.box.margin = margin(6, 6, 6, 6),  # Add some padding inside the legend box
      axis.title.y = element_text(margin = margin(r = 15))  # Increase the distance between y title and y axis
    )  +
    guides(
      color = guide_legend(order = 1),  # Ensure the color guide respects the order
      linetype = guide_legend(order = 1)  # Ensure the linetype guide respects the order
    ) +
    expand_limits(y = 0)
  
  # plot dispersion in emissions, labor, capital for NACE
  avg_dispersion_nace_no_shortage <- avg_dispersion_nace %>% 
    filter(measure != "dispersion_shortage")
  
  plot2 <- ggplot(avg_dispersion_nace_no_shortage, aes(x = year, y = value, color = measure, linetype = measure)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = c("dispersion_emissions" = "#444444", 
                                  "dispersion_capital" = "#E69F00", 
                                  "dispersion_labor" = "#0072B2"),
                       labels = c("dispersion_emissions" = expression(CO[2]), 
                                  "dispersion_capital" = "Capital", 
                                  "dispersion_labor" = "Labor")) +
    scale_linetype_manual(values = c("dispersion_emissions" = "solid", 
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
      legend.position = c(0.3, 0.9),  # Position the legend inside the plot
      legend.background = element_rect(fill = "white", color = "black"),  # White box with black lines
      legend.box.margin = margin(6, 6, 6, 6),  # Add some padding inside the legend box
      axis.title.y = element_text(margin = margin(r = 15))  # Increase the distance between y title and y axis
    )  +
    guides(
      color = guide_legend(order = 1),  # Ensure the color guide respects the order
      linetype = guide_legend(order = 1)  # Ensure the linetype guide respects the order
    ) +
    expand_limits(y = 0)
  
  # Activity graphs
  
  avg_dispersion_activity$measure <- factor(avg_dispersion_activity$measure, 
                                        levels = c("dispersion_shortage", "dispersion_emissions", "dispersion_capital", "dispersion_labor"))
  
  # plot all four measures for activity
  plot3 <- ggplot(avg_dispersion_activity, aes(x = year, y = value, color = measure, linetype = measure)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = c("dispersion_emissions" = "#444444", 
                                  "dispersion_capital" = "#E69F00", 
                                  "dispersion_labor" = "#0072B2",
                                  "dispersion_shortage" = "#C0362C"),
                       labels = c("dispersion_emissions" = expression(CO[2]), 
                                  "dispersion_capital" = "Capital", 
                                  "dispersion_labor" = "Labor",
                                  "dispersion_shortage" = "Allowance shortage")) +
    scale_linetype_manual(values = c("dispersion_emissions" = "solid", 
                                     "dispersion_capital" = "solid", 
                                     "dispersion_labor" = "solid",
                                     "dispersion_shortage" = "solid"),
                          labels = c("dispersion_emissions" = expression(CO[2]), 
                                     "dispersion_capital" = "Capital", 
                                     "dispersion_labor" = "Labor",
                                     "dispersion_shortage" = "Allowance shortage")) +
    labs(title = "",
         x = "",
         y = "Average difference in log points 90th - 10th percentile",
         color = NULL,  # Remove the title of the color legend
         linetype = NULL) +  # Remove the title of the linetype legend
    theme_minimal() +
    theme(
      legend.position = c(0.3, 0.9),  # Position the legend inside the plot
      legend.background = element_rect(fill = "white", color = "black"),  # White box with black lines
      legend.box.margin = margin(6, 6, 6, 6),  # Add some padding inside the legend box
      axis.title.y = element_text(margin = margin(r = 15))  # Increase the distance between y title and y axis
    )  +
    guides(
      color = guide_legend(order = 1),  # Ensure the color guide respects the order
      linetype = guide_legend(order = 1)  # Ensure the linetype guide respects the order
    ) +
    expand_limits(y = 0)
  
  # plot dispersion in emissions, labor, capital for activity
  avg_dispersion_activity_no_shortage <- avg_dispersion_activity %>% 
    filter(measure != "dispersion_shortage")
  
  plot4 <- ggplot(avg_dispersion_activity_no_shortage, aes(x = year, y = value, color = measure, linetype = measure)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = c("dispersion_emissions" = "#444444", 
                                  "dispersion_capital" = "#E69F00", 
                                  "dispersion_labor" = "#0072B2"),
                       labels = c("dispersion_emissions" = expression(CO[2]), 
                                  "dispersion_capital" = "Capital", 
                                  "dispersion_labor" = "Labor")) +
    scale_linetype_manual(values = c("dispersion_emissions" = "solid", 
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
      legend.position = c(0.3, 0.9),  # Position the legend inside the plot
      legend.background = element_rect(fill = "white", color = "black"),  # White box with black lines
      legend.box.margin = margin(6, 6, 6, 6),  # Add some padding inside the legend box
      axis.title.y = element_text(margin = margin(r = 15))  # Increase the distance between y title and y axis
    )  +
    guides(
      color = guide_legend(order = 1),  # Ensure the color guide respects the order
      linetype = guide_legend(order = 1)  # Ensure the linetype guide respects the order
    ) +
    expand_limits(y = 0)

# Save it --------
ggsave(filename = paste0(output,"/belgium_euets_nace_dispersion.png"), plot = plot2, width = 8, height = 6, dpi = 300)
ggsave(filename = paste0(output,"/belgium_euets_activity_dispersion.png.png"), plot = plot4, width = 8, height = 6, dpi = 300)


