#### HEADER -------

## This code generates plots that show heterogeneity in prediction errors and
# relationship between emissions and revenue in two groups:
# firms in size bin 20 vs firms in size bins 1-19

#####################

## Setup ------
rm(list = ls())

if (tolower(Sys.info()[["user"]]) == "jardang") {
  folder <- "X:/Documents/JARDANG"
}

raw_data  <- paste0(folder, "/carbon_policy_networks/data/raw")
int_data  <- paste0(folder, "/carbon_policy_networks/data/intermediate")
proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")
output    <- paste0(folder, "/carbon_policy_networks/output")
code      <- paste0(folder, "/carbon_policy_networks/code")

## Libraries -------

library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)

## Import data ------

load(paste0(int_data, "/results_from_loocv_model_nace_year_revenue_purchases_from_fuel_importers.RData"))

## Create firm-size bins and absolute errors ---------

# Define top-1% cutoff in revenue (for "big firms") if needed later
cutoff <- quantile(df_lofo$revenue, 0.99, na.rm = TRUE)

df_vis <- df_lofo %>%
  mutate(
    # 20 bins of firm size by revenue
    size_bin = ntile(revenue, 20),
    
    # fuel-only model using fuel excl EUETS importers
    abs_err_fuel_excl_euets_importers =
      abs(pred_log_fuel_excl_euets_importers - emissions),
    
    # revenue + fuel excl EUETS importers
    abs_err_rev_fuel_excl_euets_importers =
      abs(pred_log_rev_fuel_excl_euets_importers - emissions)
  )

## Plot 1: Compare absolute errors (model fuel_exl_EUETS_importers-only vs rev+excl_EUETS_importers) ----------------------

df_compare <- df_vis %>%
  pivot_longer(
    cols = c(abs_err_fuel_excl_euets_importers,
             abs_err_rev_fuel_excl_euets_importers),
    names_to = "model",
    values_to = "abs_error"
  ) %>% 
  mutate(abs_error_kton = abs_error/1e3)

# dummy data just to generate the legend squares (must match names in df_compare$model)
legend_df <- data.frame(
  model = c("abs_err_fuel_excl_euets_importers",
            "abs_err_rev_fuel_excl_euets_importers"),
  x = 1,
  y = 0
)

p_compare <- ggplot(df_compare, aes(x = factor(size_bin), y = abs_error_kton)) +
  
  # jittered points (NO legend)
  geom_jitter(
    aes(color = model),
    width = 0.25,
    alpha = 0.90,
    size  = 0.8,
    show.legend = FALSE
  ) +
  
  # boxplots (NO legend)
  geom_boxplot(
    aes(fill = model),
    alpha = 0.6,
    outlier.shape = NA,
    show.legend = FALSE
  ) +
  
  # invisible points ONLY used to create the legend
  geom_point(
    data = legend_df,
    aes(x = x, y = y, color = model, fill = model),
    shape = 22,         # filled square
    size  = 5,
    alpha = 0,          # invisible on the plot
    inherit.aes = FALSE,
    show.legend = TRUE
  ) +
  
  labs(
    title = "",
    x = "Firm size bin (based on revenue)",
    y = "Absolute error (ktons CO2)"
  ) +
  
  scale_fill_manual(
    values = c(
      "abs_err_fuel_excl_euets_importers"     = "#e57373",  # red
      "abs_err_rev_fuel_excl_euets_importers" = "#64b5f6"   # blue
    )
  ) +
  scale_color_manual(
    values = c(
      "abs_err_fuel_excl_euets_importers"     = "#e57373",
      "abs_err_rev_fuel_excl_euets_importers" = "#64b5f6"
    ),
    labels = c(
      "abs_err_fuel_excl_euets_importers"     = "Model with only fuel consumption",
      "abs_err_rev_fuel_excl_euets_importers" = "Model with revenue and fuel consumption"
    )
  ) +
  
  guides(
    color = guide_legend(
      title = NULL,
      override.aes = list(
        alpha = 1,                      # make legend squares visible
        shape = 22,                     # square
        size  = 5,
        fill  = c("#e57373", "#64b5f6"),
        color = c("#e57373", "#64b5f6")
      )
    ),
    fill = "none"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title    = element_blank(),
    legend.text = element_text(size = 16),
    axis.title.x = element_text(size = 18, margin = margin(t = 14)),
    axis.title.y = element_text(size = 18, margin = margin(r = 14)),
    axis.text    = element_text(size = 16)
  )

print(p_compare)
ggsave(paste0(output, "/abs_errors_loocv_in_models_with_and_wout_revenue.png"), p_compare, width = 6, height = 5, dpi = 300)

## ------------------------------------------------------------------
## Regressions: log(emissions) ~ log(revenue)
##  - bins 1–19 combined
##  - bin 20 only
## ------------------------------------------------------------------

# ensure we have size_bin in this object
df_bins <- df_vis

mod_small <- df_bins %>%
  filter(size_bin < 20) %>%
  lm(log(emissions) ~ log(revenue), data = .)

mod_large <- df_bins %>%
  filter(size_bin == 20) %>%
  lm(log(emissions) ~ log(revenue), data = .)

# Tidy coefficients and R^2 into a single table
coef_table <- bind_rows(
  glance(mod_small) %>%
    mutate(group = "Bins 1–19") %>%
    select(group, r.squared, adj.r.squared),
  glance(mod_large) %>%
    mutate(group = "Bin 20") %>%
    select(group, r.squared, adj.r.squared)
) %>%
  left_join(
    bind_rows(
      tidy(mod_small) %>% mutate(group = "Bins 1–19"),
      tidy(mod_large) %>% mutate(group = "Bin 20")
    ) %>%
      select(group, term, estimate, std.error),
    by = "group"
  ) %>%
  mutate(
    estimate      = round(estimate, 3),
    std.error     = round(std.error, 3),
    r.squared     = round(r.squared, 3),
    adj.r.squared = round(adj.r.squared, 3)
  )

coef_table

## ------------------------------------------------------------------
## Scatter plots on log(emissions) vs log(revenue)
##  - bins 1–19 combined
##  - bin 20 only
## ------------------------------------------------------------------


df_scatter_plot<- df_lofo %>%
  mutate(size_bin = ntile(revenue, 20))

p_small <- df_scatter_plot %>%
  filter(size_bin < 20) %>%
  ggplot(aes(x = revenue, y = emissions)) +
  geom_point(alpha = 0.4, color = "#64b5f6", size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#0d47a1", linewidth = 1) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "",
    x = "Revenue (log scale)",
    y = "Emissions (log scale)"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 18, margin = margin(t = 14)),
    axis.title.y = element_text(size = 18, margin = margin(r = 14)),
    axis.text    = element_text(size = 16)
  )

p_large <- df_scatter_plot %>%
  filter(size_bin == 20) %>%
  ggplot(aes(x = revenue, y = emissions)) +
  geom_point(alpha = 0.6, color = "#e57373", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "#b71c1c", linewidth = 1) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "",
    x = "Revenue (log scale)",
    y = "Emissions (log scale)"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 18, margin = margin(t = 14)),
    axis.title.y = element_text(size = 18, margin = margin(r = 14)),
    axis.text    = element_text(size = 16)
  )

ggsave(paste0(output, "/revenue_vs_emissions_bins_1_to_19.png"), p_small, width = 6, height = 5, dpi = 300)
ggsave(paste0(output, "/revenue_vs_emissions_bin_20.png"), p_large, width = 6, height = 5, dpi = 300)


