#### HEADER -------

## This code generates plots that help understand why LOOCV with
## revenues + fuel_from_non_EUETS vs fuel_from_non_EUETS-only
## performs differently across metrics.

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

## Import data ------

  load(paste0(int_data, "/results_from_loocv_model_nace_year_revenue_purchases_from_fuel_importers.RData"))

## Define top-1% cutoff in revenue (for "big firms") ----
cutoff <- quantile(df_lofo$revenue, 0.99, na.rm = TRUE)

## 1. Absolute errors across firm size bins -----------------------

df_vis <- df_lofo %>%
  mutate(
    # fuel-only model using fuel from non-EUETS importers
    abs_err_fuel_non_euets      = abs(pred_log_fuel_non_euets - emissions),
    # revenue + fuel_non_euets model
    abs_err_rev_fuel_non_euets  = abs(pred_log_rev_fuel_non_euets - emissions),
    size_bin = ntile(revenue, 20)  # 20 bins of firm size
  )

# Boxplot: absolute error for revenue + fuel_non_EUETS model
ggplot(df_vis, aes(x = factor(size_bin), y = abs_err_rev_fuel_non_euets)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Absolute prediction errors across firm size\n(revenue + fuel from non-EUETS importers)",
    x = "Firm size bin (based on revenue)",
    y = "Absolute error (tons CO₂)"
  ) +
  theme_minimal()

# Boxplot: compare fuel_non_euets-only vs revenue + fuel_non-euets
df_compare <- df_vis %>%
  tidyr::pivot_longer(
    cols = c(abs_err_fuel_non_euets, abs_err_rev_fuel_non_euets),
    names_to = "model",
    values_to = "abs_error"
  )

# df_compare already created earlier:
# df_compare has columns: size_bin, abs_error, model
# where model ∈ { "abs_err_fuel_non_euets", "abs_err_rev_fuel_non_euets" }

# dummy data just to generate the legend squares
legend_df <- data.frame(
  model = c("abs_err_fuel_non_euets", "abs_err_rev_fuel_non_euets"),
  x = 1,
  y = 0
)

ggplot(df_compare, aes(x = factor(size_bin), y = abs_error)) +
  
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
    y = "Absolute error (tons CO2)"
  ) +
  
  scale_fill_manual(
    values = c(
      "abs_err_fuel_non_euets"     = "#e57373",  # red
      "abs_err_rev_fuel_non_euets" = "#64b5f6"   # blue
    )
  ) +
  scale_color_manual(
    values = c(
      "abs_err_fuel_non_euets"     = "#e57373",
      "abs_err_rev_fuel_non_euets" = "#64b5f6"
    ),
    labels = c(
      "abs_err_fuel_non_euets"     = "Model with only fuel consumption",
      "abs_err_rev_fuel_non_euets" = "Model with revenue and fuel consumption"
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
    legend.position = "top",
    legend.title    = element_blank()
  )



## 2. Log residuals vs firm size ----------------------------------

df_vis2 <- df_lofo %>%
  mutate(
    log_resid_fuel_non_euets     = log(pred_log_fuel_non_euets)     - log(emissions),
    log_resid_rev_fuel_non_euets = log(pred_log_rev_fuel_non_euets) - log(emissions)
  )

# Single-model plot: revenue + fuel_non_euets
ggplot(df_vis2, aes(x = log(revenue), y = log_resid_rev_fuel_non_euets)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "red") +
  labs(
    title = "Log residuals vs firm size\n(log model with revenue + fuel from non-EUETS importers)",
    x = "log(revenue)",
    y = "log residual: log(predicted) – log(actual)"
  ) +
  theme_minimal()

# Compare fuel_non_euets-only vs revenue+fuel_non_euets
df_compare2 <- df_vis2 %>%
  pivot_longer(
    cols = c(log_resid_fuel_non_euets, log_resid_rev_fuel_non_euets),
    names_to = "model",
    values_to = "log_resid"
  )

ggplot(df_compare2, aes(x = log(revenue), y = log_resid, color = model)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Log residuals vs firm size\nfuel_non_euets-only vs revenue+fuel_non_euets",
    x = "log(revenue)",
    y = "log residual: log(predicted) – log(actual)"
  ) +
  scale_color_manual(
    values = c(
      "log_resid_fuel_non_euets"     = "darkred",
      "log_resid_rev_fuel_non_euets" = "steelblue"
    ),
    labels = c(
      "log_resid_fuel_non_euets"     = "FE + fuel_non_euets",
      "log_resid_rev_fuel_non_euets" = "FE + revenue + fuel_non_euets"
    )
  ) +
  theme_minimal()

## 3. Error contributions from top 1% firms -----------------------

df_lofo %>%
  mutate(
    abs_err_fuel_non_euets     = abs(pred_log_fuel_non_euets - emissions),
    abs_err_rev_fuel_non_euets = abs(pred_log_rev_fuel_non_euets - emissions),
    big_firm = revenue >= cutoff
  ) %>%
  group_by(big_firm) %>%
  summarise(
    share_obs = n() / nrow(df_lofo),
    err_fuel_non_euets     = sum(abs_err_fuel_non_euets, na.rm = TRUE),
    err_rev_fuel_non_euets = sum(abs_err_rev_fuel_non_euets, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    share_err_fuel_non_euets =
      err_fuel_non_euets / sum(err_fuel_non_euets),
    share_err_rev_fuel_non_euets =
      err_rev_fuel_non_euets / sum(err_rev_fuel_non_euets)
  )

# Density of absolute errors for big vs small firms (revenue+fuel_non_euets)
df_lofo %>%
  mutate(
    abs_err_rev_fuel_non_euets = abs(pred_log_rev_fuel_non_euets - emissions),
    big_firm = revenue >= cutoff
  ) %>%
  ggplot(aes(x = abs_err_rev_fuel_non_euets, fill = big_firm)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  labs(
    title = "Absolute error density (log scale)\nrevenue+fuel_non_euets, big vs small firms",
    x = "Absolute error (tons CO₂, log scale)",
    fill = "Top 1% firm?"
  ) +
  theme_minimal()
