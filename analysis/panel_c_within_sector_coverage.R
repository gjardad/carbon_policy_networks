###############################################################################
# panel_c_within_sector_coverage.R
#
# PURPOSE:
#   Histogram of EU ETS coverage shares within IPCC sectors, pooling
#   Belgium, Bulgaria, Czechia, and France (Art14 Annex XII data).
#   Excludes sectors with coverage > 100% (reporting artefacts where
#   authorities cannot separate fuel combustion from industrial processes).
#
# DATA SOURCES:
#   Art14 Annex XII files from EIONET CDR (national inventory reports)
###############################################################################

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)

# ---- Paths ----
nir_dir    <- "c:/Users/jota_/Documents/NBB_data/raw/NIR"
project_root <- here::here()
output_dir <- file.path(project_root, "output", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Helper: parse one AnnexXII file ----
parse_annex_xii <- function(path, country) {
  # Read raw (no headers)
  sheet <- if (country == "FR") 1 else 1
  raw <- read_excel(path, col_names = FALSE, sheet = sheet)

  # Standardize column names
  names(raw) <- c("sector", "inventory_kt", "ets_kt", "ratio", "comment")

  # Keep only rows where sector looks like a CRF code (starts with digit or "Iron")
  # and ratio is numeric
  df <- raw %>%
    filter(
      !is.na(sector),
      str_detect(sector, "^(\\d|Iron)")
    ) %>%
    mutate(
      ratio = suppressWarnings(as.numeric(ratio)),
      inventory_kt = suppressWarnings(as.numeric(inventory_kt)),
      ets_kt = suppressWarnings(as.numeric(ets_kt))
    ) %>%
    filter(!is.na(ratio))

  # Normalize ratio to percentage (FR, BG, CZ use 0-1; BE uses 0-100)
  # Detect scale: if max ratio <= 2, it's in 0-1 scale
  if (max(df$ratio, na.rm = TRUE) <= 2) {
    df <- df %>% mutate(ratio = ratio * 100)
  }

  df %>%
    mutate(country = country) %>%
    select(country, sector, inventory_kt, ets_kt, ratio)
}

# ---- Read all four files ----
files <- list(
  BE = file.path(nir_dir, "BE_Art14_AnnexXII_2025.xlsx"),
  FR = file.path(nir_dir, "FR_Art14_AnnexXII_2024.xlsx"),
  BG = file.path(nir_dir, "BG_Art14_AnnexXII_2026.xlsx"),
  CZ = file.path(nir_dir, "CZ_Art14_AnnexXII_2026.xlsx")
)

all_data <- bind_rows(
  lapply(names(files), function(cc) parse_annex_xii(files[[cc]], cc))
)

cat("Total sector-country observations:", nrow(all_data), "\n")
cat("Coverage > 100%:", sum(all_data$ratio > 100), "\n")

# ---- Filter ----
# Exclude aggregate rows (1.A total, 1.A stationary total) — keep leaf sectors
# Exclude "Iron and steel" combined row (starts with "Iron")
# Exclude coverage > 100%
plot_data <- all_data %>%
  filter(
    ratio > 0,
    ratio <= 100,
    # Drop aggregate rows that aren't leaf sectors
    !str_detect(sector, "^1\\.A Fuel combustion"),
    !str_detect(sector, "^1\\.A\\.2\\. Manufacturing industries and construction$"),
    !str_detect(sector, "^1\\.A\\.2 Manufacturing industries and construction$")
  )

cat("Observations after filtering:", nrow(plot_data), "\n")
cat("Coverage distribution:\n")
print(summary(plot_data$ratio))

# ---- Histogram ----
med_cov  <- median(plot_data$ratio)
n_obs    <- nrow(plot_data)

# Aggregate coverage: total ETS emissions / total inventory emissions (pooled)
agg_cov <- sum(plot_data$ets_kt, na.rm = TRUE) /
           sum(plot_data$inventory_kt, na.rm = TRUE) * 100

cat(sprintf("Aggregate coverage (ETS/inventory, pooled): %.1f%%\n", agg_cov))

panel_c <- ggplot(plot_data, aes(x = ratio)) +
  geom_histogram(
    binwidth = 5,
    boundary = 0,
    fill = "grey60",
    color = "white"
  ) +
  geom_vline(xintercept = med_cov, linetype = "dashed",
             color = "#E0405E", linewidth = 0.7) +
  annotate("text",
           x = med_cov - 2, y = Inf, vjust = 1.5,
           label = paste0("Median: ", round(med_cov), "%"),
           color = "#E0405E", size = 4, hjust = 1, fontface = "bold") +
  geom_vline(xintercept = agg_cov, linetype = "dashed",
             color = "#2166AC", linewidth = 0.7) +
  annotate("text",
           x = agg_cov - 2, y = Inf, vjust = 3.5,
           label = paste0("Aggregate: ", round(agg_cov), "%"),
           color = "#2166AC", size = 4, hjust = 1, fontface = "bold") +
  scale_x_continuous(
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "Share of sector emissions covered by the EU ETS",
    y = "Number of sector-country pairs"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 11)
  )

ggsave(file.path(output_dir, "panel_c_within_sector_coverage.pdf"),
       panel_c, width = 7, height = 4.5)
ggsave(file.path(output_dir, "panel_c_within_sector_coverage.png"),
       panel_c, width = 7, height = 4.5, dpi = 300)

cat("\nFigure saved to:", output_dir, "\n")
