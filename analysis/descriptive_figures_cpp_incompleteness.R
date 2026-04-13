###############################################################################
# descriptive_figures_cpp_incompleteness.R
#
# PURPOSE:
#   Create two descriptive figures showing that carbon pricing policies (CPPs)
#   as implemented in practice are incomplete:
#     Panel A: Across sectors — share of each sector's global GHG emissions
#              covered by an ETS or carbon tax (from Figure 3, State & Trends
#              of Carbon Pricing 2025, World Bank, p.27)
#     Panel B: Across instruments — distribution of the share of jurisdiction's
#              GHG emissions covered by each carbon pricing instrument
#              (from Annex B, State & Trends of Carbon Pricing 2025, pp.69-72)
#
# DATA SOURCES:
#   - data/raw/sector_coverage_figure3_2025.csv
#   - data/raw/annex_b_carbon_pricing_2025.csv
###############################################################################

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(forcats)

# ---- Paths ----
project_root <- here::here()
raw_data     <- file.path(project_root, "data", "raw")
output_dir   <- file.path(project_root, "output", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Panel A: Across-sector incompleteness
# =============================================================================

sector_df <- read_csv(file.path(raw_data, "sector_coverage_figure3_2025.csv"),
                      show_col_types = FALSE)

# Reshape for stacked bar
sector_long <- sector_df %>%
  select(sector, ets_pct, carbon_tax_pct) %>%
  pivot_longer(cols = c(ets_pct, carbon_tax_pct),
               names_to = "instrument",
               values_to = "coverage") %>%
  mutate(
    instrument = case_when(
      instrument == "ets_pct" ~ "ETS",
      instrument == "carbon_tax_pct" ~ "Carbon tax"
    ),
    instrument = factor(instrument, levels = c("Carbon tax", "ETS")),
    sector = fct_reorder(sector, coverage, .fun = sum)
  )

panel_a <- ggplot(sector_long,
                  aes(x = coverage, y = sector, fill = instrument)) +
  geom_col(width = 0.7) +
  geom_text(
    data = sector_df %>% mutate(sector = fct_reorder(sector, total_pct)),
    aes(x = total_pct + 1.5, y = sector, label = paste0(total_pct, "%"),
        fill = NULL),
    hjust = 0, size = 3.2, color = "grey30"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.12)),
    labels = function(x) paste0(x, "%"),
    breaks = seq(0, 50, 10)
  ) +
  scale_fill_manual(
    values = c("ETS" = "#4A3B8F", "Carbon tax" = "#E0405E"),
    name = NULL
  ) +
  labs(
    title = "Share of each sector's global GHG emissions\ncovered by a carbon pricing instrument",
    x = NULL,
    y = NULL,
    caption = "Source: State and Trends of Carbon Pricing 2025, World Bank (Figure 3, p. 27)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.caption = element_text(size = 7, color = "grey50", hjust = 0),
    axis.text.y = element_text(size = 9)
  )

ggsave(file.path(output_dir, "panel_a_sector_coverage.pdf"),
       panel_a, width = 7, height = 5)
ggsave(file.path(output_dir, "panel_a_sector_coverage.png"),
       panel_a, width = 7, height = 5, dpi = 300)

# =============================================================================
# Panel B: Distribution of jurisdiction coverage shares across instruments
# =============================================================================

annex_b <- read_csv(file.path(raw_data, "annex_b_carbon_pricing_2025.csv"),
                    show_col_types = FALSE)

# Drop instruments with missing coverage
annex_b_clean <- annex_b %>%
  filter(!is.na(share_ghg_covered_pct))

# Summary stats for annotation
med_cov  <- median(annex_b_clean$share_ghg_covered_pct)
mean_cov <- mean(annex_b_clean$share_ghg_covered_pct)
n_instr  <- nrow(annex_b_clean)

panel_b <- ggplot(annex_b_clean,
                  aes(x = share_ghg_covered_pct)) +
  geom_histogram(
    binwidth = 10,
    boundary = 0,
    fill = "#4A3B8F",
    color = "white",
    alpha = 0.85
  ) +
  geom_vline(xintercept = med_cov, linetype = "dashed",
             color = "#E0405E", linewidth = 0.7) +
  annotate("text",
           x = med_cov + 2, y = Inf, vjust = 1.5,
           label = paste0("Median: ", round(med_cov), "%"),
           color = "#E0405E", size = 3.5, hjust = 0, fontface = "bold") +
  scale_x_continuous(
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = paste0("Distribution of jurisdictional GHG coverage\nacross ",
                   n_instr, " carbon pricing instruments"),
    x = "Share of jurisdiction's GHG emissions covered",
    y = "Number of instruments",
    caption = "Source: State and Trends of Carbon Pricing 2025, World Bank (Annex B, pp. 69-72)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.caption = element_text(size = 7, color = "grey50", hjust = 0)
  )

ggsave(file.path(output_dir, "panel_b_coverage_distribution.pdf"),
       panel_b, width = 7, height = 4.5)
ggsave(file.path(output_dir, "panel_b_coverage_distribution.png"),
       panel_b, width = 7, height = 4.5, dpi = 300)

cat("Figures saved to:", output_dir, "\n")
cat("Panel A: sector coverage (across-sector incompleteness)\n")
cat("Panel B: coverage distribution (across-instrument incompleteness)\n")
cat(sprintf("Summary: Median coverage = %.0f%%, Mean = %.0f%%, N = %d instruments\n",
            med_cov, mean_cov, n_instr))
