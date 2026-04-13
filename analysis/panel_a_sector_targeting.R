###############################################################################
# panel_a_sector_targeting.R
#
# PURPOSE:
#   Bar chart showing, for each industrial activity, the share of in-force
#   ETS instruments that cover it. Data from ICAP ETS factsheets (scraped
#   April 2026).
#
# DATA SOURCE:
#   NBB_data/raw/icap_ets_sector_coverage.csv
###############################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# ---- Paths ----
project_root <- here::here()
output_dir   <- file.path(project_root, "output", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Load data ----
d <- read.csv("c:/Users/jota_/Documents/NBB_data/raw/icap_ets_sector_coverage.csv",
              stringsAsFactors = FALSE)

# Filter to in-force only
inforce <- d %>% filter(status == "In force")
cat("In-force ETSs:", nrow(inforce), "\n")

# ---- Activity columns ----
activity_cols <- c("power", "oil_refining", "iron_steel", "cement", "glass",
                   "lime", "ceramics", "pulp_paper", "chemicals", "aluminum",
                   "ammonia", "food_bev", "other_industry", "aviation",
                   "maritime", "buildings", "road_transport", "waste_incineration")

# Count "Yes" only (treat "Unclear" and "No" as not confirmed)
activity_shares <- inforce %>%
  summarise(across(all_of(activity_cols),
                   ~ sum(.x == "Yes", na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "activity", values_to = "n_yes") %>%
  mutate(
    share = n_yes / nrow(inforce) * 100,
    activity_label = case_match(
      activity,
      "power"              ~ "Power & heat",
      "oil_refining"       ~ "Oil refining",
      "iron_steel"         ~ "Iron & steel",
      "cement"             ~ "Cement",
      "glass"              ~ "Glass",
      "lime"               ~ "Lime",
      "ceramics"           ~ "Ceramics & bricks",
      "pulp_paper"         ~ "Pulp & paper",
      "chemicals"          ~ "Chemicals &\npetrochemicals",
      "aluminum"           ~ "Aluminum &\nnon-ferrous metals",
      "ammonia"            ~ "Ammonia",
      "food_bev"           ~ "Food & beverages",
      "other_industry"     ~ "Other industry",
      "aviation"           ~ "Aviation",
      "maritime"           ~ "Maritime",
      "buildings"          ~ "Buildings &\nheating fuels",
      "road_transport"     ~ "Road transport",
      "waste_incineration" ~ "Waste incineration"
    ),
    activity_label = fct_reorder(activity_label, share)
  ) %>%
  filter(activity != "other_industry")

cat("\nActivity coverage shares:\n")
print(activity_shares %>% arrange(desc(share)) %>%
        select(activity_label, n_yes, share), n = 20)

# ---- Bar chart ----
n_ets <- nrow(inforce)

panel_a <- ggplot(activity_shares, aes(x = share, y = activity_label)) +
  geom_col(fill = "grey60", width = 0.7) +
  geom_text(aes(label = paste0(round(share), "%")),
            hjust = -0.2, size = 3.5, color = "grey30") +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.15)),
    labels = function(x) paste0(x, "%"),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    x = paste0("Share of in-force ETS instruments (N = ", n_ets, ")"),
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11)
  )

ggsave(file.path(output_dir, "panel_a_sector_targeting.pdf"),
       panel_a, width = 7, height = 6)
ggsave(file.path(output_dir, "panel_a_sector_targeting.png"),
       panel_a, width = 7, height = 6, dpi = 300)

cat("\nFigure saved to:", output_dir, "\n")
