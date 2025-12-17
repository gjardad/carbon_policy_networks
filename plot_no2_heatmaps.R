#### HEADER -------

## Code that generates heatmap 

#####################

# =========================
# SETUP ------------
# =========================
rm(list = ls())

library(ggplot2)
library(sf)

#install.packages(c("rnaturalearth", "rnaturalearthdata"))
library(rnaturalearth)
library(rnaturalearthdata)

# =========================
# Import data ------------
# =========================

# Belgium boundary
be <- ne_countries(country = "Belgium", returnclass = "sf")

grid_dir <- "C:/Users/jota_/OneDrive/Documents/tropomi_no2_be_2019_may_sep/grids"
g_detect <- readRDS(file.path(grid_dir, "/GRID_DETECT_res0p02_minN5_MaySep2019_BE.rds"))

# =========================
# Generate heatmap --------
# =========================
res <- 0.02
g_plot <- transform(
  g_detect,
  lon_c = lon_bin + res/2,
  lat_c = lat_bin + res/2
)

cities <- data.frame(
  name = c("Antwerp", "Brussels", "Ghent", "Liège", "Charleroi"),
  lon  = c(4.40, 4.35, 3.72, 5.57, 4.44),
  lat  = c(51.22, 50.85, 51.05, 50.63, 50.41)
)

ggplot() +
  geom_sf(data = be, fill = NA, linewidth = 0.4) +
  geom_tile(data = g_plot, aes(x = lon_c, y = lat_c, fill = no2), alpha = 0.95) +
  geom_point(data = cities, aes(x = lon, y = lat), color = "black", size = 2) +
  geom_text(data = cities, aes(x = lon, y = lat, label = name),
            vjust = -0.8, size = 3) +
  coord_sf(xlim = c(2, 7), ylim = c(49, 52), expand = FALSE) +
  scale_fill_viridis_c(option = "magma", trans = "log10") +
  labs(
    title = "TROPOMI tropospheric NO₂ over Belgium (May–Sep 2019)",
    subtitle = "0.02° detection grid; mean NO₂ per cell (log scale)",
    fill = "NO₂ (mol/m²)"
  ) +
  theme_minimal(base_size = 12)

# ==========================
# Heatmap with hotspots ----
# ==========================

thr <- quantile(g_detect$no2, 0.99)
g_hot <- subset(g_plot, no2 >= thr)

ggplot() +
  geom_sf(data = be, fill = NA, linewidth = 0.4) +
  geom_point(data = g_hot, aes(x = lon_c, y = lat_c, color = no2), size = 1.2) +
  coord_sf(xlim = c(2.0, 7.0), ylim = c(49.0, 52.0), expand = FALSE) +
  scale_color_viridis_c(option = "magma", trans = "log10") +
  labs(
    title = "Top 1% NO₂ grid cells (hotspots)",
    x = "Longitude", y = "Latitude", color = "NO₂ (mol/m²)"
  ) +
  theme_minimal(base_size = 12)

# ====================
# Plot coverage ------
# ====================

res <- 0.02
g_n <- transform(
  g_detect,
  lon_c = lon_bin + res/2,
  lat_c = lat_bin + res/2
)

ggplot() +
  geom_sf(data = be, fill = NA, linewidth = 0.4) +
  geom_tile(data = g_n, aes(x = lon_c, y = lat_c, fill = n), alpha = 0.95) +
  coord_sf(xlim = c(2, 7), ylim = c(49, 52), expand = FALSE) +
  scale_fill_viridis_c(trans = "log10") +
  labs(
    title = "TROPOMI coverage over Belgium (May–Sep 2019)",
    subtitle = "0.02° detection grid; fill = number of pixel observations per cell (log scale)",
    x = "Longitude", y = "Latitude", fill = "Count (n)"
  ) +
  theme_minimal(base_size = 12)