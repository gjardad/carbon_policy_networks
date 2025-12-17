#### HEADER -------

## Code that creates gridded data set at 0.02 x 0.02
# and gridded data set at 0.01 x 0.01 within regions with hotspots

#####################

# =========================
# SETUP ------------
# =========================
rm(list = ls())

stacked_path <- "C:/Users/jota_/OneDrive/Documents/tropomi_no2_be_2019_may_sep/TROPOMI_NO2_BE_2019_MaySep_STACKED.rds"
df <- readRDS(stacked_path)

# =========================
# Create gridded data sets ------------
# =========================

# function grid_one: assigns each pixel to a grid cell
  # for each pixel (each row of df), compute
  # lon_bin: the lower-left long edge of the grid cell
  # lat_bin: lower-left lat edge
grid_one <- function(df, res, min_n) {
  df$lon_bin <- floor(df$lon / res) * res
  df$lat_bin <- floor(df$lat / res) * res
  
  # compute per-cell summaires:
    # g_mean: mean NO2 within cell
    # g_n: number of obs in that cell
    # g_qa: mean QA in that cell
  g_mean <- aggregate(no2 ~ lon_bin + lat_bin, data = df, FUN = mean)
  g_n    <- aggregate(no2 ~ lon_bin + lat_bin, data = df, FUN = length)
  g_qa   <- aggregate(qa  ~ lon_bin + lat_bin, data = df, FUN = mean)
  
  # combine summaries into one table
  g <- merge(g_mean, g_n, by = c("lon_bin","lat_bin"), suffixes = c("", "_n"))
  g <- merge(g, g_qa,   by = c("lon_bin","lat_bin"))
  names(g)[names(g) == "no2_n"] <- "n"
  names(g)[names(g) == "qa"]    <- "qa_mean"
  
  subset(g, n >= min_n)
}

# try grids with two different resolutions
g_002 <- grid_one(df, res = 0.02, min_n = 5)
g_001 <- grid_one(df, res = 0.01, min_n = 2)

# basic diagnostics across the two resolutions
c(
  n_cells_002 = nrow(g_002),
  n_cells_001 = nrow(g_001),
  top_002 = max(g_002$no2),
  top_001 = max(g_001$no2)
)

# =========================
# Identify hotspots ------------
# =========================

# Detection grid
g_detect <- grid_one(df, res = 0.02, min_n = 5)

# Localization grid
g_local  <- grid_one(df, res = 0.01, min_n = 2)

# Stage 1: coarse hotspots
  # among the gridded data set with coarses resolution, identify the hotspots
  # how_cells is a set of high-NO_2 grid cells
thr <- quantile(g_detect$no2, 0.99)  # top 1%
hot_cells <- subset(g_detect, no2 >= thr)

# Stage 2: refine grid around the hotspot bounding box
  # define a bounding box around hotspots: find the smallest rectangle that contains all
  # hotspot grid cells, and expand it by pad degrees on each side
pad <- 0.10
bb <- with(hot_cells, list(
  lon_min = min(lon_bin) - pad, lon_max = max(lon_bin) + pad,
  lat_min = min(lat_bin) - pad, lat_max = max(lat_bin) + pad
))

# zoom in: subset the df to the region around hotspots
df_zoom <- subset(df,
                  lon >= bb$lon_min & lon <= bb$lon_max &
                    lat >= bb$lat_min & lat <= bb$lat_max
)

# create a grid at finer resolution for the regions with hotspots
g_zoom <- grid_one(df_zoom, res = 0.01, min_n = 1)

summary(g_zoom$n)
mean(g_zoom$n >= 5)

# =========================
# Identify hotspots ------------
# =========================

grid_dir <- "C:/Users/jota_/OneDrive/Documents/tropomi_no2_be_2019_may_sep/grids"
dir.create(grid_dir, showWarnings = FALSE, recursive = TRUE)

saveRDS(
  g_detect,
  file = file.path(
    grid_dir,
    "GRID_DETECT_res0p02_minN5_MaySep2019_BE.rds"
  )
)
saveRDS(
  g_zoom,
  file = file.path(
    grid_dir,
    "GRID_ZOOM_res0p01_minN1_MaySep2019_BE.rds"
  )
)

grid_metadata <- list(
  detect = list(
    resolution = 0.02,
    min_n = 5,
    purpose = "Hotspot detection; robust grid used for firm matching"
  ),
  zoom = list(
    resolution = 0.01,
    min_n = 1,
    purpose = "Localization and visualization only; sparse by construction"
  ),
  period = "May–September 2019",
  source = "Sentinel-5P TROPOMI OFFL NO2",
  units = "mol/m^2"
)

saveRDS(
  grid_metadata,
  file = file.path(grid_dir, "GRID_METADATA.rds")
)
