#### HEADER -------

## Code to bulk download Sentinel-5P OFFL NO2 over Belgium for May-Sept 2019

# A single TROPOMI OFFL NO2 .nc file contains:

  # A global swath (thousands x thousands of pixels)
    # pixel is the spatial measurement unit made by the satellite:
    # a small area on the Earth's surface over which the satellite averages the signal
    # a TROPOMI NO2 pixel is roughly 3.5 km x 5.5 km
      # this probably covers multiple firms
  # For one satellite overpass (approx 100 minutes)
  # Each pixel has:
    # center lat and long
    # NO2 tropospheric column (mol/m^2)
    # QA value
      # QA value tells us how trustworthy the NO2 measurement is for that pixel
    # lots of metadata
    # the way to read this data is:
    # “Over a small area centered near (long, lat), the average tropospheric NO₂ column was no2 mol/m² at the time of this overpass.”

  # Grid cell 

#####################

# =========================
# SETUP ------------
# =========================
rm(list = ls())

library(httr)
library(jsonlite)
library(ncdf4)

Sys.setenv(R_CURL_SSL_BACKEND = "winssl")

base_dir <- "C:/Users/jota_/OneDrive/Documents/tropomi_no2_be_2019_may_sep"
dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)

# where big orbit files (temporary) go
nc_dir <- "C:/temp/tropomi_nc_tmp"
dir.create(nc_dir, showWarnings = FALSE, recursive = TRUE)

# where Belgium-only per-orbit files go
rds_dir <- file.path(base_dir, "be_rds")
dir.create(rds_dir, showWarnings = FALSE, recursive = TRUE)

# stacked output
stacked_path <- file.path(base_dir, "TROPOMI_NO2_BE_2019_MaySep_STACKED.rds")

odata_base <- "https://catalogue.dataspace.copernicus.eu/odata/v1"

# identity server used to have access to CDSE
token_url  <- "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token"

# =========================
# AREA OF INTEREST (AOI) ------------
# =========================

# AOI: rectangular bounding box covering Belgium
# with a small buffer to ensure that all satellite overpasses intersecting
# Belgium are retained in the API query. The same bounds are later used
# to filter pixel-level observations after download.

  # this is the area we used to filter satellite products of interest (the ones that intersect with Belgium);
  # defined in wkt (well-know text) format because that's what the OData API query requires
  aoi_wkt <- "POLYGON((2.0 49.0, 7.0 49.0, 7.0 52.0, 2.0 52.0, 2.0 49.0))"
  
  # this defines the box we use to create the Belgium-specific files
  bbox <- list(lon_min = 2.0, lon_max = 7.0, lat_min = 49.0, lat_max = 52.0)

# May–Sept 2019 (end exclusive)
start_utc <- "2019-05-01T00:00:00.000Z"
end_utc   <- "2019-10-01T00:00:00.000Z"

qa_thresh <- 0.75

# =========================
# AUTH TOKEN ------------
# =========================

  # Copernicus Data Space (CDSE) doesn't allow anonymous downloads;
  # It uses token-based authentification
  # This function returns a short-lived access token:  given username and pw,
  # get temporary access token
get_cdse_token <- function() {
  resp <- httr::POST(
    token_url,
    body = list(
      grant_type = "password",
      client_id  = "cdse-public",
      # I saved my CDSE USER and PASS in a separate .Renviron file
      # I created those online in CDSE before writing this code
      username   = Sys.getenv("CDSE_USER"),
      password   = Sys.getenv("CDSE_PASS")
    ),
    encode = "form",
    httr::timeout(60)
  )
  # resp = 200 means OK, 401/403 means wrong credentials, 500 server error
  # line below stops executiom if login fails
  httr::stop_for_status(resp)
  
  # this line extracts the access token
  httr::content(resp, as = "parsed", type = "application/json")$access_token
}

# =========================
# ODATA QUERY ---------
# =========================

# OData is a query language for web APIs
# CDSE API uses OData query language

# make_filter builds a filter string that describes which products we want;
  # look for SENTINEL-5P
  # offline (OFFL) Level-2 NO2 product
  # for orbits which intersect with region defined by aoi_wkt
  # at time periods between start_utc and end_utc
make_filter <- function() {
  paste0(
    "Collection/Name eq 'SENTINEL-5P' and ",
    "startswith(Name,'S5P_OFFL_L2__NO2____') and ",
    # filters satellite products whose footprint intersects with the aoi_wkt polygon:
    "OData.CSC.Intersects(area=geography'SRID=4326;", aoi_wkt, "') and ",
    "ContentDate/Start ge ", start_utc, " and ContentDate/Start lt ", end_utc
  )
}

# fetch_all_products queries the CDSE catalogue for all Sentinel-5P OFFL NO2 products
# matching the spatial and temporal filter from make_filter, and returns their metada
# as a single data frame

# APIs dont return all results at once, so we need to go through pages
# Here we are asking for at most page_size (100) results per request
fetch_all_products <- function(page_size = 100) {
  filt <- make_filter()
  
  # initialize storage
  all <- list()
  # pagination offset
  skip <- 0
  
  # begin pagination loop
    # this loop conitnues until API returns no more results, or
    # the last page is incomplete
  repeat {
    
    # query the catalogue:
    # sends a GET request to the API's /Prodcuts endpoint with the following parameters:
      # $filter: the spatial, temporal, and product-type filter defined earlier
      # $orderby: sort results chronologically
      # $top: returns at most page_size products
      # $skip: skip the first products (we want to move forward with pages, not start all over each time)
      # $select: return only the metadata fields listed
    resp <- httr::GET(
      paste0(odata_base, "/Products"),
      query = list(
        `$filter`  = filt,
        `$orderby` = "ContentDate/Start asc",
        `$top`     = page_size,
        `$skip`    = skip,
        `$select`  = "Id,Name,ContentDate,ContentLength"
      ),
      httr::timeout(60)
    )
    httr::stop_for_status(resp)
    
    # parse the response
    js <- content(resp, as = "parsed", type = "application/json")
    vals <- js$value
    if (length(vals) == 0) break
    
    # append results and report progress
    all <- c(all, vals)
    message("Fetched ", length(vals), " (skip=", skip, "). Total: ", length(all))
    
    if (length(vals) < page_size) break
    skip <- skip + page_size
  }
  
  if (length(all) == 0) return(data.frame())
  # convert to data frame;
  # each row of the data frame corresponds to one Sentinel-5P orbit file and contains:
    # Id: unique product identifier
    # Name: filename
    # ContentDate.Start/.End: sensing time
    # ContentLength: file size
  df <- do.call(rbind, lapply(all, as.data.frame))
  df
}

# =========================
# REDIRECT-SAFE DOWNLOADER ----------
# =========================

# function that wraps a fragile operation and automatically retries it several
# times with increasing delays, stopping only if the problem persists
  # expr_fun: function with no arguments; we will use download_one here
  # max_attemps: maximum number of tries before givin up
# high-level logic: tries to run expr_fun();
# if succeeds, returns result
# if errors, wait a bit a try again
# it if keeps failing, stop with an error
safe_get <- function(expr_fun, max_attempts = 6) {
  attempt <- 1
  # strat retry loop
  repeat {
    # expr_fun is executed; if it errors, stores the error in object res
    res <- tryCatch(expr_fun(), error = function(e) e)
    # if no error, returns result
    if (!inherits(res, "error")) return(res)
    
    # extract and print error message
    msg <- conditionMessage(res)
    message("Network error (attempt ", attempt, "/", max_attempts, "): ", msg)
    
    # stop if max attempts reached
    if (attempt >= max_attempts) stop(res)
    
    # wait before trying (backoff)
    Sys.sleep(3 * attempt)  # backoff: 3s,6s,9s,...
    attempt <- attempt + 1
  }
}

# This is the function that actually downloads the files
# What it does: Given a product id and name, download_one() downloads the corresponding .nc orbit file to nc_dir,
# handling authentication, redirects, retries, and corrupted/partial downloads,
# and returns the local file path plus the (possibly refreshed) token.

  # Inputs:
    # id, name: id and file name from the CDSE catalogue
    # token: CDSE access token
    # nc_dir: the folder to sabe the large orbit .nc files
    # max_hops: max numbers of redirects to follow manually

download_one <- function(id, name, token, nc_dir, max_hops = 10) {
  
  # creates the expected file path
  fname <- name
  nc_path <- file.path(nc_dir, fname)
  
  # if the file already exists and has positive size, assumes it was already successfully downloaded
  if (file.exists(nc_path) && file.info(nc_path)$size > 0) {
    message("Skip download (exists): ", fname)
    return(list(token = token, nc_path = nc_path, downloaded = FALSE))
  }
  
  # construct initial download URL
  url <- paste0(odata_base, "/Products(", id, ")/$value")
  
  do_get_nofollow <- function(u, tok, dest = NULL) {
    args <- list(
      url = u,
      httr::add_headers(Authorization = paste("Bearer", tok)),
      httr::config(followlocation = 0L),
      httr::timeout(600)
    )
    if (!is.null(dest)) args <- c(args, list(httr::write_disk(dest, overwrite = TRUE)))
    do.call(httr::GET, args)   # <-- IMPORTANT: httr::GET
  }
  
  current_url <- url
  refreshed <- FALSE
  
  for (hop in seq_len(max_hops)) {
    
    # ---- PROBE STEP (no download), WITH RETRIES ----
    resp <- safe_get(function() do_get_nofollow(current_url, token, dest = NULL), max_attempts = 6)
    
    # token refresh on 401 (probe)
    if (httr::status_code(resp) == 401 && !refreshed) {
      message("401: refreshing token and retrying probe: ", fname)
      token <- get_cdse_token()
      refreshed <- TRUE
      resp <- safe_get(function() do_get_nofollow(current_url, token, dest = NULL), max_attempts = 6)
    }
    
    # redirect?
    if (httr::status_code(resp) %in% c(301, 302, 303, 307, 308)) {
      loc <- httr::headers(resp)[["location"]]
      if (is.null(loc) || loc == "") stop("Redirect without Location header for ", fname)
      current_url <- loc
      next
    }
    
    # ---- FINAL DOWNLOAD STEP (write to disk), WITH RETRIES ----
    attempt <- 1
    max_attempts <- 6
    
    repeat {
      if (file.exists(nc_path)) suppressWarnings(file.remove(nc_path))
      
      resp2 <- tryCatch(
        do_get_nofollow(current_url, token, dest = nc_path),
        error = function(e) e
      )
      
      if (inherits(resp2, "error")) {
        message("Download error (attempt ", attempt, "/", max_attempts, "): ", conditionMessage(resp2))
        if (attempt >= max_attempts) stop(resp2)
        Sys.sleep(3 * attempt)
        attempt <- attempt + 1
        next
      }
      
      if (httr::status_code(resp2) == 401 && !refreshed) {
        message("401 on download: refreshing token and retrying: ", fname)
        token <- get_cdse_token()
        refreshed <- TRUE
        attempt <- attempt + 1
        next
      }
      
      if (httr::status_code(resp2) %in% c(429, 500, 502, 503, 504)) {
        message("HTTP ", httr::status_code(resp2), " (attempt ", attempt, "/", max_attempts, ") for ", fname)
        if (attempt >= max_attempts) httr::stop_for_status(resp2)
        Sys.sleep(3 * attempt)
        attempt <- attempt + 1
        next
      }
      
      httr::stop_for_status(resp2)
      
      # quick sanity: if file looks too small, treat as failed and retry
      if (!file.exists(nc_path) || file.info(nc_path)$size < 5e6) {
        message("Downloaded file too small; retrying: ", fname)
        if (attempt >= max_attempts) stop("Downloaded file too small repeatedly: ", fname)
        Sys.sleep(3 * attempt)
        attempt <- attempt + 1
        next
      }
      
      message("Downloaded: ", fname, " (", round(file.info(nc_path)$size/1e6, 1), " MB)")
      return(list(token = token, nc_path = nc_path, downloaded = TRUE))
    }
  }
  
  stop("Too many redirects (>", max_hops, ") for ", fname)
}

# =========================
# BELGIUM EXTRACTION
# =========================
extract_belgium_from_nc <- function(nc_path, bbox, qa_thresh) {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc), add = TRUE)
  
  no2 <- ncvar_get(nc, "PRODUCT/nitrogendioxide_tropospheric_column")
  qa  <- ncvar_get(nc, "PRODUCT/qa_value")
  lat <- ncvar_get(nc, "PRODUCT/latitude")
  lon <- ncvar_get(nc, "PRODUCT/longitude")
  
  lonv <- as.vector(lon)
  latv <- as.vector(lat)
  no2v <- as.vector(no2)
  qav  <- as.vector(qa)
  
  keep <- !is.na(no2v) & !is.na(qav) &
    (qav > qa_thresh) &
    (lonv >= bbox$lon_min & lonv <= bbox$lon_max) &
    (latv >= bbox$lat_min & latv <= bbox$lat_max)
  
  if (!any(keep)) {
    return(data.frame(lon=numeric(0), lat=numeric(0), no2=numeric(0), qa=numeric(0)))
  }
  
  data.frame(
    lon = lonv[keep],
    lat = latv[keep],
    no2 = no2v[keep],
    qa  = qav[keep]
  )
}

# =========================
# MAIN
# =========================
token <- get_cdse_token()
products <- fetch_all_products(page_size = 100)

message("Total products found (May–Sep 2019): ", nrow(products))
if (nrow(products) == 0) stop("No products found. (If this happens, widen AOI or confirm dates.)")

  # Downsample to make it manageable ------
  
  products <- products[order(products$ContentDate.Start), ]
  
  k <- 3  # keep every 3rd orbit (~413/3 ≈ 138)
  keep_idx <- seq(1, nrow(products), by = k)
  products_sub <- products[keep_idx, ]
  
  message("Downsampled orbits: ", nrow(products_sub), " of ", nrow(products))
  
  is_valid_netcdf <- function(path) {
    if (!file.exists(path)) return(FALSE)
    if (file.info(path)$size < 5e6) return(FALSE)  # too small to be real
    ok <- TRUE
    tryCatch({
      nc <- ncdf4::nc_open(path)
      ncdf4::nc_close(nc)
    }, error = function(e) { ok <<- FALSE })
    ok
  }
  
  process_one <- function(i) {
    # each worker uses its own token (avoid shared-token issues)
    token <- get_cdse_token()
    
    orbit_name <- products_sub$Name[i]
    orbit_id   <- products_sub$Id[i]
    
    rds_path <- file.path(rds_dir, paste0(orbit_name, ".rds"))
    if (file.exists(rds_path) && file.info(rds_path)$size > 0) {
      return(rds_path)
    }
    
    dl <- download_one(orbit_id, orbit_name, token, nc_dir)
    nc_path <- dl$nc_path
    
    # If file is corrupt/incomplete, delete and redownload once
    if (!is_valid_netcdf(nc_path)) {
      message("Invalid/corrupt nc file, re-downloading: ", orbit_name)
      suppressWarnings(file.remove(nc_path))
      dl <- download_one(orbit_id, orbit_name, token, nc_dir)
      nc_path <- dl$nc_path
      if (!is_valid_netcdf(nc_path)) {
        stop("Still invalid after re-download: ", orbit_name)
      }
    }
    
    df_be <- extract_belgium_from_nc(nc_path, bbox, qa_thresh)
    df_be$orbit_file <- rep(orbit_name, nrow(df_be))
    
    saveRDS(df_be, rds_path)
    
    # delete big orbit file after successful extraction
    if (file.exists(rds_path) && file.info(rds_path)$size > 0) {
      suppressWarnings(file.remove(nc_path))
    }
    
    rds_path
  }
  
  # Parallel download
  #install.packages(c("future.apply"))
  library(future.apply)
  
  plan(multisession, workers = 2)  # try 4; bump to 6 if stable
  
  rds_paths <- future.apply::future_lapply(seq_len(nrow(products_sub)), process_one)
  
  message("Per-orbit RDS files created: ", sum(file.exists(unlist(rds_paths))))
  
  # Stack satellite observations:
    # each row corresponds to one TROPOMI pixel obs over Belgium from one satellite overpass
    # long panel of pixel-overpass observation
  
  stacked <- do.call(rbind, lapply(rds_paths, readRDS))
  saveRDS(stacked, stacked_path)
  
  message("Saved stacked dataset: ", stacked_path)
  message("Rows in stacked dataset: ", nrow(stacked))
