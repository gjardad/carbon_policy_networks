#### HEADER -------

## Code to bulk download Sentinel-5P OFFL NO2 over Belgium for April 2018

#####################

# Setup ------
rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)

# ---- USER SETTINGS ----
out_dir <- "cdse_s5p_no2_offl_2018_04_be"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Belgium bounding box polygon (lon lat)
aoi_wkt <- "POLYGON((2.5 49.4, 6.5 49.4, 6.5 51.6, 2.5 51.6, 2.5 49.4))"

# Date range (UTC ISO)
start_utc <- "2018-06-01T00:00:00.000Z"
end_utc   <- "2018-07-01T00:00:00.000Z"  # end is exclusive, so use May 1 for April

# CDSE endpoints
token_url <- "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token"
odata_base <- "https://catalogue.dataspace.copernicus.eu/odata/v1"

# ---------
usethis::edit_r_environ()
cdse_user <- Sys.getenv("CDSE_USER")
cdse_pass <- Sys.getenv("CDSE_PASS")

if (cdse_user == "" || cdse_pass == "") {
  stop("Set environment variables CDSE_USER and CDSE_PASS before running.")
}

get_cdse_token <- function() {
  resp <- POST(
    url = token_url,
    body = list(
      grant_type = "password",
      client_id  = "cdse-public",
      username   = cdse_user,
      password   = cdse_pass
    ),
    encode = "form"
  )
  stop_for_status(resp)
  content(resp, as = "parsed", type = "application/json")$access_token
}

access_token <- get_cdse_token()

library(httr)

token_url <- "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token"

resp <- POST(
  token_url,
  body = list(
    grant_type = "password",
    client_id  = "cdse-public",
    username   = Sys.getenv("CDSE_USER"),
    password   = Sys.getenv("CDSE_PASS")
  ),
  encode = "form"
)

status_code(resp)
content(resp, as="text", encoding="UTF-8")[1:200]

