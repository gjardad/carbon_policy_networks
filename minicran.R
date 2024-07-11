#### HEADER -------

## This code manages packages offline using miniCRAN

#####################

## Can I build packages from source? ----
# install.packages("pkgbuild")
pkgbuild::check_build_tools()

#install.packages("miniCRAN")
library(miniCRAN)

# define CRAN mirror
mirror <- c(CRAN = "https://cloud.r-project.org")

# Specify list of packages to download
pkgs <- c("tidyverse")
pkgList <- pkgDep(pkgs, repos = mirror, type = "source", suggests = FALSE, 
                  availPkgs = cranJuly2014)
pkgList

# Create temporary folder for miniCRAN
dir.create(pth <- file.path(tempdir(), "miniCRAN"))

# Make repo for source and win.binary
makeRepo(pkgList, path = pth, repos = mirror, type = c("source", "win.binary"))

# List all files in miniCRAN
list.files(pth, recursive = TRUE, full.names = FALSE)

# Check for available packages
pkgAvail(repos = pth, type = "win.binary")[, c(1:3, 5)]




