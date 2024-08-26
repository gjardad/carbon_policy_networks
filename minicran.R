#### HEADER -------

## This code manages packages offline using miniCRAN

#####################

#install.packages("miniCRAN")
library(miniCRAN)

# define CRAN mirror
mirror <- c(CRAN = "https://cloud.r-project.org")

# Specify list of packages to download
pkgs <- c("tidyverse")
availPkgs <- available.packages(repos = mirror)
pkgList <- pkgDep(pkgs, repos = mirror, type = "source", suggests = FALSE, 
                  availPkgs = availPkgs)
pkgList

# Create temporary folder for miniCRAN
dir.create(pth <- "X:/Documents/JARDANG/miniCRAN")

# Make repo for source and win.binary
makeRepo(pkgList, path = pth, repos = mirror, type = c("source", "win.binary"))

# List all files in miniCRAN
list.files(pth, recursive = TRUE, full.names = FALSE)

# Check for available packages
pkgAvail(repos = pth, type = "win.binary")[, c(1:3, 5)]

# Install packages from local repo
install.packages(pkgs, repos = paste0("file:///", normalizePath(pth)), type = "win.binary")

# For some reason, package stringi not properly installed
install.packages("stringi", repos = paste0("file:///", normalizePath(pth)), type = "win.binary")


