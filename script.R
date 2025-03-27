## ---- Load required libraries ------------------------------------------------
library(tidyr)
library(dplyr)
library(lubridate)
library(mwana)
library(sf)
library(automap)
library(stars)
library(gstat)
library(ggplot2)
library(spdep)
library(cyphr)


## ---- Retrieve secret key for decryption -------------------------------------
secret_key <- data_key(".", path_user = Sys.getenv("path_secret_key"))

## ---- Load project-specific functions ----------------------------------------
lapply(list.files(path = "R", full.names = TRUE), FUN = source)

## ---- Read in nutrition data and Sudan shapefiles ----------------------------
source("scripts/read-in-data.R")

## ---- Wrangle aspatial data --------------------------------------------------
source("scripts/wrangle-aspatial-attributes-wfhz.R")
source("scripts/wrangle-aspatial-attributes-muac.R")

## ---- Run data plausibility checks -------------------------------------------
source("scripts/data-quality-check.R")

## ---- Wrangle spatial data ---------------------------------------------------
source("scripts/wrangle-spatial-attributes-wfhz.R")
source("scripts/wrangle-spatial-attributes-muac.R")

## ---- Run spatial interpolation ----------------------------------------------
source("scripts/krige-interpolate-gam-wfhz-elftawila.R")
source("scripts/krige-interpolate-gam-muac-elftawila.R")
source("scripts/krige-interpolate-gam-wfhz-laittaw.R")
source("scripts/krige-interpolate-gam-muac-laittaw.R")


