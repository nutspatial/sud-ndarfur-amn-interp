## ---- Load required libraries ------------------------------------------------
library(tidyr)
library(dplyr)
library(lubridate)
library(mwana)
library(sf)
library(automap)
library(stars)
library(ggplot2)
library(spdep)

## ---- Load utility functions -------------------------------------------------
source("R/utils.R")

## ---- Read in Sudan shapefile ------------------------------------------------
source("scripts/read-in-shapefiles.R")

## ---- Wrangle aspatial data --------------------------------------------------
source("scripts/wrangle-aspatial-attributes.R")

## ---- Run data plausibility checks -------------------------------------------
source("scripts/data-quality-check.R")

## ---- Wrangle spatial data ---------------------------------------------------
source("scripts/wrangle-spatial-attributes.R")

## ---- Run spatial interpolation ----------------------------------------------
source("scripts/krige-interpolate-wfhz-automap.R")
