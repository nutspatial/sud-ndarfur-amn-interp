## ---- Load required libraries ------------------------------------------------
library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)
library(mwana)
library(sf)
library(ggplot2)

## ---- Read in input data and set Coordinate Reference System -----------------
source("scripts/read-in-survey-data.R")

## ---- Read in Sudan shapefile, project and filter out Al Fasher --------------
source("scripts/shapefiles.R")

## ---- Wrangle non-spatial data -----------------------------------------------
source("scripts/wrangle-non-spatial-attributes.R")

## ---- Wrangle spatial data -----------------------------------------------
source("scripts/wrangle-spatial-attributes.R")

## ---- Run spatial interpolation ----------------------------------------------
source("scripts/interpolate.R")
