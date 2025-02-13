## ---- Load required libraries ------------------------------------------------
library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)
library(mwana)
library(sf)
library(tmap)

## ---- Read in input data and set Coordinate Reference System -----------------
source("scripts/read-in-survey-data.R")

## ---- Set survey data as a simple feature object and reporject ---------------
source("scripts/set-survey-as-sf.R")

## ---- Read in Sudan shapefile, project and filter out Al Fasher --------------
source("scripts/shapefiles.R")

## ---- Wrangle non-spatial data -----------------------------------------------
source("scripts/data-wrangling.R")

## ---- Run spatial interpolation ----------------------------------------------
source("scripts/interpolate.R")
