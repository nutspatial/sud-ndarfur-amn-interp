################################################################################
#                   SET SURVEY DATA AS SIMPLE FEATURE OBJECT                   #
################################################################################

## ---- Exclude rows with missing latitude and longitude coordinates -----------
data <- filter(
  .data = smart_survey,
  !is.na(latitude)
)

## ---- Set survey data as a simple feature object -----------------------------
data <- st_as_sf(
  x = data,
  coords = c("latitude", "longitude"),
  dim = "XY"
  )

## ---- Set a Coordinate Reference System of the simple file object ------------
data <- st_set_crs(
  x = data, 
  value = 4326
)

## ---- Transform the geographic CRS into Sudan's local UTM --------------------
data <- st_transform(
  x = data, 
  crs = 29636
)

################################ End of workflow ###############################