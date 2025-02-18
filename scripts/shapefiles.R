################################################################################
#                                READ SHAPEFILES                               #
################################################################################

## ---- Read Sudan shapefile in ------------------------------------------------
sudan_adm3 <- st_read(
  dsn = "data-raw/sdn_adm3.shp"
)

## ---- Set CRS to UTM and filter out Al Fasher locality -----------------------
al_fasher <- sudan_adm3 |> 
  filter(NAME_2 == "Al Fasher") |> 
  st_transform(crs = "EPSG:32635")

################################ End of workflow ###############################
