################################################################################
#                                READ SHAPEFILES                               #
################################################################################

## ---- Read Sudan shapefile in ------------------------------------------------
sudan_adm2 <- st_read(
  dsn = "data-raw/sdn_admbnda_adm2_cbs_nic_ssa_20200831.shp"
)

## ---- Set CRS to UTM and filter out Al Fasher locality -----------------------
al_fasher <- sudan_adm2 |> 
  filter(ADM2_EN == "Al Fasher") |> 
  st_transform(crs = "EPSG:32635")

################################ End of workflow ###############################
