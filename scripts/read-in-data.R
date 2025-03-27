################################################################################
#                                READ SHAPEFILES                               #
################################################################################


## ---- Read data --------------------------------------------------------------

### --------------------------------------- Decrypt and read nutrition data ----
input_data <- decrypt(
  expr = read.csv("data-raw/north-darfur-smart-survey.csv"),
  key = secret_key
)

## ---- Read Sudan shapefile in ------------------------------------------------
sudan_adm3 <- st_read(
  dsn = "data-raw/sdn_adm3.shp", 
  quiet = TRUE
)
################################ End of workflow ###############################
