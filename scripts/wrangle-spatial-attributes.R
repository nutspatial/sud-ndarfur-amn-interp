################################################################################
#            WORKFLOW TO WRANGLE DATA THROUGH SPATIAL ATTRIBUTES               #
################################################################################

## ---- Set WFHZ data as an `sf` object  ---------------------------------------
wfhz <- smart_wfhz |> 
filter(!flag_wfhz == 1) |> 
  select(locality, cluster, latitude, longitude, gam) |> 
  filter(!is.na(latitude)) |> 
  st_as_sf(
    coords = c("latitude", "longitude"),
    dim = "XY"
  ) |> 
  st_set_crs(value = "EPSG:4326")

## ---- Workflow to calculate Spatial Empirical Bayesian Rates (SEBSR) ---------
aggr_wfhz <- wfhz |>
  mutate(
    long_x = st_coordinates(geometry)[, 1],
    lati_y = st_coordinates(geometry)[, 2]
  ) |>
  group_by(cluster) |>
  summarise(
    cases = sum(gam, na.rm = TRUE),
    pop = n(),
    x = mean(long_x, na.rm = TRUE),
    y = mean(lati_y, na.rm = TRUE)
  ) |>
  as_tibble() |>
  select(-geometry) |>
  st_as_sf(
    coords = c("x", "y"),
    dim = "XY",
    crs = "EPSG:4326"
  ) |>
  st_transform(crs = "EPSG:20135")

### -------------------------- Calculate spatial weights: K-Near Neighbours ----
sp_wts_wfhz <- aggr_wfhz |>
  knearneigh(
    k = 4,
    longlat = TRUE,
    use_kd_tree = TRUE
  ) |>
  knn2nb(row.names = NULL)

## ---
ggplot(data = sudan_adm3) +
  geom_sf(
    fill = "white",
    color = "#3F4342",
    size = 0.8
  ) +
  geom_sf(
    data = aggr_wfhz,
    aes(color = cases)
  ) +
  geom_sf_text(
    data = sudan_adm3,
    mapping = aes(label = factor(NAME_3)),
    show.legend = TRUE,
    colour = "#34495E",
    size = 1.8
  ) +
  # scale_color_manual(
  #   values = apply_ipc_colours(indicator = "wfhz", .map_type = "static"),
  #   name = "Raw rates"
  # ) +
  theme_void() +
  # labs(
  #   title = "Spatial distribution GAM rates by sampling points across the Karamoja region",
  #   subtitle = "Raw rates: cases / total number of children surveyed"
  # ) +
  theme(
    plot.title = element_text(size = 11),
    plot.subtitle = element_text(size = 9, colour = "#706E6D")
  )
################################ End of workflow ###############################
