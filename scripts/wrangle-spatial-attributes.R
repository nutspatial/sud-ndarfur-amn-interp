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
  group_by(locality, cluster) |>
  summarise(
    cases = sum(gam, na.rm = TRUE),
    pop = n(),
    x = mean(long_x, na.rm = TRUE),
    y = mean(lati_y, na.rm = TRUE), 
    .groups = "drop"
  ) |>
  as_tibble() |>
  select(-geometry) |>
  st_as_sf(
    coords = c("x", "y"),
    dim = "XY",
    crs = "EPSG:4326"
  ) |>
  st_transform(crs = "EPSG:20135")

## ---- Check the spatial distribution of the sampling points ------------------
ggplot(data = sudan_adm3) +
  geom_sf(
    fill = "white",
    color = "#3F4342",
    size = 0.8
  ) +
  geom_sf(
    data = aggr_wfhz,
    aes(color = "orange"),
    alpha = 0.3
  ) +
  geom_sf_text(
    data = sudan_adm3,
    mapping = aes(label = factor(NAME_3)),
    colour = "#34495E",
    size = 1.8
  ) +
  theme_void() +
  labs(
    title = "Spatial distribution of the surveyed sampling points",
    subtitle = "Um Kaddada and El Lait, 97.5% and 46% of rows had missing XY coordinates, respectively"
  ) +
  theme(
    plot.title = element_text(size = 11),
    plot.subtitle = element_text(size = 9, colour = "#706E6D")
  )

### -------------------------- Calculate spatial weights: K-Near Neighbours ----
sp_wts_wfhz <- aggr_wfhz |>
  knearneigh(
    k = 4,
    longlat = TRUE,
    use_kd_tree = TRUE
  ) |>
  knn2nb(row.names = NULL)

### ------------------------------------------------------- Calculate rates ----
sebsr_wfhz <- EBlocal(
  ri = aggr_wfhz$cases,
  ni = aggr_wfhz$pop,
  nb = sp_wts_wfhz
)

#### Bind data.frames -----
wrangled_wfhz <- cbind(aggr_wfhz, sebsr_wfhz)

## ---- Split localities to address the issue of missing XY coordinates in some 
al_fasher_tawila <- wrangled_wfhz |> 
  filter(local)

## ---- Map rates --------------------------------------------------------------
### ----------------- Create a categorical variable with custom breakpoints ----
wrangled_wfhz <- wrangled_wfhz |>
  mutate(
    est = est * 100,
    raw = raw * 100,
    est = ifelse(est == "NaN", 0, est),
    raw_cat = cut(
      x = raw,
      breaks = c(-Inf, 5.0, 9.9, 14.9, 29.9, Inf),
      labels = c("<5.0%", "5.0-9.9%", "10.0-14.9%", "15.0-29.9%", "≥30.0%"),
      include.lowest = TRUE
    ),
    sebsr_cat = cut(
      x = est,
      breaks = c(-Inf, 5.0, 9.9, 14.9, 29.9, Inf),
      labels = c("<5.0%", "5.0-9.9%", "10.0-14.9%", "15.0-29.9%", "≥30.0%"),
      include.lowest = TRUE
    )
  )

#### ------------------------------------------------------- Plot raw rates ----
ggplot(data = sudan_adm3) +
  geom_sf(
    fill = "white",
    color = "#3F4342",
    size = 0.8
  ) +
  geom_sf(
    data = wrangled_wfhz,
    aes(color = raw_cat)
  ) +
  scale_color_manual(
    values = apply_ipc_colours(indicator = "wfhz", .map_type = "static"),
    name = "Raw rates"
  ) +
    geom_sf_text(
      data = sudan_adm3,
      mapping = aes(label = factor(NAME_3)),
      show.legend = TRUE,
      colour = "#34495E",
      size = 1.8
    ) +
  theme_void() +
  labs(
    title = "Spatial distribution GAM by WFHZ rates by sampling points across in North Darfur",
    subtitle = "Raw rates: cases / total number of children surveyed"
  ) +
  theme(
    plot.title = element_text(size = 11),
    plot.subtitle = element_text(size = 9, colour = "#706E6D")
  )

### ------------------------------------------------------------ Plot SEBSR ----
ggplot(data = sudan_adm3) +
  geom_sf(
    fill = "white",
    color = "#3F4342",
    size = 0.8
  ) +
  geom_sf(
    data = wrangled_wfhz,
    aes(color = sebsr_cat)
  ) +
  scale_color_manual(
    values = apply_ipc_colours(indicator = "wfhz", .map_type = "static"),
    name = "Smoothed rates"
  ) +
    geom_sf_text(
      data = sudan_adm3,
      mapping = aes(label = factor(NAME_3)),
      colour = "#34495E",
      size = 1.8
    ) +
  theme_void() +
  labs(
    title = "Spatial distribution GAM by WFHZ rates by sampling points across in North Darfur",
    subtitle = "Rates smoothed using Spatial Empirical Bayesian"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 9, colour = "#706E6D")
  )

################################ End of workflow ###############################
