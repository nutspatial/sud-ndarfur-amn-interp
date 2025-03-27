################################################################################
#                           INTERPOLATE WITH `{automap}`                       #
################################################################################

## ---- El Lait and El Tawisha localities --------------------------------------

### -------- Check the minimum and maximum distance between sampling points ----
ellait_eltaw_dist_max_muac <- max(dist(st_coordinates(ellait_eltaw_data_muac)))
ellait_eltaw_dist_min_muac <- min(dist(st_coordinates(ellait_eltaw_data_muac)))

### ------------------------------------------------------- Fit a variogram ----
ellait_eltaw_variogram_muac <- autofitVariogram(
  formula = est ~ 1, 
  input_data = ellait_eltaw_data_muac,
  model = c("Sph", "Exp", "Gau", "Ste"),
  verbose = FALSE,
  GLS.model = NA,
  start_vals = c(NA, NA, NA),
  miscFitOptions = list(),
  cutoff = ellait_eltaw_dist_max_muac,
  width = ellait_eltaw_dist_min_muac
)

### ----------------------------------------------------------- Interpolate ----
ellait_eltaw_krige_muac <- autoKrige(
  formula = est ~ 1, 
  input_data = ellait_eltaw_data_muac,
  new_data = ellait_eltaw_grid,
  model = "Ste",
  fix.values = c(NA, NA, NA),
  GLS.model = NA,
  start_vals = c(NA, NA, NA),
  miscFitOptions = list(),
  nmin = 3,
  nmax = 6
)

### --------- Bin interpolated GAM prevalence into IPC AMN Phase categories ----
ellait_eltaw_krige_muac$krige_output <- ellait_eltaw_krige_muac$krige_output |> 
  mutate(
    var1.pred.cat = cut(
      x = var1.pred,
      breaks = c(-Inf, 5, 10, 15, Inf),
      labels = c("<5.0%", "5.0-9.9%", "10.0-14.9%", "≥15.0%"),
      include.lowest = TRUE
    )
  )

### -------------------------------- Cross-validation: leave-one-out method ----
ellait_eltaw_cv_muac <- autoKrige.cv(
  formula = est ~ 1, 
  input_data = ellait_eltaw_data_muac,
  model = "Ste",
  fix.values = c(NA, NA, NA),
  GLS.model = NA,
  start_vals = c(NA, NA, NA),
  miscFitOptions = list(),
  nmin = 3,
  nmax = 6, 
  verbose = c(FALSE, TRUE)
)

#### Cross-validation statistics ----
ellait_eltaw_cv_stats_muac <- ellait_eltaw_cv_muac[[1]] |>
  as_tibble() |>
  summarise(
    mean_error = mean(residual, na.rm = TRUE), ## be as close to zero as possible
    MSPE = mean(residual^2, na.rm = TRUE), ## Ideally small
    MSNR = mean(zscore^2, na.rm = TRUE), ## Mean squared normalized error, should be close to 1
    r2_obspred = cor(observed, observed - residual, use = "complete.obs"), ## Ideally 1
    r2_predobs = cor(observed - residual, residual, use = "complete.obs") ## Ideally should be close to 0
  )

#### Plot predicted ~ observed ----
laittaw_scatter_muac <- ggplot(
  data = ellait_eltaw_cv_muac[[1]],
  aes(x = var1.pred, y = observed)
) +
geom_point(size = 1.2, color = "#BA4A00") +
geom_abline(
  intercept = 0,
  slope = 1,
  color = "#566573",
  linewidth = 0.3
) +
geom_smooth(
  method = "lm",
  color = "blue",
  linewidth = 0.9,
  se = FALSE
) +
theme_minimal() +
labs(
  x = "Predicted GAM rates (%)",
  y = "Observed GAM rates (%)"
)

### ----------------------- Visualize interpolated results on a map surface ----
laittaw_surface_muac <- ggplot() +
  geom_stars(
    data = ellait_eltaw_krige_muac[[1]],
    aes(fill = var1.pred.cat, x = x, y = y)
  ) +
    scale_fill_manual(
      values = apply_ipc_colours(indicator = "muac", .map_type = "static"),
      name = "", 
      na.translate = FALSE
  ) +
  geom_sf(
    data = st_cast(ellait_eltaw_shp, "MULTILINESTRING"),
    linewidth = 0.2,
    color = "grey"
  ) +
  theme_void()

### ------------------------------------------------------- Get areal means ----
ellait_eltaw_areal_mean_muac <- krige(
  formula = est ~ 1,
  locations = ellait_eltaw_data_muac,
  nmin = 2,
  nmax = 2,
  model = ellait_eltaw_variogram_muac[[2]],
  newdata = ellait_eltaw_shp
) |> 
  mutate(
    var1.pred.cat = cut(
      x = var1.pred,
      breaks = c(-Inf, 5, 10, 15, Inf),
      labels = c("<5.0%", "5.0-9.9%", "10.0-14.9%", "≥15.0%"),
      include.lowest = TRUE
    )
  )

#### Cloropleth map of the mean predicted prevalence at locality level ----
laittaw_choropleth_muac <- ggplot() +
  geom_sf(
    data = ellait_eltaw_areal_mean_muac,
    aes(fill = var1.pred.cat),
    color = "black",
    size = 0.2
  ) +
    scale_fill_manual(
      values = apply_ipc_colours(indicator = "muac", .map_type = "static"),
      name = "", 
      na.translate = FALSE
  ) +
  geom_sf(
    data = ellait_eltaw_shp,
    fill = NA,
    color = "#F2F3F4",
    size = 0.8
  ) +
  geom_sf_text(
    data = ellait_eltaw_shp,
    mapping = aes(label = factor(NAME_3)),
    show.legend = FALSE,
    color = "#34495E",
    size = 3,
  ) +
  theme_void()

#### Get minimum and maximum predicted prevalence values by locality -----
ellait_eltaw_interp_min_max_muac <- ellait_eltaw_krige_muac[[1]] |>
  st_as_sf() |>
  st_join(ellait_eltaw_shp, left = FALSE) |> # each grid cell to a polygon
  group_by(NAME_3) |>
  summarise(
    min_value = min(var1.pred, na.rm = TRUE),
    max_value = max(var1.pred, na.rm = TRUE), 
    median_value = median(var1.pred, na.rm = TRUE)
  )

#### Compare mean predicted prevalence against original survey results -----
ellait_eltaw_pred_vs_original_muac <- smart_muac |> 
  filter(locality %in% c("El Lait", "El Taweisha")) |> 
  mw_estimate_prevalence_muac(
    wt = NULL,
    edema = edema,
    .by = locality
  ) |>
  select(locality, gam_p) |>
  arrange(factor(locality)) |>
  mutate(
    survey = gam_p * 100,
    interp = ellait_eltaw_areal_mean_muac[["var1.pred"]],
    bias = interp - survey,
    min_interp = ellait_eltaw_interp_min_max_muac[[2]],
    max_interp = ellait_eltaw_interp_min_max_muac[[3]], 
    median_interp = ellait_eltaw_interp_min_max_muac[[4]]
  ) |>
  select(-gam_p)

# ############################## End of Workflow ###############################
