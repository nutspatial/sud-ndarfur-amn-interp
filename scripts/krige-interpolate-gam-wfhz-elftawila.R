################################################################################
#                           INTERPOLATE WITH `{automap}`                       #
################################################################################

## ---- El Fasher and Tawila localities ----------------------------------------

### ------------------- Create a regular grid of area to be interpolated on ----
elf_taw_grid <- elf_taw_shp |> 
  st_bbox() |> 
  st_as_stars(dx = 500) |> 
  st_crop(elf_taw_shp)

### -------- Check the minimum and maximum distance between sampling points ----
elf_taw_dist_max <- max(dist(st_coordinates(elf_taw_data_wfhz)))
elf_taw_dist_min <- min(dist(st_coordinates(elf_taw_data_wfhz)))

### ------------------------------------------------------- Fit a variogram ----
elf_taw_variogram_wfhz <- autofitVariogram(
  formula = est ~ 1, 
  input_data = elf_taw_data_wfhz,
  model = c("Sph", "Exp", "Gau", "Ste"),
  verbose = FALSE,
  GLS.model = NA,
  start_vals = c(NA, NA, NA),
  miscFitOptions = list(),
  cutoff = elf_taw_dist_max,
  width = elf_taw_dist_min
)

### ----------------------------------------------------------- Interpolate ----
elf_taw_krige_wfhz <- autoKrige(
  formula = est ~ 1, 
  input_data = elf_taw_data_wfhz,
  new_data = elf_taw_grid,
  model = "Ste",
  fix.values = c(NA, NA, NA),
  GLS.model = NA,
  start_vals = c(NA, NA, NA),
  miscFitOptions = list(),
  nmin = 3,
  nmax = 4
)

### --------- Bin interpolated GAM prevalence into IPC AMN Phase categories ----
elf_taw_krige_wfhz$krige_output <- elf_taw_krige_wfhz$krige_output |> 
  mutate(
    var1.pred.cat = cut(
      x = var1.pred,
      breaks = c(-Inf, 5.0, 9.9, 14.9, 29.9, Inf),
      labels = c("<5.0%", "5.0-9.9%", "10.0-14.9%", "15.0-29.9%", "≥30.0%"),
      include.lowest = TRUE
    )
  )


### -------------------------------- Cross-validation: leave-one-out method ----
elf_taw_cv_wfhz <- autoKrige.cv(
  formula = est ~ 1, 
  input_data = elf_taw_data_wfhz,
  model = "Ste",
  fix.values = c(NA, NA, NA),
  GLS.model = NA,
  start_vals = c(NA, NA, NA),
  miscFitOptions = list(),
  nmin = 3,
  nmax = 4, 
  verbose = c(FALSE, TRUE)
)

#### Cross-validation statistics ----
elf_taw_cv_stats_wfhz <- elf_taw_cv_wfhz[[1]] |>
  as_tibble() |>
  summarise(
    mean_error = mean(residual, na.rm = TRUE), ## be as close to zero as possible
    MSPE = mean(residual^2, na.rm = TRUE), ## Ideally small
    MSNR = mean(zscore^2, na.rm = TRUE), ## Mean squared normalized error, should be close to 1
    r2_obspred = cor(observed, observed - residual, use = "complete.obs"), ## Ideally 1
    r2_predobs = cor(observed - residual, residual, use = "complete.obs") ## Ideally should be close to 0
  )

#### Plot predicted ~ observed ----
ggplot(
  data = elf_taw_cv_wfhz[[1]],
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
  title = "Scatterplot of observed values against predicted in the model cross-validation",
  subtitle = paste("Perfect correlation between observed and predicted values: R\u00B2 =", round(elf_taw_cv_stats_wfhz$r2_obspred, 3)),
  x = "Predicted",
  y = "Observed"
) +
theme(
  plot.title = element_text(size = 11),
  plot.subtitle = element_text(size = 9, colour = "#706E6D")
)

### ----------------------- Visualize interpolated results on a map surface ----
ggplot() +
  geom_stars(
    data = elf_taw_krige_wfhz[[1]],
    aes(fill = var1.pred.cat, x = x, y = y)
  ) +
  scale_fill_manual(
    values = apply_ipc_colours(),
    name = "", 
    na.translate = FALSE
  ) +
  geom_sf(
    data = st_cast(elf_taw_shp, "MULTILINESTRING"),
    linewidth = 0.2,
    color = "grey"
  ) +
  labs(
    title = "Surface map of the predicted prevalence of GAM by WFHZ"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(colour = "#706E6D", size = 10)
  )

### ------------------------------------------------------- Get areal means ----
elf_taw_areal_mean_wfhz <- krige(
  formula = est ~ 1,
  locations = elf_taw_data_wfhz,
  nmin = 3,
  nmax = 4,
  model = elf_taw_variogram_wfhz[[2]],
  newdata = elf_taw_shp
) |> 
  mutate(
    var1.pred.cat = cut(
      x = var1.pred,
      breaks = c(-Inf, 5.0, 9.9, 14.9, 29.9, Inf),
      labels = c("<5.0%", "5.0-9.9%", "10.0-14.9%", "15.0-29.9%", "≥30.0%"),
      include.lowest = TRUE
    )
  )

#### Cloropleth map of the mean predicted prevalence at locality level ----
ggplot() +
  geom_sf(
    data = elf_taw_areal_mean_wfhz,
    aes(fill = var1.pred.cat),
    color = "black",
    size = 0.2
  ) +
  scale_fill_manual(
    values = apply_ipc_colours(),
    name = " ",
    na.translate = FALSE
  ) +
  geom_sf(
    data = elf_taw_shp,
    fill = NA,
    color = "#F2F3F4",
    size = 0.8
  ) +
  geom_sf_text(
    data = elf_taw_shp,
    mapping = aes(label = factor(NAME_3)),
    show.legend = FALSE,
    color = "#34495E",
    size = 3,
  ) +
  theme_void() +
  labs(
    title = "Mean predicted prevalence of GAM by WFHZ at locality level",
    fill = "Predicted Values"
  ) +
  theme(
    plot.title = element_text(size = 9, colour = "#706E6D")
  )

#### Get minimum and maximum predicted prevalence values by locality -----
elf_taw_interp_min_max_wfhz <- elf_taw_krige_wfhz[[1]] |>
  st_as_sf() |>
  st_join(elf_taw_shp, left = FALSE) |> # each grid cell to a polygon
  group_by(NAME_3) |>
  summarise(
    min_value = min(var1.pred, na.rm = TRUE),
    max_value = max(var1.pred, na.rm = TRUE)
  )

#### Compare mean predicted prevalence against original survey results -----
elf_taw_pred_vs_original_wfhz <- smart_wfhz |> 
  filter(locality %in% c("El Fasher", "Tawila")) |> 
  mw_estimate_prevalence_wfhz(
    wt = NULL,
    edema = edema,
    .by = locality
  ) |>
  select(locality, gam_p) |>
  arrange(factor(locality)) |>
  mutate(
    survey = gam_p * 100,
    interp = elf_taw_areal_mean_wfhz[["var1.pred"]],
    bias = interp - survey,
    min_interp = elf_taw_interp_min_max_wfhz[[2]],
    max_interp = elf_taw_interp_min_max_wfhz[[3]]
  ) |>
  select(-gam_p)

############################## End of Workflow #################################