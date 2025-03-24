################################################################################
#                           INTERPOLATE WITH `{automap}`                       #
################################################################################

## ---- El Fasher and Tawila localities ----------------------------------------

### -------- Check the minimum and maximum distance between sampling points ----
elf_taw_dist_max_muac <- max(dist(st_coordinates(elf_taw_data_muac)))
elf_taw_dist_min_muac <- min(dist(st_coordinates(elf_taw_data_muac)))

### ------------------------------------------------------- Fit a variogram ----
elf_taw_variogram_muac <- autofitVariogram(
  formula = est ~ 1, 
  input_data = elf_taw_data_muac,
  model = c("Sph", "Exp", "Gau", "Ste"),
  verbose = FALSE,
  GLS.model = NA,
  start_vals = c(NA, NA, NA),
  miscFitOptions = list(),
  cutoff = elf_taw_dist_max_muac,
  width = elf_taw_dist_min_muac
)

### ----------------------------------------------------------- Interpolate ----
elf_taw_krige_muac <- autoKrige(
  formula = est ~ 1, 
  input_data = elf_taw_data_muac,
  new_data = elf_taw_grid,
  model = "Ste",
  fix.values = c(NA, NA, NA),
  GLS.model = NA,
  start_vals = c(NA, NA, NA),
  miscFitOptions = list(),
  nmin = 3,
  nmax = 6
)

### --------- Bin interpolated GAM prevalence into IPC AMN Phase categories ----
elf_taw_krige_muac$krige_output <- elf_taw_krige_muac$krige_output |> 
  mutate(
    var1.pred.cat = cut(
      x = var1.pred,
      breaks = c(-Inf, 5, 10, 15, Inf),
      labels = c("<5.0%", "5.0-9.9%", "10.0-14.9%", "≥15.0%"),
      include.lowest = TRUE
    )
  )

### -------------------------------- Cross-validation: leave-one-out method ----
elf_taw_cv_muac <- autoKrige.cv(
  formula = est ~ 1, 
  input_data = elf_taw_data_muac,
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
elf_taw_krige_cv_stats_muac <- elf_taw_cv_muac[[1]] |>
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
  data = elf_taw_cv_muac[[1]],
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
  title = "A scatterplot of observed values against predicted",
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
    data = elf_taw_krige_muac[[1]],
    aes(fill = var1.pred.cat, x = x, y = y)
  ) +
    scale_fill_manual(
      values = apply_ipc_colours(indicator = "muac", .map_type = "static"),
      name = "", 
      na.translate = FALSE
  ) +
  geom_sf(
    data = st_cast(elf_taw_shp, "MULTILINESTRING"),
    linewidth = 0.2,
    color = "grey"
  ) +
  labs(
    title = "Surface map of the predicted prevalence of GAM by MUAC"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(colour = "#706E6D", size = 10)
  )

### ------------------------------------------------------- Get areal means ----
elf_taw_areal_mean_muac <- krige(
  formula = est ~ 1,
  locations = elf_taw_data_muac,
  nmin = 2,
  nmax = 2,
  model = elf_taw_variogram_muac[[2]],
  newdata = elf_taw_shp
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
ggplot() +
  geom_sf(
    data = elf_taw_areal_mean_muac,
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
  labs(
    title = "Mean predicted prevalence of GAM by MUAC at locality level",
    fill = "Predicted Values"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 9)
  )

#### Get minimum and maximum predicted prevalence values by locality -----
elf_taw_interp_min_max_muac <- elf_taw_krige_muac[[1]] |>
  st_as_sf() |>
  st_join(elf_taw_shp, left = FALSE) |> # each grid cell to a polygon
  group_by(NAME_3) |>
  summarise(
    min_value = min(var1.pred, na.rm = TRUE),
    max_value = max(var1.pred, na.rm = TRUE)
  )

#### Compare mean predicted prevalence against original survey results -----
elf_taw_pred_vs_original_muac <- smart_muac |> 
  filter(locality %in% c("El Fasher", "Tawila")) |> 
  mw_estimate_prevalence_muac(
    wt = NULL,
    edema = edema,
    .by = locality
  ) |>
  select(locality, gam_p) |>
  arrange(factor(locality)) |>
  mutate(
    survey = gam_p * 100,
    interp = elf_taw_areal_mean_muac[["var1.pred"]],
    bias = interp - survey,
    min_interp = elf_taw_interp_min_max_muac[[2]],
    max_interp = elf_taw_interp_min_max_muac[[3]]
  ) |>
  select(-gam_p)

# ############################## End of Workflow ###############################
