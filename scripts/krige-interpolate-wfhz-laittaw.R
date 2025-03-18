################################################################################
#                           INTERPOLATE WITH `{automap}`                       #
################################################################################

## ---- El Lait and El Tawisha localities --------------------------------------

### ------------------- Create a regular grid of area to be interpolated on ----
ellait_eltaw_grid <- ellait_eltaw_shp_wfhz |> 
  st_bbox() |> 
  st_as_stars(dx = 1200) |> 
  st_crop(ellait_eltaw_shp_wfhz)

### -------- Check the minimum and maximum distance between sampling points ----
ellait_eltaw_dist_max <- max(dist(st_coordinates(ellait_eltaw_data_wfhz)))
ellait_eltaw_dist_min <- min(dist(st_coordinates(ellait_eltaw_data_wfhz)))

### ------------------------------------------------------- Fit a variogram ----
ellait_eltaw_variogram_wfhz <- autofitVariogram(
  formula = est ~ 1, 
  input_data = ellait_eltaw_data_wfhz,
  model = c("Sph", "Exp", "Gau", "Ste"),
  verbose = FALSE,
  GLS.model = NA,
  start_vals = c(NA, NA, NA),
  miscFitOptions = list(),
  cutoff = ellait_eltaw_dist_max,
  width = ellait_eltaw_dist_min
)

### ----------------------------------------------------------- Interpolate ----
ellait_eltaw_krige_wfhz <- autoKrige(
  formula = est ~ 1, 
  input_data = ellait_eltaw_data_wfhz,
  new_data = ellait_eltaw_grid,
  model = "Ste",
  fix.values = c(NA, NA, NA),
  GLS.model = NA,
  start_vals = c(NA, NA, NA),
  miscFitOptions = list(),
  nmin = 2,
  nmax = 2
)

### -------------------------------- Cross-validation: leave-one-out method ----
ellait_eltaw_cv_wfhz <- autoKrige.cv(
  formula = est ~ 1, 
  input_data = ellait_eltaw_data_wfhz,
  model = "Ste",
  fix.values = c(NA, NA, NA),
  GLS.model = NA,
  start_vals = c(NA, NA, NA),
  miscFitOptions = list(),
  nmin = 2,
  nmax = 2, 
  verbose = c(FALSE, TRUE)
)

#### Cross-validation statistics ----
ellait_eltaw_cv_stats_wfhz <- ellait_eltaw_cv_wfhz[[1]] |>
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
  data = ellait_eltaw_cv_wfhz[[1]],
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
    data = ellait_eltaw_krige_wfhz[[1]],
    aes(fill = var1.pred, x = x, y = y)
  ) +
  scale_fill_gradientn(
    colors = apply_ipc_colours(),
    na.value = "transparent",
    name = "GAM Prevalence (%)",
    limits = c(0, 30),
    breaks = c(0, 5, 10, 15, 30),
    labels = c("<5.0", "5.0-9.9", "10.0-14.9", "15.0-29.9", "≥30.0"),
    values = scales::rescale(c(0, 5, 10, 15, 30), from = c(0, 30))
  ) +
  geom_sf(
    data = st_cast(ellait_eltaw_shp_wfhz, "MULTILINESTRING"),
    linewidth = 0.2,
    color = "grey"
  ) +
  labs(
    title = "A surface map of the predicted prevalence of GAM by WHZ"
  ) +
  theme(
    plot.title = element_text(colour = "#706E6D", size = 10)
  ) +
  theme_void()

### ------------------------------------------------------- Get areal means ----
# elf_taw_areal_mean_wfhz <- gstat::krige(
#   formula = est ~ 1,
#   locations = elf_taw_data_wfhz,
#   nmin = 3,
#   nmax = 4,
#   model = elf_taw_variogram_wfhz[[2]],
#   newdata = elf_taw_shp_wfhz
# )

# #### Cloropleth map of the mean predicted prevalence at locality level ----
# ggplot() +
#   geom_sf(
#     data = elf_taw_areal_mean_wfhz,
#     aes(fill = var1.pred),
#     color = "black",
#     size = 0.2
#   ) +
#   scale_fill_gradientn(
#     colours = apply_ipc_colours(),
#     na.value = "transparent",
#     name = "GAM Prevalence (%)",
#     limits = c(0, 30),
#     breaks = c(0, 5, 10, 15, 30),
#     labels = c("<5.0", "5.0-9.9", "10.0-14.9", "15.0-29.9", "≥30.0"),
#     values = scales::rescale(c(0, 5, 10, 15, 30), from = c(0, 30))
#   ) +
#   geom_sf(
#     data = elf_taw_shp_wfhz,
#     fill = NA,
#     color = "#F2F3F4",
#     size = 0.8
#   ) +
#   geom_sf_text(
#     data = elf_taw_shp_wfhz,
#     mapping = aes(label = factor(NAME_3)),
#     show.legend = TRUE,
#     color = "#34495E",
#     size = 3,
#   ) +
#   labs(
#     title = "Mean predicted prevalence of GAM by WFHZ at locality level",
#     fill = "Predicted Values"
#   ) +
#   theme(
#     plot.title = element_text(size = 9)
#   ) +
#   theme_void()

# #### Get minimum and maximum predicted prevalence values by locality -----
# elf_taw_interp_min_max_wfhz <- elf_taw_krige_wfhz[[1]] |>
#   st_as_sf() |>
#   st_join(elf_taw_shp_wfhz, left = FALSE) |> # each grid cell to a polygon
#   group_by(NAME_3) |>
#   summarise(
#     min_value = min(var1.pred, na.rm = TRUE),
#     max_value = max(var1.pred, na.rm = TRUE)
#   )

# #### Compare mean predicted prevalence against original survey results -----
# elf_taw_pred_vs_original_wfhz <- smart_wfhz |> 
#   filter(locality %in% c("El Fasher", "Tawila")) |> 
#   mw_estimate_prevalence_wfhz(
#     wt = NULL,
#     edema = edema,
#     .by = locality
#   ) |>
#   select(locality, gam_p) |>
#   arrange(factor(locality)) |>
#   mutate(
#     survey = gam_p * 100,
#     interp = elf_taw_areal_mean_wfhz[["var1.pred"]],
#     bias = interp - survey,
#     min_interp = elf_taw_interp_min_max_wfhz[[2]],
#     max_interp = elf_taw_interp_min_max_wfhz[[3]]
#   ) |>
#   select(-gam_p)

# ############################## End of Workflow #################################