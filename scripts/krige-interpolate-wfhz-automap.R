################################################################################
#                           INTERPOLATE WITH `{automap}`                       #
################################################################################

## ---- El Fasher and Tawila localities ----------------------------------------

### ------------------- Create a regular grid of area to be interpolated on ----
elf_taw_grid <- elf_taw_shp |> 
  st_bbox() |> 
  st_as_stars(dx = 1500) |> 
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
  maxdist = 9000, 
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
  title = "A scatterplot of observed values against predicted",
  x = "Predicted",
  y = "Observed"
) +
theme(
  plot.title = element_text(size = 11),
  plot.subtitle = element_text(size = 9, colour = "#706E6D")
)
