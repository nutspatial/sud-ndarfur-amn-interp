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
elf_taw_variogram <- autofitVariogram(
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
elf_taw_krige <- autoKrige(
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
