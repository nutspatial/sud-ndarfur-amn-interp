################################################################################
#                           INTERPOLATE WITH `{automap}`                       #
################################################################################

## ---- Create a regular grid of area to be interpolated on --------------------

### --------------------------------------- El Fasher and Tawila localities ----
elf_taw_grid <- elf_taw_shp |> 
  st_bbox() |> 
  st_as_stars(dx = 1500) |> 
  st_crop(elf_taw_shp)

