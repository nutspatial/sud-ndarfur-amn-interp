y <- st_bbox(x) |> 
  st_as_stars(dx = 1000) |> 
  st_crop(x)

i <- idw(rate~1, w, y)

ggplot() + 
  geom_stars(data = i, 
  aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
geom_sf(data = st_cast(x, "MULTILINESTRING")) + 
geom_sf(data = w)
