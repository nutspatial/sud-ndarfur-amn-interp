################################################################################
#            WORKFLOW TO WRANGLE DATA THROUGH SPATIAL ATTRIBUTES               #
#
# A SMART survey is a cluster-based survey. Observations are measured at household
# level; this includes the geocoordinates that are collected at this level as well.
# The Al Fasher SMART survey sampled 503 households. This means that there are 
# 503 geocoordinates XY, one for each household. If a household has 5 children, 
# the geocoordinate is repeated on 5 different rows binded to the same household 
# ID.
# 
# In order to feed this input data to the interpolation model, the model requires
# that the data be aggregated at a cluster level. This includes both the point 
# estimates of acute malnutrition and the geocoordinates. The workflow below does
# that. It summarizes 503 household-level geocoordinates into 37 cluster-level  
# geocoordinates by calculating the mean centroid (average latitude and longitude)
# for each cluste. 

################################################################################

## ---- Summarise XY geocoordinates and cases at cluster level -----------------

w <- smart_wfhz |> 
  summarise(
    cases = sum(gam, na.rm = TRUE), 
    pop = n(),
    rate = cases / pop, 
    geometry = st_centroid(st_union(geometry)),
    .by = cluster
  )

### --------------------------------------------------------- Plot the data ----
ggplot(data = al_fasher) +
  geom_sf(fill = "white") +
  geom_sf(
    data = w, 
    aes(color = rate)
  ) +
  scale_color_viridis_c(option = "viridis", direction = -1) +
  theme_void()

#### Plot the county where data was collected ----
al_fasher |> 
  filter(NAME_3 == "El Fashir") |> 
ggplot() +
  geom_sf(fill = "white") +
  geom_sf(
    data = w, 
    aes(color = rate)
  ) +
  scale_color_viridis_c(option = "viridis", direction = -1) +
  theme_void()


## ---- Summarise XY geocoordinates and cases at cluster level -----------------
m <- smart_muac |> 
  summarise(
    cases = sum(gam, na.rm = TRUE), 
    pop = n(), 
    rate = cases / pop,
    geometry = st_centroid(st_union(geometry)),
    .by = cluster
  )

### --------------------------------------------------------- Plot the data ----
ggplot(data = al_fasher) +
  geom_sf(fill = "white") +
  geom_sf(
    data = m, 
    aes(color = rate)
  ) +
  scale_color_viridis_c(option = "viridis", direction = -1) +
  theme_void()

#### Plot the county where data was collected ----
al_fasher |> 
  filter(NAME_3 == "El Fashir") |> 
ggplot() +
  geom_sf(fill = "white") +
  geom_sf(
    data = m, 
    aes(color = rate)
  ) +
  scale_color_viridis_c(option = "viridis", direction = -1) +
  theme_void()

################################ End of workflow ###############################
