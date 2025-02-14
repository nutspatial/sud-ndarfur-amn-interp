################################################################################
#                     WRANGLE NON-SPATIAL ATTRIBUTES - WFHZ                    #
################################################################################

## ---- Exclude rows with missing values ---------------------------------------
smart_survey <- filter(
  .data = smart_survey, 
  !is.na(weight)
)

## ---- Wrangle weight-for-height data -----------------------------------------
smart_wfhz <- smart_survey |> 
  mutate(
    age = NA_real_,
    end = date(end),
    dob = coalesce(date(dob), estimated_dob = date(estimated_dob)),
    edema = ifelse(edema == "No", "n", "y")
  ) |> 
  select(-estimated_dob) |> 
  mw_wrangle_age(
    dos = end,
    dob = dob,
    age = age, 
    .decimals = 2
  ) |> 
  mw_wrangle_wfhz(
    sex = sex, 
    weight = weight, 
    height = height, 
    .recode_sex = TRUE, 
    .decimals = 3
  ) |> 
  define_wasting(
    zscores = wfhz,
    edema = edema,
    .by = "zscores"
  ) |> 
  filter(!flag_wfhz == 1) |> 
  select(cluster, latitude, longitude, gam) |> 
  filter(!is.na(latitude)) |> 
  st_as_sf(
    coords = c("latitude", "longitude"),
    dim = "XY"
  ) |> 
  st_set_crs(value = 4326) |> 
  st_transform(crs = 29636)

## ---- Wrangle MUAC data ------------------------------------------------------
smart_muac <- smart_survey |> 
  mutate(
    age = NA_real_,
    end = date(end),
    dob = coalesce(date(dob), estimated_dob = date(estimated_dob)),
    edema = ifelse(edema == "No", "n", "y")
  ) |> 
  select(-estimated_dob) |> 
  mw_wrangle_age(
    dos = end,
    dob = dob,
    age = age, 
    .decimals = 2
  ) |> 
  mw_wrangle_muac(
    sex = sex,
    muac = muac,
    age = age,
    .recode_sex = TRUE,
    .recode_muac = TRUE, 
    .to = "cm",
    .decimals = 3
  ) |> 
  mutate(muac = recode_muac(muac, .to = "mm")) |> 
  define_wasting(
    muac = muac,
    edema = edema, 
    .by = "muac"
  ) |> 
  filter(!flag_mfaz == 1) |> 
  select(cluster, latitude, longitude, gam) |> 
  filter(!is.na(latitude)) |> 
  st_as_sf(
    coords = c("latitude", "longitude"),
    dim = "XY"
  ) |> 
  st_set_crs(value = 4326) |> 
  st_transform(crs = 29636)

################################ End of workflow ###############################
