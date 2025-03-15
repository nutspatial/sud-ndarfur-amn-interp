################################################################################
#                     WRANGLE NON-SPATIAL ATTRIBUTES - WFHZ                    #
################################################################################

## ---- Exclude rows with missing values ---------------------------------------
data <- filter(
  .data = data, 
  !is.na(longitude)
)

## ---- Wrangle weight-for-height data -----------------------------------------
smart_wfhz <- data |> 
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
  )


## ---- Wrangle MUAC data ------------------------------------------------------
smart_muac <- data |> 
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
  select(locality, cluster, latitude, longitude, gam) |> 
  filter(!is.na(latitude)) |> 
  st_as_sf(
    coords = c("latitude", "longitude"),
    dim = "XY"
  ) |> 
  st_set_crs(value = "EPSG:4326") |> 
  st_transform(crs = "EPSG:20135")
################################ End of workflow ###############################
