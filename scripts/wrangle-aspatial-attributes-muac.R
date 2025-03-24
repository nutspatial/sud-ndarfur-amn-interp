################################################################################
#                     WRANGLE NON-SPATIAL ATTRIBUTES - MUAC                    #
################################################################################


## ---- Wrangle MUAC data ------------------------------------------------------
smart_muac <- input_data |> 
  mutate(
    age = NA_real_,
    end = date(end),
    dob = coalesce(date(dob), estimated_dob = date(estimated_dob)),
    edema = case_when(edema == "No" ~ "n", edema == "Yes" ~ "y", .default = edema)
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
  )

################################ End of workflow ###############################