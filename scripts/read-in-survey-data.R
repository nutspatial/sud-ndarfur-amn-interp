################################################################################
#                                 READ IN DATASET                              #
################################################################################


## ---- Read in SMART survey dataset -------------------------------------------

### --------------------------------------------- Extract Excel sheet names ----
file_sheets <- excel_sheets("data-raw/el-fasher-smart-survey-data.xlsx")

### ------------------- Read in household roster to extract GPS coordinates ----
hh <- read_xlsx(
  path = "data-raw/el-fasher-smart-survey-data.xlsx",
  sheet = file_sheets[2]
) |> 
  select(
    end,
    state, 
    locality = Locality, 
    cluster = "Cluster number",
    index = `_index`, 
    gps = "Please take a GPS reading"
  ) |> 
  separate_wider_delim(
    cols = gps,
    delim = " ", 
    names = c("latitude", "longitude", "altitude", "accuracy")
  ) |> 
  select(-altitude) |> 
  relocate(index, .after = accuracy)


### -------------------------------------------------- Read in child roster ----
child <- read_xlsx(
  path = "data-raw/el-fasher-smart-survey-data.xlsx",
  sheet = file_sheets[5]
) |> 
  select(
    sex = CHSEX, 
    dob = "${child_name}'s date of birth:", 
    estimated_dob, 
    weight = "Weight in KG of ${child_name} :",
    height = "Height in CM of ${child_name}:",
    muac = "MUAC in MM of ${child_name}",
    edema = "Does ${child_name}  have bilateral oedema, that is swelling with pitting oedema in both feet?",
    index = "_parent_index"
  )

### ------------------------------ Join household roster with child dataset ----
smart_survey <- left_join(
  x = hh, 
  y = child, 
  by = "index"
) |> 
  select(-index)

################################ End of workflow ###############################