#'
#'
#' Apply IPC Acute Malnutrition Phase colour codes based on either GAM prevalence 
#' by WFHZ or by MUAC
#' 
#' @param .map_type Type of map that the function should apply the IPC colour 
#' code on. A choice between static or interactive.
#' 
#' @param indicator If the prevalence is based on WFHZ or by MUAC.
#' 
#' @returns A map with polygons coloured based on the IPC Acute Malnutrition Phase
#' colours. 
#'
#'

apply_ipc_colours <- function(
  .map_type = c("static", "interactive"),
  indicator = c("wfhz", "muac")) {

# Enforce options in `indicator` and `.map_type` ----
.map_type <- match.arg(.map_type)
indicator <- match.arg(indicator)

if (.map_type == "static") {
  switch(indicator,
    # Color codes for GAM by WFHZ-based thresholds ----
    "wfhz" = {
      x <- c(
        "<5.0%" = "#CDFACD",
        "5.0-9.9%" = "#FAE61E",
        "10.0-14.9%" = "#E67800",
        "15.0-29.9%" = "#C80000",
        "≥30.0%" = "#640000"
      )
      x
    },
    # Color codes for GAM by MUAC-based thresholds ----
    "muac" = {
      x <- c(
        "<5.0%" = "#CDFACD",
        "5.0-9.0%" = "#FAE61E",
        "10.0-14.9%" = "#E67800",
        "≥15.0%" = "#640000"
      )
      x
    }
  )
} else {
  x <- colorRampPalette(
    c("#CDFACD", "#FAE61E", "#E67800", "#C80000", "#640000")
  )(5)
}
x
}
