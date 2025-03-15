#'
#'
#'  Utility function to apply IPC Acute Malnutriton color codes based indicator
#'
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
        "<0.05" = "#CDFACD",
        "0.05-0.09" = "#FAE61E",
        "0.10-0.149" = "#E67800",
        "≥0.15" = "#640000"
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
