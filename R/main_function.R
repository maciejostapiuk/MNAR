library(sampling)
library(devtools)
library(rstudioapi)

source("R/gencalib.R")


mnar <- function(totals, calib_var, instr_var = NULL, target_var, initial_weights, method) {
  if (method == "gencalib") {
    weights <- gencal(totals, calib_var, instr_var, target_var, initial_weights)
    return(weights)
  } else {
    stop("Unknown method specified")
  }
}
