library(sampling)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("../methods/gencalib.R")


mnar <- function(totals, nonresponse, instrumental_variables = NULL, target_variables, initial_weights, method) {
  if (method == "gencalib") {
    weights <- gencal(totals, nonresponse, instrumental_variables, target_variables, initial_weights)
    return(weights)
  } else {
    stop("Unknown method specified")
  }
}
