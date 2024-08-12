#' @importFrom sampling gencalib
mnar <- function(totals, nonresponse, calib_var = NULL, target_var, initial_weights, method) {
  if (method == "gencalib") {
    weights <- gencal(totals, nonresponse, calib_var, target_var, initial_weights)
    return(weights)
  } else {
    stop("Unknown method specified")
  }
}


