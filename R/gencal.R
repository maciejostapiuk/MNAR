#' @title An internal function for calibration of weights to adjust for nonignorable nonresponse
#'
#' @author Maciej Ostapiuk and Maciej Beręsewicz based on Kott and Chang (2010) and Kott and Liao (2017)
#' @param totals A vector of population totals
#' @param nonresponse A matrix of calibration variables
#' @param calib_var A matrix of instrumental variables
#' @param initial_weights A vector of initial weights, derived from sampling design
#' @importFrom sampling gencalib
#' @importFrom MASS ginv
gencal <- function(totals, nonresponse, calib_var, initial_weights) {
  if (all(dim(nonresponse) == dim(calib_var))) {
    g <- sampling::gencalib(Xs = nonresponse,
                            Zs = calib_var,
                            d = initial_weights,
                            total = totals,
                            method = "raking")
    return(g)
  } else if (all(dim(nonresponse) <= dim(calib_var))) {
    d <- initial_weights
    q <- rep(1, length(initial_weights))
    EPS <- .Machine$double.eps
    XtWX <- t(nonresponse * d * q) %*% calib_var

    ginv_matrix <- MASS::ginv(XtWX, tol = EPS)



    ztilde_k <- calib_var %*% ginv_matrix


    g <- sampling::gencalib(Xs = nonresponse,
                            Zs = ztilde_k,
                            d = initial_weights,
                            total = totals,
                            method = "raking")
    return(g)
  } else {
    stop("Dimensions of nonresponse and calib_var are not compatible.")
  }
}






