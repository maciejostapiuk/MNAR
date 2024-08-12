#' @title An internal function for calibration of weights to adjust for nonignorable nonresponse
#'
#' @author Maciej Ostapiuk and Maciej Beręsewicz based on Kott and Chang (2010) and Kott and Liao (2017)
#' @param totals A vector of population totals
#' @param calib_var A matrix of calibration variables
#' @param instr_var A matrix of instrumental variables
#' @param target_var A vector of target variables
#' @param initial_weights A vector of initial weights, derived from sampling design
#'
#' @importFrom sampling gencalib
gencal <- function(totals, nonresponse, calib_var = NULL, target_var = NULL, initial_weights) {
  if (all(dim(nonresponse) == dim(calib_var))) {
    g <- sampling::gencalib(Xs = nonresponse,
                                  Zs = calib_var,
                                  d = initial_weights,
                                  total = totals,
                                  method = "raking")
    return(g)
  } else if (all(dim(nonresponse) < dim(calib_var))) {
    max_iter = 500
    EPS = .Machine$double.eps
    EPS1 = 1e-06

    lambda = as.matrix(rep(0, ncol(Xs)))
    w1 = as.vector(d * exp(Zs %*% lambda * q))
    T = t(Xs)

    Bz <- ginv(t(Xs * d * q) %*% Zs, tol = EPS) %*% t(Zs)

    for (l in 1:max_iter) {
      phi = t(Xs) %*% w1 - total
      phiprim = T %*% Zs %*% Bz

      lambda = lambda - ginv(phiprim, tol = EPS) %*% phi
      w1 = as.vector(d * exp(Zs %*% lambda * q))

      if (any(is.na(w1)) | any(is.infinite(w1)) | any(is.nan(w1))) {
        warning("No convergence")
        g = NULL
        der = g
        l = max_iter
        break
      }

      tr = crossprod(Xs, w1)
      expression = max(abs(tr - total) / total)
      if (any(total == 0)) expression = max(abs(tr - total))

      if (expression < EPS1) break
    }

    if (l == max_iter) {
      warning("No convergence")
      g = NULL
      der = g
    } else {
      g = w1 / d
      der = g
    }
  }
}

