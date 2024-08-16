#' Importing
#' @importFrom sampling gencalib
#' @importFrom MASS ginv
#'
#' @title Generalized calibration function based on sampling::gencalib
#'
#' @author Maciej Ostapiuk and Maciej Beręsewicz based on Kott and Chang (2010) and Kott and Liao (2017)
#' @param Xs A vector of population totals
#' @param Zs A matrix of calibration variables
#' @param d A matrix of instrumental variables
#' @param totals A vector of initial weights, derived from sampling design
#' @param method TBA
#' @param eps TBA
#' @param maxit TBA
#' @param tol TBA
#'
#'
#' @export
gencal <- function(Xs, Zs, d, totals, method="raking", eps, maxit, tol) {
  if (ncol(Zs) == ncol(Xs)) {
    g <- sampling::gencalib(Xs = Xs,
                            Zs = Zs,
                            d = d,
                            total = totals,
                            method = method,
                            max_iter = maxit)

  } else if (ncol(Zs) < ncol(Xs)) {
    ## reduction of Xs's dimension
    A_0_t <- MASS::ginv(t(Xs) %*% Xs) %*% (t(Xs) %*% Zs)
    x_tilde <- Xs %*% A_0_t

    ## analogous for totals
    totals <- matrix(totals)
    Zs_totals <- matrix(Zs_totals)

    A_0_t_totals <- totals %*% t(Zs_totals) * as.numeric(1/(t(totals) %*% totals))
    totals_tilde <- t(t(totals) %*% A_0_t_totals)

    g <- sampling::gencalib(Xs = x_tilde,
                            Zs = Zs,
                            d = d,
                            total = totals_tilde,
                            method = method,
                            max_iter = maxit)
  }
  return(g)
}






