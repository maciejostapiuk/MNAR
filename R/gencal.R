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
    A_0_t <- MASS::ginv(t(Xs) %*% Xs) %*% (t(Xs) %*% Zs)
    x_tilde <- Xs %*% A_0_t
    new_totals <-  colSums(x_tilde * d)
    g <- sampling::gencalib(Xs = x_tilde,
                            Zs = Zs,
                            d = d,
                            total = new_totals,
                            method = method,
                            max_iter = maxit)
  }
  return(g)
}






