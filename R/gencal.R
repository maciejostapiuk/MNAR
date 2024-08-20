#' Importing
#' @importFrom sampling gencalib
#' @importFrom MASS ginv
#'
#' @title Generalized calibration function based on sampling::gencalib
#'
#' @author Maciej Ostapiuk and Maciej Beręsewicz based on Kott and Chang (2010) and Kott and Liao (2017)
#' @param Xs A matrix of calibration variables
#' @param Zs A matrix of instrumental variables
#' @param d A vector of initial weights, derived from sampling design
#' @param totals A vector of population totals (including totals for both Xs and Zs)
#' @param method TBA
#' @param eps TBA
#' @param maxit TBA
#' @param tol TBA
#'
#'
#' @export
gencal <- function(Xs, Zs, d, pop_totals, method="raking", eps, maxit, tol) {
  if (ncol(Zs) == ncol(Xs)) {
    totals <- matrix(t(as.data.frame(pop_totals))[, c("N",intersect(colnames(Xs), colnames(t(as.data.frame(pop_totals))))), drop=FALSE])
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
    Zs_totals <- matrix(t(as.data.frame(pop_totals))[, c("N",intersect(colnames(Zs), colnames(t(as.data.frame(pop_totals))))), drop=FALSE])
    totals <- matrix(t(as.data.frame(pop_totals))[, c("N",intersect(colnames(Xs), colnames(t(as.data.frame(pop_totals))))), drop=FALSE])


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






