#' Imports for the function
#' @import mathjaxr
#' @importFrom sampling gencalib
#' @importFrom MASS ginv
#' @title The main function for the not missing at random non-response
#' @author Maciej Ostapiuk, Maciej Beręsewicz
#'
#' @description Here is the part where the description will go
#'
#' @param response response variables (instruments)
#' @param outcome outcome variables (for modelling E(Y|X))
#' @param calibration calibration variables (for which totals should be reproduced)
#' @param target target variable
#' @param data data.frame
#' @param svydesign surveydesign object
#' @param dweights design weights
#' @param N population size
#' @param pop_totals population totals (for calibration)
#' @param method methods `c("gencalib", "emplik", "gmm")`
#' @param tol tolerance `1e-8`
#' @param maxit maxit `50`
#' @param eps eps for inverse `.Machine$double.eps`
#' @param control control for methods
#' @param ... TBA
#' @references
#'
#' Deville, J. C., and Särndal, C. E. (1992). Calibration estimators in survey sampling.
#' Journal of the American statistical Association, 87(418), 376-382.
#'
#' add others
#'
#' @returns
#'
#' Returns a list with containing:\cr
#' \itemize{
#' \item{\code{g} -- g-weight that sums up to sample size,}
#' \item{\code{Xs} -- matrix used for calibration (i.e. Intercept, X and X_q transformed for calibration of quantiles),}
#' \item{\code{totals} -- a vector of totals (i.e. \code{N}, \code{pop_totals} and \code{pop_quantiles}),}
#' \item{\code{method} -- selected method,}
#' \item{\code{backend} -- selected backend.}
#' }
#'
#' @examples
#'
#' set.seed(123)
#' n <- 1000
#'
#' @seealso
#' [sampling::calib()] -- for standard calibration.
#'
#' [laeken::calibWeights()] -- for standard calibration.
#'
#' [survey::calibrate()] -- for standard and more advanced calibration.
#'
#' [ebal::ebalance()] -- for standard entropy balancing.
#'
#' @export
mnar <- function(response,
                 outcome,
                 calibration,
                 target,
                 data,
                 svydesign,
                 dweights,
                 N,
                 pop_totals,
                 method = c("gencalib", "emplik", "gmm"),
                 tol = 1e-8,
                 maxit = 50,
                 eps = .Machine$double.eps,
                 control = NULL,
                 ...) {

  ## to be considered
  Xs <- stats::model.matrix(response, data = data)
  Zs <- stats::model.matrix(calibration, data = data)
  if (method == "gencalib") {
    weights <- gencal(Xs=Xs, Zs=Zs, d=dweights, totals=pop_totals,
                      method="raking",
                      eps=eps, maxit=maxit, tol=tol)
  }
  ## return at the end
  return(weights)
}




