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
#' @param method a method upon which the `sampling::gencalib()` function will be called
#' @param eps describes desired accuracy level in any convergence-related
#' @param maxit maximum number of iterations for `sampling::gencalib()` since it works iteratively
#' @param tol desired tolerance
#'
#' @references
#' Andridge, Rebecca R, and Roderick JA Little. 2011. “Proxy Pattern-Mixture Analysis for Survey Nonresponse.” Journal of Official Statistics 27 (2): 153.
#'
#' Chang, T., and P. S. Kott. 2008. “Using Calibration Weighting to Adjust for Nonresponse Under a Plausible Model.” Biometrika 95 (3): 555–71. https://doi.org/10.1093/biomet/asn022.
#'
#' Deville, Jean-Claude, and Carl-Erik Särndal. 1992. “Calibration Estimators in Survey Sampling.” Journal of the American Statistical Association 87 (418): 376–82.
#'
#' Estevao, Victor, and Carl Särndal. n.d. “A Functional Form Approach to Calibration.” Journal of Of®cial Statistics 16 (4): 379±399. https://www.proquest.com/scholarly-journals/functional-form-approach-calibration/docview/1266846662/se-2.
#'
#' Isaki, C. T., and W. A. Fuller. 1982. “Survey Design Under the Regression Super-Population Model.” Journal of the American Statistical Association 77 (377): 89–96.
#'
#' Kott, Phillip S., and Ted Chang. 2010. “Using Calibration Weighting to Adjust for Nonignorable Unit Nonresponse.” Journal of the American Statistical Association 105 (491): 1265–75. https://doi.org/10.1198/jasa.2010.tm09016.
#'
#' Kott, Phillip S., and Dan Liao. 2017. “Calibration Weighting for Nonresponse That Is Not Missing at Random: Allowing More Calibration Than Response-Model Variables.” Journal of Survey Statistics and Methodology 5 (2): 159–74. https://doi.org/10.1093/jssam/smx003.
#'
#' @examples
#' ## Generate the data to formulate response model, calibration variables and target variable
#'
#' set.seed(123)
#' n <- 10000
#' x1 <- rlnorm(n, 0, 1)
#' x2 <- rexp(n, 1)
#' x3 <-  rpois(n,2)
#' y <- 1 + x1 + x2 + rnorm(n)
#' pr <- plogis(1 + 0.5*x1 - 0.5*y)
#' pop_data <- data.frame(x1, x2, y, pr)
#' totals <- c(N = n, colSums(pop_data[, c("x1", "x2", "y")]))
#' flag <- rbinom(n, 1, pop_data$pr)
#' sample <- pop_data[flag == 1, ]
#' sample$d <- n/nrow(sample)
#'
#' ## case when dim(Xs) == dim(Zs)
#' g <- gencal(Xs = stats::model.matrix(~ x1 + x2, data = sample),
#'             Zs = stats::model.matrix(~ x1 + y, data = sample),
#'             d = sample$d,
#'             pop_totals = totals,
#'             method = "raking",
#'             eps = .Machine$double.eps,
#'             maxit = 100,
#'             tol = 1e-8)
#'
#' head(g) #display only couple of first output values
#'
#'
#'
#' ## case when dim(Xs) > dim(Zs)
#' g1 <-gencal(Xs = stats::model.matrix(~ x1 + x2, data = sample),
#'             Zs = stats::model.matrix(~ y, data = sample),
#'             d = sample$d,
#'             pop_totals = totals,
#'             method = "raking",
#'             eps = .Machine$double.eps,
#'             maxit = 100,
#'             tol = 1e-8)
#'
#' head(g1) #display only couple of first output values
#'
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






