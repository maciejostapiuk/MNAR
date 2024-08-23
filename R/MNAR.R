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
#' @param pop_totals population totals (for both calibration and response)
#' @param method methods `c("gencalib", "emplik", "gmm")`
#' @param tol tolerance `1e-8`
#' @param maxit maxit `50`
#' @param eps eps for inverse `.Machine$double.eps`
#' @param control control for methods
#' @param ... TBA
#' @references
#'
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
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Define sample size
#' n <- 10000
#'
#' # Generate variables
#' x1 <- rlnorm(n, 0, 1)
#' x2 <- rexp(n, 1)
#' x3 <-  rpois(n,2)
#' y <- 1 + x1 + x2 + rnorm(n)
#'
#' # Calculate probabilities
#' pr <- plogis(1 + 0.5*x1 - 0.5*y)
#'
#' # Create population data frame
#' pop_data <- data.frame(x1, x2, y, pr)
#'
#' # Calculate population totals
#' totals <- c(N = n, colSums(pop_data[, c("x1", "x2", "y")]))
#'
#' # Generate flag variable based on probabilities
#' flag <- rbinom(n, 1, pop_data$pr)
#'
#' # Create sample based on flag
#' sample <- pop_data[flag == 1, ]
#' sample$d <- n/nrow(sample)
#'
#'
#' # Case when dim(response) == dim(calibration)
#' g <- mnar(response = ~ x1 + y,
#'             calibration =  ~ x1 + x2,
#'             data = sample, dweights = sample$d,
#'             pop_totals = totals,
#'             method = "gencalib")
#'
#' head(g) #display only couple of first output values
#'
#' ## Case when dim(response) < dim(calibration)
#' g1 <- mnar(response = ~ y,
#'              calibration =  ~ x1 + x2,
#'              data = sample, dweights = sample$d,
#'              pop_totals = totals,
#'              method = "gencalib")
#'
#' head(g1) #display only couple of first output values
#'
#'
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
#'
#'
#'
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
                 eps = .Machine$double.eps,
                 control = NULL,
                 maxit = 100,
                 ...) {

  ## to be considered
  Xs <- stats::model.matrix(calibration, data = data)
  Zs <- stats::model.matrix(response, data = data)
  if (method == "gencalib") {
    if("y" %in% all.vars(response)) {
    pop_totals["y"] <-  sum(sample$d * sample$y)
    }
    weights <- gencal(Xs=Xs, Zs=Zs, d=dweights, pop_totals=pop_totals,
                      method="raking",
                      eps=eps, maxit=maxit, tol=tol)
  }
  ## return at the end
  return(weights)
}




