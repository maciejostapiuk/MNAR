#' Importing
#' @importFrom momentfit momentModel
#' @importFrom momentfit gelFit
#' @importFrom momentfit getImpProb
#'
#' @title empirical likelihood method based on momentfit package
#'
#' @author Maciej Ostapiuk and Maciej Beręsewicz based on Kim, Shao (2021)
#' @param target formula for target variable
#' @param instr a formula for covariate of response
#' @param data input data
#' @param theta_0 initial moment condition for coeffs
#'
emplik <- function(target,
                   instr,
                   data,
                   dweights,
                   theta_0,
                   totals,
                   ...){

  #target_formula <- as.formula(target)
  #instr_formula <- as.formula(instr)
  #model <- momentfit::momentModel(target_formula, instr_formula, data = data, theta0 = theta_0, vcov = "iid")
  #fit <-momentfit::gelFit(model)
  #weights <-momentfit::getImpProb(fit)$pt
  Xs <-  model.matrix(instr, data = data)
  Xs <-  matrix(Xs[, all.vars(instr)])
  if(ncol(Xs) == 1){
    L <- -1 / max(Xs - totals)
    R <- -1 / min(Xs - totals)
    dif <- 1
    tol <- 1e-08
    while (dif > tol) {
      M <- (L + R) / 2
      glam <- sum((dweights * (Xs - totals)) / (1 + M * (Xs - totals)))
      if (glam > 0) L <- M
      if (glam < 0) R <- M
      dif <- abs(glam)
    }
    standarized_ds <- dweights/sum(dweights)
    imp_probs <-  standarized_ds/(1 + M*(Xs - totals))
  }
  return(imp_probs)
}

