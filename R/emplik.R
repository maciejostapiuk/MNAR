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
  if(length(instr) == 1){
    Xs <- matrix(Xs[, all.vars(instr)])
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
  else if(length(instr) > 1){
    Xs <-  Xs[,-1]
    n <- length(dweights)
    mu <- matrix(totals)
    dweights <-  matrix(dweights)
    u <- Xs - rep(1, n) %*% t(mu)

    M <- 0 * mu
    dif <- 1
    tol <- 1e-08

    while (dif > tol) {
      D1 <- 0 * mu
      DD <- D1 %*% t(D1)

      for (i in 1:n) {
        aa <- as.numeric(1 + t(M) %*% u[i, ])
        D1 <- D1 + dweights[i] * u[i, ] / aa
        DD <- DD - dweights[i] * (u[i, ] %*% t(u[i, ])) / aa^2
      }
      D2 <- solve(DD, D1, tol = 1e-12)
      dif <- max(abs(D2))
      rule <- 1
      while (rule > 0) {
        rule <- 0
        if (min(1 + t(M - D2) %*% t(u)) <= 0)
          rule <- rule + 1
        if (rule > 0)
          D2 <- D2 / 2
      }
      M <- M - D2
    }
    standarized_ds <- dweights/sum(dweights)
    imp_probs <-  numeric(n)
    for (i in 1:n) {
      lambda_term <- t(M)%*%u[i,]
      imp_probs[i] <-  standarized_ds[i]/(1+lambda_term)

  }}
  return(imp_probs)
}


