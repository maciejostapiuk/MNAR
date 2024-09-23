#' Importing
#' @importFrom momentfit momentModel, gelFit
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
                   theta_0,
                   ...){

  target_formula <- as.formula(target)
  instr_formula <- as.formula(instr)
  model <- momentfit::momentModel(target_formula, instr_formula, data = data, theta0 = theta_0, vcov = "iid")
  return(momentfit::gelFit(model))
}


