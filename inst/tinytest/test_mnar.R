set.seed(123)
n <- 1000
x1 <- rlnorm(n, 0, 1)
x2 <- rexp(n, 1)
x3 <- rpois(n,2)
y <- 1 + x1 + x2 + rnorm(n)
pr <- plogis(1 + 0.5*x1 - 0.5*y)
pop_data <- data.frame(x1, x2, x3, y, pr)
totals <- c(N = n, colSums(pop_data[, c("x1", "x2")]))
flag <- rbinom(n, 1, pop_data$pr)
sample <- pop_data[flag == 1, ]
sample$d <- n/nrow(sample)

# check if it is working --------------------------------------------------
expect_silent(
  g <- mnar(response = ~ x1 + y,
            calibration =  ~ x1 + x2,
            data = sample, dweights = sample$d,
            pop_totals = totals,
            method = "gencalib")
)



expect_silent(
  g1 <- mnar(response = ~ y,
             calibration =  ~ x1 + x2,
             data = sample, dweights = sample$d,
             pop_totals = totals,
             method = "gencalib"))



initial_phi = c(1,1/2,-1/4)
g_function <- function(X) {
  if(is.data.frame(X)) {
    return(cbind(1, X$x1, X$x2))
  } else if(is.matrix(X)) {
    return(X)
  } else {
    stop("X must be a data frame or matrix")
  }
}


response_model <- function(phi, data) {
  plogis(phi[1] + phi[2]*data$x1 + phi[3]*data$y)
}

expect_silent(
  g1 <- mnar(response = ~ y,
             calibration =  ~ x1 + x2,
             data = sample, dweights = sample$d,
             pop_totals = totals,
             propensity = response_model,
             g_function = g_function,
             phi_0 = initial_phi,
             method = "emplik")
)

