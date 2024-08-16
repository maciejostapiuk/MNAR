set.seed(123)
n <- 1000
x1 <- rlnorm(n, 0, 1)
x2 <- rexp(n, 1)
x3 <-  rpois(n,2)
y <- 1 + x1 + x2 + rnorm(n)
pr <- plogis(1 + 0.5*x1 - 0.5*y)
pop_data <- data.frame(x1, x2, x3,y, pr)
totals <- c(N = n, colSums(pop_data[, c("x1", "x2")]))


n_reps <- 100
results <- matrix(0, n_reps, 2)
for (r in 1:n_reps) {
  flag <- rbinom(n, 1, pop_data$pr)
  sample <- pop_data[flag == 1, ]
  sample$d <- n/nrow(sample)
  # Generalized calibration
  g <- mnar(response = ~ x1 +y,
            calibration =  ~ x1 + x2,
            data = sample, dweights = sample$d,
            pop_totals = totals,
            maxit = 200,
            method = "gencalib")

  # Naive
  results[r, 1] <- mean(sample$y)
  results[r, 2] <- weighted.mean(sample$y, g)
}

boxplot(results - mean(pop_data$y), ylim = c(-0.7, 0.3))
abline(h = 0, col = "red")

y_true <- pop_data$y
apply(results, 2, FUN = function(x) c(bias = mean(x) - mean(y_true),
                                      sd = sd(x),
                                      rmse = sqrt( (mean(x) - mean(y_true))^2 + var(x))))




x_totals <- c(N = n, colSums(pop_data[, c("x1", "x2", "y")]))
Zs_totals <- c(N = n, colSums(pop_data[, c("x1", "y")]))
for (r in 1:n_reps) {
  flag <- rbinom(n, 1, pop_data$pr)
  sample <- pop_data[flag == 1, ]
  sample$d <- n/nrow(sample)
  # Generalized calibration
  g1 <- mnar(response = ~ x1+ y,
             calibration =  ~ x1 + x2+y,
             data = sample, dweights = sample$d,
             pop_totals = x_totals,
             instr_totals =Zs_totals,
             method = "gencalib",
             maxit = 500)

  # Naive
  results[r, 1] <- mean(sample$y)
  results[r, 2] <- weighted.mean(sample$y, g1)
}

boxplot(results - mean(pop_data$y), ylim = c(-1, 1))
abline(h = 0, col = "red")

y_true <- pop_data$y
apply(results, 2, FUN = function(x) c(bias = mean(x) - mean(y_true),
                                      sd = sd(x),
                                      rmse = sqrt( (mean(x) - mean(y_true))^2 + var(x))))



