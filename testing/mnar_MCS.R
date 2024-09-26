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




for (r in 1:n_reps) {
  flag <- rbinom(n, 1, pop_data$pr)
  sample <- pop_data[flag == 1, ]
  sample$d <- n/nrow(sample)
  # Generalized calibration
  g1 <- mnar(response = ~ y,
             calibration =  ~ x1 + x2,
             data = sample, dweights = sample$d,
             pop_totals = totals,
             method = "gencalib")

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


theta_0 = c(1,1,1)
n_reps <- 100
results <- matrix(0, n_reps, 2)
for (r in 1:n_reps) {
  flag <- rbinom(n, 1, pop_data$pr)
  sample <- pop_data[flag == 1, ]
  sample$d <- n/nrow(sample)
  #Empirical likelihood
  g <- mnar(response = ~ x1,
            calibration =  ~ x1 + x2,
            target = y~x1+x2,
            data = sample, dweights = sample$d,
            theta_0 = theta_0,
            pop_totals = totals[2]/totals[1],
            maxit = 200,
            method = "emplik")

  # Naive
  results[r, 1] <- mean(sample$x1)
  results[r, 2] <- weighted.mean(sample$x1, g)
}

boxplot(results - mean(pop_data$x1), ylim = c(-0.2, 0.2))
abline(h = 0, col = "red")

x1_true <- pop_data$x1
apply(results, 2, FUN = function(x) c(bias = mean(x) - mean(x1_true),
                                      sd = sd(x),
                                      rmse = sqrt( (mean(x) - mean(x1_true))^2 + var(x))))



for (r in 1:n_reps) {
  flag <- rbinom(n, 1, pop_data$pr)
  sample <- pop_data[flag == 1, ]
  sample$d <- n/nrow(sample)
  #Empirical likelihood with dim(Xs) > 1
  g <- mnar(response = ~ x1+x2,
            calibration =  ~ x1 + x2,
            target = y~x1+x2,
            data = sample, dweights = sample$d,
            theta_0 = theta_0,
            pop_totals = totals[c(2,3)]/totals[1],
            maxit = 200,
            method = "emplik")
  # Naive
  results[r, 1] <- mean(sample$x1)
  results[r, 2] <- weighted.mean(sample$x1, g)
}

boxplot(results - mean(pop_data$x1), ylim = c(-0.2, 0.2))
abline(h = 0, col = "red")

x1_true <- pop_data$x1
apply(results, 2, FUN = function(x) c(bias = mean(x) - mean(x1_true),
                                      sd = sd(x),
                                      rmse = sqrt( (mean(x) - mean(x1_true))^2 + var(x))))

