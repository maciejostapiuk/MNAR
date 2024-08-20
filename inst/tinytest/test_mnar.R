set.seed(123)
n <- 10000
x1 <- rlnorm(n, 0, 1)
x2 <- rexp(n, 1)
x3 <-  rpois(n,2)
y <- 1 + x1 + x2 + rnorm(n)
pr <- plogis(1 + 0.5*x1 - 0.5*y)
pop_data <- data.frame(x1, x2, y, pr)
totals <- c(N = n, colSums(pop_data[, c("x1", "x2", "y")]))
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
             method = "gencalib")
)
