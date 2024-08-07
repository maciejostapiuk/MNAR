library(sampling)

mnar <-  function(pop_dat, samp_data, totals, Xs, Zs = NULL, Y, ds, Probs, method=NULL) {
  if (is.null(Zs)) {  
    weights<- calib(Xs,
                    ds,
                    total = totals,
                    method = "raking")
    return(weights)
                    }
  else if (!is.null(Zs) & method == "generalized calibration"){
    weights <- gencalib(Xs,
                        Zs,
                        ds,
                        total = totals,
                        method = "raking")
    return(weights)
                        }
  
}
### testing

set.seed(123)
n <- 10000
x1 <- rlnorm(n, 0, 1)
x2 <- rexp(n, 1)
y <- 1 + x1 + x2 + rnorm(n)
pr <- plogis(1 + 0.5*x1 - 0.5*y)
pop_data <- data.frame(x1, x2, y, pr)

totals <- c(N = n, colSums(pop_data[, c("x1", "x2")]))

n_reps <- 10000
results <- matrix(0, n_reps, 3)

for (r in 1:n_reps) {
  flag <- rbinom(n, 1, pop_data$pr)
  sample <- pop_data[flag == 1, ]
  xs = model.matrix(~x1 + x2, sample)
  zs = model.matrix(~x1 + y, sample)
  d = rep(1, NROW(sample))
  ## standard calib
  g_standard <- mnar(pop_dat = pop_data, samp_data = sample, totals = totals,Xs= xs, Y=y,ds=d,Probs = pr, method = NULL)
  ## generalized calibration
  g_generl <-mnar(pop_dat = pop_data, samp_data = sample, totals = totals,Xs= xs, Zs=zs, Y=y,ds=d,Probs = pr, method = "generalized calibration")
  ## naive
  results[r,1] <- mean(sample$y)
  results[r,2] <- weighted.mean(sample$y, g_standard)
  results[r,3] <- weighted.mean(sample$y, g_generl)
}

boxplot(results - mean(pop_data$y))
abline(h = 0, col = "red")

y_true <- pop_data$y
apply(results, 2, FUN = function(x) c(bias = mean(x) - mean(y_true),
                                      sd = sd(x),
                                      rmse = sqrt( (mean(x) - mean(y_true))^2 + var(x))))