library(sampling)
library(devtools)
library(rstudioapi)
check()
load_all()



set.seed(123)
n <- 10000
x1 <- rlnorm(n, 0, 1)
x2 <- rexp(n, 1)
y <- 1 + x1 + x2 + rnorm(n)
pr <- plogis(1 + 0.5*x1 - 0.5*y)
pop_data <- data.frame(x1, x2, y, pr)

totals <- c(N = n, colSums(pop_data[, c("x1", "x2")]))

n_reps <- 10000
results <- matrix(0, n_reps, 2)

for (r in 1:n_reps) {
  flag <- rbinom(n, 1, pop_data$pr)
  sample <- pop_data[flag == 1, ]
  xs = model.matrix(~x1 + x2, sample)
  zs = model.matrix(~x1 + y, sample)
  d = rep(1, NROW(sample))
  ## generalized calibration
  g_generl <-mnar(totals = totals,calib_var = xs, instr_var=zs, target_var=y,initial_weights=d, method = "gencalib")

  ## naive
  results[r,1] <- mean(sample$y)
  results[r,2] <- weighted.mean(sample$y, g_generl)
}

boxplot(results - mean(pop_data$y))
abline(h = 0, col = "red")

y_true <- pop_data$y
apply(results, 2, FUN = function(x) c(bias = mean(x) - mean(y_true),
                                      sd = sd(x),
                                      rmse = sqrt( (mean(x) - mean(y_true))^2 + var(x))))



# Pobierz wszystkie ścieżki z .libPaths()
library_paths <- .libPaths()

# Iteracja po każdej ścieżce
for (path in library_paths) {
  cat("Sprawdzanie ścieżki:", path, "\n")

  # Możesz sprawdzić, jakie pakiety są zainstalowane w danej ścieżce
  installed_packages <- list.files(path)

  # Wyświetl zainstalowane pakiety w tej ścieżce
  cat("Pakiety zainstalowane w tej ścieżce:\n")
  print(installed_packages)
  cat("\n\n")
}




