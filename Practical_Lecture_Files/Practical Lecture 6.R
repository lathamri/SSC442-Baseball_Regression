# Includes
library(tidyverse)
library(kableExtra)
library(caret)

# Define functions for random data and shaping function
cubic_mean = function(x) {
  1 - 2 * x - 3 * x ^ 2 + 5 * x ^ 3
}

gen_slr_data = function(sample_size = 100, mu) {
  x = runif(n = sample_size, min = -1, max = 1)
  y = mu(x) + rnorm(n = sample_size)
  tibble(x, y)
}

# Set seed and get our data as a variable
set.seed(1)
sim_slr_data = gen_slr_data(sample_size = 30, mu = cubic_mean)

# Pipe it into kable and get our x_y values
sim_slr_data %>%
  kable() %>%
  kable_styling(full_width = FALSE)

# plot them
plot(sim_slr_data, pch = 20, col = "grey", cex = 2,
     main = "Simulated Data")
grid()

# Check out the best linear model
lm(y ~ x + I(x ^ 2) + I(x ^ 3), data = sim_slr_data)

# KNN function
plot_knn = function(k = 5, x, main = "main") {
  distances = dist(c(x, sim_slr_data$x))[1:30]
  nn = order(distances)[1:k]
  plot(sim_slr_data, pch = 20, col = "grey", cex = 2, main = main)
  grid()
  points(sim_slr_data$x[nn],
         sim_slr_data$y[nn],
         col = "green", cex = 1.8, lwd = 2)
  points(x = x, y = mean(sim_slr_data$y[nn]), pch = 4, lwd = 3)
}

# Run our stuff with a KNN
par(mfrow = c(1, 3))
plot_knn(k = 3, x = -0.5, main = "k = 3, x = -0.5")
plot_knn(k = 5, x = 0, main = "k = 5, x = 0")
plot_knn(k = 9, x = 0.75, main = "k = 9, x = 0.75")

# Run some cool knn stuff
knn_slr_25 = knnreg(y ~ x, data = sim_slr_data, k = 25, use.all = TRUE)
knn_slr_05 = knnreg(y ~ x, data = sim_slr_data, k = 5, use.all = TRUE)
knn_slr_01 = knnreg(y ~ x, data = sim_slr_data, k = 1, use.all = TRUE)

# Plot the stuff
par(mfrow = c(1, 3))

plot(sim_slr_data, pch = 20, col = "grey", cex = 2,
     main = "k = 25")
curve(cubic_mean(x), add = TRUE, lwd = 2, lty = 2)
curve(predict(knn_slr_25, tibble(x = x)),
      col = "firebrick", lwd = 2, lty = 1, add = TRUE, n = 10000)
grid()

plot(sim_slr_data, pch = 20, col = "grey", cex = 2,
     main = "k = 5")
curve(cubic_mean(x), add = TRUE, lwd = 2, lty = 2)
curve(predict(knn_slr_05, tibble(x = x)),
      col = "dodgerblue", lwd = 2, lty = 1, add = TRUE, n = 10000)
grid()

plot(sim_slr_data, pch = 20, col = "grey", cex = 2,
     main = "k = 1")
curve(cubic_mean(x), add = TRUE, lwd = 2, lty = 2)
curve(predict(knn_slr_01, tibble(x = x)),
      col = "limegreen", lwd = 2, lty = 1, add = TRUE, n = 10000)
grid()





