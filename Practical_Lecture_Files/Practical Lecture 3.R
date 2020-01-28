# Builds data and does regression simulation with linear and smoothing effects

set.seed(42)
n = 30
beta_0 = 1
beta_1 = 0.5
sigma = 3
x = runif(n, 0, 10)
xplot = seq(0, 10, by = 0.1)

sim_slr = function(n, x, beta_0 = 10, beta_1 = 5, sigma = 1) {
  epsilon = rnorm(n, mean = 0, sd = sigma)
  y       = beta_0 + beta_1 * x + epsilon
  data.frame(predictor = x, response = y)
}

sim_data = sim_slr(n = n, x = x, beta_0 = beta_0, beta_1 = beta_1, sigma = sigma)

sim_fit_1 = lm(response ~ predictor, data = sim_data)
sim_fit_2 = loess(response ~ predictor, data = sim_data)

par(mfrow = c(1, 2))

plot(response ~ predictor, data = sim_data,
     xlab = "Predictor", ylab = "Response",
     main = "Linear Regression",
     pch  = 20, cex  = 3, col = "darkgrey",
     xlim = c(0, 10), ylim = c(-6, 11))
abline(sim_fit_1, lwd = 3, col = "dodgerblue")

plot(response ~ predictor, data = sim_data,
     xlab = "Predictor", ylab = "Response",
     main = "Smoothing",
     pch  = 20, cex  = 3, col = "darkgrey",
     xlim = c(0, 10), ylim = c(-6, 11))
lines(xplot, predict(sim_fit_2, newdata = data.frame(predictor = xplot)),
      lwd = 3, lty = 2, col = "darkorange")

# Gives us the coeffecients from the x1, x3, x5 parameters to estimate y from the some_data set
# WONT RUN
fit = lm(y ~ x1 + x3 + x5, data = some_data)

## FUNCTIONS IN R

plot_unexp_dev = function(reg_data) {
  plot(reg_data$x, reg_data$y, main = "SSE (Sum of Squares Error)",
       xlab = "x", ylab = "y", pch = 20, cex = 2, col = "grey")
  arrows(reg_data$x, reg_data$y_hat,
         reg_data$x, reg_data$y,
         col = 'dodgerblue', lwd = 1, lty = 2, length = 0.2, angle = 20)
  abline(lm(y ~ x, data = reg_data), lwd = 2, col = "grey")
}

plot_exp_dev = function(reg_data) {
  plot(reg_data$x, reg_data$y, main = "SSReg (Sum of Squares Regression)",
       xlab = "x", ylab = "y", pch = 20, cex = 2, col = "grey")
  arrows(reg_data$x, reg_data$y_bar,
         reg_data$x, reg_data$y_hat,
         col = 'darkorange', lwd = 1, length = 0.2, angle = 20)
  abline(lm(y ~ x, data = reg_data), lwd = 2, col = "grey")
  abline(h = mean(reg_data$y), col = "grey")
}






