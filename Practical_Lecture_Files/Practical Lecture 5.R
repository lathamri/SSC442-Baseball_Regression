# This code included solely for completeness; you do not need to execute this.
x = seq(0, 100, by = 0.001)
f = function(x) {
  ((x - 50) / 50) ^ 2 + 2
}
g = function(x) {
  1 - ((x - 50) / 50)
}

par(mgp = c(1.5, 1.5, 0))
plot(x, g(x), ylim = c(0, 3), type = "l", lwd = 2,
     ylab = "Error", xlab = expression(Low %<-% Complexity %->% High),
     main = "Error versus Model Complexity", col = "darkorange",
     axes = FALSE)
grid()
axis(1, labels = FALSE)
axis(2, labels = FALSE)
box()
curve(f, lty = 6, col = "dodgerblue", lwd = 3, add = TRUE)
legend("bottomleft", c("(Expected) Test", "Train"), lty = c(6, 1), lwd = 3,
       col = c("dodgerblue", "darkorange"))

# This code included solely for completeness; you do not need to execute this.
x = seq(0.01, 0.99, length.out = 1000)

par(mfrow = c(1, 3))
par(mgp = c(1.5, 1.5, 0))

b = 0.05 / x
v = 5 * x ^ 2 + 0.5
bayes = 4
epe = b + v + bayes

plot(x, b, type = "l", ylim = c(0, 10), col = "dodgerblue", lwd = 2, lty = 3,
     xlab = "Model Complexity", ylab = "Error", axes = FALSE,
     main = "More Dominant Variance")
axis(1, labels = FALSE)
axis(2, labels = FALSE)
grid()
box()
lines(x, v, col = "darkorange", lwd = 2, lty = 4)
lines(x, epe, col = "black", lwd = 2)
abline(h = bayes, lty = 2, lwd = 2, col = "darkgrey")
abline(v = x[which.min(epe)], col = "grey", lty = 3, lwd = 2)

b = 0.05 / x
v = 5 * x ^ 4 + 0.5
bayes = 4
epe = b + v + bayes

plot(x, b, type = "l", ylim = c(0, 10), col = "dodgerblue", lwd = 2, lty = 3,
     xlab = "Model Complexity", ylab = "Error", axes = FALSE,
     main = "Decomposition of Prediction Error")
axis(1, labels = FALSE)
axis(2, labels = FALSE)
grid()
box()
lines(x, v, col = "darkorange", lwd = 2, lty = 4)
lines(x, epe, col = "black", lwd = 2)
abline(h = bayes, lty = 2, lwd = 2, col = "darkgrey")
abline(v = x[which.min(epe)], col = "grey", lty = 3, lwd = 2)

b = 6 - 6 * x ^ (1 / 4)
v = 5 * x ^ 6 + 0.5
bayes = 4
epe = b + v + bayes

plot(x, b, type = "l", ylim = c(0, 10), col = "dodgerblue", lwd = 2, lty = 3,
     xlab = "Model Complexity", ylab = "Error", axes = FALSE,
     main = "More Dominant Bias")
axis(1, labels = FALSE)
axis(2, labels = FALSE)
grid()
box()
lines(x, v, col = "darkorange", lwd = 2, lty = 4)
lines(x, epe, col = "black", lwd = 2)
abline(h = bayes, lty = 2, lwd = 2, col = "darkgrey")
abline(v = x[which.min(epe)], col = "grey", lty = 3, lwd = 2)
legend("topright", c("Squared Bias", "Variance", "Bayes", "EPE"), lty = c(3, 4, 2, 1),
       col = c("dodgerblue", "darkorange", "darkgrey", "black"), lwd = 2)

f = function(x) {
  x ^ 2
}
get_sim_data = function(f, sample_size = 100) {
  x = runif(n = sample_size, min = 0, max = 1)
  y = rnorm(n = sample_size, mean = f(x), sd = 0.3)
  data.frame(x, y)
}
get_sim_data = function(f, sample_size = 100) {
  x = runif(n = sample_size, min = 0, max = 1)
  eps = rnorm(n = sample_size, mean = 0, sd = 0.75)
  y = f(x) + eps
  data.frame(x, y)
}
set.seed(1)
sim_data = get_sim_data(f)
fit_0 = lm(y ~ 1,                   data = sim_data)
fit_1 = lm(y ~ poly(x, degree = 1), data = sim_data)
fit_2 = lm(y ~ poly(x, degree = 2), data = sim_data)
fit_9 = lm(y ~ poly(x, degree = 9), data = sim_data)
set.seed(42)
plot(y ~ x, data = sim_data, col = "grey", pch = 20,
     main = "Four Polynomial Models fit to a Simulated Dataset")
grid()
grid = seq(from = 0, to = 2, by = 0.01)
lines(grid, f(grid), col = "black", lwd = 3)
lines(grid, predict(fit_0, newdata = data.frame(x = grid)), col = "dodgerblue",  lwd = 2, lty = 2)
lines(grid, predict(fit_1, newdata = data.frame(x = grid)), col = "firebrick",   lwd = 2, lty = 3)
lines(grid, predict(fit_2, newdata = data.frame(x = grid)), col = "springgreen", lwd = 2, lty = 4)
lines(grid, predict(fit_9, newdata = data.frame(x = grid)), col = "darkorange",  lwd = 2, lty = 5)

legend("topleft",
       c("y ~ 1", "y ~ poly(x, 1)", "y ~ poly(x, 2)",  "y ~ poly(x, 9)", "truth"),
       col = c("dodgerblue", "firebrick", "springgreen", "darkorange", "black"), lty = c(2, 3, 4, 5, 1), lwd = 2)

par(mfrow = c(1, 3))

# Note: This code is BAD. Don't use this. Or, like, clean it up.
# This is lazy and bad programming. Seriously, don't program like this. DON'T DO IT.

set.seed(430)
sim_data_1 = get_sim_data(f)
sim_data_2 = get_sim_data(f)
sim_data_3 = get_sim_data(f)
fit_0_1 = lm(y ~ 1, data = sim_data_1)
fit_0_2 = lm(y ~ 1, data = sim_data_2)
fit_0_3 = lm(y ~ 1, data = sim_data_3)
fit_9_1 = lm(y ~ poly(x, degree = 9), data = sim_data_1)
fit_9_2 = lm(y ~ poly(x, degree = 9), data = sim_data_2)
fit_9_3 = lm(y ~ poly(x, degree = 9), data = sim_data_3)

plot(y ~ x, data = sim_data_1, col = "grey", pch = 20, main = "Simulated Dataset 1")
grid()
grid = seq(from = 0, to = 2, by = 0.01)
lines(grid, predict(fit_0_1, newdata = data.frame(x = grid)), col = "dodgerblue", lwd = 2, lty = 2)
lines(grid, predict(fit_9_1, newdata = data.frame(x = grid)), col = "darkorange", lwd = 2, lty = 5)
legend("topleft", c("y ~ 1", "y ~ poly(x, 9)"), col = c("dodgerblue", "darkorange"), lty = c(2, 5), lwd = 2)

plot(y ~ x, data = sim_data_2, col = "grey", pch = 20, main = "Simulated Dataset 2")
grid()
grid = seq(from = 0, to = 2, by = 0.01)
lines(grid, predict(fit_0_2, newdata = data.frame(x = grid)), col = "dodgerblue", lwd = 2, lty = 2)
lines(grid, predict(fit_9_2, newdata = data.frame(x = grid)), col = "darkorange", lwd = 2, lty = 5)
legend("topleft", c("y ~ 1", "y ~ poly(x, 9)"), col = c("dodgerblue", "darkorange"), lty = c(2, 5), lwd = 2)

plot(y ~ x, data = sim_data_3, col = "grey", pch = 20, main = "Simulated Dataset 3")
grid()
grid = seq(from = 0, to = 2, by = 0.01)
lines(grid, predict(fit_0_3, newdata = data.frame(x = grid)), col = "dodgerblue", lwd = 2, lty = 2)
lines(grid, predict(fit_9_3, newdata = data.frame(x = grid)), col = "darkorange", lwd = 2, lty = 5)
legend("topleft", c("y ~ 1", "y ~ poly(x, 9)"), col = c("dodgerblue", "darkorange"), lty = c(2, 5), lwd = 2)

par(mfrow = c(1, 3))

# if you're reading this code
# it's BAD! don't use it. (or clean it up)
# also, note to self: clean up this code!!!

grid = seq(from = 0, to = 2, by = 0.01)

set.seed(430)
sim_data_1 = get_sim_data(f)
sim_data_2 = get_sim_data(f)
sim_data_3 = get_sim_data(f)
fit_0_1 = FNN::knn.reg(train = sim_data_1["x"], test = data.frame(x = grid), y = sim_data_1["y"], k = 5)$pred
fit_0_2 = FNN::knn.reg(train = sim_data_2["x"], test = data.frame(x = grid), y = sim_data_2["y"], k = 5)$pred
fit_0_3 = FNN::knn.reg(train = sim_data_3["x"], test = data.frame(x = grid), y = sim_data_3["y"], k = 5)$pred
fit_9_1 = FNN::knn.reg(train = sim_data_1["x"], test = data.frame(x = grid), y = sim_data_1["y"], k = 100)$pred
fit_9_2 = FNN::knn.reg(train = sim_data_2["x"], test = data.frame(x = grid), y = sim_data_2["y"], k = 100)$pred
fit_9_3 = FNN::knn.reg(train = sim_data_3["x"], test = data.frame(x = grid), y = sim_data_3["y"], k = 100)$pred

plot(y ~ x, data = sim_data_1, col = "grey", pch = 20, main = "Simulated Dataset 1")
grid()
lines(grid, fit_0_1, col = "dodgerblue", lwd = 1, lty = 1)
lines(grid, fit_9_1, col = "darkorange", lwd = 2, lty = 2)
legend("topleft", c("k = 5", "k = 100"), col = c("dodgerblue", "darkorange"), lty = c(1, 2), lwd = 2)

plot(y ~ x, data = sim_data_2, col = "grey", pch = 20, main = "Simulated Dataset 2")
grid()
lines(grid, fit_0_2, col = "dodgerblue", lwd = 1, lty = 1)
lines(grid, fit_9_2, col = "darkorange", lwd = 2, lty = 2)
legend("topleft", c("k = 5", "k = 100"), col = c("dodgerblue", "darkorange"), lty = c(1, 2), lwd = 2)

plot(y ~ x, data = sim_data_3, col = "grey", pch = 20, main = "Simulated Dataset 3")
grid()
lines(grid, fit_0_3, col = "dodgerblue", lwd = 1, lty = 1)
lines(grid, fit_9_3, col = "darkorange", lwd = 2, lty = 2)
legend("topleft", c("k = 5", "k = 100"), col = c("dodgerblue", "darkorange"), lty = c(1, 2), lwd = 2)

set.seed(1)
n_sims = 250
n_models = 4
x = data.frame(x = 0.90) # Fixed point at which we make predictions
predictions = matrix(0, nrow = n_sims, ncol = n_models)

for (sim in 1:n_sims) {
  
  # Simulate new, random, training data
  # This is the only random portion of the bias, var, and mse calculations
  # This allows us to calculate the expectation over D
  sim_data = get_sim_data(f)
  
  # Fit models
  fit_0 = lm(y ~ 1,                   data = sim_data)
  fit_1 = lm(y ~ poly(x, degree = 1), data = sim_data)
  fit_2 = lm(y ~ poly(x, degree = 2), data = sim_data)
  fit_9 = lm(y ~ poly(x, degree = 9), data = sim_data)
  
  # Get predictions
  predictions[sim, 1] = predict(fit_0, x)
  predictions[sim, 2] = predict(fit_1, x)
  predictions[sim, 3] = predict(fit_2, x)
  predictions[sim, 4] = predict(fit_9, x)
}

# Note that this code requires tidyverse; it can get a bit fussy if running OSX.
predictions = (predictions)
colnames(predictions) = c("0", "1", "2", "9")
predictions = as.data.frame(predictions)

tall_predictions = tidyr::gather(predictions, factor_key = TRUE)
boxplot(value ~ key, data = tall_predictions, border = "darkgrey", xlab = "Polynomial Degree", ylab = "Predictions",
        main = "Simulated Predictions for Polynomial Models")
grid()
stripchart(value ~ key, data = tall_predictions, add = TRUE, vertical = TRUE, method = "jitter", jitter = 0.15, pch = 1, col = c("dodgerblue", "firebrick", "springgreen", "darkorange"))
abline(h = f(x = 0.90), lwd = 2)

get_mse = function(truth, estimate) {
  mean((estimate - truth) ^ 2)
}

get_bias = function(estimate, truth) {
  mean(estimate) - truth
}

get_var = function(estimate) {
  mean((estimate - mean(estimate)) ^ 2)
}

bias = apply(predictions, 2, get_bias, truth = f(x = 0.90))
variance = apply(predictions, 2, get_var)
mse = apply(predictions, 2, get_mse, truth = f(x = 0.90))

results = data.frame(
  poly_degree = c(0, 1, 2, 9),
  round(mse, 5),
  round(bias ^ 2, 5),
  round(variance, 5)
)
colnames(results) = c("Degree", "Mean Squared Error", "Bias Squared", "Variance")
rownames(results) = NULL
knitr::kable(results, booktabs = TRUE, escape = TRUE, align = "c")

all(diff(bias ^ 2) < 0)
all(diff(variance) > 0)
diff(mse) < 0

bias ^ 2 + variance == mse
all.equal(bias ^ 2 + variance, mse)

get_epe = function(realized, estimate) {
  mean((realized - estimate) ^ 2)
}

y = rnorm(n = nrow(predictions), mean = f(x = 0.9), sd = 0.3)
epe = apply(predictions, 2, get_epe, realized = y)
epe

# hmmm, what's wrong here?
# the mean realative diff does go down with n
# is there really just that much compuational error?
sigma_hat = mean((y - f(x = 0.90)) ^ 2)
all.equal(epe, bias ^ 2 + variance + sigma_hat)

set.seed(1)
# Note this is intentionally incomplete
n_sims = 1000
X = runif(n = n_sims, min = 0, max = 1)
Y = rnorm(n = n_sims, mean = f(X), sd = 0.3)

f_hat_X = rep(0, length(X))

for (i in seq_along(X)) {
  sim_data = get_sim_data(f)
  fit_2 = lm(y ~ poly(x, degree = 2), data = sim_data)
  f_hat_X[i] = predict(fit_2, newdata = data.frame(x = X[i]))
}

mean((Y - f_hat_X) ^ 2)








