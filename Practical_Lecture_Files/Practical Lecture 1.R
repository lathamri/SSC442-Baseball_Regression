## Practicle Lectures 1 ##

# Get some basic info about the cars dataset
View(cars)
str(cars)
dim(cars)
nrow(cars)
ncol(cars)

# Plotting some data
plot(dist ~ speed, data = cars,
     xlab = "Speed (in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch  = 20,
     cex  = 2,
     col  = "grey")
# underfitting doesn't correlate to dist values enough
underfit_model = lm(dist ~ 1, data = cars)
abline(underfit_model, lwd = 3, col = "darkorange")

# Overfits the data and isn't realistic, tries to hit as many points as possible
overfit_model = lm(dist ~ poly(speed, 18), data = cars)
x = seq(-10, 50, length.out = 200)
plot(dist ~ speed, data = cars,
     xlab = "Speed (in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch  = 20,
     cex  = 2,
     col  = "grey")
lines(x, predict(overfit_model, data.frame(speed = x)), lwd = 2, col = "darkorange")

# Decent model, simply linear
stop_dist_model = lm(dist ~ speed, data = cars)
plot(dist ~ speed, data = cars,
     xlab = "Speed (in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch  = 20,
     cex  = 2,
     col  = "grey")
abline(stop_dist_model, lwd = 3, col = "darkorange")

## Building a Linear Prediciton Model ##
# Define Data
x = cars$speed
y = cars$dist
# Sum of Least Squares
Sxy = sum((x - mean(x)) * (y - mean(y)))
Sxx = sum((x - mean(x)) ^ 2)
Syy = sum((y - mean(y)) ^ 2)
c(Sxy, Sxx, Syy)
# Coefficients
beta_1_hat = Sxy / Sxx
beta_0_hat = mean(y) - beta_1_hat * mean(x)
c(beta_0_hat, beta_1_hat)
# Unique car speeds
unique(cars$speed)
# Estimated stopping distance with speed 8
beta_0_hat + beta_1_hat * 8
# Interpolation: 21 is not in the data set but it IS in the data range so we predict it after verifying this
8 %in% unique(cars$speed)
21 %in% unique(cars$speed)
min(cars$speed) < 21 & 21 < max(cars$speed)
beta_0_hat + beta_1_hat * 21
# Extrapolation: Estimating values outside our data set range, less sure about these
range(cars$speed)
range(cars$speed)[1] < 100 & 100 < range(cars$speed)[2]
beta_0_hat + beta_1_hat * 100
# Calculating residuals: these grab our data and the last one solves for our error
which(cars$speed == 8)
cars[5, ]
cars[which(cars$speed == 8), ]
16 - (beta_0_hat + beta_1_hat * 8)
# Estimate variance for the data
y_hat = beta_0_hat + beta_1_hat * x
e     = y - y_hat
n     = length(e)
s2_e  = sum(e^2) / (n - 2)
s2_e
# Sqrt gets our residual error standard deviation
s_e = sqrt(s2_e)
s_e
# Sum of Residual Squares and Sum of Squares Total
SST   = sum((y - mean(y)) ^ 2)
SSReg = sum((y_hat - mean(y)) ^ 2)
SSE   = sum((y - y_hat) ^ 2)
c(SST = SST, SSReg = SSReg, SSE = SSE)
# Verify accuracy of SSE
SSE / (n - 2)
s2_e == SSE / (n - 2)
# Coefficient of Regression
R2 = SSReg / SST
R2
# Linear Model using lm Function
# NOTE: dist ~ speed is saying speed predicts our dist value
stop_dist_model = lm(dist ~ speed, data = cars)
stop_dist_model
c(beta_0_hat, beta_1_hat)
# Plot our lm model
plot(dist ~ speed, data = cars,
     xlab = "Speed (in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch  = 20,
     cex  = 2,
     col  = "grey")
abline(stop_dist_model, lwd = 3, col = "darkorange")
# Gives us the names of calculations done in stop_dist_model
names(stop_dist_model)
# Tells us the residuals for our x value chosen
stop_dist_model$residuals
# These also work
coef(stop_dist_model)
resid(stop_dist_model)
fitted(stop_dist_model)
# Summary gives exactly that
summary(stop_dist_model)
# Can call names on any list of things
names(summary(stop_dist_model))
# Can also grab values out of the summary as so
summary(stop_dist_model)$r.squared
summary(stop_dist_model)$sigma
# Predict stuff using some model and some data.frame of data
predict(stop_dist_model, newdata = cars)
predict(stop_dist_model, newdata = data.frame(speed = cars$speed))
# Actually the same as calling this because stop_dist_model takes in cars already
predict(stop_dist_model)
# This is the same as calling fitted
fitted(stop_dist_model)
## Simulating Models ##
num_obs = 21
beta_0  = 5
beta_1  = -2
sigma   = 3
set.seed(1)
epsilon = rnorm(n = num_obs, mean = 0, sd = sigma)
# makes fixed sequential x values
x_vals = seq(from = 0, to = 10, length.out = num_obs)
# makes fixed random x values
set.seed(1)
x_vals = runif(num_obs, 0, 10)
# Estimate that thang
y_vals = beta_0 + beta_1 * x_vals + epsilon
# Plot and check validity
sim_fit = lm(y_vals ~ x_vals)
coef(sim_fit)
# Plots our values predicted by the x values, works great very cool 
plot(y_vals ~ x_vals)
abline(sim_fit)
# Function build of this boy
sim_slr = function(x, beta_0 = 10, beta_1 = 5, sigma = 1) {
  n = length(x)
  epsilon = rnorm(n, mean = 0, sd = sigma)
  y = beta_0 + beta_1 * x + epsilon
  data.frame(predictor = x, response = y)
}







