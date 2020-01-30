# Simulation stuff
library(lmtest)

# Linear data with normal 
sim_1 = function(sample_size = 500) {
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = 1)
  data.frame(x, y)
}

# Linear data with normal and increasing sd
sim_2 = function(sample_size = 500) {
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = x)
  data.frame(x, y)
}

# Non-linear model
sim_3 = function(sample_size = 500) {
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x ^ 2 + rnorm(n = sample_size, mean = 0, sd = 5)
  data.frame(x, y)
}

# Runing sim 1
set.seed(42)
sim_data_1 = sim_1()
head(sim_data_1)

# Plot our sim 1 and make linear model
plot(y ~ x, data = sim_data_1, col = "grey", pch = 20,
     main = "Data from Model 1")
fit_1 = lm(y ~ x, data = sim_data_1)
abline(fit_1, col = "darkorange", lwd = 3)

# Fitted vs Residuals plot
plot(fitted(fit_1), resid(fit_1), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)

# Test constant variance
# Larger p-value means it has probably constant variance
# Not more or less true for higher and lower values
bptest(fit_1)

# Sim 2
set.seed(42)
sim_data_2 = sim_2()
fit_2 = lm(y ~ x, data = sim_data_2)
plot(y ~ x, data = sim_data_2, col = "grey", pch = 20,
     main = "Data from Model 2")
abline(fit_2, col = "darkorange", lwd = 3)

# Fitted vs Residuals
plot(fitted(fit_2), resid(fit_2), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 2")
abline(h = 0, col = "darkorange", lwd = 2)


# Test constant variance
# low p-value tells us it's non-constant
bptest(fit_2)

# Sim 3
set.seed(42)
sim_data_3 = sim_3()
fit_3 = lm(y ~ x, data = sim_data_3)
plot(y ~ x, data = sim_data_3, col = "grey", pch = 20,
     main = "Data from Model 3")
abline(fit_3, col = "darkorange", lwd = 3)

# Fitted vs Residuals
plot(fitted(fit_3), resid(fit_3), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 3")
abline(h = 0, col = "darkorange", lwd = 2)

# Test constant variance
bptest(fit_3)

# Histogram stuff

par(mfrow = c(1, 3))
hist(resid(fit_1),
     xlab   = "Residuals",
     main   = "Histogram of Residuals, fit_1",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)
hist(resid(fit_2),
     xlab   = "Residuals",
     main   = "Histogram of Residuals, fit_2",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)
hist(resid(fit_3),
     xlab   = "Residuals",
     main   = "Histogram of Residuals, fit_3",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)

# Notice all the histograms are normally distributed around 0
# with fit_1 having a standard normal, fit_2 having a much higher 
# peak at the mean and much farther residuals, and fit_3 having
# very high residuals and a farther spread due to the variance

# QQ Plots make these easy to read 
# Gives the normal distribution values as a linear deal and plots
# against the expected value for them
qqnorm(resid(fit_1), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(fit_1), col = "dodgerblue", lwd = 2)

# Makes a qq_plot 
qq_plot = function(e) {
  
  n = length(e)
  normal_quantiles = qnorm(((1:n - 0.5) / n))
  # normal_quantiles = qnorm(((1:n) / (n + 1)))
  
  # plot theoretical verus observed quantiles
  plot(normal_quantiles, sort(e),
       xlab = c("Theoretical Quantiles"),
       ylab = c("Sample Quantiles"),
       col = "darkgrey")
  title("Normal Q-Q Plot")
  
  # calculate line through the first and third quartiles
  slope     = (quantile(e, 0.75) - quantile(e, 0.25)) / (qnorm(0.75) - qnorm(0.25))
  intercept = quantile(e, 0.25) - slope * qnorm(0.25)
  
  # add to existing plot
  abline(intercept, slope, lty = 2, lwd = 2, col = "dodgerblue")
}

# These run our qq plot 
set.seed(420)
x = rnorm(100, mean = 0 , sd = 1)
par(mfrow = c(1, 2))
qqnorm(x, col = "darkgrey")
qqline(x, lty = 2, lwd = 2, col = "dodgerblue")
qq_plot(x)

# More data gives better qq plot estimates
par(mfrow = c(1, 3))
set.seed(420)
qq_plot(rnorm(10))
qq_plot(rnorm(25))
qq_plot(rnorm(100))

# If things aren't normal (like t-tests with low freedom)
par(mfrow = c(1, 3))
set.seed(420)
qq_plot(rt(10, df = 4))
qq_plot(rt(25, df = 4))
qq_plot(rt(100, df = 4))

# Exponentialpar(mfrow = c(1, 3))
set.seed(420)
qq_plot(rexp(10))
qq_plot(rexp(25))
qq_plot(rexp(100))

# WHOLE POINT: if the dots are far from the blue line it 
# violates the normalcy assumption

# Shapiro-Wilk normality test
# Works similar to BP test, high p-value probably normal otherwise not
set.seed(42)
shapiro.test(rnorm(25))
shapiro.test(rexp(25))

shapiro.test(resid(fit_1))
shapiro.test(resid(fit_2))
shapiro.test(resid(fit_3))

# Unusual Observations

par(mfrow = c(1, 3))
set.seed(42)
ex_data  = data.frame(x = 1:10,
                      y = 10:1 + rnorm(n = 10))
ex_model = lm(y ~ x, data = ex_data)

# low leverage, large residual, small influence
point_1 = c(5.4, 11)
ex_data_1 = rbind(ex_data, point_1)
model_1 = lm(y ~ x, data = ex_data_1)
plot(y ~ x, data = ex_data_1, cex = 2, pch = 20, col = "grey",
     main = "Low Leverage, Large Residual, Small Influence")
points(x = point_1[1], y = point_1[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_1, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original Data", "Added Point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

# high leverage, small residual, small influence
point_2 = c(18, -5.7)
ex_data_2 = rbind(ex_data, point_2)
model_2 = lm(y ~ x, data = ex_data_2)
plot(y ~ x, data = ex_data_2, cex = 2, pch = 20, col = "grey",
     main = "High Leverage, Small Residual, Small Influence")
points(x = point_2[1], y = point_2[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_2, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original Data", "Added Point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

# high leverage, large residual, large influence
point_3 = c(14, 5.1)
ex_data_3 = rbind(ex_data, point_3)
model_3 = lm(y ~ x, data = ex_data_3)
plot(y ~ x, data = ex_data_3, cex = 2, pch = 20, col = "grey", ylim = c(-3, 12),
     main = "High Leverage, Large Residual, Large Influence")
points(x = point_3[1], y = point_3[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_3, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original Data", "Added Point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

# Blue solid is regression fit to original values
# Orange line is after adding one outlier point to the data
# Circled point is the added point

#Blue line model
coef(ex_model)[2]

# Orange line in graph 1
coef(model_1)[2]

# Orange line in 2
coef(model_2)[2]

# Orange line in 3, much smaller because high leverage point
# pulls the fit line a lot
coef(model_3)[2]

# Finds our leverage
lev_ex = data.frame(
  x1 = c(0, 11, 11, 7, 4, 10, 5, 8),
  x2 = c(1, 5, 4, 3, 1, 4, 4, 2),
  y  = c(11, 15, 13, 14, 0, 19, 16, 8))

plot(x2 ~ x1, data = lev_ex, cex = 2)
points(7, 3, pch = 20, col = "red", cex = 2)
# Linear algebra stuff to see our diag hat matrix
X = cbind(rep(1, 8), lev_ex$x1, lev_ex$x2)
H = X %*% solve(t(X) %*% X) %*% t(X)
diag(H)

sum(diag(H))

lev_fit = lm(y ~ ., data = lev_ex)
hatvalues(lev_fit)

# Find our coefs on lev fit and the max leverage point
coef(lev_fit)
which.max(hatvalues(lev_fit))
lev_ex[which.max(hatvalues(lev_fit)),]

# changing this to be a bigger outlier has a huge effect on our stuff
lev_ex_1 = lev_ex
lev_ex_1$y[1] = 20
lm(y ~ ., data = lev_ex_1)

# Outlier searching

# So we see our 11th point, the added one has a large standardized residual
resid(model_1)
rstandard(model_1)
rstandard(model_1)[abs(rstandard(model_1)) > 2]

# Small SR for our 11th point so it doesn't have much effect on the fitting
resid(model_2)
rstandard(model_2)
rstandard(model_2)[abs(rstandard(model_2)) > 2]

# Large SR again, smaller than model 1 and has high leverage so it effects the fitting a lot
resid(model_3)
rstandard(model_3)
rstandard(model_3)[abs(rstandard(model_3)) > 2]

# Cooks distance will check if we have a highly influential point, like in model 3
cooks.distance(model_1)[11] > 4 / length(cooks.distance(model_1))
cooks.distance(model_2)[11] > 4 / length(cooks.distance(model_2))
cooks.distance(model_3)[11] > 4 / length(cooks.distance(model_3))

# Car example with analysis 
mpg_hp_add = lm(mpg ~ hp + am, data = mtcars)
plot(fitted(mpg_hp_add), resid(mpg_hp_add), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residual",
     main = "mtcars: Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
# BP test with sort of low value, might not be constant variance
bptest(mpg_hp_add)
# Check the qq plots, decent fit, lets see what happens with Shapiro
qqnorm(resid(mpg_hp_add), col = "darkgrey")
qqline(resid(mpg_hp_add), col = "dodgerblue", lwd = 2)
# Good shapiro p value, could be normal 
shapiro.test(resid(mpg_hp_add))
# some of the points have high residuals and should be considered
sum(hatvalues(mpg_hp_add) > 2 * mean(hatvalues(mpg_hp_add)))
# Some even have high leverage, let's check on these
sum(abs(rstandard(mpg_hp_add)) > 2)

# Here's our influential points
cd_mpg_hp_add = cooks.distance(mpg_hp_add)
sum(cd_mpg_hp_add > 4 / length(cd_mpg_hp_add))
large_cd_mpg = cd_mpg_hp_add > 4 / length(cd_mpg_hp_add)
cd_mpg_hp_add[large_cd_mpg]

coef(mpg_hp_add)
# let's drop and compare the coef with an without these cars
mpg_hp_add_fix = lm(mpg ~ hp + am,
                    data = mtcars,
                    subset = cd_mpg_hp_add <= 4 / length(cd_mpg_hp_add))
coef(mpg_hp_add_fix)

# Nothing huge changes

par(mfrow = c(2, 2))
plot(mpg_hp_add)

