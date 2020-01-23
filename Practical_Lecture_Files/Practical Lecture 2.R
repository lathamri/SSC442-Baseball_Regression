# read the data from the web
autompg = read.table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",
  quote = "\"",
  comment.char = "",
  stringsAsFactors = FALSE)
# give the dataframe headers
colnames(autompg) = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year", "origin", "name")
# remove missing data, which is stored as "?"
autompg = subset(autompg, autompg$hp != "?")
# remove the plymouth reliant, as it causes some issues
autompg = subset(autompg, autompg$name != "plymouth reliant")
# give the dataset row names, based on the engine, year and name
rownames(autompg) = paste(autompg$cyl, "cylinder", autompg$year, autompg$name)
# remove the variable for name, as well as origin
autompg = subset(autompg, select = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year"))
# change horsepower from character to numeric
autompg$hp = as.numeric(autompg$hp)
# check final structure of data
str(autompg)
# 3D Plots for higher dimensions
library("plot3D")

x = autompg$wt
y = autompg$year
z = autompg$mpg

fit <- lm(z ~ x + y)

grid.lines = 25
x.pred     = seq(min(x), max(x), length.out = grid.lines)
y.pred     = seq(min(y), max(y), length.out = grid.lines)
xy         = expand.grid(x = x.pred, y = y.pred)

z.pred = matrix(predict(fit, newdata = xy),
                nrow = grid.lines, ncol = grid.lines)

fitpoints = predict(fit)

scatter3D(x, y, z, pch = 19, cex = 2, col = gg.col(1000), lighting = TRUE,
          theta = 25, phi = 45, ticktype = "detailed",
          xlab = "wt", ylab = "year", zlab = "mpg", zlim = c(0, 40), clim = c(0, 40),
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = NA, fit = fitpoints), main = "")
# R Solves for our coefficients
mpg_model = lm(mpg ~ wt + year, data = autompg)
coef(mpg_model)
# Verify R has done the right thing with Linear Algebra Stuff
n = nrow(autompg)
p = length(coef(mpg_model))
X = cbind(rep(1, n), autompg$wt, autompg$year)
y = autompg$mpg

(beta_hat = solve(t(X) %*% X) %*% t(X) %*% y)
coef(mpg_model)
# Summarize our error rates
summary(mpg_model)$sigma
# Verify the math with more Linear Algebra
y_hat = X %*% solve(t(X) %*% X) %*% t(X) %*% y
e     = y - y_hat
sqrt(t(e) %*% e / (n - p))
sqrt(sum((y - y_hat) ^ 2) / (n - p))
# Confidence Interval
confint(mpg_model, level = 0.99)
# SLR Dummy Data
new_cars = data.frame(wt = c(3500, 5000), year = c(76, 81))
new_cars
# Prediction of the new data with confidence interval
predict(mpg_model, newdata = new_cars, interval = "confidence", level = 0.99)
# One is outside our weight/year range, OH NO
new_cars$wt
range(autompg$wt)
new_cars$year
range(autompg$year)
# Plots some stuff to help us see whats wrong with our data, known as hidden extrapolation
plot(year ~ wt, data = autompg, pch = 20, col = "dodgerblue", cex = 1.5)
points(new_cars, col = "darkorange", cex = 3, pch = "X")
# Get an estimate using our x0 vector for the new car and regressing on beta_hat
x0 = c(1, 3500, 76)
x0 %*% beta_hat
# This helps us extract values of beta_hat if we need to
beta_hat
x0 = c(0, 0, 1)
x0 %*% beta_hat
# Finding our decomp variation works in MLR as well just like SLR, Higher r^2 gives better estimates
summary(mpg_model)$r.squared

# ANOVA table for null and alt hypothesis comparison
null_mpg_model = lm(mpg ~ 1, data = autompg)
full_mpg_model = lm(mpg ~ wt + year, data = autompg)
anova(null_mpg_model, full_mpg_model)
summary(mpg_model)

# SSReg
sum((fitted(full_mpg_model) - fitted(null_mpg_model)) ^ 2)
# SSE
sum(resid(full_mpg_model) ^ 2)
# SST
sum(resid(null_mpg_model) ^ 2)
# Degrees of Freedom: Regression
length(coef(full_mpg_model)) - length(coef(null_mpg_model))
# Degrees of Freedom: Error
length(resid(full_mpg_model)) - length(coef(full_mpg_model))
# Degrees of Freedom: Total
length(resid(null_mpg_model)) - length(coef(null_mpg_model))

# Shows all possible recorded data for our Full model
names(autompg)
# Compare wt and year to full model to test for significance of the subset 

null_mpg_model = lm(mpg ~ wt + year, data = autompg)
# full_mpg_model = lm(mpg ~ wt + year + cyl + disp + hp + acc, data = autompg)
full_mpg_model = lm(mpg ~ ., data = autompg)
anova(null_mpg_model, full_mpg_model)
# Low F-test value for 0.5533 so we maintain the null hypothesis that the added stuff doesn't matter

# Simulation for this
set.seed(1337)
n = 100 # sample size
p = 3
beta_0 = 5
beta_1 = -2
beta_2 = 6
sigma  = 4
# Matrix set up
x0 = rep(1, n)
x1 = sample(seq(1, 10, length = n))
x2 = sample(seq(1, 10, length = n))
X = cbind(x0, x1, x2)
C = solve(t(X) %*% X)
# Define variables and model form
eps      = rnorm(n, mean = 0, sd = sigma)
y        = beta_0 + beta_1 * x1 + beta_2 * x2 + eps
sim_data = data.frame(x1, x2, y)
# Plot the simulation
# make this use data.frame? or, simply hide this?
fit = lm(y ~ x1 + x2)

grid.lines = 25
x1.pred = seq(min(x1), max(x1), length.out = grid.lines)
x2.pred = seq(min(x2), max(x2), length.out = grid.lines)
x1x2 = expand.grid(x1 = x1.pred, x2 = x2.pred)

y.pred = matrix(predict(fit, newdata = x1x2),
                nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints = predict(fit)

# scatter plot with regression plane
scatter3D(x1, x2, y, pch = 20, cex = 2, col = gg.col(1000), lighting = TRUE,
          theta = 45, phi = 15, ticktype = "detailed", zlim = c(min(y.pred), max(y.pred)), clim = c(min(y.pred), max(y.pred)),
          xlab = "x1", ylab = "x2", zlab = "y",
          surf = list(x = x1.pred, y = x2.pred, z = y.pred,
                      facets = NA, fit = fitpoints), main = "")
# Run some estimator stuff to verify
(beta_hat = C %*% t(X) %*% y)
coef(lm(y ~ x1 + x2, data = sim_data))
# THEY"RE THE SAME 
c(beta_0, beta_1, beta_2)
# Now we test to make sure the std err is right
y_hat = X %*% beta_hat
(s_e = sqrt(sum((y - y_hat) ^ 2) / (n - p)))
summary(lm(y ~ x1 + x2, data = sim_data))$sigma
# And they're the same cool


