## Lab 3 ##

# Exercise 1
# Part 1

# Includes
library(tidyverse)
library(ggplot2)
library(scales)

# Read in the data set with headers TRUE
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

# Finds integer data
int_type = lapply(ameslist, class)
# Generates a dataframe from ames data with all integer type data
Ames = ameslist[int_type=='integer']
# Drop columns that don't have meaning
Ames$MSSubClass = NULL
# Drops overallcond and overallqual
Ames = Ames[ , !(names(Ames) %in% c('OverallCond', 'OverallQual'))]

# Part 2
Ames = na.omit(Ames)
# Split our data into train and test
# Makes forward selection plots up to complexity 15
lm.fit.Comp1 = lm(SalePrice ~ GrLivArea, data=Ames)

lm.fit.Comp3 = lm(SalePrice ~ GrLivArea + TotRmsAbvGrd + MasVnrArea, data=Ames)

lm.fit.Comp5 = lm(SalePrice ~ GrLivArea + LotArea + MasVnrArea + TotalBsmtSF +
                    TotRmsAbvGrd, data=Ames)

lm.fit.Comp10 = lm(SalePrice ~ GrLivArea + LotArea + MasVnrArea + TotalBsmtSF + TotRmsAbvGrd + 
                     GarageArea + X1stFlrSF + X2ndFlrSF + FullBath + Fireplaces, data=Ames)

lm.fit.Comp15 = lm(SalePrice ~ GrLivArea + LotArea + MasVnrArea + TotalBsmtSF + TotRmsAbvGrd + 
                     GarageArea + X1stFlrSF + X2ndFlrSF + FullBath + Fireplaces + 
                     HalfBath + BedroomAbvGr + GarageCars + YearBuilt + WoodDeckSF, data=Ames)

# Part 3

# Grab our functions
# RMSE function
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
# Complexity function
get_complexity = function(model) {
  length(coef(model)) - 1
}

pred.Comp1 = predict(lm.fit.Comp1)
rmse.Comp1 = rmse(Ames$SalePrice, pred.Comp1)

pred.Comp3 = predict(lm.fit.Comp3)
rmse.Comp3 = rmse(Ames$SalePrice, pred.Comp3)

pred.Comp5 = predict(lm.fit.Comp5)
rmse.Comp5 = rmse(Ames$SalePrice, pred.Comp5)

pred.Comp10 = predict(lm.fit.Comp10)
rmse.Comp10 = rmse(Ames$SalePrice, pred.Comp10)

pred.Comp15 = predict(lm.fit.Comp15)
rmse.Comp15 = rmse(Ames$SalePrice, pred.Comp15)

comp_rmse_plt = plot(c(1,3,5,10,15), 
                     c(rmse.Comp1,rmse.Comp3, rmse.Comp5, rmse.Comp10, rmse.Comp15),
                     main='RMSE vs Complexity in Ames', ylab='RMSE', xlab='Complexity')

# We can see a clear drop in RMSE with the rise in complexity. This is exactly what we would expect
# and should continue until some point where we capture the least bias without adding too many
# and increasing the variance. We haven't reached that point with the complexity at 15, but it
# could be that we are using poor categories for prediction.

# Splitting into train and test sets
set.seed(9)
num_obs = nrow(Ames)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = Ames[train_index, ]
test_data = Ames[-train_index, ]

# Now we do some analysis on our training data to fit the model for predictions and 
# analyze what RMSE does with more complexity
fit_0 = lm(SalePrice ~ 1, data = train_data)
get_complexity(fit_0)

# train RMSE
sqrt(mean((train_data$SalePrice - predict(fit_0, train_data)) ^ 2))
# test RMSE
sqrt(mean((test_data$SalePrice - predict(fit_0, test_data)) ^ 2))

# Here's an improvement for RMSE
get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}

# Now we demo the function
get_rmse(model = fit_0, data = train_data, response = "SalePrice") # train RMSE
get_rmse(model = fit_0, data = test_data, response = "SalePrice") # test RMSE

# Let's try another model
get_rmse(model = lm.fit.Comp3, data = train_data, response = "SalePrice")
get_rmse(model = lm.fit.Comp3, data = test_data, response = "SalePrice")

# It uses the function rmse we made earlier and expands the functionality to make it easier to 
# understand what's happening and take in a wider variety of data.
# Let's redefine our models from earlier with a fit_1 through fit_5

fit_1 = lm.fit.Comp1
fit_2 = lm.fit.Comp3
fit_3 = lm.fit.Comp5
fit_4 = lm.fit.Comp10
fit_5 = lm.fit.Comp15

model_list = list(fit_1, fit_2, fit_3, fit_4, fit_5)

# Perform our training and testing over the list of fits we made
train_rmse = sapply(model_list, get_rmse, data = train_data, response = "SalePrice")
test_rmse = sapply(model_list, get_rmse, data = test_data, response = "SalePrice")
model_complexity = sapply(model_list, get_complexity)

# This loops over the fits in our model_list and runs it on over our function
?sapply

# plots our train and test rmse over the complexity range.
plot(model_complexity, train_rmse, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)) - 0.02,
              max(c(train_rmse, test_rmse)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(model_complexity, test_rmse, type = "b", col = "darkorange")

# This shows us that the most complex model we have is the best model we've modeled because it 
# has the smallest RMSE on both the train and test data.

## Exercise 2 ##

















