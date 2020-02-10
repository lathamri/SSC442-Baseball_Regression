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
# Makes forward selection plots up to complexity 15
lm.fit.Comp1 = lm(SalePrice ~ GrLivArea, data=Ames)

lm.fit.Comp2 = lm(SalePrice ~ GrLivArea + TotRmsAbvGrd, data=Ames)

lm.fit.Comp3 = lm(SalePrice ~ GrLivArea + TotRmsAbvGrd + MasVnrArea, data=Ames)

lm.fit.Comp4 = lm(SalePrice ~ GrLivArea + LotArea + MasVnrArea + TotalBsmtSF, data=Ames)

lm.fit.Comp5 = lm(SalePrice ~ GrLivArea + LotArea + MasVnrArea + TotalBsmtSF +
                    TotRmsAbvGrd, data=Ames)

lm.fit.Comp6 = lm(SalePrice ~ GrLivArea + LotArea + MasVnrArea + TotalBsmtSF + TotRmsAbvGrd + 
                     GarageArea, data=Ames)

lm.fit.Comp7 = lm(SalePrice ~ GrLivArea + LotArea + MasVnrArea + TotalBsmtSF + TotRmsAbvGrd + 
                     GarageArea + X1stFlrSF, data=Ames)

lm.fit.Comp8 = lm(SalePrice ~ GrLivArea + LotArea + MasVnrArea + TotalBsmtSF + TotRmsAbvGrd + 
                     GarageArea + X1stFlrSF + X2ndFlrSF, data=Ames)

lm.fit.Comp9 = lm(SalePrice ~ GrLivArea + LotArea + MasVnrArea + TotalBsmtSF + TotRmsAbvGrd + 
                     GarageArea + X1stFlrSF + X2ndFlrSF + FullBath, data=Ames)

lm.fit.Comp10 = lm(SalePrice ~ GrLivArea + LotArea + MasVnrArea + TotalBsmtSF + TotRmsAbvGrd + 
                     GarageArea + X1stFlrSF + X2ndFlrSF + FullBath + Fireplaces, data=Ames)

lm.fit.Comp11 = lm(SalePrice ~ GrLivArea + LotArea + MasVnrArea + TotalBsmtSF + TotRmsAbvGrd + 
                     GarageArea + X1stFlrSF + X2ndFlrSF + FullBath + Fireplaces + 
                     HalfBath, data=Ames)

lm.fit.Comp12 = lm(SalePrice ~ GrLivArea + LotArea + MasVnrArea + TotalBsmtSF + TotRmsAbvGrd + 
                     GarageArea + X1stFlrSF + X2ndFlrSF + FullBath + Fireplaces + 
                     HalfBath + BedroomAbvGr, data=Ames)

lm.fit.Comp13 = lm(SalePrice ~ GrLivArea + LotArea + MasVnrArea + TotalBsmtSF + TotRmsAbvGrd + 
                     GarageArea + X1stFlrSF + X2ndFlrSF + FullBath + Fireplaces + 
                     HalfBath + BedroomAbvGr + GarageCars, data=Ames)

lm.fit.Comp14 = lm(SalePrice ~ GrLivArea + LotArea + MasVnrArea + TotalBsmtSF + TotRmsAbvGrd + 
                     GarageArea + X1stFlrSF + X2ndFlrSF + FullBath + Fireplaces + 
                     HalfBath + BedroomAbvGr + GarageCars + YearBuilt, data=Ames)

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
fit_2 = lm.fit.Comp2
fit_3 = lm.fit.Comp3
fit_4 = lm.fit.Comp4
fit_5 = lm.fit.Comp5

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

# 1. Plotting all 15 models

lm.fit.All = lm(SalePrice ~ . + GrLivArea:BsmtFinSF1 + GrLivArea:KitchenAbvGr, data=Ames)

model_comp = list(lm.fit.Comp1, lm.fit.Comp2, lm.fit.Comp3, lm.fit.Comp4, lm.fit.Comp5,
                  lm.fit.Comp6, lm.fit.Comp7, lm.fit.Comp8, lm.fit.Comp9, lm.fit.Comp10,
                  lm.fit.Comp11, lm.fit.Comp12, lm.fit.Comp13, lm.fit.Comp14, lm.fit.Comp15,
                  lm.fit.Comp19, lm.fit.All)

# Perform our training and testing over the list of fits we made
train_rmse_15 = sapply(model_comp, get_rmse, data = train_data, response = "SalePrice")
test_rmse_15 = sapply(model_comp, get_rmse, data = test_data, response = "SalePrice")
model_complexity_15 = sapply(model_comp, get_complexity)
# Plot our training and testing RMSE over Complexity
plot(model_complexity_15, train_rmse_15, type = "b",
     ylim = c(min(c(train_rmse_15, test_rmse_15)) - 0.02,
              max(c(train_rmse_15, test_rmse_15)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(model_complexity_15, test_rmse_15, type = "b", col = "darkorange")

# 2. Let's do some training
# First we run a correlation matrix across all relevant variables and use some intuition along with 
# any significant correlation we find to narrow down our variables we want. This is so
# that when we test our models they aren't computationally lethal as you'll see in a second.
# Now that we have our parred down data set we will create a model that checks all combinations
# of linear models.
# lm(y~x1), lm(y~x1 + x2), lm(y~x1 + x3) ... 
# This can give us measures like F-stat and p-values to compare across models as well as 
# RMSE so we will have the best possible model.

cor_matrix = round(cor(Ames), digits=3)
Scatter_Ames = pairs(Ames[,c(2,3,6,7,8,9,10,11,12,13,
                             14,15,16,17,18,19,20,21,22)], pch = 19,lower.panel = NULL)

# This function checks the r_sqr for a simple linear model for each value
# It should not be used as is and needs added complexity to be of value.
# Good for parring down data as as first pass.

for (parameter in c(1:34)){
  lm.fit = summary(lm(SalePrice ~ train_data[,c(parameter)], data = train_data))
  r_sqr = lm.fit$r.squared # gets the t-value for each fit
  if (r_sqr > 0.05){
    append(test_model, r_sqr)
    print(r_sqr)
    print(colnames(Ames[c(parameter)])) 
  }
}


test_model = lm(SalePrice ~ LotFrontage + LotArea + YearBuilt + YearRemodAdd +
                  MasVnrArea + BsmtFinSF1 + BsmtUnfSF + TotalBsmtSF + X1stFlrSF +
                  X2ndFlrSF + GrLivArea + BsmtFullBath + FullBath + HalfBath +
                  TotRmsAbvGrd + Fireplaces + GarageYrBlt + GarageCars + GarageArea +
                  WoodDeckSF + OpenPorchSF, data=train_data)
print(test_model)
# Drop the colnames displayed when this is run and repeat


get_rmse(test_model, data = test_data,response = "SalePrice")
summary(test_model)

library(data.table)
outlierReplace = function(dataframe, cols, rows, newValue = NA){
  if (any(rows)){
    set(dataframe, rows, cols, newValue)
  }
}

outlierReplace(train_data, "SalePrice", which(train_data$SalePrice>500000, NA))


lm.fit.All = lm(SalePrice ~ . - GrLivArea  - TotalBsmtSF +
                GrLivArea:BsmtFinSF1 + GrLivArea:KitchenAbvGr, data=train_data)

summary(lm.fit.All)

get_rmse(lm.fit.All, data = test_data,response = "SalePrice")




