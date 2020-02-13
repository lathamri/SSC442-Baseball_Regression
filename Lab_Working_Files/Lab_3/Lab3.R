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

# Finds integer data - save to variable for indexing.
int_type = lapply(ameslist, class)

# Generates a dataframe from ames data with all integer type data
Ames = ameslist[int_type=='integer']

# Drop columns that don't have meaning
Ames$MSSubClass = NULL

# Drops the columns overallcond and overallqual
# Basically re-saving Ames as Ames without (!) these two columns, but keeping all rows.
Ames = Ames[ , !(names(Ames) %in% c('OverallCond', 'OverallQual'))]

# Part 2

# Drop NA's from our df
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

## Grab our functions ##

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
                     c(rmse.Comp1, rmse.Comp3, rmse.Comp5, rmse.Comp10, rmse.Comp15),
                     main='RMSE vs Complexity in Ames', ylab='RMSE', xlab='Complexity')


####################################
## how does a full model compare? ##
####################################

# Fit the model to all predictors
lm.fit.full = lm(SalePrice ~ ., data=Ames)

# Use this model to predict values
pred.full = predict(lm.fit.full)

# run the prediction and actual values through rmse function
rmse.full = rmse(Ames$SalePrice, pred.full)

# Graph the previous data points with the full-fit model
full.rmse = plot(c(1,3,5,10,15, get_complexity(lm.fit.full)), 
     c(rmse.Comp1, rmse.Comp3, rmse.Comp5, rmse.Comp10, rmse.Comp15, rmse.full),
     main='RMSE vs Complexity in Ames', ylab='RMSE', xlab='Complexity')

# Using a full model will always be the 'best fit' as it contains all the predictors. However, 
# this is considered cheating, as with any simple lineaer model, the RMSE will decrease
# with increasing complexity. This is called 'overfitting' the data.

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

test_data<- test_data[-seq(nrow(test_data),nrow(test_data)-0),]

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

# It used the rmse function we made earlier and expanded the functionality to make it easier to 
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

## Underfitting models: In general High Train RMSE, High Test RMSE.
## Overfitting models: In general Low Train RMSE, High Test RMSE.


################
## Exercise 2 ##
################


# 1. Plotting all 15 models
# Compile all the fits into a list
model_comp = list(lm.fit.Comp1, lm.fit.Comp2, lm.fit.Comp3, lm.fit.Comp4, lm.fit.Comp5,
                  lm.fit.Comp6, lm.fit.Comp7, lm.fit.Comp8, lm.fit.Comp9, lm.fit.Comp10,
                  lm.fit.Comp11, lm.fit.Comp12, lm.fit.Comp13, lm.fit.Comp14, lm.fit.Comp15)

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
legend("topright", 
       legend = c("Train", "Test"), 
       col = c('blue', 
               'darkorange'), 
       pch = c(19,19), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

# 2. Let's do some training

# First we run a correlation matrix across all relevant variables and use some intuition along with 
# any significant correlation we find to narrow down our variables we want.
# Now that we have our parred down data set we will create a model that checks combinations
# of linear models.
# lm(y~x1), lm(y~x1 + x2), lm(y~x1 + x3) ... 
# This can give us measures like F-stat and p-values to compare across models as well as 
# RMSE so we will have the best possible model.

cor_matrix = round(cor(Ames), digits=3)
Scatter_Ames = pairs(Ames[,c(2,3,6,7,8,9,10,11,12,13,
                             14,15,16,17,18,19,20,21,22)], pch = 19,lower.panel = NULL)

# This function checks the r_sqr for a simple linear model for each value
# Heatmap add any additional insite?
heatmap(cor_matrix, scale = 'column')
library(GGally)
ggcorr(Ames)

get_rmse(test_model, data = test_data,response = "SalePrice")
summary(test_model)

lm.fit.All = lm(SalePrice ~ . - GrLivArea  - TotalBsmtSF +
                GrLivArea:BsmtFinSF1 + GrLivArea:KitchenAbvGr, data=train_data)

summary(lm.fit.All)

get_rmse(lm.fit.All, data = test_data,response = "SalePrice")


# Drop the colnames displayed when this is run and repeat


# Fit
lm.ryan = lm(SalePrice ~ GrLivArea + FullBath + LotFrontage + LotArea + WoodDeckSF + 
               GarageArea + GarageCars + YearRemodAdd:Fireplaces + TotRmsAbvGrd + BsmtFinSF1 +
               TotalBsmtSF + BsmtUnfSF + X1stFlrSF + X2ndFlrSF + BsmtFullBath + HalfBath +
               BedroomAbvGr + KitchenAbvGr + MasVnrArea + YearBuilt + ScreenPorch + YearRemodAdd , data=train_data)


#get_rmse(model = lm.ryan, data = train_data, response = "SalePrice")
get_rmse(model = lm.ryan, data = test_data, response = "SalePrice")


#lm.full = lm(SalePrice ~ ., data = train_data)
#lm.full
summary(lm.ryan)

model_comp_all = list(lm.fit.Comp1, lm.fit.Comp2, lm.fit.Comp3, lm.fit.Comp4, lm.fit.Comp5,
                  lm.fit.Comp6, lm.fit.Comp7, lm.fit.Comp8, lm.fit.Comp9, lm.fit.Comp10,
                  lm.fit.Comp11, lm.fit.Comp12, lm.fit.Comp13, lm.fit.Comp14, lm.fit.Comp15, 
                  lm.ryan, lm.fit.full)

# Perform our training and testing over the list of fits we made

train_rmse_all = sapply(model_comp_all, get_rmse, data = train_data, response = "SalePrice")
test_rmse_all = sapply(model_comp_all, get_rmse, data = test_data, response = "SalePrice")
model_complexity_all = sapply(model_comp_all, get_complexity)

plot(model_complexity_all, train_rmse_all, type = "b",
     ylim = c(min(c(train_rmse_all, test_rmse_all)) - 0.02,
              max(c(train_rmse_all, test_rmse_all)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(model_complexity_all, test_rmse_all, type = "b", col = "darkorange")


# TESTING LINEAR MODELS WITH LOOPS #

# Uses R^2 as an evaluator

MAX = -Inf
max_r_param = NULL
param_vec = c(2:34)
regress_vec = c(35)
while (length(param_vec)>3){
  for (parameter in param_vec){
    lm.fit = summary(lm(SalePrice ~ ., data=subset(train_data,
                                                   select = union(c(parameter), regress_vec))))
    r_sqr = lm.fit$r.squared
    if (r_sqr > MAX){
      MAX = r_sqr
      max_r_param = colnames(train_data[c(parameter)])
    }
  }
  param_vec = param_vec[param_vec!=grep(max_r_param, colnames(train_data))]
  regress_vec = append(regress_vec, grep(max_r_param, colnames(train_data)))
}

test.lm.1 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14)))
test.lm.2 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10)))
test.lm.3 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4)))
test.lm.4 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19)))
test.lm.5 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24)))
test.lm.6 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                  7)))
test.lm.7 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                  7, 5)))
test.lm.8 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                  7, 5, 20)))
test.lm.9 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                  7, 5, 20, 3)))
test.lm.10 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22)))
test.lm.11= lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                  7, 5, 20, 3, 22, 6)))
test.lm.12 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6, 
                                                                   2)))
test.lm.13 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6, 
                                                                   2, 26)))
test.lm.14 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6, 
                                                                   2, 26, 25)))
test.lm.15 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21)))
test.lm.16 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33)))
test.lm.17 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33, 8)))
test.lm.18 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33, 8,
                                                                   28)))
test.lm.19 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33, 8,
                                                                   28, 13)))
test.lm.20 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33, 8,
                                                                   28, 13, 16)))
test.lm.21 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33, 8,
                                                                   28, 13, 16, 30)))
test.lm.22 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33, 8,
                                                                   28, 13, 16, 30, 11)))
test.lm.23 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33, 8,
                                                                   28, 13, 16, 30, 11, 
                                                                   18)))
test.lm.24 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33, 8,
                                                                   28, 13, 16, 30, 11, 
                                                                   18, 31)))
test.lm.25 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33, 8,
                                                                   28, 13, 16, 30, 11, 
                                                                   18, 31, 15)))
test.lm.26 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33, 8,
                                                                   28, 13, 16, 30, 11, 
                                                                   18, 31, 15, 17)))
test.lm.27 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33, 8,
                                                                   28, 13, 16, 30, 11, 
                                                                   18, 31, 15, 17, 23)))

test.lm.28 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33, 8,
                                                                   28, 13, 16, 30, 11, 
                                                                   18, 31, 15, 17, 23, 
                                                                   34)))
test.lm.29 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33, 8,
                                                                   28, 13, 16, 30, 11, 
                                                                   18, 31, 15, 17, 23, 
                                                                   34, 32)))
test.lm.30 = lm(SalePrice ~ ., data = subset(train_data, select = c(35, 14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33, 8,
                                                                   28, 13, 16, 30, 11, 
                                                                   18, 31, 15, 17, 23, 
                                                                   34, 32, 27)))
test.lm.31 = lm(train_data$SalePrice ~ ., data = subset(train_data, select = c(14, 10, 4, 19, 24,
                                                                   7, 5, 20, 3, 22, 6,
                                                                   2, 26, 25, 21, 33, 8,
                                                                   28, 13, 16, 30, 11, 
                                                                   18, 31, 15, 17, 23, 
                                                                   34, 32, 27, 29)))

rmse(test_data$SalePrice, predict(test.lm.15, newdata=test_data))

## MODEL WITH RMSE MIN

# This model gives linear models with of increasingly smaller RMSE.

MIN = Inf
min_r_param = NULL
min_param_vec = c(2:34)
min_regress_vec = c(35)
while (length(min_param_vec)>17){
  for (parameter in min_param_vec){
    lm.fit = lm(SalePrice ~ ., data=subset(train_data,
                                           select = union(c(parameter), min_regress_vec)))
    min_RMSE = rmse(train_data$SalePrice, predict(lm.fit, newdata=train_data))
    print(union(c(parameter), min_regress_vec))
    if (min_RMSE < MIN){
      MIN = min_RMSE
      print(MIN)
      min_r_param = colnames(train_data[c(parameter)])
    }
  }
  min_param_vec = min_param_vec[min_param_vec!=grep(min_r_param, colnames(train_data))]
  min_regress_vec = append(min_regress_vec, grep(min_r_param, colnames(train_data)))
  print('Added Regressor')
}

length(min_param_vec)
length(min_regress_vec)
print(min_regress_vec)

rmse_min_lm_fit = lm(train_data$SalePrice ~ .,
                      data = subset(train_data, select=c(14,10,4,19,25,20,
                                                         7,5,3,6,22,21,24,
                                                         2,26)))

rmse_min_lm_fit1 = lm(train_data$SalePrice ~ .,
                     data = subset(train_data, select=c(14,10,4,19,25,20,
                                                       7,5,3,6,22,21,24,
                                                       2,26,8,13)))

rmse_min_lm_fit2 = lm(train_data$SalePrice ~ .,
                     data = subset(train_data, select=c(14,10,4,19,25,20,
                                                        7,5,3,6,22,21,24,
                                                        2,26,8,13,30,28,33,
                                                        12,16,18)))
rmse_min_lm_fit3 = lm(train_data$SalePrice ~ .,
                      data = subset(train_data, select=c(14,10,4,19,25,20,
                                                         7,5,3,6,22,21,24,
                                                         2,26,8,13,30,28,33,
                                                         12,16,18,31,15,23,17,32,27,34)))

length(c(14,10,4,19,25,20,
         7,5,3,6,22,21,24,
         2,26))

rmse(test_data$SalePrice, predict(rmse_min_lm_fit, newdata=test_data))
rmse(test_data$SalePrice, predict(rmse_min_lm_fit1, newdata=test_data))
rmse(test_data$SalePrice, predict(rmse_min_lm_fit2, newdata=test_data))
rmse(test_data$SalePrice, predict(rmse_min_lm_fit3, newdata=test_data))
rmse(test_data$SalePrice, predict(test.lm.15, newdata=test_data))
rmse(test_data$SalePrice, predict(lm.ryan, newdata=test_data))


## Build check for poly and log on all values 

# Primary Function

# Initialze some data storage variables
MIN = Inf
min_r_param = NULL
min_r_param_int = NULL

# Initialize our parameter vector, for tracking which parameters we 
# should check and a regression vector for storing which ones 
# do the best.
min_param_vec = c(2:34)
min_regress_vec = c()

# Loops over our parameters until we have a specified amount left.
while (length(min_param_vec)>15){
  # Grabs our parameter of interest
  for (parameter in min_param_vec){
    # Creates a linear model of our parameters unioned with any parameters
    # that have already been placed into our regression. For example 
    # the first run it creates a model with only one regressor on salesprice
    # then once it finds the best one it adds this to our regressor vector
    # and removes it from the check parameter vector.
    lm.fit.norm = lm(train_data$SalePrice ~ ., data=subset(train_data,
                                           select = union(c(parameter), min_regress_vec)))
    # Find the RMSE for our linear model
    min_RMSE_norm = rmse(test_data$SalePrice, predict(lm.fit.norm, newdata=test_data))
    # Check if the RMSE we just found is less than our minimum RMSE for this loop
    # If it is then update the min and parameter holders. Otherwise pass.
    if (min_RMSE_norm < MIN){
      MIN = min_RMSE_norm
      print(MIN)
      min_r_param = colnames(train_data[c(parameter)])
    }
    # Same as above but with interaction variables
    lm.fit.int = lm(train_data$SalePrice ~ . +
                      train_data[[colnames(train_data[parameter])]]:
                      train_data[[colnames(train_data[parameter+1])]],
                    data=subset(train_data,
                                select = union(c(parameter), min_regress_vec)))
    min_RMSE_int = rmse(test_data$SalePrice, predict(lm.fit.int, newdata=test_data))
    if (min_RMSE_int < MIN){
      MIN = min_RMSE_int
      print(MIN)
      min_r_param = colnames(train_data[c(parameter)])
      min_r_param_int = colnames(train_data[c(parameter+1)])
      print(min_r_param)
      print(min_r_param_int)
      print('interaction')
    }
    # Same as above but with square root. Used sqrt instead of log because of 
    # 0 data.
    lm.fit.sqrt = lm(train_data$SalePrice ~ . +
                      sqrt(train_data[[colnames(train_data[parameter])]]),
                      data=subset(train_data,
                                select = union(c(parameter), min_regress_vec)))
    min_RMSE_sqrt = rmse(test_data$SalePrice, predict(lm.fit.sqrt, newdata=test_data))
    print(union(c(parameter), min_regress_vec))
    if (min_RMSE_sqrt < MIN){
      MIN = min_RMSE_sqrt
      print(MIN)
      min_r_param = colnames(train_data[c(parameter)])
      print(min_r_param)
      print('sqrt')
    }
  }
  # Once we loop over all parameters we take the one that lowers RMSE the most 
  # and remove it from the parameter check vector and add it to the regression vector
  # in these two calls.
  min_param_vec = min_param_vec[min_param_vec!=grep(min_r_param, colnames(train_data))]
  min_regress_vec = append(min_regress_vec, grep(min_r_param, colnames(train_data)))
  print('Added Regressor')
}

# Overfits a little
yyy = lm(train_data$SalePrice ~ . + 
           BsmtHalfBath:BsmtFullBath+
           sqrt(Fireplaces)+
           train_data$BsmtFinSF2:train_data$BsmtUnfSF+
           sqrt(WoodDeckSF)+
           train_data$YearBuilt:train_data$EnclosedPorch+
           train_data$YearBuilt:train_data$GarageYrBlt,
         data=subset(train_data, select=c(14,4,24,20,15,5,22,6,30,26,16,18,29,32,34)))

# Best model yet; tests to 47472
y = lm(train_data$SalePrice ~ .,
         data=subset(train_data, select=c(14,4,24,20,15,5,22,6,30,26,16,18,29,32,34)))

rmse(test_data$SalePrice, predict(yyy, newdata=test_data))
rmse(train_data$SalePrice, predict(yyy, newdata=train_data))


# Garbage that we made, it's nice garbage tho.
add_var_rmse = lm(train_data$SalePrice ~ . + 
                    train_data$BsmtUnfSF:train_data$BsmtFullBath+
                    train_data$YearBuilt:train_data$EnclosedPorch+
                    train_data$YearBuilt:train_data$GarageYrBlt+
                    train_data$TotalBsmtSF:train_data$X1stFlrSF,
                  data = subset(train_data, select=c(14,10,19,4,25,20,
                                                     7,5,3,6,22,21,24,
                                                     2,26)))

rmse(test_data$SalePrice, predict(add_var_rmse, newdata=test_data))
rmse(train_data$SalePrice, predict(add_var_rmse, newdata=train_data))

# Graph for the best model
plot(y)









