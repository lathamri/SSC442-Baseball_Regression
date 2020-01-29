## Lab 2

# Includes
library(tidyverse)
library(ggplot2)
library(scales)

# Read in the data set with headers TRUE
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

# Read in the data set with headers FALSE
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = FALSE,
                       sep = ",")
summary(ameslist)

# Notice how this clearly breaks the data by trying to force the headers to remain as part of the data. It 
# forces R to structure all of this as vectors with no specific data type.

# Re-Read in the data with headers True
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

# Info on read.table
?read.table

# Get some info on the data
names(ameslist)

# Tells us the data is stored as lists instead of a data frame, something to care for later
typeof(ameslist)

# Perhaps we want to make some analysis of the garage type as a predictor for price because of the typically
# bad weather in Ames
unique(ameslist$GarageType)
# Lets further break this down into any garage that makes you walk outside, and those that don't

# First we assign values to the garage types of 0 or 1
GarageTemp = model.matrix( ~ ameslist$GarageType - 1, data=ameslist$GarageType )
# Now we stitch this back together using cbind
ameslist <- cbind(ameslist, GarageTemp)
# Lets see why this doesn't work, perhaps it's the NA values in garagetemp being dropped?
sum(is.na(ameslist$GarageType))
# Notice that the amount of NA values in GarageType is 81, and that 1379+81=1460 which is the number of rows 
# we have in GarageType. So we must be dropping NA values when we make GarageTemp. Lets fix that below.

# save options default values
original_options = options()
# Write NA handling exception to keep NA rows for GarageType
current_options = options(na.action='na.pass')
# Build our matrix
GarageTemp = model.matrix( ~ ameslist$GarageType - 1, data=ameslist$GarageType )
# Create our cbind and update ameslist
ameslist <- cbind(ameslist, GarageTemp)
# Restore original options
options(original_options)
# Now we see the Bind gives us no trouble and we can change the options back so they don't mess with 
# any of our later code.
unique(GarageTemp)
# Now we change our values again to indicate which of these garage types are outside

## This doesn't work, asking Bushong Tuesday what's up with it
ameslist$GarageOutside <- ifelse(ameslist$`ameslist$GarageTypeDetchd` == 1 | ameslist$`ameslist$GarageTypeCarPort` == 1, 1, 0)
unique(ameslist$GarageOutside)

vector = c(which(is.na(ameslist$GarageOutside)))

ameslist = ameslist[-c( vector ),]

unique(ameslist$GarageOutside)

## Exercise 1
# 1. Prune the data to only integers
# Generates a list of all data types for each of the columns in ameslist
int_type = lapply(ameslist, class)
# Generates a dataframe from ames data with all integer type data
Ames = ameslist[int_type=='integer']
# Drop columns that don't have meaning
Ames$MSSubClass = NULL
# 2. Produce a scatterplot matrix of 12 variables
Scatter_Ames = pairs(Ames[,c(37,3,4,12,13,14,16,19,20,23,26,32,33)], pch = 19,
                     lower.panel = NULL)
# 3. Produce a correlation matrix of the chosen variables
# Shows correlations for our chosen variables to SalesPrice
cor_matrix = round(cor(Ames[,c(37,3,4,12,13,14,16,19,20,23,26,32,33)]), digits=3)
# Yes this correlation matrix generally matches with my prior beleifs. Most are highly correlated meaning that they 
# have a large positive effect on Sales Price. The ones with a smaller correlation still have an effect just a smaller
# effect, but still measurable

# 4. Make a scatter plot of SalePrice to GrLivArea and use abline for a linear fit
p = ggplot(data=Ames,
           mapping = aes(x=Ames$GrLivArea, y=Ames$SalePrice))

j = lm(Ames$GrLivArea ~ Ames$SalePrice)

p + geom_smooth(method = lm) + geom_point()

# Modeling our data, Linear Model

attach(Ames)
lm.fit = lm(SalePrice ~ GrLivArea)
lm.fit

# GrLivArea is telling us about the size of the family room I think. It has a strong correlation with
# all of the above ground room counts, and square foot data as well.

plot(lm.fit)

summary(lm.fit)

# Plotting with more informed models
lm.fit = lm(SalePrice ~ GrLivArea + LotArea)

summary(lm.fit)

plot(lm.fit)

# Adding lot area should help us get better understandings of SalePrice which is oddly enouh not what we get.
# When we add in lotarea we actually get a lower F-stat and only a very small rise in R-Squared. This 
# indicates that our adding the variable is not a good explanatory variable for SalePrice

## Exercise 2
# 1. Make a simple linear model of garage type on sale price
GOonSP = lm(SalePrice~GarageOutside, data=ameslist)
GOonSP
# 2. Full model on SalePrice
FMonSP = lm(SalePrice~., data=Ames)
summary(FMonSP)

# The largest t-value's here are are related to the general size of the house or the quality
# of the amenities. For example the poolarea tells us there is a pool and that it's 
# influential on the price. Fireplaces are nearly at a t-value of 2 and the rest are all 
# about size, such as rooms above ground, garage cars, and the largest being overall quality.
# Most of the variables have no real effect outside of those.

# 3. Using plot on our model
plot(GOonSP)

# This is sort of a silly plot because it has binary estimator variables. The plots do suggest, as expected,
# that if you have a garage that is attached you'll have a house worth more money, but only a little.

plot(FMonSP)
# These plots are a little more telling and show a few outliers in the residual plots. The houses at
# 1299 and 524 have large negative residuals with high fitted values indicating they don't behave well in our 
# dataset and could be dropped out to give better estimates if we trust they are truly outliers.
# In the residual vs leverage graph you'll see we have houses at 1183, 1299, and nearly 524 passing over
# Cooks distance so we need to investigate them further, helping to lend some validity to the argument
# for dropping them out.

# 4. Finding interaction effects.
ModifonSP = lm(SalePrice~LotArea + OverallQual + OverallCond + YearBuilt + 
                 MasVnrArea + BsmtFinSF1 + X1stFlrSF + X2ndFlrSF + BsmtFullBath +
                 BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + GarageCars + WoodDeckSF +
                 ScreenPorch, data=Ames)
summary(ModifonSP)

# Here we've slimmed down the data set to include only the points that have t-values above 2 until all values 
# have this condition met. Now we will attempt to find interaction variables.
# Here we try grouping overall quality and condition to find interaction effects.
IntOQonOC = lm(SalePrice~LotArea + OverallQual*OverallCond + YearBuilt + 
                 MasVnrArea + BsmtFinSF1 + X1stFlrSF + X2ndFlrSF + BsmtFullBath +
                 BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + GarageCars + WoodDeckSF +
                 ScreenPorch, data=Ames)
summary(IntOQonOC)

# It doesn't do so well so we will remove this and see about linking the square footage of the floots
IntX1onX2 = lm(SalePrice~LotArea + OverallQual + OverallCond + YearBuilt + 
                 MasVnrArea + BsmtFinSF1 + X1stFlrSF + X2ndFlrSF + X1stFlrSF:X2ndFlrSF + BsmtFullBath +
                 BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + GarageCars + WoodDeckSF +
                 ScreenPorch, data=Ames)
summary(IntX1onX2)

# Unfortunately this has much the same effect as before. So let's try interactions with above ground rooms

IntAbvonAbv = lm(SalePrice~LotArea + OverallQual + OverallCond + YearBuilt + 
                 MasVnrArea + BsmtFinSF1 + X1stFlrSF + X2ndFlrSF + BsmtFullBath +
                 KitchenAbvGr + BedroomAbvGr:TotRmsAbvGrd+ GarageCars + WoodDeckSF +
                 ScreenPorch, data=Ames)
summary(IntAbvonAbv)

# Here we may have one that works. It yields a higher F-stat than our original model and this 
# makes sense because there is a direct correlation between bedrooms and total rooms above ground.
# Both of these should be and are correlated with SalePrice. So we keep this.

# 5. Let's try some transforms on the data
# Perhaps overallqual should be counted with a square since it's such a big part of buying a house.
TranXsqr = lm(SalePrice~LotArea + OverallQual + poly(OverallQual, 2) + OverallCond + YearBuilt + 
                MasVnrArea + BsmtFinSF1 + X1stFlrSF + X2ndFlrSF + BsmtFullBath +
                KitchenAbvGr + BedroomAbvGr:TotRmsAbvGrd+ GarageCars + WoodDeckSF +
                ScreenPorch, data=Ames)

summary(TranXsqr)
# This gives a much higher F-stat and both have high t-vales so we should think of keeping this in our analysis.

TranXlog = lm(SalePrice~LotArea + OverallQual + log(OverallQual) + OverallCond + YearBuilt + 
                MasVnrArea + BsmtFinSF1 + X1stFlrSF + X2ndFlrSF + BsmtFullBath +
                KitchenAbvGr + BedroomAbvGr:TotRmsAbvGrd+ GarageCars + WoodDeckSF +
                ScreenPorch, data=Ames)
summary(TranXlog)

# This does less well, but is still an increase, so this is something we want to explore in the data.
# What looks like is happening is the t-value is highly negative for the log while the regular variable
# has a highly positive t-value. This probably tells us that they do the same job and because we include
# them both it doubles up on them. Not what we want.

TranXsqrt = lm(SalePrice~LotArea + OverallQual + sqrt(OverallQual) + OverallCond + YearBuilt + 
                 MasVnrArea + BsmtFinSF1 + X1stFlrSF + X2ndFlrSF + BsmtFullBath +
                 KitchenAbvGr + BedroomAbvGr:TotRmsAbvGrd+ GarageCars + WoodDeckSF +
                 ScreenPorch, data=Ames)
summary(TranXsqrt)
# Similarly to the above value it looks like what it does is double up on both ends of the overallqual
# and one of these should be dropped out.


