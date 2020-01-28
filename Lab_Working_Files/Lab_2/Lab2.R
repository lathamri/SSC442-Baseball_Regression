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

# Now we change our values again to indicate which of these garage types are outside

## This doesn't work, asking Bushong Tuesday what's up with it
ameslist$GarageOutside <- ifelse(ameslist$GarageTypeDetchd == 1 | ameslist$GarageTypeCarPort == 1, 1, 0)
unique(ameslist$GarageOutside)

## Exercise 1
# 1. Prune the data to only integers
# Generates a list of all data types for each of the columns in ameslist
int_type = lapply(ameslist, class)
# Generates a dataframe from ames data with all integer type data
Ames = ameslist[int_type=='integer']
# Drop some columns that don't have meaning
Ames$MSSubClass = NULL
# 2. Produce a scatterplot matrix of 12 variables
Scatter_Ames = pairs(Ames[,c(3,4,5,12,13,14,19,20,23,26,32,33)], pch = 19,
                     lower.panel = NULL)
# 3. Produce a correlation matrix of the chosen variables
# Shows correlations for our chosen variables to SalesPrice
cor_matrix = round(cor(Ames[ ,c(3,4,5,12,13,14,19,20,23,26,32,33)]), digits=3)
# 4. Make a scatter plot of SalePrice to GrLivArea and use abline for a linear fit
p = ggplot(data=Ames,
           mapping = aes(x=Ames$GrLivArea, y=Ames$SalePrice))

# abline doesn't wanna work for me, look for fixes if you can
p  + geom_point() + geom_abline()






