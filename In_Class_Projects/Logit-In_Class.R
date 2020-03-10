library(tidyverse)
library(ggplot2)
library(scales)
library(effects)

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

# Drop NA's from our df
Ames = na.omit(Ames)

Ames$Fireplaces_Bin = NULL
Ames$Fireplaces_Bin <- ifelse(Ames$Fireplaces > 0, 1, 0)

# Split our testing and training data
set.seed(9)
num_obs = nrow(Ames)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = Ames[train_index, ]
test_data = Ames[-train_index, ]
test_data<- test_data[-seq(nrow(test_data),nrow(test_data)-0),]

# Creates Binary Class

logit_mod = glm(formula = Fireplaces_Bin~LotArea+OverallQual+OverallCond+YearRemodAdd+
                  GrLivArea,
    family=binomial(), data=train_data)

summary(logit_mod)

step(glm(Fireplaces_Bin~OverallCond+OverallQual+YearRemodAdd+GrLivArea+LotArea+
           WoodDeckSF+GarageArea+X2ndFlrSF+YearRemodAdd+
           MasVnrArea, data=train_data),
     scope=~OverallCond+OverallQual+YearRemodAdd+GrLivArea+LotArea+
       WoodDeckSF+GarageArea+X2ndFlrSF+YearRemodAdd+
       MasVnrArea, direction = 'both')















