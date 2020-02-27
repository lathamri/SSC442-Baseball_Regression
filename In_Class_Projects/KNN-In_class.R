library(tidyverse)
library(ggplot2)
library(scales)
library(caret)
library(class)

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
# Drop NA's from our df
Ames = na.omit(Ames)

# Split our testing and training data
set.seed(9)
num_obs = nrow(Ames)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = Ames[train_index, ]
test_data = Ames[-train_index, ]

test_data<- test_data[-seq(nrow(test_data),nrow(test_data)-0),]



KNN_reg_trn = knn(subset(train_data, select = c(14,4,24,20,15,10)),
                  subset(test_data, select = c(14,4,24,20,15,10)), cl=train_data$Fireplaces,
                  k=60)
KNN_reg_tst = knn(subset(train_data, select = c(14,4,24,20,15,10)),
                  subset(test_data, select = c(14,4,24,20,15,10)), cl=test_data$Fireplaces,
                  k=60)

tst_tab = table(predicted = KNN_reg_tst, actual = test_data$Fireplaces)
tst_con_mat = confusionMatrix(tst_tab, positive = "Yes")

tst_con_mat


plot(KNN_reg_trn, type="b", xlab="Prediction", ylab="Count")


