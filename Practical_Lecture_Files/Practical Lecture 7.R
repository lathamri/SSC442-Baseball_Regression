library(ISLR)
library(tibble)
library(caret)
library(ellipse)
as_tibble(Default)

is.factor(Default$default)
is.factor(Default$student)

# Split the data
set.seed(42)
default_idx   = sample(nrow(Default), 5000)
default_trn = Default[default_idx, ]
default_tst = Default[-default_idx, ]

# Creates a density plot
# Tells us who is defaulting given their balance and income
featurePlot(x = default_trn[, c("balance", "income")],
            y = default_trn$default,
            plot = "density",
            scales = list(x = list(relation = "free"),
                          y = list(relation = "free")),
            adjust = 1.5,
            pch = "|",
            layout = c(2, 1),
            auto.key = list(columns = 2))

# Creates a density plot
# Tells us who is a student given balance and income
featurePlot(x = default_trn[, c("balance", "income")],
            y = default_trn$student,
            plot = "density",
            scales = list(x = list(relation = "free"),
                          y = list(relation = "free")),
            adjust = 1.5,
            pch = "|",
            layout = c(2, 1),
            auto.key = list(columns = 2))
# Tells us that students have a super distinct income compared to all population

# Creates a matrix of plots showing who defaults by color
# Shows correlation between those who are students, balance, and income
featurePlot(x = default_trn[, c("student", "balance", "income")],
            y = default_trn$default,
            plot = "pairs",
            auto.key = list(columns = 2))

# Adds circles around who is defaulting and who isn't but allows
# some wiggle room for outliers.
featurePlot(x = default_trn[, c("balance", "income")],
            y = default_trn$default,
            plot = "ellipse",
            auto.key = list(columns = 2))

# boundary rule
# Check some boundary and then calissfy based on that 
simple_class = function(x, boundary, above = 1, below = 0) {
  ifelse(x > boundary, above, below)
}

# Classify with our simple_class if people default based on balance
# The boundary condition is checking if they have a balance of higher than $1400
default_trn_pred = simple_class(x = default_trn$balance,
                                boundary = 1400, above = "Yes", below = "No")
default_tst_pred = simple_class(x = default_tst$balance,
                                boundary = 1400, above = "Yes", below = "No")
head(default_tst_pred, n = 10)

# Arrange our predictions on a table. (confusion matrix)
(trn_tab = table(predicted = default_trn_pred, actual = default_trn$default))
(tst_tab = table(predicted = default_tst_pred, actual = default_tst$default))

# Using the confusion matrix call from caret gives even more info
trn_con_mat  = confusionMatrix(trn_tab, positive = "Yes")
(tst_con_mat = confusionMatrix(tst_tab, positive = "Yes"))

# Gives us how accurate our model is using error rates, if it hits an error
# it gives us a 1 an averages this over the total data passed and gives us an average
# error rate. Then accuracy is 1 - this.
1 - trn_con_mat$overall["Accuracy"]
1 - tst_con_mat$overall["Accuracy"]

# Another helpful metric is the sensitivity an specificity
# Sensitivity is measuring the True Positives
# Given as TruePositives/(TruePositives + FalseNegatives)
tst_con_mat$byClass["Sensitivity"]
# Specificity is measuring the True Negatives
# Given as TrueNegatives/(TrueNegatives + FalsePositives)
tst_con_mat$byClass["Specificity"]

# Prevalence tells us about the balance of the guesses
# It uses (TruePositives + FalseNegatives)/TotalObservations
trn_con_mat$byClass["Prevalence"]
tst_con_mat$byClass["Prevalence"]

# This is predicting nobody defaults regardless of boundary
pred_all_no = simple_class(default_tst$balance,
                           boundary = 1400, above = "No", below = "No")
table(predicted = pred_all_no, actual = default_tst$default)

# Confusion matrix won't work because it's only one row
# We find errors directly
calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}

calc_class_err(actual = default_tst$default,
               predicted = pred_all_no)

# We get the exact same as the prevalence before.
# So what we really want is to predict better than 0.033
table(default_tst$default) / length(default_tst$default)






