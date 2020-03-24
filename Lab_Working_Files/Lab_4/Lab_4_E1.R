# Gives us our spam email dataset
library(kernlab)
data("spam")
tibble::as.tibble(spam)

# Tells us the type and what factors the spam includes that we will use
is.factor(spam$type)
levels(spam$type)

# We split our data into training and testing data
set.seed(42)
# spam_idx = sample(nrow(spam), round(nrow(spam) / 2))
spam_idx = sample(nrow(spam), 1000)
spam_trn = spam[spam_idx, ]
spam_tst = spam[-spam_idx, ]

# Below we define 4 models of increasing complexity
fit_caps = glm(type ~ capitalTotal,
               data = spam_trn, family = binomial)
fit_selected = glm(type ~ edu + money + capitalTotal + charDollar,
                   data = spam_trn, family = binomial)
fit_additive = glm(type ~ .,
                   data = spam_trn, family = binomial)
fit_over = glm(type ~ capitalTotal * (.),
               data = spam_trn, family = binomial, maxit = 50)

# Here we find the misclassification rates for each of the models
# training misclassification rate
mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)

# Here we perform a 5 fold cross validation for our data
library(boot)
set.seed(42)
cv.glm(spam_trn, fit_caps, K = 5)$delta[1]
cv.glm(spam_trn, fit_selected, K = 5)$delta[1]
cv.glm(spam_trn, fit_additive, K = 5)$delta[1]
cv.glm(spam_trn, fit_over, K = 5)$delta[1]

## Exercise 1 Part 1
# 1. The models are mostly underfit in fit_caps and fit_selected with fit_additive
# performing the best although likely overfit by a little. The last and most
# overfit model is the fit_over model.

cv.glm(spam_trn, fit_caps, K = 100)$delta[1]
cv.glm(spam_trn, fit_selected, K = 100)$delta[1]
cv.glm(spam_trn, fit_additive, K = 100)$delta[1]
cv.glm(spam_trn, fit_over, K = 100)$delta[1]

# 2. Using 100 folds gives a stronger estimate but leads to the same conclusions.

# Now we make a function to create confusion matrices
make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

# Here we store our predicted values for the testing data
spam_tst_pred = ifelse(predict(fit_additive, spam_tst) > 0,
                       "spam",
                       "nonspam")
spam_tst_pred = ifelse(predict(fit_additive, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")

# Now we use these predictions to make a confusion matrix
(conf_mat_50 = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type))
# This gives us the rate we predict each one
table(spam_tst$type) / nrow(spam_tst)

## Exercise 1 Part 2
# 1. Below we create a confusion matrix for the other 3 models.
spam_tst_pred.caps = ifelse(predict(fit_caps, spam_tst) > 0,
                       "spam",
                       "nonspam")
(conf_mat_caps = make_conf_mat(predicted = spam_tst_pred.caps, actual = spam_tst$type))

spam_tst_pred.selected = ifelse(predict(fit_selected, spam_tst) > 0,
                            "spam",
                            "nonspam")
(conf_mat_selected = make_conf_mat(predicted = spam_tst_pred.selected,
                                   actual = spam_tst$type))

spam_tst_pred.over = ifelse(predict(fit_over, spam_tst) > 0,
                            "spam",
                            "nonspam")
(conf_mat_over = make_conf_mat(predicted = spam_tst_pred.over, actual = spam_tst$type))

sensitivity = function(conf_mat){
  conf_mat[1]/(conf_mat[1]+conf_mat[2])
}

specificity = function(conf_mat){
  conf_mat[4]/(conf_mat[3]+conf_mat[4])
}

# 2. The best model is, as predicted with the misclassification rates, going to be the 
# fit_additive model. The underfitted models mark a lot of the non-spam data as 
# spam which is the terrible misclassification. While the other models approach a more
# reasonable answer they still have a lower specificity than the additive model.
#
# Importantly the additive model has both the highest rates for sensitivity and
# specificity without over fitting, meaning it classifies our data as best as it can
# without biasing towards the data with more observations.
# Below you see the sensitivity followed by the specificity for all of the matrices.
# It justifies choosing the additive over the underfitted models and we reference the 
# matrices for giving reason to believe the over fitted model is over fitting.

#additive
sensitivity(conf_mat_50)
specificity(conf_mat_50)

#caps
sensitivity(conf_mat_caps)
specificity(conf_mat_caps)

#selected
sensitivity(conf_mat_selected)
specificity(conf_mat_selected)

#over
sensitivity(conf_mat_over)
specificity(conf_mat_over)









