



urlfile = 'https://raw.githubusercontent.com/MSUDataAnalytics/SSC442/master/Labs/data/bank.csv'
bank = read.csv(url(urlfile))
names(bank)

# full_mpg_model = lm(mpg ~ wt + year + cyl + disp + hp + acc, data = autompg)
full_bank_model = lm(balance ~ ., data = bank)


summary(full_bank_model)

Update_bank_model = lm(balance ~ age + marital + default + loan, data = bank)

anova(full_bank_model,Update_bank_model)

summary(Update_bank_model)

# Best model is the Update_bank_model, has t-tests above 2 for all and highest F-stat of all summary data.
# Futhermore coeffiecnts like Job and Month were removed as along with being insiginfigant they were diffuicult to interpret
# and therefore useless.
# The final model we put forth is not a good model to attempt to anaylze the effects that increases or decreases ones
# bank balance. This can be seen by the miniscule R^2 value and also as our model likely suffers from omitted variable
# bias