urlfile = 'https://raw.githubusercontent.com/MSUDataAnalytics/SSC442/master/Labs/data/bank.csv'
bank = read_csv(url(urlfile))
names(bank)

# full_mpg_model = lm(mpg ~ wt + year + cyl + disp + hp + acc, data = autompg)
full_bank_model = lm(balance ~ ., data = bank)

summary(full_bank_model)

Update_bank_model = lm(balance ~ age + marital + default + loan, data = bank)

summary(Update_bank_model)


# Best model is the Update_bank_model, has t-tests above 2 for all and highest F-stat of all summary data.