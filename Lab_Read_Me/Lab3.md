# Training and Testing: (Linear) Predictions

**Due Thursday, February 13**

This lab utilizes the “ames.csv” data [(found here)](/Labs/data/ames.csv). As a reminder, this is a dataset concerning home sales from Ames, Iowa. The end of this lab features a (lengthy) discussion about indexing in `R`; this is optional reading but highly encouraged to improve your coding.

## Linear Models

When using linear models in other classes, we often emphasize asymptotic results under distributional assumptions. That is, we make assumptions about the model in order to derive properties of large samples. This general approach is useful for creating and performing hypothesis tests. Frequently, when developing a linear regression model, part of our goal was to **explain** a relationship. However, this isn't

Now, we will ignore much of what we have learned (sorry) and instead simply use regression as a tool to **predict**. Instead of a model which supposedly explains relationships, we seek a model which minimizes **errors**.

![](/assets/regression.png)

To discuss linear models in the context of prediction, we return to the `Ames` data. Accordingly, you should utilize some of the early code from Lab 2 to hasten your progress in this lab.

### Assesing Model Accuracy

There are many metrics to assess the accuracy of a regression model. Most of these measure in some way the average error that the model makes. The metric that we will be most interested in is the root-mean-square error.

$$
\text{RMSE}(\hat{f}, \text{Data}) = \sqrt{\frac{1}{n}\displaystyle\sum_{i = 1}^{n}\left(y_i - \hat{f}(\bf{x}_i)\right)^2}
$$

While for the sake of comparing models, the choice between RMSE and MSE is arbitrary, we have a preference for RMSE, as it has the same units as the response variable. Also, notice that in the prediction context MSE refers to an average, whereas in an ANOVA context, the denominator for MSE may not be $n$.

For a linear model , the estimate of $f$, $\hat{f}$, is given by the fitted regression line.

$$
\hat{y}({\bf{x}_i}) = \hat{f}({\bf{x}_i})
$$

We can write an `R` function that will be useful for performing this calculation.

```{r}
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
```

### Model Complexity

Aside from how well a model predicts, we will also be very interested in the complexity (flexibility) of a model. For now, we will only consider nested linear models for simplicity. Then in that case, the more predictors that a model has, the more complex the model. For the sake of assigning a numerical value to the complexity of a linear model, we will use the number of predictors, $p$.

We write a simple `R` function to extract this information from a model.

```{r}
get_complexity = function(model) {
  length(coef(model)) - 1
}
```
---
**Exercise 1:**


1. Load the `Ames` data. Drop the variables `OverallCond` and `OverallQual`.
2. Using **forward selection**---see [Lecture 6](https://msudataanalytics.github.io/SSC442/Lecture6/index.html) for details---create a series of models up to complexity length 15. You may use all variables, including categorical variables.
3. Create a chart plotting the model complexity as the $x$-axis variable and RMSE as the $y$-axis variable. Describe any patterns you see. Do you think you should use the full-size model? Why or why not? What criterion are you using to make this statement?

---

### Test-Train Split

There is an issue with fitting a model to all available data then using RMSE to determine how well the model predicts: it is essentially cheating. As a linear model becomes more complex, the RSS, thus RMSE, can never go up. It will only go down---or, in very specific cases where a new predictor is completely uncorrelated with the target, stay the same. This might seem to suggest that in order to predict well, we should use the largest possible model. However, in reality we have fit to a specific dataset, but as soon as we see new data, a large model may (in fact) predict poorly. This is called **overfitting**.

The most common approach to overfitting is to take a dataset of interest and split it in two. One part of the datasets will be used to fit (train) a model, which we will call the **training** data. The remainder of the original data will be used to assess how well the model is predicting, which we will call the **test** data. Test data should *never* be used to train a model---its pupose is to evaluate the fitted model once you've settled on something.[^1]

[^1]: Note that sometimes the terms *evaluation set* and *test set* are used interchangeably. We will give somewhat specific definitions to these later. For now we will simply use a single test set for a training set.

Here we use the `sample()` function to obtain a random sample of the rows of the original data. We then use those row numbers (and remaining row numbers) to split the data accordingly. Notice we used the `set.seed()` function to allow use to reproduce the same random split each time we perform this analysis. Sometimes we don't want to do this; if we want to run lots of independent splits, then we do not need to set the initial seed.

```{r}
set.seed(9)
num_obs = nrow(Ames)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = Ames[train_index, ]
test_data = Ames[-train_index, ]
```

We will look at two measures that assess how well a model is predicting: **train RMSE** and **test RMSE**.

$$
\text{RMSE}_\text{Train} = \text{RMSE}(\hat{f}, \text{Train Data}) = \sqrt{\frac{1}{n_{\text{Tr}}}\sum_{i \in \text{Train}}\left(y_i - \hat{f}(\bf{x}_i)\right)^2}
$$

Here $n_{Tr}$ is the number of observations in the train set. Train RMSE will still always go down (or stay the same) as the complexity of a linear model increases. That means train RMSE will not be useful for comparing models, but checking that it decreases is a useful sanity check.

$$
\text{RMSE}_{\text{Test}} = \text{RMSE}(\hat{f}, \text{Test Data}) = \sqrt{\frac{1}{n_{\text{Te}}}\sum_{i \in \text{Test}} \left ( y_i - \hat{f}(\bf{x}_i) \right ) ^2}
$$

Here $n_{Te}$ is the number of observations in the test set. Test RMSE uses the model fit to the training data, but evaluated on the unused test data. This is a measure of how well the fitted model will predict **in general**, not simply how well it fits data used to train the model, as is the case with train RMSE. What happens to test RMSE as the size of the model increases? That is what we will investigate.

We will start with the simplest possible linear model, that is, a model with no predictors.

```{r}
fit_0 = lm(SalePrice ~ 1, data = train_data)
get_complexity(fit_0)

# train RMSE
sqrt(mean((train_data$SalePrice - predict(fit_0, train_data)) ^ 2))
# test RMSE
sqrt(mean((test_data$SalePrice - predict(fit_0, test_data)) ^ 2))
```

The previous two operations obtain the train and test RMSE. Since these are operations we are about to use repeatedly, we should use the function that we happen to have already written.

```{r}
# train RMSE
rmse(actual = train_data$SalePrice, predicted = predict(fit_0, train_data))
# test RMSE
rmse(actual = test_data$SalePrice, predicted = predict(fit_0, test_data))
```

This function can actually be improved for the inputs that we are using. We would like to obtain train and test RMSE for a fitted model, given a train or test dataset, and the appropriate response variable.

```{r}
get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}
```

By using this function, our code becomes easier to read, and it is more obvious what task we are accomplishing.

```{r}
get_rmse(model = fit_0, data = train_data, response = "SalePrice") # train RMSE
get_rmse(model = fit_0, data = test_data, response = "SalePrice") # test RMSE
```
**Try it:** Apply this basic function with different arguments. Do you understand how we've nested functions within functions?

**Try it:** Define a total of five models using the first five models you fit in Exercise 1. Define these as `fit_1` through `fit_5`


### Adding Flexibility to Linear Models

Each successive model we fit will be more and more flexible using both interactions and polynomial terms. We will see the training error decrease each time the model is made more flexible. We expect the test error to decrease a number of times, then eventually start going up, as a result of overfitting. To better understand the relationship between train RMSE, test RMSE, and model complexity, we'll explore the results from Exercise 1.

Hopefully, you tried the in-line excercise above. If so, we can create a list of the models fit.

```{r}
model_list = list(fit_1, fit_2, fit_3, fit_4, fit_5)
```

We then obtain train RMSE, test RMSE, and model complexity for each. In doing so, we'll introduce a handy function from `R` called `sapply()`. You can likely intuit what it does by looking at the code below.

```{r}
train_rmse = sapply(model_list, get_rmse, data = train_data, response = "SalePrice")
test_rmse = sapply(model_list, get_rmse, data = test_data, response = "SalePrice")
model_complexity = sapply(model_list, get_complexity)
```

**Try it:** Run `?sapply()` to understand what are valid arguments to the function.

Once you've done this, you'll notice the following:

```{r}
# This is the same as the apply command above

test_rmse = c(get_rmse(fit_1, test_data, "SalePrice"),
              get_rmse(fit_2, test_data, "SalePrice"),
              get_rmse(fit_3, test_data, "SalePrice"),
              get_rmse(fit_4, test_data, "SalePrice"),
              get_rmse(fit_5, test_data, "SalePrice"))
```

We can plot the results. The train RMSE can be seen in blue, while the test RMSE is given in orange.[^3]

[^3]: The train RMSE is guaranteed to follow this non-increasing pattern. The same is not true of test RMSE. We often see a nice U-shaped curve. There are theoretical reasons why we should expect this, but that is on average. Because of the randomness of one test-train split, we may not always see this result. Re-perform this analysis with a different seed value and the pattern may not hold. We will discuss why we expect this next chapter. We will discuss how we can help create this U-shape much later. Also, we might intuitively expect train RMSE to be lower than test RMSE. Again, due to the randomness of the split, you may get (un)lucky and this will not be true.

```{r}
plot(model_complexity, train_rmse, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)) - 0.02,
              max(c(train_rmse, test_rmse)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(model_complexity, test_rmse, type = "b", col = "darkorange")
```

We could also summarize the results as a table. `fit_1` is the least flexible, and `fit_5` is the most flexible. We see the Train RMSE decrease as flexibility increases.

| Model   | Train RMSE        | Test RMSE        | Predictors              |
|---------|-------------------|------------------|-------------------------|
| `fit_1` | RMSE$_{\text{train}}$ for model 1 | RMSE$_{\text{test}}$ for model 1 | put predictors here|
| ...| ... | .... | ... |
| `fit_5` | RMSE$_{\text{train}}$ for model 5  | RMSE$_{\text{train}}$ for model 5  | $p$ predictors |

**Try it:**  When is the Test RMSE is smallest amongst the five models? Note this may not be the **best** model, but it is the best model of the class of models you've attempted?

To summarize:

- **Underfitting models:** In general *High* Train RMSE, *High* Test RMSE.
- **Overfitting models:** In general *Low* Train RMSE, *High* Test RMSE.

Specifically, we say that a model is overfitting if there exists a less complex model with lower Test RMSE.[^2] Then a model is underfitting if there exists a more complex model with lower Test RMSE.

[^2]: The labels of under and overfitting are *relative* to the best model we see. Any model more complex with higher Test RMSE is overfitting. Any model less complex with higher Test RMSE is underfitting.

---
**Exercise 2:**

1. Plot the Train and Test RMSE for the 15 models you fit in Exercise 1.
2. **This question is the most time-consuming question.** Using any method you choose and any number of regressors, predict `SalePrice`. Calculate the Train and Test RMSE.
3. In a PDF write-up, describe the resulting model. Discuss how you arrived at this model, what interactions you're using (if any) and how confident you are that your group's prediction will perform well, relative to other groups.
4. **Difficult; extra credit:** Visualize your final model in a sensible way and provide a two-paragraph interpretation.


**Evaluation:** Exercise 2 will be evaluated according to lowest RMSE. The five groups with the lowest RMSE will receive top credit. Next five will receive second highest score; and so on.

---

A final note on the analysis performed here; we paid no attention whatsoever to the "assumptions" of a linear model. We only sought a model that **predicted** well, and paid no attention to a model for **explaination**. Hypothesis testing did not play a role in deciding the model, only prediction accuracy. Collinearity? We don't care. Assumptions? Still don't care. Diagnostics? Never heard of them. (These statements are a little over the top, and not completely true, but just to drive home the point that we only care about prediction. Often we latch onto methods that we have seen before, even when they are not needed.)


---

## An Aside: Indexing in `R`

Indexing  is a very helpful way to examine data and to subdivide it into specific bits. To highlight this, we'll use some basic nonsense data and start with a simple reminder of other forms of indexing.

```{r}
boat.df <- data.frame(
boat.names = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
boat.colors = c("black", "green", "pink", "blue", "blue",
                "green", "green", "yellow", "black", "black"),
boat.ages = c(143, 53, 356, 23, 647, 24, 532, 43, 66, 86),
boat.prices = c(53, 87, 54, 66, 264, 32, 532, 58, 99, 132),
boat.costs = c(52, 80, 20, 100, 189, 12, 520, 68, 80, 100)
)
```

Indexing is a simple tool but it can answer both simple and complex questions. For example, in the data above, we can answer the following questions using a single line of code:

**Try it:** See the exercises below for a simple guide to using logical indexing.
1. What was the price of the first boat? ```boat.prices[1]```
2. What were the ages of the first 5 boats? ```boat.ages[1:5]```
3. What were the names of the black boats?```boat.names[boat.colors == "black"]```
4. What were the prices of either green or yellow boats? ```boat.prices[boat.colors == "green" | boat.colors == "yellow"]```
5. Can we change the price of boat "s" to 100?```boat.prices[boat.names == "s"] <- 100```
6. What was the median price of black boats less than 100 years old? ```median(boat.prices[boat.colors == "black" & boat.ages < 100])```
7. How many pink boats were there? ```sum(boat.colors == "pink")```
8. What percent of boats were older than 100 years old?```mean(boat.ages < 100)```


By now you should be adept at applying simple functions like `mean()` and `table()` to vectors. However, in many analyses, you won't want to calculate statistics of an entire vector. Instead, you will want to access specific *subsets* of values of a vector based on some criteria. For example, you may want to access values in a specific location in the vector (i.e.; the first 10 elements) or based on some criteria within that vector (i.e.; all values greater than 0), or based on criterion from values in a *different* vector (e.g.; All values of `age` where `sex == Female`). To access specific values of a vector in `R`, we use *indexing*. This utilizes brackets `[]`.

In general, whatever you put inside the brackets tells `R` which values of the vector object you want. There are two main ways that you can use indexing to access subsets of data in a vector: numerical and logical indexing.

### Numerical Indexing

With numerical indexing, you enter a vector of integers corresponding to the values in the vector you want to access in the form `a[index]`, where `a` is the vector, and `index` is a vector of index values. For example, let's use numerical indexing to get values from our boat vectors.

```{r}
# What is the first boat name?
boat.names[1]
# What are the first five boat colors?
boat.colors[1:5]
# What is every second boat age?
boat.ages[seq(1, 5, by = 2)]
```

You can use any indexing vector as long as it contains integers. You can even access the same elements multiple times:

```{r}
# What is the first boat age (3 times)
boat.ages[c(1, 1, 1)]
```

If it makes your code clearer, you can define an indexing object before doing your actual indexing. For example, let's define an object called `my.index` and use this object to index our data vector:

```{r}
my.index <- 3:5
boat.names[my.index]
```


## Logical Indexing

The second way to index vectors is with *logical vectors*. A logical vector is a vector that *only* contains `TRUE` and `FALSE` values. In `R`, true values are designated with `TRUE`, and false values with `FALSE` (in other languages, zero and one are acceptable). When you index a vector with a logical vector, `R` will return values of the vector for which the indexing vector is `TRUE`.[^12]

[^12]: If that was confusing, think about it this way: a logical vector, combined with the brackets `[ ]`, acts as a *filter* for the vector it is indexing. That is, it only lets values of the vector pass through the filter if the logical vector is `TRUE`. `FALSE` values in a logical vector are like mini-Gandalfs. If we index a vector `x` with a logical vector `y`, then mini-Gandalf stops all the values of `x` for which `y` was `FALSE`.


The easiest way to create logical vectors is from *existing vectors* using comparison operators like `<` (less than), `==` (equals to), and `!=` (not equal to).

**Try it:** Find a complete list of logical operators in `R`.

You can also create logical vectors by comparing a vector to another vector of the same length. When you do this, R will compare values in the same position (e.g.; the first values will be compared, then the second values, etc.). For example, we can compare the `boat.cost` and `boat.price` vectors to see which boats sold for a higher price than their cost:

```{r}
# Which boats had a higher price than cost?
boat.prices > boat.costs
# Which boats had a lower price than cost?
boat.prices < boat.costs
```

Once you've created a logical vector using a comparison operator, you can use it to index any vector with the same length. Here, I'll use logical vectors to get the prices of boats whose ages were greater than 100:

```{r}
# What were the prices of boats older than 100?
boat.prices[boat.ages > 100]
```


Here's how logical indexing works step-by-step:

1. Which boats are older than 100 years? ```boat.ages > 100```
2. Write the logical index by hand (of course, you will never do this).
3. Show me all of the boat prices where the logical vector is TRUE: ```boat.prices[c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)]```

- Doing it all in one step! You get the same answer: ```boat.prices[boat.ages > 100]```


### `&` (and), `|` (or), `%in%`

In addition to using single comparison operators, you can combine multiple logical vectors using the OR (which looks like `|` and AND `&` commands. The OR `|` operation will return TRUE if any of the logical vectors is TRUE, while the AND `&` operation will only return TRUE if all of the values in the logical vectors is TRUE. This is especially powerful when you want to create a logical vector based on criteria from multiple vectors.

For example, let's create a logical vector indicating which boats had a price greater than 200 OR less than 100, and then use that vector to see what the names of these boats were:

```{r}
# Which boats had prices greater than 200 OR less than 100?
boat.prices > 200 | boat.prices < 100
# What were the NAMES of these boats
boat.names[boat.prices > 200 | boat.prices < 100]
```

You can combine as many logical vectors as you want (as long as they all have the same length!):

```{r}
# Boat names of boats with a color of black OR with a price > 100
boat.names[boat.colors == "black" | boat.prices > 100]
# Names of blue boats with a price greater than 200
boat.names[boat.colors == "blue" & boat.prices > 200]
```


You can combine as many logical vectors as you want to create increasingly complex selection criteria. For example, the following logical vector returns TRUE for cases where the boat colors are black OR brown, AND where the price was less than 100:

```{r}
# Which boats were eithe black or brown, AND had a price less than 100?
(boat.colors == "black" | boat.colors == "brown") & boat.prices < 100
# What were the names of these boats?
boat.names[(boat.colors == "black" | boat.colors == "brown") & boat.prices < 100]
```

When using multiple criteria, make sure to use parentheses when appropriate. If I didn't use parentheses above, I would get a different answer.

The `%in%` operation helps you to easily create multiple OR arguments.Imagine you have a vector of categorical data that can take on many different values. For example, you could have a vector x indicating people's favorite letters.

```{r}
x <- c("a", "t", "a", "b", "z")
```


Now, let's say you want to create a logical vector indicating which values are either a or b or c or d. You could create this logical vector with multiple | (OR) commands:

```{r}
x == "a" | x == "b" | x == "c" | x == "d"
```


However, this takes a long time to write. Thankfully, the `%in%` operation allows you to combine multiple OR comparisons much faster. To use the `%in%` function, just put it in between the original vector, and a new vector of possible values. The `%in%` function goes through every value in the vector x, and returns TRUE if it finds it in the vector of possible values -- otherwise it returns FALSE.

```{r}
x %in% c("a", "b", "c", "d")
```


As you can see, the result is identical to our previous result.


### Counts and percentages from logical vectors

Many (if not all) R functions will interpret TRUE values as 1 and FALSE values as 0. This allows us to easily answer questions like "How many values in a data vector are greater than 0?" or "What percentage of values are equal to 5?" by applying the `sum()` or `mean()` function to a logical vector.

We'll start with a vector x of length 10, containing 3 positive numbers and 5 negative numbers.

```{r}
x <- c(1, 2, 3, -5, -5, -5, -5, -5)
```


We can create a logical vector to see which values are greater than 0:

```{r}
x > 0
```


Now, we'll use `sum()` and `mean()` on that logical vector to see how many of the values in x are positive, and what percent are positive. We should find that there are 5 TRUE values, and that 50\% of the values (5 / 10) are TRUE.

```{r}
sum(x > 0)
mean(x > 0)
```


This is a *really* powerful tool. Pretty much *any* time you want to answer a question like "How many of X are Y" or "What percent of X are Y", you use `sum()` or `mean()` function with a logical vector as an argument.

### Additional Logical functions

R has lots of special functions that take vectors as arguments, and return logical vectors based on multiple criteria. For example, you can use the `is.na()` function to test which values of a vector are missing. Table \@ref(tab:logicalfunctions) contains some that I frequently use:


| Function| Description|Example | Result
|:--------------|:-----------------|:-----------------------|----|
|     `is.na(x)`| Which values in x are NA?|`is.na(c(2, NA, 5))`    | `r is.na(c(2, NA, 5))`|
|     `is.finite(x)`|    Which values in x are numbers?    | `is.finite(c(NA, 89, 0))` | `r is.finite(c(NA, 89, 0))`|
|     `duplicated(x)`|    Which values in x are duplicated? | `duplicated(c(1, 4, 1, 2))` | `r duplicated(c(1, 4, 1, 2))`|
|     `which(x)`|    Which values in x are TRUE? | `which(c(TRUE, FALSE, TRUE))` | `r which(c(TRUE, FALSE, TRUE))`|

Table: (\#tab:logicalfunctions) Functions to create and use logical vectors.

Logical vectors aren't just good for indexing, you can also use them to figure out which values in a vector satisfy some criteria. To do this, use the function `which()`. If you apply the function `which()` to a logical vector, R will tell you which values of the index are TRUE. For example:

```{r}
# A vector of sex information
sex <- c("m", "m", "f", "m", "f", "f")
# Which values of sex are m?
which(sex == "m")
# Which values of sex are f?
which(sex == "f")
```



## Changing values of a vector

Now that you know how to index a vector, you can easily change specific values in a vector using the assignment (`<-`) operation. To do this, just assign a vector of new values to the indexed values of the original vector:

Let's create a vector `a` which contains 10 1s:

```{r}
a <- rep(1, 10)
```


Now, let's change the first 5 values in the vector to 9s by indexing the first five values, and assigning the value of 9:

```{r}
a[1:5] <- 9
a
```



Now let's change the last 5 values to 0s. We'll index the values 6 through 10, and assign a value of 0.

```{r}
a[6:10] <- 0
a
```


Of course, you can also change values of a vector using a logical indexing vector. For example, let's say you have a vector of numbers that should be from 1 to 10. If values are outside of this range, you want to set them to either the minimum (1) or maximum (10) value:

```{r}
# x is a vector of numbers that should be from 1 to 10
x <- c(5, -5, 7, 4, 11, 5, -2)
# Assign values less than 1 to 1
x[x < 1] <- 1
# Assign values greater than 10 to 10
x[x > 10] <- 10
# Print the result!
x
```

As you can see, our new values of x are now never less than 1 or greater than 10![^11]

[^11]:**A Note on How R Handles Indexing:** Technically, when you assign new values to a vector, you should always assign a vector of the same length as the number of values that you are updating. For example, given a vector a with 10 1s: ```a <- rep(1, 10)``` To update the first 5 values with 5 9s, we should assign a new vector of 5 9s: ```a[1:5] <- c(9, 9, 9, 9, 9)``` However, if we repeat this code but just assign a single 9, `R` will repeat the value as many times as necessary to fill the indexed value of the vector. That's why the following code still works: ```a[1:5] <- 9``` In other languages this code **wouldn't work** because we're trying to replace 5 values with just 1. However, this is a case where `R` bends the rules of programming a bit. But don't do this.

### Example: Fixing Missing or Incorrect Data

Assigning and indexing is a particularly helpful tool when, for example, you want to remove invalid values in a vector before performing an analysis. For example, let's say you asked 10 people how happy they were on a scale of 1 to 5 and received the following responses:

```{r}
happy <- c(1, 4, 2, 999, 2, 3, -2, 3, 2, 999)
```

As you can see, we have some invalid values (999 and -2) in this vector. To remove them, we'll use logical indexing to change the invalid values (999 and -2) to NA. We'll create a logical vector indicating which values of `happy` are *invalid* using the `%in%` operation. Because we want to see which values are *invalid*, we'll add the `== FALSE` condition (If we don't, the index will tell us which values *are* valid).

```{r}
# Which values of happy are NOT in the set 1:5?
invalid <- (happy %in% 1:5) == FALSE
invalid
```

Now that we have a logical index `invalid` telling us which values are invalid (that is, not in the set 1 through 5), we'll index `happy` with `invalid`, and assign the invalid values as NA:

```{r}
# Convert any invalid values in happy to NA
happy[invalid] <- NA
happy
```

We can also recode all the invalid values of `happy` in one line as follows:

```{r}
# Convert all values of happy that are NOT integers from 1 to 5 to NA
happy[(happy %in% 1:5) == FALSE] <- NA
```


As you can see, `happy` now has NAs for previously invalid values. Now we can take a `mean()` of the vector and see the mean of the valid responses.

```{r}
# Include na.rm = TRUE to ignore NA values
mean(happy, na.rm = TRUE)
```
