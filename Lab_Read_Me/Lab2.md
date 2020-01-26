# Lab 2: Linear Regression and Simple Analyses

This question will require you to analyze a new dataset. Specifically, this lab utilizes the “ames.csv” data [(found here)](/SSC442/Labs/data/ames.csv). This is a famous dataset about home sales from Ames, Iowa.

## Backstory and Set Up
You have been recently hired to Zillow’s Zestimate product team as a junior analyst. As a part of their regular hazing, they have given you access to a small subset of their historic sales data. Your job is to present some basic predictions for housing values in a small geographic area (Ames, IA) using this historical pricing.

First, let's load the data.

```{r}
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                 header = TRUE,
                 sep = ",")
```

Before we proceed, let's note a few things about the (simple) code above. First, we have specified `header = TRUE` because---you guessed it---the original dataset has headers. Although simple, this is an incredibly important step because it allows `R` to do some smart `R` things. Specifically, once the headers are in, the variables are formatted as `int` and `factor` where appropriate. It is absolutely vital that we format the data correctly; otherwise, many `R` commands will whine at us.

**Try it:** Run the above, but instead specifying `header = FALSE`. What data type are the various columns? Now try ommitting the line altogether. What is the default behavior of the `read.table` function?[^1]

[^1]: Of course, you could find out the defaults of the function by simply using the handy `?` command. Don't forget about this tool!

### Data Exploration and Processing

We are not going to tell you anything about this data. This is intended to replicate a real-world experience that you will all encounter in the (possibly near) future: someone hands you data and you're expected to make sense of it. Fortunately for us, this data is (somewhat) self-contained. We'll first check the variable names to try to divine some information. Recall, we have a handy little function for that:

```{r}
names(ameslist)
```

Note that, when doing data exploration, we will sometimes choose to not save our output. This is a judgement call; here we've chosen to merely inspect the variables rather than diving in.

Inspection yields some obvious truths. For example:

| Variable      | Explanation           | Type  |
| ------------- |:-------------:| -----:|
| `ID`      | Unique identifier for each row | `int` |
| `LotArea`    | Size of lot (**units unknown**)      |  `int`|
| `SalePrice` | Sale price of house ($)      |    `int` |

...but we face some not-so-obvious things as well. For example:

| Variable      | Explanation           | Type  |
| ------------- |:-------------:| -----:|
| `LotShape`      | ? Something about the lot| `factor` |
| `MSSubClass`    | ? No clue at all      |  `int`|
| `Condition1` | ? Seems like street info      |    `factor` |

It will be difficult to learn anything about the data that is of type `int` without outside documentation. However, we can learn something more about the `factor`-type variables. In order to understand these a little better, we need to review some of the values that each take on.

**Try it:** Go through the variables in the dataset and make a note about your interpretation for each. Many will be obvious, but some require additional thought.

We now turn to another central issue---and one that explains our nomenclature choice thus far: the data object is of type `list`. To verify this for yourself, check:
```{r}
typeof(ameslist)
```
This isn't ideal---for some visualization packages, for instance, we need data frames and not lists. We'll make a mental note of this as something to potentially clean up if we desire.

Although there are some variables that would be difficult to clean, there are a few that we can address with relative ease. Consider, for instance, the variable `GarageType`. This might not be that important, but, remember, the weather in Ames, IA is pretty crummy---a detached garage might be a dealbreaker for some would-be homebuyers. Let's inspect the values:

```{r}
> unique(ameslist$GarageType)
[1] Attchd  Detchd  BuiltIn CarPort <NA> Basment 2Types
```

With this, we could make an informed decision and create a new variable. Let's create `OutdoorGarage` to indicate, say, homes that have any type of garage that requires the homeowner to walk outdoors after parking their car. (For those who aren't familiar with different garage types, a car port is not insulated and is therefore considered outdoors. A detached garage presumably requires that the person walks outside after parking. The three other types are inside the main structure, and `2Types` we can assume includes at least one attached garage of some sort). This is going to require a bit more coding and we will have to think through each step carefully.

First, let's create a new object that has indicator variables (that is, a variable whose values are either zero or one) for each of the `GarageType` values. As with everything in `R`, there's a handy function to do this for us:

```{r}
GarageTemp = model.matrix( ~ GarageType - 1, data=ameslist$GarageType )
```
We now have two separate objects living in our computer's memory: `ameslist` and `GarageTemp`---so named to indicate that it is a temporary object.[^4] We now need to stitch it back onto our original data; we'll use a simple concatenation and write over our old list with the new one:

```{r}
ameslist <- cbind(ameslist, GarageTemp)
> Error in data.frame(..., check.names = FALSE) :
  arguments imply differing number of rows: 1460, 1379
```

Huh. What's going on?

**Try it:** Figure out what's going on above. Fix this code so that you have a working version.

Now that we've got that working (ha!) we can generate a new variable for our outdoor garage. We'll use a somewhat gross version below because it is *verbose*; that said, this can be easily accomplished using logical indexing for those who like that approach.

```{r}
ameslist$GarageOutside <- ifelse(ameslist$GarageTypeDetchd == 1 | ameslist$GarageTypeCarPort == 1, 1, 0)
unique(ameslist$GarageOutside)
[1]  0  1 NA
```

This seems to have worked. The command above `ifelse()` does what it says: `if` some condition is met (here, either of two variables equals one) then it returns a one; `else` it returns a zero. Such functions are very handy, though as mentioned above, there are other ways of doing this. Also note, that while fixed the issue with `NA` above, we've got new issues: we definitely don't want `NA` outputted from this operation. Accordingly, we're going to need to deal with it somehow.

**Try it:** Utilizing a similar approach to what you did above, fix this so that the only outputs are zero and one.

Generally speaking, this is a persistent issue, and you will spend an extraordinary amount of time dealing with missing data or data that does not encode a variable exactly as you want it. This is expecially true if you deal with real-world data: you will need to learn how to handle `NA`s. There are a number of fixes (as always, Google is your friend) and anything that works is good. But you should spend some time thinking about this and learning at least one approach.

[^4]: It's not exactly true that these objects are in memory. They are... sort of. But how `R` handles memory is complicated and silly and blah blah who cares. It's basically in memory.

---

**Exercise 1:**
1. Prune the data to all of the variables that are `type = int` about which you have some reasonable intuition for what they mean. This **must** include the variable `SalePrice`. Save this new dataset as `Ames`. Produce documentation for this object in the form of a .txt file. This must describe each of the preserved variables, the values it can take (e.g., can it be negative?) and your interpretation of the variable.

2. Produce a *scatterplot matrix* which includes 12 of the variables that are `type = int` in the data set. Choose those that you believe are likely to be correlated with `SalePrice`.[^2]
3. Compute a matrix of correlations between these variables using the function `cor()`. Does this match your prior beliefs? Briefly discuss the correlation between the miscellaneous variables and `SalePrice`.

4. Produce a scatterplot between `SalePrice` and `GrLivArea`. Use the `abline()` function to plot the relationship that you've found in the simple linear regression.
    - What is the largest outlier that is above the regression line? Produce the other information about this house?

**(Bonus; more challenging)** Create a visualization that shows the proliferation of air conditioning in homes in Ames.

[^2]: If you are not familiar with this type of visualization, consult the course textbook (*ISL*), Chapters 2 and 3.

---

## Building a Model

We're now ready to start playing with a model. We will start by using the `lm()` function to fit a simple linear regression
model, with `SalePrice` as the response and lstat as the predictor.

Recall that the basic `lm()` syntax is `lm(y∼x,data)`, where `y` is the **response**, `x` is the **predictor**, and `data` is the data set in which these two variables are kept. Let's quickly run this with two variables:

```{r}
lm.fit = lm(SalePrice ~ GrLivArea)
```
This yields:
`Error in eval(expr, envir, enclos) : Object "SalePrice" not found`

This command causes an error because `R` does not know where to find the variables. We can fix this by attaching the data:

```{r}
attach(Ames)
lm.fit = lm(SalePrice ~ GrLivArea)
```

**Try it:** What is `GrLivArea`? If you did not include this variable above, check its relationship to other variables in the dataset to get a better idea what it is.

The next line tells `R` that the variables are in the object known as `Ames`. If you haven't created this object yet (as in Exercise 1) you'll get an error at this stage. But once we attach `Ames`, the first line works fine because `R` now recognizes the variables. Alternatively, we could specify this within the `lm()` call using `data = Ames`. We've presented this way because it may be new to you; choose whichever you find most reasonable.

If we type `lm.fit`, some basic information about the model is output. For more detailed information, we use `summary(lm.fit)`. This gives us p-values and standard errors for the coefficients, as well as the $R^2$ statistic and $F$-statistic for the entire model.[^3]

[^3]: When we use the simple regression model with a single input, the $F$-stat includes the intercept term. Otherwise, it does not. See Lecture 5 for more detail.

Utilizing these functions hels us see some interesting results. Note that we built (nearly) the simplest possible model:

$$\text{SalePrice} = \beta_0 + \beta_1*(\text{GrLivArea}) + \epsilon.$$

But even on its own, this model is instructive. It suggest that an increase in overall living area of 1 ft $^2$ is correlated with an expected increase in sales price of $107. (Note that we **cannot** make causal claims!)

Saving the model as we did above is useful because we can explore other pieces of information it stores. Specifically, we can use the `names()` function in order to find out what else is stored in `lm.fit`. Although we can extract these quan- tities by name---e.g. `lm.fit$coefficients`---it is safer to use the extractor functions like `coef()` to access them. We can also use a handy tool like `plot()` applied directly to `lm.fit` to see some interesting data that is automatically stored by the model.

**Try it:** Use `plot()` to explore the model above. Do you suspect that some outliers have a large influence on the data? We will explore this point specifically in the future.

We can now go crazy adding variables to our model. It's as simple as appending them to the previous code---though you should be careful executing this, as it will overwrite your previous output:

```{r}
lm.fit = lm(SalePrice ~ GrLivArea + LotArea)
```

**Try it:** Does controlling for `LotArea` change the *qualitative* conclusions from the previous regression? What about the *quantitative* results? Does the direction of the change in the quantitative results make sense to you?

---
**Exercise 2:**

1. Use the `lm()` function in a **simple** linear regression (e.g., with only one predictor) with `SalePrice` as the response to determine the value of an indoor garage.

2. Use the `lm()` function to perform a multiple linear regression with `SalePrice` as the response and all other variables from your `Ames` data as the predictors. Use the `summary()` function to print the results. Comment on the output. For instance:
    - Is there a relationship between the predictors and the response?
    - Which predictors appear to have a statistically significant relationship to the response?
    - What does the coefficient for the year variable suggest?

3. Use the `plot()` function applied to your model to produce diagnostic plots of the linear regression fit. Comment on any problems you see with the fit. Do the residual plots suggest any unusually large outliers? Does the leverage plot identify any observations with unusually high leverage?

4. Use the `*` and `:` symbols to fit linear regression models with some well-chosen interaction effects. Do any interactions appear to be statistically significant?

5. Try a few (e.g., three) different transformations of the variables, such as $ln(x)$, $x^2$, $\sqrt x$. Do any of these make sense to include in a model of `SalePrice`? Comment on your findings.

**(Bonus; very very challenging)** How might we build a model to estimate the elasticity of demand from this dataset?

---
