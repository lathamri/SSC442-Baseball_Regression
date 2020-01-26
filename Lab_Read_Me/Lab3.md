# Training and Testing: (Linear) Predictions







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

The second way to index vectors is with *logical vectors*. A logical vector is a vector that *only* contains `TRUE` and `FALSE` values. In `R`, true values are designated with `TRUE`, and false values with `FALSE` (in other languages, zero and one are acceptable). When you index a vector with a logical vector, `R` will return values of the vector for which the indexing vector is `TRUE`.[^1]

[^1]: If that was confusing, think about it this way: a logical vector, combined with the brackets `[ ]`, acts as a *filter* for the vector it is indexing. That is, it only lets values of the vector pass through the filter if the logical vector is `TRUE`. `FALSE` values in a logical vector are like mini-Gandalfs. If we index a vector `x` with a logical vector `y`, then mini-Gandalf stops all the values of `x` for which `y` was `FALSE`.


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

As you can see, our new values of x are now never less than 1 or greater than 10![^2]

[^2]:**A Note on Indexing:** Technically, when you assign new values to a vector, you should always assign a vector of the same length as the number of values that you are updating. For example, given a vector a with 10 1s: ```a <- rep(1, 10)``` To update the first 5 values with 5 9s, we should assign a new vector of 5 9s: ```a[1:5] <- c(9, 9, 9, 9, 9)``` However, if we repeat this code but just assign a single 9, `R` will repeat the value as many times as necessary to fill the indexed value of the vector. That's why the following code still works: ```a[1:5] <- 9``` In other languages this code **wouldn't work** because we're trying to replace 5 values with just 1. However, this is a case where `R` bends the rules of programming a bit. But don't do this.

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
