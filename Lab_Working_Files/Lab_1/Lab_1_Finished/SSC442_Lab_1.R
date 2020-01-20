library(tidyverse)
library(ggplot2)
library(gapminder)
library(scales)

ggplot2::mpg
# typing geom_ will show all possible options of plots from ggplot2

# generates gapminder data set
head(gapminder)
# binds our variables of interest, gpdpercap and life exp
p = ggplot(data=gapminder,
           mapping=aes(x=gdpPercap, y=lifeExp))
# generates a scatter plot for p
p + geom_point()

# binds our variables of interest, engine size and highway milage
q = ggplot(data=mpg,
           mapping=aes(x=displ, y=hwy, color=class))
# generates a scatter plot for q
q + geom_point()

# It generates what you'd expect, as engine size increases you would
# likely see highway milages decrease as larger engines usually belong
# to larger vehicles which have lower highway milages.

# binds our variables of interest, class and drive type
r = ggplot(data=mpg,
           mapping=aes(x=class, y=drv))

# generates a scatter plot of r
r + geom_point()

# This isn't a helpful plot because of the data bindings. It simply shows
# cars that have front, rear, or 4-wheel drive by class which doesn't tell 
# us anyting intersting about our data. Perhaps a count of cars fitting into
# these categories would be more insightful.

p + geom_point() + geom_smooth()

?geom_smooth()

# Running different methods and aes for smooth
p + geom_point() + geom_smooth(method='gam')
p + geom_point() + geom_smooth(method='auto')
# this is the best for small data sets but is computationally inefficient
p + geom_point() + geom_smooth(method='loess', colour='red')

# log scaling to spread the data better
p + geom_point() + 
  geom_smooth(method='lm') + 
  scale_x_log10(labels = scales::dollar)

# scale_x_log10 description:
# The data appears to be logarithmic because of the grouping so we apply a log
# scale to the data to better visualize it. What ends up happening is instead 
# of using a linear scale we change the x axis to use a logarithmic scale 
# so that our data appears more linear without loss of generality.

?dollar
?scales

# This call changes our labels on the x-axis to dollars instead of the current
# scientific notation that comes with scale_x_log10.
# Other label changes can be applied according to the scales documentation.
# Some examples include byte size, dates, percents and more.

# Redefines p variables
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp, color = 'yellow'))
p + geom_point() + scale_x_log10()

# Our mapping aes changes the color value for the data p but this is
# then looked as a data descriptor (obviously it's not). Rhe color
# is determined in the geom calls so we have to set the color value there.
# Interestingly the color call in mapping defines our data label as whatever you set
# the value to be, in this case yellow but any string works.

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(colour='yellow') + scale_x_log10()

# The above changes fix the issue and craft yellow data points.

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point() + 
  geom_smooth(color = "orange", se = FALSE, size = 8, method = "lm") + 
  scale_x_log10()

# Color sets our approx. line to orange, se=False turns on conf. intervals,
# size sets the thickness of the line to 8 and method creates
# a linear model of the data.

p + geom_point(alpha = 0.3) +
  geom_smooth(method = "gam") +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data Points are country-years",
       caption = "Source: Gapminder")

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp, color = continent, fill = continent))

p + geom_point()
p + geom_point() + scale_x_log10(labels = dollar)
p + geom_point() + scale_x_log10(labels = dollar) + geom_smooth()

# fill=continent is sorting the data by continent and applying unique colors to
# each point in the data and the smoothing lines as well.
# Personally I think it's a little messy and maybe silly to have the data share
# exact colors and slightly more translucent confidence intervals. I would prefer
# to change the alpha values of our dots to better allow us to see the trend lines.
# I've plotted this below but it still needs tweaks to capture the best 
# interpretation for the dataset.

p + geom_point(alpha=0.3) + scale_x_log10(labels = dollar) + geom_smooth()

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = continent)) + geom_smooth() + scale_x_log10()

# This code is giving us one regression line because of the mapping to
# color continent being assigned in the geom_point call. Coloring by
# continenet is a direct effect of the data and must be applied in the data
# section when we assign data to our p object.

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = continent)) +
  geom_smooth(mapping = aes(color = continent, fill = continent)) +
  scale_x_log10() +
  geom_smooth(mapping = aes(color = continent), method = "gam")

# Instead of assiging aes mappings for each geom we're better off assigning 
# them all at once in our first point so they stick to the data rather 
# than the geom. The following function demonstrates the difference.
# It's a much more readable function with the same graph.

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp, color=continent, fill=continent))
p + geom_point() +
  geom_smooth() +
  scale_x_log10() +
  geom_smooth(method = "gam")



