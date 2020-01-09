library(tidyverse)
library(ggplot2)
library(gapminder)

ggplot2::mpg
# typing geom_ will show all possible options of plots from ggplot2

# generates gapminder data set
library(gapminder)
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

q + geom_point()

p + geom_point() + geom_smooth()

?geom_smooth()



