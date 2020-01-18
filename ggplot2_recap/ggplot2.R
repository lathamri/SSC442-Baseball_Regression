# 6 aspects of the gg2plot
#
# 1. Data
# 2. Layers - what you see on the plots (e.g. points, lines, bars)
# 3. Scales - Maps the data to the graphical output
# 4. Coordinates - The visualization's perspective (e.g. a grid)
# 5. Faceting - Provides "visual drill-down" into the data
# 6. Themes - Controls the details of the display (e.g. fonts)

# ggplot2 has three required components:
#
# 1. Data
# 2. Aesthetics - The mappings of the data to the visualization. For example, mapping the value of Titanic passenger
#                 ages to the y-axis of a graph.
# 3. Layers - A visualization requires at least one layer to render the data and aesthetics to the screen.
#             These layers typically take the form of a ggplot2 geom function - for example, a scatter plot.

library(ggplot2)

# Load the Titanic titanicing data for analysis.
titanic <- read.csv('titanic.csv', stringsAsFactors = FALSE)

# Open data in speadsheet view.
View(titanic)

# Set up factors.
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

#
# First question: Whats the survival rate:
# As Survived is a factor (i.e., categorical) variable, a bar chart is a great visualization choice.
#
ggplot(titanic , aes(x=Survived)) +
  geom_bar()

# If really want percentages.
prop.table(table(titanic$Survived))

# Save percentage into a variable
percent_die = prop.table(table(titanic$Survived))[1]

# Add some customization for labels and theme.
# labs = labels.
ggplot(titanic, aes(x=Survived)) +
  theme_bw() +
  geom_bar() +
  labs(y= 'Passenger Count',
       title = 'Titanic Survival Rates')

#
# Second question: What was the survival rate by gender?
#
# Can use color to look at two aspects (i.e., dimentions) 
# of the data simultaneously.
# fill = fill bars based on values.
#
ggplot(titanic, aes(x= Sex, fill = Survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y= "Passenger Count",
       title = ' Titanic Survival Rates by sex')

#
# Third question: What was the survival rate by class of ticket?
#
ggplot(titanic, aes(x= Pclass, fill = Survived)) +
  theme_bw() +
  geom_bar() +
  labs(x= 'Class of Ticket', y= 'Passenger Count',
       title = 'Survival Rate by Class of Ticket')

#
# Forth question: What was the survival rate by class of ticket AND gender?
# facet will add the drilldown based on Pclass. "~" think of it as "by" Pclass.
#
ggplot(titanic, aes(x = Sex, fill = Survived)) +
  theme_bw() +
  facet_wrap(~ Pclass) +
  geom_bar() +
  labs(x= 'Class of Ticket', y= 'Passenger Count',
       title = 'Survival Rate by Ticket and Sex')

#
# Fifth question: What is the distribution of passenger ages?
#
# The histogram is a staple of visualizing numeric data as it very powerfully
# communicates the distribution of a variable (i.e., column)
#
# The binwidth can be thought of blocking them by 5 years wide. e.g., 1-5, 6-10, 11-15, 16-20,...
#

ggplot(titanic, aes(x = Age)) +
  theme_bw() +
  geom_histogram(binwidth = 5) + 
  labs( y = "Passenger Count",
        x = "Age (binwidth = 5)",
        title = "Titanic Age Distribution")

#
# Sixth question: What are the survival rates by age?
#

ggplot(titanic, aes(x = Age, fill = Survived)) +
  theme_bw() +
  geom_histogram(binwidth = 5) + 
  labs( y = "Passenger Count",
        x = "Age (binwidth = 5)",
        title = "Titanic Survival Rate by Age")

#
# Another great visualization for this question is the box-and-whisker plot
#

ggplot(titanic, aes(x = Survived, y= Age)) +
  theme_bw() +
  geom_boxplot() + 
  labs( y = "Age",
        x = "Survived",
        title = "Titanic Survival Rate by Age")

# all this is showing is that people who survived tended to be a slight be younger

#
# Seventh question: What is the survival rates by age when segmented by gender and class of ticket?
#
# A related visualization to the histogram is a density plot. Think of a density plot as a smoothed
# version of the histogram. Using ggplot2 we can use facets to allow for visual drill-down 
# via density plots.
#

ggplot(titanic, aes( x = Age, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_density(alpha = 0.5) +
  labs(y = 'Age',
       x = 'Survived',
       title = 'Titanic Survival Rates by Age, Pclass, and Sex.')

# If histogram is prefered...

ggplot(titanic, aes( x = Age, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_histogram(binwidth = 5) +
  labs(y = 'Age',
       x = 'Survived',
       title = 'Titanic Survival Rates by Age, Pclass, and Sex.')

