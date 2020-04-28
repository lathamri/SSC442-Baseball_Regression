library(readxl)

data = read_excel('/Users/rileylatham/Downloads/SSC442/Group_Project/Data/Michigan Data.xlsx')

View(data)

drops=c('Union membership percentage','Union Coverage', 'Overall Homeless', '...16')

data = data[,!(names(data))%in%drops]

summary(lm(`Unemployment Year Average`~.,data=data[2:19,]))

step()