#Load in data for Lab 2
#
# If we do not lead header = TRUE, the variables loaded in will not be labeled as factor
# and int. It will instead load everything in as a factor, leading to issues later.
#

ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")
#
# Using the str() function, we can see what categories are factors and ints.
# Using the names() function, we can see all the column names in the data.
#

str(ameslist)
names(ameslist)

#
# What type of object is ameslist: list
# This can lead to issues, as we want a data frame structure for the data.
#

typeof(ameslist)

#
# Find unique styles of garage listed in the data set
#

unique(ameslist$GarageType)
  
#
# Create a new indicator variable to group the styles of garage
#

GarageTemp = model.matrix( ~ ameslist$GarageType - 1, data=ameslist$GarageType)

#
# Need to stich the GarageTemp variable into the ameslist data.
# However, the two data frames are of different row lengths due to NA.
  
ameslist <- cbind(ameslist, GarageTemp)
ameslist <- merge(ameslist, GarageTemp)

which(is.na(GarageTemp), arr.ind=TRUE)

View(ameslist)

ameslist$GarageOutside <- ifelse(ameslist$`ameslist$GarageTypeDetchd` == 1 | ameslist$`ameslist$GarageTypeCarPort` == 1, 1, 0)
unique(ameslist$GarageOutside)


colnames(ameslist)[colnames(ameslist)=="v1"] <- "GarageOutside"

ameslist$new <- ifelse(ameslist$GarageTypeAttchd == 1, 1, 0)

which(is.na(ameslist$GarageOutside), arr.ind=TRUE)

newdata = ameslist[-c( vector ),]

newdata = ameslist[-c(which(is.na(ameslist$v1)))]

View(newdata$GarageOutside)

vector = c(which(is.na(ameslist$GarageOutside)))
vector
