## Lab 2

# Read in the data set with headers TRUE
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

# Read in the data set with headers FALSE
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = FALSE,
                       sep = ",")
summary(ameslist)

# Notice how this clearly breaks the data by trying to force the headers to remain as part of the data. It 
# forces R to structure all of this as vectors with no specific data type.

# Re-Read in the data with headers True
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

# Info on read.table
?read.table

# Get some info on the data
names(ameslist)

# Tells us the data is stored as lists instead of a data frame, something to care for later
typeof(ameslist)

# Perhaps we want to make some analysis of the garage type as a predictor for price because of the typically
# bad weather in Ames
unique(ameslist$GarageType)
# Lets further break this down into any garage that makes you walk outside, and those that don't

# First we assign values to the garage types of 0 or 1
GarageTemp = model.matrix( ~ ameslist$GarageType - 1, data=ameslist$GarageType )
# Now we stitch this back together using cbind
ameslist <- cbind(ameslist, GarageTemp)
# Lets see why this doesn't work, perhaps it's the NA values in garagetemp being dropped?
sum(is.na(ameslist$GarageType))
# Notice that the amount of NA values in GarageType is 81, and that 1379+81=1460 which is the number of rows 
# we have in GarageType. So we must be dropping NA values when we make GarageTemp. Lets fix that below.

# save options default values
original_options = options()
# Write NA handling exception to keep NA rows for GarageType
current_options = options(na.action='na.pass')
# Build our matrix
GarageTemp = model.matrix( ~ ameslist$GarageType - 1, data=ameslist$GarageType )
# Create our cbind and update ameslist
ameslist <- cbind(ameslist, GarageTemp)
# Restore original options
options(original_options)
# Now we see the Bind gives us no trouble and we can change the options back so they don't mess with 
# any of our later code.

# Now we change our values again to indicate which of these garage types are outside
ameslist['GarageOutside']=0
ameslist$GarageOutside = ifelse(ameslist$GarageTypeDetchd == 1 | ameslist$GarageTypeCarPort == 1, 1, 0)
unique(ameslist$GarageOutside)





