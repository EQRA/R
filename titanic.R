# Load raw data

train <- read.csv("train.csv", header = TRUE)
names(train)
test <- read.csv("test.csv", header = TRUE)
names(test)


# Add a "Survived" variable to the test set to allow for combining data sets

test.survived <- data.frame(test[1],Survived = rep("None", nrow(test)), test[-1])


str(test.survived)
summary(test.survived)

# Combine data sets

data.combined <- rbind(train, test.survived)

# A bit about R data types (e.g., factors)

str(data.combined)


data.combined$Survived <- as.factor(data.combined$Survived)

data.combined$Pclass <- as.factor(data.combined$Pclass)


# Take a look at gross survival rates

table(data.combined$Survived)





# Distribution across classes

table(data.combined$Pclass)





# Load up ggplot2 package to use for visualizations

library(ggplot2)





# Hypothesis - Rich folks survived at a higer rate

train$Pclass <- as.factor(train$Pclass)

ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
    geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 





# Examine the first few names in the training data set

head(as.character(train$name))





# How many unique names are there across both train & test?

length(unique(as.character(data.combined$name)))





# Two duplicate names, take a closer look

# First, get the duplicate names and store them as a vector

dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])





# Next, take a look at the records in the combined data set

data.combined[which(data.combined$name %in% dup.names),]





# What is up with the 'Miss.' and 'Mr.' thing?

library(stringr)





# Any correlation with other variables (e.g., sibsp)?

misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]

misses[1:5,]





# Hypothesis - Name titles correlate with age

mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")), ]

mrses[1:5,]





# Check out males to see if pattern continues

males <- data.combined[which(train$Sex == "male"), ]

males[1:5,]

seq(from=1, to=10)


# Expand upon the realtionship between `Survived` and `Pclass` by adding the new `Title` variable to the

# data set and then explore a potential 3-dimensional relationship.



# Create a utility function to help with title extraction

# NOTE - Using the grep function here, but could have used the str_detect function as well.

extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    
    return ("Miss.")
    
  } else if (length(grep("Master.", name)) > 0) {
    
    return ("Master.")
    
  } else if (length(grep("Mrs.", name)) > 0) {
    
    return ("Mrs.")
    
  } else if (length(grep("Mr.", name)) > 0) {
    
    return ("Mr.")
    
  } else {
    
    return ("Other")
    
  }
  
}





# NOTE - The code below uses a for loop which is not a very R way of

#        doing things

titles <- NULL

for (i in 1:nrow(data.combined)) {
    titles <- c(titles, extractTitle(data.combined[i,"Name"]))
  
}
str(data.combined)
data.combined[1,"Name"]
titles
data.combined$Title <- as.factor(titles)

str(data.combined)

other<-data.combined[which(str_detect(data.combined$Title, "Other")), ]

other[,"Name"]

# Since we only have survived lables for the train set, only use the

# first 891 rows

ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")
