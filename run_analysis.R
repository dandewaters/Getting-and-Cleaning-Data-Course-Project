library(plyr)
library(dplyr)
library(reshape2)
library(data.table)
library(tidyverse)

## File paths
# Test datasets and its corresponding activity labels
test_data_path <- "./Data/test/X_test.txt"
test_labels_path <- "./Data/test/y_test.txt"
# Training dataset and its corresponding activity labels
train_data_path <- "./Data/train/X_train.txt"
train_labels_path <- "./Data/train/y_train.txt"
# Column names
features_path <- "./Data/features.txt"



## Read in files
# Test datasets and its corresponding activity labels
test_data <- read.delim(test_data_path, header=FALSE, sep="", fill=TRUE)
test_labels <- read.delim(test_labels_path, header=FALSE, sep="", fill = TRUE)
# Training dataset and its corresponding activity labels
train_data <- read.delim(train_data_path, header=FALSE, sep="", fill=TRUE,)
train_labels <- read.delim(train_labels_path, header=FALSE, sep="", fill=TRUE)
# Column names
features <- read.delim(features_path, header=FALSE, sep="", fill=TRUE)



## Make descriptive labels
# Convert labels to character vectors
test_labels <- as.character(test_labels$V1)
train_labels <- as.character(train_labels$V1)

# Replace numbers with descriptive strings and convert to factor
better_test_labels <- as.factor(revalue(test_labels, replace = c("1"="WALKING", "2"="WALKING UPSTAIRS", "3"="WALKING DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING")))
better_train_labels <- as.factor(revalue(train_labels, replace = c("1"="WALKING", "2"="WALKING UPSTAIRS", "3"="WALKING DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING")))

# Add the new activity labels to test and training set
labeled_test_data <- cbind(test_data, better_test_labels)
labeled_train_data <- cbind(train_data, better_train_labels)

# Fix label column name to avoid error when merging
setnames(labeled_test_data, "better_test_labels", "labels")
setnames(labeled_train_data, "better_train_labels", "labels")


## Merge the train and test data sets
combined_data <- rbind(labeled_test_data, labeled_train_data)



## Fix column names
# extract actual feature names from table
fixed_features <- as.character(features[,2])
# Add the "activity" column name to vector of column names
all_features <- append(fixed_features, "activity")
# Add column names to data frame
names(combined_data) <- all_features



## Extract columns that are means and standard deviations
# Search for columns with names that contain "mean" or "std"
relevant_columns <- grep("std|mean", names(combined_data))
relevant_data <- combined_data[relevant_columns]
# Makes row names as IDs for melting
subjectID <- 1:dim(relevant_data)[1]
# Re-add activity labels and subject ID columns
relevant_data <- cbind(relevant_data, combined_data["activity"], subjectID)



## Reshape Data into narrow form, melting columns by feature, dimension, and measurement
# Get column names for melting (Same columns I used from before)
melting_columns <- names(combined_data[relevant_columns])
# Melt features
molten_data <- melt(relevant_data, id=c("subjectID", "activity"), measure.vars = melting_columns)
# Fix feature column name
setnames(molten_data, "variable", "feature")
# Cast feature column to character vector for splitting
molten_data$feature <- as.character(molten_data$feature)

# add columns for dimension and measurement
# Searches feature names for dimension 
make_dimension <- function(feature){
  sapply(feature, function(x) if(grep("-X", x)) "X" else if(grep("-Y", x)) "Y" else if(grep("-Z", x)) "Z" else NA)
}
# Searches feature names for measurement
make_measurement <- function(measurement){
  sapply(measurement, function(x) if(grep("mean()", x)) "Mean" else if(grep("std()", x)) "Standard Deviation" else if(grep("meanFreq()", x)) "Mean Frequency")
}
#make_dimension <- function(value){
#       if(grep("-X", value)){"X"}
#  else if(grep("-Y", value)){"Y"}
#  else if(grep("-Z", value)){"Z"}
#  else{return(NA)}
#}
# Searches feature names for measurement
#make_measurement <- function(value){
#  if(grep("mean()", value)){return("Mean")}
#  else if(grep("std()", value)){return("Standard Deviation")}
#  else if(grep("meanFreq()", value)){return("Mean Frequency")}
#  else{return(NA)}
#}

final_tidy_data <- tidyr::separate(data=molten_data, col=feature, into=c("feature", "measurement", "dimension"), sep="-", fill="right")
final_tidy_data %>%
  mutate(measurement=gsub("\\(\\)", "", measurement)) %>%
  mutate(measurement=as.factor(measurement)) %>%
  mutate(dimension=as.factor(dimension))
  
# Adds dimension and measurement columns, cleans variable column
#final_tidy_data <- molten_data %>%
#  mutate(dimension=make_dimension(feature)) %>%
#  mutate(measurement=make_measurement(feature)) %>%
#  mutate(variable=gsub("-mean|-std|-meanfreq|-X|-Y|-Z|\\(\\)", "", variable))

View(final_tidy_data)
sum(is.na(final_tidy_data$dimension))
table()

## Make second dataset with average of each variable for each activity and subject
# Make groups

## Try this vvv
#by_activity_groups <- group_by(final_tidy_data, activity)
#summarize(by_activity_groups, mean)
tidy_data_mean <-
  final_tidy_data %>%
  group_by(activity) %>%
  summarise(tBodyAccMean = mean())



# Incase that doesn't work:
walking_data <- final_tidy_data[final_tidy_data$activity == "WALKING",]
walking_upstairs_data <- final_tidy_data[final_tidy_data$activity == "WALKING UPSTAIRS",]
walking_downstairs_data <- final_tidy_data[final_tidy_data$activity == "WALKING DOWNSTAIRS",]
sitting_data <- final_tidy_data[final_tidy_data$activity == "SITTING",]
standing_data <- final_tidy_data[final_tidy_data$activity == "STANDING",]
laying_data  <- final_tidy_data[final_tidy_data$activity == "LAYING",]

# Get mean of each feature by activity group
walking_mean <- lapply(walking_data, mean)
walking_upstairs_mean <- lapply(walking_upstairs_data, mean)
walking_downstairs_mean <- lapply(walking_downstairs_data, mean)
sitting_mean <- lapply(sitting_data, mean)
standing_mean <- lapply(standing_data, mean)
laying_mean <- lapply(laying_data, mean)

# Recombine activity groups into one tidy dataset
tidy_data_mean <- rbind(walking_mean, walking_upstairs_mean, walking_downstairs_mean, sitting_mean, standing_mean, laying_mean)

--"tBodyAcc-mean()-X"               "tBodyAcc-mean()-Y"               "tBodyAcc-mean()-Z"              
--"tBodyAcc-std()-X"                "tBodyAcc-std()-Y"                "tBodyAcc-std()-Z"               
--"tGravityAcc-mean()-X"            "tGravityAcc-mean()-Y"            "tGravityAcc-mean()-Z"           
--"tGravityAcc-std()-X"             "tGravityAcc-std()-Y"             "tGravityAcc-std()-Z"            
--"tBodyAccJerk-mean()-X"           "tBodyAccJerk-mean()-Y"           "tBodyAccJerk-mean()-Z"          
--"tBodyAccJerk-std()-X"            "tBodyAccJerk-std()-Y"            "tBodyAccJerk-std()-Z"           
--"tBodyGyro-mean()-X"              "tBodyGyro-mean()-Y"              "tBodyGyro-mean()-Z"             
--"tBodyGyro-std()-X"               "tBodyGyro-std()-Y"               "tBodyGyro-std()-Z"              
--"tBodyGyroJerk-mean()-X"          "tBodyGyroJerk-mean()-Y"          "tBodyGyroJerk-mean()-Z"         
--"tBodyGyroJerk-std()-X"           "tBodyGyroJerk-std()-Y"           "tBodyGyroJerk-std()-Z"          
--"tBodyAccMag-mean()"              "tBodyAccMag-std()"               
--"tGravityAccMag-mean()"           "tGravityAccMag-std()"
--"tBodyAccJerkMag-mean()"          "tBodyAccJerkMag-std()"          
--"tBodyGyroMag-mean()"             "tBodyGyroMag-std()"
--"tBodyGyroJerkMag-mean()"         "tBodyGyroJerkMag-std()"          
--"fBodyAcc-mean()-X"               "fBodyAcc-mean()-Y"               "fBodyAcc-mean()-Z"               
--"fBodyAcc-std()-X"                "fBodyAcc-std()-Y"                "fBodyAcc-std()-Z"                
--"fBodyAcc-meanFreq()-X"           "fBodyAcc-meanFreq()-Y"           "fBodyAcc-meanFreq()-Z"           
--"fBodyAccJerk-mean()-X"           "fBodyAccJerk-mean()-Y"           "fBodyAccJerk-mean()-Z"           
--"fBodyAccJerk-std()-X"            "fBodyAccJerk-std()-Y"            "fBodyAccJerk-std()-Z"            
--"fBodyAccJerk-meanFreq()-X"       "fBodyAccJerk-meanFreq()-Y"       "fBodyAccJerk-meanFreq()-Z"       
--"fBodyGyro-mean()-X"              "fBodyGyro-mean()-Y"              "fBodyGyro-mean()-Z"              
--"fBodyGyro-std()-X"               "fBodyGyro-std()-Y"               "fBodyGyro-std()-Z"               
--"fBodyGyro-meanFreq()-X"          "fBodyGyro-meanFreq()-Y"          "fBodyGyro-meanFreq()-Z"          
--"fBodyAccMag-mean()"              "fBodyAccMag-std()"               "fBodyAccMag-meanFreq()"          
--"fBodyBodyAccJerkMag-mean()"      "fBodyBodyAccJerkMag-std()"       "fBodyBodyAccJerkMag-meanFreq()"  
--"fBodyBodyGyroMag-mean()"         "fBodyBodyGyroMag-std()"          "fBodyBodyGyroMag-meanFreq()"     
--"fBodyBodyGyroJerkMag-mean()"     "fBodyBodyGyroJerkMag-std()"      "fBodyBodyGyroJerkMag-meanFreq()"


feature, measurement, dimension
tBodyAcc - mean, std - x,y,z
tGravityAcc
tBodyAccJerk
tBodyGyro
tBodyGyroJerk
fBodyAccJerk
fBodyGyro

tBodyAccMag - mean, std
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc - meanfreq - x,y,z
fBodyAccJerk
fBdoyGyro

fBodyAccMag - mean,std,meanfreq
fBodyBodyAccJerkMag
fBodyBodyGyroMag
fBodyBodyGyroJerkMag

test <- c("tBodyAcc-mean()-X", "tBodyAcc-mean()-Y", "tBodyAcc-mean()-Z", "tBodyAcc-std()-X", "tBodyAcc-std()-Y", "tBodyAcc-std()-Z")

