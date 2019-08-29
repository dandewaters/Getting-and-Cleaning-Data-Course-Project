library(plyr)
library(dplyr)
library(reshape2)
library(data.table)
library(tidyverse)

# Get data file
data_file_name <- "UCI HAR Dataset"
if(!file.exists(data_file_name)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, destfile = "./Data.zip")
  unzip("./Data.zip", exdir = "./", overwrite = TRUE)
}

## File paths
# Test datasets and its corresponding activity labels and subject IDs
test_data_path <- "./UCI HAR Dataset/test/X_test.txt"
test_labels_path <- "./UCI HAR Dataset/test/y_test.txt"
test_subjects_path <- "./UCI HAR Dataset/test/subject_test.txt"
# Training dataset and its corresponding activity labels and subject IDs
train_data_path <- "./UCI HAR Dataset/train/X_train.txt"
train_labels_path <- "./UCI HAR Dataset/train/y_train.txt"
train_subjects_path <- "./UCI HAR Dataset/train/subject_train.txt"
# Column names
features_path <- "./UCI HAR Dataset/features.txt"



## Read in files
# Test datasets and its corresponding activity labels
test_data <- read.delim(test_data_path, header=FALSE, sep="", fill=TRUE)
test_labels <- read.delim(test_labels_path, header=FALSE, sep="", fill=TRUE)
test_subjects <- read.delim(test_subjects_path, header=FALSE, sep="", fill=TRUE)
# Training dataset and its corresponding activity labels
train_data <- read.delim(train_data_path, header=FALSE, sep="", fill=TRUE,)
train_labels <- read.delim(train_labels_path, header=FALSE, sep="", fill=TRUE)
train_subjects <- read.delim(train_subjects_path, header=FALSE, sep="", fill=TRUE)
# Column names
features <- read.delim(features_path, header=FALSE, sep="", fill=TRUE)



## Objective 1 - Merge the training and test data set
# Convert labels to character vectors
test_labels <- as.character(test_labels$V1)
train_labels <- as.character(train_labels$V1)

# Fix subject ID column names for merging
names(test_subjects) <- "subjectID"
names(train_subjects) <- "subjectID"

# Add the activity labels to test and training set
labeled_test_data <- cbind(test_data, test_labels, test_subjects)
labeled_train_data <- cbind(train_data, train_labels, train_subjects)

# Fix label and subjectID column names to avoid error when merging
setnames(labeled_test_data, "test_labels", "labels")
setnames(labeled_train_data, "train_labels", "labels")

# Merge the train and test data sets
combined_data <- rbind(labeled_test_data, labeled_train_data)



## Objective 3 - Add descriptive activity labels to dataset
combined_data$labels <- as.factor(revalue(combined_data$labels, replace = c("1"="walking", "2"="walking upstairs", "3"="walking downstairs", "4"="sitting", "5"="standing", "6"="laying")))



## Objective 4 - Appropriately label dataset with descriptive variable names
# Extract actual feature names from table
fixed_features <- as.character(features[,2])
# Add the "activity" column name to vector of column names
all_features <- append(fixed_features, c("activity", "subjectID"))
# Add column names to data frame
names(combined_data) <- all_features



## Objective 2 - Extract columns that are means and standard deviations
# Search for columns with names that contain "mean" or "std"
relevant_columns <- grep("std\\(\\)|mean\\(\\)", names(combined_data))
relevant_data <- combined_data[relevant_columns]

# Re-add activity labels and subject ID columns
relevant_data <- cbind(relevant_data, combined_data["activity"], combined_data["subjectID"])

# Convert subject IDs column to factor
relevant_data$subjectID <- as.factor(relevant_data$subjectID)



## Reshape Data into narrow form, melting columns by feature, dimension, and variable (for objective 5)
# Get column names for melting (Same columns I used from before)
melting_columns <- names(combined_data[relevant_columns])
# Melt features
molten_data <- melt(relevant_data, id=c("subjectID", "activity"), measure.vars = melting_columns)
# Fix feature column name
setnames(molten_data, "variable", "feature")

# Cast feature column to character vector for splitting
molten_data$feature <- as.character(molten_data$feature)
# Separate features, variables, and dimensions
molten_data <- tidyr::separate(data=molten_data, col=feature, into=c("feature", "variable", "dimension"), sep="-", fill="right")

# Clean up columns 
final_tidy_data <- 
  molten_data %>%
  # Remove parenthesis
  mutate(variable=gsub("\\(\\)", "", variable)) %>%
  # Replace "std" with "standard deviation" and "meanfreq" with "mean frequency"
  mutate(variable = revalue(variable, replace = c("std"="standard deviation"))) %>%
  # Clean up feature names
  mutate(feature = gsub("^t", "TimeDomain", feature)) %>%
  mutate(feature = gsub("^f", "FrequencyDomain", feature)) %>%
  mutate(feature = gsub("Freq", "Frequency", feature)) %>%
  mutate(feature = gsub("Mag", "Magnitude", feature)) %>%
  mutate(feature = gsub("Acc", "Accelerometer", feature)) %>%
  mutate(feature = gsub("Gyro", "Gyroscope", feature)) %>%
  # Cast features, variables, and dimensions to factors
  mutate(feature = as.factor(feature)) %>%
  mutate(variable = as.factor(variable)) %>%
  mutate(dimension = as.factor(dimension))
  
# Display the final result
View(final_tidy_data)
write.table(final_tidy_data, file="tidyData.txt", row.names=FALSE)


## Objective 5 - Make second dataset with average of each subject for each variable, activity, and subject
tidy_data_mean <-
  final_tidy_data %>%
  # Make groups
  group_by(subjectID, feature, variable, dimension, activity) %>%
  # Get means of groups
  summarise(value = mean(value))

# Display final result of means
View(tidy_data_mean)
write.table(tidy_data_mean, file="tidyDataMean.txt", row.names=FALSE)


