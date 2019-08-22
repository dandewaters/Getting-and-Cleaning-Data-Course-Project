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



## Objective 1 - Merge the training and test data set
# Convert labels to character vectors
test_labels <- as.character(test_labels$V1)
train_labels <- as.character(train_labels$V1)

# Add the activity labels to test and training set
labeled_test_data <- cbind(test_data, test_labels)
labeled_train_data <- cbind(train_data, train_labels)

# Fix label column name to avoid error when merging
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
all_features <- append(fixed_features, "activity")
# Add column names to data frame
names(combined_data) <- all_features



## Objective 2 - Extract columns that are means and standard deviations
# Search for columns with names that contain "mean" or "std"
relevant_columns <- grep("std\\(\\)|mean\\(\\)", names(combined_data))
relevant_data <- combined_data[relevant_columns]

# Makes row names as IDs for melting
subjectID <- 1:dim(relevant_data)[1]

# Re-add activity labels and subject ID columns
relevant_data <- cbind(relevant_data, combined_data["activity"], subjectID)



## Reshape Data into narrow form, melting columns by feature, dimension, and measurement (for objective 5)
# Get column names for melting (Same columns I used from before)
melting_columns <- names(combined_data[relevant_columns])
# Melt features
molten_data <- melt(relevant_data, id=c("subjectID", "activity"), measure.vars = melting_columns)
# Fix feature column name
setnames(molten_data, "variable", "feature")

# Cast feature column to character vector for splitting
molten_data$feature <- as.character(molten_data$feature)
# Separate features, measurements, and dimensions
molten_data <- tidyr::separate(data=molten_data, col=feature, into=c("feature", "measurement", "dimension"), sep="-", fill="right")

# Clean up columns 
final_tidy_data <- 
  molten_data %>%
  # Remove parenthesis
  mutate(measurement=gsub("\\(\\)", "", measurement)) %>%
  # Replace "std" with "standard deviation" and "meanfreq" with "mean frequency"
  mutate(measurement = revalue(measurement, replace = c("std"="standard deviation"))) %>%
  # Cast features, measurements, and dimensions to factors
  mutate(feature = as.factor(feature)) %>%
  mutate(measurement = as.factor(measurement)) %>%
  mutate(dimension = as.factor(dimension))


# Display the final result
View(final_tidy_data)


## Objective 5 - Make second dataset with average of each variable for each activity and subject
tidy_data_mean <-
  final_tidy_data %>%
  # Make groups
  group_by(feature, measurement, dimension, activity) %>%
  # Get means of groups
  summarise(value = mean(value))

# Display final result of means
View(tidy_data_mean)



