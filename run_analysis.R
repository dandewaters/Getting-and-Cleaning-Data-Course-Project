library(plyr)
library(dplyr)

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
# Add in activity labels column, completing the tidy dataset in objectives 1-4
final_tidy_data <- cbind(relevant_data, combined_data["activity"])



## Make second dataset with average of each variable for each activity and subject
# Make groups

## Try this vvv
by_activity_groups <- group_by(final_tidy_data, activity)
summarize(by_activity_groups, mean)

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














