library(plyr)

## File paths
test_data_path <- "./Data/test/X_test.txt"
test_labels_path <- "./Data/test/y_test.txt"

train_data_path <- "./Data/train/X_train.txt"
train_labels_path <- "./Data/train/y_train.txt"

activity_labels_path <- "./Data/activity_labels.txt"



## Read in files
test_data <- read.delim(test_data_path, header=FALSE, sep="", fill=TRUE)
test_labels <- read.delim(test_labels_path, header=FALSE, sep="", fill = TRUE)

train_data <- read.delim(train_data_path, header=FALSE, sep="", fill=TRUE,)
train_labels <- read.delim(train_labels_path, header=FALSE, sep="", fill=TRUE)

activity_labels <- read.delim(activity_labels_path, header=FALSE, sep="", fill=TRUE)



## Sanity check
#dim(test_data)
#dim(test_labels)
#str(test_data)

#dim(train_data)
#dim(train_labels)
#str(train_data)



## Make descriptive labels
# Convert labels to character vectors
test_labels <- as.character(test_labels$V1)
train_labels <- as.character(train_labels$V1)

# Replace numbers with descriptive strings and convert to factor
better_test_labels <- as.factor(revalue(test_labels, replace = c("1"="WALKING", "2"="WALKING_UPSTAIRS", "3"="WALKING_DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING")))
better_train_labels <- as.factor(revalue(train_labels, replace = c("1"="WALKING", "2"="WALKING_UPSTAIRS", "3"="WALKING_DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING")))



# Add the new labels to test and training set
labeled_test_data <- cbind(test_data, better_test_labels)
labeled_train_data <- cbind(train_data, better_train_labels)

# 
names(labeled_test_data)[better_test_labels] = "labels"
names(labeled_train_data)["better_train_labels"] = "labels"












