################################################################################
# Coursera Getting and Cleaning Data - Project
# This script does the following: 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each
#    measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
################################################################################

# libraries
library(plyr)

# set work dir
setwd("C:/Users/mdragt/SkyDrive/Coursera/DataCleaning/")

# 1. Merges the training and the test sets to create one data set.
###############################################################################
# get training data
x_train <- read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
y_train <- read.csv("UCI HAR Dataset/train/y_train.txt", sep="", header=FALSE)
subject_train <- read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", 
                          header=FALSE)

# get test data
x_test <- read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
y_test <- read.csv("UCI HAR Dataset/test/y_test.txt", sep="", header=FALSE)
subject_test <- read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", 
                         header=FALSE)

# make x dataset
x_data <- rbind(x_train, x_test)

# make y dataset
y_data <- rbind(y_train, y_test)

# make subject dataset
subject_data <- rbind(subject_train, subject_test)

# 2. Extracts only the measurements on the mean and standard deviation for each 
# measurement. 
###############################################################################

# get features
feature_data <- read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)

# select only mean() or std() columns. 
# There are other columns containing "mean" but they are not relevant
selectedMeasures <- grep("-(mean|std)\\(\\)", feature_data[, 2])

# get selected columns
x_data <- x_data[, selectedMeasures]

# clean the names of selected columns for future analysis
feature_data[,2] = gsub('-mean', 'Mean', feature_data[,2])
feature_data[,2] = gsub('-std', 'Std', feature_data[,2])
feature_data[,2] = gsub('[-()]', '', feature_data[,2])

# give data correct variable names
names(x_data) <- feature_data[selectedMeasures, 2]

# 3. Uses descriptive activity names to name the activities in the data set
###############################################################################

# get activity data
activities <- read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)

# update y_data values with activity names
y_data[, 1] <- activities[y_data[, 1], 2]


# 4. Appropriately labels the data set with descriptive variable names. 
###############################################################################

# correct column name
names(y_data) <- "activity"

# correct column name
names(subject_data) <- "subject"

# combine the 3 datasets
all_data <- cbind(subject_data, y_data, x_data)

# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
###############################################################################

# Get average of all variables (column 1 and 2 are subject and activity)
colNum <- ncol(all_data)
data_avg <- ddply(all_data, .(subject, activity), function(x) colMeans(x[, 3:colNum]))

# write file with header and separator to facilitate futur import
write.table(data_avg, "data_avg.txt", row.name=FALSE, sep=",")