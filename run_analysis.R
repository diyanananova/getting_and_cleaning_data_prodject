# download zip file containing data if it hasn't already been downloaded
zip_Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zip_File <- "UCI HAR Dataset.zip"

if (!file.exists(zip_File)) {
  download.file(zip_Url, zip_File, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
data_url <- "UCI HAR Dataset"
if (!file.exists(data_url)) {
  unzip(zip_File)
}



# 1. read datei
## 1.1.1. read train vector
x_test <- read.table(file.path(data_url, "train", "X_train.txt"))
y_test <- read.table(file.path(data_url, "train", "y_train.txt"))
subject_test <- read.table(file.path(data_url, "train", "subject_train.txt"))

## 1.1.2.read test vector
x_train <- read.table(file.path(data_url, "test", "X_test.txt"))
y_train <- read.table(file.path(data_url, "test", "y_test.txt"))
subject_train <- read.table(file.path(data_url, "test", "subject_test.txt"))

# 1.1.3 Read feature vector
features <- read.table(file.path(data_url, "features.txt"), as.is = TRUE)

# 1.1.4 Read activity labels
activityLabels = read.table(file.path(data_url, "activity_labels.txt"))

# 1.2 Assigning variable names
colnames(x_train) <- features[,2]
colnames(y_train) <- "activity_ID"
colnames(subject_train) <- "subject_ID"

colnames(x_test) <- features[,2]
colnames(y_test) <- "activity_ID"
colnames(subject_test) <- "subject_ID"

colnames(activityLabels) <- c("activity_ID", "activity_Type")

# 1.3 Merging all datasets into one set

test_all <- cbind(y_test, subject_test, x_test)
train_all <- cbind(y_train, subject_train, x_train)
test_train <- rbind(train_all, test_all)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.

mean_sd <- select(test_train, activity_ID, subject_ID, contains(c("mean", "std")))


#3. Use descriptive activity names
mean_sd <- left_join(activityLabels, mean_sd)
mean_sd$activity <- paste(mean_sd$activity_ID, mean_sd$activity_Type)



#4. Label the data set with descriptive variable names

# 4.1. remove special characters
names(mean_sd) <- gsub("-", "", names(mean_sd))
names(mean_sd) <- gsub("()", "", names(mean_sd))

# 4.2. cleaning up /names
names(mean_sd) <- gsub("^f", "frequencyDomain", names(mean_sd))
names(mean_sd)<- gsub("^t", "timeDomain", names(mean_sd))
names(mean_sd)<- gsub("Acc", "Accelerometer", names(mean_sd))
names(mean_sd) <- gsub("Gyro", "Gyroscope", names(mean_sd))
names(mean_sd) <- gsub("Mag", "Magnitude", names(mean_sd))
names(mean_sd) <- gsub("Freq", "Frequency", names(mean_sd))
names(mean_sd) <- gsub("mean", "Mean", names(mean_sd))
names(mean_sd) <- gsub("std", "StandardDeviation", names(mean_sd))

# 4.3. correct 
names(mean_sd) <- gsub("BodyBody", "Body", names(mean_sd))


#5. Creating a second,  independent tidy data set with the avg of each variable for each activity and subject
avg_man_sd <- mean_sd %>%
              select(-c('activity_ID','activity_Type')) %>%
              group_by(activity, subject_ID) %>%
              summarise_all(mean) %>%
              arrange(.by_group = FALSE) %>% 
              relocate(activity)

#6. output to file "tidy_data.txt"
write.table(avg_man_sd, "tidy_data_exmpl.txt", row.names = FALSE, 
            quote = FALSE)
              