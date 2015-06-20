 Merges the training and the test sets to create one data set.
 
train <- read.table("train/X_train.txt")
test <- read.table("test/X_test.txt")
x <- rbind(train, test)
train <- read.table("train/subject_train.txt")
test <- read.table("test/subject_test.txt")
x1 <- rbind(train, test)
train <- read.table("train/y_train.txt")
test <- read.table("test/y_test.txt")
x2 <- rbind(train, test)

Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
x <- x[, indices_of_good_features]
names(x) <- features[indices_of_good_features, 2]
names(x) <- gsub("\\(|\\)", "", names(x))
names(x) <- tolower(names(x))

Uses descriptive activity names to name the activities in the data set.

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
x2[,1] = activities[x2[,1], 2]

Appropriately labels the data set with descriptive activity names.

names(x2) <- "activity"
names(x1) <- "subject"
cleaned <- cbind(x1, x2, x)
write.table(cleaned, "merged_clean_data.txt")

Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(x1)[,1]
numSubjects = length(unique(x1)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]
row = 1
for (s in 1:numSubjects) {
for (a in 1:numActivities) {
result[row, 1] = uniqueSubjects[s]
result[row, 2] = activities[a, 2]
set1 <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
result[row, 3:numCols] <- colMeans(set1[, 3:numCols])
row = row+1
}
}
write.table(result, "data_set_with_the_averages.txt")
