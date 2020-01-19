# Download the dataset
filename <- "Dataset.zip"

# Checking if archieve already exists.
if (!file.exists(filename)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename)
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename) 
}

# Assinging names for data frames and column names
features <- read.table("UCI HAR Dataset\\features.txt", col.names = c("n","functions"))
activity_labels <- read.table("UCI HAR Dataset\\activity_labels.txt", col.names = c("label", "activity"))
subject_test <- read.table("UCI HAR Dataset\\test\\subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset\\test\\X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset\\test\\y_test.txt", col.names = "label")
subject_train <- read.table("UCI HAR Dataset\\train\\subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset\\train\\X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset\\train\\y_train.txt", col.names = "label")

# Merge the training and the test sets to create one data set
a <- rbind(x_train, x_test)
b <- rbind(y_train, y_test)
subject <- rbind(subject_test, subject_train)
merged_data <- cbind(subject, b, a)

# Extract only the measurements on the mean and standard deviation for each measurement
x <- grepl("subject" , colnames(merged_data)) | grepl("label" , colnames(merged_data)) | grepl("[Mm]ean" , colnames(merged_data)) | grepl("[Ss][Tt][Dd]" , colnames(merged_data))
meanandstd <- merged_data[ , x == TRUE]

# Use descriptive activity names to name the activities in the data set
meanandstd$label <- activity_labels[meanandstd$label, 2]

# Appropriately label the data set with descriptive variable names
names(meanandstd)[2] = "activity"
names(meanandstd)<-gsub("Acc", "Accelerometer", names(meanandstd))
names(meanandstd)<-gsub("Gyro", "Gyroscope", names(meanandstd))
names(meanandstd)<-gsub("BodyBody", "Body", names(meanandstd))
names(meanandstd)<-gsub("Mag", "Magnitude", names(meanandstd))
names(meanandstd)<-gsub("^t", "Time", names(meanandstd))
names(meanandstd)<-gsub("^f", "Frequency", names(meanandstd))
names(meanandstd)<-gsub("tBody", "TimeBody", names(meanandstd))
names(meanandstd)<-gsub("-mean()", "Mean", names(meanandstd), ignore.case = TRUE)
names(meanandstd)<-gsub("-std()", "STD", names(meanandstd), ignore.case = TRUE)
names(meanandstd)<-gsub("-freq()", "Frequency", names(meanandstd), ignore.case = TRUE)
names(meanandstd)<-gsub("angle", "Angle", names(meanandstd))
names(meanandstd)<-gsub("gravity", "Gravity", names(meanandstd))

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
data <- aggregate(. ~subject + activity, meanandstd, mean)
data <- data[order(data$subject, data$activity),]
write.table(data, "data.txt", row.name=FALSE)