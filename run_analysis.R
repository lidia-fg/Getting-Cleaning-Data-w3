readfile <- function(name) {
  read.csv(sprintf ("UCI HAR Dataset/%s", name), sep="", header=FALSE)
}

# Read files
training = readfile("train/X_train.txt")
training[,562] = readfile("train/y_train.txt")
training[,563] = readfile("train/subject_train.txt")
testing = readfile("test/X_test.txt")
testing[,562] = readfile("test/y_test.txt")
testing[,563] = readfile("test/subject_test.txt")
activityLabels = readfile("activity_labels.txt")

# Replace feature names with better ones
features = readfile("features.txt")
features[,2] = gsub('-mean', 'Mean', features[,2])
features[,2] = gsub('-std', 'Std', features[,2])
features[,2] = gsub('[-()]', '', features[,2])

# Merge training and test sets
allData = rbind(training, testing)

# Get only the data on mean and std. dev.
colsWeWant <- grep(".*Mean.*|.*Std.*", features[,2])

# Not interested on the whole features table
features <- features[colsWeWant,]

# Add the last two columns (subject and activity)
colsWeWant <- c(colsWeWant, 562, 563)

# Also crop allData. Not everything is needed there
allData <- allData[,colsWeWant]

# Add the column names (features) to allData
colnames(allData) <- c(features$V2, "Activity", "Subject")
colnames(allData) <- tolower(colnames(allData))

currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
  allData$activity <- gsub(currentActivity, currentActivityLabel, allData$activity)
  currentActivity <- currentActivity + 1
}

allData$activity <- as.factor(allData$activity)
allData$subject <- as.factor(allData$subject)

tidy = aggregate(allData, by=list(activity = allData$activity, subject=allData$subject), mean)

# Finally, eliminate the subject and activity columns since their mean is useless
tidy[,90] = NULL
tidy[,89] = NULL

# Write the output file
write.table(tidy, "tidy.txt", row.name=FALSE)
