
# preparations
setwd("G:/RSpace/project")

path = getwd()
inputPath = file.path(path, "UCI HAR Dataset")
outputPath = path
list.files(inputPath, recursive = TRUE)

# set the path of datasets
subjectTrainPath = file.path(inputPath, "train", "subject_train.txt")
subjectTestPath = file.path(inputPath, "test", "subject_test.txt")
YTrainPath = file.path(inputPath, "train", "Y_train.txt")
YTestPath = file.path(inputPath, "test", "Y_test.txt")
XTrainPath = file.path(inputPath, "train", "X_train.txt")
XTestPath = file.path(inputPath, "test", "X_test.txt")
featurePath = file.path(inputPath, "features.txt")
activityPath = file.path(inputPath, "activity_labels.txt")

# result path
tidyDataPath = file.path(outputPath, "tidy_data.txt")
meansDataPath = file.path(outputPath, "means_data.txt")

# # step 1 Merges the training and the test sets to create one data set.
# # read subject dataset
# subjectTrainData = read.table(subjectTrainPath)
# subjectTestData = read.table(subjectTestPath)
# 
# # read test and train dataset
# XTrainData = read.table(XTrainPath)
# XTestData = read.table(XTestPath)
# YTrainData = read.table(YTrainPath)
# YTestData = read.table(YTestPath)
# 
# # merging
# XData = rbind(XTrainData, XTestData)
# if(dim(XTrainData)[1] + dim(XTestData)[1] == dim(XData)[1] &
#      dim(XTrainData)[2] == dim(XTestData)[2]){
#   print("X data successfully merged.")
# }
# YData = rbind(YTrainData, YTestData)
# if(dim(YTrainData)[1] + dim(YTestData)[1] == dim(YData)[1] &
#      dim(YTrainData)[2] == dim(YTestData)[2]){
#   print("Y data successfully merged.")
# }
# subjectData = rbind(subjectTrainData, subjectTestData)
# if(dim(subjectTrainData)[1] + dim(subjectTestData)[1] == dim(subjectData)[1] &
#      dim(subjectTrainData)[2] == dim(subjectTestData)[2]){
#   print("subject data successfully merged.")
# }
# 
# # step 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
# featuresData = read.table(featurePath)
# meanStdIndices = grep("mean\\(\\)|std\\(\\)", featuresData[, 2])
# XData = XData[, meanStdIndices]
# 
# # step 3 Uses descriptive activity names to name the activities in the data set
# activity = read.table(activityPath)
# activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
# substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
# substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
# activityLabel <- activity[YData[, 1], 2]
# YData[, 1] <- activityLabel
# names(YData) <- c("activity")
# 
# step 4 Appropriately labels the data set with descriptive variable names. 
names(subjectData) = c("subject ID")
tidyData <- cbind(subjectData, YData, XData)
write.table(tidyData, tidyDataPath, row.name = FALSE)

# step 5 Creates a second, independent tidy data set with the average of each variable for 
# each activity and each subject.
subjectLen <- length(table(subjectData)) 
activityLen <- dim(activity)[1] 
columnLen <- dim(tidyData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(tidyData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(subjectData)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == tidyData$subject
    bool2 <- activity[j, 2] == tidyData$activity
    result[row, 3:columnLen] <- colMeans(tidyData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(result)
write.table(result, meansDataPath, row.name = FALSE) # write out the 2nd dataset