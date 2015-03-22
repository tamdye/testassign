

featuresColNames <- read.table("/users/txd031/Downloads/UCI HAR dataset/features.txt")

trainFeatures <- read.table("/users/txd031/Downloads/UCI HAR dataset/train/x_train.txt")
colnames(trainFeatures) = featuresColNames[,2]
trainSubjects <- read.table("/users/txd031/Downloads/UCI HAR dataset/train/subject_train.txt")
trainActivities <- read.table("/users/txd031/Downloads/UCI HAR dataset/train/y_train.txt")
totalTrainData <- cbind(trainSubjects,trainActivities,trainFeatures)

testFeatures <- read.table("/users/txd031/Downloads/UCI HAR dataset/test/x_test.txt")
colnames(testFeatures) = featuresColNames[,2]
testSubjects <- read.table("/users/txd031/Downloads/UCI HAR dataset/test/subject_test.txt")
testActivities <- read.table("/users/txd031/Downloads/UCI HAR dataset/test/y_test.txt")
totalTestData <- cbind(testSubjects,testActivities,testFeatures)

totalData <- rbind(totalTrainData,totalTestData

extractColumns <- c(1, 2, 3:8, 43:48, 83:88, 123:128, 163:168, 203:204, 216:217, 229:230,
                    242:243, 255:256, 268:273, 296:298, 347:352, 375:377, 426:431,
                    454:456, 505:506, 515, 518:519, 528, 531:532, 541, 544:545, 554)
extractTotalData <- totalData[,extractColumns]

activityLabels <- read.table("/users/txd031/Downloads/UCI HAR dataset/activity_labels.txt")

mergedData <- merge(extractTotalData,activityLabels,by.x="V1.1",by.y="V1")
tidyData <- cbind("subject"=mergedData[,1],"activity"=mergedData[,82],
                    mergedData[,3:81])

smallTidyData <- tidyData[1:10,1:5]

moltenTidyData <- melt(smallTidyData,id=c("subject","activity"),
                       measure.vars=c("tBodyAcc-mean()-X","tBodyAcc-mean()-Y",
                                      "tBodyAcc-mean()-Z"))

groups <- group_by(moltenTidyData,subject,activity,variable)
summarize(groups,avg=mean(value))





##Step 1:  Merge the training and test sets to create on data set
trainFeatures <- read.table("/users/txd031/Downloads/UCI HAR dataset/train/x_train.txt")
trainSubjects <- read.table("/users/txd031/Downloads/UCI HAR dataset/train/subject_train.txt")
trainActivities <- read.table("/users/txd031/Downloads/UCI HAR dataset/train/y_train.txt")
totalTrainData <- cbind(trainSubjects,trainActivities,trainFeatures)

testFeatures <- read.table("/users/txd031/Downloads/UCI HAR dataset/test/x_test.txt")
testSubjects <- read.table("/users/txd031/Downloads/UCI HAR dataset/test/subject_test.txt")
testActivities <- read.table("/users/txd031/Downloads/UCI HAR dataset/test/y_test.txt")
totalTestData <- cbind(testSubjects,testActivities,testFeatures)

totalData <- rbind(totalTrainData,totalTestData)

##Step 2:  Extract only the measurements on the mean and standard deviation
meanCols <- grep("mean", featuresColNames[,2])
stdCols <- grep("std", featuresColNames[,2])
Cols <- c(1,2,sort(c(meanCols,stdCols))+2)  ##added two because the data has two more cols than the features

extractTotalData <- totalData[,Cols]

##Step 3:  Use descriptive activity names to name the activities in the dataset
activityLabels <- read.table("/users/txd031/Downloads/UCI HAR dataset/activity_labels.txt")

mergedData <- merge(extractTotalData,activityLabels,by.x="V1.1",by.y="V1")

finalData <- cbind("subject"=mergedData[,1],"activity"=mergedData[,82],
                  mergedData[,3:81])

##Step 4:  Appropriately label the dataset with descriptive variable names
variableLabels <- c(
"tBodyAccMeanX",
"tBodyAccMeanY",
"tBodyAccMeanZ",
"tBodyAccStdX",
"tBodyAccStdY",
"tBodyAccStdZ",
"tGravityAccMeanX",
"tGravityAccMeanY",
"tGravityAccMeanZ",
"tGravityAccStdX",
"tGravityAccStdY",
"tGravityAccStdZ",
"tBodyAccJerkMeanX",
"tBodyAccJerkMeanY",
"tBodyAccJerkMeanZ",
"tBodyAccJerkStdX",
"tBodyAccJerkStdY",
"tBodyAccJerkStdZ",
"tBodyGyroMeanX",
"tBodyGyroMeanY",
"tBodyGyroMeanZ",
"tBodyGyroStdX",
"tBodyGyroStdY",
"tBodyGyroStdZ",
"tBodyGyroJerkMeanX",
"tBodyGyroJerkMeanY",
"tBodyGyroJerkMeanZ",
"tBodyGyroJerkStdX",
"tBodyGyroJerkStdY",
"tBodyGyroJerkStdZ",
"tBodyAccMagMean",
"tBodyAccMagStd",
"tGravityAccMagMean",
"tGravityAccMagStd",
"tBodyAccJerkMagMean",
"tBodyAccJerkMagStd",
"tBodyGyroMagMean",
"tBodyGyroMagStd",
"tBodyGyroJerkMagMean",
"tBodyGyroJerkMagStd",
"fBodyAccMeanX",
"fBodyAccMeanY",
"fBodyAccMeanZ",
"fBodyAccStdX",
"fBodyAccStdY",
"fBodyAccStdZ",
"fBodyAccMeanFreqX",
"fBodyAccMeanFreqY",
"fBodyAccMeanFreqZ",
"fBodyAccJerkMeanX",
"fBodyAccJerkMeanY",
"fBodyAccJerkMeanZ",
"fBodyAccJerkStdX",
"fBodyAccJerkStdY",
"fBodyAccJerkStdZ",
"fBodyAccJerkMeanFreqX",
"fBodyAccJerkMeanFreqY",
"fBodyAccJerkMeanFreqZ",
"fBodyGyroMeanX",
"fBodyGyroMeanY",
"fBodyGyroMeanZ",
"fBodyGyroStdX",
"fBodyGyroStdY",
"fBodyGyroStdZ",
"fBodyGyroMeanFreqX",
"fBodyGyroMeanFreqY",
"fBodyGyroMeanFreqZ",
"fBodyAccMagMean",
"fBodyAccMagStd",
"fBodyAccMagMeanFreq",
"fBodyAccJerkMagMean",
"fBodyAccJerkMagStd",
"fBodyAccJerkMagMeanFreq",
"fBodyGyroMagMean",
"fBodyGyroMagStd",
"fBodyGyroMagMeanFreq",
"fBodyGyroJerkMagMean",
"fBodyGyroJerkMagStd",
"fBodyGyroJerkMagMeanFreq")
colnames(finalData) = c("subject","activity",variableLabels)

moltenFinalData <- melt(finalData,id=c("subject","activity"),
                        measure.vars=variableLabels)

##Step 5:  create a second, independent tidy dataset with the average of each
##variable for each activity and each subject

groups <- group_by(moltenFinalData,subject,activity,variable)
sumData <- summarize(groups,avg=mean(value))



