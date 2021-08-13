setwd("C:/Users/mathn/OneDrive/Desktop")
#Prep the Data and get the libraries (data.table and dplyr)
#that will be utilized
#All files were previously extracted and placed into data folder
library(data.table)
library(dplyr)

#Read metadata into variables, featureNames and activityLabels
featureNames <- read.table("./data/features.txt")
activityLabels <- read.table("./data/activity_labels.txt", header = FALSE)

#Read training data
subjectTrain <- read.table("./data/subject_train.txt", header = FALSE)
activityTrain <- read.table("./data/y_train.txt", header = FALSE)
featuresTrain <- read.table("./data/X_train.txt", header = FALSE)

#Read test data
subjectTest <- read.table("./data/subject_test.txt", header = FALSE)
activityTest <- read.table("./data/y_test.txt", header = FALSE)
featuresTest <- read.table("./data/X_test.txt", header = FALSE)

##Part 1: Merge training and test sets to create one dataset
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

#Name the columns
colnames(features) <- t(featureNames[2])

#Merge the data bind columns features, Activity and Subject
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)


##Part 2: Extract mean and standard deviation for each measurement
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

#Checking dimensions of extracted data
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

extractedData <- completeData[,requiredColumns]
dim(extractedData)


##Part 3: Uses descriptive activity names to name the activities in the data set

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

extractedData$Activity <- as.factor(extractedData$Activity)

##Part 4: Appropriately labels the data set with descriptive
##variable names

#List names
names(extractedData)

#By examining extractedData, we can say that the following acronyms can be replaced:
#Acc can be replaced with Accelerometer
#Gyro can be replaced with Gyroscope
#BodyBody can be replaced with Body
#Mag can be replaced with Magnitude
#Character f can be replaced with Frequency
#Character t can be replaced with Time

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

#List new names
names(extractedData)


##Part 5: From the data set in step 4, creates a second, independent tidy data set with the average of each 
##variable for each activity and each subject

#Set subject as a factor variable
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

#Create tidyData as a data set with average for each activity and subject. 

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)



  