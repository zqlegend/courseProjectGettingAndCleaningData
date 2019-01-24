library(dplyr)

#Dowlowd the zip file
workPath <- "./GettingAndCleaningData/courseProject"
setwd(workPath)
urlFile <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if(!file.exists(zipFile)){
        download.file(urlFile, zipFile)
}

if(!file.exists(datapath)){
        unzip(zipFile)
}

#----------------------------------------------------------------------------#
#Read all the data files
#----------------------------------------------------------------------------#
#
#Read features
dataPath <- "UCI HAR Dataset"
features <- read.table(file.path(dataPath, "features.txt"), stringsAsFactors = FALSE)
#Read activity labels
actLabels <- read.table(file.path(dataPath, "activity_labels.txt"), stringsAsFactors = FALSE)
#
#Read training data
##training subjects
subTrain <- read.table(file.path(dataPath, "train", "subject_train.txt"))
##training values
valTrain <- read.table(file.path(dataPath, "train", "X_train.txt"))
##traingin labels
lablTrain <- read.table(file.path(dataPath, "train", "y_train.txt"))
#
#Read test data
##test subjects
subTest <- read.table(file.path(dataPath, "test", "subject_test.txt"))
##test values
valTest <- read.table(file.path(dataPath, "test", "X_test.txt"))
##test labels
lablTest <- read.table(file.path(dataPath, "test", "y_test.txt"))
#
#----------------------------------------------------------------------------#
#Merge the train and test data sets
#----------------------------------------------------------------------------#
#
#Merge all the train and test data into one data set
totalData <- rbind(cbind(subTrain, lablTrain, valTrain), 
                   cbind(subTest, lablTest, valTest))
#Remove the individual data set
rm("lablTest", "lablTrain", "subTest", "subTrain", "valTest", "valTrain")
#Assign column names
colnames(totalData) <- c("Subject", "Activity", features[,2])
#
#totalData <- tbl_df(totalData)
#
#---------------------------------------------------------------------------#
#Extracts only the measurements on the mean and 
#standard deviation for each measurement.
#---------------------------------------------------------------------------#
#
dataSelect <- totalData[, grepl("Subject|Activity|mean|std", colnames(totalData))]
#
#--------------------------------------------------------------------------#
#Uses descriptive activity names to name the activities in the data set
#--------------------------------------------------------------------------#
#
#Create a list where all the descriptive activity name corresponding to the labels
#are contained, assign it to the Activity column
act <- character()
for(i in dataSelect$Activity){
       act <- c(act, actLabels[which(actLabels[,1]==i), 2]) 
}
dataSelect$Activity <- act
#
#------------------------------------------------------------------------#
#Appropriately labels the data set with descriptive variable names
#-----------------------------------------------------------------------#
colNameData <- colnames(dataSelect)
colNameData <- gsub("^f","frequencyDomainSignal", colNameData)
colNameData <- gsub("^t","timeDomainSignal", colNameData)
colNameData <- gsub("Acc", "Accelerometer", colNameData)
colNameData <- gsub("Gyro", "Gyroscope", colNameData)
colNameData <- gsub("Mag", "Magnitude", colNameData)
colNameData <- gsub("mean\\(\\)", "mean", colNameData)
colNameData <- gsub("std\\(\\)", "standardDeviation", colNameData)
colNameData <- gsub("meanFreq\\(\\)", "meanFrequency", colNameData)
colNameData <- gsub("BodyBody", "Body", colNameData)
#
colnames(dataSelect) <- colNameData
#
#-----------------------------------------------------------------------#
#From the data set in step 4, creates a second,
#independent tidy data set with the average of each variable for 
#each activity and each subject.
#-----------------------------------------------------------------------#
#
dataSelect <- tbl_df(dataSelect)
dataSelect <- group_by(dataSelect, Activity, Subject)
tidyData <- summarise_each(dataSelect, funs(mean))
#
#----------------------------------------------------------------------#
#Write out the tidy data
#----------------------------------------------------------------------#
#
write.table(tidyData, file = "tidyData.txt",quote = FALSE, row.names = FALSE)
