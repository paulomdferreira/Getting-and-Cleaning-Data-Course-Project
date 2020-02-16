#run_analysis.R
#Paulo Ferreira
#Getting and Cleaning Data
# 16 February 2020
#install.packages("dplyr")
#install.packages("data.table")
#Load packages
library(data.table)
library(dplyr)



#0. Getting and Reading data into RStudio
#Download UCI data files from the web, unzip them, and specify time/date settings
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destFile <- "CourseDataset.zip"
if (!file.exists(destFile)){
    download.file(URL, destfile = destFile, mode='wb')
}
if (!file.exists("./UCI_HAR_Dataset")){
    unzip(destFile)
}
dateDownloaded <- date()

#Reading Activity files
ActivityTest_Y <- read.table("./UCI HAR Dataset//test/y_test.txt", header = F)
ActivityTrain_Y<- read.table("./UCI HAR Dataset//train/y_train.txt", header = F)
#Read features files
FeaturesTest_X <- read.table("./UCI HAR Dataset//test/X_test.txt", header = F)
FeaturesTrain_X <- read.table("./UCI HAR Dataset//train/X_train.txt", header = F)
#Read subject files
SubjectTest <- read.table("./UCI HAR Dataset//test/subject_test.txt", header = F)
SubjectTrain <- read.table("./UCI HAR Dataset//train/subject_train.txt", header = F)
#Read Activity Labels
ActivityLabels <- read.table("./UCI HAR Dataset//activity_labels.txt", header = F)
#Read Feature Names
FeaturesNames <- read.table("./UCI HAR Dataset/features.txt", header = F)

#1. Merges the training and the test sets to create one data set. 
## Merge dataframes: Features Test&Train, Activity Test&Train, Subject Test&Train
FeaturesData <- rbind(FeaturesTest_X, FeaturesTrain_X)
SubjectData <- rbind(SubjectTest, SubjectTrain)
ActivityData <- rbind(ActivityTest_Y, ActivityTrain_Y)

#4. Appropriately labels the data set with descriptive variable names.
##Renaming colums in ActivityData & ActivityLabels dataframes
names(ActivityData) <- "ActivityN"
names(ActivityLabels) <- c("ActivityN", "Activity")
##Get factor of Activity names
Activity <- left_join(ActivityData, ActivityLabels, "ActivityN")[, 2]
##Rename SubjectData columns
names(SubjectData) <- "Subject"
##Rename FeaturesData columns using columns from FeaturesNames
names(FeaturesData) <- FeaturesNames$V2
##Create one large Dataset with only these variables: SubjectData,  Activity,  FeaturesData
DataSet <- cbind(SubjectData, Activity)
DataSet <- cbind(DataSet, FeaturesData)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
##Create New datasets by extracting only the measurements on the mean and standard deviation for each measurement
subFeaturesNames <- FeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", FeaturesNames$V2)]
DataNames <- c("Subject", "Activity", as.character(subFeaturesNames))
DataSet <- subset(DataSet, select=DataNames)

#3. Uses descriptive activity names to name the activities in the data set
##Rename the columns of the large dataset using more descriptive activity names
names(DataSet)<-gsub("^t", "time", names(DataSet))
names(DataSet)<-gsub("^f", "frequency", names(DataSet))
names(DataSet)<-gsub("Acc", "Accelerometer", names(DataSet))
names(DataSet)<-gsub("Gyro", "Gyroscope", names(DataSet))
names(DataSet)<-gsub("Mag", "Magnitude", names(DataSet))
names(DataSet)<-gsub("BodyBody", "Body", names(DataSet))

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
SecondDataSet<-aggregate(. ~Subject + Activity, DataSet, mean)
SecondDataSet<-SecondDataSet[order(SecondDataSet$Subject,SecondDataSet$Activity),]
##Save this data to  local file "cleandata.txt"
write.table(SecondDataSet, file = "cleandata.txt",row.name=FALSE)