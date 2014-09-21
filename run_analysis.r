###############################################################################
# This script is used for reading and extracting the raw data (variables
# measure using sensors) for different subjects performing different activities
# The script then cleans the data by retaining only relevant information. It also
# updates the variable names and activity labels to provide a more understandable
# name to them. The script finally performs mean calculation on the variable columns
# (after grouping them by subject and activity parameters) and writes a clean
# data set to a text file ready to be used by the end user
###############################################################################
###############################################################################
# Downloading raw data from http link and unzipping it in HAR1 directory#
###############################################################################
 zipdir <- tempfile()
 
 dir.create(zipdir)
 
 
 if(!file.exists("HAR1")){
   dir.create("HAR1")
 }
 fileurl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
 download.file(fileurl,dest = "./HAR1/zipfile.zip",mode ="wb")
 unzip("./HAR1/zipfile.zip",exdir="zipdir")
###############################################################################

###############################################################################
#Reading raw data files from the download unzipped files
###############################################################################
y_testDF <- read.table("./zipdir/UCI HAR Dataset/test/y_test.txt",sep="")
X_testDF <- read.table("./zipdir/UCI HAR Dataset/test/X_test.txt",sep="")
subject_testDF <- read.table("./zipdir/UCI HAR Dataset/test/subject_test.txt",sep="")

y_trainDF <- read.table("./zipdir/UCI HAR Dataset/train/y_train.txt",sep="")
X_trainDF <- read.table("./zipdir/UCI HAR Dataset/train/X_train.txt",sep="")
subject_trainDF <- read.table("./zipdir/UCI HAR Dataset/train/subject_train.txt",sep="")
###############################################################################
#Reading variable names from features.txt file
###############################################################################
featuresDF <- read.table("./zipdir/UCI HAR Dataset/features.txt",stringsAsFactors=FALSE,sep="")
featuresDF <- featuresDF[,c(2)]
###############################################################################
#Combining subject_train.txt, y_train.txt, X_train.txt file contents to get
#train data frame. Renaming train data frame columns with a descriptive name
###############################################################################
trainDF <- cbind(subject_trainDF,y_trainDF,X_trainDF)
names(trainDF) <- c("Subject","ActivityLabels",featuresDF )
###############################################################################
#Combining subject_test.txt, y_test.txt, X_test.txt file contents to get
#test data frame. Renaming test data frame columns with a descriptive name
###############################################################################
testDF <- cbind(subject_testDF,y_testDF,X_testDF)
names(testDF) <- c("Subject","ActivityLabels",featuresDF)
###############################################################################
# Step 1 - Merging train and test data to obtain a single data frame (still dirty)
###############################################################################
traintestDF <- rbind(trainDF,testDF)
###########################################################################
###############################################################################
# Step 2 - Creating a data frame with only mean() and std() variables and 
#          subject ID and activity labels
###############################################################################
features_mean_std <- grep("mean\\(\\)|std\\(\\)",featuresDF,perl=TRUE)
traintestDF <- traintestDF[,c(1,2,features_mean_std +2)]
############################################################################
###############################################################################
# Step 3 - Subsituting descriptive names for activity labels (replacing numeric
#          value with descriptive names)
###############################################################################
traintestDF$ActivityLabels <- gsub("1","WALKING",traintestDF$ActivityLabels,perl=TRUE)
traintestDF$ActivityLabels <- gsub("2","WALKING_UPSTAIRS",traintestDF$ActivityLabels,perl=TRUE)
traintestDF$ActivityLabels <- gsub("3","WALKING_DOWNSTAIRS",traintestDF$ActivityLabels,perl=TRUE)
traintestDF$ActivityLabels <- gsub("4","SITTING",traintestDF$ActivityLabels,perl=TRUE)
traintestDF$ActivityLabels <- gsub("5","STANDING",traintestDF$ActivityLabels,perl=TRUE)
traintestDF$ActivityLabels <- gsub("6","LAYING",traintestDF$ActivityLabels,perl=TRUE)
###############################################################################
# Step 4 - Replacing variables with a better description 
###############################################################################
names(traintestDF)<-gsub("(t)(\\w+)Mag-mean\\(\\)","Mean in time domain of magnitude of \\2 in \\3 direction",names(traintestDF),perl=TRUE)
names(traintestDF)<-gsub("(t)(\\w+)Mag-std\\(\\)","Standard Deviation in time domain of magnitude of \\2 in \\3 direction",names(traintestDF),perl=TRUE)
names(traintestDF)<-gsub("(f)(\\w+)Mag-mean\\(\\)","Mean in frequency domain of magnitude of \\2 in \\3 direction",names(traintestDF),perl=TRUE)
names(traintestDF)<-gsub("(f)(\\w+)Mag-std\\(\\)","Standard Deviation in frequency domain of magnitude of \\2 in \\3 direction",names(traintestDF),perl=TRUE)

names(traintestDF)<-gsub("(t)(\\w+)-mean\\(\\)-(\\w+)","Mean in time domain of \\2 in \\3 direction",names(traintestDF),perl=TRUE)
names(traintestDF)<-gsub("(t)(\\w+)-std\\(\\)-(\\w+)","Standard Deviation in time domain of \\2 in \\3 direction",names(traintestDF),perl=TRUE)
names(traintestDF)<-gsub("(f)(\\w+)-mean\\(\\)-(\\w+)","Mean in frequency domain of \\2 in \\3 direction",names(traintestDF),perl=TRUE)
names(traintestDF)<-gsub("(f)(\\w+)-std\\(\\)-(\\w+)","Standard Deviation in frequency domain of \\2 in \\3 direction",names(traintestDF),perl=TRUE)

names(traintestDF)<-gsub("BodyBody","Body",names(traintestDF),perl=TRUE)

names(traintestDF)<-gsub("(\\w+)\\sBodyAccJerk\\s(\\w+)","\\1 jerk in linear acceleration of body \\2",names(traintestDF),perl=TRUE)
names(traintestDF)<-gsub("(\\w+)\\sBodyGyroJerk\\s(\\w+)","\\1 jerk in angular velocity of body \\2",names(traintestDF),perl=TRUE)
names(traintestDF)<-gsub("(\\w+)\\sGravityAcc\\s(\\w+)","\\1 gravity acceleration \\2",names(traintestDF),perl=TRUE)
names(traintestDF)<-gsub("(\\w+)\\sBodyAcc\\s(\\w+)","\\1 body acceleration \\2",names(traintestDF),perl=TRUE)
names(traintestDF)<-gsub("(\\w+)\\sBodyGyro\\s(\\w+)","\\1 body angular velocity \\2",names(traintestDF),perl=TRUE)
###############################################################################
# Step 5 - Splitting data frame based on Subject ID and Activity and then calculating
#          mean values for all the variables
###############################################################################
traintestDF <- ddply(traintestDF,.(Subject,ActivityLabels),numcolwise(mean))
#names(traintestDF)<-gsub("^","Mean of ",names(traintestDF),perl=TRUE)
###############################################################################
# Writing out the tidy data value in traintest.txt file
###############################################################################
write.table(traintestDF,file="./traintest.txt",row.name=FALSE)
