#*************************************************************************************************************************
# Getting and Cleaning Data Course Project
#
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to 
# prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions 
# related to the project. 
# You will be required to submit: 
# 1) a tidy data set as described below, 
# 2) a link to a Github repository with your script for performing the analysis, and 
# 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up 
# the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how 
# all of the scripts work and how they are connected.  
#
# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . 
# Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
# The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S 
# smartphone. A full description is available at the site where the data was obtained: 
#        
#        http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#
#
# Here are the data for the project: 
#        
#        https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#
# Good luck!
#*************************************************************************************************************************

#*************************************************************************************************************************
# 1. Merges the training and the test sets to create one data set.

#set directory to where UCI HAR Dataset exists !!!CHANGE THIS!!!
setwd('C:/Users/hrhay/Documents/Micron/WFI Program/WFI Learning/Getting and cleaning data/data/UCI HAR Dataset/')

# get the training data from the various files and define column headers
activitylabels = read.table('./activity_labels.txt',header=FALSE);
features       = read.table('./features.txt',header=FALSE);  
subjecttrain   = read.table('./train/subject_train.txt',header=FALSE); 
xtrain         = read.table('./train/x_train.txt',header=FALSE); 
ytrain         = read.table('./train/y_train.txt',header=FALSE); 

colnames(activitylabels)  = c('activityid','activitylabels');
colnames(subjecttrain)  = "subjectid";
colnames(xtrain)        = features[,2]; 
colnames(ytrain)        = "activityid";

# Use cbind to merge the data into one training file
trainingdata = cbind(ytrain,subjecttrain,xtrain);

# get the testing data and define column headers
subjecttest = read.table('./test/subject_test.txt',header=FALSE); 
xtest       = read.table('./test/x_test.txt',header=FALSE); 
ytest       = read.table('./test/y_test.txt',header=FALSE); 

colnames(subjecttest) = "subjectid";
colnames(xtest)       = features[,2]; 
colnames(ytest)       = "activityid";

# Use cbind to merge the data into one test file
testdata = cbind(ytest,subjecttest,xtest);

# Use rbind to combine the training and the testing data
traintestdata = rbind(trainingdata,testdata);

# Create a vector for the column names from the traintestdata, which will be used to select the desired mean and standard deviation columns
colNames  = colnames(traintestdata); 

#*************************************************************************************************************************
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.  

# Get mean and standard deviation data only
meanstandarddata = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

traintestdata = traintestdata[meanstandarddata==TRUE];


#*************************************************************************************************************************
# 3. Uses descriptive activity names to name the activities in the data set

# Change to descriptive labels
traintestdata = merge(traintestdata,activitylabels,by='activityid',all.x=TRUE);
colNames  = colnames(traintestdata); 


#*************************************************************************************************************************
# 4. Appropriately labels the data set with descriptive variable names.

for (i in 1:length(colNames)) 
{
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("-std$","StdDev",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("^(t)","time",colNames[i])
        colNames[i] = gsub("^(f)","freq",colNames[i])
        colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

colnames(traintestdata) = colNames;


#*************************************************************************************************************************
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each 
# activity and each subject. 

# Remove activity labels in order to create tidy dataset
nolabels  = traintestdata[,names(traintestdata) != 'activitylabels'];

# Get average for of each variable for each activity and each subject
tidydata  = aggregate(nolabels[,names(nolabels) != c('activityid','subjectid')],by=list(activityid=nolabels$activityid,subjectid = nolabels$subjectid),mean);

# Assign activity names back
tidydata  = merge(tidydata,activitylabels,by='activityid',all.x=TRUE);

# Write the data back to working directory
write.table(tidydata, './tidydata.txt',row.names=FALSE,sep='\t');
