# Script for Getting and Cleaning Data Course Project
# 
# Goals: 
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# 


#Open files

conTrain<-file("UCI HAR Dataset/train/X_train.txt","r")
conTest<-file("UCI HAR Dataset/test/X_test.txt","r")
conFeature<-file("UCI HAR Dataset/features.txt","r")
conCombined<-file("combined.txt","w")


#Read train and test data

DS_Train<-readLines(conTrain)
DS_Test<-readLines(conTest)


#Read field's names
DS_Features<-readLines(conFeature)
header<-""
DS_Features<-gsub(" ",":",DS_Features)
for (i in 1:length(DS_Features)){header<-paste(header,DS_Features[i])}

#Field subset with mean and std
Fields<-sort(c(grep("mean",DS_Features),grep("Mean",DS_Features),grep("std",DS_Features),grep("Std",DS_Features)))+1



#Clean train and test data sets from double/triple spaces
DS_Train<-gsub("  "," ",DS_Train);DS_Train<-gsub("  "," ",DS_Train)
DS_Test<-gsub("  "," ",DS_Test);DS_Test<-gsub("  "," ",DS_Test)

#Write combined data with appropriate fields' names

writeLines(header,conCombined)
writeLines(DS_Train,conCombined)
writeLines(DS_Test,conCombined)

close(conTrain);close(conTest);close(conFeature);close(conCombined);

#Pick up clean dataset

DS_Combined<-read.csv("combined.txt",sep = " ",colClasses = rep("numeric",length(DS_Features)+1))


#Get lables and put names to them
DS_Lables_Train<-read.csv2("UCI HAR Dataset/train/y_train.txt",sep = " ",header = FALSE)
DS_Lables_Test<-read.csv2("UCI HAR Dataset/test/y_test.txt",sep = " ",header = FALSE)
DS_Lables_Names<-read.csv2("UCI HAR Dataset/activity_labels.txt",sep = " ",header = FALSE)



#Get subjects for train and test
DS_Subject_Train<-read.csv2("UCI HAR Dataset/train/subject_train.txt",sep = " ",header = FALSE)
DS_Subject_Test<-read.csv2("UCI HAR Dataset/test/subject_test.txt",sep = " ",header = FALSE)



Activity<-merge(rbind(DS_Lables_Train,DS_Lables_Test),DS_Lables_Names)[,2]
Subject<-rbind(DS_Subject_Train,DS_Subject_Test)[,1]


#Merge activities and subject to data, filter fields with std,mean,Mean
DS_Extract<-cbind(Activity,Subject,DS_Combined[,Fields])




#ITEM 5 

#generate command for last set
comm<-""
for (i in 3:length(DS_Extract)){
    
fName<-names(DS_Extract)[i]    
comm<-paste(comm,fName,"=mean(",fName,"),")    
    
}


#Build group for summarization (Activity by Subject for mean of variables)

DS_Ext_Gr<-mutate(DS_Extract,Group=paste(Activity," by ",Subject))

#Grouping data
DS_Grouped_A_by_S<-group_by(DS_Ext_Gr,Group)


#Calculatiion of means for the grouping (generated in comm + formating)
DS_AVG_BY_Activity_Subject<-summarise(DS_Grouped_A_by_S,
          X1.tBodyAcc.mean...X =mean( X1.tBodyAcc.mean...X ), 
          X2.tBodyAcc.mean...Y =mean( X2.tBodyAcc.mean...Y ), 
          X3.tBodyAcc.mean...Z =mean( X3.tBodyAcc.mean...Z ), 
          X4.tBodyAcc.std...X =mean( X4.tBodyAcc.std...X ), 
          X5.tBodyAcc.std...Y =mean( X5.tBodyAcc.std...Y ), 
          X6.tBodyAcc.std...Z =mean( X6.tBodyAcc.std...Z ), 
          X41.tGravityAcc.mean...X =mean( X41.tGravityAcc.mean...X ), 
          X42.tGravityAcc.mean...Y =mean( X42.tGravityAcc.mean...Y ), 
          X43.tGravityAcc.mean...Z =mean( X43.tGravityAcc.mean...Z ), 
          X44.tGravityAcc.std...X =mean( X44.tGravityAcc.std...X ), 
          X45.tGravityAcc.std...Y =mean( X45.tGravityAcc.std...Y ), 
          X46.tGravityAcc.std...Z =mean( X46.tGravityAcc.std...Z ), 
          X81.tBodyAccJerk.mean...X =mean( X81.tBodyAccJerk.mean...X ), 
          X82.tBodyAccJerk.mean...Y =mean( X82.tBodyAccJerk.mean...Y ), 
          X83.tBodyAccJerk.mean...Z =mean( X83.tBodyAccJerk.mean...Z ), 
          X84.tBodyAccJerk.std...X =mean( X84.tBodyAccJerk.std...X ), 
          X85.tBodyAccJerk.std...Y =mean( X85.tBodyAccJerk.std...Y ), 
          X86.tBodyAccJerk.std...Z =mean( X86.tBodyAccJerk.std...Z ), 
          X121.tBodyGyro.mean...X =mean( X121.tBodyGyro.mean...X ), 
          X122.tBodyGyro.mean...Y =mean( X122.tBodyGyro.mean...Y ), 
          X123.tBodyGyro.mean...Z =mean( X123.tBodyGyro.mean...Z ), 
          X124.tBodyGyro.std...X =mean( X124.tBodyGyro.std...X ), 
          X125.tBodyGyro.std...Y =mean( X125.tBodyGyro.std...Y ), 
          X126.tBodyGyro.std...Z =mean( X126.tBodyGyro.std...Z ), 
          X161.tBodyGyroJerk.mean...X =mean( X161.tBodyGyroJerk.mean...X ), 
          X162.tBodyGyroJerk.mean...Y =mean( X162.tBodyGyroJerk.mean...Y ), 
          X163.tBodyGyroJerk.mean...Z =mean( X163.tBodyGyroJerk.mean...Z ), 
          X164.tBodyGyroJerk.std...X =mean( X164.tBodyGyroJerk.std...X ), 
          X165.tBodyGyroJerk.std...Y =mean( X165.tBodyGyroJerk.std...Y ), 
          X166.tBodyGyroJerk.std...Z =mean( X166.tBodyGyroJerk.std...Z ), 
          X201.tBodyAccMag.mean.. =mean( X201.tBodyAccMag.mean.. ), 
          X202.tBodyAccMag.std.. =mean( X202.tBodyAccMag.std.. ), 
          X214.tGravityAccMag.mean.. =mean( X214.tGravityAccMag.mean.. ), 
          X215.tGravityAccMag.std.. =mean( X215.tGravityAccMag.std.. ), 
          X227.tBodyAccJerkMag.mean.. =mean( X227.tBodyAccJerkMag.mean.. ), 
          X228.tBodyAccJerkMag.std.. =mean( X228.tBodyAccJerkMag.std.. ), 
          X240.tBodyGyroMag.mean.. =mean( X240.tBodyGyroMag.mean.. ), 
          X241.tBodyGyroMag.std.. =mean( X241.tBodyGyroMag.std.. ), 
          X253.tBodyGyroJerkMag.mean.. =mean( X253.tBodyGyroJerkMag.mean.. ), 
          X254.tBodyGyroJerkMag.std.. =mean( X254.tBodyGyroJerkMag.std.. ), 
          X266.fBodyAcc.mean...X =mean( X266.fBodyAcc.mean...X ), 
          X267.fBodyAcc.mean...Y =mean( X267.fBodyAcc.mean...Y ), 
          X268.fBodyAcc.mean...Z =mean( X268.fBodyAcc.mean...Z ), 
          X269.fBodyAcc.std...X =mean( X269.fBodyAcc.std...X ), 
          X270.fBodyAcc.std...Y =mean( X270.fBodyAcc.std...Y ), 
          X271.fBodyAcc.std...Z =mean( X271.fBodyAcc.std...Z ), 
          X294.fBodyAcc.meanFreq...X =mean( X294.fBodyAcc.meanFreq...X ), 
          X295.fBodyAcc.meanFreq...Y =mean( X295.fBodyAcc.meanFreq...Y ), 
          X296.fBodyAcc.meanFreq...Z =mean( X296.fBodyAcc.meanFreq...Z ), 
          X345.fBodyAccJerk.mean...X =mean( X345.fBodyAccJerk.mean...X ), 
          X346.fBodyAccJerk.mean...Y =mean( X346.fBodyAccJerk.mean...Y ), 
          X347.fBodyAccJerk.mean...Z =mean( X347.fBodyAccJerk.mean...Z ), 
          X348.fBodyAccJerk.std...X =mean( X348.fBodyAccJerk.std...X ), 
          X349.fBodyAccJerk.std...Y =mean( X349.fBodyAccJerk.std...Y ), 
          X350.fBodyAccJerk.std...Z =mean( X350.fBodyAccJerk.std...Z ), 
          X373.fBodyAccJerk.meanFreq...X =mean( X373.fBodyAccJerk.meanFreq...X ), 
          X374.fBodyAccJerk.meanFreq...Y =mean( X374.fBodyAccJerk.meanFreq...Y ), 
          X375.fBodyAccJerk.meanFreq...Z =mean( X375.fBodyAccJerk.meanFreq...Z ), 
          X424.fBodyGyro.mean...X =mean( X424.fBodyGyro.mean...X ), 
          X425.fBodyGyro.mean...Y =mean( X425.fBodyGyro.mean...Y ), 
          X426.fBodyGyro.mean...Z =mean( X426.fBodyGyro.mean...Z ), 
          X427.fBodyGyro.std...X =mean( X427.fBodyGyro.std...X ), 
          X428.fBodyGyro.std...Y =mean( X428.fBodyGyro.std...Y ), 
          X429.fBodyGyro.std...Z =mean( X429.fBodyGyro.std...Z ), 
          X452.fBodyGyro.meanFreq...X =mean( X452.fBodyGyro.meanFreq...X ), 
          X453.fBodyGyro.meanFreq...Y =mean( X453.fBodyGyro.meanFreq...Y ), 
          X454.fBodyGyro.meanFreq...Z =mean( X454.fBodyGyro.meanFreq...Z ), 
          X503.fBodyAccMag.mean.. =mean( X503.fBodyAccMag.mean.. ), 
          X504.fBodyAccMag.std.. =mean( X504.fBodyAccMag.std.. ), 
          X513.fBodyAccMag.meanFreq.. =mean( X513.fBodyAccMag.meanFreq.. ), 
          X516.fBodyBodyAccJerkMag.mean.. =mean( X516.fBodyBodyAccJerkMag.mean.. ), 
          X517.fBodyBodyAccJerkMag.std.. =mean( X517.fBodyBodyAccJerkMag.std.. ), 
          X526.fBodyBodyAccJerkMag.meanFreq.. =mean( X526.fBodyBodyAccJerkMag.meanFreq.. ), 
          X529.fBodyBodyGyroMag.mean.. =mean( X529.fBodyBodyGyroMag.mean.. ), 
          X530.fBodyBodyGyroMag.std.. =mean( X530.fBodyBodyGyroMag.std.. ), 
          X539.fBodyBodyGyroMag.meanFreq.. =mean( X539.fBodyBodyGyroMag.meanFreq.. ), 
          X542.fBodyBodyGyroJerkMag.mean.. =mean( X542.fBodyBodyGyroJerkMag.mean.. ), 
          X543.fBodyBodyGyroJerkMag.std.. =mean( X543.fBodyBodyGyroJerkMag.std.. ), 
          X552.fBodyBodyGyroJerkMag.meanFreq.. =mean( X552.fBodyBodyGyroJerkMag.meanFreq.. ), 
          X555.angle.tBodyAccMean.gravity. =mean( X555.angle.tBodyAccMean.gravity. ), 
          X556.angle.tBodyAccJerkMean..gravityMean. =mean( X556.angle.tBodyAccJerkMean..gravityMean. ), 
          X557.angle.tBodyGyroMean.gravityMean. =mean( X557.angle.tBodyGyroMean.gravityMean. ), 
          X558.angle.tBodyGyroJerkMean.gravityMean. =mean( X558.angle.tBodyGyroJerkMean.gravityMean. ), 
          X559.angle.X.gravityMean. =mean( X559.angle.X.gravityMean. ), 
          X560.angle.Y.gravityMean. =mean( X560.angle.Y.gravityMean. ), 
          X561.angle.Z.gravityMean. =mean( X561.angle.Z.gravityMean. ))


#put results to file
                                             )
write.table(DS_AVG_BY_ACtivity_Subject,"AVG_BY_Activity_Subject.txt", row.name=FALSE)

