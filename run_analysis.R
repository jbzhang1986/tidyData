rm(list=ls())

setwd('D:/Dropbox/Data Science JHU/Getting and Cleaning Data/UCI HAR Dataset');

features = read.table('./features.txt',header=FALSE); 
activity = read.table('./activity_labels.txt',header=FALSE); 
subject = read.table('./train/subject_train.txt',header=FALSE); 
xTrain  = read.table('./train/x_train.txt',header=FALSE); 
yTrain = read.table('./train/y_train.txt',header=FALSE); 

colnames(activity)  = c('activityId','activityType');
colnames(subject)  = "subjectId";
colnames(xTrain)  = features[,2]; 
colnames(yTrain)  = "activityId";

trainingData = cbind(yTrain,subject,xTrain);

subjectTest = read.table('./test/subject_test.txt',header=FALSE);
xTest = read.table('./test/x_test.txt',header=FALSE);
yTest = read.table('./test/y_test.txt',header=FALSE);


colnames(subjectTest) = "subjectId";
colnames(xTest) = features[,2]; 
colnames(yTest) = "activityId";

testData = cbind(yTest,subjectTest,xTest);

finalData = rbind(trainingData,testData);

colNames  = colnames(finalData); 

selected = (grepl("activity..",colNames) | grepl("subject..",colNames) | 
            grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & 
            !grepl("mean..-",colNames) | grepl("-std..",colNames) & 
            !grepl("-std()..-",colNames));

finalData = finalData[selected==TRUE];

finalData = merge(finalData,activity,by='activityId',all.x=TRUE);

colNames  = colnames(finalData); 

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

colnames(finalData) = colNames;

finalDataNoActivity = finalData[,names(finalData) != 'activityType'];

tidyData = aggregate(finalDataNoActivity[,names(finalDataNoActivity) 
          != c('activityId','subjectId')],by=list(activityId=finalDataNoActivity$activityId,
          subjectId = finalDataNoActivity$subjectId),mean);

tidyData = merge(tidyData,activity,by='activityId',all.x=TRUE);

write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');