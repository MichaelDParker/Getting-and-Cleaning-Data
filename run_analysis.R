# Merges the training and the test sets to create one data set.
        # The path to the data on my PC is : C:\Users\mparker1\Documents\BusinessAnalyticsTeam\Coursera\R Files\GetandCleanData\UCI HAR Dataset
        # starting from "C:/Users/mparker1/Documents"
        setwd("C:/Users/mparker1/Documents/BusinessAnalyticsTeam/Coursera/R Files/GetandCleanData/UCI HAR Dataset")
   #read files
   xtest <- read.table("test/X_test.txt")
   xtrain<- read.table("train/X_train.txt")
   ytest <- read.table("test/Y_test.txt")
   ytrain<- read.table("train/Y_train.txt")
   subjecttest<-read.table("test/subject_test.txt")
   subjecttrain<-read.table("train/subject_train.txt")
   
   #see if the columns match before merging
   all.equal(names(xtrain), names(xtest))
   all.equal(names(ytrain), names(ytest))
   all.equal(names(subjecttrain), names(subjecttest))
   
   #merge the files and add an index to the merged files
   new_x_data<- rbind(xtrain, xtest)
   new_x_data$ID<- seq.int(nrow(new_x_data))
   new_y_data<- rbind(ytrain, ytest)
   new_y_data$ID<- seq.int(nrow(new_y_data))
   new_subject_data<- rbind(subjecttrain, subjecttest)
   new_subject_data$ID<- seq.int(nrow(new_subject_data))
   
# Uses descriptive activity names to name the activities in the data set
   #put the feature name as a column heading
   #features file is the 561 column names
   features <- read.table("features.txt", stringsAsFactors = FALSE)
   features <- rbind(features,"ID")
   #copying the dataset and adding names to columns
   xdatanamed<- new_x_data
   names(xdatanamed)<- features$V2
   
   #add activities column from Y table to the x table
   activities<-read.table("activity_labels.txt")
   names(activities)<-c("activitycode","activityname")
   names(new_y_data)<-c("activitycode","ID")
   #add the activity name from Y file code to the x measurement file
   allydata<- merge(new_y_data,activities,sort=F)
   #head(allydata)
   
# Extracts only the measurements on the mean and standard deviation for each measurement.
   allnames<- names(xdatanamed) 
   cnms<- grep("mean|std|ID", allnames)
   s<-xdatanamed[,cnms]
   
   #head(s)
   #head(new_y_data)
   #Combine the x and y data
   x_y_combined<- merge(s, allydata, by.x="ID", by.y="ID")
   #combine the xy with subject
   xyscombined<- merge(x_y_combined,new_subject_data, by="ID")
   #rename subject
   names(xyscombined)[83]<- "subjectid"
   names(xyscombined)
   
 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    r1<-aggregate(.~activityname+subjectid,data=xyscombined ,FUN=mean)
    # save a data frame to local drive
    write.table(r1, file = "summary_data_file.txt", row.names = FALSE)
   
    print(r1)
    write.table(names(r1), file="codebook_data_list.txt", row.names=F)
    write.table(unique(r1$activityname),file="activity_names.txt", row.names=F)
   