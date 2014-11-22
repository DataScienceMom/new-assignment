## step1: Merges the training and the test sets to create one data set  ##

features <- read.table(file='features.txt',row.names=1)

# for training data ##

training_set<-read.table(file='X_train.txt')
training_lab<-read.table(file='y_train.txt')
names(training_set) <- t(features)
names(training_lab) <- "activity"

training_data<-cbind(training_lab, training_set)

# for testing data ##

testing_set<-read.table(file='X_test.txt')
testing_lab<-read.table(file='y_test.txt')
names(testing_set) <- t(features)
names(testing_lab) <- "activity"

testing_data<-cbind(testing_lab, testing_set)

# merging training and testing data 

data<-rbind(training_data,testing_data)
rm(list=setdiff(ls(), "data"))

## step 2: Extracts only the measurements on the mean and standard deviation for each measurement ##

features=names(data)

index1=grep('mean()',features)
index2=grep('std()',features)

data1 <- data[,c(1,index1,index2)]
rm(features, index1,index2,data)

## step 3: Uses descriptive activity names to name the activities in the data set ##

labs<-read.table(file='activity_labels.txt',stringsAsFactors=FALSE)

newlab<-data1$activity

for (i in 1:dim(labs)[1]){
        newlab[which(newlab==labs[i,1])]=labs[i,2]
}

data1$activity<-newlab

rm(i,labs,newlab)

## setp 4: Appropriately labels the data set with descriptive variable names. ##

name1<-names(data1)[-1]
list1<-strsplit(name1,'-')  # list1 is a list
list2<-vector()

for (i in 1:length(list1)){
        list2[i]<- paste(gsub('[()]','',list1[[i]][2]), ' of ', list1[[i]][1], 
                         ' from signal ', list1[[i]][3], sep='')
}

colnames(data1) <- c('activity', list2)

rm(list1,list2,name1,i)

## step 5: From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.  ##

library(reshape2)
mtdata <- melt (data1,id='activity', measure.vars=names(data1)[-1]) 

tidy_data <- dcast(mtdata, activity~variable, mean)

write.table(tidy_data, file="tidy data.txt", row.name=FALSE, sep='\t') 

