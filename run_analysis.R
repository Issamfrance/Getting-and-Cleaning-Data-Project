#Step 1 : Merges the training and the test sets to create one data set.

require("data.table")

## Read datas into data frame
subjtest <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
subjtrain <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
Xtest <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
Ytest <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
Xtrain <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
Ytrain <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")

## Change Column name for Subject Files

names(subjtest) = "IDSubject"
names(subjtrain) = "IDSubject"

## Read Feature data into a data frame
Featnames <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")

##Change Column names for measurement files
names(Xtrain) = Featnames$V2
names(Xtest) = Featnames$V2


## Merge Data Files into one Dataset

train <- cbind(subjtrain, Xtrain, Ytrain)
test <- cbind(subjtest, Xtest, Ytest)
DS <- rbind(train, test)

##Step 2 : Extracts only the measurements on the mean and standard deviation for each measurement. 

## Looking for mean and standard Columns
MScols <- grepl("mean", names(DS)) | grepl("std", names(DS))

## Extract only mean and std Columns
Ext <- DS[, MScols]

##Step 3 : Uses descriptive activity names to name the activities in the data set

desact <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")[,2]

Ytrain[,2] <- desact[Ytrain[,1]]
Ytest [,2] <- desact[Ytest[,1]]

names(Ytrain) = c("Activity_ID", "Activity_Label")
names(Ytest) = c("Activity_ID", "Activity_Label")

## Step 4 : Appropriately labels the data set with descriptive variable names. 

Final_Train <- cbind(subjtrain, Xtrain, Ytrain)
Final_Test <- cbind(subjtest, Xtest, Ytest)
Final_DS <- rbind(Final_Train, Final_Test)

## Step 5 : From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

require("reshape2")

ID_labels = c("IDSubject", "Activity_ID", "Activity_Label")
Data_labels = setdiff(colnames(Final_DS), ID_labels)
melt_Data = melt(Final_DS, id = ID_labels, measure.vars = Data_labels)

tidy_Data = dcast(melt_Data, IDSubject + Activity_Label ~ variable, mean)

write.table (tidy_Data, file = "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/Tidy_Data.txt")

