library(rex)
library(reshape2)

#Download
path <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
download.file(path, 'f.zip'); unzip('f.zip')

#Appropriately labels the data set with descriptive variable names.
cnames <- as.character(read.csv('UCI HAR Dataset/features.txt', 
                                sep = '\n', header = FALSE)[,1])
cnames <- as.character(sapply(cnames, function(x) sub('\\d+\\s', '', x)))
x_test <- read.csv('UCI HAR Dataset/test/X_test.txt', sep = '', 
                  header = FALSE, strip.white = TRUE, col.names = cnames)
y_test <- read.csv('UCI HAR Dataset/test/Y_test.txt', sep = '\n', 
                   header = FALSE, strip.white = TRUE, col.names = 'Action')
sj_test <- read.csv('UCI HAR Dataset/test/subject_test.txt', sep = '\n', 
                     header = FALSE, strip.white = TRUE, col.names = 'Subject')
x_train <- read.csv('UCI HAR Dataset/train/X_train.txt', sep = '', 
                   header = FALSE, strip.white = TRUE, col.names = cnames)
y_train <- read.csv('UCI HAR Dataset/train/Y_train.txt', sep = '\n', 
                   header = FALSE, strip.white = TRUE, col.names = 'Action')
sj_train <- read.csv('UCI HAR Dataset/train/subject_train.txt', sep = '\n', 
                    header = FALSE, strip.white = TRUE,col.names = 'Subject')


#Merges the training and the test sets to create one data set.
all <- rbind(cbind(sj_train, y_train, x_train), cbind(sj_test, y_test, x_test))
remove(sj_train, y_train, x_train, sj_test, y_test, x_test)

#Uses descriptive activity names to name the activities in the data set
all$Subject <- as.factor(all$Subject)
all$Action <- as.factor(all$Action)
alab <- read.csv('UCI HAR Dataset/activity_labels.txt', sep = '', header = FALSE)
levels(all$Action) <- tolower(alab[,2])

#Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std <- grep('\\.(mean|std)\\..', colnames(all))
all_mean_std <- all[c(1,2,mean_std)]

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
mean_col <- grep('\\.mean\\.', colnames(all_mean_std))

tidyAverage <- all_mean_std[c(1,2,mean_col)] %>% melt(id.vars = c(1,2)) %>% 
  dcast(Subject + Action ~ variable, mean) %>% melt




