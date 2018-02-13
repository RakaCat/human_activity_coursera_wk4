---
title: "CodeBook"
output: 
  html_document: 
    keep_md: yes
---

## Codebook

Content:  
1.  Scripts  
2.  Variable descriptions and units  


### Scripts

Download the data

```r
#Download
path <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
download.file(path, 'f.zip'); unzip('f.zip')
```

```r
library(rex)
library(reshape2)
```

Load the data and add colnames

```r
#Extract the name of the features
cnames <- as.character(read.csv('UCI HAR Dataset/features.txt', 
                                sep = '\n', header = FALSE)[,1])
cnames <- as.character(sapply(cnames, function(x) sub('\\d+\\s', '', x)))
head(cnames)
```

```
## [1] "tBodyAcc-mean()-X" "tBodyAcc-mean()-Y" "tBodyAcc-mean()-Z"
## [4] "tBodyAcc-std()-X"  "tBodyAcc-std()-Y"  "tBodyAcc-std()-Z"
```

```r
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
```

Merges the training and the test sets to create one data set.

```r
all <- rbind(cbind(sj_train, y_train, x_train), cbind(sj_test, y_test, x_test))
#remove df of no use
remove(sj_train, y_train, x_train, sj_test, y_test, x_test)
```

Change subject&Action to factor.


```r
str(all$Subject)
```

```
##  int [1:10299] 1 1 1 1 1 1 1 1 1 1 ...
```

```r
all$Subject <- as.factor(all$Subject)
all$Action <- as.factor(all$Action)
str(all$Subject) 
```

```
##  Factor w/ 30 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
```

Also, uses descriptive activity names to name the activities in the data set.

```r
alab <- read.csv('UCI HAR Dataset/activity_labels.txt', sep = '', header = FALSE)
levels(all$Action) <- tolower(alab[,2])
```

Extracts only the measurements on the mean and standard deviation for each measurement. 
Some variables have the name 'mean' in it. Thus the regular expression has to include "." before and after "mean|std"

```r
mean_std <- grep('\\.(mean|std)\\..', colnames(all))
all_mean_std <- all[c(1,2,mean_std)]
colnames(all_mean_std)[1:10]
```

```
##  [1] "Subject"              "Action"               "tBodyAcc.mean...X"   
##  [4] "tBodyAcc.mean...Y"    "tBodyAcc.mean...Z"    "tBodyAcc.std...X"    
##  [7] "tBodyAcc.std...Y"     "tBodyAcc.std...Z"     "tGravityAcc.mean...X"
## [10] "tGravityAcc.mean...Y"
```

From all_mean_std, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

```r
mean_col <- grep('\\.mean\\.', colnames(all_mean_std))

tidyAverage <- all_mean_std[c(1,2,mean_col)] %>% melt(id.vars = c(1,2)) %>% 
  dcast(Subject + Action ~ variable, mean) %>% melt
```

```
## Using Subject, Action as id variables
```

```r
head(tidyAverage)
```

```
##   Subject             Action          variable     value
## 1       1            walking tBodyAcc.mean...X 0.2773308
## 2       1   walking_upstairs tBodyAcc.mean...X 0.2554617
## 3       1 walking_downstairs tBodyAcc.mean...X 0.2891883
## 4       1            sitting tBodyAcc.mean...X 0.2612376
## 5       1           standing tBodyAcc.mean...X 0.2789176
## 6       1             laying tBodyAcc.mean...X 0.2215982
```


### Variable description

####Feature Selection

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

Features are normalized and bounded within [-1,1].

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

- tBodyAcc-XYZ
- tGravityAcc-XYZ
- tBodyAccJerk-XYZ
- tBodyGyro-XYZ
- tBodyGyroJerk-XYZ
- tBodyAccMag
- tGravityAccMag
- tBodyAccJerkMag
- tBodyGyroMag
- tBodyGyroJerkMag
- fBodyAcc-XYZ
- fBodyAccJerk-XYZ
- fBodyGyro-XYZ
- fBodyAccMag
- fBodyAccJerkMag
- fBodyGyroMag
- fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

- mean(): Mean value
- std(): Standard deviation
- mad(): Median absolute deviation 
- max(): Largest value in array
- min(): Smallest value in array
- sma(): Signal magnitude area
- energy(): Energy measure. Sum of the squares divided by the number of values. 
- iqr(): Interquartile range 
- entropy(): Signal entropy
- arCoeff(): Autorregresion coefficients with Burg order equal to 4
- correlation(): correlation coefficient between two signals
- maxInds(): index of the frequency component with largest magnitude
- meanFreq(): Weighted average of the frequency components to obtain a mean frequency
- skewness(): skewness of the frequency domain signal 
- kurtosis(): kurtosis of the frequency domain signal 
- bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
- angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

- gravityMean
- tBodyAccMean
- tBodyAccJerkMean
- tBodyGyroMean
- tBodyGyroJerkMean

####Units
The units for acceleration (Acc) data is *g*   
The units for gyroscope data is *radian/second*
