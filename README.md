---
title: "README"
output: 
  html_document: 
    keep_md: yes
---
## README

This repository contains the following files to clean human activity recognition data.

1. *tidyAverage.txt* is the tidy data containing the mean values of 33 features associated with 6 human activity measured on 30 subjects. 
2. *CodeBook.Rmd* outlines the source, transformation, and aggregation done to obtain the tidyAverage.txt above. It also explains the names of the variables.
3. *run_analysis.R* is the full scripts from downloading raw data to outputing tidyAverage.txt
4. *UCI HAR Dataset* is the downloaded raw data.
5. *README.md*

### Background of data
The data is generated by a wide range of motion sensors (see CodeBook for more detail) on subjects performing different activities. The data is then used to predict the activity one is performing.  

The original researchers that perform the above study performed normalization and initial processing of the data (see Source of data). Features are generated using the raw data through statistical methods including mean, median, standard deviation etc.The raw data was splitted into a training set and testing set.  

The two sets are combined, together with the subject and activity name, for practice. After extracting and labelling appropriate column names, the mean of each activity, subject and variables are extracted into 'long format' and exported as tidyAverage.txt.



### Source of data

Full description of where the data was obtained
<http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>

The work is a course project to *Getting and Cleaning Data* at *Coursera* <https://www.coursera.org/>

