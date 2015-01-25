do_download <- function()
{
   url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
   destfile <- "getdata_projectfiles_UCI HAR Dataset.zip"

   if (!file.exists(destfile) || (file.info(destfile))["size"] == 0) {
      download.file(url, destfile = destfile)
   }
   unzip(destfile)
}

load_test <- function(activity.labels, features)
{
  test_data_file <- "UCI HAR Dataset/test/X_test.txt"
  subject_test_data_file <- "UCI HAR Dataset/test/subject_test.txt"
  activity_data_file <- "UCI HAR Dataset/test/y_test.txt"

  test <- read.table(test_data_file)
  subject.test <- read.table(subject_test_data_file)
  activity <- read.table(activity_data_file, colClasses=c("numeric"))
  
  test <- cbind(test, subject.test, factor(activity$V1, labels = activity.labels))
  return(test)
}

load_train <- function(activity.labels, features)
{
  train_data_file <- "UCI HAR Dataset/train/X_train.txt"
  subject_test_data_file <- "UCI HAR Dataset/train/subject_train.txt"
  activity_data_file <- "UCI HAR Dataset/train/y_train.txt"
  
  train.data <- read.table(train_data_file)
  subject.test <- read.table(subject_test_data_file)
  activity <- read.table(activity_data_file, colClasses=c("numeric"))
  
  train <- cbind(train.data, subject.test, factor(activity$V1, labels = activity.labels))
  return(train)
}

load_features <- function()
{
  features_data_file <- "UCI HAR Dataset/features.txt"
  features <- read.table(features_data_file, colClasses = c("NULL", "character"))
  return(features$V2)
}

load_activity_labels <- function()
{
  activity_labels_data_file <- "UCI HAR Dataset/activity_labels.txt"
  activity_labels <- read.table(activity_labels_data_file, colClasses = c("NULL", "factor"))
  return(activity_labels$V2)
}


merge_training_and_test_sets <- function(test, train)
{
  merged.data <- rbind(train, test)
  return(merged.data)
}

label_data <- function(data, features)
{
  names(data) <- c(features, "Subject", "Activity")
  return(data)
}

extract_mean_and_sd <- function(data)
{
  return(data[,grepl('Subject|Activity|.*-(mean|std)\\(\\)-.*', names(data), perl=T)])
}

tidy_data <- function(data)
{
  grouped.data <- group_by(data, Subject, Activity)
  summarised.data <- summarise(grouped.data, mean=mean())
}
  
run_analysis <- function()
{
  library(dlyr)
  do_download()
  activity.labels <- load_activity_labels()
  features <- load_features()
  test <- load_test(activity.labels, features)
  train <- load_train(activity.labels, features)

  merged.data <- merge_training_and_test_sets(test, train)
  labeled.data <- label_data(merged.data, features)
  mean.and.sd <- extract_mean_and_sd(labeled.data)
  tidy.data <- tidy_up_data(labeled.data)
  return(tidy.data)
}