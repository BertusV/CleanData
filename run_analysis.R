run_analysis <- function() {
    ## load the packages required
    library(dplyr)
    ## Download the data and unzip is in the current directory
    tempzip <- tempfile()
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",tempzip)
    unzip(tempzip,exdir=".")

    ## Read training data
    temp_columnNames <- read.table("./UCI HAR Dataset/features.txt")
    columnNames <- temp_columnNames$V2
    temp_train <- read.table("./UCI HAR Dataset/train/X_train.txt",col.names=columnNames)
    temp_train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt",col.names=c("Subject"))
    temp_train_activity <- read.table("./UCI HAR Dataset/train/y_train.txt",col.names=c("Activity"))
    train_data <- cbind(temp_train_subject, temp_train_activity)
    train_data <- cbind(train_data,temp_train)
    
    ## Read test data
    temp_test <- read.table("./UCI HAR Dataset/test/X_test.txt",col.names=columnNames)
    temp_test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt",col.names=c("Subject"))
    temp_test_activity <- read.table("./UCI HAR Dataset/test/y_test.txt",col.names=c("Activity"))
    test_data <- cbind(temp_test_subject, temp_test_activity)
    test_data <- cbind(test_data,temp_test)

    ## Merge the test and training data
    temp_data <- rbind(train_data, test_data)
    
    colNames <- colnames(temp_data)

    ## Get all the mean and std column names
    incColumnNames <- (colNames[((grepl("mean",colNames) | grepl("std",colNames)
                                 | grepl("Subject",colNames)| grepl("Activity",colNames)) & !grepl("meanFreq",colNames)) == TRUE])
    temp_data <- temp_data[, ((grepl("mean",colNames) | grepl("std",colNames)
                  | grepl("Subject",colNames)| grepl("Activity",colNames)) & !grepl("meanFreq",colNames))]

    ## Group and summerize the data
    summaryData <- temp_data %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))
    write.table(summaryData, file = "Result.txt", row.names = FALSE)

}
