run_analysis <- function() {
  ####### Get Data from the Files #######
  
  #variable (column) headers for the actual data
  features <- read.table("features.txt")
  #labels for y_test.txt or y_train.txt; maps a number to a string
  activity_labels <- read.table("activity_labels.txt")
  
  #.txt file holding numbers that represent certain participants
  subject_test <- read.table("./test/subject_test.txt")
  #the actual test data
  X_test <- read.table("./test/X_test.txt")
  #contains numbers corresponding to activities mapped in activity_labels.txt
  y_test <- read.table("./test/y_test.txt")
  
  #.txt file holding numbers that represent certain participants
  subject_train <- read.table("./train/subject_train.txt")
  #the actual train data
  X_train <- read.table("./train/X_train.txt")
  #contains numbers corresponding to activities mapped in activity_labels.txt
  y_train <- read.table("./train/y_train.txt")
  
  ####### Fix Up and Prep the Data #######
  
  #fix up test data
  total_test <- X_test
  #use the second column for the actual names; first column is just an index
  colnames(total_test) <- features$V2
  #translate y_train numbers to corresponding names
  y_test <- as.data.frame(sapply(y_test, function(x) gsub("1", activity_labels[1,2], x)))
  y_test <- as.data.frame(sapply(y_test, function(x) gsub("2", activity_labels[2,2], x)))
  y_test <- as.data.frame(sapply(y_test, function(x) gsub("3", activity_labels[3,2], x)))
  y_test <- as.data.frame(sapply(y_test, function(x) gsub("4", activity_labels[4,2], x)))
  y_test <- as.data.frame(sapply(y_test, function(x) gsub("5", activity_labels[5,2], x)))
  y_test <- as.data.frame(sapply(y_test, function(x) gsub("6", activity_labels[6,2], x)))
  #add this fixed y_test variable as a column which indicates which activity for the row
  total_test$ActivityType <- y_test
  #convert to character so that rbind will work later on
  total_test$ActivityType <- sapply(total_test$ActivityType, function(x) as.character(x))
  #add the subject numbers as a column
  total_test$SubjectNum <- subject_test
  total_test$SubjectNum <- sapply(total_test$SubjectNum, function(x) as.character(x))
  
  #fix up the train data
  total_train <- X_train
  colnames(total_train) <- features$V2
  y_train <- as.data.frame(sapply(y_train, function(x) gsub("1", activity_labels[1,2], x)))
  y_train <- as.data.frame(sapply(y_train, function(x) gsub("2", activity_labels[2,2], x)))
  y_train <- as.data.frame(sapply(y_train, function(x) gsub("3", activity_labels[3,2], x)))
  y_train <- as.data.frame(sapply(y_train, function(x) gsub("4", activity_labels[4,2], x)))
  y_train <- as.data.frame(sapply(y_train, function(x) gsub("5", activity_labels[5,2], x)))
  y_train <- as.data.frame(sapply(y_train, function(x) gsub("6", activity_labels[6,2], x)))
  total_train$ActivityType <- y_train
  total_train$ActivityType <- sapply(total_train$ActivityType, function(x) as.character(x))
  total_train$SubjectNum <- subject_train
  total_train$SubjectNum <- sapply(total_train$SubjectNum, function(x) as.character(x))
  
  #append the test and train dataframes together
  master_df <- rbind(total_test, total_train)
  
  #find only the columns for mean or standard deviation
  names <- grep("mean()|std()|ActivityType|SubjectNum", colnames(master_df))
  exclude <- grep("Freq", colnames(master_df))
  clean <- names[!names %in% exclude]
  
  #clean up the master_df so it only has mean and std columns
  master_df <- master_df[, clean]
  
  #create the final tidy dataset
  final <- aggregate(copy[, 1:66], list(SubjectID = copy$SubjectNum, ActivityType = copy$ActivityType), mean)
  
  #write the file to current working directory
  write.table(final, file = "clean_data_assignment_output.txt", row.name=FALSE)
  
  print("Done running program.")
}