run_analysis <- function() {
	library(dplyr)
	library(reshape2)
	
	#read list of features/variables
	features <- read.table("features.txt")
	#read list of activities
	activity <- read.table("activity_labels.txt")
	
	
	#read training data
	x_train <- read.table("train/X_train.txt")
	y_train <- read.table("train/y_train.txt")
	subject_train <- read.table("train/subject_train.txt")
	
	#store all training data (activity and subject and measurement) in one data frame
	train_data <- cbind(subject_train,y_train,x_train)
	
	#read test data
	x_test <- read.table("test/X_test.txt")
	y_test <- read.table("test/y_test.txt")
	subject_test <- read.table("test/subject_test.txt")
	
	#store all test data (activity and subject and measurement) in one data frame
	test_data <- cbind(subject_test,y_test,x_test)
	
	#merge test and training data
	merge_data <- rbind(train_data,test_data)
	
	#getting the col indexes and names for mean and std measurements only
	feature_cols <- features[grepl("mean\\(", features$V2)| grepl("std\\(", features$V2) ,]$V1
	feature_names <- features[grepl("mean\\(", features$V2)| grepl("std\\(", features$V2) ,]$V2
	
	#getting the wanted cols indexes and names from the merge data
	merge_cols <- c(1:2, feature_cols+2) 
	final_names <- c("subject", "activity", as.character(feature_names))
	
	#extracting only wanted data
	extract_data <- merge_data[merge_cols]
	#naming columns
	names(extract_data) <- final_names
	
	#using activity full name as factor for activiy column
	extract_data$activity <-factor(extract_data$activity, levels = activity$V1, labels = activity$V2)
	
	
	#All following steps are used to create the final_table
	# final table will have 4 columns : subject, activity, variable (feature) and average
	# one row will give the average value of "variable" for subject and activity
	melted <- melt(extract_data, id.vars=c("subject", "activity"))
	grouped <- group_by(melted, subject, activity, variable)
	final_data <- summarize(grouped, average = mean(value))

	#writing final table in text file name "res_tidy_table.txt"
	write.table(final_data, file="res_tidy_table.txt", row.names=FALSE)
	
	#return the final table 
	final_data
	
}
