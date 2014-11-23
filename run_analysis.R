#561 * 2 matrix, first column is index, the second column stores the feature name
features <- read.table("features.txt", stringsAsFactors=FALSE);
# vector stores all subject identifiers
subject_test <- read.table("test/subject_test.txt", stringsAsFactors=FALSE);
# vector stores all activity identifiers
y_test <- read.table("test/y_test.txt", stringsAsFactors=FALSE);
# matrix stores all test data
x_test <- read.table("test/X_test.txt", stringsAsFactors=FALSE);
# data frame for test data
test_data <- data.frame(subject_test, y_test, x_test);
names(test_data) <- c("Subject", "Activity", features[,2]);
#vector stores activity name, the indexes match with the data frame
activity <- read.table("activity_labels.txt", stringsAsFactors=FALSE);
test_data$Activity <- sapply(test_data$Activity, function(x) activity[x,2]);

#do the previous routine on training data
subject_train <- read.table("train/subject_train.txt", stringsAsFactors=FALSE);
y_train <- read.table("train/y_train.txt", stringsAsFactors=FALSE);
x_train <- read.table("train/X_train.txt", stringsAsFactors=FALSE);
train_data <- data.frame(subject_train, y_train, x_train);
names(train_data) <- c("Subject", "Activity", features[,2]);
train_data$Activity <- sapply(train_data$Activity, function(x) activity[x,2]);

#Here's the merge part for the step 1
test_data$Type = "Test";
train_data$Type = "Train";
total_data <- rbind(test_data, train_data);

#Step2: extract mean and std for each measurement
cols <- grep("mean|std", names(total_data), ignore.case=TRUE);
total_data_clean <- data.frame(total_data$Subject, total_data$Activity, total_data[, cols]);

#step3: the activity names have been changed at lines 13-14 and 22
#step4: the variable names have been assigned at lines 11 and 21
#step5: independent tidy data set with the average of each variable for each activity and each subject.
#create a matrix of combinations of subjects and activities
sub_activity <- NULL;
for (i in 1:30) {
	for (j in activity[, 2]) {
		sub_activity <- rbind(sub_activity, c(i, j));
	}
}
#loop through the dataset to calculate mean for each subject and activity combinations
tidy_data <- NULL;
for ( i in 1 : dim(sub_activity)[1]) {
	#row vector, first element is  subject, second is activity
	v <- sub_activity[i,];
	#vector of means for each measurement for that subject and  activity combination
	means <- NULL;
	#looping through all measurements
	for (j in 3 : dim(total_data_clean)[2]) {
		#row indexes for each subject and activity combination
		rows <- total_data_clean[,1] == as.numeric(v[1]) & total_data_clean[,2] == v[2];
		means <- c(means, mean(total_data_clean[rows, j]));
	}
	tidy_data <- rbind(tidy_data, c(v, means));
}
#assign header for the tidy dataset
colnames(tidy_data) <- names(total_data_clean);

#write out the tidy dataset into a file
write.table(tidy_data, "tidy_data.txt", row.name=FALSE);

