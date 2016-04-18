What the script does?
run_analysis.R is a script that reads the data, formats the data and returns a final table 
where the average of each feature (mean and std only) for each subject and activity is store

Assumptions:
As advised in the instructions, we assume that all data are in the working directory.
For clarity we kept the test and train sub directories with their data.
Therefore the files features.txt and activity_labels.txt are the working directory
the directories train and test are in the working directory

Steps:
I am using the library dplyr and reshape2

read the file features.txt
read the file activity_labels

read the training data.
I decided to add the columns activity and subject before merging to prevent mixing test and train data

read the test data in the same way

I wasn't sure what features to select for the final table. I decided to go with all features which names
contained 'mean(' or 'std('. I found the names of the features clear enough and didn't change them!

As requested in the instructions, the activity column is set as factor using the activity names

Using the melt and group_by, I created the final table (for step 5) which has 4 columns subject, activity, variable (feature) and average
I like having only one feature per row : one row gives the average value of "variable" for subject and activity