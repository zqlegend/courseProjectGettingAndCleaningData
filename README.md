Here, in this repository, contains the data for the course project of "Getting And Cleaning Data" course on Coursera. Complete raw data set can be found in downloaded `"UCI HAR Dataset.zip"` file and corresponding unzipped directory.

`"run_analysis.R"` script has been used to approach the required tidy data set contained in `"tidyData.txt"` from the raw data. It has followed the procedure as described below:

1. Download the zip file and unzip it;

2. Read the test and traning data sets;

3. Merge them into one data set;

4. Extracts only the measurements on the mean and standard deviation for each measurement;

5. Uses descriptive activity names to name the activities in the data set;

6. Appropriately labels the data set with descriptive variable names;

7. Creates a independent tidy data set with the average of each variable for each activity and each subject;

8. Write out the tidy data.
