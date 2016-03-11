# Getting-and-cleaning-data-course-project
Code book, R script, and README files for clean data project

### The contents of this Repo include:
  1. `README.md` file (this file) explanining the contents of this repo
  2. `Codebook.md` detailing the transformations performed to UCI HAR dataset to obtain a merged clean data for test and train data
  3. `run_analysis.R` file that downloads the UCI HAR wearable device data set and creates the text file:
      * `summaryData.txt` (a summary of mean measurments by subject and activity)


#### To run the `run_analysis.R` file do the following:
1. Open RStudio and install packages `dplyr` and `tidyr`
2. Make sure there is an internet connection
3. Execute `run_analysis.R` in RStudio

#### `run_analysis.R` will do the following:
1. Download the UCI HAR files in the current working directory of RSudio
2. Read the test and train data sets
3. Apply descriptive names for the activity and features of test and train data set
4. Remove unwanted variables from the test and train data sets keeping only the mean and std varaiables
5. Write `test_train_merged.csv`.
6. Summarise the mean and std by `subject_id` and `activity` by averaging the mean std observations
7. Write `summaryData.txt`
