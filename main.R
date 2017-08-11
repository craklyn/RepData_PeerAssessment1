setwd("~/RepData_PeerAssessment1")

# Load the data
unzip('activity.zip')
theData <- read.csv('activity.csv')

# Process/transform data to simplify downstream analysis
theData$date <- as.Date(theData$date)

# Q1: What is mean total number of steps taken per day?  Ignore missing values in daataset.
library(dplyr)
totalStepsSummary <- tbl_df(theData) %>% 
  group_by(date) %>%
  summarize(totalSteps = sum(steps, na.rm=TRUE))
# Make a histogram of the total number of steps taken each day
hist(totalStepsSummary$totalSteps)
# Calculate and report the mean and median total number of steps taken per day
summary(totalStepsSummary$totalSteps)

# Q2: What is the average daily activity pattern?
totalStepsSummary <- tbl_df(theData) %>% 
  group_by(interval) %>%
  summarize(totalSteps = mean(steps, na.rm=TRUE))
# Make a time series plot (i.e. type='l') of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
plot(totalStepsSummary, type='l')
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
totalStepsSummary$interval[which.max(totalStepsSummary$totalSteps)]

# Q3: Imputing missing values
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
length(which(is.na(theData$steps)))

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#--> Strategy: Find average value by interval.  Impute that value in missing entries.
avgIntervalSteps <- tbl_df(theData) %>%
  group_by(interval) %>%
  summarize(avgSteps = mean(steps, na.rm=TRUE))

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
imputedData <- theData
for(i in 1:nrow(imputedData)) {
  if(is.na(imputedData$steps[i])) {
    imputedData$steps[i] <- avgIntervalSteps$avgSteps[avgIntervalSteps$interval == imputedData$interval[i]]
  }
}

# Make a histogram of the total number of steps taken each day.
imputedTotalStepsSummary <- imputedData %>%
  group_by(date) %>%
  summarize(totalSteps = sum(steps))
hist(imputedTotalStepsSummary$totalSteps)
# Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
summary(imputedTotalStepsSummary$totalSteps)
cat("Yes, they differ.  Median rises by about 400 steps, mean by about 1,400")
# What is the impact of imputing missing data on the estimates of the total daily number of steps?
cat("The impact is that we are now assigning steps to days/intervals that previously were being treated as 0.  The imputed data either has the number of steps in each interval remain the same, or if the data is missing we give it the average value for that interval.  Since each interval's step count remains the same or increases, the total number of steps in the day remains the same or increases.")



# Are there differences in activity patterns between weekdays and weekends?

# Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
imputedDataWithWeekends <- imputedData %>%
  mutate(dow = weekdays(date)) %>%
  mutate(dayType = factor(dow %in% c("Saturday", "Sunday"), 
                          labels=c("weekday", "weekend"))) %>%
  group_by(dayType, interval) %>%
  summarize(avgSteps = mean(steps))

library(lattice)
xyplot(avgSteps ~  interval| dayType, data = imputedDataWithWeekends, type = "l")



