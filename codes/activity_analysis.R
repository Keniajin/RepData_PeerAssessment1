#set the working directory 
#setwd("~\rr_assignment_week1")
#load libraries
library(ggplot2)
library(lattice)
library(data.table)
library(plyr)
#load the data set
activity <- read.csv("rr_assignment_week1/data/activity.csv" ,header = TRUE, na.string = "NA")

#steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
#date: The date on which the measurement was taken in YYYY-MM-DD format
#interval: Identifier for the 5-minute interval in which measurement was taken

#generate a new variable to represent date
activity$ndate <- as.Date(activity$date, "%Y-%m-%d")
activity$day <- as.factor(format(activity$ndate, "%d"))
activity$month <- as.factor(format(activity$ndate, "%m"))
#view the interval
table(activity$interval)
#observed each 5th minute has 61 observations

#solutions for each question
#What is mean total number of steps taken per day?
sumData <- aggregate(steps ~ ndate, activity, sum)  
#countSteps<-ddply(activity,.(ndate),summarize,tot=sum(steps,na.rm = T))
# qplot(sumData$ndate, sumData$steps, geom="bar", stat="identity", xlab = "Date" , ylab = "Steps Count")
png("rr_assignment_week1/figures/steps_per_day.png", width = 800, height = 600)
ggplot(sumData, aes(x=steps)) + geom_histogram(binwidth = 1000)
dev.off()
#mean of steps
stepsMean<-as.integer(mean(sumData$steps,na.rm = T))
#median steps
stepsMedian<-as.integer(median(sumData$steps))


#What is the average daily activity pattern?
#Make a time series plot (i.e. type = "l") of the 5-minute interval 
#(x-axis) and the average number of steps taken, averaged across all days (y-axis)
stepsInterval<-aggregate(steps~interval,activity, FUN=mean)
png("rr_assignment_week1/figures/Avg_daily_activity_pattern.png", width = 800, height = 600)
plot(stepsInterval$interval,stepsInterval$steps,type='l',
     main='Average daily activity pattern',xlab='Interval in 5 min',
     ylab='Average number of steps',bty="n")
dev.off()
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 
intervalMax<-stepsInterval$interval[stepsInterval$steps==max(stepsInterval$steps)]


#Imputing missing values
#Note that there are a number of days/intervals where there are missing values (coded as NA). 
#The presence of missing days may introduce bias into some calculations or summaries of the data

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
numberNA2<-length(activity[!complete.cases(activity),1])

#Strategy to replace missing values in the dataset 
#For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
activityImputed <- activity
#imputing the values with mean since the data is not normally distibuted
activityImputed$steps[is.na(activityImputed$steps)] <- mean(activityImputed$steps, na.rm = TRUE)
sumImputedData <- aggregate(steps ~ ndate, activityImputed, sum)  
png("rr_assignment_week1/figures/stepsImputed_per_day.png", width = 800, height = 600)
ggplot(sumImputedData, aes(x=steps)) + geom_histogram(binwidth = 1000)
dev.off()
#imputed mean and median
stepsImputedMean<-as.integer(mean(sumImputedData$steps))
stepsImputtedMedian<-as.integer(median(sumImputedData$steps))

#What is the impact of imputing missing data on the estimates of the total daily number of steps?
#the imputation made a slight difference in the mean but the median remained the same

#Are there differences in activity patterns between weekdays and weekends?
activityImputed$dayOfWeek <-weekdays(activityImputed$ndate)
activityImputed$timeOfWeek <- "weekDay"
activityImputed$timeOfWeek[activityImputed$dayOfWeek=="Sunday" | activityImputed$dayOfWeek=="Saturday"  ] <- "weekEnd"
table(activityImputed$timeOfWeek)

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and
#the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
stepsImputedInterval<-aggregate(steps~interval + timeOfWeek ,activityImputed, FUN=mean)
png("rr_assignment_week1/figures/steps_weekTime.png", width = 800, height = 600)
xyplot(steps~interval|timeOfWeek,stepsImputedInterval,type="l",layout=c(1,2), ylab = "Number Steps", xlab="Interval")
dev.off()

