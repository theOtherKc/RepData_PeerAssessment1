##Loading and preprocessing the data

Following code uncompresses the data file and stores it in a R dataset. 

filename <- unzip("activity.zip")
activity_ds <- read.csv(filename, stringsAsFactors = FALSE)
str(activity_ds)

##Dates are not formatted to a Date class.
##Following code converts the dates into proper format.

activity_ds$date <- as.Date(activity_ds$date)
str(activity_ds)

##Column 'Steps' has some missing values.
##Following code counts the number of missing values.

sum(is.na(activity_ds$steps))

##What is mean total number of steps taken per day?

##There are 2304 missing values.  These values will be excluded for this part of the assignment.
##Dataset 'activity_1' is created for this purpose.

activity_1<-activity_ds[which(!is.na(activity_ds$steps)),]

##Number of steps taken is measured in 5-minute intervals.
##To caluculate the total number of steps taken for each day, data will be aggregated by day.

steps_perday1<-tapply(activity_1$steps, activity_1$date, sum)

##steps_perday1 dataset contains total number of steps taken for each day in October and November (total of 53 days)

##Following code makes a histogram of the total number of steps taken each day.

hist(steps_perday1,10, main = "Total number of steps taken per day", xlab = "")

##plot of Total number of steps taken per day

##The mean total number of steps in one day is 1.0766 × 104.
##The median of the total steps is 10765.

mean(steps_perday1)
median(steps_perday1)

##What is the average daily activity pattern?

##To explore the data throughout the day, dataset needs to be aggregated by intervals. 
##A per interval array is created to do this and a time series plot will also be created.

##The x-axis labels are names of the intervals in the dataset.
##The coding of the interval names is as follows:
##1000 will be considered as 10:00, 1500 will be considered as 15:00. 
##x-axis is a 24-hour-day starting from 12 am to 11:59:59 pm.

daily_activity<-tapply(activity_1$steps, activity_1$interval, mean)
plot(y = daily_activity, x = names(daily_activity), type = "l", xlab = "5-Minute-Interval", 
    main = "Daily Activity Pattern", ylab = "Average no. of steps")

##plot of daily average activity

##Interval with maximum average number of steps throughout the day is 835 with   steps.

daily_activity[daily_activity==max(daily_activity)]

##Imputing missing values

##As mentioned before there are many os days/intervals where there are missing values (these are coded as NA). 
##Missing days can affect the summary data calculations.

sum(is.na(activity_ds$steps))
sum(is.na(activity_ds))

##The total number of missing values for the column 'steps' is the same as the total number missing in the whole dataset.
##This implies intervals and the dates do not have any missing values.

##Following code creates a new dataset with the missing values filled in.

activity_2<-activity_ds
activity_2[which(is.na(activity_2$steps)),1]<-
        daily_activity[as.character(activity_2[which(is.na(activity_2$steps)),3])]

##There are no missing values in the new dataset.

sum(is.na(activity_2))

##Following code creates a histogram using the new dataset.

steps_perday2<-tapply(activity_2$steps, activity_2$date, sum)
par(mfrow=c(1,2))
hist(steps_perday1,10, main = "Total number of steps taken per day", xlab = "Steps", ylim =c(0, 25))
abline(v = median(steps_perday1), col = 4, lwd = 4)
hist(steps_perday2,10, main = "Total number of steps taken per day(missing values replaced with mean of interval)", xlab = "Steps",ylim =c(0, 25))
abline(v = median(steps_perday2), col = 4, lwd = 4)

##plot of chunk histogram both
##Median and the mean of the filled in dataset:

mean(steps_perday2)
median(steps_perday2)

##Imputting missing data has a minimal impact.
##The median seems to be changing but by just over one step.

mean(steps_perday2)-mean(steps_perday1)
median(steps_perday2)-median(steps_perday1)


##Are there differences in activity patterns between weekdays and weekends?
##In the code below variables “week_day” and “week_end” denote if the day is weekday or weekend.

activity_2$wd<-weekdays(activity_2$date)
activity_2$fwd<- as.factor(c("week_end", "week_day"))
activity_2[activity_2$wd == "Sunday" | activity_2$wd == "Saturday" ,5]<- factor("week_end")
activity_2[!(activity_2$wd == "Sunday" | activity_2$wd == "Saturday"),5 ]<- factor("week_day")


##Following code creates two aggregated arrays for the total number of steps taken per 5 minute time interval for weekdays and weekends and creates a graph to compare if there are any differences.

activity_2_we <- subset(activity_2, fwd == "week_end") 
activity_2_wd <- subset(activity_2, fwd == "week_day") 
daily_activity_we<-tapply(activity_2_we$steps, activity_2_we$interval, mean)
daily_activity_wd<-tapply(activity_2_wd$steps, activity_2_wd$interval, mean)
par(mfrow=c(2,1), mar=c(2,4,1,1))
plot(y = daily_activity_wd, x = names(daily_activity_wd), type = "l", xlab = "5 Minute Interval",main = "Daily Activity Pattern for Weekdays", ylab = "Average no. of steps",ylim =c(0, 250))
plot(y = daily_activity_we, x = names(daily_activity_we), type = "l", xlab = "5 Minute Interval",main = "Daily Activity Pattern for Weekends", ylab = "Average no. of steps",ylim =c(0, 250))

