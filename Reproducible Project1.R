## Download and unzip the files
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, file.path(getwd(), "Activity.zip"))
unzip(zipfile = "Activity.zip")

activity <- data.table::fread("activity.csv")


## Load libraries to be used
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
# 1. Mean steps per day
## 1-1 Calculate the total number of steps taken per day

dailysteps <- group_by(activity,date) %>%
        summarise(total_steps = sum(steps))
dailysteps$day <- seq(1:nrow(dailysteps))

## 1-2 Barplot of daily steps

hist(dailysteps$total_steps,xlab = "Steps",main = "Daily steps")

barplot(dailysteps$total_steps,xlab = "Steps", main = "Daily steps")

## 1-3 Mean and Median of total number of steps per day
mean_steps <- mean(dailysteps$total_steps, na.rm = TRUE)
median_steps <- median(dailysteps$total_steps,na.rm = TRUE)
report <- print(c("Median Steps",median_steps,"Mean Steps",mean_steps))

# 2. Average daily activity pattern
## 2-1 time-series plot of the 5 minute interval
tsdf <- group_by(activity,interval) %>%
        summarise(ave_steps = mean(steps,na.rm = TRUE))

b <- ggplot(tsdf,aes(x=interval, y=ave_steps))
        b + geom_line() + labs(title = "Average steps per day",y = "Average Steps",x="Intervals")


## 2-2 Which 5-minute interval contains max number of steps                

tsdf[tsdf$ave_steps == max(tsdf$ave_steps),"interval"]


# 3. Inputting Missing Values

## 3-1 Number of missing values in the dataset

activity[is.na(steps),.N]

## 3-2 fill out the missing values with mean/median for that day

z <- activity
y <- inner_join(z,tsdf,by.x = z$interval,by.y = tsdf$interval)
NAdf <-y[is.na(y$steps),] 
NAdf$steps <- NAdf$ave_steps
NotNAdf <- y[!is.na(y$steps),]
clean <- rbind(NotNAdf,NAdf) %>%
        select(steps,date,interval)



## 3-3 New data frame with missing values filled in using mean steps per interval
write.csv(x=clean,file = "activityTIDY.csv")

## 3-4 Make a histogram total number of steps daily, calculate mean and median

clean_daily <- group_by(clean,date) %>%
        summarise(daily_steps = sum(steps))
hist(clean_daily$daily_steps,xlab = "Daily steps",main = "Histogram of Clean data")


mean_clean <- mean(clean_daily$daily_steps)
median_clean <- median(clean_daily$daily_steps)
print(c(mean_clean,median_clean))
# 4. Weekday patterns vs. Weekend patterns

## 4-1 Factor variable  with two levels - "weekday" and "weekend"
x <- clean
x$day <- wday(x$date)

weekend <- group_by(x,day) %>%
        subset(day==1| day == 7)
weekend$weekstatus <- "weekend"        
weekend <-select(weekend,steps,date,interval,weekstatus)

weekdays <- group_by(x,day) %>%
        subset(day==2| day == 3 | day == 4 | day == 5 | day == 6)
weekdays$weekstatus <- "weekday"
weekdays <- select(weekdays,steps,date,interval,weekstatus)


finaldf<-rbind(weekdays,weekend)


## 4-2 Panel plot time series

# weekdayplot <- group_by(weekdays,interval) %>%
#                 summarise(ave_steps = mean(steps))
# 
# weekendplot <- group_by(weekend,interval) %>%        
#         summarise(ave_steps = mean(steps))
# 
# par(mfrow = c(1,2))
# plot(weekdayplot,type = "l",main = "Weekday steps",ylab = "Average Steps","Interval",ylim=c(0,300))
# plot(weekendplot,type = "l",main = "Weekend steps",ylab = "Average Steps",xlab="Interval",ylim=c(0,300))
#        
f<-ggplot(finaldf,aes(x=interval,y=steps,color=weekstatus))
f+geom_line() + labs(title="Avg. Daily steps by Weekday Type",x = "Interval", y="No. of steps") + facet_wrap(~`weekstatus`,ncol = 1, nrow =2)
