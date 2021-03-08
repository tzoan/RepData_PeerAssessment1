
##download file and unzip data
if(!file.exists("./reproducibleresearch")){dir.create("./reproducibleresearch")}
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl, destfile = "./reproducibleresearch/repdata_data_activity.zip")
unzip(zipfile = "./reproducibleresearch/repdata_data_activity.zip", exdir = "./reproducibleresearch")


#reading and exploring activity data
library(lubridate)
activity<-read.csv("./reproducibleresearch/activity.csv",header=TRUE)
str(activity)## 17568 observations of 3 variables:steps, data, interval
head(activity,n=3)
activity$date <- ymd(activity$date)
activity$weekend <- as.factor(ifelse(weekdays(activity$date)=="Saturday" | weekdays(activity$date)=="Sunday","weekend","weekday"))
activity$dayofweek <- as.factor(weekdays(activity$date))

##total number of steps
library(dplyr)
library(ggplot2)
stepsbyday<- activity %>%  # Group by day and summarize total steps across the board
        select(date,steps)%>%
        group_by(date) %>%
        summarize(Stepsperday= sum(steps, na.rm = TRUE))
#Histogram of the total number of steps taken each day
png("plot1.png",width=480, height=480)
qplot(Stepsperday,data=stepsbyday,na.rm=TRUE,binwidth=500,xlab='Total steps per day', ylab='Frequency using binwith 500',main = 'Histogram of the total number of steps taken each day')
dev.off

# Mean and median steps taken each day
summary(stepsbyday)
Mean=mean(stepsbyday$Stepsperday)
Median=as.integer(median(stepsbyday$Stepsperday))

#time series plot of the average number of steps taken
interval_average <- activity %>% 
        group_by(interval) %>% 
        summarise(average = mean(steps,na.rm = TRUE))
interval_average
ggplot(interval_average,aes(interval,average))+geom_line()+labs(x="5-minute interval",y="Average steps taken across all days")

##The 5-minute interval that, on average, contains the maximum number of steps
interval_average[which.max(interval_average$average),]

##Imputing data
activity_no_NA <- activity[which(!is.na(activity$steps)),]

##average number of steps taken without NA
interval_average_no_NA <- activity_no_NA %>% 
        group_by(interval) %>% 
        summarise(average = mean(steps,na.rm = TRUE))
interval_average_no_NA

# convert the average to integer
interval_average_no_NA$average <- as.integer(interval_average_no_NA$average)

#subset dataset where steps have NAs
activity_na <- activity[which(is.na(activity$steps)),]

# fill NAs with average steps based on interval
activity_na$steps <- ifelse(activity_na$interval==interval_average_no_NA$interval,interval_average_no_NA$average)

# row bind the datasets that do not have NAs and the dataset where NAs are replaced with
activity_impute <- rbind(activity_no_NA,activity_na)
str(activity_impute)

##Steps per day
stepsbyday_impute<- activity_impute %>%  # Group by day and summarize total steps across the board
        select(date,steps)%>%
        group_by(date) %>%
        summarize(Stepsperday= sum(steps, na.rm = TRUE))
stepsbyday_impute

#Histogram of the total number of steps taken each day after impute data
png("plot1.png",width=480, height=480)
qplot(Stepsperday,data=stepsbyday_impute,na.rm=TRUE,binwidth=500,xlab='Total steps per day', ylab='Frequency using binwith 500',main = 'Histogram of the total number of steps taken each day')
dev.off

#mean and median
summary(stepsbyday_impute)
mean(stepsbyday_impute$Stepsperday)
median(stepsbyday_impute$Stepsperday)

#Panel plot
meansteps <- activity_impute %>% group_by(interval,weekend) %>% summarise(average = mean(steps))
ggplot(meansteps,aes(interval,average,color=weekend))+geom_line()+facet_grid(weekend~.) +xlab("Interval") + ylab("Mean of Steps") +ggtitle("Comparison of Average Number of Steps in Each Interval")



