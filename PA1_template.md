---
title: "PA1_template"
output: 
  html_document:
    keep_md: true
---



#Reproducible Research: Assignment 1

##Download file and unzip data

```r
if(!file.exists("./reproducibleresearch")){dir.create("./reproducibleresearch")}
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl, destfile = "./reproducibleresearch/repdata_data_activity.zip")
unzip(zipfile = "./reproducibleresearch/repdata_data_activity.zip", exdir = "./reproducibleresearch")
```

##Reading and exploring activity data

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
activity<-read.csv("./reproducibleresearch/activity.csv",header=TRUE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activity$date <- ymd(activity$date)
activity$weekend <- as.factor(ifelse(weekdays(activity$date)=="Saturday" | weekdays(activity$date)=="Sunday","weekend","weekday"))
activity$dayofweek <- as.factor(weekdays(activity$date))
```
##Total number of steps taken each day

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
stepsbyday<- activity %>%  # Group by day and summarize total steps across the board
        select(date,steps)%>%
        group_by(date) %>%
        summarize(Stepsperday= sum(steps, na.rm = TRUE))
#Histogram of the total number of steps taken each day
qplot(Stepsperday,data=stepsbyday,na.rm=TRUE,binwidth=500,xlab='Total steps per day', ylab='Frequency using binwith 500',main = 'Histogram of the total number of steps taken each day')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
## Mean and median steps taken each day

```r
summary(stepsbyday)
```

```
##       date             Stepsperday   
##  Min.   :2012-10-01   Min.   :    0  
##  1st Qu.:2012-10-16   1st Qu.: 6778  
##  Median :2012-10-31   Median :10395  
##  Mean   :2012-10-31   Mean   : 9354  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

```r
Mean=as.integer(mean(stepsbyday$Stepsperday))
Median=as.integer(median(stepsbyday$Stepsperday))
```
The mean steps taken each day is **9354** and the median **10395**.

##Time series plot of the average number of steps taken

```r
interval_average <- activity %>% 
        group_by(interval) %>% 
        summarise(average = mean(steps,na.rm = TRUE))
ggplot(interval_average,aes(interval,average))+geom_line()+labs(x="5-minute interval",y="Average steps taken across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

##The 5-minute interval that, on average, contains the maximum number of steps

```r
Max=interval_average[which.max(interval_average$average),]
```

##Imputing data

Subset dataset where there are no Missing Values


```r
activity_no_NA <- activity[which(!is.na(activity$steps)),]
```

Average number of steps taken without Missing Values


```r
interval_average_no_NA <- activity_no_NA %>% 
        group_by(interval) %>% 
        summarise(average = mean(steps,na.rm = TRUE))
```

Convert the average to integer.


```r
interval_average_no_NA$average <- as.integer(interval_average_no_NA$average)
```

Subset dataset where steps have Missing Values.


```r
activity_na <- activity[which(is.na(activity$steps)),]
```



```r
dataset.na=nrow(activity_na)
```

The number of Missing Values in the dataset:**2304**

Fill NAs with average steps based on interval


```r
activity_na$steps <- ifelse(activity_na$interval==interval_average_no_NA$interval,interval_average_no_NA$average)
```

Row bind the datasets that do not have Missing Values and the dataset where Mising Values are replaced with mean values.


```r
activity_impute <- rbind(activity_no_NA,activity_na)
str(activity_impute)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date     : Date, format: "2012-10-02" "2012-10-02" ...
##  $ interval : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ weekend  : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
##  $ dayofweek: Factor w/ 7 levels "Friday","Monday",..: 6 6 6 6 6 6 6 6 6 6 ...
```

##Steps per day after imputing data


```r
stepsbyday_impute<- activity_impute %>%  # Group by day and summarize total steps across the board
        select(date,steps)%>%
        group_by(date) %>%
        summarize(Stepsperday= sum(steps, na.rm = TRUE))
```

##Histogram of the total number of steps taken each day after impute data


```r
qplot(Stepsperday,data=stepsbyday_impute,na.rm=TRUE,binwidth=500,xlab='Total steps per day', ylab='Frequency using binwith 500',main = 'Histogram of the total number of steps taken each day')
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

##Mean and Median

```r
summary(stepsbyday_impute)
```

```
##       date             Stepsperday   
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10641  
##  Mean   :2012-10-31   Mean   :10750  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

```r
Mean_impute=as.integer(mean(stepsbyday_impute$Stepsperday))
Median_impute=as.integer(median(stepsbyday_impute$Stepsperday))
```

The mean steps taken each day is **10749** and the median **10641**.

##Panel plot in activity patterns between weekdays and weekends


```r
meansteps <- activity_impute %>% group_by(interval,weekend) %>% summarise(average = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
ggplot(meansteps,aes(interval,average,color=weekend))+geom_line()+facet_grid(weekend~.) +xlab("Interval") + ylab("Mean of Steps") +ggtitle("Comparison of Average Number of Steps in Each Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->






