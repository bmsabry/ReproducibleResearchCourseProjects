---
title: "Project1ReproducibleResearch"
author: "Bassam Abdelnabi"
date: "5/30/2019"
output: 
  html_document: 
    keep_md: yes
---


```r
knitr::opts_chunk$set(echo = TRUE)
```

### Mean per day analysis


```r
library(ggplot2)
library(dplyr)
setwd("C:/Data")
Activity <- data.table::fread("activity.csv", header=TRUE)

#getting mean number of steps
sumstepsperday<-tapply(Activity$steps, Activity$date, sum,na.rm=TRUE)
#this is the total number of steps per day
meanstepsperday<-mean(sumstepsperday)
#this is the mean number of steps for all days
medianstepsperday<-median(sumstepsperday)
#this is the median number of steps for all days

hist(sumstepsperday, xlab="Total Steps per day", ylab="freq", main="Histogram Total steps per day", breaks=1000)
```

![](Project1ReproducibleResearch_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
cat("The mean steps per day are", meanstepsperday, "steps")
```

```
## The mean steps per day are 9354.23 steps
```

```r
cat("The median steps per day are", medianstepsperday, "steps")
```

```
## The median steps per day are 10395 steps
```

### Daily activity pattern


```r
#getting mean number of steps
Meanstepsperinterval<-tapply(Activity$steps, Activity$interval, mean,na.rm=TRUE)
#this is the Mean number of steps per interval
oneday <- filter(Activity,Activity$date=="2012-10-01")
time <-oneday$interval
plot(time,Meanstepsperinterval,type = "l")
```

![](Project1ReproducibleResearch_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
MaxInterval<-which.max(Meanstepsperinterval)
cat("The max number of steps occurs at interval", MaxInterval[[1]], "which corresponds to",Activity$interval[MaxInterval[[1]]] )
```

```
## The max number of steps occurs at interval 104 which corresponds to 835
```
### Handling NA


```r
#getting the number of NA
NAInTheData<-sum(is.na(Activity))
cat("The steps are showing NA values", NAInTheData, "times")
```

```
## The steps are showing NA values 2304 times
```

```r
ActivityNAFilled<-Activity
ActivityNAFilled$AvgSteps<-Meanstepsperinterval
#now replacing each NA with the corresponding average number of steps for the same period for all the data set
ActivityNAFilled$steps[is.na(ActivityNAFilled$steps)]<-ActivityNAFilled$AvgSteps[which(is.na(ActivityNAFilled$steps))]

sumstepsperday<-tapply(ActivityNAFilled$steps, ActivityNAFilled$date, sum,na.rm=TRUE)
#this is the total number of steps per day
meanstepsperday<-mean(sumstepsperday)
#this is the mean number of steps for all days
medianstepsperday<-median(sumstepsperday)
#this is the median number of steps for all days

hist(sumstepsperday, xlab="Adjusted Total Steps per day", ylab="freq", main="Adjusted Histogram Total steps per day", breaks=1000)
```

![](Project1ReproducibleResearch_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
cat("The adjusted (NA replaced) mean steps per day are", meanstepsperday, "steps")
```

```
## The adjusted (NA replaced) mean steps per day are 10766.19 steps
```

```r
cat("The adjusted (NA replaced) median steps per day are", medianstepsperday, "steps")
```

```
## The adjusted (NA replaced) median steps per day are 10766.19 steps
```

### Weekend vs Weekday differences


```r
library(chron)
#now adding a variable with True for a weekend and False for weekday
ActivityNAFilled$DayType<-is.weekend(ActivityNAFilled$date)
WeekdayData<-subset(ActivityNAFilled,ActivityNAFilled$DayType==FALSE)
WeekEndData<-subset(ActivityNAFilled,ActivityNAFilled$DayType==TRUE)
MeanstepsperintervalWeekDay<-tapply(WeekdayData$steps, WeekdayData$interval, mean,na.rm=TRUE)
MeanstepsperintervalWeekEnd<-tapply(WeekEndData$steps, WeekEndData$interval, mean,na.rm=TRUE)

layout(matrix(1:2, ncol = 1), widths = 1.5, heights = c(5,5), respect = FALSE)
par(mar = c(0, 5, 5, 2.1))
plot(time,MeanstepsperintervalWeekDay,type = "l", ylab = "WeekDay")
par(mar = c(5, 5, 0, 2.1))
plot(time,MeanstepsperintervalWeekEnd,type = "l",ylab = "WeekEnd")
```

![](Project1ReproducibleResearch_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
