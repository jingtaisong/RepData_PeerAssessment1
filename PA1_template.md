---
title: "Reproducible Research: Peer Assignment 1"
author: "Taisong Jing"
date: "Friday, August 15, 2014"
output: html_document
---

This is the report for Peer Assignment 1 for the Reproducible Research course. Several questions on a dataset about personal movement using monitoring device will be answered below.

---

## Data

The data is downloaded from [here][1]. The variables included are: 


```r
data<-read.csv("activity.csv")
```

- steps: the number of steps that the subject takes during some 5-minute interval
- date: the date on which the measurement is taken
- interval: a number as the identifier for the 5-minute interval

The data is stored in a comma-separated-value (csv) document named "activity.csv" and there are a total of 17568 observations.

----

## Question 1: What is mean total number of steps taken per day?

We first aggregate the total number of steps according to date and make a histogram:


```r
steps.by.date<-aggregate(subset(data,select=steps),by=list(date=data$date),FUN=function(x) sum(x,na.rm=TRUE))
hist(steps.by.date$steps,xlab="steps",ylim=c(0,30),main="Histogram of total number of steps by date")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

The mean and median of the total number of steps per day are


```r
mean(steps.by.date$steps)
```

```
## [1] 9354
```

```r
median(steps.by.date$steps)
```

```
## [1] 10395
```
----

## Question 2: What is the average daily activity pattern?

We then aggregate the average number of steps according to interval and make a time series plot:


```r
steps.by.interval<-aggregate(subset(data,select=steps),by=list(interval=data$interval),FUN=function(x) mean(x,na.rm=TRUE))
plot(steps.by.interval$interval,steps.by.interval$steps,xlab="5-minute interval", ylab="average steps across all days", main="Time series plot of average steps across all days by the 5-minute intervals", type='l')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

The identifier for the 5-minute interval during which most steps are taken is


```r
steps.by.interval$interval[which.max(steps.by.interval$steps)]
```

```
## [1] 835
```

---

## Question 3: Imputing missing values

The missing values in the data set may cause bias in the analysis. The total number of observations with missing data is:


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
We fill the missing data with the mean of the average steps across all days of the corresponding 5-minute interval, and save the modified data as a new dataset.


```r
newdata<-data
N<-dim(newdata)[1]
for (i in 1:N) {
  if (is.na(newdata$steps[i])) {
    time<-newdata$interval[i]
    pos<-which(steps.by.interval$interval==time)
    newdata$steps[i]<-steps.by.interval$steps[pos]
  }
}
```

The histogram of the total number of steps taken each day for the new data set with missing values filled is


```r
new.steps.by.date<-aggregate(subset(newdata,select=steps),by=list(date=newdata$date),FUN=function(x) sum(x,na.rm=TRUE))
hist(new.steps.by.date$steps,xlab="steps",ylim=c(0,40),main="Histogram of total number of steps by date with missing values filled")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

The mean and median of the total number of steps per day for the new data set with missing values filled is

```r
mean(new.steps.by.date$steps)
```

```
## [1] 10766
```

```r
median(new.steps.by.date$steps)
```

```
## [1] 10766
```
The mean and median are different from those before filling the missing values. Filling the missing values reduces the difference between the median and the mean (so the distribution is less skewed afterwards) and increases the mean and median.

---

## Question 4: Are there differences in activity patterns between weekdays and weekends?

We create a new factor variable in the dataset to indicate whether the date is a weekday or a weekend:

```r
newdata$weekdays<-rep("weekdays",N)
date.format<-as.Date(newdata$date)
for (i in 1:N) {
  if (weekdays(date.format[i])=="ÐÇÆÚÁù" | weekdays(date.format[i])=="ÐÇÆÚÈÕ") newdata$weekdays[i]<-"weekends"
}
newdata$weekdays<-as.factor(newdata$weekdays)
```
We create time series plots of the 5-minute intervals and the average number of steps taken, averaged over all weekday days or weekend days.

```r
newdata.steps.by.interval.and.weekdays<-aggregate(steps~interval+weekdays,data=newdata,FUN=mean)
library(ggplot2)
g<-qplot(x=interval,y=steps,data=newdata.steps.by.interval.and.weekdays,geom="line")
g<-g+facet_grid(weekdays~.)
g+labs(x="5-minute interva", y="average steps")+labs(title="Average steps across all weekdays/weekends by the 5-minute intervals")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

From the panel plots we can see that over the weekends, one tends to start walking at a much later time and the distribution of steps spreads over the entire day. This could be interpreted as more activities during the daytime comparing to work during the weekdays.

[1]: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
