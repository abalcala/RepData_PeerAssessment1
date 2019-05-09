---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Load libraries and read file

```r
library(dplyr)
library(ggplot2)
library(lubridate)
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")
unzip("activity.zip")
act0 <- read.csv("activity.csv")
```
Convert date column to date and omit NA rows.

```r
act0$date<- as.Date(act0$date, format = "%Y-%m-%d")
act<-na.omit(act0)
```


## What is mean total number of steps taken per day?
Get sum of steps of each data

```r
stepssum<-aggregate(steps~date,data=act,sum)
hist(stepssum$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## What is the average daily activity pattern?
Get the average steps of each interval

```r
stepsinterval<-aggregate(steps~interval,data=act,mean)
g<-ggplot(stepsinterval,aes(interval,steps))
g+geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Imputing missing values


```r
stepsinterval<-aggregate(steps~interval,data=act,mean)
merged<-merge(act0,stepsinterval,by.x="interval",by.y="interval")
ordered<-merged[order(merged$date,merged$interval),]
```
Create list of row index that has NA steps in act0

```r
act0na<-which(is.na(act0$steps)==TRUE)
```
If row index in ordered is in list previously created, replace steps in df act0 with the value of steps in df stepsinterval. Create histogram of the sum of steps.

```r
for (i in 1:nrow(ordered))
{
  ifelse(i %in% act0na,ordered$steps.x[i]<-ordered$steps.y[i],ordered$steps.x[i])
}
ordered$day<-ifelse(wday(ordered$date) %in% c(2:6),"weekday","weekend")
steps3sum<-aggregate(steps.x~date,data=ordered,sum)
steps3mean<-aggregate(steps.x~date,data=ordered,mean)
steps3median<-aggregate(steps.x~date,data=ordered,median)

hist(steps3sum$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?
Get average steps per day("weekday" or "weekends"). Create line chart.

```r
steps4mean<-aggregate(steps.x~day+interval,data=ordered,mean)
g<-ggplot(steps4mean,aes(interval,steps.x,group=day))
g+geom_line()+ facet_grid(day  ~ ., scales="free")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
