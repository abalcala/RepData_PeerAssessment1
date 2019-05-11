---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
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

![](PA1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
steps1mean<-aggregate(steps~date,data=act,mean)
steps1median<-aggregate(steps~date,data=act,median)
df1<-data.frame(mean=steps1mean,median=steps1median)
df
```

```
## function (x, df1, df2, ncp, log = FALSE) 
## {
##     if (missing(ncp)) 
##         .Call(C_df, x, df1, df2, log)
##     else .Call(C_dnf, x, df1, df2, ncp, log)
## }
## <bytecode: 0x000000001db06d50>
## <environment: namespace:stats>
```

## What is the average daily activity pattern?
Get the average steps of each interval

```r
stepsinterval<-aggregate(steps~interval,data=act,mean)
g<-ggplot(stepsinterval,aes(interval,steps))
g+geom_line()
```

![](PA1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
Get the row with the maximum steps

```r
maxinterval<-which(stepsinterval$steps==max(stepsinterval$steps))
stepsinterval[maxinterval,]
```

```
##     interval    steps
## 104      835 206.1698
```
## Imputing missing values


```r
stepsinterval<-aggregate(steps~interval,data=act,mean)
merged<-merge(act0,stepsinterval,by.x="interval",by.y="interval")
ordered<-merged[order(merged$date,merged$interval),]
```
Create list of row index that has NA steps in act0 and get no. of NA

```r
act0na<-which(is.na(act0$steps)==TRUE)
length(act0na)
```

```
## [1] 2304
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

![](PA1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
df3<-data.frame(mean=steps3mean,median=steps3median)
df3
```

```
##     mean.date mean.steps.x median.date median.steps.x
## 1  2012-10-01   37.3825996  2012-10-01       34.11321
## 2  2012-10-02    0.4375000  2012-10-02        0.00000
## 3  2012-10-03   39.4166667  2012-10-03        0.00000
## 4  2012-10-04   42.0694444  2012-10-04        0.00000
## 5  2012-10-05   46.1597222  2012-10-05        0.00000
## 6  2012-10-06   53.5416667  2012-10-06        0.00000
## 7  2012-10-07   38.2465278  2012-10-07        0.00000
## 8  2012-10-08   37.3825996  2012-10-08       34.11321
## 9  2012-10-09   44.4826389  2012-10-09        0.00000
## 10 2012-10-10   34.3750000  2012-10-10        0.00000
## 11 2012-10-11   35.7777778  2012-10-11        0.00000
## 12 2012-10-12   60.3541667  2012-10-12        0.00000
## 13 2012-10-13   43.1458333  2012-10-13        0.00000
## 14 2012-10-14   52.4236111  2012-10-14        0.00000
## 15 2012-10-15   35.2048611  2012-10-15        0.00000
## 16 2012-10-16   52.3750000  2012-10-16        0.00000
## 17 2012-10-17   46.7083333  2012-10-17        0.00000
## 18 2012-10-18   34.9166667  2012-10-18        0.00000
## 19 2012-10-19   41.0729167  2012-10-19        0.00000
## 20 2012-10-20   36.0937500  2012-10-20        0.00000
## 21 2012-10-21   30.6284722  2012-10-21        0.00000
## 22 2012-10-22   46.7361111  2012-10-22        0.00000
## 23 2012-10-23   30.9652778  2012-10-23        0.00000
## 24 2012-10-24   29.0104167  2012-10-24        0.00000
## 25 2012-10-25    8.6527778  2012-10-25        0.00000
## 26 2012-10-26   23.5347222  2012-10-26        0.00000
## 27 2012-10-27   35.1354167  2012-10-27        0.00000
## 28 2012-10-28   39.7847222  2012-10-28        0.00000
## 29 2012-10-29   17.4236111  2012-10-29        0.00000
## 30 2012-10-30   34.0937500  2012-10-30        0.00000
## 31 2012-10-31   53.5208333  2012-10-31        0.00000
## 32 2012-11-01   37.3825996  2012-11-01       34.11321
## 33 2012-11-02   36.8055556  2012-11-02        0.00000
## 34 2012-11-03   36.7048611  2012-11-03        0.00000
## 35 2012-11-04   37.3825996  2012-11-04       34.11321
## 36 2012-11-05   36.2465278  2012-11-05        0.00000
## 37 2012-11-06   28.9375000  2012-11-06        0.00000
## 38 2012-11-07   44.7326389  2012-11-07        0.00000
## 39 2012-11-08   11.1770833  2012-11-08        0.00000
## 40 2012-11-09   37.3825996  2012-11-09       34.11321
## 41 2012-11-10   37.3825996  2012-11-10       34.11321
## 42 2012-11-11   43.7777778  2012-11-11        0.00000
## 43 2012-11-12   37.3784722  2012-11-12        0.00000
## 44 2012-11-13   25.4722222  2012-11-13        0.00000
## 45 2012-11-14   37.3825996  2012-11-14       34.11321
## 46 2012-11-15    0.1423611  2012-11-15        0.00000
## 47 2012-11-16   18.8923611  2012-11-16        0.00000
## 48 2012-11-17   49.7881944  2012-11-17        0.00000
## 49 2012-11-18   52.4652778  2012-11-18        0.00000
## 50 2012-11-19   30.6979167  2012-11-19        0.00000
## 51 2012-11-20   15.5277778  2012-11-20        0.00000
## 52 2012-11-21   44.3993056  2012-11-21        0.00000
## 53 2012-11-22   70.9270833  2012-11-22        0.00000
## 54 2012-11-23   73.5902778  2012-11-23        0.00000
## 55 2012-11-24   50.2708333  2012-11-24        0.00000
## 56 2012-11-25   41.0902778  2012-11-25        0.00000
## 57 2012-11-26   38.7569444  2012-11-26        0.00000
## 58 2012-11-27   47.3819444  2012-11-27        0.00000
## 59 2012-11-28   35.3576389  2012-11-28        0.00000
## 60 2012-11-29   24.4687500  2012-11-29        0.00000
## 61 2012-11-30   37.3825996  2012-11-30       34.11321
```

## Are there differences in activity patterns between weekdays and weekends?
Get average steps per day("weekday" or "weekends"). Create line chart.

```r
steps4mean<-aggregate(steps.x~day+interval,data=ordered,mean)
g<-ggplot(steps4mean,aes(interval,steps.x,group=day))
g+geom_line()+ facet_grid(day  ~ ., scales="free")
```

![](PA1_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
