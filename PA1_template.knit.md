---
title: "Course Project 1"
author: "M Dupont"
date: "23 October 2018"
output: 
  html_document: 
    keep_md: yes
---



## R Markdown

Import .csv file containing data from working directory


```r
data <- read.csv("activity.csv")
```

## What is the mean total number of steps taken per day? 

 Convert date column to date format and sum total steps for each day

```r
data$date <- as.Date(as.character(data$date))
steps.day <- as.data.frame(data %>% group_by(date) %>% summarise(total.steps=sum(steps)))
```

Make a histogram of the total steps taken each day

```r
hist(steps.day$total.steps,breaks=12, col="blue",main="Total Steps per Day",xlab="Total Daily Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


Calculate the mean and median of the total number of steps taken each day

```r
mean.steps <- format(mean(steps.day$total.steps,na.rm=TRUE), digits=7)
print(mean.steps)
```

```
## [1] "10766.19"
```

```r
median.steps <- format(median(steps.day$total.steps,na.rm=TRUE),digits=7)
print(median.steps)
```

```
## [1] "10765"
```
The mean number of steps taken per day is 10766.19 and the median steps taken per day is 10765. 

## What is the average daily activity pattern? 
Group data by interval and calculate average steps for each interval

```r
steps.int <- as.data.frame(data %>% na.omit %>% group_by(interval) %>% summarise(av.steps=mean(steps)))

par(mar=c(5,4,1,1),las=1)
plot(steps.int$interval,steps.int$av.steps,type='l',main="Average Steps per Interval",xlab="interval",ylab="average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Find the interval corresponding to the maximum average steps

```r
max.int <- steps.int$interval[which.max(steps.int$av.steps)]
print(max.int)
```

```
## [1] 835
```

The 5 minute interval which contains the maximum number of average steps is 835. 

## Input missing values

Calculate the total number of missing values (NAs)

```r
tot.NA <- sum(is.na(data$steps))
print(tot.NA)
```

```
## [1] 2304
```

The total number of missing values (NAs) is 2304.

Replace all NA values with the mean value for the interval over all days.

```r
impute.mean <- function(x) replace(x,is.na(x),mean(x,na.rm=TRUE))
na.replace <- ddply(data,~interval,transform,steps=impute.mean(steps))
data.na.rm <- as.data.frame(data.na.rm <- na.replace %>% group_by(date) %>% summarise(daily.steps=sum(steps)))
```
Create histogram of the total number of steps taken each day and calculate mean and median values.


```r
hist(data.na.rm$daily.steps,breaks=12,col="red",xlab="Average Steps per day", main="Average Steps per Day (NA replaced)")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean.na.rm <- format(mean(data.na.rm$daily.steps), digits=7)
print(mean.na.rm)
```

```
## [1] "10766.19"
```

```r
median.na.rm <- format(median(data.na.rm$daily.steps), digits=7)
print(median.na.rm)
```

```
## [1] "10766.19"
```
The mean total number of daily steps is 10766.19, and the median is 10766.19. The mean number of steps is the same as for the dataset without NAs replaced. The median number of steps has changed from 10765 to 10766.19


## Are there differences in activity patterns between weekdays and weekends?

Create factor variable designating 'weekday' or 'weekend'.

```r
weekdays1 <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
na.replace$wday <- factor((weekdays(na.replace$date) %in% weekdays1), levels=c(FALSE,TRUE),labels=c('weekend','weekday'))
```
Make a panel plot containing data averaged over 5 min intervals for weekdays and weekends


```r
data.wday <- as.data.frame(na.replace %>% group_by(interval,wday) %>% summarise(av.steps=mean(steps)))

p1 <- ggplot(data.wday, aes(x=interval,y=av.steps)) +
               geom_line()+
               facet_wrap(.~wday,nrow=2,ncol=1)+
               xlab("Interval")+
               ylab("Average Steps")

print(p1)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

The activity pattern of average steps for each interval is different for weekdays and weekends. 
