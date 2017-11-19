---
output: 
  html_document: 
    keep_md: yes
---
*Steps and fitness monitors*
========================================================================

Preppring R studio 

```r
library(knitr)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.2
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.4.2
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

First we are going to read in the data and place it in the appropriate form

```r
activityData <- read.csv ("activity.csv", header = T, sep = ",", stringsAsFactors = F)
activityData$date <- as.Date(activityData$date, "%Y-%m-%d")
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Then we are going to get some information about the data set 


```r
dim(activityData)
```

```
## [1] 17568     3
```

```r
head(activityData)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

What are thr total number of steps per day?
--------------------------------------------------------------------------------------------------
The next step is to understand what is the mean total number of steps taken per day. 

```r
activity_per_day<-activityData[which(!is.na(activityData$steps)),]
perday<-tapply(activity_per_day$steps, activity_per_day$date, sum)
```
Now we can create a histogram 

```r
hist(perday,10, main = "Total number of steps taken per day", xlab = "")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
Calculatoring the mean

```r
mean(activity_per_day$steps)
```

```
## [1] 37.3826
```
Calculating the median

```r
median(activity_per_day$steps)
```

```
## [1] 0
```

What is the average daily activity pattern?
---------------------------------------------------------------------
Make a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
agginterval <- aggregate(steps ~ interval, activityData, FUN=sum)

plot(activityData$interval, activityData$steps, 
     type = "l", lwd = 2,
     xlab = "Interval", 
     ylab = "Total Steps",
     main = "Total Steps vs. 5-Minute Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Which 5 minutes internal,on average across all days in the dataset, contains the maximum number of steps? 

```g
filter(agginterval, steps==max(steps))
```

Missing Values
--------------------------------------------------------------------------------------
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 
NA's


```r
missing_vals <- sum(is.na(activityData$steps))
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
meaninterval<- aggregate(steps ~ interval, activityData, FUN=mean)
anew <- merge(x=activityData, y=meaninterval, by="interval")
anew$steps <- ifelse(is.na(anew$steps.x), anew$steps.y, anew$steps.x)
head(anew)
```

```
##   interval steps.x       date  steps.y    steps
## 1        0      NA 2012-10-01 1.716981 1.716981
## 2        0       0 2012-11-23 1.716981 0.000000
## 3        0       0 2012-10-28 1.716981 0.000000
## 4        0       0 2012-11-06 1.716981 0.000000
## 5        0       0 2012-11-24 1.716981 0.000000
## 6        0       0 2012-11-15 1.716981 0.000000
```
Checking for missing values 

```r
sum(is.na(anew$steps))
```

```
## [1] 0
```
A histogram of the total number of steps taken each day
-----------------------------------------------------------------------------
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
anew$dow = as.factor(ifelse(is.element(weekdays(as.Date(anew$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval <- aggregate(steps ~ interval + dow, anew, mean)

library(lattice)

xyplot(steps_by_interval$steps ~ steps_by_interval$interval|steps_by_interval$dow, main="Average Steps per Day ",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

