---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data
extracting the activity zip file and reading the  file contents        

```r
unzip(zipfile = "activity.zip")
activityData <-read.csv("activity.csv",sep = ",")
```
## What is mean total number of steps taken per day?
1.Calculating total number of steps taken per day
Splitting the data by date - running on this on the data where NA's were ignored

```r
naOmittedData <-na.omit(activityData)
naOmittedData$date <- factor(naOmittedData$date)
splitNaOmmittedDataByDate <- split(naOmittedData,naOmittedData$date)
noOfStepsPerDay <- lapply(splitNaOmmittedDataByDate,function(x){sum(x["steps"])})
```
2.Histogram

```r
stepsTakenPerDay<-as.numeric(noOfStepsPerDay)
hist(stepsTakenPerDay)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3.Calculating mean and median

```r
mean(stepsTakenPerDay)
```

```
## [1] 10766.19
```

```r
median(stepsTakenPerDay)
```

```
## [1] 10765
```
## What is the average daily activity pattern?
1.time series plot of the 5minute interval and the average number of steps taken

```r
splitByIntervalData <- split(naOmittedData,naOmittedData$interval)
noOfStepsPerInterval <- lapply(splitByIntervalData,
                               function(x){sum(x["steps"])/nrow(x["steps"])}
                               )
stepsInterval<-as.numeric(names(noOfStepsPerInterval))
steps<-as.numeric(noOfStepsPerInterval)
df<-as.data.frame(m<-cbind(stepsInterval,steps))
plot(stepsInterval,steps)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2.The 5-minute interval that contains the maximum number of steps on average 
across all the days in the dataset

```r
tail(df[order(steps),],1)["stepsInterval"]
```

```
##     stepsInterval
## 104           835
```
## Imputing missing values
1.Missing Data in actual dataset 
Missing Data count in steps column

```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```
Missing Data count in date column

```r
sum(is.na(activityData$date))
```

```
## [1] 0
```
Missing Data count in interval column

```r
sum(is.na(activityData$interval))
```

```
## [1] 0
```
2.Strategy used to fill the missing values -
All the NA values in steps column were filled with average steps of all 
the observed data of all the dates and intervals i.e average steps/interval.

3.Creating a new dataset that is equal to the original dataset but  the missing 
data filled is filled in the new dataset.

```r
cleanActivityData <- activityData
```
Calculating the mean of steps for all the observered dates data

```r
sum<-sum(naOmittedData["steps"])
length <- nrow(naOmittedData)
averageSteps <- sum/length
```
Replace all the steps - NA values with the average no of steps

```r
cleanActivityData$steps[is.na(cleanActivityData$steps)] <- averageSteps
```
splitDataByDate <- split(cleanActivityData,cleanActivityData$date)
noOfStepsPerDay <- lapply(splitDataByDate,function(x){sum(x["steps"])})
```
4.Histogram

```r
stepsTakenPerDay<-as.numeric(noOfStepsPerDay)
hist(stepsTakenPerDay)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

Mean , Median calculation

```r
mean(stepsTakenPerDay)
```

```
## [1] 10766.19
```

```r
median(stepsTakenPerDay)
```

```
## [1] 10765
```
As the NA's were replaced by the average of all the observed steps-
i.e sum of all the steps for all days on all intervals/number of observations,
mean and median were same before and after imputing values

## Are there differences in activity patterns between weekdays and weekends?
1.new factor variable 

```r
cleanActivityData$day <- ifelse(weekdays(as.Date(cleanActivityData$date)) != "Saturday" & weekdays(as.Date(cleanActivityData$date)) != "Sunday","weekday", "weekend")
cleanActivityData$day<-as.factor(cleanActivityData$day)
```
2.panel plot containing a time series plot  of the 5-minute interval
and the average number of steps 

```r
library(lattice)
par(mfrow=c(2,1))
plot <- xyplot(steps~interval|day,data = cleanActivityData,xlab = "Interval",ylab = "Number of steps",type = "l",layout = c(1,2))
print(plot)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 
