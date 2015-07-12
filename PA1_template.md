# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
unzip("activity.zip")
myData<-read.csv("activity.csv",header=TRUE)
head(myData)
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

```r
tail(myData)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```
We select the complete cases for further processing

```r
goodData<-complete.cases(myData$steps)
```

## What is mean total number of steps taken per day?

Summarize the steps by the date  
We exclude the values that are Not Available

```r
stepsPerDay<-aggregate(myData$steps, by=list(myData$date), FUN=sum,na.rm=TRUE)
```
Rename columns

```r
names(stepsPerDay) <- c("date", "sumSteps")
head(stepsPerDay)
```

```
##         date sumSteps
## 1 2012-10-01        0
## 2 2012-10-02      126
## 3 2012-10-03    11352
## 4 2012-10-04    12116
## 5 2012-10-05    13294
## 6 2012-10-06    15420
```

```r
hist(stepsPerDay$sumSteps,xlab="steps", main="Total Steps in a day",breaks=10)
meanStep<-mean(stepsPerDay$sumSteps)
abline(v = meanStep, lwd = 2, lty = 2,col=2)
medianStep<-median(stepsPerDay$sumSteps)
abline(v = medianStep, lwd = 2, lty = 2,col=3)
legend("topright", pch = "-", col = c("red", "green"), legend = c("mean", "median"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 
  
Mean of the total steps

```r
meanStep
```

```
## [1] 9354.23
```
Median of the total steps

```r
medianStep
```

```
## [1] 10395
```
## What is the average daily activity pattern?


```r
stepsOnTime<-aggregate(myData$steps,by=list(myData$interval),FUN=mean,na.rm=TRUE)
names(stepsOnTime) <- c("interval", "meanSteps")
str(stepsOnTime)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ interval : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ meanSteps: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

```r
with(stepsOnTime,plot(interval/100,meanSteps,type="l",xlab="hours",ylab="Means of steps",main="Means of Steps in 2 month interval"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 
  
The most "stepping" interval

```r
maxInt<-(subset(stepsOnTime,stepsOnTime$meanSteps==max(stepsOnTime$meanSteps)))$interval
maxInt
```

```
## [1] 835
```
Interval in hours

```r
maxInt/100
```

```
## [1] 8.35
```
## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
