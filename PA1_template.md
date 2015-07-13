# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(ggplot2)
options(digits=12)
unzip("activity.zip")
myData<-read.csv("activity.csv",header=TRUE)
summary(myData)
```

```
##      steps                     date          interval      
##  Min.   :  0.0000000   2012-10-01:  288   Min.   :   0.00  
##  1st Qu.:  0.0000000   2012-10-02:  288   1st Qu.: 588.75  
##  Median :  0.0000000   2012-10-03:  288   Median :1177.50  
##  Mean   : 37.3825996   2012-10-04:  288   Mean   :1177.50  
##  3rd Qu.: 12.0000000   2012-10-05:  288   3rd Qu.:1766.25  
##  Max.   :806.0000000   2012-10-06:  288   Max.   :2355.00  
##  NA's   :2304          (Other)   :15840
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
summary(stepsPerDay$sumSteps)
```

```
##        Min.     1st Qu.      Median        Mean     3rd Qu.        Max. 
##     0.00000  6778.00000 10395.00000  9354.22951 12811.00000 21194.00000
```

```r
hist(stepsPerDay$sumSteps,xlab="steps", main="Total Steps in a day",breaks=10)
meanStep<-mean(stepsPerDay$sumSteps)
abline(v = meanStep, lwd = 2, lty = 2,col=2)
medianStep<-median(stepsPerDay$sumSteps)
abline(v = medianStep, lwd = 2, lty = 2,col=3)
legend("topright", pch = "-", col = c("red", "green"), legend = c("mean", "median"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 
  
Mean of the total steps

```r
meanStep
```

```
## [1] 9354.2295082
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

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 
  
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


```r
goodData<-complete.cases(myData$steps)
```
Number of rows NA

```r
nrow(myData[!goodData,])
```

```
## [1] 2304
```
The strategy is to fill with the mean of the 5 minute interval, because is more probable  
We recall from the activity pattern what we calculate, stepsOnTime  
That is the mean of steps in the intervals of 5 minutes

```r
#stepsOnTime<-aggregate(myData$steps,by=list(myData$interval),FUN=mean,na.rm=TRUE)
#names(stepsOnTime) <- c("interval", "meanSteps")
```
We merge the NA data with the average data. The common field is "interval"

```r
replacedData<-merge(myData[!goodData,],stepsOnTime)
```
We delete the extra column with NA values

```r
replacedData$steps<-NULL
```
Renaming the columns and reaggarange for binding

```r
names(replacedData) <- c("interval", "date","steps")
replaced2<-replacedData[,c(3,2,1)]
```
Binding the good data and the replaced data

```r
myData2<-rbind(myData[goodData,],replaced2)
```
Now we recall the first diagram for the new dataset

```r
stepsPerDay2<-aggregate(myData2$steps, by=list(myData2$date), FUN=sum,na.rm=TRUE)
names(stepsPerDay2) <- c("date", "sumSteps")
hist(stepsPerDay2$sumSteps,xlab="steps", main="Total Steps in a day",breaks=10)
meanStep2<-mean(stepsPerDay2$sumSteps)
abline(v = meanStep2, lwd = 2, lty = 2,col=2)
medianStep2<-median(stepsPerDay2$sumSteps)
abline(v = medianStep2, lwd = 2, lty = 2,col=3)
legend("topright", pch = "-", col = c("red", "green"), legend = c("mean", "median"))
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png) 

```r
meanStep2
```

```
## [1] 10766.1886792
```

```r
medianStep2
```

```
## [1] 10766.1886792
```

## Are there differences in activity patterns between weekdays and weekends?

Introduct a new column "wd" and fill with "weekdays"

```r
namevector <- c("wd")
myData2[,namevector] <- "weekdays"
```
Replace Saturdays and Sundays with "weekend"

```r
myData2[which(weekdays(as.Date(myData2$date,format="%Y-%m-%d"))=="Saturday", arr.ind=TRUE), 4] <- "weekend"
myData2[which(weekdays(as.Date(myData2$date,format="%Y-%m-%d"))=="Sunday", arr.ind=TRUE), 4] <- "weekend"
nrow(subset(myData2,wd=="weekend"))
```

```
## [1] 4608
```
Plotting

```r
stepsWeek<-aggregate(myData2$steps,by=list(myData2$interval,myData2$wd),FUN=mean,na.rm=TRUE)
names(stepsWeek) <- c("interval", "wd","meanSteps")

wdays<-subset(stepsWeek,wd=="weekdays")
wend<-subset(stepsWeek,wd=="weekend")

qplot(interval,meanSteps,data=stepsWeek,col=wd,geom="line")
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png) 

```r
#with(wdays,plot(interval/100,meanSteps,type="l",xlab="hours",ylab="Means of steps",main="Means of Steps in 2 month interval"))
#points(wend$interval,wend$meanSteps,col="red",type="l")
```
