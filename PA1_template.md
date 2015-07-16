# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Load the data and add a new column called intervalTime. intervalTime is cleaned version
of interval.

```r
ORIGIN = "2015-10-21"
TZ = "UTC"
activityData <- read.csv('activity/activity.csv', na.strings = "NA")

setIntervalTime <- function(row) {
  totalMinutes <- as.integer(row[3])
  hours <- totalMinutes %/% 100
  minutes <- totalMinutes %% 60
  
  timeStr <- sprintf("%02d:%02d", hours, minutes) 
  intervalTime_lt <- strptime(timeStr, "%H:%M")
  return(as.POSIXct(intervalTime_lt, origin = ORIGIN, tz = TZ))
}

activityData$intervalTime <- apply(activityData, 1, setIntervalTime)
```


## What is mean total number of steps taken per day?

```r
activityDataByDay <- aggregate(activityData$steps, by=list(date = activityData$date), 
                               FUN=sum, na.rm = TRUE)
colnames(activityDataByDay)[2] <- 'steps'

hist(activityDataByDay$steps, main = "Histogram of the Mean Steps by Day", xlab='Number of Steps')
legend("topright", c("mean", "median"), col=c("blue", "red"), lwd=2, lty=c(1,2))

meanStepsPerDay <- mean(activityDataByDay$steps, na.rm = TRUE)
abline(v = meanStepsPerDay, col = "blue", lwd = 2)
text(20, x=meanStepsPerDay, pos = 2, offset = 0.1, col = "blue",
     paste('mean = ', format(round(meanStepsPerDay, 2), nsmall = 2)))

medianStepsPerDay <- median(activityDataByDay$steps, na.rm = TRUE)
abline(v = medianStepsPerDay, col = "red", lwd = 2, lty=2)
text(15, x=medianStepsPerDay, pos = 4, offset = 0.1, col = "red",
     paste('median = ', format(round(medianStepsPerDay, 2), nsmall = 2)))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

The mean is 9354.23 and the median is 10395.

## What is the average daily activity pattern?

```r
activityDataByInterval <- aggregate(activityData$steps, 
                                    by=list(intervalTime = activityData$intervalTime), 
                                    FUN=mean, na.rm = TRUE)
colnames(activityDataByInterval)[2] <- 'steps'

maxIntervalSteps <- max(activityDataByInterval$steps)
maxIntervalStepsIdx <- which.max(activityDataByInterval$steps) 
maxIntervalTime <- activityDataByInterval$intervalTime[maxIntervalStepsIdx]
maxIntervalStr <- sprintf("%s", format(as.POSIXct(maxIntervalTime, 
                                            origin = ORIGIN, tz = TZ), "%H:%M"))

plot(x = activityDataByInterval$intervalTime, 
     y = activityDataByInterval$steps, type="l", xaxt="n",
    main = "Average Daily Activity", xlab = "Interval", ylab = "Number of Steps")

xaxis <- activityDataByInterval$intervalTime[seq(1, 
                                                 length(activityDataByInterval$intervalTime), 20)]
axis(1, at=xaxis,
     labels=sprintf("%s", format(as.POSIXct(xaxis, 
                                            origin = ORIGIN, tz = TZ), "%H:%M")))

abline(v = maxIntervalTime, col = "blue", lwd = 2)
text(maxIntervalSteps, x=maxIntervalTime, pos = 4, offset = 0.25, col = "blue",
     paste('max steps interval = ', maxIntervalStr))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

The maximum number of steps is 206.17 at interval, 08:55.

## Imputing missing values
Number of rows with missing values:

```r
length(is.na(activityData$steps))
```

```
## [1] 17568
```

Replace NA values with average daily interval values.

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activityDataByInterval <- aggregate(activityData$steps, 
                                    by=list(intervalTime = activityData$intervalTime), 
                                    FUN=mean, na.rm = TRUE)
colnames(activityDataByInterval)[2] <- 'meanSteps'

activityData2 <- merge(activityData, activityDataByInterval, by="intervalTime")
activityData2 <- mutate(activityData2, imputedSteps = ifelse(is.na(steps), meanSteps, steps))
```

Plot histogram.

```r
activityDataByDay2 <- aggregate(activityData2$imputedSteps, 
                               by=list(date = activityData2$date), 
                               FUN=sum)
colnames(activityDataByDay2)[2] <- 'steps'

hist(activityDataByDay2$steps, main = "Histogram of the Mean (Imputed) Steps by Day", xlab='Steps')
legend("topright", c("mean", "median"), col=c("blue", "red"), lwd=2, lty=c(1,2))

meanStepsPerDay <- mean(activityDataByDay2$steps)
abline(v = meanStepsPerDay, col = "blue", lwd = 2)
text(20, x=meanStepsPerDay, pos = 2, offset = 0.1, col = "blue",
     paste('mean = ', format(meanStepsPerDay, digits = 7)))

medianStepsPerDay <- median(activityDataByDay2$steps)
abline(v = medianStepsPerDay, col = "red", lwd = 2, lty=2)
text(15, x=medianStepsPerDay, pos = 4, offset = 0.1, col = "red",
     paste('median = ', format(medianStepsPerDay, digits = 7)))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

The mean is 10766.19 and the median is 10766.19. The main difference between the Imputed data and the data with "NA" omitted is that mean and median for the Imputed data is the same.  

## Are there differences in activity patterns between weekdays and weekends?

```r
isWeekday <- function(d) {
  day <- weekdays(as.Date(d, format="%Y-%m-%d"))

  if (day == "Saturday" || day == "Sunday") {
    return("weekend")
  } else {
    return("weekday")
  }
}

activityData2$dayType = sapply(activityData$date, isWeekday)

library(lattice) 

xaxis <- activityData2$intervalTime[seq(1, length(activityData2$intervalTime), 2110)]
xaxisFormatted <- sprintf("%s", format(as.POSIXct(xaxis, 
                                            origin = ORIGIN, tz = TZ), "%H:%M"))
                          
scales <-list(
     x = list(
            at = xaxis,
            labels = xaxisFormatted))                            

xyplot(activityData2$imputedSteps~activityData2$intervalTime|activityData2$dayType, 
   ylab="Number of Steps", xlab="Interval", type="l", layout = c(1,2)
   ,
   scales = scales
   )
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 
