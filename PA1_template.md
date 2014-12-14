---
title: "Reproducible Research Assignment 1"
output: html_document
---

First, we download and read in the the dataset. On a Linux machine we utilize wget as follow:



```r
require(RCurl)
temp <- tempfile()
mURL <- "http://github.com/dashaub/RepData_PeerAssessment1/blob/master/activity.zip?raw=true"
download.file(mURL, temp, method="wget")
df <- read.table(unz(temp, "activity.csv"), sep=",", header=T)
df$date <- as.Date(df$date)
unlink(temp)
```

We aggregate the number of steps by day and ignore missing NA values to calculate the mean and median and construct a histogram for number of steps per day.


```r
adf <- with(df, aggregate(x=steps, by=list(date), function(x) sum(x, na.rm=T)))
names(adf) <- c("Date", "Steps")
with(adf, hist(Steps, main="Histogram of Steps per Day"))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean(adf$Steps)
```

```
## [1] 9354.23
```

```r
median(adf$Steps)
```

```
## [1] 10395
```
Similarly, we exclude all entries with NA values and find the mean for each interval. This allows us to plot the average number of steps in each interval and find that the 835 interval has the highest average steps per day at 206.17.

```r
mdf <- na.omit(df)
mdf <- with(mdf, aggregate(x=steps, by=list(interval), FUN=mean))
names(mdf)<-c("Interval", "AverageStepsPerDay")
with(mdf, plot(Interval, AverageStepsPerDay, type="l", main="Average Steps on each 5 Minute Interval", ylab="Steps"))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
maxsteps <- max(mdf$AverageStepsPerDay)
with(mdf, Interval[which(AverageStepsPerDay==maxsteps)])
```

```
## [1] 835
```
The dataset has many missing values. We see that there are 2,304 missing values

```r
missing <- which(is.na(df$steps))
length(missing)
```

```
## [1] 2304
```
We will impute missing values by taking inserting the average steps for the five minute interval that is missing based on all days that we calculated above.

```r
ndf <- df
names(mdf) <- c("interval", "steps")
for(i in missing){
  ndf$steps[i] <- mdf$steps[which(mdf$interval==df$interval[i])]
}
```
We calculte the mean and median again for our new imputed dataset and once again aggregate by interval. The mean and median are now 10,766.19, an increase from their previous value ignoring NAs.

```r
newdf <- with(ndf, aggregate(x=steps, by=list(date), FUN=sum))
names(newdf) <- c("Date", "Steps")
with(newdf, hist(Steps, main="Histogram of Steps per Day"))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
mean(newdf$Steps)
```

```
## [1] 10766.19
```

```r
median(newdf$Steps)
```

```
## [1] 10766.19
```
Next, we examine the interval patterns for weekdays vs a weekedn. Using the "lubridate" package and its wday() funciton we binary code each day as a weekend or weekday

```r
require(lubridate)
ndf$weekend <- ifelse(wday(ndf$date)<7 & wday(ndf$date)>1, 0, 1)
```
With the weekends now flagged, we find the mean for each group and plot the two series.

```r
wend <- which(ndf$weekend==1)
weekendaverage <- with(ndf[wend,], aggregate(x=steps, by=list(interval),
                                            FUN=mean))
nwend <-which(ndf$weekend==0)
weekdayaverage <- with(ndf[nwend,], aggregate(x=steps, by=list(interval),
                                            FUN=mean))
fdf <- data.frame(cbind(weekendaverage, weekdayaverage[2]))
names(fdf) <- c("Interval", "Weekends", "Weekdays")
par(mfcol=c(2,1))
with(fdf, plot(Interval, Weekends, type="l", main="Steps on Weekends",
               ylab="Steps"))
with(fdf, plot(Interval, Weekdays, type="l", main="Steps on Weekdays",
               ylab="Steps"))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 


