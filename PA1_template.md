# Reproducible Research: Peer Assessment 1




```r
library(lattice)
```

## Loading and preprocessing the data

### fetch data function
1. Determine if compressed file is present, if not load (assumes file name is activity.zip) and log 
2. Detemine if uncompressed file is present, if not, decompress (assumes file name is activity.csv)


```r
compressedfilename <- "activity.zip"
uncompressedfilename <- "activity.csv"
fetchdata <- function() {
   if(!file.exists(compressedfilename)) {
      url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
      download.file(source_url, destfile=compressedfilename, method="curl")
      print(c(Sys.time(), url))
   }
   if (!file.exists(uncompressedfilename)) {
      unz(uncompressedfilename, uncompressedfilename)
   }
}
```

### Preprocess function
2. Load decompressed file into a data frame
3. Convert rows in 'date' column to Date class from character


```r
preprocessdata <- function() {
   dat <- read.csv(uncompressedfilename, header=T, colClasses=c("numeric", "character", "numeric"))
   #dat$interval <- factor(dat$interval)
   dat$date <- as.Date(dat$date, format="%Y-%m-%d")
   str(dat)
   dat
}
```
###  Execute

```r
fetchdata()
dat <- preprocessdata()
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?
1.Make a histogram of the total number of steps taken each day
2.Calculate and report the mean and median total number of steps taken per day

### Histogram of Steps per Day
Originally used a bin size of 200, but that seemed to fine-grain, reduced it to 20
This plot uses data which is ignoring NA values

```r
stepsperday <- function(dat) {
    res <- aggregate(steps ~ date, dat, sum)
    colnames(res) <- c("date", "steps")
    res
}
spd <- stepsperday(dat)
hist(spd$steps,main="Histogram of Total Steps per Day (ignore na)",xlab="Steps", breaks=20)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
meanperday <- aggregate(steps ~ date, dat, mean)
medianperday <- aggregate(steps ~ date, dat, median)

meanspd = round(mean(spd$steps), 4)
medianspd = round(median(spd$steps), 4)
```

**Mean and median for total number of steps taken per day (ignoring NA values):**  
- **Mean Steps/Day: 10766.1887**
- **Median Steps/Day: 10765**

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

```r
spi <- aggregate(steps ~ interval, dat, mean)
plot(spi$interval, spi$steps, ylab="average number of steps", xlab ="5-minute interval", type = "n", main="Average Daily Activity Pattern")
lines(spi$interval, spi$steps, type="l", col="black")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
idx<-which.max( spi[,2] )
maxinterval<-spi[idx,1]
```

**For the total number of steps taken per day:**  
- **Five minute interval with average maximum steps: 835**

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The
strategy selected is the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the
missing data filled in. Must convert step back to int 

4. Make a histogram of the total number of steps taken each day and Calculate
and report the mean and median total number of steps taken per day.


```r
numNAs <- sum(rowSums(is.na(dat)))
```
**For the total number of missing 'steps' values:**  
- **Total number of missing steps values: 2304**


```r
filleddat<-dat
filleddat$steps[is.na(filleddat$steps)]<-spi$steps
filleddat$steps<-as.integer(filleddat$steps)


spd <- stepsperday(filleddat)
hist(spd$steps,main="Histogram of Total Steps per Day (replace NA with interval average)",xlab="Steps", breaks=20)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
meanspd = round(mean(spd$steps), 4)
medianspd = round(median(spd$steps), 4)
```

**Mean and median for total number of steps taken per day (replace NA with interval average):**  
- **Mean Steps/Day: 10749.7705**
- **Median Steps/Day: 10641**

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday"
and "weekend" indicating whether a given date is a weekday or weekend
day

```r
filleddat$weekday.or.weekend <- as.factor(c("weekday","weekend"))
filleddat$weekday.or.weekend[(!(weekdays(filleddat$date) %in% c('Saturday','Sunday')))]<- as.factor("weekday")
filleddat$weekday.or.weekend[((weekdays(filleddat$date) %in% c('Saturday','Sunday')))]<- as.factor("weekend")
```

Make a panel plot containing a time series plot (i.e. type = "l") of the
5-minute interval (x-axis) and the average number of steps taken, averaged
across all weekday days or weekend days (y-axis).


```r
weekenddat <-filleddat[which(filleddat$weekday.or.weekend == 'weekend'),]
spiweekend <- aggregate(steps ~ interval, weekenddat, mean)
spiweekend$weekday.or.weekend<-as.factor('weekend');

weekdaydat <-filleddat[which(filleddat$weekday.or.weekend == 'weekday'),]
spiweekday <- aggregate(steps ~ interval, weekdaydat, mean)
spiweekday$weekday.or.weekend<-as.factor('weekday')

spibydayofweek <- rbind(spiweekend,spiweekday)

xyplot(steps ~ interval | factor(weekday.or.weekend), data = spibydayofweek, layout=c(1,2), ylab="Steps", xlab="Interval",type="l")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

Comparing weekdays to weekends, the steps taken per interval seems less concentrated on weekends.  
This could be accounted for by the data being collected from individuals with an interest in fitness and therefore walking or running during their lunch hour while primarily sendentary during the rest of the working hours. Such individuals would have more opportunities to run or walk over the weekend, leading to more difuse data.  
