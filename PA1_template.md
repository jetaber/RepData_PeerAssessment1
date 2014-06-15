# Reproducible Research: Peer Assessment 1




```r
library(ggplot2) # we shall use ggplot2 for plotting figures
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

print(spibydayofweek)
```

```
##     interval     steps weekday.or.weekend
## 1          0   0.12500            weekend
## 2          5   0.00000            weekend
## 3         10   0.00000            weekend
## 4         15   0.00000            weekend
## 5         20   0.00000            weekend
## 6         25   3.50000            weekend
## 7         30   0.00000            weekend
## 8         35   0.00000            weekend
## 9         40   0.00000            weekend
## 10        45   0.50000            weekend
## 11        50   0.00000            weekend
## 12        55   0.43750            weekend
## 13       100   0.00000            weekend
## 14       105   2.25000            weekend
## 15       110   0.00000            weekend
## 16       115   0.00000            weekend
## 17       120   0.00000            weekend
## 18       125   0.12500            weekend
## 19       130   0.62500            weekend
## 20       135   0.56250            weekend
## 21       140   0.00000            weekend
## 22       145   0.68750            weekend
## 23       150   0.00000            weekend
## 24       155   0.00000            weekend
## 25       200   0.00000            weekend
## 26       205   0.00000            weekend
## 27       210   0.37500            weekend
## 28       215   0.00000            weekend
## 29       220   0.00000            weekend
## 30       225   0.00000            weekend
## 31       230   0.00000            weekend
## 32       235   0.00000            weekend
## 33       240   0.00000            weekend
## 34       245   0.00000            weekend
## 35       250   0.12500            weekend
## 36       255   0.00000            weekend
## 37       300   0.00000            weekend
## 38       305   0.00000            weekend
## 39       310   0.00000            weekend
## 40       315   0.00000            weekend
## 41       320   0.68750            weekend
## 42       325   0.00000            weekend
## 43       330   2.62500            weekend
## 44       335   0.68750            weekend
## 45       340   0.62500            weekend
## 46       345   0.00000            weekend
## 47       350   0.00000            weekend
## 48       355   0.00000            weekend
## 49       400   3.75000            weekend
## 50       405   0.00000            weekend
## 51       410   3.43750            weekend
## 52       415   0.00000            weekend
## 53       420   0.00000            weekend
## 54       425   1.18750            weekend
## 55       430   6.18750            weekend
## 56       435   1.81250            weekend
## 57       440   2.62500            weekend
## 58       445   0.56250            weekend
## 59       450   5.25000            weekend
## 60       455   2.18750            weekend
## 61       500   0.00000            weekend
## 62       505   0.12500            weekend
## 63       510   0.37500            weekend
## 64       515   2.37500            weekend
## 65       520   0.75000            weekend
## 66       525   3.56250            weekend
## 67       530   0.25000            weekend
## 68       535   0.75000            weekend
## 69       540   3.68750            weekend
## 70       545   3.37500            weekend
## 71       550   8.75000            weekend
## 72       555  11.31250            weekend
## 73       600   3.87500            weekend
## 74       605   6.12500            weekend
## 75       610   7.81250            weekend
## 76       615  24.87500            weekend
## 77       620  10.56250            weekend
## 78       625  10.62500            weekend
## 79       630  11.93750            weekend
## 80       635  15.00000            weekend
## 81       640  11.25000            weekend
## 82       645  14.06250            weekend
## 83       650  10.06250            weekend
## 84       655  17.00000            weekend
## 85       700  24.62500            weekend
## 86       705  26.18750            weekend
## 87       710  18.06250            weekend
## 88       715  12.75000            weekend
## 89       720  12.75000            weekend
## 90       725  28.00000            weekend
## 91       730  25.93750            weekend
## 92       735  16.06250            weekend
## 93       740  22.81250            weekend
## 94       745  30.56250            weekend
## 95       750  29.93750            weekend
## 96       755  26.81250            weekend
## 97       800  47.06250            weekend
## 98       805  57.62500            weekend
## 99       810  88.37500            weekend
## 100      815  88.68750            weekend
## 101      820  88.37500            weekend
## 102      825  76.00000            weekend
## 103      830 116.56250            weekend
## 104      835 138.06250            weekend
## 105      840 131.18750            weekend
## 106      845 162.37500            weekend
## 107      850 161.31250            weekend
## 108      855 138.68750            weekend
## 109      900  75.31250            weekend
## 110      905 119.12500            weekend
## 111      910 151.75000            weekend
## 112      915 166.62500            weekend
## 113      920 104.12500            weekend
## 114      925 105.68750            weekend
## 115      930  87.81250            weekend
## 116      935  71.56250            weekend
## 117      940  17.18750            weekend
## 118      945  32.75000            weekend
## 119      950  23.18750            weekend
## 120      955  30.68750            weekend
## 121     1000  48.06250            weekend
## 122     1005  51.50000            weekend
## 123     1010  51.75000            weekend
## 124     1015  66.18750            weekend
## 125     1020  62.93750            weekend
## 126     1025  94.68750            weekend
## 127     1030  75.62500            weekend
## 128     1035  74.37500            weekend
## 129     1040  66.06250            weekend
## 130     1045  35.12500            weekend
## 131     1050  33.68750            weekend
## 132     1055  56.25000            weekend
## 133     1100  58.50000            weekend
## 134     1105  42.50000            weekend
## 135     1110  48.37500            weekend
## 136     1115  51.56250            weekend
## 137     1120  40.12500            weekend
## 138     1125  34.12500            weekend
## 139     1130  35.25000            weekend
## 140     1135  49.25000            weekend
## 141     1140  34.93750            weekend
## 142     1145  35.18750            weekend
## 143     1150  34.56250            weekend
## 144     1155  67.75000            weekend
## 145     1200  86.68750            weekend
## 146     1205 129.37500            weekend
## 147     1210 126.25000            weekend
## 148     1215 141.87500            weekend
## 149     1220 104.62500            weekend
## 150     1225  59.56250            weekend
## 151     1230  31.62500            weekend
## 152     1235  37.06250            weekend
## 153     1240  39.25000            weekend
## 154     1245  61.31250            weekend
## 155     1250  79.56250            weekend
## 156     1255  97.31250            weekend
## 157     1300  92.18750            weekend
## 158     1305  79.56250            weekend
## 159     1310  95.81250            weekend
## 160     1315 112.12500            weekend
## 161     1320  76.06250            weekend
## 162     1325  88.93750            weekend
## 163     1330  73.56250            weekend
## 164     1335  30.25000            weekend
## 165     1340  81.25000            weekend
## 166     1345  91.06250            weekend
## 167     1350 108.43750            weekend
## 168     1355 129.56250            weekend
## 169     1400  80.50000            weekend
## 170     1405  86.75000            weekend
## 171     1410  75.75000            weekend
## 172     1415  58.87500            weekend
## 173     1420  57.87500            weekend
## 174     1425  56.56250            weekend
## 175     1430  70.87500            weekend
## 176     1435  64.00000            weekend
## 177     1440  32.75000            weekend
## 178     1445  37.56250            weekend
## 179     1450  48.50000            weekend
## 180     1455  59.12500            weekend
## 181     1500  27.62500            weekend
## 182     1505  38.93750            weekend
## 183     1510  51.00000            weekend
## 184     1515  58.25000            weekend
## 185     1520  63.00000            weekend
## 186     1525  76.93750            weekend
## 187     1530  65.00000            weekend
## 188     1535 105.75000            weekend
## 189     1540  61.25000            weekend
## 190     1545 106.43750            weekend
## 191     1550 125.06250            weekend
## 192     1555 122.25000            weekend
## 193     1600 105.00000            weekend
## 194     1605 117.37500            weekend
## 195     1610 124.93750            weekend
## 196     1615 139.18750            weekend
## 197     1620 141.43750            weekend
## 198     1625 144.75000            weekend
## 199     1630 103.81250            weekend
## 200     1635  85.56250            weekend
## 201     1640  97.43750            weekend
## 202     1645  83.25000            weekend
## 203     1650  98.43750            weekend
## 204     1655  75.25000            weekend
## 205     1700 111.37500            weekend
## 206     1705  88.18750            weekend
## 207     1710  97.12500            weekend
## 208     1715  98.18750            weekend
## 209     1720 108.06250            weekend
## 210     1725  97.31250            weekend
## 211     1730 104.81250            weekend
## 212     1735  42.25000            weekend
## 213     1740  53.18750            weekend
## 214     1745  48.50000            weekend
## 215     1750  35.43750            weekend
## 216     1755  37.00000            weekend
## 217     1800  80.06250            weekend
## 218     1805  90.06250            weekend
## 219     1810  95.62500            weekend
## 220     1815  92.81250            weekend
## 221     1820  53.25000            weekend
## 222     1825  51.68750            weekend
## 223     1830  73.31250            weekend
## 224     1835  53.81250            weekend
## 225     1840  67.37500            weekend
## 226     1845  54.37500            weekend
## 227     1850  45.12500            weekend
## 228     1855  71.50000            weekend
## 229     1900  77.18750            weekend
## 230     1905  79.43750            weekend
## 231     1910  45.81250            weekend
## 232     1915  50.43750            weekend
## 233     1920  31.87500            weekend
## 234     1925  21.06250            weekend
## 235     1930  22.56250            weekend
## 236     1935  23.25000            weekend
## 237     1940  30.62500            weekend
## 238     1945  45.06250            weekend
## 239     1950  49.50000            weekend
## 240     1955  51.00000            weekend
## 241     2000  37.06250            weekend
## 242     2005  56.87500            weekend
## 243     2010  54.50000            weekend
## 244     2015  87.37500            weekend
## 245     2020  77.62500            weekend
## 246     2025  64.62500            weekend
## 247     2030  76.56250            weekend
## 248     2035  61.18750            weekend
## 249     2040  49.25000            weekend
## 250     2045  44.37500            weekend
## 251     2050  50.06250            weekend
## 252     2055  28.12500            weekend
## 253     2100  28.68750            weekend
## 254     2105  12.50000            weekend
## 255     2110   9.18750            weekend
## 256     2115  20.06250            weekend
## 257     2120   7.25000            weekend
## 258     2125   7.93750            weekend
## 259     2130  19.81250            weekend
## 260     2135  15.68750            weekend
## 261     2140  12.93750            weekend
## 262     2145   8.25000            weekend
## 263     2150   7.75000            weekend
## 264     2155   0.25000            weekend
## 265     2200   1.18750            weekend
## 266     2205   1.50000            weekend
## 267     2210   0.50000            weekend
## 268     2215   1.00000            weekend
## 269     2220   0.87500            weekend
## 270     2225   2.56250            weekend
## 271     2230   1.12500            weekend
## 272     2235   0.25000            weekend
## 273     2240   1.06250            weekend
## 274     2245   0.00000            weekend
## 275     2250   0.68750            weekend
## 276     2255  11.81250            weekend
## 277     2300   2.56250            weekend
## 278     2305   0.25000            weekend
## 279     2310   0.00000            weekend
## 280     2315   0.00000            weekend
## 281     2320   0.00000            weekend
## 282     2325   0.68750            weekend
## 283     2330   1.31250            weekend
## 284     2335  11.50000            weekend
## 285     2340   6.25000            weekend
## 286     2345   1.62500            weekend
## 287     2350   0.00000            weekend
## 288     2355   0.12500            weekend
## 289        0   2.15556            weekday
## 290        5   0.40000            weekday
## 291       10   0.15556            weekday
## 292       15   0.17778            weekday
## 293       20   0.08889            weekday
## 294       25   1.57778            weekday
## 295       30   0.62222            weekday
## 296       35   1.02222            weekday
## 297       40   0.00000            weekday
## 298       45   1.73333            weekday
## 299       50   0.35556            weekday
## 300       55   0.00000            weekday
## 301      100   0.37778            weekday
## 302      105   0.00000            weekday
## 303      110   0.17778            weekday
## 304      115   0.40000            weekday
## 305      120   0.00000            weekday
## 306      125   1.44444            weekday
## 307      130   2.11111            weekday
## 308      135   0.00000            weekday
## 309      140   0.20000            weekday
## 310      145   0.20000            weekday
## 311      150   0.31111            weekday
## 312      155   0.00000            weekday
## 313      200   0.00000            weekday
## 314      205   0.00000            weekday
## 315      210   1.37778            weekday
## 316      215   0.00000            weekday
## 317      220   0.00000            weekday
## 318      225   0.15556            weekday
## 319      230   0.00000            weekday
## 320      235   0.26667            weekday
## 321      240   0.00000            weekday
## 322      245   0.00000            weekday
## 323      250   1.95556            weekday
## 324      255   1.11111            weekday
## 325      300   0.00000            weekday
## 326      305   0.00000            weekday
## 327      310   0.00000            weekday
## 328      315   0.00000            weekday
## 329      320   0.00000            weekday
## 330      325   0.73333            weekday
## 331      330   1.15556            weekday
## 332      335   0.44444            weekday
## 333      340   0.35556            weekday
## 334      345   0.08889            weekday
## 335      350   0.00000            weekday
## 336      355   0.00000            weekday
## 337      400   0.24444            weekday
## 338      405   1.11111            weekday
## 339      410   2.15556            weekday
## 340      415   0.00000            weekday
## 341      420   0.40000            weekday
## 342      425   0.00000            weekday
## 343      430   3.35556            weekday
## 344      435   0.13333            weekday
## 345      440   3.71111            weekday
## 346      445   0.77778            weekday
## 347      450   2.33333            weekday
## 348      455   0.71111            weekday
## 349      500   0.00000            weekday
## 350      505   1.97778            weekday
## 351      510   3.93333            weekday
## 352      515   2.15556            weekday
## 353      520   4.17778            weekday
## 354      525   2.57778            weekday
## 355      530   2.73333            weekday
## 356      535   7.93333            weekday
## 357      540  20.40000            weekday
## 358      545  23.60000            weekday
## 359      550  50.28889            weekday
## 360      555  56.20000            weekday
## 361      600  41.22222            weekday
## 362      605  64.55556            weekday
## 363      610  69.97778            weekday
## 364      615  77.08889            weekday
## 365      620  63.80000            weekday
## 366      625  60.02222            weekday
## 367      630  66.42222            weekday
## 368      635  47.93333            weekday
## 369      640  55.66667            weekday
## 370      645  54.84444            weekday
## 371      650  47.00000            weekday
## 372      655  60.42222            weekday
## 373      700  50.48889            weekday
## 374      705  50.77778            weekday
## 375      710  61.95556            weekday
## 376      715  69.26667            weekday
## 377      720  62.97778            weekday
## 378      725  58.97778            weekday
## 379      730  66.13333            weekday
## 380      735  54.31111            weekday
## 381      740  62.68889            weekday
## 382      745  83.31111            weekday
## 383      750  67.62222            weekday
## 384      755  66.55556            weekday
## 385      800  82.66667            weekday
## 386      805  71.93333            weekday
## 387      810 143.95556            weekday
## 388      815 181.91111            weekday
## 389      820 200.55556            weekday
## 390      825 183.55556            weekday
## 391      830 198.84444            weekday
## 392      835 230.35556            weekday
## 393      840 218.77778            weekday
## 394      845 185.57778            weekday
## 395      850 191.17778            weekday
## 396      855 177.08889            weekday
## 397      900 167.60000            weekday
## 398      905 125.77778            weekday
## 399      910  93.93333            weekday
## 400      915  87.28889            weekday
## 401      920 103.44444            weekday
## 402      925  92.33333            weekday
## 403      930  58.48889            weekday
## 404      935  35.82222            weekday
## 405      940  27.35556            weekday
## 406      945  40.75556            weekday
## 407      950  39.00000            weekday
## 408      955  17.62222            weekday
## 409     1000  37.80000            weekday
## 410     1005  18.08889            weekday
## 411     1010  39.02222            weekday
## 412     1015  47.73333            weekday
## 413     1020  30.22222            weekday
## 414     1025  35.04444            weekday
## 415     1030  33.08889            weekday
## 416     1035  24.20000            weekday
## 417     1040  23.42222            weekday
## 418     1045  25.86667            weekday
## 419     1050  22.02222            weekday
## 420     1055  23.13333            weekday
## 421     1100  21.64444            weekday
## 422     1105  25.00000            weekday
## 423     1110  11.64444            weekday
## 424     1115  16.20000            weekday
## 425     1120  24.13333            weekday
## 426     1125  23.66667            weekday
## 427     1130  32.71111            weekday
## 428     1135  50.06667            weekday
## 429     1140  44.55556            weekday
## 430     1145  47.84444            weekday
## 431     1150  50.11111            weekday
## 432     1155  56.11111            weekday
## 433     1200  55.60000            weekday
## 434     1205  72.75556            weekday
## 435     1210  83.53333            weekday
## 436     1215  75.17778            weekday
## 437     1220  48.66667            weekday
## 438     1225  46.80000            weekday
## 439     1230  62.51111            weekday
## 440     1235  30.68889            weekday
## 441     1240  21.91111            weekday
## 442     1245  29.22222            weekday
## 443     1250  32.77778            weekday
## 444     1255  56.55556            weekday
## 445     1300  24.55556            weekday
## 446     1305  25.62222            weekday
## 447     1310  24.53333            weekday
## 448     1315  15.51111            weekday
## 449     1320  35.60000            weekday
## 450     1325  44.80000            weekday
## 451     1330  31.66667            weekday
## 452     1335  23.28889            weekday
## 453     1340  25.11111            weekday
## 454     1345  40.11111            weekday
## 455     1350  25.53333            weekday
## 456     1355  36.22222            weekday
## 457     1400  46.82222            weekday
## 458     1405  39.42222            weekday
## 459     1410  32.04444            weekday
## 460     1415  44.95556            weekday
## 461     1420  27.42222            weekday
## 462     1425  30.68889            weekday
## 463     1430  31.37778            weekday
## 464     1435  14.44444            weekday
## 465     1440  11.53333            weekday
## 466     1445  21.97778            weekday
## 467     1450  41.77778            weekday
## 468     1455  38.17778            weekday
## 469     1500  30.86667            weekday
## 470     1505  35.04444            weekday
## 471     1510  29.88889            weekday
## 472     1515  31.80000            weekday
## 473     1520  39.73333            weekday
## 474     1525  37.24444            weekday
## 475     1530  42.11111            weekday
## 476     1535  50.88889            weekday
## 477     1540  90.44444            weekday
## 478     1545  95.77778            weekday
## 479     1550  93.93333            weekday
## 480     1555  70.17778            weekday
## 481     1600  46.86667            weekday
## 482     1605  45.17778            weekday
## 483     1610  56.53333            weekday
## 484     1615  36.11111            weekday
## 485     1620  26.68889            weekday
## 486     1625  29.42222            weekday
## 487     1630  22.40000            weekday
## 488     1635  21.75556            weekday
## 489     1640  25.77778            weekday
## 490     1645  31.93333            weekday
## 491     1650  27.60000            weekday
## 492     1655  32.33333            weekday
## 493     1700  23.48889            weekday
## 494     1705  44.91111            weekday
## 495     1710  34.08889            weekday
## 496     1715  48.04444            weekday
## 497     1720  60.02222            weekday
## 498     1725  72.24444            weekday
## 499     1730  56.02222            weekday
## 500     1735  65.73333            weekday
## 501     1740  82.86667            weekday
## 502     1745  59.26667            weekday
## 503     1750  34.40000            weekday
## 504     1755  37.53333            weekday
## 505     1800  26.55556            weekday
## 506     1805  46.62222            weekday
## 507     1810  67.13333            weekday
## 508     1815  82.60000            weekday
## 509     1820  61.35556            weekday
## 510     1825  73.35556            weekday
## 511     1830  79.13333            weekday
## 512     1835  81.46667            weekday
## 513     1840  91.66667            weekday
## 514     1845 115.40000            weekday
## 515     1850 101.22222            weekday
## 516     1855  90.51111            weekday
## 517     1900  87.44444            weekday
## 518     1905  77.11111            weekday
## 519     1910  62.37778            weekday
## 520     1915  54.33333            weekday
## 521     1920  37.84444            weekday
## 522     1925  20.46667            weekday
## 523     1930  29.04444            weekday
## 524     1935  45.97778            weekday
## 525     1940  30.02222            weekday
## 526     1945  18.51111            weekday
## 527     1950  44.17778            weekday
## 528     1955  27.22222            weekday
## 529     2000  13.31111            weekday
## 530     2005   5.55556            weekday
## 531     2010   6.77778            weekday
## 532     2015  14.06667            weekday
## 533     2020   8.60000            weekday
## 534     2025   5.68889            weekday
## 535     2030   9.73333            weekday
## 536     2035   7.11111            weekday
## 537     2040   8.88889            weekday
## 538     2045  13.06667            weekday
## 539     2050  25.93333            weekday
## 540     2055  17.28889            weekday
## 541     2100  11.24444            weekday
## 542     2105  18.86667            weekday
## 543     2110  28.44444            weekday
## 544     2115  18.91111            weekday
## 545     2120  14.22222            weekday
## 546     2125   8.04444            weekday
## 547     2130  12.71111            weekday
## 548     2135  16.46667            weekday
## 549     2140   7.04444            weekday
## 550     2145   7.48889            weekday
## 551     2150   8.24444            weekday
## 552     2155   3.35556            weekday
## 553     2200   1.46667            weekday
## 554     2205   4.33333            weekday
## 555     2210   6.20000            weekday
## 556     2215  11.08889            weekday
## 557     2220   9.26667            weekday
## 558     2225  10.75556            weekday
## 559     2230  12.68889            weekday
## 560     2235   2.86667            weekday
## 561     2240   0.00000            weekday
## 562     2245   0.13333            weekday
## 563     2250   1.82222            weekday
## 564     2255   1.93333            weekday
## 565     2300   3.51111            weekday
## 566     2305   3.62222            weekday
## 567     2310   0.00000            weekday
## 568     2315   0.97778            weekday
## 569     2320   1.13333            weekday
## 570     2325   1.80000            weekday
## 571     2330   2.95556            weekday
## 572     2335   2.15556            weekday
## 573     2340   2.20000            weekday
## 574     2345   0.17778            weekday
## 575     2350   0.26667            weekday
## 576     2355   1.40000            weekday
```

```r
xyplot(steps ~ interval | factor(weekday.or.weekend), data = spibydayofweek, layout=c(1,2), ylab="Interval", xlab="Steps",type="l")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
#plot(spiweekend$interval, spiweekend$steps, ylab="average number of steps", xlab ="5-minute #interval", type = "n", main="Average Daily Activity Pattern Weekend")
#lines(spiweekend$interval, spiweekend$steps, type="l", col="black")

#plot(spiweekday$interval, spiweekday$steps, ylab="average number of steps", xlab ="5-minute #interval", type = "n", main="Average Daily Activity Pattern Weekday" )
#lines(spiweekday$interval, spiweekday$steps, type="l", col="black")
```


# Reproducible Research: Peer Assessment 1

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data

1. The data file is downloaded if it is not present
2. It is uncompressed and read as a data frame.
3. The interval column is converted to factor type.
4. The date column is converted to Date type.
5. The data is examined by using summary and str methods on it.


```r
library(ggplot2) # we shall use ggplot2 for plotting figures

# download and read the data, convert columns for convenience
read_data <- function() {
    fname = "activity.zip"
    source_url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    if(!file.exists(fname)) {
        download.file(source_url, destfile=fname, method="curl")
    }
    con <- unz(fname, "activity.csv")
    tbl <- read.csv(con, header=T, colClasses=c("numeric", "character", "numeric"))
    tbl$interval <- factor(tbl$interval)
    tbl$date <- as.Date(tbl$date, format="%Y-%m-%d")
    tbl
}
tbl <- read_data()
```

Examine data.

```r
summary(tbl)
```

```
##      steps            date               interval    
##  Min.   :  0.0   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   5      :   61  
##  Median :  0.0   Median :2012-10-31   10     :   61  
##  Mean   : 37.4   Mean   :2012-10-31   15     :   61  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   20     :   61  
##  Max.   :806.0   Max.   :2012-11-30   25     :   61  
##  NA's   :2304                         (Other):17202
```

```r
str(tbl)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

## What is mean total number of steps taken per day?

Below is a histogram of the daily total number of steps taken, plotted with a bin interval of 1500 steps. Also marked on the plot are the mean and median of the daily total steps.


```r
calc_steps_per_day <- function(tbl) {
    steps_per_day <- aggregate(steps ~ date, tbl, sum)
    colnames(steps_per_day) <- c("date", "steps")
    steps_per_day
}

plot_steps_per_day <- function(steps_per_day, mean_steps, median_steps) {
    col_labels=c(paste("Mean:", mean_steps), paste("Median:", median_steps))
    cols = c("green", "yellow")
    
    ggplot(steps_per_day, aes(x=steps)) + 
        geom_histogram(fill="steelblue", binwidth=1500) + 
        geom_point(aes(x=mean_steps, y=0, color="green"), size=4, shape=15) + 
        geom_point(aes(x=median_steps, y=0, color="yellow"), size=4, shape=15) + 
        scale_color_manual(name=element_blank(), labels=col_labels, values=cols) + 
        labs(title="Histogram of Steps Taken per Day", x="Number of Steps", y="Count") + 
        theme_bw() + theme(legend.position = "bottom")    
}

steps_per_day <- calc_steps_per_day(tbl)
mean_steps = round(mean(steps_per_day$steps), 2)
median_steps = round(median(steps_per_day$steps), 2)
plot_steps_per_day(steps_per_day, mean_steps, median_steps)
```

![plot of chunk steps_per_day](figure/steps_per_day.png) 

**For the total number of steps taken per day:**  
- **Mean: 10766.19**
- **Median: 10765**


## What is the average daily activity pattern?

Below is a plot of the average daily pattern of the number of steps plotted against the interval number. The interval that clocks the maximum number of steps on the average is also marked.



```r
calc_steps_per_interval <- function(tbl) {
    steps_pi <- aggregate(tbl$steps, by=list(interval=tbl$interval),
                          FUN=mean, na.rm=T)
    # convert to integers for plotting
    steps_pi$interval <- as.integer(levels(steps_pi$interval)[steps_pi$interval])
    colnames(steps_pi) <- c("interval", "steps")
    steps_pi
}

plot_activity_pattern <- function(steps_per_interval, max_step_interval) {
    col_labels=c(paste("Interval with Maximum Activity: ", max_step_interval))
    cols = c("red")
    
    ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
        geom_line(color="steelblue", size=1) +  
        geom_point(aes(x=max_step_interval, y=0, color="red"), size=4, shape=15) +  
        scale_color_manual(name=element_blank(), labels=col_labels, values=cols) +     
        labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
        theme_bw() + theme(legend.position = "bottom")
}

steps_per_interval <- calc_steps_per_interval(tbl)
max_step_interval <- steps_per_interval[which.max(steps_per_interval$steps),]$interval

plot_activity_pattern(steps_per_interval, max_step_interval)
```

![plot of chunk steps_per_interval](figure/steps_per_interval.png) 

The **835<sup>th</sup> interval** has the maximum activity on the average.

## Imputing missing values

To populate missing values, we choose to replace them with the mean value at the same interval across days. The choice is based on the assumption that activities usually follow a daily pattern.



```r
impute_means <- function(tbl, defaults) {
    na_indices <- which(is.na(tbl$steps))
    defaults <- steps_per_interval
    na_replacements <- unlist(lapply(na_indices, FUN=function(idx){
        interval = tbl[idx,]$interval
        defaults[defaults$interval == interval,]$steps
        }))
    imp_steps <- tbl$steps
    imp_steps[na_indices] <- na_replacements
    imp_steps
}
complete_tbl <- data.frame(  
    steps = impute_means(tbl, steps_per_interval),  
    date = tbl$date,  
    interval = tbl$interval)
```

Summarizing the new dataset with imputed values:

```r
summary(complete_tbl)
```

```
##      steps            date               interval    
##  Min.   :  0.0   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   5      :   61  
##  Median :  0.0   Median :2012-10-31   10     :   61  
##  Mean   : 37.4   Mean   :2012-10-31   15     :   61  
##  3rd Qu.: 27.0   3rd Qu.:2012-11-15   20     :   61  
##  Max.   :806.0   Max.   :2012-11-30   25     :   61  
##                                       (Other):17202
```

With the imputed dataset, below is a histogram of the daily total number of steps taken, plotted with a bin interval of 1500 steps. Also marked on the plot are the mean and median of the daily total steps.


```r
complete_steps_per_day <- calc_steps_per_day(complete_tbl)
complete_mean_steps = round(mean(complete_steps_per_day$steps), 2)
complete_median_steps = round(median(complete_steps_per_day$steps), 2)
plot_steps_per_day(complete_steps_per_day, complete_mean_steps, complete_median_steps)
```

![plot of chunk complete_steps_per_day](figure/complete_steps_per_day.png) 

Comparing with the calculations done in the first section of this document, we observe that while the mean value remains unchanghed, the median value has shifted closer to the mean.

## Are there differences in activity patterns between weekdays and weekends?

We do this comparison with the table with filled-in missing values.

1. Augment the table with a column that indicates the day of the week
2. Subset the table into two parts - weekends (Saturday and Sunday) and weekdays (Monday through Friday).
3. Tabulate the average steps per interval for each dataset.
4. Plot the two datasets side by side for comparison.


```r
calc_day_of_week_data <- function(tbl) {
    tbl$weekday <- as.factor(weekdays(tbl$date))
    weekend_data <- subset(tbl, weekday %in% c("Saturday","Sunday"))
    weekday_data <- subset(tbl, !weekday %in% c("Saturday","Sunday"))
    
    weekend_spi <- calc_steps_per_interval(weekend_data)
    weekday_spi <- calc_steps_per_interval(weekday_data)
    
    weekend_spi$dayofweek <- rep("weekend", nrow(weekend_spi))
    weekday_spi$dayofweek <- rep("weekday", nrow(weekday_spi))
    
    day_of_week_data <- rbind(weekend_spi, weekday_spi)
    day_of_week_data$dayofweek <- as.factor(day_of_week_data$dayofweek)
    day_of_week_data
}
plot_day_of_week_comparison <- function(dow_data) {
    ggplot(dow_data, 
        aes(x=interval, y=steps)) + 
        geom_line(color="steelblue", size=1) + 
        facet_wrap(~ dayofweek, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()
}
day_of_week_data <- calc_day_of_week_data(complete_tbl)
plot_day_of_week_comparison(day_of_week_data)
```

![plot of chunk weekday_compare](figure/weekday_compare.png) 

We observe that activity on the weekends tends to be more spread out over the day compared to the weekdays. This could be due to the fact that activities on weekdays mostly follow a work related routine, whereas weekends tend to be more adhoc.

