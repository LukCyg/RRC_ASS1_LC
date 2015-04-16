# Reproducible Research Course - Peer Assessment 1
Lukasz Cyganik  
Thursday, April 16, 2015  
I'm not a programmer...so please, be tolerant :-P

*Setting global options* 


```r
library(knitr)
library(ggplot2)
opts_chunk$set(echo = TRUE, results = 'hold')
```


*Loading data*


```r
mydata <- read.csv('activity.csv', header = TRUE, sep = ",",
                  colClasses=c("numeric", "character", "numeric"))
```

*Preprocessing:*


```r
mydata$date <- as.Date(mydata$date, format = "%Y-%m-%d")
mydata$interval <- as.factor(mydata$interval)
str(mydata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

*Calculating total number of steps per day*


```r
total_steps_per_day <- aggregate(steps ~ date, mydata, sum)
colnames(total_steps_per_day) <- c("date","steps")
head(total_steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

*Making histogram*


```r
ggplot(total_steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "orange", binwidth = 1000) + 
        labs(title="Histogram of total steps taken per Day", 
             x = "Number of steps per day", y = "number of times in a day") + theme_bw() 
```

![](PA1_LC_files/figure-html/unnamed-chunk-5-1.png) 

*Mean and median of the total number of steps taken per day*


```r
total_steps_mean   <- mean(total_steps_per_day$steps, na.rm=TRUE)
total_steps_median <- median(total_steps_per_day$steps, na.rm=TRUE)
total_steps_mean
total_steps_median
```

```
## [1] 10766.19
## [1] 10765
```

*Average daily activity pattern*


```r
steps_per_interval <- aggregate(mydata$steps, 
                                by = list(interval = mydata$interval),
                                FUN=mean, na.rm=TRUE)
steps_per_interval$interval <- 
        as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])
colnames(steps_per_interval) <- c("interval", "steps")
```

*Plot*


```r
ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
        geom_line(color="black", size=2) +  
        labs(title="Average daily activity pattern", x="interval", y="number of steps") +  
        theme_bw()
```

![](PA1_LC_files/figure-html/unnamed-chunk-8-1.png) 

*The 5-minute interval with the containing the maximum number of steps*


```r
max_interval <- steps_per_interval[which.max(  
        steps_per_interval$steps),]
max_interval
```

```
##     interval    steps
## 104      835 206.1698
```

*Imputing missing values*
*Calculating the total number of missing values in the dataset*

```r
missing_values <- sum(is.na(mydata$steps))
missing_values
```

```
## [1] 2304
```

*Filling in all of the missing values in the dataset*


```r
na_filling <- function(data, pervalue) {
        na_index <- which(is.na(mydata$steps))
        na_replace <- unlist(lapply(na_index, FUN=function(idx){
                interval = data[idx,]$interval
                pervalue[pervalue$interval == interval,]$steps
        }))
        fill_steps <- mydata$steps
        fill_steps[na_index] <- na_replace
        fill_steps
}

mydata_fill <- data.frame(  
        steps = na_filling(mydata, steps_per_interval),  
        date = mydata$date,  
        interval = mydata$interval)
str(mydata_fill)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

*Checking that there are any missing values remaining*


```r
sum(is.na(mydata_fill$steps))
```

```
## [1] 0
```

*A histogram of the total number of steps taken each day*


```r
fill_steps_per_day <- aggregate(steps ~ date, mydata_fill, sum)
colnames(fill_steps_per_day) <- c("date","steps")

ggplot(fill_steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "grey", binwidth = 1000) + 
        labs(title="Histogram of steps taken per day", 
             x = "number of steps per day", y = "number of times in a day(Count)") + theme_bw() 
```

![](PA1_LC_files/figure-html/unnamed-chunk-13-1.png) 

*The mean and median total number of steps taken per day*


```r
steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)
steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)
steps_mean_fill
steps_median_fill
```

```
## [1] 10766.19
## [1] 10766.19
```

**Do these values differ from the estimates from the first part of the assignment?**

Yes, values do differ slightly, because before filling the data the mean value was 10766.189 and the median was 10765 but after filling the data  
the mean value was 10766.19  while the median was 10766.19.

**What is the impact of imputing missing data on the estimates of the total daily number of steps?**

The mean value remains unchanged, but the median value has shifted and virtual matches to the mean.The impact of imputing missing values has increase the histogram peak, but not affect negatively the prediction.

*Differences in activity patterns between weekdays and weekends*


```r
weekdays_steps <- function(data) {
    weekdays_steps <- aggregate(mydata$steps, by=list(interval = mydata$interval),
                          FUN=mean, na.rm=T)
  
    weekdays_steps$interval <- 
            as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
    colnames(weekdays_steps) <- c("interval", "steps")
    weekdays_steps
}

data_by_weekdays <- function(data) {
    data$weekday <- 
            as.factor(weekdays(mydata$date)) # weekdays
    weekend_data <- subset(data, weekday %in% c("Saturday","Sunday"))
    weekday_data <- subset(data, !weekday %in% c("Saturday","Sunday"))

    weekend_steps <- weekdays_steps(weekend_data)
    weekday_steps <- weekdays_steps(weekday_data)

    weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
    weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))

    data_by_weekdays <- rbind(weekend_steps, weekday_steps)
    data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
    data_by_weekdays
}

data_weekdays <- data_by_weekdays(mydata_fill)
```

*Comparison*


```r
ggplot(data_weekdays, aes(x=interval, y=steps)) + 
        geom_line(color="brown") + 
        facet_wrap(~ dayofweek, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()
```

![](PA1_LC_files/figure-html/unnamed-chunk-16-1.png) 

*ANSWER*

##The activity on the weekday has the greatest peak from all steps intervals. Weekends activities has more peaks over a hundred than weekday. This is probably caused by the fact that activities during weekdays mostly follow a work related routine - there is some more intensity activity in  a free time out of work. At weekend the better distribution of effort along the time was observed.

*Thanks for Your attention!*
