---
output: 
  html_document:
    keep_md: true
---

Reproducible Research: Peer Assessment 1
========================================

Author: jfs<br />
Date: 14 sep 2014





## Loading and preprocessing the data
Note: I assume that we already are in the correct working directory.

We read the data directly from the zipped file, and have a look at them:

```r
data <- read.csv(unz(description = "activity.zip", filename = "activity.csv"))
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

The data file contains three variables:

1. `steps`: Number of steps taking in a 5-minute interval. Missing values are
coded as NA.
2. `date`: The date on which the measurement was taken in YYYY-MM-DD format.
3. `interval`: Identifier for the 5-minute interval in which the measurement was
taken.

We convert the `date values to type Date (just in case).

```r
data$date <- as.Date(data$date, "%Y-%m-%d")
str(data$date)
```

```
##  Date[1:17568], format: "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
```



## What is the mean total number of steps taken per day?
We plot a histogram of the number of total steps taken each day, ignoring the
NA values:

```r
library(ggplot2)

data2 <- na.omit(data)
df <- aggregate(x = list(sumsteps = data2$steps),
                by = list(date = data2$date),
                FUN = sum)

ggplot(df, aes(x = sumsteps)) +
       geom_histogram(binwidth = 1000) +
       xlab("Total number of steps") +
       ggtitle("Total Number of Steps per Day")
```

![plot of chunk totalsteps](figure/totalsteps.png) 

Computing the mean and the median:

```r
c(mean = mean(df$sumsteps), median = median(df$sumsteps))
```

```
##   mean median 
##  10766  10765
```
(The R console returns Mean = 10766.19 and Median = 10765)



## What is the average daily activity pattern?

Let's just average the number of steps by 5-minute intervals, without taking
into account the day each interval belongs to, ignoring the NA values.

```r
library(ggplot2)

data2 <- na.omit(data)
df <- aggregate(x = list(avgsteps = data2$steps),
                by = list(interval = data2$interval),
                FUN = mean)

ggplot(df, aes(x = interval, y = avgsteps)) +
       geom_line() +
       xlab("5-minute interval") + ylab("Average number of steps") +
       ggtitle("Average Number of Steps across all Days")
```

![plot of chunk timeseries](figure/timeseries.png) 

The maximum of the mean of the number of steps, i.e., the highest peak in the
graph, is contained in a 5-minute interval:

```r
maxmean <- df[which.max(df$avgsteps), ]$interval
maxmean
```

```
## [1] 835
```
This value, 835, corresponds to a moment of the day, namely
 8:35.
The first thing that crosses the mind is that this
peak corresponds to the moment where the person goes to work, and/or practices
sport, and/or takes the children to school, and/or has a walk with the dog,
or things like that.  Actually, at the end of the day, the peak that could
correspond to the person going back home, around 1800 and 1930 (i.e., 18:00 and
19:30), is less than half that high. So we might think there is/are some
activity/ies in the morning that is/are not performed in the afternoon/evening.



## Imputing missing values
The dataset contains a certain number of missing values, i.e., rows that are not
complete cases:

```r
sum(!complete.cases(data))
```

```
## [1] 2304
```
And we can see that the NA values are present only in the `steps` column:

```r
colSums(is.na(data))
```

```
##    steps     date interval 
##     2304        0        0
```

To fill the missing data, let's just use the mean of steps for that 5-minute
interval, creating a new column:

```r
df <- aggregate(x = list(meansteps = data2$steps),
                by = list(interval = data2$interval),
                FUN = mean)

# New column named 'meansteps'
df$meansteps <- round(df$meansteps)

# Merging both dataframes by their common columns, i.e., 'interval'
data3 <- merge(data, df)

# Replace each NA 'steps' value with the corresponding 'meansteps' value
data3[is.na(data3$steps), ]$steps <- data3[is.na(data3$steps), ]$meansteps

# Drop the 'meansteps' column
data3 <- within(data3, rm("meansteps"))
```

Now we basically copy-paste the code used for the first graph in this document,
but we use **data3** as the dataset:

```r
library(ggplot2)

df <- aggregate(x = list(sumsteps = data3$steps),
                by = list(date = data3$date),
                FUN = sum)

ggplot(df, aes(x = sumsteps)) +
       geom_histogram(binwidth = 1000) +
       xlab("Total number of steps") +
       ggtitle("Total Number of Steps per Day")
```

![plot of chunk totalsteps2](figure/totalsteps2.png) 

```r
c(mean = mean(df$sumsteps), median = median(df$sumsteps))
```

```
##   mean median 
##  10766  10762
```
The values of the mean and the median are really close to those computed in the
first part of this document:

- Mean: 10766 here vs 10766 there above
- Median: 10762 here vs 10765 there above



## Are there differences in activity patterns between weekdays and weekends?
New factor variable:

```r
# Weekend days are Saturday and Sunday (days 6 and 7 of the week), all others
# are weekdays
wend <- format(data3$date, "%u") %in% c(6, 7) 
wend[wend == TRUE] <- "weekend"
wend[wend == FALSE] <- "weekday"

# Add the new column
data4 <- cbind(data3, wend)
```


```r
library(ggplot2)

df <- aggregate(x = list(meansteps = data4$steps),
                by = list(interval = data4$interval, weekend = data4$wend),
                FUN = mean)

ggplot(df, aes(x = interval, y = meansteps)) +
       geom_line() +
       facet_grid(. ~ weekend) +
       facet_wrap(~ weekend, ncol = 1) +
       xlab("Interval") + ylab("Average number of steps") +
       ggtitle("Number of Steps")
```

![plot of chunk timeseries2](figure/timeseries2.png) 

The overall activity during the weekend seems to be higher than during the
weekdays. On weekdays, the highest activity is rather concentrated in the
morning, especially between 800 and 930 (i.e., 8:30 and 9:30), but the activity
starts abruptly at 530 (5:30 in the morning) or so. The activity starts to be
low at around 1900 or 1930 (19:00 or 19:30).


On weekends, the activity is kind of more homogeneously distributed, so to say,
with the highest peaks in the morning, between 800 and 930. The start of the
activity, say between 530 and 800, is also slower than on weekdays. The activity
starts to be low at 2130 or 2200, later than on weekdays.

<hr>
