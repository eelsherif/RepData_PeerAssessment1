# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if(!file.exists("application.zip")) {
        temp <- tempfile()
        download.file(url,temp)
        unzip(temp)
        unlink(temp)
}

activity <- read.csv("./activity.csv")
```

## What is mean total number of steps taken per day?

```r
total_steps <- aggregate(steps ~ date, activity, sum)
hist(total_steps$steps, main = paste("Total Steps Per Day"), col="red", xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
steps_mean <- mean(total_steps$steps)
steps_media <- median(total_steps$steps)
```


## What is the average daily activity pattern?


```r
total_steps_by_interval <- aggregate(steps ~ interval, activity, mean)
with(total_steps_by_interval, plot(total_steps_by_interval$interval, total_steps_by_interval$steps, main = "Average Number of Steps per Day by Interval", type = "l", xlab="Interval", ylab="Steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
most_average <- total_steps_by_interval[which.max(total_steps_by_interval[,2]),1]
```

## Imputing missing values


```r
missing_steps <- sum(!complete.cases(activity))
```

## Are there differences in activity patterns between weekdays and weekends?
