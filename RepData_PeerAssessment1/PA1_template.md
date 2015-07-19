---
title: "PA1_template"
output: html_document
---


```r
library (dplyr)
library (lattice)
```


## Loading and preprocessing the data

```r
# Load dataset and convert format for date.
activity <- read.csv("activity.csv", header = T, sep = ",")
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

```r
# Calc total steps per day and plot histogram. NA values are to be ignored.
steps <- tapply(activity$steps, activity$date, sum, na.rm=T)
hist(steps, xlab = "Total steps per day", main = "Steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
# Calc the mean and median no. of steps per day. 
mean_steps <- round(mean(steps))
median_steps <- round(median(steps))

print(c("The mean of the total number of steps taken per day is", mean_steps))
```

```
## [1] "The mean of the total number of steps taken per day is"
## [2] "9354"
```

```r
print(c("The median of the total number of steps taken per day is", median_steps))
```

```
## [1] "The median of the total number of steps taken per day is"
## [2] "10395"
```


## What is the average daily activity pattern?

```r
# Calc ave no. of steps taken in 5 min interval.
FiveMin_interval <- tapply(activity$steps, activity$interval, mean, na.rm=T)
plot(FiveMin_interval ~ unique(activity$interval), type="l", xlab = "5 min interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
print(c("The 5-minute interval with the max number of steps and its max steps are"))
```

```
## [1] "The 5-minute interval with the max number of steps and its max steps are"
```

```r
FiveMin_interval[which.max(FiveMin_interval)]
```

```
##   835 
## 206.2
```


## Imputing missing values

```r
# Check how many missing values (NA) and where they are. 
table(is.na(activity) == T)
```

```
## 
## FALSE  TRUE 
## 50400  2304
```

```r
summary (activity)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```

```r
# Create a copy of the dataset.
NewActivity <- activity

# Use the average of that interval to substitute missing value.
for (i in 1:nrow(activity)){
  if(is.na(activity$steps[i])){
    NewActivity$steps[i]<- FiveMin_interval[[as.character(activity[i, "interval"])]]
  }
}

NewSteps <- tapply(NewActivity$steps, NewActivity$date, sum, na.rm=T)
hist(NewSteps, xlab = "Total steps per day", main = "Steps per day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
mean_NewSteps <- round(mean(NewSteps))
median_NewSteps <- round(median(NewSteps))

print(c("After imputation of missing values, the mean of the total number of steps taken per day is",mean_NewSteps))
```

```
## [1] "After imputation of missing values, the mean of the total number of steps taken per day is"
## [2] "10766"
```

```r
print(c("After imputation of missing values, the median of the total number of steps taken per day is",median_NewSteps))
```

```
## [1] "After imputation of missing values, the median of the total number of steps taken per day is"
## [2] "10766"
```


## Are there differences in activity patterns between weekdays and weekends?

```r
compare <- NULL
compare <- rbind(compare, data.frame(mean = c(mean_steps, mean_NewSteps), median = c(median_steps, median_NewSteps)))
rownames(compare) <- c("with missing values", "without missing values")
print(compare)
```

```
##                         mean median
## with missing values     9354  10395
## without missing values 10766  10766
```

```r
summary (NewActivity)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 27.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355
```

```r
# Use NewActivity to check for differences in activity patterns between weekdays and weekends.
NewActivity$wkday <- ifelse(weekdays(NewActivity$date) %in% c("Saturday", "Sunday"), "wkend", "wkday")
table(NewActivity$wkday)
```

```
## 
## wkday wkend 
## 12960  4608
```

```r
DayTypeSteps <- NewActivity %>% group_by(interval, wkday) %>% 
  summarise(avg.steps = mean(steps))

xyplot(avg.steps ~ interval | wkday, data = DayTypeSteps, type = "l", 
       layout = c(1, 2))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

We can see the following from the comparison of the average steps taken on weekdays and weekends:
a. Individuals tend to take more steps on weekdays.
b. Individuals tend to start taking steps earlier. 
