# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(dplyr)
library(ggplot2)
```


```r
if (!file.exists("data/activity.csv")) {
    unzip("activity.zip", exdir = "data")    
}
data <- read.csv("data/activity.csv")
datadt <- tbl_df(data)
datadt$date <- as.Date(datadt$date)
datadt$steps <- as.numeric(datadt$steps)
datadt$interval <- as.numeric(datadt$interval)
```



## What is mean total number of steps taken per day?

```r
grouppedByDate <- group_by(datadt, date)
stepsByDate <- summarise(grouppedByDate, steps = sum(steps, na.rm = TRUE))
stepsByDate$date <- as.Date(stepsByDate$date)
plot(stepsByDate$date, stepsByDate$steps, type = "h", xlab = "Date", ylab = "Steps", main = "Total steps by date")
```

![](PA1_template_files/figure-html/mean_values-1.png) 

```r
print(paste("Steps mean: ", mean(stepsByDate$steps, na.rm = TRUE)))
```

```
## [1] "Steps mean:  9354.22950819672"
```

```r
print(paste("Steps median: ", median(stepsByDate$steps, na.rm = TRUE)))
```

```
## [1] "Steps median:  10395"
```



## What is the average daily activity pattern?


```r
averagedData <- summarise(group_by(datadt, interval), steps = mean(steps, na.rm = TRUE))
plot(x = averagedData$interval, y = averagedData$steps, type = "l", xlab="5-minute intervals", ylab="Steps", main="Average daily activity pattern")
```

![](figure/daily_pattern-1.png) 

```r
maximumAveInterval <- averagedData[order(averagedData$steps, decreasing = TRUE), ][[1, "interval"]]
```



**Maximum steps interval is: 835**


## Imputing missing values


```r
# 1. Missing vals
missingRows <- filter(datadt, is.na(steps))
print(paste("Missing rows: ", count(missingRows)$n))
```

```
## [1] "Missing rows:  2304"
```

```r
# 2. Devise a stratagy for filling NA steps
fixSteps <- function (steps, interv) { 
    if (is.na(steps)) { 
        round(filter(averagedData, interval == interv)$steps)
    } else { 
        steps 
    }  
};

# 3. Fill the missing values
filledDatadt <- 
    tbl_dt(datadt) %>% 
    rowwise() %>% 
    mutate(steps = fixSteps(steps, interval))
```

```
## Loading required namespace: data.table
```

```r
# 4. Make a histogram and calculate means
grouppedByDate <- group_by(filledDatadt, date)
stepsByDate <- summarise(grouppedByDate, steps = sum(steps))
plot(stepsByDate$date, stepsByDate$steps, type = "h", xlab = "Date", ylab = "Steps", main = "Total steps by date (filled by mean interval value across days)")
```

![](PA1_template_files/figure-html/missing_vals-1.png) 

```r
print(paste("Steps mean: ", mean(stepsByDate$steps)))
```

```
## [1] "Steps mean:  10765.6393442623"
```

```r
print(paste("Steps median: ", median(stepsByDate$steps)))
```

```
## [1] "Steps median:  10762"
```

**Wee can see that imputing the NA values lead to increasing the mean and median.**


## Are there differences in activity patterns between weekdays and weekends?


```r
# Function to mark day type for rows
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
mark.weekday <- function (rowDate) {
    dayName <- weekdays(rowDate)
    if (dayName == "Sat" || dayName == "Sun") { "weekend" } else { "weekday" }
}

# Add new field
filledDatadt <- 
    filledDatadt %>% 
    rowwise() %>% 
    mutate(dayType = mark.weekday(date))


filledDatadt$dayType <- factor(filledDatadt$dayType)

# Draw a plot
averagedData <- summarise(group_by(filledDatadt, dayType, interval), steps = mean(steps, na.rm = TRUE))
```

```
## Warning: Grouping rowwise data frame strips rowwise nature
```

```r
qplot(x = averagedData$interval, y = averagedData$steps, xlab="5-minute intervals", ylab="Steps", main="Average daily activity pattern",geom = "path", data = averagedData, facets =  dayType ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

