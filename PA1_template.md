---
output: html_document
---


# Reproducible Research: Peer Assessment 1


##Loading and preprocessing the data

```r
unzip("activity.zip")
data <- read.csv("activity.csv", header= TRUE, na.strings= "NA", colClasses = c("numeric", "Date", "numeric"))
```

Get a list of different days.

```r
dates <- unique(data$date)
```

Split the data by dates, ignoring NA values.

```r
splitdata <- list()
for (i in 1:length(dates)){
    splitdata[i] <- subset(data, data$date == dates[i] & data$steps != "NA")
}
```

Get values for the graph, then create the graph.

```r
total <- lapply(splitdata, sum)
plot(dates, total, main="Total number of steps per day", xlab="Dates", ylab= "Total average steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Calculate the mean and median of each day.

```r
mean <- mean(unlist(lapply(splitdata, sum)))
median <- median(unlist(lapply(splitdata, sum)))
```

The mean number of steps taken each day is 9354.23. The median number of steps are 1.0395 &times; 10<sup>4</sup>

## What is the average daily activity pattern?


Split the data by observation interval.

```r
interval <- unique(data$interval)
intervaldata <- list()
for(i in 1:length(interval)){
    intervaldata[i] <- subset(data, data$interval == interval[i]  & data$steps != "NA")
}
```

Get the mean and total, rounding the mean.

```r
intertotal <- lapply(intervaldata, sum)
intermean <- lapply(intervaldata, mean)
intermean <- lapply(intermean, round)
```

Plot the mean.

```r
plot(interval, intermean, type="l", main="Average number of steps per 5 minute interval", xlab="Interval", ylab="Mean number of steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

Get the max steps

```r
maxinterval <- interval[which.max(intermean)]
```

The maximum number of steps is taken at the 835 interval.

##Imputing missing values

To impute the missing values, I created a vector of the indexs with NA values, then I extract the intervals of those indexs. Next I created a vector of the same length that has the daily averages for those intervals. Finally I insert each value of the new vector into the location indicated by the original index.

Calculate the number of NA values.

```r
numNA <- sum(is.na(data[,1]))
```

The total number of NA values is 2304.

Get the location of the NA values.

```r
naindex <- which(is.na(data)==TRUE) 
```

Get the intervals at those locations.

```r
naint <- data$interval[naindex]
```

Build a vector of the average values at those intervals.

```r
nanewval <- numeric()
for(i in 1:length(naint)) {
    for(j in 1:length(interval)){
        if(naint[i] == interval[j]) {
            nanewval <- rbind(nanewval, intermean[[j]])
        }
    }
}
```

Insert the vector into the original data.

```r
data[naindex,1] <- nanewval
```

Split the data by dates.

```r
filledsplitdata <- list()
for (i in 1:length(dates)) {
    filledsplitdata[i] <- subset(data, data$date == dates[i]) 
}
```

Get the values for the graph, then create the graph.

```r
filledtotal <- lapply(filledsplitdata, sum)
plot(dates, filledtotal, main="Total number of steps per day with missing values added", xlab="Date", ylab="Total")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 

Calculate the mean and median.

```r
filledmean <- mean(unlist(lapply(filledsplitdata, sum)))
filledmedian <- median(unlist(lapply(filledsplitdata, sum)))
```

The mean with the NA values filled is 1.0766 &times; 10<sup>4</sup>. The median with the NA values filled is 1.0762 &times; 10<sup>4</sup>

Get the difference.

```r
mediandiff <- filledmedian-median
meandiff <- filledmean-mean
```

This results in a 1411.41 change in mean and a 367 change in median.

## Are there differences in activity patterns between weekdays and weekends?

Build a vector indicating which dates are weekends and weekdays.

```r
indicator <- character()
days <- weekdays(data$date)
key <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
for( i in 1:length(days)) {
    if(days[i] %in% key) {
        indicator <- rbind(indicator, "weekday")
    }
    else indicator <- rbind(indicator, "weekend")
}
```

Convert the vector to factors and append to the original data.

```r
factorlist <- as.factor(indicator)
data <- cbind(data, factorlist)
```

Split by weekend / weekday.

```r
daysplit <- split(data, factorlist)
```

Split the data by observation interval.

```r
weekdaydata <- list()
for(i in 1:length(interval)){
    weekdaydata[i] <- subset(daysplit[[1]], daysplit[[1]]$interval == interval[i])
}

weekenddata <- list()
for(i in 1:length(interval)){
    weekenddata[i] <- subset(daysplit[[2]], daysplit[[2]]$interval == interval[i])
}
```

Get the averages for each interval.

```r
weekdaymean <- lapply(weekdaydata, mean)
weekendmean <- lapply(weekenddata, mean)
```

Create the graph.

```r
par(mfrow=c(2,1))
plot(interval, weekdaymean, type="l", main="Mean number of steps on weekdays", xlab="Interval", ylab="Mean steps")
plot(interval, weekendmean, type="l", main="Mean number of steps on weekends", xlab="Interval", ylab="Mean steps")
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24.png) 
