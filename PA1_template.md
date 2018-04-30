---
output: 
  html_document: 
    fig_caption: yes
    keep_md: yes
---


**Introduction**

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

  1. steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

  2. date: The date on which the measurement was taken in YYYY-MM-DD format 

  3. interval: Identifier for the 5-minute interval in which measurement was taken
  
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.




```r
    activity <- read.csv("./repdata_data_activity/activity.csv")
```


=============================================================================================

*What is mean total number of steps taken per day?*


For this part of the assignment, you can ignore the missing values in the dataset.

  1. Calculate the total number of steps taken per day

  2. Make a histogram of the total number of steps taken each day

  3. Calculate and report the mean and median of the total number of steps taken per day


```r
    dailySteps <- aggregate(steps ~ date, activity, sum)
    hist(dailySteps$steps, main = paste("Total Daily Steps"), xlab="Number of Steps")
```

![](PA1_template_files/figure-html/part 1-1.png)<!-- -->

```r
    dailyStepsMean <- mean(dailySteps$steps)
    dailyStepsMedian <- median(dailySteps$steps)
```
The total daily steps mean is 1.0766189\times 10^{4} and the total daily steps median is 10765.

=============================================================================================

*What is the average daily activity pattern?*


  1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken,   averaged across all days (y-axis)

  2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
  

```r
## create dataset of average steps by interval
    avgSteps_int <- aggregate(steps ~ interval, activity, mean)
    plot(avgSteps_int$interval,avgSteps_int$steps, type="l", xlab="Interval", ylab="Number of Steps", main="Average Daily Number of Steps by Interval")
```

![](PA1_template_files/figure-html/part 2-1.png)<!-- -->

```r
    maxInterval <- avgSteps_int[which.max(avgSteps_int$steps),1]
```
  
Across all days in the dataset, the 5-minute inteval which contains the maximum number of steps is 835
  
=============================================================================================

*Imputing missing values*


1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
        ## calculate number of missing values
        nas <- nrow(activity[is.na(activity$steps),])

## create clean dataset that excludes NA data
notMissingData <- activity[!is.na(activity$steps),]

## create dataset that only includes NA data
missingData <- activity[is.na(activity$steps),]

## create dataset of average steps by interval excluding missing data
avgSteps <- aggregate(steps ~ interval, activity, mean)  

## replace NA with averages
noLongerMissingData <- merge(missingData, avgSteps, by= "interval")

## reorder, rename and merge
noLMD2 <- noLongerMissingData[,c(4,3,1)]
colnames(noLMD2) <- c("steps", "date", "interval")
completeData <- rbind(notMissingData, noLMD2)

##Test that completeData contains no nas
completedatanas <- nrow(completeData[is.na(completeData$steps),])
```
The total number of missing values is 2304

My new dataset, completedata,  contains 0  NAs


```r
        dailySteps2 <- aggregate(steps ~ date, completeData, sum)
        hist(dailySteps2$steps, main = paste("Total Daily Steps"), xlab="Number of Steps")
```

![](PA1_template_files/figure-html/histogram and new averages-1.png)<!-- -->

```r
    dailyStepsMean2 <- mean(dailySteps2$steps)
    dailyStepsMedian2 <- median(dailySteps2$steps)
```

The total daily steps mean is 1.0766189\times 10^{4} and the total daily steps median is 1.0766189\times 10^{4}.

The impact of imputing the data is that now the mean and median are the same.

=============================================================================================

*Are there differences in activity patterns between weekdays and weekends?*

For this part the weekdays()function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
        ## add day_type column to identify weekday or weekend
        completeData$day_type <- ifelse(weekdays(as.Date(completeData$date)) 
                 == "Saturday" | weekdays(as.Date(completeData$date)) == "Sunday", "weekend", "weekday")

        ## calculate average weekday steps per interval 
        weekdaySteps <- tapply(completeData[completeData$day == "weekday" ,]$steps, 
                       completeData[completeData$day == "weekday" ,]$interval, mean, na.rm = TRUE)

        ## calculate average weekend steps per interval 
        weekendSteps <- tapply(completeData[completeData$day == "weekend" ,]$steps,
                       completeData[completeData$day == "weekend" ,]$interval, mean, na.rm = TRUE)

        ## plot
        par(mfrow=c(2,1))
        plot(as.numeric(names(weekendSteps)), weekendSteps, main="Weekend", 
                ylab="Number of Steps", xlab="Interval", type = "l") 
        plot(as.numeric(names(weekdaySteps)), weekdaySteps, main="Weekday",
                ylab="Number of Steps", xlab="Interval", type = "l")
```

![](PA1_template_files/figure-html/part 4-1.png)<!-- -->
 
                
