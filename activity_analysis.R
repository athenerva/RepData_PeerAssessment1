
## 1. Loading and preprocessing the data
activity <- read.csv("activity.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE)
activity$date <- as.Date(activity$date, "%Y-%m-%d")

## 2. What is mean total number of steps taken per day?
stepsperday <- tapply(activity$steps, activity$date, sum, na.rm = TRUE)
hist(stepsperday, xlab = "Steps per day", ylab = "Count", main = "Histogram of steps per day", col = "blue")
mean(stepsperday)
median(stepsperday)

## 3. What is the average daily activity pattern?
avgPerInt <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(unique(activity$interval), avgPerInt, type = "l", xlab = "Interval", ylab = "Average number of steps", main = "Average Daily Activity Pattern")
maxAvgInt <- activity$interval[which.max(avgPerInt)]
maxAvgInt

## 4. Impute missing values by assigning the mean steps of the interval
na_count <- sum(is.na(activity$steps))
len <- length(activity$steps)
activity_new <- activity

for(i in 1:len) {
        s <- activity_new$steps[i]
        if(is.na(s)) {
                c <- as.character(activity_new$interval[i])
                activity_new$steps[i] <- avgPerInt[c]
        }
}

## Create new histogram for total steps per day with missing values imputed
spd2 <- tapply(activity_new$steps, activity_new$date, sum)
hist(spd2, xlab = "Steps per day", ylab = "Count", main = "Histogram of steps per day", col = "green")
mean(spd2)
median(spd2)

## 5. Are there differences in activity patterns between weekdays and weekends?
wd <- weekdays(activity_new$date)
wd_factor <- wd
wd_factor <- gsub("Saturday|Sunday", "weekend", wd_factor)
wd_factor <- gsub(".*day", "weekday", wd_factor)
activity_new$dayOfWeek <- as.factor(wd_factor)

## Plot the weekday-weekend two-panel graph
library(ggplot2)

wday <- activity_new[activity_new$dayOfWeek == "weekday", ]
wend <- activity_new[activity_new$dayOfWeek == "weekend", ]

wd_avg <- tapply(wday$steps, wday$interval, mean)
we_avg <- tapply(wend$steps, wend$interval, mean)

wdInt <- data.frame(interval = unique(wday$interval), avg_steps = wd_avg, dayOfWeek = rep("weekday", length(wd_avg)))
weInt <- data.frame(interval = unique(wend$interval), avg_steps = we_avg, dayOfWeek = rep("weekend", length(we_avg)))

intAvg <- rbind(wdInt, weInt)

qplot(interval, avg_steps, data = intAvg, geom="line", xlab = "Interval", ylab = "Average number of steps") + facet_wrap(~dayOfWeek, ncol=1)
