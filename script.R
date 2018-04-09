## Fetch 

## Setup
options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)
library(chron)
library(dplyr)
library(lattice)

##data("summary", package = "ggplot2")  

# load the data
summary <- read.csv("activity.csv", colClasses = c("numeric", "character", 
                                                 "numeric"))
summary$date <- as.Date(summary$date)

##Group by Step Amounts
by_steps <- aggregate(steps ~ date, data = summary, sum, na.rm = TRUE)

step_plot <- ggplot(by_steps, aes(x=steps)) + geom_histogram(bins=7)
step_plot <- step_plot + ggtitle("Steps Histogram") + scale_y_continuous(breaks =seq(0,25,2))

print(step_plot)

##Group by Date
by_date <- aggregate(summary$steps,by=list((substr(summary$date,1,53))),sum, na.rm = TRUE)
colnames(by_date)<-c("date","steps")

mean(by_date$steps)
median(by_date$steps)

## Group by Minute Interval

interval_plot <- tapply(summary$steps, summary$interval, mean, na.rm = TRUE)

plot(row.names(interval_plot), interval_plot, type = "l", xlab = "5 Minute Interval", 
     ylab = "Average Steps", main = "Average Steps Taken by Interval", 
     col = "red")

##Prints out the name of where the max value is
names(which.max(interval_plot))

## number of NA values
sum(is.na(summary))

interval <- as.data.frame.table(interval_plot)
colnames(interval) <- c("interval", "step_mean")

summary_na_free <- merge(summary,interval,by=c("interval"))

##Replace NA steps with mean of specific 5 min interval
suppressWarnings(summary_na_free[, 2][is.na(summary_na_free[, 2])] <- summary_na_free[,4])

## New Step Histogram
by_steps_na <- aggregate(steps ~ date, data = summary_na_free, sum, na.rm = TRUE)

step_plot_na <- ggplot(by_steps_na, aes(x=steps)) + geom_histogram(bins=7)
step_plot_na <- step_plot_na + ggtitle("Steps Histogram") + scale_y_continuous(breaks =seq(0,25,2))

print(step_plot_na)

##Group by Date for NA
by_date_na <- aggregate(summary_na_free$steps,by=list((substr(summary_na_free$date,1,53))),sum, na.rm = TRUE)
colnames(by_date_na)<-c("date","steps")

mean(by_date_na$steps)
median(by_date_na$steps)

##Add column for weekday/weekend
summary_na_free$is_weekend <- factor(is.weekend(summary_na_free[, 3]))
levels(summary_na_free$is_weekend) <- list(weekend = TRUE, weekday = FALSE)

## Group by Minute Interval
weekend_plot <- na.omit(aggregate(summary_na_free$steps, 
                          list(as.numeric(summary_na_free$interval),
                          summary_na_free$is_weekend), FUN = "mean"))
colnames(weekend_plot) <- c("interval","day_type","step_avg")

print(xyplot(weekend_plot$step_avg ~ weekend_plot$interval | weekend_plot$day_type, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of Steps", col = "red"))