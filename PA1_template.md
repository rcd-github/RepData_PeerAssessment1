# Report Course Project 1
RCD  
February 2, 2016  

In order to answer the questions posed by the instructors, we first load the data and convert it to a data table called _mon_, as shown in the code below. 
  

```r
library(data.table); library(dplyr)
mon <- fread("activity.csv") [, date := as.Date(date, format="%m/%d/%Y")] [, interval := as.character(interval)]

# create a an interval2 and a time column in POSIXct format
mon[, interval2 := interval]
for (i in 1:dim(mon)[1]){
  if (nchar(mon$interval[i])==1) {
  mon$interval2[i] <- paste0("000", mon$interval[i])
  }
  if (nchar(mon$interval[i])==2) {
  mon$interval2[i] <- paste0("00", mon$interval[i])
  }
  if (nchar(mon$interval[i])==3) {
  mon$interval2[i] <- paste0("0", mon$interval[i])
  }
}
mon[, time := as.POSIXct(strptime(mon$interval2, format="%H%M"))] 
```

Now, we will use this data to answer the questions posed by the assignment.

### 1 - What is the Mean Total Number of Steps Taken Per Day?


The first chart presents a histogram with the total number of steps taken per day, as well as the mean and the median of this variable. 

<img src="PA1_template_files/figure-html/Code Question 1-1.png" title="" alt="" style="display: block; margin: auto;" />
  
Note that the mean and the median overlap in the plot, as their values are almost the same: **the median total number of daily steps is 10765** and **the mean total number of daily steps is 10766.2.** The code generating these results (embedded in the .Rmd file) is: `median(tot.daily.steps$V1)` and ` as.character(round(mean(tot.daily.steps$V1), digits=1))`, respectively.
  
The code used to calculate the mean number of steps per day and to subsequently elaborate the histogram is the following:



```r
# Calculate the total number of steps per day
tot.daily.steps <- mon[!is.na(steps), sum(steps), by=date] 

# Histogram of the total number of steps taken per day, including mean and median

hist(tot.daily.steps$V1, breaks="FD", freq=FALSE, col="red", xlab = "Total steps per day", ylim=c(0, 0.0002),main="Histogram of the Total Number of Daily Steps", mar=c(3,3,1,1))
rug(tot.daily.steps$V1)
lines(density(tot.daily.steps$V1, bw="nrd"), col="black", lwd=2)
abline(v=median(tot.daily.steps$V1), col="lawngreen", lwd=2, lty=1)
abline(v=mean(tot.daily.steps$V1), col="royalblue", lwd=2, lty=5)
legend("topright", legend=c("Kernel Density", "median",  "mean"), col=c("black", "lawngreen", "royalblue"), lwd=c(2,2,2), lty=c(1,5,1), bty="n", cex = 0.7)
```


### 2 - What is the Average Daily Activity Pattern?
  
The second chart presents a time series plot of the 5-minute intervals during the 24-hour day in the x-axis and the average number of steps taken, averaged across all days in the y-axis. 
<img src="PA1_template_files/figure-html/Code Question 2-1.png" title="" alt="" style="display: block; margin: auto;" />
  
As shown in the chart above, **the 5-minute interval which on average across all the days in the dataset, contains the maximum mean number of steps is 08:35**, and the mean number of steps value registered at this time is 206.2. The code generating these results (embedded in the .Rmd file) is: `format(index(int[int==max(int)]), "%H:%M")` and `round(largest[[1]], digits=1)`, respectively.
  
The code used to generate the previous chart and associated calculations is the following: 


```r
# Calculate the average number of steps per interval for all days
interval.steps <- mon[!is.na(steps), mean(steps), by=.(interval2, time)]

# convert the data into time series format
library(zoo)
int <- zooreg(interval.steps$V1, order.by = interval.steps$time)

# calculate what is the maximum number of steps per interval
largest <- max(int)

# Plot a time series with the average number of steps taken per interval vs hour of the day. Include the maximum value of the series
plot.zoo(int, type="l", col="royalblue", lwd=2, xlab = "The day divided into 5-minute intervals", ylab = "average steps taken", main="Average Steps Taken in Each 5-minute Interval", cex.main=1.1, cex.axis=0.75, xaxt="n", mar=c(2,2,1,1), mgp=c(2.5,1,0))
x <- c(seq(index(int)[1], index(int)[length(index(int))], by="4 hour"), index(int)[length(index(int))])
axis(1, at=x, labels=format(x, "%H:%M"), cex.axis=0.7)
text(index(int[int==max(int)]), largest, paste0(paste0("Max: ", round(largest[[1]], digits=1)), paste0(", at ", format(index(int[int==max(int)]), "%H:%M"))),pos=4, cex=0.8)
```


### 3 - Imputing Missing Values

The number of missing values is **2304 observations**, which equals **13.1 %** of the total number of observations.

By looking at the data, we find that missing observations correspond to whole days rather than specific time intervals withing given days. Specifically, all the missing observations are concentrated on the following 8 days: _2012-10-01, 2012-10-08, 2012-11-01, 2012-11-04, 2012-11-09, 2012-11-10, 2012-11-14, 2012-11-30_. The rest of the 53 days in the data have complete observations. 

This would suggest as a strategy to impute data to the missing days, to attribute a value that is the most representative of the time of the day to be replaced. There are different possible values that we can assign to the missing values, like the median across all days, the mean across all days, the mean (or median) across the same day of the week (e.g Monday, Tuesday, etc), among other possibilities.

Here, we will  assign the __mean__ value per five-minute interval across all days. Using the new 8 available days, to which we assigned the same daily pattern of activity, we then add up all the steps taken per day. The following chart presents a histogram with the total number of steps taken per day, as well as the mean and the median of this variable.  

<img src="PA1_template_files/figure-html/Code Question 3-1.png" title="" alt="" style="display: block; margin: auto;" />

We observe that this histogram differs from the one we drew without including the NAs (note that both are drawn to the same scale). The assignment of eight new days, each with total steps taken equal to 10766.1886792453, leads to the visible concentration of the distribution of the _total number of steps per day_ around the mean. This is evident not only from looking at the two histograms, but also by looking at the density kernels of both distributions. Note that for ease of comparison, we have added to this plot in red the kernel density of the previous histogram.

The [sample kurtosis][1] of the _total number of steps per day_ increases from 3.7308046 in the case when the NAs where ignored to 4.2939449 with the eight new observations with identical values. 

We can also compare the two cases, of the data with and without the imputed values, by looking at the following boxplot:

<img src="PA1_template_files/figure-html/Boxplot Question 3-1.png" title="" alt="" style="display: block; margin: auto;" />
  
We can see how the addition of the 8 observations reduce the inter-quantile range so as to bring five observations that before were within 1.5 times within the IQR,  outside of it.
  
The code for imputing the mean values across days to the NAs, for calculating the total number of steps including the imputed data, and for drawing the histogram is as follows:

```r
# create a new column imputing to the NAs per 5-minute interval the average across all days
mon[, imputed.steps := as.numeric(steps)]
missing.date <- as.character(unique(mon[is.na(steps), date]))
for (n in 1:length(missing.date)) {
  mon[date==missing.date[n], imputed.steps := interval.steps[, V1]]
}

# Calculate the total number of steps per day
tot.daily.steps2 <- mon[,sum(imputed.steps), by=date] 

# Histogram of the total number of steps taken per day using imputed values to the former NAs,  including mean and median
hist(tot.daily.steps2$V1, breaks="FD", freq=FALSE, col="green", xlab = "Total steps per day", ylim=c(0, 0.0002), main="Histogram of the Total Number of Daily Steps (NAs replaced)", mar=c(3,3,1,1))
rug(tot.daily.steps2$V1)
lines(density(tot.daily.steps2$V1, bw="nrd"), col="black", lwd=2)
lines(density(tot.daily.steps$V1, bw="nrd"), col="red", lwd=2)
abline(v=median(tot.daily.steps2$V1), col="lawngreen", lwd=2, lty=1)
abline(v=mean(tot.daily.steps2$V1), col="royalblue", lwd=2, lty=5)
legend("topright", legend=c("Kernel Density", "Old Kernel Density", "median",  "mean"), col=c("black", "red", "lawngreen", "royalblue"), lwd=c(2,2,2,2), lty=c(1,1,5,1), bty="n", cex = 0.7)

# Sample kurtosis formula
s.kurtosis <- function(x) {
  ((sum((x - mean(x))^4))/length(x))/(((sum((x - mean(x))^2))/length(x))^2)
}
```
  
And the code for drawing the boxplots comparing both cases is here:

```r
# Boxplot comparing the cases with and without the imputed data
boxplot(tot.daily.steps2$V1, tot.daily.steps$V1, col=c("green", "red"), names=c("With imputed data","Without imputed data"), ylab="Total steps per day", main="Boxplot Comparing Both Cases")
abline(h=mean(tot.daily.steps2$V1), col="black", lwd=2, lty=5)
```
  
### 4 - Are There Differences in Activity Patterns Between Weekdays and Weekends?

In order to answer this question, using the imputed data we first created a new factor variable in the dataset with two levels - "weekday" and "weekend", indicating whether a given date is a weekday or weekend day. Subsequently, we created the following time series plot, showing the 5-minute intervals in the x-axis and the average number of steps taken, averaged across all weekday days or weekend days, on the y-axis.
  
<img src="PA1_template_files/figure-html/Code Question 4-1.png" title="" alt="" style="display: block; margin: auto;" />

The code for generating the calculations leading to the construction of the last plot is: 
  

```r
# Create a new factor variable with two levels: weekday and weekend
for (i in 1:dim(mon)[1]) {
mon[i, wday.wend := as.factor(ifelse(weekdays(date) %in%  c("Monday","Tuesday","Wednesday","Thursday","Friday"), "weekday", "weekend"))]
 }

# calculate the mean of steps (including imputed data) grouping by interval and the new weekday-weekend variable (called here "wday.wend")
interval.steps2 <- mon[, mean(imputed.steps), by=.(interval2, time, wday.wend)]

## Make time series plot of the the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
library(ggplot2)
ggplot(interval.steps2, aes(as.numeric(interval2), V1, color=wday.wend)) + facet_grid(wday.wend ~ . ) + geom_line() +  xlab("A day divided into 5-minute intervals ") + ylab("average number of steps per interval") + ggtitle("Average Steps Taken per Interval: Weekday vs Weekend") + theme(legend.position = "none", plot.title = element_text(size=12, face="bold"))
```
  

[1]: https://en.wikipedia.org/wiki/Kurtosis 
