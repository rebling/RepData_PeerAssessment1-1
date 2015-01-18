---
title: "repdataPA1.Rmd"
author: "Richard Ebling"
date: "01/18/2015"
output: html_document
---
Assignment PA1  for repdata-010

```r
library(knitr)
library(dplyr)
```


```r
# setting global options
opts_chunk$set(echo=TRUE)
```

Load and preprocess data (file located in working directory; acquired by creating fork of assignment from Github)

```r
unzip("activity.zip", exdir="./uz")
acdat <- tbl_df(read.csv("uz/activity.csv"))
# rename vars to avoid confusion with function names
names(acdat) <- c("Steps", "Date", "Interval")
```
Mean total number of steps taken per day (histogram and narrative results): 

```r
ac_by_date <- group_by(acdat, Date)
nsteps <- summarise(ac_by_date, Tsteps=sum(Steps))

TSmean <- mean(nsteps$Tsteps, na.rm=TRUE)
TSmedian <- median(nsteps$Tsteps, na.rm=TRUE)
hist(nsteps$Tsteps, breaks=12, xlab="# of steps per day", main="Histogram #1: steps per day")
```

![plot of chunk calc_mean_med_steps](figure/calc_mean_med_steps-1.png) 


Mean total number of steps per day = 10766.19

Median total number of steps per day = 10765

```r
# summarize steps per day and compare to above 
# results
summary(nsteps$Tsteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```
#### Display average daily activity pattern:
(time series plot of 5-min intervals x avg # of steps per interval, averaged over all #days; show the interval with overall maximum average # of steps)

```r
ac_by_int  <- group_by(acdat, Interval)
Ints <- summarise(ac_by_int, avsteps=mean(Steps, na.rm=TRUE))
plot(Ints, type="l", main="Average steps per 5-min interval")
```

![plot of chunk timeseries_maxInt](figure/timeseries_maxInt-1.png) 

```r
maxInt <- max(Ints$avsteps)
MaxIntervalID <- Ints[grep(maxInt,Ints$avsteps),1]
```
The interval with ID "835" contains the maximum number of steps (206.1698).

#### Impute missing values*

```r
totalNAs <- sum(is.na(acdat$Steps))
```
1. The total number of missing values in the dataset is 2304 .  


```r
# sanitycheck: look for obvious patterns in missing values: 
# unique(acdat$Date[is.na(acdat$Steps)])
# weekdays(as.Date(unique(acdat$Date[is.na(acdat$Steps)])))
#Conclusion: no obvious patterns found. Of 8 dates with missing values, #all weekdays are represented except Tuesday; no weekday is present more #than twice.
#values for Sunday 2012-11-04 are missing, therefor no DST confounds.
```
2. Strategy for filling in missing values
  Split dataset into 'norm_' (valid data) and NA_ (rows with NA) 
  Replace NA (steps) with avg steps for that interval, 

```r
norm_acdat <- acdat[!is.na(acdat$Steps),]
NA_acdat <- acdat[is.na(acdat$Steps),]
```

```r
replaceNAs <- select(inner_join(Ints, NA_acdat), avsteps, Date, Interval)
```

```
## Joining by: "Interval"
```

```r
names(replaceNAs) <- c("Steps", "Date", "Interval")
```
3. recombine datasets via rbind, re-sort into original sequence*:

```r
newset <- arrange(rbind(norm_acdat,replaceNAs), Date, Interval)
```
4. Histogram and narrative summary, conclusions:

```r
new_by_date  <- group_by(newset, Date)
newsteps <- summarise(new_by_date, TNsteps = sum(Steps))
TNmean <- mean(newsteps$TNsteps, na.rm=TRUE)
TNmedian <- median(newsteps$TNsteps, na.rm=TRUE)
hist(newsteps$TNsteps, breaks=12, xlab="# of steps per day", main="Histogram#2: steps per day (imputed)")
```

![plot of chunk analyze+report](figure/analyze+report-1.png) 

Total number of steps per day:   
With imputed values: 
mean = 10766.19 ; median = 10766.19  

With uncorrected values: 
mean = 10766.19 ; median = 10765  
  
Impact of imputing missing values: *Mean* of steps was unchanged due to method of imputing values; median could change slightly. 

After imputing missing data, estimated *Total* daily # of steps 
increased markedly due to increase in number of intervals summed, when no longer dropping intervals with missing values. (Compare Histogram#1 and Histogram#2). 

#### Comparing weekday/weekend activity patterns
1. Create new factor variable with two levels

```r
neweek <- cbind(newset, dow=weekdays(as.POSIXlt(newset$Date)))
# using “weekday” & “weekend” to flag type of day for a given date
# substitute desired label for dow into var wk1 then wktype
wk1 <- gsub("Saturday|Sunday", "weekend", neweek$dow)
wktype <- gsub("Monday|Tuesday|Wednesday|Thursday|Friday", "weekday", wk1)

# Create dataset including new factor and update var names
newsetDays <- cbind(newset, data.frame(as.factor(wktype)))
names(newsetDays) <- c("Steps", "Date", "Interval", "Daytype")

#
ac_by_wd  <- group_by(newsetDays, Interval, Daytype)
newkdata <- summarise(ac_by_wd, newAvsteps=mean(Steps, na.rm=TRUE))
WeekdaySteps <- newkdata[newkdata$Daytype=="weekday",]
WeekendSteps <- newkdata[newkdata$Daytype=="weekend",]
```
2. Display panel plot using time series to show intervals & AVERAGE
 steps taken per interval AVERAGED across all weekdays vs weekend days.

```r
# (yeah, I know--average of averages...) 
par(mfrow=c(2,1))
plot(WeekendSteps$Interval, WeekendSteps$newAvsteps, type="l", main="weekend", ylab="steps per interval", xlab="Interval ID")
plot(WeekdaySteps$Interval, WeekdaySteps$newAvsteps, type="l", main="weekday", ylab="steps per interval", xlab="Interval ID")
```

![plot of chunk panel_plot](figure/panel_plot-1.png) 
-------
