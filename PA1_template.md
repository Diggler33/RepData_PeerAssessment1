---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

loading required packages

```r
library(ggplot2)
library(lattice)
```

The data used for this analysis can be downloaded from the following location:

* [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

This file should be unzipped and the file path within the following code changed to suit your location of the data.
This code block does the following steps:

1. **Load Data**
2. **Aggregate Data:** Total steps taken over all days

note: NAs are ignored in this section

### 1. Load data

```r
#Data Source
datafile <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'

#Download file if required
if (file.exists('activity.zip') == FALSE) {
        download.file(datafile, dest='activity.zip') }

#unzip data
unzip("activity.zip", overwrite = T, unzip="internal")

#Read csv Data into data frame
activity<-read.csv("activity.csv",header=TRUE,sep=",",stringsAsFactors=FALSE,strip.white = TRUE)
```
### Aggregate Data

```r
activity_Agg <- aggregate(steps ~ date, data = activity,sum,na.rm=TRUE)
```

## What is mean total number of steps taken per day?  

The following Histogram uses the aggregated dataset to report total steps taken for each day


```r
#Display Histogram showing frequency of Steps
hist(activity_Agg$steps, main="Frequency of Steps",xlab="Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

To determine the Mean and Median values the following code is used.

```r
#Display Mean and Median of total steps over all days
Mean <- mean(activity_Agg$steps)
Median <- median(activity_Agg$steps)
SummaryResults <- data.frame(cbind(Mean,Median))
SummaryResults
```

```
##       Mean Median
## 1 10766.19  10765
```

## What is the average daily activity pattern?  

This section calculates the mean for the steps over intervals for all days. A time plot is then displayed with these outputs.
I have used POSIXct to convert the intervals into a time format for accuracy of plotting.

```r
substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
}


#----- FORMAT DATES -------#
dFormat <- "%Y-%m-%d %H%M"
activity$Date.Time <-as.POSIXct(paste(activity$date,substrRight(paste("0000",activity$interval,sep=""),4)), format=dFormat)
activity$Time <-strftime(activity$Date.Time, format="%H:%M:%S")
        
avg.daily.pattern <- aggregate(steps ~ Time, data = activity,mean)

avg.daily.pattern$Row <- as.numeric(rownames(avg.daily.pattern))

Mean.Max <- avg.daily.pattern[avg.daily.pattern$steps == max(avg.daily.pattern$steps),]
Mean.Max$Time <- as.character(Mean.Max$Time)


ggplot(avg.daily.pattern, aes(x=as.numeric(rownames(avg.daily.pattern)), y=steps)) +
        geom_line() + scale_x_continuous(breaks=seq(1,nrow(avg.daily.pattern),by=48), labels=avg.daily.pattern$Time[seq(1,nrow(avg.daily.pattern),by=48)]) +
        geom_hline(yintercept=max(avg.daily.pattern$steps), colour="red", linetype=5) +
        geom_vline(xintercept=Mean.Max[[3]], colour="red", linetype=5) +
        geom_text(mapping=aes(0,y=Mean.Max[[2]], label = round(Mean.Max[[2]],2),hjust=-0.2, vjust=-0.5),size = 3) +
        geom_text(aes(Mean.Max[[3]],0, label = Mean.Max[[1]], hjust=-0.2),size = 3) +
        xlab("Interval") + ylab("Avg Steps Per Interval") + xlab("Interval") +
        ggtitle("Average Daily Activity Pattern") +theme_bw()
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

From this we can see, on average the 08:35 interval contains the maximum number of steps (206.17)

## Imputing missing values  

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

The following code identifies NA values within the dataset

```r
summary(activity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304                                        
##    Date.Time                       Time          
##  Min.   :2012-10-01 00:00:00   Length:17568      
##  1st Qu.:2012-10-16 05:58:45   Class :character  
##  Median :2012-10-31 11:57:30   Mode  :character  
##  Mean   :2012-10-31 11:30:52                     
##  3rd Qu.:2012-11-15 17:56:15                     
##  Max.   :2012-11-30 23:55:00                     
## 
```

We can see from this that there are 2304 NAs present in the "Steps" data.

To address this I have taken the following steps:

1. Identify Mean value for every interval over all days
2. Determine all rows where there is an NA present for the steps (Missing)
3. Determine all rows where there is not an NA value (Not Missing)
4. Merge the Mean values with the Missng dataset and replace NA with Mean
5. Bind the Missing dataset with the Not Missing dataset
6. Aggregate the new Imputed data
7. Calculate Mean and Median


```r
#Get Mean Value for each interval over all days  to impute NAs
avg.daily.pattern <- aggregate(activity$steps,list(interval = activity$interval),FUN=mean,na.rm=TRUE)
names(avg.daily.pattern) <- c("interval","mean_steps")
missing <- is.na(activity$steps)                                #Identify NAs
notmissing <- !is.na(activity$steps)                            #Identify non NAs
activity.missing <- activity[missing,]                          #Subset data for NAs

#Merge missing data with mean values
activity.missing <- merge(activity.missing,avg.daily.pattern,by.x = "interval",by.y = "interval")


#Set NAs equal to mean value
activity.missing$steps <- activity.missing$mean_steps


#Bind imputed rows with non NA rows
master <- rbind(activity.missing[,c(2,3,1,4,5)],activity[notmissing,])
master$date <-as.Date(master$date)                              #FormatDate

master <- master[order(master$date),]                           #Order data by date
master_Agg <- aggregate(steps ~ date, data = master,sum)        #Aggregate (Total) data step over date
master_Agg$date <-as.Date(master_Agg$date)                      #FormatDate

#Display Histograms for both Imputed and previous datasets
par(mfrow = c(1,2))
hist(activity_Agg$steps, main="Total Steps (NA Ignored)",xlab="Steps")
hist(master_Agg$steps, main="Total Steps (NA Imputed)",xlab="Steps") 
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
#Display Mean and Median comparisons for both Imputed and previous datasets
NA_Handling <- c("Ignored","Imputed")
Mean <- c(round(mean(activity_Agg$steps),2),round(mean(master_Agg$steps),2))
Median <- c(round(median(activity_Agg$steps),2),round(median(master_Agg$steps),2))
Summaryresults <- data.frame(cbind(NA_Handling,Mean,Median))

Summaryresults                                                  #View results
```

```
##   NA_Handling     Mean   Median
## 1     Ignored 10766.19    10765
## 2     Imputed 10766.19 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

To answer this question I first had to address the time formating issue. Interval cannot be used on the x axis as it is integer format and will not be accurate. Fir example the difference between 1155 and 1200 is 45 rather than 5 minutes. I take the following steps:

1. Create a Date time variable using interval as the time stamp
2. Create a Time variable using the strftime function

The next step was to create a factor with two levels Weekday & Weekend. This is done using the weekdays() function
Once this has been done I use the lattice package to display the outputs as a time series graph split by Weekday and Weekend



```r
#----------------------------------------------------------------------------------------------------------
## Activity patterns between weekdays and weekends
#----------------------------------------------------------------------------------------------------------

master$Date.Time <-as.POSIXct(paste(master$date,substrRight(paste("0000",master$interval,sep=""),4)), format=dFormat)
master$Time <-strftime(master$Date.Time, format="%H:%M:%S")

#activity$date <- as.Date(activity$date)
weekend <- weekdays(master$date) %in% c("Saturday","Sunday")
weekend.f <- factor(weekend, labels = c("Weekday","Weekend"))
master$Weekday <- weekend.f


Weekday_Agg <- aggregate(steps ~ Time + Weekday, data = master,mean)
Weekday_Agg$steps <- round(Weekday_Agg$steps,4)

Weekday_Agg$Time <- factor(Weekday_Agg$Time)

at<- seq(1,nrow(Weekday_Agg),by=48)
labels <-  Weekday_Agg$Time[seq(1,nrow(Weekday_Agg),by=48)]



xyplot(steps ~ Time|Weekday,data=Weekday_Agg,type = "l",scales=list(x=list(at=at,labels=labels)),layout= c(1,2))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
#----------------------------------------------------------------------------------------------------------
```

We can see from this that the main differences appear to be 

1. Weekday activity greater than Weekend in the 8am time region
2. Greater midday activity during the Weekend
3. Weekend activity greater than the Weekday in the 8pm time region
