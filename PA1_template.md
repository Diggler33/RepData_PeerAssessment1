# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

The data used for this analysis can be downloaded from the following location:

* [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

This file should be unzipped and the file path within the following code changed to suit your location of the data.
This code block does the following steps:

1. **Load Data**
2. **Aggregate Data:** Total steps taken over all days

note: NAs are ignored in this section


```r
activity <- read.csv("C:/Users/ben33_000/Documents/DataScientistTraining/ReproducibleResearch/Assignment01/RepData_PeerAssessment1/activity.csv",header=TRUE,sep=",",stringsAsFactors=FALSE,strip.white = TRUE)
activity_Agg <- aggregate(steps ~ date, data = activity,sum,na.rm=TRUE)
```


## What is mean total number of steps taken per day?  

In this section I display a Histogram using the aggregated dataset and the Mean and Median values of the Total steps taken for each day.


```r
#Display Histogram showing frequency of Steps
hist(activity_Agg$steps, main="Frequency of Steps",xlab="Steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

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

This section calculates the men for the steps over intervals for all days. A time plot is then displayed with these outputs.
To ensure the data displays in a readable format, I am displaying the "intervals"" on the x axis in 100 intervals

```r
#Get average steps taken over intervals
avg.daily.pattern <- aggregate(activity$steps,list(interval = activity$interval),FUN=mean,na.rm=TRUE)
names(avg.daily.pattern) <- c("interval","steps")

#set the x axis sequence

at <- seq(from = 0, to = 2400, by = 100)

#Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all days (y-axis)
plot(avg.daily.pattern$interval,avg.daily.pattern$steps,type = "l",ylab = "Steps", main = "Average Daily Activity Pattern", xlab = "Interval",xaxt = "n")
axis(side = 1, at = at, las = 2, hadj = 0.9)
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
rm(avg.daily.pattern)
rm(at)
```

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

missing <- is.na(activity$steps)                                #Identify NAs
notmissing <- !is.na(activity$steps)                            #Identify non NAs
activity.missing <- activity[missing,]                          #Subset data for NAs

#Merge missing data with mean values
activity.missing <- merge(activity.missing,avg.daily.pattern,by.x = "interval",by.y = "interval")
names(activity.missing) <- c("interval","steps","date","mean_steps")

#Set NAs equal to mean value
activity.missing$steps <- activity.missing$mean_steps

#Bind imputed rows with non NA rows
master <- rbind(activity.missing[,c(1,2,3)],activity[notmissing,])
master$date <-as.Date(master$date)                              #FormatDate

master <- master[order(master$date),]                           #Order data by date
master_Agg <- aggregate(steps ~ date, data = master,sum)        #Aggregate (Total) data step over date
master_Agg$date <-as.Date(master_Agg$date)                      #FormatDate

#Display Histograms for both Imputed and previous datasets
par(mfrow = c(1,2))
hist(activity_Agg$steps, main="Total Steps (NA Ignored)",xlab="Steps")
hist(master_Agg$steps, main="Total Steps (NA Imputed)",xlab="Steps") 
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

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
