---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
This document looks into the following problems...

## Step 1: Loading and preprocessing the data
Let's first load the data and make sure that the observations in each column
is of the right class.


```r
library (dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library (lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     intersect, setdiff, union
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
data <- read.csv("activity.csv", header = TRUE, colClasses = "character")


data %>%
  mutate (date = as.Date (date)) %>%
  mutate (steps = as.integer(steps)) %>%
  mutate (interval = as.integer (interval)) -> data
  head (data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## Step 2: What is mean total number of steps taken per day?
Now let's look at the data, especially the total numbers of steps taken by the
test subject.


```r
#calculate the total steps per day
data %>%
  group_by (date) %>%
  summarize (steps = sum (steps, na.rm = TRUE)) -> data1

#make a histogram
hist (data1$steps)
rug (data1$steps)
```

![](PA1_template_files/figure-html/totalsteps-1.png)<!-- -->

```r
#return the mean and median values
summary (data1$steps) 
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```


Voila! Now we know the mean and the median of the total steps taken per day.


## Step 3: What is the average daily activity pattern?

Okay, now let's look at the daily activity pattern, by investigating how the
total steps increases in 5 mins intervals


```r
library (ggplot2)

#format the data and calculate the mean steps for each interval averaged by days
data %>%
  select (interval, steps) %>%
  group_by (interval) %>%
  summarize (steps = mean(steps, na.rm = TRUE)) -> data2
 
#plot the daily activity pattern
    ggplot (data2, aes (interval, steps)) +
      geom_line () +
      labs (title  = "Test subject daily activity pattern on average")
```

![](PA1_template_files/figure-html/dailypattern-1.png)<!-- -->


## Step 4: Imputing missing values
Now, we noticed that there are quite a few missing values in our observations.
Next we will impute these missing values by replacing the NAs with the mean steps of that 5-min interval.


```r
#calculate the total of NAs
id <- is.na(data$steps)
sum(id)
```

```
## [1] 2304
```

```r
#the distribution of NA data is even across the intervals
table(data$interval[id])
```

```
## 
##    0    5   10   15   20   25   30   35   40   45   50   55  100  105  110  115 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  120  125  130  135  140  145  150  155  200  205  210  215  220  225  230  235 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  240  245  250  255  300  305  310  315  320  325  330  335  340  345  350  355 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  400  405  410  415  420  425  430  435  440  445  450  455  500  505  510  515 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  520  525  530  535  540  545  550  555  600  605  610  615  620  625  630  635 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  640  645  650  655  700  705  710  715  720  725  730  735  740  745  750  755 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  800  805  810  815  820  825  830  835  840  845  850  855  900  905  910  915 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  920  925  930  935  940  945  950  955 1000 1005 1010 1015 1020 1025 1030 1035 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1040 1045 1050 1055 1100 1105 1110 1115 1120 1125 1130 1135 1140 1145 1150 1155 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1200 1205 1210 1215 1220 1225 1230 1235 1240 1245 1250 1255 1300 1305 1310 1315 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1320 1325 1330 1335 1340 1345 1350 1355 1400 1405 1410 1415 1420 1425 1430 1435 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1440 1445 1450 1455 1500 1505 1510 1515 1520 1525 1530 1535 1540 1545 1550 1555 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1600 1605 1610 1615 1620 1625 1630 1635 1640 1645 1650 1655 1700 1705 1710 1715 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1720 1725 1730 1735 1740 1745 1750 1755 1800 1805 1810 1815 1820 1825 1830 1835 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1840 1845 1850 1855 1900 1905 1910 1915 1920 1925 1930 1935 1940 1945 1950 1955 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 2000 2005 2010 2015 2020 2025 2030 2035 2040 2045 2050 2055 2100 2105 2110 2115 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 2120 2125 2130 2135 2140 2145 2150 2155 2200 2205 2210 2215 2220 2225 2230 2235 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 2240 2245 2250 2255 2300 2305 2310 2315 2320 2325 2330 2335 2340 2345 2350 2355 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8
```

```r
#filling in all the NAs in the data set with their interval means
  data[id,]%>%
   group_by (interval) %>%
   arrange (interval) -> df
 
  temp <- vector ("numeric")

for (i in 1:nrow(data2)) {
  
  temp <- c(temp, rep (data2$steps[i], times = 8))
}    

    as.matrix(df) %>%
    cbind (temp) -> data3
    
    as.data.frame(data3) %>%
      select (temp, date, interval) -> data3
    
    names (data3) <- c("steps", "date", "interval")
    
    data3$steps <- as.numeric (data3$steps)
    data3$date <- as.Date(data3$date)
    data3$interval <- as.numeric (data3$interval)
    
    data[!id,] %>%
      bind_rows (data3) -> data3

#compute the mean and median total of the total steps which the test
#subject has taken per day
    
data3 %>%
  group_by (date) %>%
  summarize (steps = sum(steps)) -> data4

hist (data4$steps)
rug(data4$steps)
```

![](PA1_template_files/figure-html/imputingNAs_with interval means-1.png)<!-- -->

```r
summary (data4$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   11458   13665   15084   32873
```

Now we can compare the Mean and Median of the total steps per day with those obtained before imputing the data.
You probably noticed that the extreme value (35000 steps) in the histogram that looks very different from our histogram with the raw data in Step 2.


So, what if we use another imputing method? Let's impute the missing values by replacing the NAs with the mean steps of that day. How different will the imputed data set look?

```r
#filling in all the NAs in the data set with their day means
#pull all the NA observations from the raw data
  data[id,]%>%
   group_by (date) %>%
   arrange (date) -> df

#observe that the NA observations are evenly distributed across the days
table (df$date)
```

```
## 
## 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 2012-11-14 
##        288        288        288        288        288        288        288 
## 2012-11-30 
##        288
```

```r
#calculate the mean steps by days
  data %>%
    group_by(date) %>%
    summarize (steps = mean(steps, na.rm = TRUE)) -> data5

#replace the NAN results with 0s
  for (i in 1:nrow(data5)) {
    if (is.nan(data5$steps[i])) {
      data5$steps[i] <- 0
    }
  }
  
#save the day means in a temp vector 
  temp <- vector ("numeric")
  for (i in 1:nrow(data5)) {
  
  temp <- c(temp, rep (data5$steps[i], times = 288))
  }    

#replace the NAs with temp in df
    as.matrix(df) %>%
    cbind (temp) -> data6
```

```
## Warning in cbind(., temp): number of rows of result is not a multiple of vector
## length (arg 2)
```

```r
    as.data.frame(data6) %>%
      select (temp, date, interval) -> data6
    
    names (data6) <- c("steps", "date", "interval")
    
    data6$steps <- as.numeric (data6$steps)
    data6$date <- as.Date(data6$date)
    data6$interval <- as.numeric (data6$interval)

#bind the imputed observations with the rest of the dataframe    
    data[!id,] %>%
      bind_rows (data6) -> data6

#compute the mean and median total of the total steps which the test
#subject has taken per day  
    
  data6 %>%
  group_by (date) %>%
  summarize (steps = sum(steps)) -> data7

hist (data7$steps)
rug(data7$steps)
```

![](PA1_template_files/figure-html/imputingNAs with daymeans-1.png)<!-- -->

```r
summary (data7$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    6778   10395    9491   12811   21194
```

Comparing the two imputing methods with the data summary and histogram of the original data, it is clear that the second imputing method, replacing NAs with daily mean, introduces less distortion to the original data.

We will continue our analysis with data imputed from the 2nd method.

## Step 5: Are there differences in activity patterns between weekdays and weekends?


```r
#extract the date col. and return its day in the week

vect <- weekdays(data6$date)
temp <- seq_along(vect)

for (i in 1:length(vect)) {
  if (vect[i]== "Saturday"|vect[i]== "Sunday") {
    temp [i] <- "Weekend"
  }
  else {
    temp[i] <- "Weekday"
  }
}

cbind (data6, as.factor(temp)) -> data8
names (data8) <- c("steps", "date", "interval", "weekday")

data8 %>%
  select (-(date)) %>%
  group_by (interval, weekday) %>%
  summarize (steps = mean(steps)) -> data8

ggplot(data8, aes(interval, steps)) +
  geom_line()+
  facet_grid(rows = vars(weekday))+
  labs (title  = "Test subject daily activity pattern")
```

![](PA1_template_files/figure-html/weekdays-1.png)<!-- -->
Voila! Here is the test subject's daily activity pattern with imputed data!
Thank you for reading my analysis!
