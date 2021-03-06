---
title: "PA1_template.md"
author: "Cesar Ascencio"
date: "18 de febrero de 2019"
output: html_document
---

---
title: "Activity monitoring data"
author: "Cesar Ascencio"
date: "18 de febrero de 2019"
output: html_document
---


## Loading and preprocessing the data
```{r loaddata}

setwd("C:/Users/cesar ascencio/Desktop/Data_Science_especialización/Curso 5 Reproductible researc/Proyecto")

unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```{r}
library(lubridate)
data$date<-as.Date(data$date, format("%Y-%m-%d")) 

total_number_steps<-aggregate(x = data$steps, 
                             by = list(day(data$date)), 
                             FUN = sum, na.rm = T)

colnames(total_number_steps)<-c("Day", "N_Total_Steps")


hist(total_number_steps$N_Total_Steps,col="lightcyan",
     main = "Histogram of total number of steps taken per day",
     xlab = "Total number of steps")

##Calculate and report the mean and median of the total number of steps taken per day
mean(total_number_steps$N_Total_Steps)

median(total_number_steps$N_Total_Steps)

## Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute #interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

mean_number_steps<-aggregate(x = data$steps, 
                             by = list(data$interva), 
                             FUN = mean, na.rm = T)

plot(mean_number_steps, type = "l", xlab = "mean number of steps by interval", ylab="mean")

#Which 5-minute interval, on average across all the days in the dataset, contains the #maximum number of steps?

mean_number_steps[which.max(mean_number_steps$x),]

#Calculate and report the total number of missing values in the dataset (i.e. the total #number of rows with \color{red}{\verb|NA|}NAs)

table(is.na(data$steps))

##Devise a strategy for filling in all of the missing values in the dataset. The strategy #does not need to be sophisticated. For example, you could use the mean/median for that day, #or the mean for that 5-minute interval, etc.

#Create a new dataset that is equal to the original dataset but with the missing data filled in.

datafilled<-data

datafilled$steps<-ifelse(is.na(datafilled$steps)=="TRUE",mean(data$steps,na.rm = TRUE), datafilled$steps)

datafilled$date<-as.Date(data$date, format("%Y-%m-%d")) 

datafilled$date<-ifelse(is.na(datafilled$date)=="TRUE",mean(data$date,na.rm = TRUE), datafilled$date)

data$interval<-ifelse(is.na(datafilled$interval)=="TRUE",mean(data$interval,na.rm = TRUE), datafilled$interval)

#Make a histogram of the total number of steps taken each day and Calculate and report the mean #and median total number of steps taken per day. Do these values differ from the estimates from #the first part of the assignment? What is the impact of imputing missing data on the estimates of 3the total daily number of steps?


datafilled$date<-as.Date(data$date, format("%Y-%m-%d")) 

total_number_steps<-aggregate(x = datafilled$steps, 
                             by = list(day(datafilled$date)), 
                             FUN = sum, na.rm = T)

colnames(total_number_steps)<-c("Day", "N_Total_Steps")


hist(total_number_steps$N_Total_Steps,col="lightcyan",
     main = "Histogram of total number of steps taken per day",
     xlab = "Total number of steps")

##Calculate and report the mean and median of the total number of steps taken per day
mean(total_number_steps$N_Total_Steps)

median(total_number_steps$N_Total_Steps)

```

 Mean and median values are higher after imputing missing data. The reason is
 #that in the original data, there are some days with `steps` values `NA` for 
 #any `interval`. The total number of steps taken in such days are set to 0s by
 #default. However, after replacing missing `steps` values with the mean `steps`
 #of associated `interval` value, these 0 values are removed from the histogram
 #of total number of steps taken each day.
 
 
 
 Are there differences in activity patterns between weekdays and weekends?
First, let's find the day of the week for each measurement in the dataset. In
this part, we use the dataset with the filled-in values.


```{r}
datafilled$date <- as.Date(datafilled$date)
datafilled$con<-ifelse(wday(datafilled$date) %in% c("1","2","3","4","5"), "Weekday", "Weekend")

```

```{r}
library(ggplot2)
averages <- aggregate(steps ~ interval + con, data=datafilled, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(con ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")

#There are some differences in average steps between Weekdays and Weekends. During weekdays, the person is more active at the start of the day and less active during the day. Meanwhile, during weekends, the person is less active at start of the day and more active throughout the day.

#This is probably because the person is commuting to work in the morning and less active during work hours (sitting at desk). During weekends, the person does not have to prepare for work and therefore less active in the mornings, but more active during the day as the person is off from work.




```



