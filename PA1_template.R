
        
        ## Loading and preprocessing the data

setwd("C:/Users/cesar ascencio/Desktop/Data_Science_especialización/Curso 5 Reproductible researc/Proyecto")
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")


## What is mean total number of steps taken per day?

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


#Are there differences in activity patterns between weekdays and weekends?
      #  First, let's find the day of the week for each measurement in the dataset. In
#this part, we use the dataset with the filled-in values.


datafilled$date <- as.Date(datafilled$date)
datafilled$con<-ifelse(wday(datafilled$date) %in% c("1","2","3","4","5"), "Weekday", "Weekend")


library(ggplot2)
averages <- aggregate(steps ~ interval + con, data=datafilled, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(con ~ .) +
xlab("5-minute interval") + ylab("Number of steps")


