library(dplyr)
library(ggplot2)



# Download the file if it does not exist
if (!file.exists("activity.zip"))
{
    download.file(
        "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile = "activity.zip"
    )
}

if (!file.exists('activity.csv'))
{
    unzip("activity.zip",exdir = "./")
    
}

# Read data
if (!'activitydata' %in% ls())
{
    activitydata <- read.csv('activity.csv')
}

#preprocess the data
activityperday <- group_by(activitydata, date)
#total number of steps each day summary
activitysummary <-
    summarise(activityperday,totalsteps = sum(steps, na.rm = TRUE))

#plot histogram
hist(activitysummary$totalsteps,main = 'Total Number of Steps Taken Each day', xlab = 'Number of Steps')

#mean of total stpes per day
mean(activitysummary$totalsteps)

#median of totalsteps  per day
median(activitysummary$totalsteps)

activitymin <- activitydata[,c('steps', 'interval')]

activitypermin <- group_by(activitymin, interval)

activityperminsum <-
    summarise(activitypermin, avgsteps = mean(steps, na.rm = TRUE))

plot(
    activityperminsum$interval, activityperminsum$avgsteps, type = 'l',
    main = 'Average number of steps per 5 min for Oct & Nov', xlab = 'Time (HHMM)',
    ylab = 'Average Steps'
)

abline(v = activityperminsum[activityperminsum$avgsteps == max(activityperminsum$avgsteps),1],
       col = 'red')

#print time interval for max average steps
print(activityperminsum[activityperminsum$avgsteps == max(activityperminsum$avgsteps),])


#total number of NA
print(sum(is.na(activitydata$steps)))

# fill no na values with average steps for that 5 min interval
activitynona <- merge(activitydata, activityperminsum, all = TRUE)
activitynona$steps[which(is.na(activitynona$steps))] <-
    activitynona$avgsteps[which(is.na(activitynona$steps))]

#new activity data without na
newactivitydata <- activitynona[,1:3]

#activity per day summary with  new data
newactivityperday <- group_by(newactivitydata, date)
#total number of steps each day summary
newactivitysummary <-
    summarise(newactivityperday,totalsteps = sum(steps, na.rm = TRUE))

#plot histogram
hist(newactivitysummary$totalsteps,main = 'Total Number of Steps Taken Each day - NA removed', xlab = 'Number of Steps')

#mean of total stpes per day
print(mean(activitysummary$totalsteps))
print(mean(newactivitysummary$totalsteps))

#median of totalsteps  per day
print(median(activitysummary$totalsteps))
print(median(newactivitysummary$totalsteps))

#add weekday column to the activity data
newactivitydataw <-
    mutate(newactivitydata, weekday = factor(1 * (
        weekdays(as.Date(date)) %in% c('Saturday', 'Sunday')
    ), labels = c('Weekend', 'Weekday')))

#group 
newactivitydatawg<-group_by(newactivitydataw, weekday, interval)
#summarise
newactivitydatawgs<-summarise(newactivitydatawg, avgsteps = mean(steps,na.rm = TRUE))


#plot
g<-ggplot(data = newactivitydatawgs,aes(interval,avgsteps))+facet_grid(.~weekday)+geom_line()
print(g)







