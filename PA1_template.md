#Load needed packages
library(ggplot2)
library(plyr)
library(lattice)

#unzip and assign file to variable
unzip("activity.zip")
activity <- read.csv("activity.csv")

#quickly examine preview of data	
head(activity)

#Assign subsets to days and date and time
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")

#Clean the data to remove nas
clean <- activity[!is.na(activity$steps),]

#aggregate columns to table
sumTable <- aggregate(activity$steps ~ activity$date, FUN=sum, )
colnames(sumTable)<- c("Date", "Steps")

#Produce histogram
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")

#Display mean and median
mean(sumTable$Steps)
median(sumTable$Steps)

#Produce interval table
intervalTable <- ddply(clean, .(interval), summarize, Avg = mean(steps))

#Create plot of intervals
p <- ggplot(intervalTable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")

#Get max steps
maxSteps <- max(intervalTable$Avg)

intervalTable[intervalTable$Avg==maxSteps,1]

nrow(activity[is.na(activity$steps),])

avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))

nadata <- activity[is.na(activity$steps),]

#Merge data
newdata <-merge(nadata, avgTable, by=c("interval", "day"))

newdata2 <- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")

mergeData <- rbind(clean, newdata2)

sumTable2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
colnames(sumTable2)<- c("Date", "Steps")

#Calculate mean and median of table
mean(sumTable2$Steps)
median(sumTable2$Steps)

#Reproduce histogram with separated data
hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )

#merge categories to separate weekend and weekdays
mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))

#Plot weekday and weekend
xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),main="Average Steps per Interval Based on Type of Day", ylab="Average Number of Steps", xlab="Interval")
