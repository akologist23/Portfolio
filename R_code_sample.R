#DESCRIPTION - Answering questions on data from a body monitor tracking a user's movements
#AUTHOR - Alicia Korol

#Retrieve data from internet
setwd("~./DataScience/ReproducibleResearch/Assignment1")
destination <- getwd()
URL <-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"    
download.file(URL, destfile = "activitymonitoringdata.zip")
unzip("activitymonitoringdata.zip")
list.files()

activity <- read.csv("activity.csv",header=TRUE, stringsAsFactors = FALSE)

####
####QUESTION 1 - What is the mean total number of steps taken per day per user?
####

library(dplyr)

act_tbl <- tbl_df(activity)

result <- act_tbl %>%
    group_by(date) %>%
    summarize(step.sum = sum(steps, na.rm=TRUE))

library(xtable)
x1<-xtable(head(result))
print(x1, type="html")

hist(result$step.sum,breaks=12,col="lightblue",main="Total number of steps taken per day", xlab="Daily step count")

#####ANSWER QUESTION 1###########
mean(result$step.sum, na.rm=TRUE)
#################################

####
####QUESTION 2 - What is the average daily activity pattern?
####

pat <- act_tbl %>%
    filter(!is.na(steps)) %>%
    group_by(interval) %>%
    summarize(av.steps = mean(steps))    

with(pat, plot(interval,av.steps,type="l",xlab="interval",ylab="Average number of steps taken",col="blue"))

whichinterval<- pat[pat$av.steps==max(pat$av.steps),"interval"]
data.frame(whichinterval)[1,1]

#Imputing missing values

sum(is.na(act_tbl$steps)) #reports number of missing values

#Strategy: Using mean steps per interval across all days to impute
pat1 <- as.data.frame(pat) #converting previously generated summary data by interval to dataframe

#Function to extract the mean steps given an interval from a dataframe
av.value <- function(df,x){    
    row <- df[df$interval %in% x,]
    row[,2]    
    }

av.value(pat1,5)
head(pat1)

#Creating a new variable of mean steps per interval with original and imputed values
    activity$steps_imp <- NA #initializing vector
for(i in seq_along(activity$steps)){
    if(is.na(activity$steps[i])==TRUE){
        activity$steps_imp[i] <- av.value(pat1,x=activity$interval[i])
        }
    else {
        activity$steps_imp[i] <- activity$steps[i]    
        }
    }

activity_imp <- data.frame(activity$steps_imp,activity$date,activity$interval)
colnames(activity_imp) <- c("steps","date","interval")


act_imp_tbl <- tbl_df(activity_imp)

result_imp <- act_imp_tbl %>%
    group_by(date) %>%
    summarize(step.sum = sum(steps, na.rm=TRUE))

#############ANSWER QUESTION 2####################
hist(result_imp$step.sum,breaks=12,col="yellow",main="Total number of steps taken per day with imputed data", xlab="Daily step count")

mean(result_imp$step.sum, na.rm=TRUE)
##################################################

####
####QUESTION 3 - Are there differences in activity patterns between weekdays and weekends?
####

#use activity_imp
activity_imp$date <- as.Date(activity_imp$date, format="%Y-%m-%d")
activity_imp$weekdays <- weekdays(activity_imp$date,abbreviate=TRUE)
table(activity_imp$weekdays)

for(i in seq_along(activity_imp$date)){
    if(weekdays(activity_imp$date[i],abbreviate=TRUE) %in% c("Mon","Tue","Wed","Thu","Fri")){
        activity_imp$weekdays[i] <- "weekday"   
    }
    else {
        activity_imp$weekdays[i] <- "weekend" 
    }
}

#activity_imp$weekdays <- as.factor(activity_imp$weekdays)

act_imp_tbl1 <- tbl_df(activity_imp)

pat_imp <- act_imp_tbl1 %>%
    group_by(weekdays, interval) %>%
    #group_by(interval) %>%
    summarize(av.steps = mean(steps))    

library(lattice)

#############ANSWER QUESTION 3##################
xyplot(av.steps~interval|weekdays,pat_imp,layout=c(1,2),type="l",xlab="5-minute interval",ylab="mean number of steps")
################################################