read.csv("C:/Data Science with R/Assignments/Non Graded Assignments/Topic 6.1 - Data Manipulation/FlightDelays.csv")->fd
dim(fd)
names(fd)
str(fd)
unique(fd$date)
fd$weather<-as.factor(fd$weather)
fd$date<-mdy(fd$date)
library(dplyr)
library(lubridate)
fd$date<-mdy(fd$date)

#1.Find out the number of delayed flights for all weekdays 

fd$date<-mdy(fd$date)

table(fd$delay,weekdays(fd$date))
fd%>%filter(delay=="delayed",weekdays(date)!="Saturday",weekdays(date)!="Sunday")%>%nrow()



#2.Find the average distance, total distance and count for all delayed flights on Friday.

fd%>%filter(delay=="delayed"&weekdays(date)=="Friday")%>%summarise(mean(distance),sum(distance),n())

#3. Find out how many flights were on time on Week days and Weekends (Consider Saturday and Sunday as weekends)

fd%>%filter(delay=="ontime",weekdays(date)!="Saturday",weekdays(date)!="Sunday")%>%summarize(n())  #for weekdays
fd%>%filter(delay=="ontime",weekdays(date)=="Saturday"|weekdays(date)=="Sunday")%>%summarize(n())  #for weekends


#4.Find out the number of flights for each destination across all weekdays


filter(fd,weekdays(date)!="Saturday",weekdays(date)!="Sunday")->wd
table(wd$dest,weekdays(wd$date))


#5.Find out the number of times weather was bad across all weekdays. (1 indicates bad weather)
str(fd$weather)
fd$weather<-as.factor(fd$weather)
fd%>%filter(weekdays(date)!="Saturday",weekdays(date)!="Sunday")%>%filter(weather==1)%>%summarize(n())

q()
























