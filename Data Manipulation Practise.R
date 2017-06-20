library(lubridate)
library(dplyr)
data("lakers")
# opponent== LAL,team=POR

### Use the lakers data set in the package lubridate. How many matches were played when opponent to LAL was POR?
data("lakers")
# opponent== LAL,team=POR
lakers%>%filter(opponent=="POR" & team== "LAL")%>%nrow()



### Use the inbuilt mtcars data set, what is the average "mpg" for each category of car defined based on "number of gears".
data(mtcars)
aggregate(mtcars$mpg,by=list(mtcars$gear),mean)
               ##### or ######
with(mtcars,aggregate(mpg,by=list(gear),mean))

### Use the AdultUCI data set from the arules() package. How many 38 year olds are divorced?
library(arules)
data("AdultUCI")
names(AdultUCI) #age,"marital-status"
unique(AdultUCI$`marital-status`)#Divorced
AdultUCI%>%filter(age==38, `marital-status`=="Divorced")%>%nrow()#225

### Use the lakers data set in the package lubridate. How many matches were played by Pau Gasol against POR on Sundays?
d<-lakers$date
d<-ymd(d)
head(weekdays(d))
lakers$day<-weekdays(d)
lakers%>%filter(opponent=="POR" & player=="Pau Gasol",day=="Sunday")%>%nrow()

### Use the lakers data set in the package lubridate. How many matches were played at 12:00 and on Monday?
lakers$date_time<-ymd_hm(paste(lakers$date,lakers$time,sep=""))
# lakers$date_time<-as.POSIXlt(lakers$date_time)
head(hour(lakers$date_time))
unique(hour(lakers$date_time))
unique(weekdays(lakers$date_time))
lakers%>%filter(hour(date_time)==12,weekdays(date_time)=="Monday")%>%nrow()





### Use the lakers data set in the package lubridate. How many matches were played 
# in the months of October, December, January and April.
lakers$date<-ymd(lakers$date)

table(months(lakers$date)) #displays all months
                    ### OR ###
lakers%>%group_by(Months=months(date))%>%summarise(n())%>%filter(Months=="October"|Months=="December"|Months=="January"|Months=="April")
# April December February  January    March November  October 
# 3618     6240     5461     6515     6447     5472      871 

### Use the inbult iris dataset, what is mean Sepal.Length for species setosa?
data("iris")
aggregate(iris$Sepal.Length,by=list(iris$Species),mean)#for all species
                                ###### OR ########
iris%>%group_by(Species)%>%summarise(mean(Sepal.Length))%>%filter(Species=="setosa")

# Using the data set AdultUCI from the package arules() find out 
# the mean age corresponding to all categories in the column "income" for all white females 
# who work less than 25 hours a week. (Consider the missing values in the column "income" also as a separate group)

AdultUCI%>%filter(sex=="Female",race=="White",`hours-per-week`<25)%>%group_by(income)%>%summarise(Avg_age=mean(age))%>%data.frame()
# income    Avg_age
# 1  small 34.42763
# 2  large 45.38462
# 3   <NA> 34.53442

# Load the inbuilt dataset AdultUCI from arules package. 
# How many females are there in the data set whose age is less than 50 and who are black?
AdultUCI%>%filter(age<50,race=="Black",sex=="Female")%>%nrow()#1870 










