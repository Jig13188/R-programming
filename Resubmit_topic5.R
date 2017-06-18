data.frame(Name=c("Ramya","Ali","Jim"),Age=c(25,30,35),Telephone_bill_rs=c(600,400,200),Month=rep("Aug",3))
Names<-list(LastName=c("Potter","Riddle","Dumbledore"),FirstName=c("Harry","Tom","Albus"),Age=c(18,50,120),Proffesion=c("Student","Magician","HeadMaster"))
class(Names)
names(Names)
Names$LastName
Names$Age[3]
Names[[3]][3]
install.packages("ggplot2")
library(ggplot2)
msleep
names(msleep)[3]<-"type"
str(msleep)
msleep$type<-as.factor(msleep$type)
msleep$type<-as.character(msleep$type)
msleep_new<-msleep[,c("name","genus","type","sleep_total")]
str(msleep_new)
class(msleep_new)
getwd()
write.csv(msleep_new,"msleep_new.csv")
install.packages("XLConnect")
library(XLConnect)
load<-loadWorkbook(("C:/Data Science with R/Assignments/Non Graded Assignments/Topic 5 - Introduction to R/retail.xlsx"),create = F)
class(load)
str(load)
data1<-readWorksheet(load,"data1",header = T)
class(data1)
data2<-readWorksheet(load,"data2",header = T)
class(data2)