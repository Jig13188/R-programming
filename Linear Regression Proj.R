######------- Linear Regression-------###
library(ggplot2)
library(dplyr)
library(car)#VIF function
library(irr)

setwd("F:\\JIGSAW\\DSR\\Topic 9 Linear Regression\\Code Snippets")
read.csv(file = 'DirectMarketing.csv',stringsAsFactors = T,header = T)->dm
str(dm)
summary(dm)
dm%>%group_by(History)%>%summarise(mean(AmountSpent))

dm$History<-ifelse(is.na(dm$History),"Missing",dm$History)

dm$History[index]

names(dm)
##----- Exploration---#

dm%>%group_by(Gender,Married)%>%summarise(mean(AmountSpent))

dm%>%group_by(Children)%>%summarise(mean(AmountSpent))
cor(dm$Children,dm$AmountSpent)

dm%>%group_by(Gender)%>%summarise(mean(AmountSpent))#Male

dm%>%group_by(Location)%>%summarise(mean(AmountSpent))#Far

dm%>%group_by(Age,Married)%>%summarise(Avg=mean(AmountSpent))%>%arrange(-Avg)

hist(dm$Salary,breaks = c(seq(10100,168800,by=40000)))

dm$Salary[929]<-mean(dm$Salary) #Replacing Missing Value with mean

cor(dm$Salary,dm$AmountSpent) #70.03%

dm%>%group_by(Catalogs)%>%summarise(Avg=mean(AmountSpent))%>%arrange(-Avg)

plot(x=dm$Age,y=dm$AmountSpent)

plot(x=dm$Gender,y=dm$AmountSpent)

dm$Catalogs<-as.factor(dm$Catalogs)

dm$Children<-as.factor(dm$Children)

summary(lm(data = dm,formula = AmountSpent~.))

mod1<-lm(data = dm,formula = AmountSpent~.)

step(mod1,direction = "both")

mod2<- lm(formula = AmountSpent ~ Age + Location + Salary + Children + 
     History + Catalogs, data = dm)

summary(mod2)

#creating Dummies

dm$Age_old<-ifelse(dm$Age=="Old",1,0)

dm$Location_far<-ifelse(dm$Location=="Far",1,0)

dm$Children_1<-ifelse(dm$Children==1,1,0)

dm$Children_2<-ifelse(dm$Children==2,1,0)

dm$Children_3<-ifelse(dm$Children==3,1,0)

dm$History_2<-ifelse(dm$History==2,1,0)

dm$History_3<-ifelse(dm$History==3,1,0)

dm$History_missing<-ifelse(dm$History=="Missing",1,0)

dm$Catalogs_12<-ifelse(dm$Catalogs==12,1,0)

dm$Catalogs_18<-ifelse(dm$Catalogs==18,1,0)
dm$Catalogs_24<-ifelse(dm$Catalogs==24,1,0)





mod3<-lm(data=dm,formula=AmountSpent~Age_old+Location_far+
           Children_1+Children_2+Children_3+History_2+History_3+Catalogs_12+
           Catalogs_18+Catalogs_24+Salary)

summary(mod3)

for(i in 11:ncol(dm))
{
  dm[,i]<-as.factor(dm[,i])  
}

mod4<-lm(data=dm,formula=AmountSpent~Location_far+
           Children_2+Children_3+History_2+History_3+Catalogs_12+
           Catalogs_18+Catalogs_24+Salary)


summary(mod4)# 0.7432 - Adjusted R square

#Final mod4 contains all the variable which explains better variation of the DV.


plot(dm$AmountSpent,type = "l",color="red")
line(pred,color="red")
plot(pred,type = "l",col="blue")
line(dm$AmountSpent,color="red")
plot(mod4$residuals)


vif(mod = mod4) #None of the variables are correlated.
mod5<-lm(data=dm,formula=log(AmountSpent)~Location_far+
           Children_2+Children_3+History_2+History_3+Catalogs_12+
           Catalogs_18+Catalogs_24+Salary)
plot(mod5$fitted.values,mod5$residuals)
hist(mod5$residuals)

