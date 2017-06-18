setwd("C:\\Jig13188\\Topic 6.2")
getwd()
brands<-read.csv("brands_new.csv",na.strings = c("-","NA"))
library(dplyr)
#To change the company Advt coloumn we stored it into 
#an object x. Later after finising my Conversion i will restore into my dataframe.
x<-brands$Company.Advertising
class(x)
as.character(x)->x

#To Remove the B sign only and convert into Millions
for(i in 1:length(x))
  if(grepl("B",x[i]))
    x[i]<- as.numeric(gsub("B","",x[i]))*1000
else
  x[i]<-(gsub("M","",x[i])
  
#TO remove the M signs and convert into numeric
for(i in 1:length(x))
  if(grepl("M",x[i]))
    
    
    
    
p+geom_point(aes(col=Brand,size=as.factor(Brand)),show.legend = F)+xlab("Comapany Advertising in Billion $")+ylab("Brand Revenue in Billion $")+ggtitle("Technology")+theme(plot.title = element_text(face="bold",size=30,hjust = 0.5,vjust =2.0))+theme_bw()


    x[i]<-(gsub("M","",x[i]))
else
  x[i]<-as.numeric(x[i]) 

x<-as.numeric(x)/1000 

#Restoring the desired output (Millions to Billions) into my dataframe
brands$Company.Advertising<-x/1000 
#Data Visualization:
#Brand Revenue(on Y-axis) and Brand Advt(On x-axis)
#Technology brands
library(dplyr)
library(ggplot2)



#1.Technology(Object saved as technology.rda)

which(!complete.cases(filter(brands,Industry=="Technology")))->index1
technology<-filter(brands,Industry=="Technology")
technology<-technology[-index1,]

T<-ggplot(technology,aes(x=Company.Advertising,y=Brand.Revenue,size=Brand.Value,colour=Brand))+geom_point()+xlab("Comapany Advertising in Billion $")+ylab("Brand Revenue in Billion $")+geom_text(aes(label=Brand,color=Brand),vjust=1.0)+scale_size_continuous(name="Brand Value $(Billions)",range = c(2,6),breaks=c(30,60,100))+theme_bw()+theme(panel.grid.major = element_line(colour = "Grey"))+guides(color=F)+ggtitle("Technology")+theme(plot.title = element_text(face="bold",hjust = 0.7,size=20))+theme(panel.border = element_rect(color="grey"))+theme(legend.background = element_rect(fill = "lightblue",linetype = "solid"))

#2.Luxury (Object saved as Luxury.rda)

which(!complete.cases(filter(brands,Industry=="Luxury")))->index2
Luxury<-filter(brands,Industry=="Luxury")
Luxury<-Luxury[-index2,]
L<-ggplot(Luxury,aes(x=Company.Advertising,y=Brand.Revenue))+geom_point(aes(col=Brand,size=Brand.Value))+xlab("Company Advertising in Billion $")+ylab("Brand Revenue in Billion $")+geom_text(aes(label=Brand,color=Brand),vjust=2.0)+scale_x_continuous(breaks=seq(0,4.8,0.1))+theme_bw()+guides(color=F)+scale_size_continuous(breaks=c(10,28.0))+ggtitle("Luxury")+theme(plot.title = element_text(face="bold",size = 20))+theme(legend.background = element_rect(fill="lightblue",linetype = "solid",colour = "black"))+theme(legend.title = element_text(face = "bold"))+theme(axis.title.x = element_text(face = "bold"))+theme(axis.title.y = element_text(face="bold"))

#3.Automotive(object saved as automotive.rda)

index3<-which(!complete.cases(filter(brands,Industry=="Automotive")))
automotive<-filter(brands,Industry=="Automotive")
automotive<-automotive[-index3,]
A<-ggplot(automotive,aes(x=Company.Advertising,y=Brand.Revenue))+geom_point(aes(col=Brand,size=Brand.Value))+scale_size_continuous(name = "Brand Value $(Billions)",breaks=c(6.2,20.0,37.8))+guides(color=F)+theme_bw()+scale_x_continuous(breaks=seq(0.8,5.4,0.1))+scale_y_continuous(breaks=seq(40,170,10))+geom_text(aes(label=Brand,color=Brand),vjust=1.0)+xlab("Company Advertising in Billions of $")+ylab("Brand Revenue in Billions of $")+ggtitle("Automotive")+theme(plot.title = element_text(hjust=0.5))+theme(legend.background = element_rect(fill = "lightblue"))

#4.financial (object saved as financial.rda)

index4<-which(!complete.cases(filter(brands,Industry=="Financial Services")))
financial<-filter(brands,Industry=="Financial Services")
financial<-financial[-index4,]
A<-ggplot(financial,aes(x=Company.Advertising,y=Brand.Revenue))+geom_point(aes(col=Brand,size=Brand.Value))+guides(color=F)+ggtitle("Financial")+theme_bw()+scale_x_continuous(breaks=seq(0.6,3.4,0.1))+scale_y_continuous(breaks=seq(10,90,10))+geom_text(aes(label=Brand,col=Brand),hjust=-0.03,vjust=1)+theme(panel.border = element_rect(colour = "grey"))+theme(plot.title = element_text(face="bold",size=20,hjust = -0.1,vjust = 1))+xlab("Company advertising in Billion $")+ylab("Brand Revenue in Billion $")
