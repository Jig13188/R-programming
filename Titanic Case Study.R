library(dplyr)
library(caret)
library(irr)
library(ROCR)
library(randomForest)
setwd("F:\\Kaggle\\Titanic Dataset")

titanic.train<-read.csv(file = "train.csv",stringsAsFactors = F,header = T,na.strings = c(" ",NA))


titanic.test<-read.csv(file = "test.csv",stringsAsFactors = F,header = T,na.strings = c(" ",NA))

titanic.test$Survived<-NA

titanic.test$Is.train<-"False"

titanic.train$Is.train<-"True"

titanic.full<-rbind(titanic.train,titanic.test)

dim(titanic.full)#1309   13

summary(titanic.full)

#Removing Rows which are missing values

colSums(is.na(titanic.full))

titanic.full[is.na(titanic.full$Survived),]

#Age value has to replace

titanic.full$Survived<-as.factor(titanic.full$Survived)

aggregate(titanic.full$age,by=list(titanic.full$Survived),mean,na.rm=T)

titanic.full%>%filter(Survived==1)%>%summarise(mean(Age,na.rm=T))

titanic.full%>%filter(Survived==0)%>%summarise(mean(Age,na.rm=T))#30.62

index<-which(is.na(titanic.full$Age)) 
 
prop.table(table(titanic.full$Survived[index]))

#70.6% people are missing which are not survived.Imputing with mean of those people who are not survived

titanic.full[index,"Age"]<-30.62

table(titanic.full$Fare)

titanic.full[is.na(titanic.full$Fare),"Fare"]<-mean(titanic.full$Fare,na.rm = T)
# titanic.full$Survived<-na.omit(titanic.full$Survived)

index2<-which(is.na(titanic.full$Survived))

titanic.full_new<-titanic.full[-index,] # 1046   13

titanic.test_new<-titanic.full[titanic.full$Is.train=="False",]

titanic.train_new<-titanic.full[titanic.full$Is.train=="True",]
#---------------------------Data Cleaning Completed-------------#

#-----------Data Exploration-------#
plot(y=titanic.train_new$Age,x=titanic.train_new$Survived)

titanic.train_new$Sex<-ifelse(titanic.train_new$Sex=="female","F","M")

titanic.train_new$Sex<-as.factor(titanic.train_new$Sex)

round(prop.table(table(titanic.train_new$Sex,titanic.train_new$Survived)),2)
# 0    1
# F 0.09 0.26
# M 0.53 0.12 (Males death rate is high compared to Females)

titanic.train_new$Pclass<-as.factor(titanic.train_new$Pclass)

prop.table(table(titanic.train_new$Sex,titanic.train_new$Pclass,titanic.train_new$Survived))

prop.table(table(titanic.train_new$Pclass,titanic.train_new$Sex,titanic.train_new$Survived))

#Pclass==3 contains high death rate.

aggregate(titanic.train_new$Fare,by=list(titanic.train_new$Survived),mean)

#Obviously Low fare paid people dead(Lakc of proper facilities at the time of help boats)

round(prop.table(table(titanic.train_new$Embarked,titanic.train_new$Survived)),2)

titanic.train_new[titanic.train_new$Embarked=="","Embarked"]<-"S"#Bcoz S has higest proportion.

#Obviously as more people in S, simillarly death rate also high.

###################------MODELLING-------################

mod1<-glm(formula = Survived~.,data = titanic.train_new[,-c(4,1,13,9,11)],family = "binomial")
summary(mod1)

step(mod1,direction = "both")

# formula = Survived ~ Pclass + Sex + Age + SibSp + Embarked (FROM STEPWISE ALGORITHM)

mod2<-glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Embarked,data=titanic.train_new[,-c(4,1,13,9,11)],family = "binomial")
summary(mod2)

#----------dummy varibale creation--------------#

titanic.train_new$Embarked_Q<-ifelse(titanic.train_new$Embarked=="Q",1,0)

mod3<-glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Embarked_Q,data=titanic.train_new[,-c(4,1,13,9,11)],family = "binomial")
summary(mod3)

mod4<-glm(formula = Survived ~ Pclass + Sex + Age + SibSp,data=titanic.train_new[,-c(4,1,13,9,11)],family = "binomial")
summary(mod4)
# mod1:
#   ====
#   Null deviance: 1186.66  on 890  degrees of freedom
# Residual deviance:  784.04  on 881  degrees of freedom
# AIC: 804.04
# 
# mod2:
#   ====
#   Null deviance: 1186.66  on 890  degrees of freedom
# Residual deviance:  785.38  on 883  degrees of freedom
# AIC: 801.38
# 
# mod3:
#   ====
#   Null deviance: 1186.7  on 890  degrees of freedom
# Residual deviance:  789.4  on 884  degrees of freedom
# AIC: 803.4
# 
# mod4:
#   ====
#   Null deviance: 1186.7  on 890  degrees of freedom
# Residual deviance:  790.7  on 885  degrees of freedom
# AIC: 802.7

######---F I N A L M O D E L IS mod3--##########

titanic.test_new$Pclass<-as.factor(titanic.test_new$Pclass)

titanic.test_new$Sex<-ifelse(titanic.test_new$Sex=="female","F","M")

pred<-predict(mod4,titanic.test_new,type = "response")

head(pred)

pred2<-prediction(predict(mod4,newdata=titanic.test_new,type = "response"),titanic.test_new$Survived)

prop.table(table(titanic.train_new$Survived))

pred<-ifelse(pred<=0.383,1,0)

titanic.test_new$Survived<-pred


length(pred)

final<-data.frame(titanic.test_new$PassengerId,titanic.test_new$Survived)

names(final)<-c("PassengerId","Survived")

write.csv(final,file = "Test_predictions.csv",row.names = F)
names(Final.submission)
