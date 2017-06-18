library(dplyr)
library(caret)
library(irr)
library(ggplot2)
library(ggthemes)
library(gains)
options(scipen=999)

setwd("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 13 -  Final Case Study Course Wrap up")

tele_data<-read.csv("telecomfinal.csv",stringsAsFactors = T)

##-----------------------DATA PREPARATION(Quality Report)--------------------------------------------------------------------------------------#

str(tele_data)
sort(names(tele_data))->co_names

#-----------numeric variables col numbers------
for(i in 1:length(co_names))
{data_type[i]<-class(tele_data[,co_names[i]])}

num_var<-which(data_type=="numeric")
fact_var<-which(data_type=="factor")
int_var<-which(data_type=="integer")

length(num_var)
##35 variables are numeric

length(fact_var)
#21 variables are factor

length(int_var)
#25 variables are integer.

numbers<-c(num_var,int_var)
 #-------------Colnames Initilization----------------------# 

obs_name<-c()
no_obs<-c()
data_type<-c()
uniqueRecords<-c()
data_available<-c()
missing<-c()


pct1<-c(length=81)#5th
length(pct1)<-81

pct2<-c(length=81)#10th
length(pct2)<-81

pct3<-c(length=81)#25th
length(pct3)<-81


pct4<-c(length=81)#50
length(pct4)<-81


pct5<-c(length=81)#75
length(pct5)<-81


pct6<-c(length=81)#90
length(pct6)<-8

pct7<-c(length=81)#95
length(pct7)<-81




for(i in 1:length(co_names)){
  obs_name[i]<-co_names[i]
  no_obs[i]<-length(tele_data[,co_names[i]])
  data_type[i]<-class(tele_data[,co_names[i]])
  uniqueRecords[i]<-length(unique(tele_data[,co_names[i]]))
  data_available[i]<-sum(complete.cases(tele_data[,co_names[i]]))
  missing[i]<-sum(is.na(tele_data[,co_names[i]]))
  }
 
#--------------For calculating min values--------------------
  
min_val<-vector(mode="numeric",length = 81)
  for(i in num_var){
  min_val[i]<-min(tele_data[,co_names[i]],na.rm = T)
  }

  for(i in int_var){
    min_val[i]<-min(tele_data[,co_names[i]],na.rm = T)
  }
  #--------------For calculating max values--------------------
max_val<-vector(mode="numeric",length = 81)
  for(i in num_var){
    max_val[i]<-max(tele_data[,co_names[i]],na.rm = T)
  }
  
  for(i in int_var){
    max_val[i]<-max(tele_data[,co_names[i]],na.rm = T)
  }
  
#------------------------------MeanVaues for all Variables-----#
mean_val<-vector(mode="numeric",length = 81)
for(i in num_var){
  mean_val[i]<-mean(tele_data[,co_names[i]],na.rm = T)
}

for(i in int_var){
  mean_val[i]<-mean(tele_data[,co_names[i]],na.rm = T)
}
#---------------------------caluculating percentiles--------------------------------
quantile_var<-vector(length = 81)
quantile(tele_data[,co_names[1]],p=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm = T)



#--------------------------------5th Percentile-------------------
for(i in num_var){
  pct1[i]<-quantile(tele_data[,co_names[i]],p=c(0.05),na.rm = T)
}

for(i in int_var){
  pct1[i]<-quantile(tele_data[,co_names[i]],p=c(0.05),na.rm = T)
}


#--------------------------------10th Percentile-------------------
for(i in num_var){
  pct2[i]<-quantile(tele_data[,co_names[i]],p=c(0.10),na.rm = T)
}

for(i in int_var){
  pct2[i]<-quantile(tele_data[,co_names[i]],p=c(0.10),na.rm = T)
}

#--------------------------------25th Percentile-------------------
for(i in num_var){
  pct3[i]<-quantile(tele_data[,co_names[i]],p=c(0.25),na.rm = T)
}

for(i in int_var){
  pct3[i]<-quantile(tele_data[,co_names[i]],p=c(0.25),na.rm = T)
}

#--------------------------------50th Percentile-------------------
for(i in num_var){
  pct4[i]<-quantile(tele_data[,co_names[i]],p=c(0.5),na.rm = T)
}

for(i in int_var){
  pct4[i]<-quantile(tele_data[,co_names[i]],p=c(0.5),na.rm = T)
}

#--------------------------------75th Percentile-------------------
for(i in num_var){
  pct5[i]<-quantile(tele_data[,co_names[i]],p=c(0.75),na.rm = T)
}

for(i in int_var){
  pct5[i]<-quantile(tele_data[,co_names[i]],p=c(0.75),na.rm = T)
}

#--------------------------------90th Percentile-------------------
for(i in num_var){
  pct6[i]<-quantile(tele_data[,co_names[i]],p=c(0.9),na.rm = T)
}

for(i in int_var){
  pct6[i]<-quantile(tele_data[,co_names[i]],p=c(0.9),na.rm = T)
}


#--------------------------------95th Percentile-------------------
for(i in num_var){
  pct7[i]<-quantile(tele_data[,co_names[i]],p=c(0.95),na.rm = T)
}

for(i in int_var){
  pct7[i]<-quantile(tele_data[,co_names[i]],p=c(0.95),na.rm = T)
}



dataquality<-data.frame(obs_name,no_obs, data_type, uniqueRecords,data_available, missing,Minvalue=round(min_val,2),MaxValue=round(max_val,2),
                        round(pct1,2),round(pct2,2),round(pct3,2),round(pct4,2),round(pct5,2),round(pct6,2),round(pct7,2))



pct1<-ifelse(is.na(pct1),0,pct1)
pct2<-ifelse(is.na(pct2),0,pct1)
pct3<-ifelse(is.na(pct3),0,pct1)
pct4<-ifelse(is.na(pct4),0,pct1)
pct5<-ifelse(is.na(pct5),0,pct1)
pct6<-ifelse(is.na(pct6),0,pct1)
pct7<-ifelse(is.na(pct7),0,pct1)

setwd("C:\\Jig13188\\Topic 13")
write.csv(dataquality,"qualityReport.csv")

# forgntvl-Foreign travel dummy variable.
# ;mtrcycle;truck are seems to be categorical variables. (Min: 0 , Max:1)




#--------------------------------------------------END OF Quality Report Preparation----------------------------------------------#

#----------------------------Continuous variable profiling-----------------------------

a<-vector(length = 60)
for(i in 1:length(numbers)){
  a[i]<-paste(co_names[numbers][i],numbers[i],sep="-dat")
}


cont_profiling<-rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat9,dat10,dat11,dat12,dat13,dat14,dat15,dat16,dat17,dat18,dat21,dat22,dat25,dat26,
                      dat29,dat31,dat32,dat33,dat34,dat36,dat37,dat38,dat39,dat42,dat44,dat45,dat47,dat48,dat52,dat53,dat54,dat55,dat56,dat57,
                      dat58,dat59,dat61,dat62,dat63,dat64,dat65,dat66,dat69,dat71,dat72,dat73,dat74,dat76,dat77,dat78,dat79,dat80)

# [1] "actvsubs-dat1"          "adjmou-dat2"            "adjqty-dat3"            "adjrev-dat4"           
# [5] "age1-dat5"              "age2-dat6"              "avg3mou-dat9"           "avg3qty-dat10"         
# [9] "avg6mou-dat11"          "avg6qty-dat12"          "avgmou-dat13"           "avgqty-dat14"          
# [13] "avgrev-dat15"           "blck_dat_Mean-dat16"    "callwait_Mean-dat17"    "callwait_Range-dat18"
#[17] "ccrndmou_Range-dat21"   "change_mou-dat22"       "(DV)churn-dat24"            "comp_dat_Mean-dat25"   
# [21] "comp_vce_Mean-dat26"    "custcare_Mean-dat29"    "(NU)Customer_ID-dat30"      "da_Mean-dat31" 
# [25] "da_Range-dat32"         "datovr_Mean-dat33"      "datovr_Range-dat34"     "drop_blk_Mean-dat36"
# [29] "drop_dat_Mean-dat37"    "drop_vce_Mean-dat38"    "drop_vce_Range-dat39"   "****eqpdays-dat42"         
# [33] "forgntvl-dat44"         "hnd_price-dat45"        "income-dat47"           "iwylis_vce_Mean-dat48" 
# [37] "models-dat52"           "months-dat53"           "mou_Mean-dat54"         "mou_opkv_Range-dat55"  
# [41] "mou_pead_Mean-dat56"    "mou_Range-dat57"        "mtrcycle-dat58"         "numbcars-dat59"        
# [45] "opk_dat_Mean-dat61"     "ovrmou_Mean-dat62"      "ovrrev_Mean-dat63"      "owylis_vce_Range-dat64"
# [49] "plcd_dat_Mean-dat65"    "**plcd_vce_Mean-dat66"    "recv_sms_Mean-dat69"    "retdays-dat71"         
# [53] "rev_Mean-dat72"         "rev_Range-dat73"        "roam_Mean-dat74"        "totcalls-dat76"        
# [57] "totmrc_Mean-dat77"      "totrev-dat78"           "truck-dat79"            "uniqsubs-dat80" 
library(dplyr)
#dat52
b<-10
save.image()


tele_data%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat42

dat42$N<-unclass(tele_data%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]

dat42$churn_perc<-dat42$n/dat42$N

dat42$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]

dat42$LessThan<-unclass(tele_data%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]

#----------------------------------------------------------------------------------
tele_data%>%mutate(dec=ntile(models,n=3))%>%count(churn,dec)%>%filter(churn==1)->dat52

dat52$N<-unclass(tele_data%>%mutate(dec=ntile(models,n=3))%>%count(dec)%>%unname())[[2]]

dat52$churn_perc<-dat52$n/dat52$N

dat52$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(models,n=3))%>%group_by(dec)%>%summarise(min(models)))[[2]]

dat52$LessThan<-unclass(tele_data%>%mutate(dec=ntile(models,n=3))%>%group_by(dec)%>%summarise(max(models)))[[2]]

dat52$varname<-rep("models",nrow(dat52))
#-------------------------------
tele_data%>%mutate(dec=ntile(uniqsubs,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat80

dat80$N<-unclass(tele_data%>%mutate(dec=ntile(uniqsubs,n=2))%>%count(dec)%>%unname())[[2]]

dat80$churn_perc<-dat80$n/dat80$N

dat80$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(uniqsubs,n=2))%>%group_by(dec)%>%summarise(min(uniqsubs)))[[2]]

dat80$LessThan<-unclass(tele_data%>%mutate(dec=ntile(uniqsubs,n=2))%>%group_by(dec)%>%summarise(max(uniqsubs)))[[2]]

dat80$varname<-rep("uniqsubs",nrow(dat80))
#---------------------------------------------
tele_data%>%mutate(dec=ntile(avg6qty,n=b))%>%count(churn,dec)%>%filter(churn==1)->dat12

dat12$N<-unclass(tele_data%>%mutate(dec=ntile(avg6qty,n=b))%>%count(dec)%>%unname())[[2]]

dat12$churn_perc<-dat12$n/dat12$N

dat12$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(avg6qty,n=b))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]

dat12$LessThan<-unclass(tele_data%>%mutate(dec=ntile(avg6qty,n=b))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]

dat12$varname<-rep("avg6qty",nrow(dat12))
#-------------------------------------
tele_data%>%mutate(dec=ntile(truck,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat79

dat79$N<-unclass(tele_data%>%mutate(dec=ntile(truck,n=2))%>%count(dec)%>%unname())[[2]]

dat79$churn_perc<-dat79$n/dat79$N

dat79$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(truck,n=2))%>%group_by(dec)%>%summarise(min(truck)))[[2]]

dat79$LessThan<-unclass(tele_data%>%mutate(dec=ntile(truck,n=2))%>%group_by(dec)%>%summarise(max(truck)))[[2]]

dat79$varname<-rep("truck",nrow(dat79))
#----------------------------------------------------------
tele_data%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat78

dat78$N<-unclass(tele_data%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]

dat78$churn_perc<-dat78$n/dat78$N

dat78$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]

dat78$LessThan<-unclass(tele_data%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]

dat78$varname<-rep("totrev",nrow(dat78))
#------------------------
tele_data%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat77

dat77$N<-unclass(tele_data%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]

dat77$churn_perc<-dat77$n/dat77$N

dat77$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]

dat77$LessThan<-unclass(tele_data%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]

dat77$varname<-rep("totmrc_Mean",nrow(dat77))
#---------------------------------------------

tele_data%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat76

dat76$N<-unclass(tele_data%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]

dat76$churn_perc<-dat76$n/dat76$N

dat76$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]

dat76$LessThan<-unclass(tele_data%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]

dat76$varname<-rep("totcalls",nrow(dat76))
#---------------
tele_data%>%mutate(dec=ntile(roam_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat74

dat74$N<-unclass(tele_data%>%mutate(dec=ntile(roam_Mean,n=2))%>%count(dec)%>%unname())[[2]]

dat74$churn_perc<-dat74$n/dat74$N

dat74$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(roam_Mean,n=2))%>%group_by(dec)%>%summarise(min(roam_Mean)))[[2]]

dat74$LessThan<-unclass(tele_data%>%mutate(dec=ntile(roam_Mean,n=2))%>%group_by(dec)%>%summarise(max(roam_Mean)))[[2]]

dat74$varname<-rep("roam_Mean",nrow(dat74))
#------------------------------------------
tele_data%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat73

dat73$N<-unclass(tele_data%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]

dat73$churn_perc<-dat73$n/dat73$N

dat73$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]

dat73$LessThan<-unclass(tele_data%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]

dat73$varname<-rep("rev_Range",nrow(dat73))
#---------------------------------------------------
tele_data%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat72

dat72$N<-unclass(tele_data%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]

dat72$churn_perc<-dat72$n/dat72$N

dat72$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]

dat72$LessThan<-unclass(tele_data%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]

dat72$varname<-rep("rev_Mean",nrow(dat72))
#----------------------------------------------
tele_data%>%mutate(dec=ntile(retdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat71

dat71$N<-unclass(tele_data%>%mutate(dec=ntile(retdays,n=10))%>%count(dec)%>%unname())[[2]]

dat71$churn_perc<-dat71$n/dat71$N

dat71$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(retdays,n=10))%>%group_by(dec)%>%summarise(min(retdays)))[[2]]

dat71$LessThan<-unclass(tele_data%>%mutate(dec=ntile(retdays,n=10))%>%group_by(dec)%>%summarise(max(retdays)))[[2]]

dat71$varname<-rep("retdays",nrow(dat71))
#---------------------------------------
tele_data%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat69

dat69$N<-unclass(tele_data%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%count(dec)%>%unname())[[2]]

dat69$churn_perc<-dat69$n/dat69$N

dat69$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%group_by(dec)%>%summarise(min(recv_sms_Mean)))[[2]]

dat69$LessThan<-unclass(tele_data%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%group_by(dec)%>%summarise(max(recv_sms_Mean)))[[2]]

dat69$varname<-rep("recv_sms_Mean",nrow(dat69))
#-------------------------
tele_data%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat66

dat66$N<-unclass(tele_data%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]

dat66$churn_perc<-dat66$n/dat66$N

dat66$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]

dat66$LessThan<-unclass(tele_data%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]

dat66$varname<-rep("plcd_vce_Mean",nrow(dat66))
#------------------------------------------------
tele_data%>%mutate(dec=ntile(owylis_vce_Range,n=b))%>%count(churn,dec)%>%filter(churn==1)->dat64

dat64$N<-unclass(tele_data%>%mutate(dec=ntile(owylis_vce_Range,n=b))%>%count(dec)%>%unname())[[2]]

dat64$churn_perc<-dat64$n/dat64$N

dat64$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(owylis_vce_Range,n=b))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]

dat64$LessThan<-unclass(tele_data%>%mutate(dec=ntile(owylis_vce_Range,n=b))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]

dat64$varname<-rep("owylis_vce_Range",nrow(dat64))
#-----------------------------------------
tele_data%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat63

dat63$N<-unclass(tele_data%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(dec)%>%unname())[[2]]

dat63$churn_perc<-dat63$n/dat63$N

dat63$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]

dat63$LessThan<-unclass(tele_data%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]

dat63$varname<-rep("ovrrev_Mean",nrow(dat63))
#------------------------------------------------------------------------
tele_data%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat62

dat62$N<-unclass(tele_data%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(dec)%>%unname())[[2]]

dat62$churn_perc<-dat62$n/dat62$N

dat62$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]

dat62$LessThan<-unclass(tele_data%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]

dat62$varname<-rep("ovrmou_Mean",nrow(dat62))
#-------------------------------------------
tele_data%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat61

dat61$N<-unclass(tele_data%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]

dat61$churn_perc<-dat61$n/dat61$N

dat61$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(opk_dat_Mean)))[[2]]

dat61$LessThan<-unclass(tele_data%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(opk_dat_Mean)))[[2]]

dat61$varname<-rep("opk_dat_Mean",nrow(dat61))
#-------------------------------------------------
tele_data%>%mutate(dec=ntile(numbcars,n=3))%>%count(churn,dec)%>%filter(churn==1)->dat59

dat59$N<-unclass(tele_data%>%mutate(dec=ntile(numbcars,n=3))%>%count(dec)%>%unname())[[2]]

dat59$churn_perc<-dat59$n/dat59$N

dat59$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(numbcars,n=3))%>%group_by(dec)%>%summarise(min(numbcars)))[[2]]

dat59$LessThan<-unclass(tele_data%>%mutate(dec=ntile(numbcars,n=3))%>%group_by(dec)%>%summarise(max(numbcars)))[[2]]

dat59$varname<-rep("numbcars",nrow(dat59))
#------------------------------------------------
tele_data%>%mutate(dec=ntile(mtrcycle,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat58

dat58$N<-unclass(tele_data%>%mutate(dec=ntile(mtrcycle,n=2))%>%count(dec)%>%unname())[[2]]

dat58$churn_perc<-dat58$n/dat58$N

dat58$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(mtrcycle,n=2))%>%group_by(dec)%>%summarise(min(mtrcycle)))[[2]]

dat58$LessThan<-unclass(tele_data%>%mutate(dec=ntile(mtrcycle,n=2))%>%group_by(dec)%>%summarise(max(mtrcycle)))[[2]]

dat58$varname<-rep("mtrcycle",nrow(dat58))
#------------------------------------------
tele_data%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat57

dat57$N<-unclass(tele_data%>%mutate(dec=ntile(mou_Range,n=b))%>%count(dec)%>%unname())[[2]]

dat57$churn_perc<-dat57$n/dat57$N

dat57$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(mou_Range,n=b))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]

dat57$LessThan<-unclass(tele_data%>%mutate(dec=ntile(mou_Range,n=b))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]

dat57$varname<-rep("mou_Range",nrow(dat57))
#---------------------------
tele_data%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat56

dat56$N<-unclass(tele_data%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%count(dec)%>%unname())[[2]]

dat56$churn_perc<-dat56$n/dat56$N

dat56$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%group_by(dec)%>%summarise(min(mou_pead_Mean)))[[2]]

dat56$LessThan<-unclass(tele_data%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%group_by(dec)%>%summarise(max(mou_pead_Mean)))[[2]]

dat56$varname<-rep("mou_pead_Mean",nrow(dat56))
#------------------------------------
tele_data%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat55

dat55$N<-unclass(tele_data%>%mutate(dec=ntile(mou_opkv_Range,n=b))%>%count(dec)%>%unname())[[2]]

dat55$churn_perc<-dat55$n/dat55$N

dat55$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(mou_opkv_Range,n=b))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]

dat55$LessThan<-unclass(tele_data%>%mutate(dec=ntile(mou_opkv_Range,n=b))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]

dat55$varname<-rep("mou_opkv_Range",nrow(dat55))
#------------------------------------
tele_data%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat54

dat54$N<-unclass(tele_data%>%mutate(dec=ntile(mou_Mean,n=b))%>%count(dec)%>%unname())[[2]]

dat54$churn_perc<-dat54$n/dat54$N

dat54$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(mou_Mean,n=b))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]

dat54$LessThan<-unclass(tele_data%>%mutate(dec=ntile(mou_Mean,n=b))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]

dat54$varname<-rep("mou_Mean",nrow(dat54))
#---------------------------------------------
tele_data%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat53

dat53$N<-unclass(tele_data%>%mutate(dec=ntile(months,n=b))%>%count(dec)%>%unname())[[2]]

dat53$churn_perc<-dat53$n/dat53$N

dat53$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(months,n=b))%>%group_by(dec)%>%summarise(min(months)))[[2]]

dat53$LessThan<-unclass(tele_data%>%mutate(dec=ntile(months,n=b))%>%group_by(dec)%>%summarise(max(months)))[[2]]

dat53$varname<-rep("months",nrow(dat53))
#-------------------------------------------------------

tele_data%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat48

dat48$N<-unclass(tele_data%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(dec)%>%unname())[[2]]

dat48$churn_perc<-dat48$n/dat48$N

dat48$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]

dat48$LessThan<-unclass(tele_data%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]

dat48$varname<-rep("iwylis_vce_Mean",nrow(dat48))
#-------------------------------------------------------

tele_data%>%mutate(dec=ntile(income,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat47

dat47$N<-unclass(tele_data%>%mutate(dec=ntile(income,n=6))%>%count(dec)%>%unname())[[2]]

dat47$churn_perc<-dat47$n/dat47$N

dat47$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(income,n=6))%>%group_by(dec)%>%summarise(min(income)))[[2]]

dat47$LessThan<-unclass(tele_data%>%mutate(dec=ntile(income,n=6))%>%group_by(dec)%>%summarise(max(income)))[[2]]

dat47$varname<-rep("income",nrow(dat47))
#-----------------------------------------------------

tele_data%>%mutate(dec=ntile(hnd_price,n=b))%>%count(churn,dec)%>%filter(churn==1)->dat45

dat45$N<-unclass(tele_data%>%mutate(dec=ntile(hnd_price,n=b))%>%count(dec)%>%unname())[[2]]

dat45$churn_perc<-dat45$n/dat45$N

dat45$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(hnd_price,n=b))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]

dat45$LessThan<-unclass(tele_data%>%mutate(dec=ntile(hnd_price,n=b))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]

dat45$varname<-rep("hnd_price",nrow(dat45))
#------------------------------------------

tele_data%>%mutate(dec=ntile(forgntvl,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat44

dat44$N<-unclass(tele_data%>%mutate(dec=ntile(forgntvl,n=2))%>%count(dec)%>%unname())[[2]]

dat44$churn_perc<-dat44$n/dat44$N

dat44$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(forgntvl,n=2))%>%group_by(dec)%>%summarise(min(forgntvl)))[[2]]

dat44$LessThan<-unclass(tele_data%>%mutate(dec=ntile(forgntvl,n=2))%>%group_by(dec)%>%summarise(max(forgntvl)))[[2]]

dat44$varname<-rep("forgntvl",nrow(dat44))
#------------------------------------------------------
tele_data%>%mutate(dec=ntile(drop_vce_Range,n=b))%>%count(churn,dec)%>%filter(churn==1)->dat39

dat39$N<-unclass(tele_data%>%mutate(dec=ntile(drop_vce_Range,n=b))%>%count(dec)%>%unname())[[2]]

dat39$churn_perc<-dat39$n/dat39$N

dat39$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(drop_vce_Range,n=b))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]

dat39$LessThan<-unclass(tele_data%>%mutate(dec=ntile(drop_vce_Range,n=b))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]

dat39$varname<-rep("drop_vce_Range",nrow(dat39))
#-------------------------------------------
tele_data%>%mutate(dec=ntile(drop_vce_Range,n=b))%>%count(churn,dec)%>%filter(churn==1)->dat39

dat39$N<-unclass(tele_data%>%mutate(dec=ntile(drop_vce_Range,n=b))%>%count(dec)%>%unname())[[2]]

dat39$churn_perc<-dat39$n/dat39$N

dat39$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(drop_vce_Range,n=b))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]

dat39$LessThan<-unclass(tele_data%>%mutate(dec=ntile(drop_vce_Range,n=b))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]

dat39$varname<-rep("drop_vce_Range",nrow(dat39))
#--------------------------------------------------
tele_data%>%mutate(dec=ntile(drop_vce_Mean,n=b))%>%count(churn,dec)%>%filter(churn==1)->dat38

dat38$N<-unclass(tele_data%>%mutate(dec=ntile(drop_vce_Mean,n=b))%>%count(dec)%>%unname())[[2]]

dat38$churn_perc<-dat38$n/dat38$N

dat38$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(drop_vce_Mean,n=b))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]

dat38$LessThan<-unclass(tele_data%>%mutate(dec=ntile(drop_vce_Mean,n=b))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]

dat38$varname<-rep("drop_vce_Mean",nrow(dat38))
#---------------------------------------------------
tele_data%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat37

dat37$N<-unclass(tele_data%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]

dat37$churn_perc<-dat37$n/dat37$N

dat37$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]

dat37$LessThan<-unclass(tele_data%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]

dat37$varname<-rep("drop_dat_Mean",nrow(dat37))
#-----------------------------------
tele_data%>%mutate(dec=ntile(drop_blk_Mean,n=b))%>%count(churn,dec)%>%filter(churn==1)->dat36

dat36$N<-unclass(tele_data%>%mutate(dec=ntile(drop_blk_Mean,n=b))%>%count(dec)%>%unname())[[2]]

dat36$churn_perc<-dat36$n/dat36$N

dat36$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(drop_blk_Mean,n=b))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]

dat36$LessThan<-unclass(tele_data%>%mutate(dec=ntile(drop_blk_Mean,n=b))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]

dat36$varname<-rep("drop_blk_Mean",nrow(dat36))
#--------------------------------------
tele_data%>%mutate(dec=ntile(datovr_Range,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat34

dat34$N<-unclass(tele_data%>%mutate(dec=ntile(datovr_Range,n=2))%>%count(dec)%>%unname())[[2]]

dat34$churn_perc<-dat34$n/dat34$N

dat34$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(datovr_Range,n=2))%>%group_by(dec)%>%summarise(min(datovr_Range)))[[2]]

dat34$LessThan<-unclass(tele_data%>%mutate(dec=ntile(datovr_Range,n=2))%>%group_by(dec)%>%summarise(max(datovr_Range)))[[2]]

dat34$varname<-rep("datovr_Range",nrow(dat34))
#--------------------------------------------------------
tele_data%>%mutate(dec=ntile(datovr_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat33

dat33$N<-unclass(tele_data%>%mutate(dec=ntile(datovr_Mean,n=2))%>%count(dec)%>%unname())[[2]]

dat33$churn_perc<-dat33$n/dat33$N

dat33$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(datovr_Mean,n=2))%>%group_by(dec)%>%summarise(min(datovr_Mean)))[[2]]

dat33$LessThan<-unclass(tele_data%>%mutate(dec=ntile(datovr_Mean,n=2))%>%group_by(dec)%>%summarise(max(datovr_Mean)))[[2]]

dat33$varname<-rep("datovr_Mean",nrow(dat33))

#-------------------------------
tele_data%>%mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat32

dat32$N<-unclass(tele_data%>%mutate(dec=ntile(da_Range,n=4))%>%count(dec)%>%unname())[[2]]

dat32$churn_perc<-dat32$n/dat32$N

dat32$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]

dat32$LessThan<-unclass(tele_data%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]

dat32$varname<-rep("da_Range",nrow(dat32))
#-------------------------------
tele_data%>%mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat31

dat31$N<-unclass(tele_data%>%mutate(dec=ntile(da_Mean,n=4))%>%count(dec)%>%unname())[[2]]

dat31$churn_perc<-dat31$n/dat31$N

dat31$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]

dat31$LessThan<-unclass(tele_data%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]

dat31$varname<-rep("da_Mean",nrow(dat31))
#------------------------------------------------------------------------------------------
tele_data%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%count(churn,dec)%>%filter(churn==1)->dat21

dat21$N<-unclass(tele_data%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%count(dec)%>%unname())[[2]]

dat21$churn_perc<-dat21$n/dat21$N

dat21$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%group_by(dec)%>%summarise(min(ccrndmou_Range)))[[2]]

dat21$LessThan<-unclass(tele_data%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%group_by(dec)%>%summarise(max(ccrndmou_Range)))[[2]]

dat21$varname<-rep("ccrndmou_Range",nrow(dat21))
#-------------------
tele_data%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat22

dat22$N<-unclass(tele_data%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]

dat22$churn_perc<-dat22$n/dat22$N

dat22$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]

dat22$LessThan<-unclass(tele_data%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]

dat22$varname<-rep("change_mou",nrow(dat22))
#-------------------------------------------
tele_data%>%mutate(dec=ntile(comp_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat25

dat25$N<-unclass(tele_data%>%mutate(dec=ntile(comp_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]

dat25$churn_perc<-dat25$n/dat25$N

dat25$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(comp_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(comp_dat_Mean)))[[2]]

dat25$LessThan<-unclass(tele_data%>%mutate(dec=ntile(comp_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(comp_dat_Mean)))[[2]]
dat4
dat25$varname<-rep("comp_dat_Mean",nrow(dat25))
#----------------------------------------------
tele_data%>%mutate(dec=ntile(comp_vce_Mean,n=b))%>%count(churn,dec)%>%filter(churn==1)->dat26

dat26$N<-unclass(tele_data%>%mutate(dec=ntile(comp_vce_Mean,n=b))%>%count(dec)%>%unname())[[2]]

dat26$churn_perc<-dat26$n/dat26$N

dat26$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(comp_vce_Mean,n=b))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]

dat26$LessThan<-unclass(tele_data%>%mutate(dec=ntile(comp_vce_Mean,n=b))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]

dat26$varname<-rep("comp_vce_Mean",nrow(dat26))
#---------------------------
tele_data%>%mutate(dec=ntile(custcare_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat29

dat29$N<-unclass(tele_data%>%mutate(dec=ntile(custcare_Mean,n=2))%>%count(dec)%>%unname())[[2]]

dat29$churn_perc<-dat29$n/dat29$N

dat29$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(custcare_Mean,n=2))%>%group_by(dec)%>%summarise(min(custcare_Mean)))[[2]]

dat29$LessThan<-unclass(tele_data%>%mutate(dec=ntile(custcare_Mean,n=2))%>%group_by(dec)%>%summarise(max(custcare_Mean)))[[2]]

dat29$varname<-rep("custcare_Mean",nrow(dat29))
#------------------------------------------------

#--- Preparing CSV file of profiling of continuous variables-------------------

setwd("C:\\Jig13188\\Topic 13")
write.csv(cont_profiling,"cont_profiling.csv")

cont_profiling<-rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat9,dat10,dat11,dat12,dat13,dat14,dat15,dat16,dat17,dat18,dat21,dat22,dat25,dat26,
                      dat29,dat31,dat32,dat33,dat34,dat36,dat37,dat38,dat39,dat42,dat44,dat45,dat47,dat48,dat53,dat54,dat55,dat56,dat57,
                      dat58,dat59,dat61,dat62,dat63,dat64,dat65,dat66,dat69,dat71,dat72,dat73,dat74,dat76,dat77,dat78,dat79,dat80)

length(unique(cont_profiling$varname))
#[1] 58

setwd("C:\\Jig13188\\Topic 13")
write.csv(cont_profiling,"ContVarProfiling.csv")
#------------------------------------

#--- Before converting some variables into factor explicitly-------------#
b<-vector(length = length(fact_var1))
for(i in 1:length(fact_var1)){
  b[i]<-paste(co_names[fact_var1][i],fact_var1[i],sep="-datC")
}


tele_data$forgntvl<-as.factor(tele_data$forgntvl)
tele_data$actvsubs<-as.factor(tele_data$actvsubs)
tele_data$truck<-as.factor(tele_data$truck)
tele_data$churn<-as.factor(tele_data$churn)
 
#--- After converting some variables into factor explicitly-------------#

fact_var1<-vector()
for(i in 1:length(co_names))
   {data_type1[i]<-class(tele_data[,co_names[i]])}
fact_var1<-which(data_type1=="factor")
length(fact_var1)

# [1] "actvsubs-datC1"          "area-datC7"              "asl_flag-datC8"          "car_buy-datC19"          "cartype-datC20"         
# [6] "children-datC23"         "(DV)churn-datC24"            "crclscod-datC27"         "csa-datC28"              "div_type-datC35"        
# [11] "dwllsize-datC40"         "dwlltype-datC41"         "ethnic-datC43"           "forgntvl-datC44"         "hnd_webcap-datC46"      
# [16] "mailordr-datC49"         "mailresp-datC50"         "marital-datC51"          "occu1-datC60"            "prizm_social_one-datC67"
# [21] "proptype-datC68"         "refurb_new-datC70"       "solflag-datC75"          "truck-datC79"            "wrkwoman-datC81"        
  

tele_data %>% count(churn,levels=actvsubs) %>% filter(churn==1)->datC1

datC1$N<-unclass(tele_data%>% filter(actvsubs %in% datC1$levels)%>%count(actvsubs))[[2]]

datC1$ChurnPerc<-datC1$n/datC1$N

datC1$Var.Name<-rep("actvsubs",nrow(datC1))

#----------------------Creatign Csv file for Categorical Variable profiling

cat_var_profiling<-rbind(datC1,datC7,datC8,datC19,
                        datC20,datC23,datC27,datC28,datC35,datC40,datC41,datC43,datC44,datC46,datC49,datC50,datC51,datC60,datC67,datC68,datC70,datC75,
                        datC75,datC79,datC81)
length(unique(cat_var_profiling$Var.Name))
setwd("C:/Jig13188/Topic 13")
write.csv(cat_var_profiling,"Categoircal_var_prof.csv")

#Variables following trend which are continous(29)
# adjmou,adjqty,adjrev,age1,age2,avg3mou,avg3qty,avg6mou,avg6qty,callwait_Mean
#callwait_Range,ccrndmou_Range,change_mou,comp_vce_Mean,da_Range,drop_blk_Mean
#drop_vce_Mean,eqpdays,hnd_price,income,mou_Mean,mou_opkv_Range,ovrmou_Mean,owylis_vce_Range
#plcd_dat_Mean,plcd_vce_Mean,retdays,roam_Mean,totcalls,totrev

#variables almost following Trend(Increasing/Decreasing Churn Rate)

# adjmou,adjqty,adjrev,age1,age2,avg3mou,avg3qty,avg6mou,callwait_Mean
#callwait_Range,ccrndmou_Range,comp_vce_Mean,da_Range,drop_blk_Mean
#mou_Mean,mou_opkv_Range
#plcd_vce_Mean,retdays,totcalls,totrev





#Categoricla varible selection for my final model.
#I have ommited those variables which has missing values >15% because they might not be significant... !

#dwlltype,dwllsize,mailresp,mailordr,children,cartype,proptype,occu1,div_type,wrkwoman,solflag--- ommiting variables 

#Creating new dataframe with all those variales which are shortlisted based on certain constraints.

# [1] "actvsubs-datC1"          "area-datC7"              "asl_flag-datC8"          "car_buy-datC19"          
# [6]        "(DV)churn-datC24"            "crclscod-datC27"         "csa-datC28"                   
# [11]                  "ethnic-datC43"           "forgntvl-datC44"         "hnd_webcap-datC46"      
# [16]              "marital-datC51"                    "prizm_social_one-datC67"
      #  "refurb_new-datC70"               "truck-datC79"               





teledata_final<-select(tele_data,adjmou,adjqty,adjrev,age1,age2,avg3mou,avg3qty,avg6mou,callwait_Mean,
                       callwait_Range,ccrndmou_Range,comp_vce_Mean,da_Range,drop_blk_Mean,mou_Mean,mou_opkv_Range,
                       plcd_vce_Mean,totcalls,totrev,actvsubs,area,asl_flag,car_buy,ethnic,forgntvl,hnd_webcap,
                       marital,prizm_social_one,refurb_new,truck,churn)

#----------------------------REPLACING MISSNG VALUES WITH MEAN/GROUP MEAN--------------------------#
colSums(is.na(teledata_final))
# adjmou           adjqty           adjrev             age1             age2 
# 0                0                0                  1152             1152 
# avg3mou          avg3qty          avg6mou    callwait_Mean   callwait_Range 
# 0                0                 2056                0                0 
# ccrndmou_Range    comp_vce_Mean         da_Range    drop_blk_Mean         mou_Mean 
# 0                0                       181                0              181 
# mou_opkv_Range    plcd_vce_Mean         totcalls           totrev         actvsubs 
#          0                0                0                0                0 
# area         asl_flag          car_buy           ethnic         forgntvl 
# 19                1             1153             1153             1152 
# hnd_webcap         marital  prizm_social_one       refurb_new            truck 
# 6063                1153             4752                1                 1152 
# churn 
# 0 

#Most of the variabels missing percentage is 2. so i am using here mean imputation.

teledata_final[which(is.na(teledata_final$age1)),"age1"]<-mean(teledata_final$age1,na.rm = T)

teledata_final[which(is.na(teledata_final$age2)),"age2"]<-mean(teledata_final$age2,na.rm = T)

teledata_final[which(is.na(teledata_final$avg6mou)),"avg6mou"]<-mean(teledata_final$avg6mou,na.rm = T)

teledata_final[which(is.na(teledata_final$da_Range)),"da_Range"]<-mean(teledata_final$da_Range,na.rm = T)

teledata_final[which(is.na(teledata_final$mou_Mean)),"mou_Mean"]<-mean(teledata_final$mou_Mean,na.rm = T)

teledata_final$churn<-tele_data$churn

#----------------categorical Variables----------------#

teledata_final$churn<-tele_data$churn #Before this step once read file from source directory.

teledata_final$area<-tele_data$area #Before this step once read file from source directory.

sum(is.na(teledata_final$area))

tab<-table(tele_data$area,teledata_final$churn)

tab_pct<-tab/rowSums(tab)

index<-which(is.na(teledata_final$area))

table(teledata_final$churn[index]) #4/18=0.22

#Missing values group nearer to HOUSTON AREA   group churn rate 1. so Replacing with HOUSTON AREA 

teledata_final$area<-as.character(teledata_final$area)

teledata_final$area[index]<-"HOUSTON AREA"

unique(teledata_final$area)

#----------
sum(is.na(teledata_final$car_buy))

tab2<-table(teledata_final$car_buy,teledata_final$churn)

tab2_pct<-tab2/rowSums(tab2)

index1<-which(is.na(teledata_final$car_buy))

teledata_final$car_buy<-as.character(teledata_final$car_buy)

table(teledata_final$churn[index1]) #249/(249+904)=0.21

teledata_final$car_buy[index1]<-"New"

teledata_final$car_buy<-as.factor(teledata_final$car_buy)
#------------------

sum(is.na(teledata_final$ethnic))#1153
unique(teledata_final$ethnic)
tab3<-table(teledata_final$ethnic,teledata_final$churn)
tab3_pct<-tab3/rowSums(tab3)
index3<-which(is.na(teledata_final$ethnic))
table(teledata_final$churn[index3]) #> 249/(904+249)=0.2159584
# nearer to "M" 0.77528090 0.22471910. so Missing replace with "M"
teledata_final$ethnic[index3]<-"M"

#----------
teledata_final$forgntvl<-tele_data$forgntvl
sum(is.na(teledata_final$forgntvl))#1152
unique(teledata_final$forgntvl)
tab4<-table(teledata_final$forgntvl,teledata_final$churn)
tab4_pct<-tab4/rowSums(tab4)
index4<-which(is.na(teledata_final$forgntvl))
table(teledata_final$churn[index4])# 249/(903+249)=0.216
#Replacing with 1 because of near percentage.
teledata_final$forgntvl[index4]<-1

#------------------


unique(teledata_final$hnd_webcap)
tab5<-table(teledata_final$hnd_webcap,teledata_final$churn)
tab5_pct<-tab5/rowSums(tab5)
index5<-which(is.na(teledata_final$hnd_webcap))
table(teledata_final$churn[index5])# 1901/(4162+1901)=0.313
#Replacing with "WC" because of near percentage.
teledata_final$hnd_webcap[index5]<-"WC"

#-------------------

unique(teledata_final$marital)
tab6<-table(teledata_final$marital,teledata_final$churn)
tab6_pct<-tab6/rowSums(tab6)
index6<-which(is.na(teledata_final$marital))
table(teledata_final$churn[index6])# 249/(249+904)=0.215
#Replacing with "S" because of near percentage.
teledata_final$marital[index6]<-"S"

#-----------------------------------
unique(teledata_final$prizm_social_one)
tab7<-table(teledata_final$prizm_social_one,teledata_final$churn)
tab7_pct<-tab7/rowSums(tab7)
index7<-which(is.na(teledata_final$prizm_social_one))
table(teledata_final$churn[index7])# 1173/(1173+3579)=0.246
#Replacing with "U" because of near percentage.
teledata_final$prizm_social_one[index7]<-"U"

#-------------

unique(teledata_final$refurb_new)
tab8<-table(teledata_final$refurb_new,teledata_final$churn)
tab8_pct<-tab8/rowSums(tab8)
index8<-which(is.na(teledata_final$refurb_new))
table(teledata_final$churn[index8])
#Replacing with "U" because of near percentage.
teledata_final$refurb_new[index8]<-"N"

#----------

unique(teledata_final$truck)
tab9<-table(teledata_final$truck,teledata_final$churn)
tab9_pct<-tab9/rowSums(tab9)
index9<-which(is.na(teledata_final$truck))
table(teledata_final$churn[index9])# 249/(903+249)=0.216
#Replacing with "U" because of near percentage.
teledata_final$truck[index9]<-1

#----------------END of DATA PREPARATION-----------------------#


k<-vector()
names(teledata_final)->noun

for(i in 1:length(teledata_final))
{
  k[i]<-paste(noun[i],class(teledata_final[,noun[i]]),sep = "-")
}

# [1] "adjmou-numeric"          "adjqty-numeric"          "adjrev-numeric"         
# [4] "age1-numeric"            "age2-numeric"            "avg3mou-numeric"        
# [7] "avg3qty-numeric"         "avg6mou-numeric"         "callwait_Mean-numeric"  
# [10] "callwait_Range-numeric"  "ccrndmou_Range-numeric"  "comp_vce_Mean-numeric"  
# [13] "da_Range-numeric"        "drop_blk_Mean-numeric"   "mou_Mean-numeric"       
# [16] "mou_opkv_Range-numeric"  "plcd_vce_Mean-numeric"   "totcalls-numeric"       
# [19] "totrev-numeric"          "actvsubs-factor"         "area-factor"            
# [22] "asl_flag-factor"         "car_buy-factor"          "ethnic-factor"          
# [25] "forgntvl-factor"         "hnd_webcap-factor"       "marital-factor"         
# [28] "prizm_social_one-factor" "refurb_new-factor"       "truck-factor"           
# [31] "churn-factor"  


teledata_final$forgntvl<-as.factor(teledata_final$forgntvl)
teledata_final$churn<-as.factor((teledata_final$churn))

teledata_final1<-teledata_final
  
#SO MY FINAL OBJECT TO PERFORM MY MODEL ANALYSIS IS teledata_final1 which contain shortlisted variables..

which(lapply(teledata_final1,class)=="factor")

names(which(lapply(teledata_final1,class)=="factor"))
#  

#--------------META DATA FOR CLASSIFICATION VARIABLES----

# "actvsubs" -Number of active subscribers in household
# "area"-Geographic area
# "asl_flag"   -      
# "ethnic" -Ethnicity roll-up code(G = German,J = Jewish,--etc)
         
#  "forgntvl" -Foreign travel dummy variable
# "hnd_webcap" -Handset web capability
    # "marital" -Marital status
         # "prizm_social_one"-Social group letter only

#  "refurb_new"-Handset: refurbished or new
       # "truck"  -Truck indicator
          
# "churn"-Instance of churn between 31-60 days after observation date
          # Dependent Variable
          # 1 = Churned
          # 0 = Did not churn
          # 



teledata_final %>% count(churn,levels=actvsubs) %>% filter(churn==1)->C1

C1$N<-unclass(teledata_final%>% filter(actvsubs %in% C1$levels)%>%count(actvsubs))[[2]]

C1$ChurnPerc<-C1$n/C1$N

C1$Var.Name<-rep("actvsubs",nrow(C1))
C1<-as.data.frame(C1)

arrange(C1, -ChurnPerc)


# "actvsubs"         "area"             "asl_flag"         "ethnic"          
# [5] "forgntvl"         "hnd_webcap"       "marital"          "prizm_social_one"
# [9] "refurb_new"       "truck"            "churn"           
# > 

#--------------------DECREASING LEVEL BASED ON SIMILLAR CHURN RATE.-------------------------------#

teledata_final1$actvsubs<-ifelse(teledata_final1$actvsubs==3|
                                 teledata_final1$actvsubs==5|
                                 teledata_final1$actvsubs==4|
                                 teledata_final1$actvsubs==2,"3/5/4/2",as.character(teledata_final1$actvsubs))


teledata_final1$actvsubs<-ifelse(teledata_final1$actvsubs==1|
                                   teledata_final1$actvsubs==6,"1/6",as.character(teledata_final1$actvsubs))
#-------------------------
teledata_final1$area<-ifelse(teledata_final1$area=="NORTHWEST/ROCKY MOUNTAIN AREA"|
                               teledata_final1$area=="SOUTH FLORIDA AREA",
                               "NW_SF AREA",as.character(teledata_final1$area))


teledata_final1$area<-ifelse(teledata_final1$area=="NEW ENGLAND AREA"|
                               teledata_final1$area=="CALIFORNIA NORTH AREA"|
                               teledata_final1$area=="NORTH FLORIDA AREA"|
                               teledata_final1$area=="PHILADELPHIA AREA "|
                               teledata_final1$area=="SOUTHWEST AREA "|
                               teledata_final1$area=="NEW YORK CITY AREA "|
                               teledata_final1$area=="CHICAGO AREA"|
                               teledata_final1$area=="LOS ANGELES AREA"|
                               teledata_final1$area=="DALLAS AREA"|
                               teledata_final1$area=="ATLANTIC SOUTH AREA","AREA_2",as.character(teledata_final1$area))


teledata_final1$area<-ifelse(teledata_final1$area=="GREAT LAKES AREA"|
                               teledata_final1$area=="DC/MARYLAND/VIRGINIA AREA"|
                               teledata_final1$area=="OHIO AREA"|
                               teledata_final1$area=="HOUSTON AREA"|
                               teledata_final1$area=="CENTRAL/SOUTH TEXAS AREA"|
                               teledata_final1$area=="MIDWEST AREA"|
                               teledata_final1$area=="TENNESSEE AREA"|
                               teledata_final1$area=="LOS ANGELES AREA",
                               "AREA_3",as.character(teledata_final1$area))
#------------------------------------

teledata_final1$ethnic<-ifelse(teledata_final1$ethnic=="Z"|
                               teledata_final1$ethnic=="P"|
                                 teledata_final1$ethnic=="M"|
                                 teledata_final1$ethnic=="X"|
                                 teledata_final1$ethnic=="N"|
                                 teledata_final1$ethnic=="S"|
                                 teledata_final1$ethnic=="U"|
                                 teledata_final1$ethnic=="H"|
                               teledata_final1$ethnic=="G",
                               "Z/P/MXNSSUH/G",as.character(teledata_final1$ethnic))

teledata_final1$ethnic<-ifelse(teledata_final1$ethnic=="R"|
                                 teledata_final1$ethnic=="I"|
                                 teledata_final1$ethnic=="F"|
                                 teledata_final1$ethnic=="J",
                                 "R/I/F/J",as.character(teledata_final1$ethnic))
                                 
                                 


                            
#------------------------

teledata_final1$marital<-ifelse(teledata_final1$marital=="A"|
                                 teledata_final1$marital=="B",
                                "A/B",as.character(teledata_final1$marital))
                                 
teledata_final1$marital<-ifelse(teledata_final1$marital=="M"|
                                  teledata_final1$marital=="S",
                                "M/S",as.character(teledata_final1$marital))

#-----------------------

                               
teledata_final1$prizm_social_one<-ifelse(teledata_final1$prizm_social_one=="R"|
                                  teledata_final1$prizm_social_one=="T",
                                "R/T",as.character(teledata_final1$prizm_social_one))

teledata_final1$prizm_social_one<-ifelse(teledata_final1$prizm_social_one=="U"|
                                           teledata_final1$prizm_social_one=="S"|
                                           teledata_final1$prizm_social_one=="C",
                                         "U/S/C",as.character(teledata_final1$prizm_social_one))
#--------------------------------

teledata_final1$actvsubs<-ifelse(teledata_final1$actvsubs==0|
                                           teledata_final1$actvsubs==5,
                                         "0_5",as.character(teledata_final1$actvsubs))
#------------------------------

teledata_final1$area<-as.factor(teledata_final1$area)
teledata_final1$car_buy<-as.factor(teledata_final1$car_buy)
 teledata_final1$ethnic<-as.factor(teledata_final1$ethnic)
 teledata_final1$marital<-as.factor(teledata_final1$marital)
 teledata_final1$prizm_social_one<-as.factor(teledata_final1$prizm_social_one)
 
 teledata_final1$area<-as.factor(teledata_final1$area)
 teledata_final1$car_buy<-as.factor(teledata_final1$car_buy)
 teledata_final1$ethnic<-as.factor(teledata_final1$ethnic)
 
 
 
 #-------------------------- END OF LEVEL REDUCING OF CAT_VARIABLES---------------#
 
 #---------------------------DERIVED VARIABLES-----------------------
 # Here denominator(plcd_vcemean) having 0 so replaced with mean() value
 index_0<-which(teledata_final1$plcd_vce_Mean==0)
 length(index_0)
 teledata_final1[index_0,"plcd_vce_Mean"]<-mean(teledata_final1$plcd_vce_Mean)
 teledata_final1[index_1,"comp_vce_Mean"]<-mean(teledata_final1$comp_vce_Mean)
 
 
 teledata_final1$comp_vce_pct<-teledata_final1$comp_vce_Mean/teledata_final1$plcd_vce_Mean
 
 ##------------------DUMMY VARIABLES CREATION--------------------#
 
 #To find which variables belongs to categorical
 names(which(lapply(teledata_final1,class)=="factor"))
 
 #------For Train dataset-------------#
 x<-train$area
 area_d<-model.matrix(~x)
 area_d<-as.data.frame(area_d)
 
 
 y<-train$asl_flag
 asl_flag_d<-model.matrix(~y)
 asl_flag_d<-as.data.frame(asl_flag_d)
 
 
 z<-train$ethnic
 ethnic_d<-model.matrix(~z)
 ethnic_d<-as.data.frame(ethnic_d)

 
 m<-train$car_buy
 carbuy_d<-model.matrix(~m)
 carbuy_d<-as.data.frame(carbuy_d)

 
 n<-train$forgntvl
 forgntvl_d<-model.matrix(~n)
 forgntvl_d<-as.data.frame(forgntvl_d)

 
 b<-train$hnd_webcap
 hnd_webcap_d<-model.matrix(~b)
 hnd_webcap_d<-as.data.frame(hnd_webcap_d)
 
 
 v<-train$marital
 marital_d<-model.matrix(~v)
 marital_d<-as.data.frame(marital_d)

 
 c<-train$prizm_social_one
 prizm_social_one_d<-model.matrix(~c)
 prizm_social_one_d<-as.data.frame(prizm_social_one_d)

 
 l<-train$refurb_new
 refurb_new_d<-model.matrix(~l)
 refurb_new_d<-as.data.frame(refurb_new_d)

 
 j<-train$truck
 truck_d<-model.matrix(~j)
 truck_d<-as.data.frame(truck_d)
 
 #------For Test dataset-------------#
 X<-test$area
 area_t_d<-model.matrix(~X)
 area_t_d<-as.data.frame(area_t_d)
 
 
 Y<-test$asl_flag
 asl_flag_t_d<-model.matrix(~Y)
 asl_flag_t_d<-as.data.frame(asl_flag_t_d)
 
 
 Z<-test$ethnic
 ethnic_t_d<-model.matrix(~Z)
 ethnic_t_d<-as.data.frame(ethnic_t_d)
 
 
 M<-test$car_buy
 carbuy_t_d<-model.matrix(~M)
 carbuy_t_d<-as.data.frame(carbuy_t_d)
 
 
 N<-test$forgntvl
 forgntvl_t_d<-model.matrix(~N)
 forgntvl_t_d<-as.data.frame(forgntvl_t_d)
 
 
 B<-test$hnd_webcap
 hnd_webcap_t_d<-model.matrix(~B)
 hnd_webcap_t_d<-as.data.frame(hnd_webcap_t_d)
 
 
 V<-test$marital
 marital_t_d<-model.matrix(~V)
 marital_t_d<-as.data.frame(marital_t_d)
 
 
 C<-test$prizm_social_one
 prizm_social_one_t_d<-model.matrix(~C)
 prizm_social_one_t_d<-as.data.frame(prizm_social_one_t_d)
 
 
 L<-test$refurb_new
 refurb_new_t_d<-model.matrix(~L)
 refurb_new_t_d<-as.data.frame(refurb_new_t_d)
 
 
 J<-test$truck
 truck_t_d<-model.matrix(~J)
 truck_t_d<-as.data.frame(truck_t_d)
 

 
#-----------------------------MODELLING-------------------------------#
 teledata_final1$cust_id<-tele_data$Customer_ID
 
 #splitting dataset into testing(70%) and training datasets(30%).
 
 set.seed(200)
 sampling<-sample(nrow(teledata_final1),0.7*nrow(teledata_final1),replace = FALSE)
 
 train<-teledata_final1[sampling,]
 test<-teledata_final1[-sampling,]
 
 #proportion of 1s and 0s in origina dataset
 prop.table(table(teledata_final1$churn))
 #     0         1 
 # 0.7607886 0.2392114 
 
 #proportion of 1s and 0s in training dataset
 prop.table(table(train$churn))
 # 0         1 
 # 0.7615877 0.2384123 
  
 #proportion of 1s and 0s in testing dataset
 prop.table(table(test$churn))
 # #     0         1 
 # 0.7631473 0.2368527
 
 
 #--------------------------------MODELLING-----------------------------------
 
 # COL 33 is cust_id so have to ommit from final model.
 #churn=1---> CHURNED
 #churn=0--->Not Churned
 
 model1<-glm(data = train[,-33],churn~.,family = "binomial")
 summary(model1)
 
 #To choose significant variables by performing number of itterations
 
 step(model1,direction = "both")

  # glm(formula = churn ~ adjmou + adjqty + adjrev + age1 + avg3mou + 
 #       avg6mou + callwait_Range + ccrndmou_Range + comp_vce_Mean + 
 #       da_Range + drop_blk_Mean + mou_Mean + mou_opkv_Range + plcd_vce_Mean + 
 #       totcalls + actvsubs + area + asl_flag + ethnic + hnd_webcap + 
 #       prizm_social_one + refurb_new + truck + comp_vce_pct, family = "binomial", 
 #     data = train[-33])
 
 #-------------adding significant levels of categorical variables as factors---------
 
 

 
 mod2<-glm(data=train[,-33],churn ~ adjmou + adjqty + adjrev + age1 + avg3mou + 
             avg6mou + callwait_Range + ccrndmou_Range + comp_vce_Mean + 
             da_Range + drop_blk_Mean + mou_Mean + mou_opkv_Range + plcd_vce_Mean + 
             totcalls + actvsubs + area + asl_flag + ethnic + hnd_webcap + 
             prizm_social_one + refurb_new + truck + comp_vce_pct,family = "binomial")
summary(mod2)


#AFTER RUNNING MOD2 SOME OF THE VARIABLES ONLY GETTING SIGNIFICANT.SO BY TAKING OUT
#ONLY THOSE VARIABLES.


#ADDING CATEGORICAL LEVELS AS DUMMY VARIABLES TO TRAIN DATASET.
train$AREA3_d<-area_d$xAREA_3
train$Nw_RM_area_d<-area_d$`xNORTHWEST/ROCKY MOUNTAIN AREA`
 train$SF_area_d<-area_d$`xSOUTH FLORIDA AREA`
 train$asl_flag_Y_d<-asl_flag_d$yY
 train$ethnic_C_d<-ethnic_d$zC
 train$ethnic_Z_P_MXNSSUH_G_d<-ethnic_d$`zZ/P/MXNSSUH/G`
 train$hnd_webcap_WC_d<-hnd_webcap_d$bWC
 train$prizm_social_one_R_d<-prizm_social_one_d$cR
 train$prizm_social_one_T_d<-prizm_social_one_d$cT
 train$refurb_new_R_d<-refurb_new_d$lR
 
 #ADDING CATEGORICAL LEVELS AS DUMMY VARIABLES TO TESTING DATASET.
test$AREA3_d<-area_t_d$XAREA_3
test$Nw_RM_area_d<-area_t_d$`XNORTHWEST/ROCKY MOUNTAIN AREA`
 test$SF_area_d<-area_t_d$`XSOUTH FLORIDA AREA`
 test$asl_flag_Y_d<-asl_flag_t_d$YY
 test$ethnic_C_d<-ethnic_t_d$ZC
 test$ethnic_Z_P_MXNSSUH_G_d<-ethnic_t_d$`ZZ/P/MXNSSUH/G`
 test$hnd_webcap_WC_d<-hnd_webcap_t_d$BWC
 test$prizm_social_one_R_d<-prizm_social_one_t_d$CR
 test$prizm_social_one_T_d<-prizm_social_one_t_d$CT
 test$refurb_new_R_d<-refurb_new_t_d$LR


#Creating Itteration using all signifcant variables shown in mod2
 mod3<-glm(data = train,churn~adjmou + adjqty + age1 + 
             age2 + avg6mou + da_Range + drop_blk_Mean +
             mou_Mean + AREA3_d + Nw_RM_area_d + SF_area_d +
             asl_flag_Y_d + ethnic_C_d + ethnic_Z_P_MXNSSUH_G_d +
             hnd_webcap_WC_d + prizm_social_one_R_d + prizm_social_one_T_d+
             refurb_new_R_d,family = "binomial")
 
summary(mod3)
pred3<-prediction(predict(mod3,type = "response",newdata = test),test$churn)
perf3<-performance(pred4,"tpr","fpr")
plot(perf3)
performance(pred4,"auc")->auc3
auc3@y.values

 mod4<-glm(data = train,churn~  age1 + 
               avg6mou + da_Range + drop_blk_Mean +
             mou_Mean + AREA3_d + Nw_RM_area_d + SF_area_d +
             asl_flag_Y_d + ethnic_C_d + ethnic_Z_P_MXNSSUH_G_d +
             hnd_webcap_WC_d + prizm_social_one_R_d + prizm_social_one_T_d+
             refurb_new_R_d,family = "binomial")
summary(mod4)

prop.table(table(train$churn))
#     0         1 
# 0.7615877 0.2384123 
pred<-predict(mod4,type = "response",newdata = train)
ifelse(pred>=0.2384123,1,0)->pred
table(pred,train$churn) #57.1%


#  pred       0     1
#    0     20250  4815
#    1     15093  6249



kappa2(data.frame(pred,train$churn))
 # Cohen's Kappa for 2 Raters (Weights: unweighted)
 # 
 # Subjects = 46407 
 #   Raters = 2 
 #    Kappa = 0.104 
 # 
 #        z = 25.4 
 #  p-value = 0 

#plotting ROCR Curve and PEformance Measures.

pred4<-prediction(predict(mod4,type = "response",newdata = test),test$churn)

perf4<-performance(pred4,"tpr","fpr")

plot(perf4)

performance(pred4,"auc")->auc4

auc4@y.values


#-------------------------#####FINAL MODEL####--------------------------------------------------

mod5<-glm(data = train,churn~  age1 + 
            avg6mou  + drop_blk_Mean +comp_vce_pct+
            mou_Mean + AREA3_d + Nw_RM_area_d  +
            asl_flag_Y_d + ethnic_C_d + ethnic_Z_P_MXNSSUH_G_d +
            hnd_webcap_WC_d + prizm_social_one_R_d + prizm_social_one_T_d+
            refurb_new_R_d,family = "binomial")

summary(mod5)

test$churn<-as.factor(test$churn)

#INORDER TO APPLY MOD ON TEST DATA COLNAMES SHOULD BE SAME

pred_prob<-predict(mod5,type = "response",newdata = test)

test$prob<-pred_prob

ifelse(pred>=0.2384123,1,0)->pred

table(pred,test$churn) # (8568+2674)/length(test$churn)=56.5%

library(ROCR)

pred<-prediction(predict(mod5,type="response",newdata = test),test$churn)

perf<-performance(pred,"tpr","fpr")

plot(perf,main="MOD5 ROC CURVE")

performance(pred,"auc")->auc

auc@y.values #~  59% giving model accuracy from AUC.
  

test$churn<-as.numeric(test$churn)

#--------------------GAINCHART--------------

gains(test$churn,predict(mod5,type = "response",newdata = test),groups = 10)


# Depth                            Cume   Cume Pct                     Mean
#  of           Cume     Mean      Mean   of Total    Lift   Cume     Model
# File     N      N      Resp      Resp      Resp    Index   Lift     Score
# -------------------------------------------------------------------------
#   10  1989   1989      1.35      1.35      10.9%     109    109      0.34
#   20  1989   3978      1.31      1.33      21.5%     106    107      0.30
#   30  1989   5967      1.27      1.31      31.7%     102    106      0.27
#   40  1989   7956      1.25      1.30      41.8%     101    104      0.26
#   50  1989   9945      1.26      1.29      51.9%     101    104      0.24
#   60  1989  11934      1.23      1.28      61.9%      99    103      0.23
#   70  1989  13923      1.20      1.27      71.5%      97    102      0.22
#   80  1989  15912      1.20      1.26      81.2%      97    102      0.20
#   90  1989  17901      1.17      1.25      90.6%      94    101      0.19
#  100  1989  19890      1.17      1.24     100.0%      94    100      0.15


quantile(test$prob,prob=seq(0.1,1,by=0.1))

# 10%       20%       30%       40%       50%       60%       70%       80%       90%      100% 
# 0.1733355 0.1946705 0.2094215 0.2216083 0.2334859 0.2478451 0.2629183 0.2844269 0.3124564 0.7220290 


#Here customer segmentation done as HIGH(70%-100% probs) 
                                  #MEDIUM(40%-60%)
                                  #LOW(10%-30%)

target_High<-test[test$prob>=0.26&test$prob<=0.72,"cust_id"]#possibility of High churn
target_Med<-test[test$prob>=0.22&test$prob<=0.24,"cust_id"]#possibility of medium churn
target_Low<-test[test$prob>=0.17&test$prob<=0.20,"cust_id"]#possibility of low churn

########------------------------------------$$$ MODEL INSIGTS $$$--------------------------------#######

#1.TOP 5 FACTORS THAT CUSTOMER BEING CHURN MOSTLY DUE TO

# DROP BLK MEAN
# hnd_webcap_WC
# comp_vce_mean
# PRIMZ_SOICAL ONE_R
# PRIMZ_SOICAL ONE_T
# REFRUB_NEW_R

#2.As comp_vce_mean is significant so "costing and billing" is important factor,
# As hnd_webcap turns out to be significant so data usage connectivity leading to churn

#3. Yes i would recommend rate plan migration is a proactive retention strategy.

#4.Based on the top influencing factors that customers is churned or not we have to use this model.

#5.As customer segmentaion above LOW(10%-30%) and MEdium(40%-70%) segments along with their ID's we have
# pritorize the subscriber and also by applying same model on the another testdata we have to take ID's of 
#subscribers in the same way(By taking low probablites)
