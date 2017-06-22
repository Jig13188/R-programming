
#1. Create a data frame named customer_details, with 3 rows and 4 columns and following values

data.frame(Name=c("Ramya","Ali","Jim"),Age=c(25,30,35),Telephone_bill_rs=c(600,400,200),Month=rep("Aug",3))

############-------------------------#####################
# 2. This question is related to creating lists. Write code for following steps :
#   a) Create a list called Names, include following attributes in this list :
# a. Last Name: Potter, Riddle, Dumbledore
# b. First Name: Harry, Tom, Albus
# c. Age: 18,50,120
# d. Profession: Student, Magician, Headmaster

Names<-list(LastName=c("Potter","Riddle","Dumbledore"),FirstName=c("Harry","Tom","Albus"),Age=c(18,50,120),Proffesion=c("Student","Magician","HeadMaster"))
class(Names)
names(Names)
#b) How will you display all the values in attribute Last Name?
Names$LastName

# c) How will you display the 3rd element of attribute Age?
Names$Age[3]
Names[[3]][3]

##############------------------------------############
# 3. Write code for the following steps :

# a) Load the package ggplot2 into the workspace
install.packages("ggplot2")
library(ggplot2)

# b) Load the data msleep into the workspace.
data(msleep)

# c) Display all the names of this dataset
str(msleep)

# d) Rename the column vore to type
which(names(msleep)=="vore")
names(msleep)[3]<-"type"

# e) Display first ten values in the column type you just created
 fir_ten<-msleep[1:10,"type"]

 # f) Convert the column type into a character data type.
  msleep$type<-as.character(msleep$type)
  
 # g)Use str(msleep) and take a look at the data types of various columns in this
  # data, paying attention to where columns are of type "factor", do you think
  # that use of factors for all columns justified? Would you like to convert any
  # column into a character type? Write down your reasons.
  
  str
  names_msleep<-names(msleep)
  length(unique(msleep$name))
  length(unique(msleep$genus))
  length(unique(msleep$type))#should convert into factor
  length(unique(msleep$order))#should convert into factor
  length(unique(msleep$conservation))#should convert into factor
  
  msleep$type<-as.factor(msleep$type)
  msleep$order<-as.factor(msleep$order)
  msleep$conservation<-as.factor(msleep$conservation)
  
  
# h) Choose the columns name, genus, vore, and sleep_total from the dataset
# and store it in a new dataset. Save the converted data set in your working
# directory using write.csv () function.
  msleep_new<-msleep[,c("name","genus","type","sleep_total")] 
  getwd()
  write.csv(msleep_new,"msleep_new.csv")
  
  
  
  # a) Load the package XLConnect to read excel files  
install.packages("XLConnect")
library(XLConnect)
# Write a code to load the workbook retail.xlsx in the location -C/Data Science
# R/Assignments/Non Graded Assignments/Topic 5 Introduction to R
load<-loadWorkbook(("C:/Data Science with R/Assignments/Non Graded Assignments/Topic 5 - Introduction to R/retail.xlsx"),create = F)
XLConnect::readWorksheet(object = load,sheet = "data2",header = TRUE)
class(data1)
XLConnect::readWorksheet(object = load,sheet = "data2",header = TRUE)
class(data2)
