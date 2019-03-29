#Milestone 1
#-----------
library(jsonlite)
data <- fromJSON("C:/Users/csame/Documents/airline.json")
write.csv(data,"airdata.csv")

#Milestone 2
#-----------
library(tidyr)
library(stringr)
library(foreign)
str(data)
data<-flatten(data)                 
str(data)
data2<-as.data.frame(data)
data2
install.packages("stringr",dependencies = TRUE)
data1<-tolower(data2[,2])
first_split<- str_split(data1, ",", 2)
(y <- data.frame(
  LASTNAME = sapply(first_split, head, n = 1),
  MAKE  = sapply(first_split, tail, n = 1)
))
data2$LastName<-y[,1]

second_split <- str_split(y[,2], ". ", 2)
(z <- data.frame(
  prefix = sapply(second_split, head, n = 1),
FIRSTNAME  = sapply(second_split, tail, n = 1)
))
data2$Prefix<-z[,1]
data8<-z[,2]
data8
third_split <- str_split(data8, ";", 2)
(p <- data.frame(
  firstname = sapply(third_split, head, n = 1),
  AGE  = sapply(third_split, tail, n = 1)
))
data2$Age<-p[,2]
fullfirstname<-p[,1]
fullfirstname
fourth_split<-str_split(fullfirstname, " ", 2)
( g<- data.frame(
  firstname = sapply(fourth_split, head, n = 1),
  MIDDLENAME = sapply(fourth_split, tail, n = 1)
))
data2$Firstname<-g[,1]
middlename=g[,2]
middlename
data7<-y[,2]
data7
data$genderMr= grepl(pattern = "mr. ", x= data7 )
data$genderMrs= grepl(pattern = "mrs. ", x= data7 )
data$genderMiss= grepl(pattern = "miss. ", x= data7 )
data$genderMaster= grepl(pattern = "master. ", x= data7 )
data$genderRev= grepl(pattern = "rev. ", x= data7 )
data$gendercapi=grepl(pattern = "capt. ", x= data7 )
data$genderdr=grepl(pattern = "dr. ", x= data7 )
data$gendermme=grepl(pattern = "mme", x= data7 )
row=nrow(data2)
for(i in 1:row)
{
  if( data$genderMr[i]== TRUE)
  { data2$GENDER[i]<- "Male"}
}
for(i in 1:row)
{
  if( data$genderMrs[i]== TRUE)
  { data2$GENDER[i]<- "Female"}
}
for(i in 1:row)
{
  if( data$genderMiss[i]== TRUE)
  { data2$GENDER[i]<- "Female"}
}
for(i in 1:row)
{
  if( data$genderMaster[i]== TRUE)
  { data2$GENDER[i]<- "Male"}
}
for(i in 1:row)
{
  if( data$genderRev[i]== TRUE)
  { data2$GENDER[i]<- "Male"}
}
for(i in 1:row)
{
  if( data$genderdr[i]== TRUE)
  { data2$GENDER[i]<- "Male"}
}
for(i in 1:row)
{
  if( data$gendercapi[i]== TRUE)
  { data2$GENDER[i]<- "Male"}
}
for(i in 1:row)
{
  if( data$gendermme[i]== TRUE)
  { data2$GENDER[i]<- "Female"}
}
data$genderms=grepl(pattern = "ms", x= data7 )
for(i in 1:row)
{
  if( data$genderms[i]== TRUE)
  { data2$GENDER[i]<- "Male"}
}
data$gendermajor=grepl(pattern = "major", x= data7 )
for(i in 1:row)
{
  if( data$gendermajor[i]== TRUE)
  { data2$GENDER[i]<- "Male"}
}
data$gendersir=grepl(pattern = "sir", x= data7 )
for(i in 1:row)
{
  if( data$gendersir[i]== TRUE)
  { data2$GENDER[i]<- "Male"}
}
data$gendermile=grepl(pattern = "mlle", x= data7 )
for(i in 1:row)
{
  if( data$gendermile[i]== TRUE)
  { data2$GENDER[i]<- "Male"}
}
data$gendercol=grepl(pattern = "col", x= data7 )
for(i in 1:row)
{
  if( data$gendercol[i]== TRUE)
  { data2$GENDER[i]<- "Male"}
}
data$gendercountess=grepl(pattern = "countess", x= data7 )
for(i in 1:row)
{
  if( data$gendercountess[i]== TRUE)
  { data2$GENDER[i]<- "Female"}
}

#data2$gender=sapply(data2["gender"],function(x) ifelse(data2$gender == TRUE, "Male","female"))
row=nrow(data2)
data2=data2[-1,]
data2<-as.data.frame(data2)
data2<-data2[c(6,3,2,4,5,1,8,10,7,9,11)]
data2$Age <- as.numeric(as.character(data2$Age))
data2$Age[is.na(data2$Age)] = 0
#data2$Age = sapply(data2["Age"],function(x) ifelse(data2$Age == 0,30 ,data2$Age))
row=nrow(data2)

count1<-0
actual_sum2<-0
for(i in 1:row)
{
  if( data2$Age[i]!= 0)
  { count1<-count1+1
  actual_sum2<-data2$Age[i]+actual_sum2
  }
}
actual_avg3<- (actual_sum2/count1)
actual_avg3<-round(actual_avg3, digits = 0)
for(i in 1:row)
{
  if( data2$Age[i]==0)
  { data2$Age[i]<-30}
}
for(i in 1:row)
{
  if( data2$Age[i]> 18)
  { data2$MinAdlt[i] <- "Adult"}
  else
  {data2$MinAdlt[i] <- "Minor"}
}
for(i in 1:row)
{
  if( data2$SUCCESS[i]==0)
  { data2$SUCCESS[i]<- "NO"}
  else
  {data2$SUCCESS[i]<- "YES"}
}
for(i in 1:row)
{
  data2$Yearborn[i]<- (as.integer(2018 - data2$Age[i]))
}
#normalization
#1. Age Normalization using Z-score
zscore.age<-(data2$Age-mean(data2$Age))/sd(data2$Age)
zscore.age
m<-mean(zscore.age)
std<-sqrt(var(zscore.age))
hist(zscore.age,prob=T,main="Z-Score Normalized Age")
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE)

#2. Fare normalization using Z-score
#zscore.fare<-(data2$FARE-mean(data2$FARE))/sd(data2$FARE)
#zscore.fare
#m<-mean(zscore.fare)
#std<-sqrt(var(zscore.fare))
#hist(zscore.fare,prob=T,main="Z-Score Normalized fare")
#curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE)

#Write to CSV
write.csv(data2,"C:/Users/csame/Desktop/AirlineFinal.csv")
byd = read.csv('AirlineFinal.csv')
byd %>% glimpse()
byd = byd %>% mutate(tradeDate = as.Date(tradeDate))
write.arff(byd, file='AirlineFinal.arff')
