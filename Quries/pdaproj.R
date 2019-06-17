setwd("E:\\PDA\\project\\h1b")
h1b2018<-read.csv(file = "H-1B_Disclosure_Data_FY2018-Q3.csv")
#filtering required columns
h1b2018fil<-h1b2018[,c(2,3,4,8,11,12,22,24,33,34,35,43)]
#removing missing values
h1b2018fil<-h1b2018fil[Reduce("&", lapply(h1b2018fil, function(x) !(is.na(x)|x==""))),]
h1b2017<-read.csv(file = "H-1B_Disclosure_Data_FY17.csv")
#filtering required columns
h1b2017fil<-h1b2017[,c(2,3,4,8,11,12,22,24,33,34,35,43)]
#removing missing values
h1b2017fil<-h1b2017fil[Reduce("&", lapply(h1b2017fil, function(x) !(is.na(x)|x==""))),]
h1b2016<-read.csv(file="H-1B_Disclosure_Data_FY16.csv")
#filtering required columns
h1b2016fil<-h1b2016[,c(2,3,4,8,10,11,20,22,26,27,34)]
#missing values
h1b2016fil<-h1b2016fil[Reduce("&", lapply(h1b2016fil, function(x) !(is.na(x)|x==""))),]

h1b2016fil<-h1b2016fil[1:610355,]

h1b2016file<-as.data.frame(append(h1b2016fil, list(FULL_TIME_POISTION=h1b2017fil$FULL_TIME_POSITION), after = 8))

h1b2016fil<-h1b2016file


colnames(h1b2016fil)[12] <- "H1B_DEPENDENT"
colnames(h1b2016fil)[9]<-"FULL_TIME_POSITION"

rm(h1b2018)
rm(h1b2017)
rm(h1b2016)

h1b2018fil$year<-rep(2018,times=530755)
h1b2017fil$year<-rep(2017,times=610855)
h1b2016fil$year<-rep(2016,times=610355)
h1b2018fil<-as.data.frame(h1b2018fil)
h1b2017fil<-as.data.frame(h1b2017fil)
h1b2016fil<-as.data.frame(h1b2016fil)

finaldf<-merge(h1b2018fil,h1b2017fil,all=TRUE)
finaldf<-merge(finaldf,h1b2016fil,all=TRUE)

library(sqldf)
#converting wage into year
hourdata<-sqldf("select * from finaldf where PW_UNIT_OF_PAY=='Hour'")
hourdata<-data.frame(hourdata)
h<-as.vector(hourdata$PREVAILING_WAGE)
h<-as.numeric(h)
hourdata$PREVAILING_WAGE <- h * 2080 

biweekData<-sqldf("select * from finaldf where PW_UNIT_OF_PAY=='Bi-Weekly'")
biweekData<-data.frame(biweekData)
b<-as.vector(biweekData$PREVAILING_WAGE)
b<-as.numeric(biweekData$PREVAILING_WAGE)
biweekData$PREVAILING_WAGE <- b* 26

weekData<-sqldf("select * from finaldf where PW_UNIT_OF_PAY=='Week'")
weekData<-data.frame(weekData)
w<-as.vector(weekData$PREVAILING_WAGE)
w<-as.numeric(weekData$PREVAILING_WAGE)
weekData$PREVAILING_WAGE <- w* 52

monthData<-sqldf("select * from finaldf where PW_UNIT_OF_PAY=='Month'")
monthData<-data.frame(monthData)
m<-as.vector(monthData$PREVAILING_WAGE)
m<-as.numeric(monthData$PREVAILING_WAGE)
monthData$PREVAILING_WAGE <- m* 12


yearData<-sqldf("select * from finaldf where PW_UNIT_OF_PAY=='Year'")
yearData<-data.frame(yearData)
y<-as.vector(yearData$PREVAILING_WAGE)
y<-as.numeric(yearData$PREVAILING_WAGE)
yearData$PREVAILING_WAGE<-y*10


fnldf<-merge(yearData,biweekData,all = TRUE)
fnldf<-merge(fnldf,weekData,all=TRUE)
fnldf<-merge(fnldf,hourdata,all=TRUE)


fnldata<-sqldf("select * from fnldf where PREVAILING_WAGE>=20000")
str(fnldata$CASE_STATUS)
summary(fnldata$CASE_STATUS)
fnldata<-fnldata[,c(-11)]
write.csv(fnldata,file = "FinalH1B.csv")

h1b<-read.csv(file="FinalH1B.csv")

h1b<-fnldata

h1bm<-as.matrix(h1b)
h1bm<-gsub(",",'',as.matrix(h1bm))
h1bm<-gsub("\\s+",'',h1bm)

h1bl<-as.data.frame(h1bm)

h1bl<-h1bl[,c(-2,-3)]

h1bl$PREVAILING_WAGE<-as.vector(h1bl$PREVAILING_WAGE)
h1bl$PREVAILING_WAGE<-as.numeric(h1bl$PREVAILING_WAGE)

h1bl$CASE_SUBMITTED <- as.Date(h1bl$CASE_SUBMITTED,"%d-%m-%Y")
h1bl$DECISION_DATE <- as.Date(h1bl$DECISION_DATE,"%d-%m-%Y")


write.csv(h1bl,file ="h1blt.csv", row.names = FALSE)
write.csv(h1bl,file ="finalh1btxt.txt", row.names = FALSE,col.names = FALSE)















