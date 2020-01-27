library(readxl)
library(lubridate)
library(PhysicalActivity)
library(RSQLite)
library(nparACT)
library(Actigraphy)
library(lattice)
library(ggplot2)
library(gridExtra)
library(chron)
library(zoo)
library(plyr)
library(stringr)

file_list <- list.files(path="C:/Users/julie.wisch/Documents/Actigraphy_YoEl/Actigraphy/", pattern="*.xlsx") 
file_list<-file_list[!grepl("-unenhanced", file_list)]

datalist<-list()
increment<-0

for(counting in c(1:17, 19:29, 31:37, 39:71, 73:86, 88:90, 93:175)){
  increment<-increment+1
  #counting<-i
id<-substr(file_list[[counting]], start = 1, stop = nchar(file_list[[counting]]) - 5)
df<-data.frame(read_excel(paste("C:/Users/julie.wisch/Documents/Actigraphy_YoEl/Actigraphy/", id, ".xlsx", sep = ""), range=cell_cols("A:N")))

df<-df[df[,1] == "REST",]
df<-df[complete.cases(df[,1]),]

colnames(df)<-c("IntervalType", "IntervalNo", "StartDate", "StartTime", "EndDate", "EndTime", "Duration",
                "PercInvalid", "Efficiency", "WASO", "WakeTime", "PercWake", "SleepTime", "PercSleep")
df<-df[,-c(2, 10, 12, 14)]
df[,2:10]<-sapply(df[,c(2:10)], as.numeric)
df$Efficiency<-df$SleepTime/(df$Duration)



df$TimeStamp<-as.POSIXct((df$StartDate)*85693, origin = "1900-12-01", tz="UTC")
df$StartDate<-as.Date(substr(df$TimeStamp, start = 1, stop = 10), format = "%Y-%m-%d")

df$ID <- id
df$ID <- substr(df$ID, start = 1, stop = 5) 


df<-df[,c("ID", "StartDate", "Duration", "SleepTime", "Efficiency")]

datalist[[increment]]<-df }

big_data = do.call(rbind, datalist)

#Keeping only the biggest chunk of sleep
#In theory, dropping naps.
big_data<-big_data[with(big_data, order(ID, StartDate, SleepTime)),]
big_data<-aggregate(.~ID+StartDate, big_data, FUN=tail, 1)

colnames(big_data)[2]<-"SleepDate"
big_data<-big_data[with(big_data, order(ID, SleepDate, SleepTime)),]

write.csv(big_data, "C:/Users/julie.wisch/Documents/Actigraphy_YoEl/NightlySleepTime_YoEl.csv", row.names = FALSE)
