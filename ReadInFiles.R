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
df<-data.frame(read_excel(paste("C:/Users/julie.wisch/Documents/Actigraphy_YoEl/Actigraphy/", id, ".xlsx", sep = ""), range=cell_cols("A:H")))
# Remove rows after the end of the first table
df = df[-(1:(which(df[,1]=="Line" & df[,8] == "Interval Status") + 1)), ]
colnames(df)<-c("Line", "Date", "Time", "Activity", "Marker", "WhiteLight", "Sleep_Wake", "IntervalStatus")

df$Date<-as.numeric(df$Date)
df$Line<-as.numeric(df$Line)
df$Activity<-as.numeric(df$Activity)
df$Marker<-as.numeric(df$Marker)
df$WhiteLight<-as.numeric(df$WhiteLight)
df$Sleep_Wake<-as.factor(df$Sleep_Wake)
df$IntervalStatus<-as.factor(df$IntervalStatus)
df$Time<-as.numeric(df$Time)
df$TimeStamp<-as.POSIXct((df$Date+df$Time)*86400 + 85*60, origin = "1900-12-01", tz="UTC")
df$Date<-as.Date(substr(df$TimeStamp, start = 1, stop = 10), format = "%Y-%m-%d")
df$Time<-as.factor(substr(df$TimeStamp, start = 12, stop = 19))
df$Day <- weekdays(as.Date(df$TimeStamp))
df$id <- id
df$Visit <- 1
df$Time<-chron(times=df$Time)


#Need to make sure stuff starts at midnight.  Need to drop everything before the first midnight timestamp
index<-as.numeric((df[df$Time == "23:59:59", "Line"] ))
df<-df[df$Line > index[1] & df$Line <= index[length(index)],]

#######################################################################################
#######################################################################################
colnames(df)[4]<-"vm"  
df1 <- wearingMarking(dataset = df, frame = 90,perMinuteCts = 2, TS = "TimeStamp",
                        cts = "vm", streamFrame = NULL, allowanceFrame= 2, newcolname = "wearing")
  df1$Date <- as.Date(df1$TimeStamp)
  df1$Time <- format(df1$TimeStamp,"%H:%M:%S")
  d <- dataCollapser(df1, TS = "TimeStamp", col = "vm", by = 60)
  d<-merge(d, df1[,c("TimeStamp", "Day", "id", "wearing")], by = "TimeStamp", all.x = TRUE, all.y = FALSE)
  

  d$DayDay<-as.Date(substr(d$TimeStamp, start = 1, stop = 10), format = "%Y-%m-%d")
  
  hold<-d[d$wearing == "nw",]
  NotWearing<-ddply( hold , .(DayDay) , summarise , Count = length(wearing) )
  TotalNum<-ddply( d , .(DayDay) , summarise , Count = length(wearing) )
  hold<-merge(NotWearing, TotalNum, by = "DayDay")
  hold$Percent<-hold$Count.x / hold$Count.y
  
  #If person is flagged as not wearing their watch for 2 or more hours/day, drop that entire day.
  hold<-hold[hold$Percent > 2/24,]
  
  d<-d[!(d$DayDay %in% hold$DayDay),]
  
  # 
  # ggplot(d, aes(x = TimeStamp, y = vm)) +
  #   geom_line(size = .5, alpha = .5) + 
  #   xlab("Time") + ylab("activity") + ggtitle("Timepoint 1 - raw data")+ ylim(c(0, 3500)) + 
  # xlim(c(as.POSIXct("2012-12-29 00:00:00"), as.POSIXct("2012-12-31 00:00:00")))


data<-d
names(data)[1] <- "time"
names(data)[2] <- "activity"
bin_hr <- 60 
SR<-2/60
cutoff<-1



a <- nrow(data) 
e <- SR*60 ## samples per minute
m <- bin_hr*SR*60  ## samples per hour
full_days <- floor(a/(e*bin_hr*24))

## --- Cut data to full days
data <- data[1:(e*bin_hr*24*full_days),]

#Dropping Weekends
data<-data[!(data$Day == "Saturday" | data$Day == "Sunday"),]
NoofDays <- length(TotalNum$DayDay)

b <- floor(a/(SR*60)) ## full minutes recorded
#data<-data[data$wearing == "w",]

data_min <- data$activity


# ## ---- Calculate hourly averages
# data_hrs <- nparACT_data_hrs(data, a, m)
# data_hrs<- data_hrs[1:(full_days*24),]
# 
# result_ISIV <- nparACT_ISIVfunctions$nparACT_ISIV(data_hrs, bin_hr)
# IS <- result_ISIV[1]
# IV <- result_ISIV[2]
# 
# minaverage <- nparACT_auxfunctions1$nparACT_minaverage(a, data_min)
# result_RA <- nparACT_RAfunctions$nparACT_L5M10(data, minaverage, a, SR)
# L5 <- result_RA[1]
# L5_starttime <- result_RA[2]
# M10 <- result_RA[3]
# M10_starttime <- result_RA[4]
# RA <- result_RA[5]


#Skipping this chunk for now and seeing what happens. I think I made a mess.
#---------------------------------------------
#   bin_hr <- 60 
#   SR<-2/60
#   #cutoff<-5
#   colnames(data)[1:2]<-c("TimeStamp", "vm")
#   df1<-data[,c("TimeStamp", "vm")]
#   df1<-df1[,c("TimeStamp", "vm")]  
#   #df1<-df1[complete.cases(df1),]
#   # df1 <- wearingMarking(dataset = df1, frame = 90,perMinuteCts = 1, TS = "TimeStamp",
#   #                       cts = "vm", streamFrame = NULL, allowanceFrame= 2, newcolname = "wearing")
#   # df1<-df1[df1$wearing == "w",]
#   df1<-dataCollapser(df1, TS = "TimeStamp", col = "vm", by = 60)
# #  result<-data.frame(npar_Base(df1, bin_hr, SR, cutoff))
# 
# data<-df1
#-------------------------------------
  nparACT_data_hrs<-function(data, a, m){
    data_hrs <- matrix(NA, nrow = a/m) 
    for (i in 1:(a/m)){
      subset_h <- data$activity[(((i-1)*m)+1):((i*m))]
      mean_subset_h <- mean(subset_h, na.rm=TRUE)
      data_hrs[i] <- mean_subset_h 
    }
    return(data_hrs)
  }
  
 
    names(data)[1] <- "time"
    names(data)[2] <- "activity"
    
    
    
    a <- nrow(data) 
       full_days <- floor(a/(e*bin_hr*24))

    ## --- Cut data to full days
    data <- data[1:(e*bin_hr*24*full_days),]
    
    b <- floor(a/(SR*60)) ## full minutes recorded
    ## ------------------------------------------
    
    ## ---- Filtering, Cutoff for classification as movement
    # for (k in 1:length(data$activity)){
    #   if (data$activity[k] < cutoff){
    #     data$activity[k] <- 0
    #   }
    # }
    
    
    ## ------------------------------------------
    
    ## ---- Calculate average for each minute (needed if SR != 1/60)
    # if (SR != 1/60){
    #   data_min <- nparACT_auxfunctions1$nparACT_data_min(b, SR, data)
    # }  else {
    data_min <- data$activity
    #}
    ## ------------------------------------------
    
    
    ## ---- Calculate hourly averages
    data_hrs <- nparACT_data_hrs(data, a, m)
    
    p <- 1440/bin_hr  
    l <- 60/bin_hr  
    data_hrs<-data_hrs[complete.cases(data_hrs)]
    n <- length((data_hrs))
    mean_all <- mean(data_hrs[1:n], na.rm = T) 

    result_IVnum <- matrix(NA, nrow = n) 
    for (k in 2:n){
      z <- ((data_hrs[k]-data_hrs[(k-1)])^2)  
      result_IVnum[k] <- z  
    }
 
    IVnumerator <- n*sum(result_IVnum, na.rm = T)  
    ## ---- IV denominator calculation
    result_ISdenom <- matrix(NA, nrow = n)  
    for (j in 1:n){
      y <- ((data_hrs[j]-mean_all)^2)
      result_ISdenom[j] <- y  
    }
    ISdenom <- sum(result_ISdenom)   
    IVdenominator <- (n-1)*ISdenom ## ISdenom can be used!
    ## -----------------------------
    IV <- round(IVnumerator/IVdenominator, digits = 2)

    result_ISnum <- matrix(NA, nrow = 24) 
    n <- length(data_hrs)
    p <- 1440/bin_hr  
    for (h in 1:24){ 
      s <- ceiling(n/p) 
      data_hrs3 <- data_hrs
      data_hrs3[s*p] <- NA 
      data_hrs3 <- matrix(data_hrs3) 
      hrlydat <- data_hrs3[c(seq(h,nrow(data_hrs3),24)),]
      hrlymean <- mean(hrlydat, na.rm = T) 
      x <- (hrlymean-mean_all)^2 
      result_ISnum[h,] <- x 
    }
  
    ISnumerator <- n*sum(result_ISnum)  
    ## ---- IS denominator calculation
    result_ISdenom <- matrix(NA, nrow = n)  
    for (j in 1:n){
      y <- ((data_hrs[j]-mean_all)^2)
      result_ISdenom[j] <- y  
    }
    ISdenom <- sum(result_ISdenom)   
    ISdenominator <- p*ISdenom  
    ## -----------------------------
    IS <- round(ISnumerator/ISdenominator, digits = 2)
    
    
    
    ## -----------------------------------------------------------------------------


    ## ---------------------------------------------------------------------------------
    
    ## ---------- Relative Amplitude (RA) calculation
    ## ---- Minutewise averages across 24hrs
    minaverage <- nparACT_auxfunctions1$nparACT_minaverage(a, data_min)
    minaverage<-(rbind(minaverage, minaverage))
    minaverage<-data.frame("Time" = seq(1:length(minaverage)), "minaverage" = minaverage)
    ## --------------------------------
    
    #########################################################################
    #########################################################################

    L5_attempt<-rollapply(minaverage[,2], width = 300, by = 1, FUN = mean, na.rm=TRUE, align = "left")
    M10_attempt<-rollapply(minaverage[,2], width = 600, by = 1, FUN = mean,na.rm=TRUE, align = "left")
    minaverage<-minaverage[1:1440,]
    L5_attempt<-data.frame("Time" = minaverage$Time, "RollAvg" = L5_attempt[1:1440])
    M10_attempt<-data.frame("Time" = minaverage$Time, "RollAvg" = M10_attempt[1:1440])
    L5_attempt<-L5_attempt[complete.cases(L5_attempt),]
    M10_attempt<-M10_attempt[complete.cases(M10_attempt),]
    L5_start_time<-L5_attempt[L5_attempt$RollAvg == min(L5_attempt$RollAvg), "Time"]
    M10_start_time<-M10_attempt[M10_attempt$RollAvg == max(M10_attempt$RollAvg), "Time"]
    L5_start_time<-L5_start_time / 60
    M10_start_time<-M10_start_time / 60
    if(L5_start_time == 24){L5_start_time<-0.1}
    if(M10_start_time == 24){M10_start_time<-0.1}
    L5_start_time<-strptime(paste(floor( L5_start_time), round(( L5_start_time-floor( L5_start_time))*60), sep=":"), format="%H:%M")
    M10_start_time<-strptime(paste(floor( M10_start_time), round(( M10_start_time-floor( M10_start_time))*60), sep=":"), format="%H:%M")
    L5<-min(L5_attempt$RollAvg)
    M10<-max(M10_attempt$RollAvg)
    
    
     ## --------------------------------
    RA <- round((M10-L5)/(M10+L5), digits = 2)
    result <- data.frame(L5, L5_start_time, M10, M10_start_time, RA)
    result$L5_start_time<-chron(times=as.factor(substr(result$L5_start_time, start = 12, stop = 19)))
    result$M10_start_time<-chron(times=as.factor(substr(result$M10_start_time, start = 12, stop = 19)))
    
    result<-data.frame(cbind(id, IS, IV, result, length(hold$Percent), NoofDays))
    colnames(result)[length(result)-1]<-"DaysDroppedforCompliance"
    colnames(result)[length(result)]<-"AnalysisStartingNoofWholeDaysofCollectedData"
    datalist[[increment]]<-result
     }
    #########################################################################
    #########################################################################

big_data = do.call(rbind, datalist)
bigdata_2<-big_data[!duplicated(big_data$id),]

write.csv(bigdata_2, "C:/Users/julie.wisch/Documents/Actigraphy_YoEl/Processed20190605.csv")
      ## ---- Plot Minutewise averages

        nparACT_auxfunctions2$nparACT_plot_minaverage(data, minaverage, start.time, a, SR)
 


