MatchbyNearestDate<-function(df1, df2, ID, Date1, Date2){
  z <- lapply(intersect(df1[,ID],df2[,ID]),function(id) {
    df1 <- df1[df1[,ID] == id,]
    df2 <- df2[df2[,ID] == id,]
    
    df1[,"indices"] <- sapply(df1[,Date1],function(d) which.min(abs(df2[,Date2] - d)))
    df2[,"indices"] <- 1:nrow(df2)
    
    merge(df1,df2,by=c(ID,'indices'))
  })
  
  df_matched <- do.call(rbind,z)
  df_matched$indices <- NULL
  return(df_matched)
}

library(PhysicalActivity)
library(RSQLite)

IndividualPlots<-function(PARTICIPANTID, NoOfVisits = 3){
  
  df1<-readActigraph(paste("C:/Users/julie.wisch/Documents/Actigraphy/", PARTICIPANTID, "_T1.agd", sep= ""))
  # df1 <- wearingMarking(dataset = df1,
  #                       frame = 90,
  #                       perMinuteCts = 1,
  #                       TS = "TimeStamp",
  #                       cts = "axis1",
  #                       streamFrame = NULL,
  #                       allowanceFrame= 2,
  #                       newcolname = "wearing")
  df1$Date <- as.Date(df1$TimeStamp)
  df1$Time <- format(df1$TimeStamp,"%H:%M:%S")
  d <- dataCollapser(df1, TS = "TimeStamp", col = "steps", by = 600)
  
  p1<-ggplot(d, aes(x = TimeStamp, y = steps)) +
    geom_line(size = .5, alpha = .5) + ylim(c(min(df1$vm), max(df1$vm)))+
    xlab("Time") + ylab("steps") + ggtitle("Timepoint 1")
  
  if(NoOfVisits > 1){
    df2<-readActigraph(paste("C:/Users/julie.wisch/Documents/Actigraphy/", PARTICIPANTID, "_T2.agd", sep = ""))
    # df2 <- wearingMarking(dataset = df2,
    #                       frame = 90,
    #                       perMinuteCts = 1,
    #                       TS = "TimeStamp",
    #                       cts = "axis1",
    #                       streamFrame = NULL,
    #                       allowanceFrame= 2,
    #                       newcolname = "wearing")
    df2$Date <- as.Date(df2$TimeStamp)
    df2$Time <- format(df2$TimeStamp,"%H:%M:%S")
    d <- dataCollapser(df2, TS = "TimeStamp", col = "steps", by = 600)
    
    p2<-ggplot(d, aes(x = as.POSIXct(TimeStamp), y = steps)) +
      geom_line(size = .5, alpha = .5) + ylim(c(min(df1$vm), max(df1$vm)))+
      xlab("Time") + ylab("steps") + ggtitle("Timepoint 2")}
  if(NoOfVisits > 2){
    df3<-readActigraph(paste("C:/Users/julie.wisch/Documents/Actigraphy/", PARTICIPANTID, "_T3.agd", sep = ""))
    # df3 <- wearingMarking(dataset = df3,
    #                       frame = 90,
    #                       perMinuteCts = 1,
    #                       TS = "TimeStamp",
    #                       cts = "axis1",
    #                       streamFrame = NULL,
    #                       allowanceFrame= 2,
    #                       newcolname = "wearing")
    df3$Date <- as.Date(df3$TimeStamp)
    df3$Time <- format(df3$TimeStamp,"%H:%M:%S")
    d <- dataCollapser(df3, TS = "TimeStamp", col = "steps", by = 600)
    
    p3<-ggplot(d, aes(x = as.POSIXct(TimeStamp), y = steps)) +
      geom_line(size = .5, alpha = .5) + ylim(c(min(df1$vm), max(df1$vm)))+
      xlab("Time") + ylab("steps") + ggtitle("Timepoint 3")+labs(caption = PARTICIPANTID)}
  
  
  plots<-list()
  plots<-list(p1, p2, p3)
  return(plots)}


NonparStats<-function(PARTICIPANTID, NoOfVisits = 3){
  bin_hr <- 60 
  SR<-1/60
  cutoff<-1
  
  df1 <- wearingMarking(dataset = df1, frame = 90,perMinuteCts = 1, TS = "TimeStamp",
                        cts = "axis1", streamFrame = NULL, allowanceFrame= 2, newcolname = "wearing")
  df1$Date <- as.Date(df1$TimeStamp)
  df1$Time <- format(df1$TimeStamp,"%H:%M:%S")
  df1_cleaned<-df1[df1$wearing == "w",]
  d <- dataCollapser(df1, TS = "TimeStamp", col = "steps", by = 60)
  
  
  df1<-readActigraph(paste("C:/Users/julie.wisch/Documents/Actigraphy/", PARTICIPANTID, "_T1.agd", sep= ""))
  df1<-df1[,c("TimeStamp", "vm")]  
  df1<-dataCollapser(df1, TS = "TimeStamp", col = "vm", by = 10)
  result1<-npar_Base(df1, bin_hr, SR, cutoff)
  result1$ID<-PARTICIPANTID
  result1$Timepoint<-1
  if(NoOfVisits > 1){
    df2<-readActigraph(paste("C:/Users/julie.wisch/Documents/Actigraphy/", PARTICIPANTID, "_T2.agd", sep= ""))
    df2<-df2[,c("TimeStamp", "vm")]  
    df2<-dataCollapser(df2, TS = "TimeStamp", col = "vm", by = 10)
    result2<-npar_Base(df2, bin_hr, SR, cutoff)
    result2$ID<-PARTICIPANTID
    result2$Timepoint<-2 }
  if(NoOfVisits > 2){
    df3<-readActigraph(paste("C:/Users/julie.wisch/Documents/Actigraphy/", PARTICIPANTID, "_T3.agd", sep= ""))
    df3<-df3[,c("TimeStamp", "vm")]  
    df3<-dataCollapser(df3, TS = "TimeStamp", col = "vm", by = 10)
    result3<-npar_Base(df3, bin_hr, SR, cutoff)
    result3$ID<-PARTICIPANTID
    result3$Timepoint<-3 }
  result<-data.frame(rbind(result1, result2, result3))
  return(result)}


#This loops through and grabs all of the vm data and puts it in the wide format that matches act_8pt from
#Wang et al, 2011
GetToActFormat<-function(Timepoint_DF){
  df_hold<-result[Timepoint_DF[,"id"]== unique(Timepoint_DF[,"id"])[1], c("Day", "Time", "vm")]
  colnames(df_hold)[length(df_hold)]<-unique(Timepoint_DF[,"id"])[1]
  for(i in 2:length(unique(Timepoint_DF[,"id"]))){
    df_hold2<-result[Timepoint_DF[,"id"] == unique(Timepoint_DF[,"id"])[i],c("Day", "Time", "vm")]
    colnames(df_hold2)[length(df_hold2)]<-unique(Timepoint_DF[,"id"])[i]
    
    df_hold<-merge(df_hold, df_hold2, by = c("Day", "Time"), all = TRUE)
    df_hold <- df_hold[!duplicated(paste(df_hold$Day, df_hold$Time, sep = "")),]
  }
  df_hold_T<-df_hold[df_hold$Day != "Saturday" & df_hold$Day != "Sunday",]
  #Reorder
  df_hold_T<-data.frame(rbind(df_hold_T[df_hold_T$Day == "Monday",], df_hold_T[df_hold_T$Day == "Tuesday",],
                              df_hold_T[df_hold_T$Day == "Wednesday",], df_hold_T[df_hold_T$Day == "Thursday",],
                              df_hold_T[df_hold_T$Day == "Friday",]))
  df_hold_T$time<-seq(from = 1, to = length(df_hold_T[,1]), by = 1)
  df_hold_T<-df_hold_T[,c(length(df_hold_T), 3:(length(df_hold_T)-1))]
  #Could drop the weekend?

  return(df_hold_T)}

SetUp<-function(ListItem){
  bin_hr <- 60 
  SR<-1/60
  cutoff<-1
  df1<-ListItem
  df1<-df1[,c("TimeStamp", "vm")]  
  df1<-df1[complete.cases(df1),]
  df1<-dataCollapser(df1, TS = "TimeStamp", col = "vm", by = 60)
  df1$Date <- as.Date(df1$TimeStamp)
  df1$Time <- format(df1$TimeStamp,"%H:%M:%S")
  df1$Day <- weekdays(as.Date(df1$Date))
  return(df1)}


IndividualPlots<-function(PARTICIPANTID, NoOfVisits = 3){
  
  df1<-readActigraph(paste("C:/Users/julie.wisch/Documents/Actigraphy/", PARTICIPANTID, "_T1.agd", sep= ""))
  df1 <- wearingMarking(dataset = df1, frame = 90,perMinuteCts = 1, TS = "TimeStamp",
                        cts = "axis1", streamFrame = NULL, allowanceFrame= 2, newcolname = "wearing")
  df1$Date <- as.Date(df1$TimeStamp)
  df1$Time <- format(df1$TimeStamp,"%H:%M:%S")
  df1_cleaned<-df1[df1$wearing == "w",]
  d <- dataCollapser(df1, TS = "TimeStamp", col = "steps", by = 60)
  d_cleaned <- dataCollapser(df1_cleaned, TS = "TimeStamp", col = "steps", by = 60)
  
  p1<-ggplot(d, aes(x = TimeStamp, y = steps)) +
    geom_line(size = .5, alpha = .5) + 
    xlab("Time") + ylab("steps") + ggtitle("Timepoint 1 - raw data")+ ylim(c(0, 105))
  
  p2<-ggplot(d_cleaned, aes(x = TimeStamp, y = steps)) +
    geom_line(size = .5, alpha = .5) + 
    xlab("Time") + ylab("steps") + ggtitle("Timepoint 1 - cleaned data") + ylim(c(0, 105))
  
  grid.arrange(p1, p2)
  if(NoOfVisits > 1){
    df2<-readActigraph(paste("C:/Users/julie.wisch/Documents/Actigraphy/", PARTICIPANTID, "_T2.agd", sep = ""))
    # df2 <- wearingMarking(dataset = df2,
    #                       frame = 90,
    #                       perMinuteCts = 1,
    #                       TS = "TimeStamp",
    #                       cts = "axis1",
    #                       streamFrame = NULL,
    #                       allowanceFrame= 2,
    #                       newcolname = "wearing")
    df2$Date <- as.Date(df2$TimeStamp)
    df2$Time <- format(df2$TimeStamp,"%H:%M:%S")
    d <- dataCollapser(df2, TS = "TimeStamp", col = "steps", by = 600)
    
    p2<-ggplot(d, aes(x = as.POSIXct(TimeStamp), y = steps)) +
      geom_line(size = .5, alpha = .5) + ylim(c(min(df1$vm), max(df1$vm)))+
      xlab("Time") + ylab("steps") + ggtitle("Timepoint 2")}
  if(NoOfVisits > 2){
    df3<-readActigraph(paste("C:/Users/julie.wisch/Documents/Actigraphy/", PARTICIPANTID, "_T3.agd", sep = ""))
    # df3 <- wearingMarking(dataset = df3,
    #                       frame = 90,
    #                       perMinuteCts = 1,
    #                       TS = "TimeStamp",
    #                       cts = "axis1",
    #                       streamFrame = NULL,
    #                       allowanceFrame= 2,
    #                       newcolname = "wearing")
    df3$Date <- as.Date(df3$TimeStamp)
    df3$Time <- format(df3$TimeStamp,"%H:%M:%S")
    d <- dataCollapser(df3, TS = "TimeStamp", col = "steps", by = 600)
    
    p3<-ggplot(d, aes(x = as.POSIXct(TimeStamp), y = steps)) +
      geom_line(size = .5, alpha = .5) + ylim(c(min(df1$vm), max(df1$vm)))+
      xlab("Time") + ylab("steps") + ggtitle("Timepoint 3")+labs(caption = PARTICIPANTID)}
  
  
  plots<-list()
  plots<-list(p1, p2, p3)
  return(plots)}


DoTheFTest<-function(DF){
  smoothDatav2<-DF
  cov2 <- smoothDatav2$cov[, -1]
  grp2 <- ncol(cov2)
  fd <- smoothDatav2$fd
  L <- length(fd$argvals)
  npt <- ncol(fd$y)
  fbase <- create.fourier.basis(rangeval=c(0, length(data[,1])), nbasis=9)
  fpar <- fdPar(fbase)
  xfdlist <- vector("list", grp2)
  xfdlist[[1]] <- cov2[, 1] + 0
  for(i in 2:grp2){
    xfdlist[[i]] <- cov2[, i] + 0}
  betalist <- xfdlist
  for(i in 1:grp2){
    betalist[[i]] <- fpar}
  freg2 <- fRegress(fd$fd, xfdlist, betalist)
  preact2 <- predict(freg2$yhatfdobj, c(1:L))
  resid2 <- fd$y - preact2[, 1:npt]
  sigma2 <- cov(t(resid2))
  fregstd2 <- fRegress.stderr(freg2, fd$y2cMap, sigma2)
  Fratio <- Ftest(fd$fd, xfdlist, betalist, argvals = c(1:length(data$T1)), nperm=1000, xaxt="n")
  return(Fratio)
}


SmoothCurves<-function(data, clinic){
  matchid<-fda.matchid(data[,-1], clinic, type = "factor", grouplab = c("excer", "stretch"))
  FDcont <- fda.smoothdata(matchid)
  
  geftFDcont <- flm_cate(FDcont)
  par(mfrow=c(1,1))
  
  ### Smooth the Results
  #ts.plot(predict(FDcont$fd$fd, c(1:length(data[,1]))), main="Smoothed Activity Data")
  result<-flm_cate(FDcont, basistype="fourier", nbasis=9, norder=4)
  
  ahidatav2<-fda.matchid(data[,-1], clinic, type = "factor", grouplab = c("excer", "stretch"))
  tempv2 <- ahidatav2[[2]]
  tempv2[,3] <- ifelse(tempv2[,3] == 0, -1, 1)
  ahidatav2$cov <- data.frame(id=tempv2$id, mean=1, ahi=tempv2[,3])
  colv2 <- ifelse(tempv2[,3] == -1, 4, 2)
  smoothDatav2 <- fda.smoothdata(ahidatav2)
  results<-list()
  results<-list(colv2, smoothDatav2)
  return(results)}

GetClinic<-function(DF, DF_STRETCH){
  clinic<-data.frame(c(names(DF[-1]),names(DF_STRETCH[-1])), c(rep(-1, length(DF[-1])), rep(1, length(DF_STRETCH[-1]))))
  colnames(clinic)<-c("id", "Timepoint")
  return(clinic)}
