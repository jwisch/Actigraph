library(nparACT)
library(Actigraphy)
library(lattice)
library(dplyr)
source("C:/Users/julie.wisch/Documents/nparACT_auxfunctions1.R")
source("C:/Users/julie.wisch/Documents/nparACT_auxfunctions2.R")
source("C:/Users/julie.wisch/Documents/npar_Base.R")
source("C:/Users/julie.wisch/Documents/Actigraphy_otherData/CommonFuncs.R")
#######
file_list <- list.files(path="C:/Users/julie.wisch/Documents/Actigraphy/", pattern="*.agd") 
file_list<-file_list[c(1:19, 21:94, 96:117, 119:133, 137:143)]

setwd("C:/Users/julie.wisch/Documents/Actigraphy/")
myfiles = lapply(file_list, readActigraph)

datalist = list()

for (i in 1:length(file_list)) {
  result<-data.frame(SetUp(myfiles[[i]]))
  result$id <- substr(file_list[i], start = 1, stop = 9)
  result$Timepoint <- substr(file_list[i], start = 11, stop = 12)
  datalist[[i]] <- result
}

result = do.call(rbind, datalist)

df_T1<-result[result$Timepoint == "T1",]
df_T2<-result[result$Timepoint == "T2",]
df_T3<-result[result$Timepoint == "T3",]


df_hold_T1<-GetToActFormat(df_T1)
df_hold_T2<-GetToActFormat(df_T2)
df_hold_T3<-GetToActFormat(df_T3)

#Then have to drop participants that aren't in all the datasets

df_hold_T1<-select(df_hold_T1, names(df_hold_T3))
df_hold_T2<-select(df_hold_T2, names(df_hold_T3))


#Getting their cohort info
Info<-read.csv("C:/Users/julie.wisch/Documents/Actigraphy_otherData/HECS_Data_021219.csv")
Info<-Info[,c("related_study_id", "Group")]
Info$id<-paste("HECS", substr(Info$related_study_id, 1, 5), sep = "")
Info<-Info[,c("id", "Group")]
Info<-Info[!duplicated(Info$id),]

Exercisers<-Info[Info$Group == 1,"id"]
Exercisers<-Exercisers[!is.na(Exercisers)]
Exercisers<-c("time", Exercisers)

##############################################################################
#Setting things up and getting data frames for analysis together
#Have data frames with only the exercisers in them
df_T1<-df_hold_T1[,colnames(df_hold_T1) %in% Exercisers]
df_T2<-df_hold_T2[,colnames(df_hold_T2) %in% Exercisers]
df_T2_stretch<-df_hold_T2[,!(colnames(df_hold_T2) %in% Exercisers)]
df_T3<-df_hold_T3[,colnames(df_hold_T3) %in% Exercisers]

clinic<-data.frame(c(names(df_T2[-1]),names(df_T2_stretch[-1])), c(rep(-1, length(df_T2[-1])), rep(1, length(df_T2_stretch[-1]))))
colnames(clinic)<-c("id", "Timepoint")

data<-data.frame(cbind(df_T2, df_T2_stretch[,-1]))

#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

matchid  <- fda.matchid(data[,-1], clinic, "contin") #Can analyze relative to a continuous measure as well
matchid<-fda.matchid(data[,-1], clinic, type = "factor", grouplab = c("excer", "stretch"))
FDcont <- fda.smoothdata(matchid)

geftFDcont <- flm_cate(FDcont)
par(mfrow=c(1,1))

### Smooth the Results
ts.plot(predict(FDcont$fd$fd, c(1:length(data[,1]))), main="Smoothed Activity Data")
result<-flm_cate(FDcont, basistype="fourier", nbasis=9, norder=4)

ahidatav2<-fda.matchid(data[,-1], clinic, type = "factor", grouplab = c("excer", "stretch"))
tempv2 <- ahidatav2[[2]]
tempv2[,3] <- ifelse(tempv2[,3] == 0, -1, 1)
ahidatav2$cov <- data.frame(id=tempv2$id, mean=1, ahi=tempv2[,3])
colv2 <- ifelse(tempv2[,3] == -1, 4, 2)
smoothDatav2 <- fda.smoothdata(ahidatav2)
par(mfrow=c(1,1))
ts.plot(predict(smoothDatav2$fd$fd, c(1:length(data[,1]))), main="Smoothed Activity Data", ylim = c(0, 2800))

geftahiv2 <- flm_cate(smoothDatav2, basistype="fourier", nbasis=9, norder=4)
meanefv2 <- geftahiv2$freg$betaestlist[[1]]
ahiefv2 <- geftahiv2$freg$betaestlist[[2]]
### Plot Options and Parameters
L <- 1440
xat <- c(0, L/4, L/2, 3*L/4, L)
lb <- c("Midnight", "6AM", "Noon", "6PM", "Midnight")
### Plot Figure
ts.plot(predict(smoothDatav2$fd$fd, c(1:length(data[,1]))), main="Smoothed Activity Data")

par(mfrow=c(2,1), mar=c(4,4,3,1))
plot(0, 0, xlim=c(0,L), ylim=c(600,2600), xaxt="n", xlab="(a)", ylab="Activity", type="n", main="Smoothed Circadian Activity Curves ")
for(i in 1:(length(data)-1)){lines(predict(smoothDatav2$fd$fd, c(1:L))[,i], col=colv2[i], lwd = 0.2, lty = 2) }

### Plot the group mean activities
lines(meanefv2$fd-ahiefv2$fd, col=4, lwd=3)
lines(meanefv2$fd+ahiefv2$fd, col=2, lwd=3)
### Plot the overall mean
lines(meanefv2$fd, col=1, lwd=1)
axis(1, at=xat, labels=lb)

plot(0, 0, xlim=c(0,L), ylim=c(600,2600), xaxt="n", xlab="(a)", ylab="Activity", type="n", main="Average Circadian Activity Curves ")
### Plot the group mean activities
lines(meanefv2$fd-ahiefv2$fd, col=4, lwd=3)

lines(meanefv2$fd+ahiefv2$fd, col=2, lwd=3)

### Add the axis and legend to finish the plot
axis(1, at=xat, labels=lb)
#legend("topleft", c("AHI High Curves", "AHI High Mean", "AHI Low Curves", "AHI Low Mean ", "Overall Mean"),
#lty=1, col=c(4,4,2,2,1), lwd=c(1,3,1,3,3), cex=.8)
### F Test
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


##############Plotting options follow
#################################################################################################
#################################################################################################
#################################################################################################
###This block lets you smooth multiple files from an individual and create a plot that shows their activity for each of the times they were sampled
library(PhysicalActivity)
library(RSQLite)


plots<-IndividualPlots("HECS80001", 3)
grid.arrange(plots[[1]], plots[[2]], plots[[3]], ncol = 1)

########################################################################################
########################################################################################
########################################################################################
########################################################################################



#This is how to make 3 plots - one smoothed by mins, one smoothed by hours, one showing each day of the week
PARTICIPANTID<-"HECS80001"
df1<-readActigraph(paste("C:/Users/julie.wisch/Documents/Actigraphy/", PARTICIPANTID, "_T1.agd", sep= ""))
df1<-df1[,c("TimeStamp", "vm")]
df1<-dataCollapser(df1, TS = "TimeStamp", col = "vm", by = 60)
bin_hr <- 60 
SR<-1/60
cutoff<-1
nparACT_plot(df1, SR, cutoff = 1, plot = T, fulldays = T)

rm(df1)



########################################################################################
########################################################################################
########################################################################################
########################################################################################

