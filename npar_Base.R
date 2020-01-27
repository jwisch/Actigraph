nparACT_data_hrs<-function(data, a, m){
  data_hrs <- matrix(NA, nrow = a/m) 
  for (i in 1:(a/m)){
    subset_h <- data$activity[(((i-1)*m)+1):((i*m))]
    mean_subset_h <- mean(subset_h)
    data_hrs[i] <- mean_subset_h 
  }
  return(data_hrs)
}

npar_Base<-function(data, bin_hr, SR, cutoff){
names(data)[1] <- "time"
names(data)[2] <- "activity"


 
a <- nrow(data) 
e <- SR*60 ## samples per minute
m <- bin_hr*SR*60  ## samples per hour
full_days <- floor(a/(e*bin_hr*24))

## --- Cut data to full days
  data <- data[1:(e*bin_hr*24*full_days),]

b <- floor(a/(SR*60)) ## full minutes recorded
## ------------------------------------------

## ---- Filtering, Cutoff for classification as movement
nparACT_auxfunctions1$nparACT_filt(data, a, cutoff)
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
## -----------------------------------------------------------------------------
library(stringr)
## ---- Plot hourly data


## ---- IS/IV calculation (based on data_hrs!)
result_ISIV <- nparACT_ISIVfunctions$nparACT_ISIV(data_hrs, bin_hr)
IS <- result_ISIV[1]
IV <- result_ISIV[2]
## ---------------------------------------------------------------------------------

## ---------- Relative Amplitude (RA) calculation
## ---- Minutewise averages across 24hrs
minaverage <- nparACT_auxfunctions1$nparACT_minaverage(a, data_min)
## --------------------------------


## ---- L5, M10, RA calculation
result_RA <- nparACT_RAfunctions$nparACT_L5M10(data, minaverage, a, SR)
L5 <- result_RA[1]
L5_starttime <- result_RA[2]
M10 <- result_RA[3]
M10_starttime <- result_RA[4]
RA <- result_RA[5]

nparACT_result <- data.frame(IS, IV, RA, L5, L5_starttime, M10, M10_starttime)
return(nparACT_result)}



nparACT_plot <-
  function (data, SR, cutoff = 1, plot = T, fulldays = T){
      data[,1] <- as.POSIXct(data[,1])
      data[,2] <- as.numeric(as.character(data[,2]))
      names(data)[1] <- "time"
      names(data)[2] <- "activity"

    
    bin_hr <- 60  
    a <- nrow(data) 
    e <- SR*60 ## samples per minute
    m <- bin_hr*SR*60  ## samples per hour
    full_days <- floor(a/(e*bin_hr*24))
    
    ## --- Cut data to full days
    if (fulldays == T){
      data <- data[1:(e*bin_hr*24*full_days),]
    }
    a <- nrow(data) 
    b <- floor(a/(SR*60)) ## full minutes recorded
    ## ------------------------------------------
    
    ## ---- Filtering, Cutoff for classification as movement
    nparACT_auxfunctions1$nparACT_filt(data, a, cutoff)
    ## ------------------------------------------
    
    ## ---- Calculate average for each minute (needed if SR != 1/60)
      data_min <- data$activity
    ## ------------------------------------------
    
    ## ---- Calculate hourly averages
    data_hrs <- nparACT_auxfunctions1$nparACT_data_hrs(data, a, m)
    ## -----------------------------------------------------------------------------
    
    ## ---- Plot hourly data
    if (plot == T) {
      nparACT_auxfunctions2$nparACT_plot_hourly(data, data_hrs, SR)
    }
    
    ## ---------------------------------------------------------------------------------
    
    ## ---------- Relative Amplitude (RA) calculation
    ## ---- Minutewise averages across 24hrs
    minaverage <- nparACT_auxfunctions1$nparACT_minaverage(a, data_min)
    ## --------------------------------
    
    ## ---- Plot Minutewise averages
    if (plot == T){
      start.time <- NULL
      nparACT_auxfunctions2$nparACT_plot_minaverage(data, minaverage, start.time, a, SR)
    }
    ## --------------------------------      
    
    ## ---- Plot Hourly averages
    if (plot == T){
      nparACT_auxfunctions2$nparACT_plot_hraverage(data, minaverage, start.time, a, SR)
    }
    ## --------------------------------      
    
  }
