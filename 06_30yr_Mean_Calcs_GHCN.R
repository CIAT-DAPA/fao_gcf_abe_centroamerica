#Julian Ramirez
#University of Leeds / CIAT
#17 March 2011

##############################################################
#Calculating 30-yr running means
##############################################################

#Calculate 30 year running average between 1961-1990, for comparisons with the same period of GCM data
#Load stations, and then per station calculate the average over the desired period for each month
baselineMean <- function(outvn, wd="") {
  setwd(wd)
  #outvn <- "rain_adj"
  
  #Loading station data
  od <- paste("./organized-data/", outvn, "-per-station", sep="")
  st.data <- read.csv(paste("./organized-data/ghcn_", outvn, "_data_all_xy.csv", sep=""))
  
  #Looping through stations to average and count number of years with data per month
  stations <- unique(st.data$ID)
  for (st in stations) {
    st.years <- read.csv(paste(od, "/", st, ".csv", sep=""))
    st.years <- st.years[which(st.years$YEAR >= 1970 & st.years$YEAR <= 2000),]
    
    #Calculating mean and counting
    st.mean <- t(colMeans(st.years[,2:ncol(st.years)], na.rm=T))
    st.count <- apply(st.years[,2:ncol(st.years)], 2, FUN=function(x) {length(which(!is.na(x)))}); st.count <- t(st.count)
    
    #Creating output data frame
    if (st == stations[1]) {
      summ.mean <- c(ID=st,st.mean)
      summ.count <- c(ID=st,st.count)
    } else {
      summ.mean <- rbind(summ.mean, c(ID=st,st.mean))
      summ.count <- rbind(summ.count, c(ID=st,st.count))
    }
  }
  
  #Fixing final data frames
  summ.mean <- as.data.frame(summ.mean); summ.mean <- summ.mean[,c(1,3:18)]
  names(summ.mean) <- c("ID","REPLICATED","LAT","LONG","ALT","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  
  summ.count <- as.data.frame(summ.count); summ.count <- summ.count[,c(1,3:18)]
  names(summ.count) <- c("ID","REPLICATED","LAT","LONG","ALT","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  summ.count$REPLICATED <- summ.mean$REPLICATED; summ.count$LAT <- summ.mean$LAT; summ.count$LONG <- summ.mean$LONG; summ.count$ALT <- summ.mean$ALT
  
  #Writing final data frames
  write.csv(summ.mean, paste("./organized-data/ghcn_", outvn, "_1970_2000_mean.csv", sep=""), row.names=F, quote=F)
  write.csv(summ.count, paste("./organized-data/ghcn_", outvn, "_1970_2000_count.csv", sep=""), row.names=F, quote=F)
}


baselineMean("rain", "S:/observed/weather_station/ghcn")
baselineMean("rain_adj", "S:/observed/weather_station/ghcn")
baselineMean("tmax", "S:/observed/weather_station/ghcn")
baselineMean("tmax_adj", "S:/observed/weather_station/ghcn")
baselineMean("tmin", "S:/observed/weather_station/ghcn")
baselineMean("tmin_adj", "S:/observed/weather_station/ghcn")
baselineMean("tmean", "S:/observed/weather_station/ghcn")
baselineMean("tmean_adj", "S:/observed/weather_station/ghcn")
