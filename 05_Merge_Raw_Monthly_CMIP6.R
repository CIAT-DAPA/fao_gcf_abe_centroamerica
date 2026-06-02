# Author: Carlos Navarro
# UNIGIS 2022
# Purpose: Average historical raw CMIP6 files

library(ncdf4)
library(raster)
library(maptools)
library(stringr)

dirwork <- "E:/cmip6_raw_historical"
outdir <- "E:/cmip6_raw_historical/monthly" 
avgdir <- "D:/cenavarro/msc_gis_thesis/01_baseline/gcm_data/1970_2000"
# cdo <- "C:/cdo/cdo.exe"
mask1deg <- raster(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, res=1, vals=1)

monthList <- c("01","02","03","04","05","06","07","08","09","10","11","12")
monthListMod <- c(1:12)
ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)

# Combine number of month and days in one single data frame
ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
names(ndaymtx) <- c("Month", "Ndays", "MonthMod")
varList <- c("pr", "tasmax", "tasmin")
varListMod <- c("prec", "tmin", "tmax")

# Get GCM list
listNC <-  list.files(paste0(dirwork), recursive = F, full.names = T,pattern = ".nc")   
gcmList <- unique(sapply(strsplit(basename(listNC), '[_]'), "[[", 3))

for(gcm in gcmList){
  
  listNc_gcm <-  list.files(paste0(dirwork), recursive = F, full.names = T,pattern = paste0("*", gcm, "*") )
  
  oDirGCM <- paste0(outdir, "/", tolower(str_replace(gcm, "-", "_")) )
  if (!file.exists(oDirGCM)) {dir.create(oDirGCM, recursive=T)}
  
  oDirAvgGCM <- paste0(avgdir, "/", tolower(str_replace(gcm, "-", "_")) )
  if (!file.exists(oDirAvgGCM)) {dir.create(oDirAvgGCM, recursive=T)}
  
  for(var in varList){
    
    listNc_gcm_var <- listNc_gcm[grepl(var,listNc_gcm)]

    for (i in 1:length(listNc_gcm_var)){

      ncStk <- stack(listNc_gcm_var[i])
      dates <- names(ncStk)

      for (n in 1:nlayers(ncStk)){

        if( as.numeric(substring(str_replace(substr(dates[n], 2, 8), "[.]", ""), 1, 4)) >= 1970 &&
            as.numeric(substring(str_replace(substr(dates[n], 2, 8), "[.]", ""), 1, 4)) <= 2000){

          oNc <- paste0(oDirGCM, "/", var, ".", str_replace(substr(dates[n], 2, 8), "[.]", ""), ".nc")

          if (!file.exists(oNc)) {
            writeRaster(ncStk[[n]], oNc)
          }

        }

      }

    }
    
    for (mth in monthList){
      
      listNC_period <-  list.files(paste0(oDirGCM), recursive = F, full.names = T,pattern = paste0(mth, ".nc"))
      listNC_period_var <- listNC_period[grepl(var,listNC_period)]
      
      mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
      
      
      if (var == "pr"){
        
        daysmth <- as.numeric(paste((ndaymtx$Ndays[which(ndaymtx$Month == mth)])))
        varMod <- "prec"
        mthNcAvg <- rotate(mean(stack(listNC_period_var))) * 86400 * (daysmth)
        
      } else {
        
        if(var == "tasmax") {varMod <- "tmax"}
        if(var == "tasmin") {varMod <- "tmin"}
        
        mthNcAvg <- rotate(mean(stack(listNC_period_var))) - 273.15
        
      }
      
      mthNcAvgStk <- resample(mthNcAvg, mask1deg)
      oAvg <- paste0(oDirAvgGCM, "/", varMod, "_", mthMod, ".nc")
      
      if (!file.exists(oAvg)){
        writeRaster(mthNcAvgStk, oAvg)
      }
      
      cat(paste(".> Average", gcm, varMod, mth, "done!\n"))
      
      if(var == "tasmin") {
        oAvgTmean <- mean(stack(paste0(oDirAvgGCM, "/", c("tmin", "tmax"), "_", mthMod, ".nc")))
        writeRaster(oAvgTmean, paste0(oDirAvgGCM, "/tmean_", mthMod, ".nc"))
        cat(paste(".> Average", gcm, "tmean", mth, "done!\n"))
        }
      

      
    }
    
  }
  
}
