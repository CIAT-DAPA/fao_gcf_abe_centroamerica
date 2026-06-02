# Author: Carlos Navarro
# UNIGIS 2022
# Purpose: Average historical raw CMIP6 files

library(ncdf4)
library(raster)
library(maptools)
library(stringr)

dirwork <- "E:/cmip6_raw_future"
outdir <- "E:/cmip6_raw_future/monthly" 
avgdir <- "E:/cmip6_raw_future/average"
hisdir <- "D:/cenavarro/msc_gis_thesis/01_baseline/gcm_data/1970_2000"
deldir <- "D:/cenavarro/msc_gis_thesis/02_climate_change/camexca_2_5min"
deldirint <- "D:/cenavarro/msc_gis_thesis/02_climate_change/camexca_2_5min_int"
wcldir <- "D:/cenavarro/msc_gis_thesis/01_baseline/wcl_v21_2_5min"

# cdo <- "C:/cdo/cdo.exe"
mask1deg <- raster(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, res=1, vals=1)
msk <- raster("D:/cenavarro/msc_gis_thesis/00_admin_data/camexca_msk_2_5m_v2.tif")

monthList <- c("01","02","03","04","05","06","07","08","09","10","11","12")
monthListMod <- c(1:12)
ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)

# Combine number of month and days in one single data frame
ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
names(ndaymtx) <- c("Month", "Ndays", "MonthMod")
# varList <- c("pr", "tasmax", "tasmin")
varList <- c("tasmax")
perList <- c("2021_2040", "2041_2060", "2061_2080", "2081_2100")
perListMod <- c("2030s", "2050s", "2070s", "2090s")

# Get GCM list
listNC <-  list.files(paste0(dirwork), recursive = F, full.names = T,pattern = ".nc")   
gcmList <- unique(sapply(strsplit(basename(listNC), '[_]'), "[[", 3))

for(gcm in gcmList){
  
  listNc_gcm <-  list.files(paste0(dirwork), recursive = F, full.names = T,pattern = paste0("*", gcm, "*") )
  
  for (p in 1:length(perList)){
    
    yi <- str_split(perList[p], "_")[[1]][1]
    yf <- str_split(perList[p], "_")[[1]][2]
    
    for(var in varList){
      
      listNc_gcm_var <- listNc_gcm[grepl(var,listNc_gcm)]
      
      for (i in 1:length(listNc_gcm_var)){
        
        ssp <- str_split(basename(listNc_gcm_var[i]), "_")[[1]][4]
        
        oDirGCM <- paste0(outdir, "/", ssp, "/", tolower(str_replace_all(gcm, "-", "_")) )
        if (!file.exists(oDirGCM)) {dir.create(oDirGCM, recursive=T)}
        
        oDirAvgGCM <- paste0(avgdir, "/", ssp, "/", tolower(str_replace_all(gcm, "-", "_")))
        if (!file.exists(oDirAvgGCM)) {dir.create(oDirAvgGCM, recursive=T)}
        
        ncStk <- stack(listNc_gcm_var[i])
        dates <- names(ncStk)
        
        for (n in 1:nlayers(ncStk)){
          
          if( as.numeric(substring(str_replace_all(substr(dates[n], 2, 8), "[.]", ""), 1, 4)) >= yi &&
              as.numeric(substring(str_replace_all(substr(dates[n], 2, 8), "[.]", ""), 1, 4)) <= yf){
            
            oNc <- paste0(oDirGCM, "/", var, ".", str_replace_all(substr(dates[n], 2, 8), "[.]", ""), ".nc")
            
            if (!file.exists(oNc)) {
              writeRaster(ncStk[[n]], oNc)
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
          
          mthNcAvgStk1deg <- resample(mthNcAvg, mask1deg)
          oAvg <- paste0(oDirAvgGCM, "/", varMod, "_", mthMod, ".nc")
          
          if (!file.exists(oAvg)){
            writeRaster(mthNcAvgStk1deg, oAvg)
          }
          
          cat(paste(".> Average", gcm, varMod, mth, "done!\n"))
          
          
          histNcAvg1deg <- raster(paste0(hisdir, "/", tolower(str_replace_all(gcm, "-", "_")), "/", varMod, "_", mthMod, ".nc"))
          mthAnm1deg <- mthNcAvgStk1deg - histNcAvg1deg
          
          mthAnmStk <- mask(resample(crop(mthAnm1deg, msk), msk), msk) 
          wclStk <-  raster(paste0(wcldir, "/", varMod, "_", mthMod, ".tif")) 
          
          
          oDel <- paste0(deldir, "/ssp_", str_split(ssp, "ssp")[[1]][2], "/", 
                         tolower(str_replace_all(gcm, "-", "_")), "/", perListMod[p], "/", varMod, "_", mthMod, ".tif")
          writeRaster(wclStk + mthAnmStk, oDel, overwrite=T)
          
          oDelInt <- paste0(deldirint, "/ssp_", str_split(ssp, "ssp")[[1]][2], "/", 
                         tolower(str_replace_all(gcm, "-", "_")), "/", perListMod[p], "/", varMod, "_", mthMod, ".tif")
          writeRaster((wclStk + mthAnmStk)*10, oDelInt, overwrite=TRUE, format="GTiff", datatype='INT2S')
          
          cat(paste(".> Anomaly", ssp, gcm, varMod, mth, "done!\n"))
          
        }
        

      }
      
    }
    
  }
  
}