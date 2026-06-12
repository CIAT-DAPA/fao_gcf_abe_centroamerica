# Author: Carlos Navarro
# UNIGIS 2022
# Purpose: Anomalies calcs of CMIP6 GCM data

# Load libraries
require(raster)
#require(ncdf)
#require(maptools)
#require(rgdal)

## Parameters
dDir <- "Z:/1.Data/Results/climate/02_climate_change/pan_30s"
bDir <- "Z:/1.Data/Results/climate/01_baseline/pan/atlas_1981-2022_30s"
scn_list <- c("ssp_245","ssp_585")
# scn_list <- "ssp_585"
perList <- c("2050s", "2070s","2090s") #, "2030s")
varList <- c("prec", "tmax", "tmin", "tmean")
seasons <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)


## Monthly
for (scn in scn_list){
  
  gcmList <- list.dirs(paste0(dDir, "/", scn), recursive = FALSE, full.names = FALSE)
  
  gcmList <- gcmList[gcmList != "awi_cm_1_1_mr"] # este gcm esta incompleto
  
  for (gcm in gcmList){
    
    for (period in perList) {
      
      cat("Anomalies calcs over: ", scn, gcm, period, "\n")
      
      oDir <- paste0(dDir, "_anom/", scn, "/", gcm, "/", period)
      if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
      
      for (var in varList){
        
        for (mth in 1:12){
          
          if (!file.exists(paste(oDir, "/", var, "_", mth,".tif", sep=""))) {
            
            bsl <- raster(paste0(bDir, "/", var, "_",mth, ".tif"))
            del <- raster(paste0(dDir, "/", scn, "/", gcm, "/", period, "/", var, "_", mth, ".tif"))
            
            if (var == "prec"){
              anom <- del/bsl - 1
            } else {
              anom <- del - bsl
            }
            
            writeRaster(anom, paste(oDir, "/", var, "_", mth, '.tif',sep=''), format="GTiff", overwrite=T)
            
          }
          
        }
        
      }
      
    }
    
  }
  
}

for (var in varList){  
  
  # Loop throught seasons
  for (i in 1:length(seasons)){
    
    #baseline 
    if (!file.exists(paste(bDir,'/', var, "_", names(seasons[i]), '.tif',sep=''))){
      
      cat("Seasonal calcs baseline \n")
      
      # Load averages files 
      iAvg <- stack(paste(bDir,'/', var, "_", 1:12, ".tif",sep=''))
      
      if (var == "prec"){
        sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){sum(x,na.rm=any(!is.na(x)))})
      } else {
        sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
      }
      writeRaster(sAvg, paste(bDir,'/', var, "_", names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T)
    }
  }
}


#Seasonal calcs future and anomalies
for (scn in scn_list){
  
  gcmList <- list.dirs(paste0(dDir, "/", scn), recursive = FALSE, full.names = FALSE)
  
  gcmList <- gcmList[gcmList != "awi_cm_1_1_mr"] # este gcm esta incompleto
  
  for (gcm in gcmList){
    
    for (period in perList) {
      
      oDir <- paste0(dDir, "_anom/", scn, "/", gcm, "/", period)
      if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
      
      for (var in varList){  
        
        # Future 
        cat("Seasonal calcs future: ", scn, gcm, period, "\n")
        
        ot_dir_gcm <- paste0(dDir, "/", scn, "/", gcm, "/", period)
        iAvg <- stack(paste(ot_dir_gcm,'/', var, "_", 1:12, ".tif",sep=''))
        for (i in 1:length(seasons)){
          if (!file.exists(paste(ot_dir_gcm,'/', var, "_", names(seasons[i]), '.tif',sep=''))){
            cat("Calcs ", scn, per, gcm, var, names(seasons[i]), "\n")
            if (var == "prec"){
              sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){sum(x,na.rm=any(!is.na(x)))})
            } else {
              sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
            }
            writeRaster(sAvg, paste(ot_dir_gcm,'/', var, "_", names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T)
          }
        }
        
        
        #Seasons anomalies
        cat("Seasonal calcs anomalies: ", scn, gcm, period, "\n")
        
        # Loop throught seasons
        for (i in 1:length(seasons)){
          
          if (!file.exists(paste(oDir,'/', var, "_", names(seasons[i]), '.tif',sep=''))){
            
            cat("Calcs ", var, names(seasons[i]), "\n")
            
            bsl <- raster(paste0(bDir, "/", var, "_", names(seasons[i]), ".tif"))
            del <- raster(paste0(dDir, "/", scn, "/", gcm, "/", period, "/", var, "_",names(seasons[i]), ".tif"))
            
            if (var == "prec"){
              anom <- del/bsl - 1
            } else {
              anom <- del - bsl
            }
            
            writeRaster(anom, paste(oDir,'/', var, "_", names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T)
            
          }
          
        } 
        
        
      }
      
    }
    
  }
  
}

