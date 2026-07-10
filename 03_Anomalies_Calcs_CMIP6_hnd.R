# Author: Carlos Navarro
# UNIGIS 2022
# Purpose: Anomalies calcs of CMIP6 GCM data

# Load libraries
require(raster)
#require(ncdf)
#require(maptools)
require(rgdal)
library(terra)
library(sf)

## Parameters
dDir <- "Z:/1.Data/Results/climate/02_climate_change/hnd_watershed_30s"
bDir <- "Z:/1.Data/Results/climate/01_baseline/hnd/average_v2"
scn_list <- c("ssp_126", "ssp_245", "ssp_370", "ssp_585")
perList <- c("2030s", "2050s", "2070s") #, "2090s")
varList <- c("prec", "tmax", "tmin", "tmean")
seasons <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)
#msk <- as(vect("Z:/1.Data/Results/climate/00_admin_data/gadm41_HND_0.shp"), "Spatial")
mask <- raster("Z:/1.Data/Results/climate/00_admin_data/hnd/hnd_watersheed_msk_30s.tif")

scn_list <- c("ssp_370")


for (scn in scn_list){
  
  gcmList <- list.dirs(paste0(dDir, "/", scn), recursive = FALSE, full.names = FALSE)
  
  gcmList <- gcmList[gcmList != "awi_cm_1_1_mr"] # este gcm esta incompleto
  
  for (gcm in gcmList){
    
    for (period in perList) {
      
      cat("Anomalies calcs over: ", scn, gcm, period, "\n")
      
      oDir <- paste0(gsub("2_5min", "30s", dDir), "_anom_v2/", scn, "/", gcm, "/", period)
      if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
      
      for (var in varList){
        
        for (mth in 1:12){
          
          if (!file.exists(paste(oDir, "/", var, "_", mth,".tif", sep=""))) {
            
            bsl <- raster(paste0(bDir, "/", var, "_",mth, ".asc"))
            del <- raster(paste0(dDir, "/", scn, "/", gcm, "/", period, "/", var, "_", mth, ".tif"))
            
            bsl <- mask(crop(bsl, mask), mask)
            del <- resample(del, mask, method = "bilinear")
            
            if (var == "prec"){
              
              # # anom_classic <- del/bsl - 1
              
              #modifica las zonas donde la precipitación climatológica mensual es inferior a 5 mm, evitando anomalías numéricamente inestables
              denom <- bsl
              denom[denom < 5] <- 5 # piso mínimo de 5 mm
              anom <- (del - bsl) / denom
              
              # eps <- 0.01 # mm
              # anom <- del / (bsl + eps) - 1
              anom[anom > 1] <- 1     # máximo +100%
              anom[anom < -1] <- -1   # mínimo -100%
              
            } else {
              anom <- del - bsl
            }
            
            writeRaster(anom, paste(oDir, "/", var, "_", mth, '.tif',sep=''), format="GTiff", overwrite=T)
            
          }
          
        }
        
        
        # Loop throught seasons
        for (i in 1:length(seasons)){
          
          if (!file.exists(paste(bDir,'/', var, "_", names(seasons[i]), '.asc',sep=''))){
            cat("Calcs ", var, names(seasons[i]), "\n")
            
            # Load averages files 
            iAvg <- stack(paste(bDir,'/', var, "_", 1:12, ".asc",sep=''))
            
            if (var == "prec"){
              sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){sum(x,na.rm=any(!is.na(x)))})
            } else {
              sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
            }
            writeRaster(sAvg, paste(bDir,'/', var, "_", names(seasons[i]), '.asc',sep=''), overwrite=T)
          }
          
          
          if (!file.exists(paste(oDir,'/', var, "_", names(seasons[i]), '.tif',sep=''))){
            
            cat("Calcs ", var, names(seasons[i]), "\n")
            
            bsl <- mask(crop(raster(paste0(bDir, "/", var, "_", names(seasons[i]), ".asc")), mask), mask)
            del <- raster(paste0(dDir, "/", scn, "/", gcm, "/", period, "/", var, "_",names(seasons[i]), ".tif"))
            
            if (var == "prec"){
              # anom <- del/bsl - 1
              
              #modifica las zonas donde la precipitación climatológica mensual es inferior a 5 mm, evitando anomalías numéricamente inestables
              denom <- bsl
              denom[denom < 20] <- 20 # piso mínimo de 5 mm
              anom <- (del - bsl) / denom
              
              # eps <- 0.01 # mm
              # anom <- del / (bsl + eps) - 1
              anom[anom > 1] <- 1     # máximo +100%
              anom[anom < -1] <- -1   # mínimo -100%
              
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
  
  
  