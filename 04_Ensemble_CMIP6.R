# Author: Carlos Navarro
# UNIGIS 2022
# Purpose: Ensemble and seasonal calcs of CMIP6 GCM data

# Load libraries
require(raster)
#require(ncdf)
#require(maptools)
#require(rgdal)

## Parameters
dDir <- "Z:/1.Data/Results/climate/02_climate_change/hnd_watershed_30s"
oDir <- "Z:/1.Data/Results/climate/02_climate_change/hnd_watershed_30s_ens"
scn_list <- c("ssp_126", "ssp_245", "ssp_370", "ssp_585")
perList <- c("2030s", "2050s", "2070s") #, "2090s")
varList <- c("prec", "tmax", "tmin", "tmean")
seasons <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)

dir.create(oDir, recursive = TRUE, showWarnings = FALSE)

scn_list <- "ssp_585"

for (scn in scn_list){
  
  cat("Ensemble over: ", scn, "\n")
  
  gcmList <- list.dirs(paste0(dDir, "/", scn), recursive = FALSE, full.names = FALSE)
  # gcmList <- gcmList[!grepl("bcc_csm2_mr", gcmList)]
  
  gcmList <- gcmList[gcmList != "awi_cm_1_1_mr"] # este GCM no esta completo
  
  for (var in varList){
    
    setwd(paste(dDir, "/", scn, sep=""))
    
    for (period in perList) {
      
      oDirEns <- paste0(oDir, "/", scn, "/", period)
      if (!file.exists(oDirEns)) {dir.create(oDirEns, recursive=T)}
      
      for (mth in 1:12){
        
        if (!file.exists(paste(oDirEns, "/", var, "_", mth, "_q75.tif", sep=""))) {
          
          gcmStack <- stack(paste0(gcmList, "/", period, "/", var, "_",mth, ".tif"))
          
          gcmMean <- mean(gcmStack, na.rm=TRUE)
          gcmStd <- calc(gcmStack, fun = function(x) { sd(x) })
          gcmQ25 <- calc(gcmStack, fun = function(x) {quantile(x,probs = c(.25,.75),na.rm=TRUE)} )
          
          writeRaster(gcmMean, paste(oDirEns, "/", var, "_", mth, '_avg.tif',sep=''), format="GTiff", overwrite=T)
          writeRaster(gcmStd, paste(oDirEns, "/", var, "_", mth, '_std.tif', sep=""), format="GTiff", overwrite=T)
          writeRaster(gcmQ25[[1]], paste(oDirEns, "/", var, "_", mth, '_q25.tif',sep=''), format="GTiff", overwrite=T)
          writeRaster(gcmQ25[[2]], paste(oDirEns, "/", var, "_", mth, '_q75.tif',sep=''), format="GTiff", overwrite=T)
          
        }
      }
      
      # Loop throught seasons
      for (i in 1:length(seasons)){

        if (!file.exists(paste(oDirEns, "/", var, "_", names(seasons[i]), "_q75.tif", sep=""))) {


          gcmStack <- stack(paste0(dDir, "/", scn, "/", gcmList, "/", period, "/", var, "_", names(seasons[i]), ".tif"))

          gcmMean <- mean(gcmStack, na.rm=TRUE)
          gcmStd <- calc(gcmStack, fun = function(x) { sd(x) })
          gcmQ25 <- calc(gcmStack, fun = function(x) {quantile(x,probs = c(.25,.75),na.rm=TRUE)} )

          writeRaster(gcmMean, paste(oDirEns, "/", var, "_", names(seasons[i]), '_avg.tif',sep=''), format="GTiff", overwrite=T)
          writeRaster(gcmStd, paste(oDirEns, "/", var, "_", names(seasons[i]), '_std.tif', sep=""), format="GTiff", overwrite=T)
          writeRaster(gcmQ25[[1]], paste(oDirEns, "/", var, "_", names(seasons[i]), '_q25.tif',sep=''), format="GTiff", overwrite=T)
          writeRaster(gcmQ25[[2]], paste(oDirEns, "/", var, "_", names(seasons[i]), '_q75.tif',sep=''), format="GTiff", overwrite=T)

        }

      }
      
      
    }
    
    
  }
  
}

