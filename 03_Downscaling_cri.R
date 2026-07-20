# Carlos Navarro 
# CIAT
# May 2026

require(raster)
require(maptools)
require(latticeExtra)
library(terra)
library(sf)

bslDir  <- "Z:/1.Data/Results/climate/01_baseline/cri/wcl_v21_30s"
anomDir <- "C:/Users/cenavarro/Workspace/msc_gis_thesis/02_climate_change/camexca_2_5min_anom_ens"
oDir <- "Z:/1.Data/Results/climate/02_climate_change/cri_2_5min_ens" 
crs_ref <- crs(raster("Z:/1.Data/Results/climate/02_climate_change/cri_2_5min_anom_ens/prec_ssp_126_season_avg.tif"))
mask_adm1 <- as(project(vect("Z:/1.Data/Process/Info_Inputs_SWAT/Costa_Rica/Tempisque/Division_Administativa/cuenca_distritos_wgs84.shp"), crs_ref), "Spatial")
varList <- c("prec", "tmin", "tmax", "tmean")
sspList <- c("ssp_245", "ssp_585")
perList <- c("2050s", "2070s") #, "2090s") #,"2030s", )

for (ssp in sspList) {
  
  for (period in perList) {
    
    oDirSspPer <- paste0(oDir, "/", ssp, "/", period)
    if (!file.exists(oDirSspPer)) {dir.create(oDirSspPer, recursive=T)}
    
    for (var in varList){
      
      cat("Donwscaling process over ", ssp, " ", period, " ", var, "\n")
      
      for(j in 1:12){
        
        oTif <- paste0(oDirSspPer, "/", var, "_",j, ".tif")
        
        if (!file.exists(oTif)){
          
          if (!file.exists(paste0(oDirSspPer, "/", var, "_", j, ".tif"))){
            
            ##carga las anomalias de cada gcm segun ssp especificado
            anom <- raster(paste0(anomDir, "/", ssp, "/", period, "/", var, "_", j, "_avg.tif"))
            
            ###carga datos de promedios generados de estaciones
            bsl <- raster(paste0(bslDir, "/", var, "_", j, ".tif"))
            
            bsl <- mask(crop(bsl, extent(mask_adm1)), mask_adm1)
            anom <- resample(anom, bsl)
            anom <- mask(crop(anom, extent(mask_adm1)), mask_adm1)

            if (var == "prec"){
              del <- bsl * abs(1 + anom)
            } else {
              del <- bsl + anom
            }
            
            writeRaster(del, paste0(oDirSspPer, "/", var, "_", j, ".tif"), overwrite=TRUE)
            
          } 
          
          
          # if (var == "prec"){
          #   del_msk <- raster(paste0(oDir, "/", var, "_", j, ".asc"))
          # } else {
          #   del_msk <- raster(paste0(oDir, "/", var, "_", j, ".asc"))* 10
          # }
          # 
          # del_msk <- crop(del_msk, extent(mask))
          # del_msk <- mask(del_msk, mask)
          # writeRaster(del_msk, oTif, format="GTiff", overwrite=F, datatype='INT2S')
          
        }
        
      }
      
    }
    
  }  
}
