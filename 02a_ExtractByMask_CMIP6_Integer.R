# Author: Carlos Navarro
# UNIGIS 2022
# Purpose: Extract by mask CMIP6 data, tmean calcs, seasonal calcs

# Libraries
library(raster)
library(stringr)

## Parameters
bs_dir <- "E:/ipcc_6ar_wcl_downscaled"
# bs_dir <- "U:/GLOBAL/Climate/CL17_CCAFS-Agroclimas/Agroclimas/data/ipcc_6ar_wcl_downscaled"
# scn_list <- c("ssp_126", "ssp_245", "ssp_370", "ssp_585")
scn_list <- c("ssp_126")
per_list <- c("2030s", "2050s", "2070s", "2090s")
var_list <- c("prec", "tmax", "tmin") #bio
var_list <- c("tmax") #bio
msk <- "D:/cenavarro/msc_gis_thesis/00_admin_data/camexca_msk_2_5m_v2.tif"
res <- "2_5min"
reg <- "camexca"
ot_dir <- "D:/cenavarro/msc_gis_thesis/02_climate_change"
seasons <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)

# scn <- scn_list[1]
# per <- per_list[1]
# gcm <- gcm_list[1]
# var <- var_list[1]

mask <- raster(msk)

for (scn in scn_list){
  
  for (per in per_list){
    
    gcm_list <- list.dirs(paste0(bs_dir, "/", scn, "/", per), full.names = FALSE, recursive = FALSE)
    
    for (gcm in gcm_list){
      
      rs_dir <- paste0(bs_dir, "/", scn, "/", per, "/", gcm, "/", res)
      
      ot_dir_gcm <- paste0(ot_dir, "/", reg, "_", res, "_int/", scn, "/", str_replace(gcm, "-", "_"), "/", per)
      if (!file.exists(ot_dir_gcm)) {dir.create(ot_dir_gcm, recursive=T)}
      
      
      for (var in var_list){
        
        rs_flname <- paste0(gcm, "_", str_replace(scn, "_", ""), "_", per, "_", var, "_", str_replace(res, "in", ""), "_no_tile_tif.tif")
        
        
        if (!file.exists(paste0(ot_dir_gcm, "/", var, "_12.tif"))) {
          
          stk <- stack(paste0(rs_dir, "/", rs_flname))
          stk_crop <- crop(stk, mask)
          stk_mask <- mask(stk_crop, mask)
          
          # if (var == "tmax" || var == "tmin"){
          #   if (gcm == "cnrm_cm6_1_hr" || gcm == "ec_earth3_veg_lr" || gcm == "giss_e2_1_g" || gcm == "giss_e2_1_h" || gcm == "mpi_esm1_2_hr" || gcm == "mpi_esm1_2_lr"){
          #     stk_mask <- stk_mask / 10
          #   }
          # }
          
          
          if (var == "tmax" && gcm == "bcc_csm2_mr"){
            stk_mask <- stk_mask - 100
          }
          
          
          if (var != "prec"){
            stk_mask <- stk_mask * 10
          }
          
          
          ##Write outputfile
          for (i in 1:nlayers(stk_mask)){
            if (!file.exists(paste0(ot_dir_gcm, "/", var, "_", i, ".tif"))) {
              cat(paste0(scn, " ", gcm, " ",  per, " ", var, " ", i, " done!","\n"))
              writeRaster(stk_mask[[i]], paste0(ot_dir_gcm, "/", var, "_", i, ".tif"), overwrite=TRUE, format="GTiff", datatype='INT2S')
            }
          }
          
        }
        

      }
      
    }
    
  }
  
}


## Seasonal calcs and Tmean calcuation

for (scn in scn_list){
  
  for (per in per_list){
    
    gcm_list <- list.dirs(paste0(bs_dir, "/", scn, "/", per), full.names = FALSE, recursive = FALSE)
    
    for (gcm in gcm_list){
      
      rs_dir <- paste0(bs_dir, "/", scn, "/", per, "/", gcm, "/", res)
      
      ot_dir_gcm <- paste0(ot_dir, "/", reg, "_", res, "_int/", scn, "/", str_replace(gcm, "-", "_"), "/", per)
      if (!file.exists(ot_dir_gcm)) {dir.create(ot_dir_gcm, recursive=T)}
      
      
      for (var in var_list){
        
        # Seasonal calcs
        iAvg <- stack(paste(ot_dir_gcm,'/', var, "_", 1:12, ".tif",sep=''))
        # Loop throught seasons
        for (i in 1:length(seasons)){
          if (!file.exists(paste(ot_dir_gcm,'/', var, "_", names(seasons[i]), '.tif',sep=''))){
            cat("Calcs ", scn, per, gcm, var, names(seasons[i]), "\n")
            if (var == "prec"){
              sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){sum(x,na.rm=any(!is.na(x)))})
            } else {
              sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
            }
            writeRaster(sAvg, paste(ot_dir_gcm,'/', var, "_", names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T, datatype='INT2S')
          }
        }
        
        
        ## Tmean calculation
        
        for (mth in 1:12){
          if (!file.exists(paste(ot_dir_gcm, "/tmean_", mth,".tif", sep=""))) {
            tmin <- raster(paste0(ot_dir_gcm, "/tmin_",mth, ".tif"))
            tmax <- raster(paste0(ot_dir_gcm, "/tmax_",mth, ".tif"))
            tmean <- (tmax + tmin)/2
            writeRaster(tmean, paste0(ot_dir_gcm, "/tmean_",mth, ".tif"), format="GTiff", overwrite=T, datatype='INT2S')
          }
        }
        
        
        # Seasonal calcs Tmean
        iAvg <- stack(paste(ot_dir_gcm,'/', "tmean", "_", 1:12, ".tif",sep=''))
        
        # Loop throught seasons
        for (i in 1:length(seasons)){
          if (!file.exists(paste(ot_dir_gcm,'/', "tmean", "_", names(seasons[i]), '.tif',sep=''))){
            cat("Calcs ", "tmean", names(seasons[i]), "\n")
            sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
            writeRaster(sAvg, paste(ot_dir_gcm,'/', "tmean", "_", names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T, datatype='INT2S')
          }
        }
        
        
      }
    }
  }
}

