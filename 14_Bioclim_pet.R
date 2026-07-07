### Author : Carlos Navarro c.e.navarro@cgiar.org
### Date : Jan 2016

######################
#### 01 Baseline  ####
######################
bioclim_calc <- function(bDir, ext, suffix){
  
  require(dismo)
  require(raster)
  
  # Stack by variables
  prec_stk <- stack(paste0(bDir, "/prec_", 1:12, suffix, ".", ext))
  tmin_stk <- stack(paste0(bDir, "/tmin_", 1:12, suffix, ".", ext))
  tmax_stk <- stack(paste0(bDir, "/tmax_", 1:12, suffix, ".", ext))
  
  # PET Holdridge
  tmean_stk <- (tmin_stk + tmax_stk) / 2
  
  tbio_monthly <- calc(tmean_stk, fun = function(x) {
    x[x < 0] <- 0
    x[x > 30] <- 30
    return(x)
  })
  
  tbio <- calc(tbio_monthly, mean, na.rm = TRUE)
  pann <- calc(prec_stk, sum, na.rm = TRUE)
  
  pet <- 58.93 * tbio
  pet_ratio <- pet / pann
  
  writeRaster(tbio, paste0(bDir, "/tbio.", ext),overwrite = TRUE)
  writeRaster(pet, paste0(bDir, "/pet.", ext),overwrite = TRUE)
  writeRaster(pet_ratio, paste0(bDir, "/pet_ratio.", ext), overwrite = TRUE)
  
  # Bioclim variables calculation using dismo package
  bios <- biovars(prec_stk, tmin_stk, tmax_stk)
  
  for(i in 1:19){

    cat("Writting bio", i, "\n")
    bioAsc <- writeRaster(bios[[i]], paste0(bDir, "/bio_", i, ".", "asc"))
    cat(" .. done")
  }
  
  # for(i in 1:19){
  #   cat("Writing", names(bios)[i], "\n")
  #   writeRaster(
  #     bios[[i]],
  #     filename = paste0(bDir, "/", names(bios)[i], "_.asc"),
  #     overwrite = TRUE
  #   )
  #   cat(" .. done\n")
  # }
  
}

bDir <- "Z:/1.Data/Results/climate/01_baseline/hnd/average_v2"
ext <- "asc"
suffix <- ""
otp <- bioclim_calc(bDir, ext, suffix)

####################
#### 02 Future  ####
####################
downdir <- "Z:/1.Data/Results/climate/02_climate_change/hnd_30s_ens"
sspList   <- c("ssp_245", "ssp_585")
perList <- c("2050s", "2070s") 
ext <- "tif"
suffix <- "_avg"

for (ssp in sspList) {
  for (per in perList){
    otp <- bioclim_calc(paste0(downdir, "/", ssp, "/", per), ext, suffix)
  }
}
