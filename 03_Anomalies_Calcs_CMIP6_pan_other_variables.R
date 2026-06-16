library(terra)
library(ncdf4)

varList <- c("etp", "hurs", "rsds", "wspd")

inDir <- "Z:/1.Data/Results/climate/02_climate_change/pan_30s_anom_ens/ssp_585/2050s"
outDir <- "Z:/1.Data/Results/climate/02_climate_change/pan_30s_anom_ens/ssp_585/2050s"

dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

msk <- rast("Z:/1.Data/Results/climate/00_admin_data/pan_msk_30s.tif")

files <- c(
  etp  = file.path(inDir, "ETP_ensemble_ssp585_2041_2060.nc"),
  hurs = file.path(inDir, "hurs_ensemble_ssp585_2041_2060.nc"),
  rsds = file.path(inDir, "rsds_ensemble_ssp585_2041_2060.nc"),
  wspd = file.path(inDir, "sfcWind_ensemble_ssp585_2041_2060.nc")
)

nc_vars <- c(
  etp  = "ETP_anom",
  hurs = "hurs_anom",
  rsds = "rsds_anom",
  wspd = "sfcWind_anom"
)

for (v in varList) {
  
  message("Procesando: ", v)
  
  f <- files[[v]]
  r <- rast(f)

  months_idx <- which(n$dim$season$vals %in%
                        c("Jan","Feb","Mar","Apr","May","Jun",
                          "Jul","Aug","Sep","Oct","Nov","Dec"))

  anom50 <- r[[grep("_anom_quantile=0.5_", names(r))]]
  
  anom50_mon <- anom50[[months_idx]]
  
  # if (!compareGeom(anom50_mon, msk, stopOnError = FALSE)) {
  #   msk_use <- project(msk, anom50_mon[[1]], method = "near")
  # } else {
  #   msk_use <- msk
  # }
  
  anom50_msk <- mask(crop(anom50_mon, msk), msk)
  
  for (i in 1:12) {
    outFile <- file.path(outDir, paste0(v, "_", i, "_avg.tif"))
    
    writeRaster(
      anom50_msk[[i]],
      outFile,
      overwrite = TRUE,
      filetype = "GTiff"
    )
  }
}
