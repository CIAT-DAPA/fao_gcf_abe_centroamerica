# Carlos Navarro 
# UNIGIS 2020A
# Purpose: Statistics of climate projections
# Load libraries

require(raster)
require(maptools)
require(sf)
require(dismo)

# Set params
cDir <- "Z:/1.Data/Results/climate/01_baseline/gtm/wcl_v21_2_5min"
fDir <- "Z:/1.Data/Results/climate/02_climate_change/gtm_2_5min_anom_ens"
varLs <- c("prec", "tmean", "tmax", "tmin")
oDir <- "Z:/1.Data/Results/climate/02_climate_change/gtm_evaluations"
mask <- "Z:/1.Data/Results/climate/00_admin_data/gtm/cuenca_salinas_gtm_mun.shp"
mask_rs <- "Z:/1.Data/Results/climate/00_admin_data/gtm/gtm_msk_2_5m.tif"
sspLs <- c("ssp_245", "ssp_585")
prdLs <- c("2050s", "2070s")
ssnLs <- c("djf", "mam", "jja", "son", "ann")
resample <- F

if (!file.exists(paste0(oDir))) {
  dir.create(paste0(oDir), recursive = TRUE)
}

# Read mask
poly <- sf::st_read(mask, quiet = TRUE)
poly <- as(poly, "Spatial")
poly$CTR_CODE <- 1:nrow(poly)
id <- "CTR_CODE"

#####################################
### Climate statistics by month #####
#####################################

# Open empty data frame
stats <- c()

## Looping vars
for (var in varLs){
  
  # Load current climate files
  cStk <- stack(paste0(cDir, "/", var, "_", 1:12, ".tif"))
  poly <- spTransform(poly, crs(cStk))
  
  ## Rasterize polygon
  cStk_crop <- crop(cStk, extent(poly))
  if(resample == T){cStk_crop <- resample(cStk_crop, raster(mask_rs), method="ngb")}
  extent(cStk_crop) <- extent(poly)
  poly_rs <- rasterize(poly, cStk_crop[[1]], id)
  
  ## Calculate stats for current
  cStk_stat <- zonal(cStk_crop, poly_rs, mean)
  stats <- rbind(stats, cbind("current", "1981-2022", var, "mean", cStk_stat))
  
  cat(paste("\n >. Calcs stats current", var))
  
  for (ssp in sspLs){
    
    for (prd in prdLs){
      
      # Load future climate files
      fStk <- stack(paste0(fDir, "/", ssp, "/", prd, "/", var, "_", 1:12, "_avg.tif"))
      fStk_crop <- crop(fStk, extent(poly))
      if(resample == T){fStk_crop <- resample(fStk_crop, raster(mask_rs), method="bilinear")}
      extent(fStk_crop) <- extent(poly)
      poly_rs <- rasterize(poly, fStk_crop[[1]], id)
      
      ## Calculate stats for future
      fStk_stat <- zonal(fStk_crop, poly_rs, mean)
      stats <- rbind(stats, cbind(ssp, prd, var, "mean", fStk_stat))
      
      # Load future climate files (STD)
      fStk <- stack(paste0(fDir, "/", ssp, "/", prd, "/", var, "_", 1:12, "_std.tif"))
      fStk_crop <- crop(fStk, extent(poly))
      extent(fStk_crop) <- extent(poly)
      poly_rs <- rasterize(poly, fStk_crop[[1]], id)
      
      ## Calculate stats for future (STD)
      fStk_stat <- zonal(fStk_crop, poly_rs, mean)
      stats <- rbind(stats, cbind(ssp, prd, var, "std", fStk_stat))

      # Load future climate files (Q25)
      fStk <- stack(paste0(fDir, "/", ssp, "/", prd, "/", var, "_", 1:12, "_q25.tif"))
      fStk_crop <- crop(fStk, extent(poly))
      extent(fStk_crop) <- extent(poly)
      poly_rs <- rasterize(poly, fStk_crop[[1]], id)
      
      ## Calculate stats for future (Q25)
      fStk_stat <- zonal(fStk_crop, poly_rs, mean)
      stats <- rbind(stats, cbind(ssp, prd, var, "q25", fStk_stat))
      
      # Load future climate files (Q75)
      fStk <- stack(paste0(fDir, "/", ssp, "/", prd, "/", var, "_", 1:12, "_q75.tif"))
      fStk_crop <- crop(fStk, extent(poly))
      extent(fStk_crop) <- extent(poly)
      poly_rs <- rasterize(poly, fStk_crop[[1]], id)
      
      ## Calculate stats for future (Q75)
      fStk_stat <- zonal(fStk_crop, poly_rs, mean)
      stats <- rbind(stats, cbind(ssp, prd, var, "q75", fStk_stat))
      
      cat(paste("\n >. Calcs stats", ssp, prd, var))
      
    }
    
  }
  
}

## Set colnames stat table
colnames(stats) <- c("ssp", "Period", "Variable", "Stat", "Zone", 1:12)

# Write the outputs
write.csv(stats, paste0(oDir, "/climate_stats_by_month_v2.csv"), row.names=F)
 
cat("\nDone!!!")





#####################################
### Climate statistics by season ####
#####################################

# Open empty data frame
stats <- c()

## Looping vars
for (var in varLs){
  
  cat(paste("\nCalcs stats", var))
  
  # Load current climate files
  cStk <- stack(paste0(cDir, "/", var, "_", ssnLs, ".tif"))
  
  ## Rasterize polygon
  poly <- spTransform(poly, crs(cStk))
  cStk_crop <- crop(cStk, extent(poly))
  if(resample == T){cStk_crop <- resample(cStk_crop, raster(mask_rs), method="ngb")}
  extent(cStk_crop) <- extent(poly)
  poly_rs <- rasterize(poly, cStk_crop[[1]], id)
  
  ## Calculate stats for current
  cStk_stat <- zonal(cStk_crop, poly_rs, mean)
  stats <- rbind(stats, cbind("current", "1981-2022", var, "mean", cStk_stat))
  
  cat(paste("\n >. Calcs stats current", var))
  
  for (ssp in sspLs){
    
    for (prd in prdLs){
      
      # Load future climate files
      fStk <- stack(paste0(fDir, "/", ssp, "/", prd, "/", var, "_", ssnLs, "_avg.tif"))
      fStk_crop <- crop(fStk, extent(poly))
      if(resample == T){fStk_crop <- resample(fStk_crop, raster(mask_rs), method="bilinear")}
      extent(fStk_crop) <- extent(poly)
      poly_rs <- rasterize(poly, fStk_crop[[1]], id)
      
      ## Calculate stats for future
      fStk_stat <- zonal(fStk_crop, poly_rs, mean)
      stats <- rbind(stats, cbind(ssp, prd, var, "mean", fStk_stat))
      
      # Load future climate files (STD)
      fStk <- stack(paste0(fDir, "/", ssp, "/", prd, "/", var, "_", ssnLs, "_std.tif"))
      fStk_crop <- crop(fStk, extent(poly))
      extent(fStk_crop) <- extent(poly)
      poly_rs <- rasterize(poly, fStk_crop[[1]], id)
      
      ## Calculate stats for future (STD)
      fStk_stat <- zonal(fStk_crop, poly_rs, mean)
      stats <- rbind(stats, cbind(ssp, prd, var, "std", fStk_stat))
      
      # Load future climate files (Q25)
      fStk <- stack(paste0(fDir, "/", ssp, "/", prd, "/", var, "_", ssnLs, "_q25.tif"))
      fStk_crop <- crop(fStk, extent(poly))
      extent(fStk_crop) <- extent(poly)
      poly_rs <- rasterize(poly, fStk_crop[[1]], id)
      
      ## Calculate stats for future (Q25)
      fStk_stat <- zonal(fStk_crop, poly_rs, mean)
      stats <- rbind(stats, cbind(ssp, prd, var, "q25", fStk_stat))
      
      # Load future climate files (Q75)
      fStk <- stack(paste0(fDir, "/", ssp, "/", prd, "/", var, "_", ssnLs, "_q75.tif"))
      fStk_crop <- crop(fStk, extent(poly))
      extent(fStk_crop) <- extent(poly)
      poly_rs <- rasterize(poly, fStk_crop[[1]], id)
      
      ## Calculate stats for future (Q75)
      fStk_stat <- zonal(fStk_crop, poly_rs, mean)
      stats <- rbind(stats, cbind(ssp, prd, var, "q75", fStk_stat))
      
      cat(paste("\n >. Calcs stats", ssp, prd, var))
      
    }
    
  }
  
}

## Set colnames stat table
colnames(stats) <- c("ssp", "Period", "Variable", "Stat", "Zone", ssnLs)

# Write the outputs
write.csv(stats, paste0(oDir, "/climate_stats_by_season_v2.csv"), row.names=F)

cat("\nDone!!!")

