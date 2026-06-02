# Carlos Navarro 
# UNIGIS 2020A
# Purpose: Statistics of climate projections

# Load libraries
require(raster)
require(maptools)
require(rgdal)
require(dismo)

# Set params
cDir <- "D:/cenavarro/msc_gis_thesis/01_baseline/wcl_v21_2_5min"
fDir <- "D:/cenavarro/msc_gis_thesis/02_climate_change/camexca_2_5min_anom_ens"
varLs <- c("prec", "tmean", "tmax", "tmin")
oDir <- "D:/cenavarro/msc_gis_thesis/02_climate_change/evaluations/statistics"
mask <- "D:/cenavarro/msc_gis_thesis/00_admin_data/CAMEXCA_adm0.shp"
sspLs <- c("ssp_126", "ssp_245", "ssp_585")
prdLs <- c("2030s", "2050s", "2070s")
ssnLs <- c("djf", "mam", "jja", "son", "ann")

if (!file.exists(paste0(oDir))) {dir.create(paste0(oDir), recursive = TRUE)}

# Read mask
poly <- readOGR(mask) 
poly$CTR_CODE <- c(1:nrow(poly))
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
  
  ## Rasterize polygon
  cStk_crop <- crop(cStk, extent(poly))
  extent(cStk_crop) <- extent(poly)
  poly_rs <- rasterize(poly, cStk_crop[[1]], id)
  
  ## Calculate stats for current
  cStk_stat <- zonal(cStk_crop, poly_rs, mean)
  stats <- rbind(stats, cbind("current", "1971-2000", var, "mean", cStk_stat))
  
  cat(paste("\n >. Calcs stats current", var))
  
  for (ssp in sspLs){
    
    for (prd in prdLs){
      
      # Load future climate files
      fStk <- stack(paste0(fDir, "/", ssp, "/", prd, "/", var, "_", 1:12, "_avg.tif"))
      fStk_crop <- crop(fStk, extent(poly))
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
write.csv(stats, paste0(oDir, "/climate_stats_by_month.csv"), row.names=F)
 
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
  cStk_crop <- crop(cStk, extent(poly))
  extent(cStk_crop) <- extent(poly)
  poly_rs <- rasterize(poly, cStk_crop[[1]], id)
  
  ## Calculate stats for current
  cStk_stat <- zonal(cStk_crop, poly_rs, mean)
  stats <- rbind(stats, cbind("current", "1971-2000", var, "mean", cStk_stat))
  
  cat(paste("\n >. Calcs stats current", var))
  
  for (ssp in sspLs){
    
    for (prd in prdLs){
      
      # Load future climate files
      fStk <- stack(paste0(fDir, "/", ssp, "/", prd, "/", var, "_", ssnLs, "_avg.tif"))
      fStk_crop <- crop(fStk, extent(poly))
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
write.csv(stats, paste0(oDir, "/climate_stats_by_season.csv"), row.names=F)

cat("\nDone!!!")
