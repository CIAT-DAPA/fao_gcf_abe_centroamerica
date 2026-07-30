# ---------------------------------------------------------
# Author: Carlos Navarro/Mario Chavez
# Purpose: Extract by mask WorldClim grids
# R version
# ---------------------------------------------------------

# Libraries
require(raster)

# ---------------------------------------------------------
# PARAMETERS
# ---------------------------------------------------------

dirbase <- "Y:/observed/gridded_products/worldclim/Global_2_5min_v2_1"

dirout <- "X:/1.Data/Results/climate/01_baseline/hnd/wcl_v21_2_5min"

msk <- "X:/1.Data/Results/climate/00_admin_data/hnd_msk_2_5m.tif"

wildcard <- "ALL"

# ---------------------------------------------------------
# HEADER
# ---------------------------------------------------------

cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("     EXTRACT BY MASK\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~\n\n")

# ---------------------------------------------------------
# CREATE OUTPUT DIRECTORY
# ---------------------------------------------------------

if (!file.exists(dirout)){
  dir.create(dirout, recursive = TRUE)
}

# ---------------------------------------------------------
# LOAD MASK
# ---------------------------------------------------------

mask_r <- raster(msk)

# ---------------------------------------------------------
# LIST RASTERS
# ---------------------------------------------------------

cat("..listing grids into", dirbase, "\n")

if (toupper(wildcard) == "ALL") {
  
  rasters <- sort(
    list.files(
      dirbase,
      pattern = "^wc2\\.1.*\\.tif$",
      full.names = TRUE
    )
  )
  
} else {
  
  rasters <- sort(
    list.files(
      dirbase,
      pattern = paste0("^", wildcard, ".*\\.tif$"),
      full.names = TRUE
    )
  )
  
}

# ---------------------------------------------------------
# PROCESS RASTERS
# ---------------------------------------------------------

for (r in rasters){
  
  filename <- basename(r)
  
  parts <- strsplit(filename, "_")[[1]]
  
  # Example:
  # wc2.1_2.5m_tavg_01.tif
  
  var <- parts[3]
  
  if (var == "tavg"){
    varmod <- "tmean"
  } else {
    varmod <- var
  }
  
  mon <- as.integer(gsub(".tif", "", parts[4], fixed = TRUE))
  
  outname <- paste0(varmod, "_", mon, ".tif")
  
  outraster <- file.path(dirout, outname)
  
  # -------------------------------------------------------
  # LOAD RASTER
  # -------------------------------------------------------
  
  rs <- raster(r)
  
  # -------------------------------------------------------
  # EXTRACT BY MASK
  # -------------------------------------------------------
  
  rs_crop <- crop(rs, mask_r)
  
  rs_mask <- mask(rs_crop, mask_r)
  
  # -------------------------------------------------------
  # SAVE
  # -------------------------------------------------------
  
  writeRaster(
    rs_mask,
    outraster,
    format = "GTiff",
    overwrite = TRUE
  )
  
  cat(filename, " extracted\n")
  
}

# ---------------------------------------------------------
# FINISH
# ---------------------------------------------------------

cat("\nProcess done!!\n")