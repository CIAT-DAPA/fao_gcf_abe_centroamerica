# ---------------------------------------------------------
# Author: Carlos Navarro / Mario Chavez
# Purpose: Extract by mask baseline climate grids
# R version
# ---------------------------------------------------------

# ---------------------------------------------------------
# LIBRARIES
# ---------------------------------------------------------

require(raster)

# ---------------------------------------------------------
# PARAMETERS
# ---------------------------------------------------------

dirbase <- "X:/1.Data/Results/climate/01_baseline/hnd/average_v2"
dirout <- "X:/1.Data/Results/climate/01_baseline/hnd/average_v2_watersheed"
msk <- "X:/1.Data/Results/climate/00_admin_data/hnd/hnd_watersheed_msk_30s.tif"
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

if (!file.exists(dirout)) {
  dir.create(dirout, recursive = TRUE)
}

# ---------------------------------------------------------
# LOAD MASK
# ---------------------------------------------------------

mask_r <- raster(msk)

# ---------------------------------------------------------
# LIST INPUT RASTERS (.asc)
# ---------------------------------------------------------

cat("Listing grids in:\n", dirbase, "\n\n")

if (toupper(wildcard) == "ALL") {
  
  rasters <- sort(
    list.files(
      dirbase,
      pattern = "\\.asc$",
      full.names = TRUE
    )
  )
  
} else {
  
  rasters <- sort(
    list.files(
      dirbase,
      pattern = paste0("^", wildcard, ".*\\.asc$"),
      full.names = TRUE
    )
  )
  
}

cat(length(rasters), "grids found.\n\n")

# ---------------------------------------------------------
# PROCESS RASTERS
# ---------------------------------------------------------

for (r in rasters) {
  
  filename <- basename(r)
  
  # Same name, different extension
  outname <- sub("\\.asc$", ".tif", filename)
  
  outraster <- file.path(dirout, outname)
  
  cat("Processing:", filename, "\n")
  
  # Load raster
  rs <- raster(r)
  
  # Crop
  rs_crop <- crop(rs, mask_r)
  
  # Mask
  rs_mask <- mask(rs_crop, mask_r)
  
  # Save GeoTIFF
  writeRaster(
    rs_mask,
    filename = outraster,
    format = "GTiff",
    overwrite = TRUE
  )
  
}

# ---------------------------------------------------------
# FINISH
# ---------------------------------------------------------

cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Process completed successfully.\n")
cat("Output folder:\n", dirout, "\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~\n")