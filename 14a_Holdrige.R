library(raster)
library(raster)
library(sf)
library(rasterVis)
library(RColorBrewer)
library(latticeExtra)

# ============================================================
# Holrdige calcs
# ============================================================

outDir <- "Z:/1.Data/Results/climate/03_ecosistems"
bDir <- "Z:/1.Data/Results/climate/01_baseline/pan/atlas_1981-2022_30s"
downdir <- "Z:/1.Data/Results/climate/02_climate_change/pan_30s_ens"
sspList   <- c("ssp_245", "ssp_585")
perList <- c("2050s", "2070s", "2090s") 
maskFile <- "Z:/1.Data/Process/Info_Inputs_SWAT/Panama/Tonosi_La_Villa/Division_administrativa/Tonosi_la_Villa_corregimientos.shp"

dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Holdridge classification function
# ============================================================

# holdridge_class <- function(tbio, pann, pet_ratio) {
# 
#   out <- tbio
#   out[] <- NA
# 
#   # -----------------------------
#   # Tropical / premontane / montane belts by biotemperature
#   # -----------------------------
# 
#   # Bosque seco tropical
#   out[tbio >= 24 & pann >= 500 & pann < 2000 & pet_ratio >= 1 & pet_ratio < 2] <- 1
# 
#   # Bosque húmedo tropical
#   out[tbio >= 24 & pann >= 2000 & pann < 4000 & pet_ratio >= 0.5 & pet_ratio < 1] <- 2
# 
#   # Bosque muy húmedo tropical
#   out[tbio >= 24 & pann >= 4000 & pet_ratio < 0.5] <- 3
# 
#   # Bosque seco premontano
#   out[tbio >= 18 & tbio < 24 & pann >= 500 & pann < 2000 & pet_ratio >= 1 & pet_ratio < 2] <- 4
# 
#   # Bosque húmedo premontano
#   out[tbio >= 18 & tbio < 24 & pann >= 1000 & pann < 4000 & pet_ratio >= 0.5 & pet_ratio < 1] <- 5
# 
#   # Bosque muy húmedo premontano
#   out[tbio >= 18 & tbio < 24 & pann >= 2000 & pet_ratio < 0.5] <- 6
# 
#   # Bosque húmedo montano bajo
#   out[tbio >= 12 & tbio < 18 & pann >= 1000 & pann < 4000] <- 7
# 
#   # Bosque muy húmedo montano bajo
#   out[tbio >= 12 & tbio < 18 & pann >= 2000 & pet_ratio < 0.5] <- 8
# 
#   # Bosque húmedo montano
#   out[tbio >= 6 & tbio < 12 & pann >= 500 & pann < 3000] <- 9
# 
#   # Bosque muy húmedo montano
#   out[tbio >= 6 & tbio < 12 & pann >= 2000] <- 10
# 
#   # Páramo
#   out[tbio >= 3 & tbio < 6 & pann >= 500] <- 11
# 
#   # Superpáramo / zonas muy frías
#   out[tbio < 3] <- 12
# 
#   return(out)
# }

holdridge_class <- function(tbio, pann, pet_ratio) {
  
  out <- tbio
  out[] <- NA
  
  # -----------------------------
  # Tropical / premontane / montane belts by biotemperature
  # -----------------------------
  
  # 1. Tropical: tbio >= 24
  out[tbio >= 24 & pann < 500] <- 13                              # Tropical muy seco / árido
  out[tbio >= 24 & pann >= 500 & pann < 2000 & pet_ratio >= 2] <- 13
  out[tbio >= 24 & pann >= 500 & pann < 2000 & pet_ratio >= 1 & pet_ratio < 2] <- 1
  out[tbio >= 24 & pann >= 500 & pann < 2000 & pet_ratio < 1] <- 2
  
  out[tbio >= 24 & pann >= 2000 & pann < 4000 & pet_ratio >= 1] <- 1
  out[tbio >= 24 & pann >= 2000 & pann < 4000 & pet_ratio >= 0.5 & pet_ratio < 1] <- 2
  out[tbio >= 24 & pann >= 2000 & pann < 4000 & pet_ratio < 0.5] <- 3
  
  out[tbio >= 24 & pann >= 4000 & pet_ratio >= 0.5] <- 2
  out[tbio >= 24 & pann >= 4000 & pet_ratio < 0.5] <- 3
  
  # 2. Premontano: 18 <= tbio < 24
  out[tbio >= 18 & tbio < 24 & pann < 500] <- 14                  # Premontano muy seco / árido
  out[tbio >= 18 & tbio < 24 & pann >= 500 & pann < 1000 & pet_ratio >= 2] <- 14
  out[tbio >= 18 & tbio < 24 & pann >= 500 & pann < 2000 & pet_ratio >= 1 & pet_ratio < 2] <- 4
  out[tbio >= 18 & tbio < 24 & pann >= 500 & pann < 1000 & pet_ratio < 1] <- 5
  
  out[tbio >= 18 & tbio < 24 & pann >= 1000 & pann < 4000 & pet_ratio >= 1] <- 4
  out[tbio >= 18 & tbio < 24 & pann >= 1000 & pann < 4000 & pet_ratio >= 0.5 & pet_ratio < 1] <- 5
  out[tbio >= 18 & tbio < 24 & pann >= 1000 & pann < 4000 & pet_ratio < 0.5] <- 6
  
  out[tbio >= 18 & tbio < 24 & pann >= 4000 & pet_ratio >= 0.5] <- 5
  out[tbio >= 18 & tbio < 24 & pann >= 4000 & pet_ratio < 0.5] <- 6
  
  # 3. Montano bajo: 12 <= tbio < 18
  out[tbio >= 12 & tbio < 18 & pann < 500] <- 15                  # Montano bajo seco / muy seco
  out[tbio >= 12 & tbio < 18 & pann >= 500 & pann < 1000] <- 15
  out[tbio >= 12 & tbio < 18 & pann >= 1000 & pann < 4000] <- 7
  out[tbio >= 12 & tbio < 18 & pann >= 2000 & pet_ratio < 0.5] <- 8
  out[tbio >= 12 & tbio < 18 & pann >= 4000 & pet_ratio >= 0.5] <- 7
  out[tbio >= 12 & tbio < 18 & pann >= 4000 & pet_ratio < 0.5] <- 8
  
  # 4. Montano: 6 <= tbio < 12
  out[tbio >= 6 & tbio < 12 & pann < 500] <- 16                   # Montano seco / muy seco
  out[tbio >= 6 & tbio < 12 & pann >= 500 & pann < 3000] <- 9
  out[tbio >= 6 & tbio < 12 & pann >= 2000] <- 10
  out[tbio >= 6 & tbio < 12 & pann >= 3000] <- 10
  
  # 5. Páramo: 3 <= tbio < 6
  out[tbio >= 3 & tbio < 6 & pann < 500] <- 17                    # Páramo seco
  out[tbio >= 3 & tbio < 6 & pann >= 500] <- 11
  
  # 6. Superpáramo / zonas muy frías
  out[tbio < 3] <- 12
  
  # 7. Valores válidos que aún no entraron en ninguna regla
  out[is.na(out) & !is.na(tbio) & !is.na(pann) & !is.na(pet_ratio)] <- 99
  
  return(out)
}

# ============================================================
# Function to process one period
# ============================================================

run_holdridge <- function(climDir, suffix, ext) {
  
  cat("Processing:", suffix, "\n")
  
  tbio <- raster(file.path(climDir, paste0("tbio.", ext)))
  pet_ratio <- raster(file.path(climDir, paste0("pet_ratio.", ext)))
  pann <- raster(file.path(climDir, paste0("bio_12", ".", ext)))
  
  hz <- holdridge_class(tbio, pann, pet_ratio)
  
  writeRaster(
    hz,
    filename = file.path(outDir, paste0("holdridge_", suffix, ".tif")),
    format = "GTiff",
    overwrite = TRUE
  )

  return(hz)
}

# ============================================================
# 01 Baseline
# ============================================================

hz_bsl <- run_holdridge(
  climDir = bDir,
  suffix = "1981_2022",
  ext = "tif"
)

# ============================================================
# 02 Future
# ============================================================

for (ssp in sspList) {
  for (per in perList) {
    futDir <- file.path(downdir, ssp, per)
    run_holdridge(
      climDir = futDir,
      suffix = paste0(ssp, "_", per),
      ext = "tif"
    )
  }
}




