library(raster)

# ============================================================
# Directories
# ============================================================

outDir <- "Y:/1.Data/RAW/Ecosystems"
dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Holdridge classification function
# ============================================================

holdridge_class <- function(tbio, pann, pet_ratio) {
  
  out <- tbio
  out[] <- NA
  
  # -----------------------------
  # Tropical / premontane / montane belts by biotemperature
  # -----------------------------
  
  # Bosque seco tropical
  out[tbio >= 24 & pann >= 500 & pann < 2000 & pet_ratio >= 1 & pet_ratio < 2] <- 1
  
  # Bosque húmedo tropical
  out[tbio >= 24 & pann >= 2000 & pann < 4000 & pet_ratio >= 0.5 & pet_ratio < 1] <- 2
  
  # Bosque muy húmedo tropical
  out[tbio >= 24 & pann >= 4000 & pet_ratio < 0.5] <- 3
  
  # Bosque seco premontano
  out[tbio >= 18 & tbio < 24 & pann >= 500 & pann < 2000 & pet_ratio >= 1 & pet_ratio < 2] <- 4
  
  # Bosque húmedo premontano
  out[tbio >= 18 & tbio < 24 & pann >= 1000 & pann < 4000 & pet_ratio >= 0.5 & pet_ratio < 1] <- 5
  
  # Bosque muy húmedo premontano
  out[tbio >= 18 & tbio < 24 & pann >= 2000 & pet_ratio < 0.5] <- 6
  
  # Bosque húmedo montano bajo
  out[tbio >= 12 & tbio < 18 & pann >= 1000 & pann < 4000] <- 7
  
  # Bosque muy húmedo montano bajo
  out[tbio >= 12 & tbio < 18 & pann >= 2000 & pet_ratio < 0.5] <- 8
  
  # Bosque húmedo montano
  out[tbio >= 6 & tbio < 12 & pann >= 500 & pann < 3000] <- 9
  
  # Bosque muy húmedo montano
  out[tbio >= 6 & tbio < 12 & pann >= 2000] <- 10
  
  # Páramo
  out[tbio >= 3 & tbio < 6 & pann >= 500] <- 11
  
  # Superpáramo / zonas muy frías
  out[tbio < 3] <- 12
  
  return(out)
}

# ============================================================
# Function to process one period
# ============================================================

run_holdridge <- function(climDir, outName, ext = "tif") {
  
  cat("Processing:", outName, "\n")
  
  tbio <- raster(file.path(climDir, paste0("tbio.", ext)))
  pet_ratio <- raster(file.path(climDir, paste0("pet_ratio.", ext)))
  
  # precipitation annual
  if (grepl("1981_2010", outName)) {
    prec_files <- file.path(climDir, paste0("prec_1981_2010_", 1:12, ".", ext))
  } else {
    prec_files <- file.path(climDir, paste0("prec_", 1:12, ".", ext))
  }
  
  prec_stk <- stack(prec_files)
  pann <- calc(prec_stk, sum, na.rm = TRUE)
  
  hz <- holdridge_class(tbio, pann, pet_ratio)
  
  writeRaster(
    hz,
    filename = file.path(outDir, paste0("holdridge_", outName, ".tif")),
    format = "GTiff",
    overwrite = TRUE
  )
  
  writeRaster(
    hz,
    filename = file.path(outDir, paste0("holdridge_", outName, ".asc")),
    format = "ascii",
    overwrite = TRUE
  )
  
  return(hz)
}

# ============================================================
# 01 Baseline
# ============================================================

bslDir <- "Y:/1.Data/RAW/Clima/Climatologia/outputs/average"

hz_bsl <- run_holdridge(
  climDir = bslDir,
  outName = "1981_2010",
  ext = "asc"
)

# ============================================================
# 02 Future
# ============================================================

futBaseDir <- "Y:/1.Data/Results/Cambio_Climatico/huila_30s_downscaling_ens_sq"

sspList <- c("ssp126", "ssp245", "ssp585")
perList <- c("2030s", "2050s")

for (ssp in sspList) {
  for (per in perList) {
    
    futDir <- file.path(futBaseDir, ssp, per)
    
    run_holdridge(
      climDir = futDir,
      outName = paste0(ssp, "_", per),
      ext = "tif"
    )
  }
}






library(raster)
library(rgdal)
library(rasterVis)
library(RColorBrewer)

# -----------------------------
# Paths
# -----------------------------
ecoDir <- "Y:/1.Data/RAW/Ecosystems"
figDir <- file.path(ecoDir, "_plot")
dir.create(figDir, recursive = TRUE, showWarnings = FALSE)

mask_adm1 <- readOGR("Y:/1.Data/RAW/Admin/COL1_HUI.shp")
mask_adm2 <- readOGR("Y:/1.Data/RAW/Admin/COL2_HUI.shp")


sspList <- c("ssp126", "ssp245", "ssp585")
perList <- c("2030s", "2050s")

# -----------------------------
# Leyenda Holdridge
# -----------------------------
holdridge_classes <- c(
  "Bosque seco tropical",
  "Bosque húmedo tropical",
  "Bosque muy húmedo tropical",
  "Bosque seco premontano",
  "Bosque húmedo premontano",
  "Bosque muy húmedo premontano",
  "Bosque húmedo montano bajo",
  "Bosque muy húmedo montano bajo",
  "Bosque húmedo montano",
  "Bosque muy húmedo montano",
  "Páramo",
  "Superpáramo / zonas muy frías"
)

hold_cols <- colorRampPalette(brewer.pal(12, "Paired"))(12)

# -----------------------------
# Function crop + mask
# -----------------------------
clip_huila <- function(r) {
  
  shp <- mask_adm1
  
  if (!compareCRS(r, shp)) {
    shp <- spTransform(shp, CRS(projection(r)))
  }
  
  r <- crop(r, shp)
  r <- mask(r, shp)
  
  return(r)
}
# ============================================================
# 1. Holdridge actual
# ============================================================

hz_base <- raster(file.path(ecoDir, "holdridge_1981_2010.tif"))
hz_base <- clip_huila(hz_base)

png(
  file.path(figDir, "map_holdridge_actual.png"),
  width = 1800,
  height = 1600,
  res = 200
)

levelplot(
  hz_base,
  margin = FALSE,
  main = "Zonas de vida de Holdridge - Línea base 1981-2010",
  col.regions = hold_cols,
  at = seq(0.5, 12.5, 1),
  colorkey = list(
    labels = list(
      at = 1:12,
      labels = holdridge_classes,
      cex = 0.7
    )
  )
) + latticeExtra::layer(sp.polygons(mask_adm2, lwd = 0.8))


dev.off()

# ============================================================
# 2. Holdridge futuro: 6 mapas
# ============================================================

future_list <- list()
future_names <- c()

for (per in perList) {
  for (ssp in sspList) {
    
    r <- raster(file.path(ecoDir, paste0("holdridge_", ssp, "_", per, ".tif")))
    r <- clip_huila(r)
    
    future_list[[paste0(ssp, "_", per)]] <- r
    future_names <- c(future_names, paste0(toupper(ssp), " - ", per))
  }
}

future_stack <- stack(future_list)
names(future_stack) <- future_names

png(
  file.path(figDir, "map_holdridge_futuro.png"),
  width = 2400,
  height = 1800,
  res = 200
)

levelplot(
  future_stack,
  layout = c(3, 2),
  margin = FALSE,
  main = "Zonas de vida de Holdridge futuras",
  col.regions = hold_cols,
  at = seq(0.5, 12.5, 1),
  names.attr = future_names,
  colorkey = list(
    labels = list(
      at = 1:12,
      labels = holdridge_classes,
      cex = 0.65
    )
  )
) + latticeExtra::layer(sp.polygons(mask_adm2, lwd = 0.8))


dev.off()

# ============================================================
# 3. Cambio Holdridge: estable vs cambio
# ============================================================

change_list <- list()
change_names <- c()

for (per in perList) {
  for (ssp in sspList) {
    
    fut <- raster(file.path(ecoDir, paste0("holdridge_", ssp, "_", per, ".tif")))
    fut <- clip_huila(fut)
    
    # 0 = sin cambio, 1 = cambio de zona de vida
    chg <- fut
    chg[] <- NA
    chg[!is.na(hz_base[]) & !is.na(fut[]) & hz_base[] == fut[]] <- 0
    chg[!is.na(hz_base[]) & !is.na(fut[]) & hz_base[] != fut[]] <- 1
    
    change_list[[paste0(ssp, "_", per)]] <- chg
    change_names <- c(change_names, paste0(toupper(ssp), " - ", per))
  }
}

change_stack <- stack(change_list)
names(change_stack) <- change_names

png(
  file.path(figDir, "map_holdridge_cambio.png"),
  width = 2400,
  height = 1800,
  res = 200
)

levelplot(
  change_stack,
  layout = c(3, 2),
  margin = FALSE,
  main = "Cambio en zonas de vida de Holdridge respecto a 1981-2010",
  col.regions = c("gray80", "red3"),
  at = c(-0.5, 0.5, 1.5),
  names.attr = change_names,
  colorkey = list(
    labels = list(
      at = c(0, 1),
      labels = c("Sin cambio", "Cambio"),
      cex = 0.8
    )
  )
) + latticeExtra::layer(sp.polygons(mask_adm2, lwd = 0.8))


dev.off()
