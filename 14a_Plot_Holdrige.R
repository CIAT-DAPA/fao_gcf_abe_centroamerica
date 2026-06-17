library(raster)
library(sf)
library(sp)
library(rasterVis)
library(RColorBrewer)
library(latticeExtra)

# ============================================================
# Paths
# ============================================================

ecoDir <- "Z:/1.Data/Results/climate/03_ecosistems"
figDir <- file.path(ecoDir, "_plot")
dir.create(figDir, recursive = TRUE, showWarnings = FALSE)

maskFile <- "Z:/1.Data/Process/Info_Inputs_SWAT/Panama/Tonosi_La_Villa/Division_administrativa/Tonosi_la_Villa_corregimientos.shp"

sspList <- c("ssp_245", "ssp_585")
perList <- c("2050s", "2070s")

# ============================================================
# Mask: corregimientos + límite de cuenca
# ============================================================

mask_correg_sf <- sf::st_read(maskFile, quiet = TRUE)
mask_cuenca_sf <- sf::st_union(mask_correg_sf)

mask_correg <- as(mask_correg_sf, "Spatial")
mask_cuenca <- as(sf::st_as_sf(mask_cuenca_sf), "Spatial")

# ============================================================
# Leyenda Holdridge
# ============================================================

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
zvalues <- seq(0.5, 12.5, 1)

# ============================================================
# Theme
# ============================================================

myTheme <- rasterTheme(region = hold_cols)
myTheme$strip.border$col <- "white"
myTheme$axis.line$col <- "white"
myTheme$background$col <- "white"

# ============================================================
# Function crop + mask + reprojection of polygons
# ============================================================

prep_map <- function(r) {
  
  shp_correg <- mask_correg
  shp_cuenca <- mask_cuenca
  
  if (!compareCRS(r, shp_correg)) {
    shp_correg <- spTransform(shp_correg, crs(r))
    shp_cuenca <- spTransform(shp_cuenca, crs(r))
  }
  
  r <- crop(r, shp_correg)
  r <- mask(r, shp_correg)
  
  list(
    raster = r,
    correg = shp_correg,
    cuenca = shp_cuenca
  )
}

# ============================================================
# 1. Holdridge actual
# ============================================================

hz_base_raw <- raster(file.path(ecoDir, "holdridge_1981_2022.tif"))
mp_base <- prep_map(hz_base_raw)
hz_base <- mp_base$raster

oPlot <- file.path(figDir, "map_holdridge_actual_tlv.tif")

tiff(
  oPlot,
  width = 1200,
  height = 800,
  pointsize = 8,
  compression = "lzw",
  res = 200
)

print(
  levelplot(
    hz_base,
    at = zvalues,
    scales = list(draw = FALSE),
    xlab = "",
    ylab = "",
    margin = FALSE,
    par.settings = myTheme,
    colorkey = list(
      space = "right",
      width = 1.2,
      height = 1,
      labels = list(
        at = 1:12,
        labels = holdridge_classes,
        cex = 0.55
      )
    )
  ) +
    layer(sp.polygons(mp_base$correg, lwd = 0.35, col = "gray30")) +
    layer(sp.polygons(mp_base$cuenca, lwd = 1.2, col = "black"))
)

dev.off()

# ============================================================
# 2. Holdridge futuro
# ============================================================

future_list <- list()
future_names <- c()

for (per in perList) {
  
  for (ssp in sspList) {
    
    f <- file.path(ecoDir, paste0("holdridge_", ssp, "_", per, ".tif"))
    
    if (!file.exists(f)) {
      warning("No existe: ", f)
      next
    }
    
    r <- raster(f)
    mp <- prep_map(r)
    
    future_list[[paste0(ssp, "_", per)]] <- mp$raster
    future_names <- c(future_names, paste0(toupper(ssp), " - ", per))
  }
}

future_stack <- stack(future_list)
names(future_stack) <- future_names

# Usar polígonos reproyectados al CRS del primer mapa futuro
mp_future_ref <- prep_map(raster(file.path(ecoDir, paste0("holdridge_", sspList[1], "_", perList[1], ".tif"))))

oPlot <- file.path(figDir, "map_holdridge_futuro_tlv.tif")

tiff(
  oPlot,
  width = 1600,
  height = 1200,
  pointsize = 8,
  compression = "lzw",
  res = 200
)

print(
  levelplot(
    future_stack,
    at = zvalues,
    scales = list(draw = FALSE),
    layout = c(2, 2),
    xlab = "",
    ylab = "",
    margin = FALSE,
    par.settings = myTheme,
    names.attr = future_names,
    colorkey = list(
      space = "right",
      width = 1.2,
      height = 1,
      labels = list(
        at = 1:12,
        labels = holdridge_classes,
        cex = 0.55
      )
    )
  ) +
    layer(sp.polygons(mp_future_ref$correg, lwd = 0.35, col = "gray30")) +
    layer(sp.polygons(mp_future_ref$cuenca, lwd = 1.2, col = "black"))
)

dev.off()

# ============================================================
# 3. Cambio Holdridge: estable vs cambio
# ============================================================

change_list <- list()
change_names <- c()

for (per in perList) {
  
  for (ssp in sspList) {
    
    f <- file.path(ecoDir, paste0("holdridge_", ssp, "_", per, ".tif"))
    
    if (!file.exists(f)) {
      warning("No existe: ", f)
      next
    }
    
    fut <- raster(f)
    mp_fut <- prep_map(fut)
    fut_clip <- mp_fut$raster
    
    chg <- fut_clip
    chg[] <- NA
    
    chg[!is.na(hz_base[]) & !is.na(fut_clip[]) & hz_base[] == fut_clip[]] <- 0
    chg[!is.na(hz_base[]) & !is.na(fut_clip[]) & hz_base[] != fut_clip[]] <- 1
    
    change_list[[paste0(ssp, "_", per)]] <- chg
    change_names <- c(change_names, paste0(toupper(ssp), " - ", per))
  }
}

change_stack <- stack(change_list)
names(change_stack) <- change_names

changeTheme <- rasterTheme(region = c("gray80", "red3"))
changeTheme$strip.border$col <- "white"
changeTheme$axis.line$col <- "white"
changeTheme$background$col <- "white"

oPlot <- file.path(figDir, "map_holdridge_cambio_tlv.tif")

tiff(
  oPlot,
  width = 1600,
  height = 1200,
  pointsize = 8,
  compression = "lzw",
  res = 200
)

print(
  levelplot(
    change_stack,
    at = c(-0.5, 0.5, 1.5),
    scales = list(draw = FALSE),
    layout = c(2, 2),
    xlab = "",
    ylab = "",
    margin = FALSE,
    par.settings = changeTheme,
    names.attr = change_names,
    colorkey = list(
      space = "right",
      width = 1.2,
      height = 1,
      labels = list(
        at = c(0, 1),
        labels = c("Sin cambio", "Cambio"),
        cex = 0.8
      )
    )
  ) +
    layer(sp.polygons(mp_future_ref$correg, lwd = 0.35, col = "gray30")) +
    layer(sp.polygons(mp_future_ref$cuenca, lwd = 1.2, col = "black"))
)

dev.off()

