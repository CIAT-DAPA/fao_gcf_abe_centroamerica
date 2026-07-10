library(raster)
library(sf)
library(sp)
library(rasterVis)
library(RColorBrewer)
library(latticeExtra)
library(lwgeom)


# ============================================================
# Paths
# ============================================================

ecoDir <- "Z:/1.Data/Results/climate/03_ecosistems"
figDir <- file.path(ecoDir, "_plot")
dir.create(figDir, recursive = TRUE, showWarnings = FALSE)

# maskFile <- "Z:/1.Data/Process/Info_Inputs_SWAT/Honduras/Choluteca/Division_Administrativa/Choluteca_adm2.shp"
maskFile <- "Z:/1.Data/Process/Info_Inputs_SWAT/Republica_Dominicana/Division_administrativa/Guayubin_Mao_secciones.shp"


sspList <- c("ssp_245", "ssp_585")
perList <- c("2050s", "2070s")

# ============================================================
# Mask: corregimientos + límite de cuenca
# ============================================================

# mask_correg_sf <- sf::st_read(maskFile, quiet = TRUE)
# mask_cuenca_sf <- sf::st_union(mask_correg_sf)
# 
# mask_correg <- as(mask_correg_sf, "Spatial")
# mask_cuenca <- as(sf::st_as_sf(mask_cuenca_sf), "Spatial")

sf::sf_use_s2(FALSE)

mask_correg_sf <- sf::st_read(maskFile, quiet = TRUE)

mask_correg_sf <- mask_correg_sf |>
  st_make_valid() |>
  st_buffer(0)

mask_cuenca_sf <- mask_correg_sf |>
  st_union() |>
  st_make_valid() |>
  st_buffer(0)

mask_correg <- as(mask_correg_sf, "Spatial")
mask_cuenca <- as(st_as_sf(mask_cuenca_sf), "Spatial")



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
  "Superpáramo / zonas muy frías",
  "Tropical muy seco / árido",
  "Premontano muy seco / árido",
  "Montano bajo seco / muy seco",
  "Montano seco / muy seco",
  "Páramo seco",
  "No clasificado"
)

# IDs originales
hold_ids <- c(1:17, 99)

# IDs para graficar: 99 se convierte temporalmente a 18
plot_ids <- 1:18

hold_cols <- colorRampPalette(brewer.pal(12, "Paired"))(18)
hold_cols[18] <- "grey80"

zvalues <- seq(0.5, 18.5, 1)

hold_legend <- data.frame(
  ID_original = hold_ids,
  ID_plot = plot_ids,
  Clase = holdridge_classes,
  Color = hold_cols
)

# ============================================================
# Theme
# ============================================================

myTheme <- rasterTheme(region = hold_cols)
myTheme$strip.border$col <- "white"
myTheme$axis.line$col <- "white"
myTheme$background$col <- "white"

# ============================================================
# Function: recode 99 to 18 only for plotting
# ============================================================

prep_plot_holdridge <- function(r) {
  r_plot <- r
  r_plot[r_plot == 99] <- 18
  return(r_plot)
}

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

hz_base_raw <- raster(file.path(ecoDir, "holdridge_1981-2010.tif"))

mp_base <- prep_map(hz_base_raw)
hz_base <- mp_base$raster
hz_base_plot <- prep_plot_holdridge(hz_base)

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
    hz_base_plot,
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
        at = plot_ids,
        labels = holdridge_classes,
        cex = 0.55
      )
    )
  ) +
    latticeExtra::layer(sp.polygons(mp_base$correg, lwd = 0.35, col = "gray30")) +
    latticeExtra::layer(sp.polygons(mp_base$cuenca, lwd = 1.2, col = "black"))
)

dev.off()

# ============================================================
# 2. Holdridge futuro
# ============================================================

future_list <- list()
future_names <- c()
mp_future_ref <- NULL

for (per in perList) {
  
  for (ssp in sspList) {
    
    f <- file.path(ecoDir, paste0("holdridge_", ssp, "_", per, ".tif"))
    
    if (!file.exists(f)) {
      warning("No existe: ", f)
      next
    }
    
    r <- raster(f)
    mp <- prep_map(r)
    
    r_plot <- prep_plot_holdridge(mp$raster)
    
    future_list[[paste0(ssp, "_", per)]] <- r_plot
    future_names <- c(future_names, paste0(toupper(ssp), " - ", per))
    
    if (is.null(mp_future_ref)) {
      mp_future_ref <- mp
    }
  }
}

future_stack <- stack(future_list)
names(future_stack) <- future_names

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
        at = plot_ids,
        labels = holdridge_classes,
        cex = 0.55
      )
    )
  ) +
    latticeExtra::layer(sp.polygons(mp_future_ref$correg, lwd = 0.35, col = "gray30")) +
    latticeExtra::layer(sp.polygons(mp_future_ref$cuenca, lwd = 1.2, col = "black"))
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
    
    # Asegurar que futuro y baseline tengan misma geometría
    if (!compareRaster(hz_base, fut_clip, extent = TRUE, rowcol = TRUE, crs = TRUE, res = TRUE, stopiffalse = FALSE)) {
      fut_clip <- resample(fut_clip, hz_base, method = "ngb")
    }
    
    chg <- hz_base
    chg[] <- NA
    
    valid <- !is.na(hz_base[]) & !is.na(fut_clip[])
    
    chg[valid & hz_base[] == fut_clip[]] <- 0
    chg[valid & hz_base[] != fut_clip[]] <- 1
    
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
    latticeExtra::layer(sp.polygons(mp_base$correg, lwd = 0.35, col = "gray30")) +
    latticeExtra::layer(sp.polygons(mp_base$cuenca, lwd = 1.2, col = "black"))
)

dev.off()

