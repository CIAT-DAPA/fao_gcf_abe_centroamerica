library(terra)
library(data.table)

# -----------------------------
# Paths
# -----------------------------

inputDir <- "Z:/1.Data/Results/climate/04_species"
outFolder <- file.path(inputDir, "mxe_outputs")
oDir <- file.path(outFolder, "_stats")
dir.create(oDir, recursive = TRUE, showWarnings = FALSE)

maskFile <- "Z:/1.Data/Process/Info_Inputs_SWAT/Panama/Tonosi_La_Villa/Division_administrativa/Tonosi_la_Villa_corregimientos.shp"

crs_ref <- crs(rast("Z:/1.Data/Results/climate/01_baseline/pan/atlas_1981-2022_30s/prec_1.tif"))

mask_adm1 <- project(vect(maskFile), crs_ref)
mask_adm1$ZONE_ID <- 1:nrow(mask_adm1)

adm_lookup <- data.table(
  ZONE_ID = mask_adm1$ZONE_ID,
  Provincia = mask_adm1$Provincia,
  Distrito = mask_adm1$Distrito,
  Corregimiento = mask_adm1$Corregimie
)

projectionList_raw <- c(
  "ssp_245/2050s",
  "ssp_245/2070s",
  "ssp_585/2050s",
  "ssp_585/2070s"
)

projectionList <- gsub("/", "_", projectionList_raw)

projection_lookup <- data.table(
  projection = projectionList,
  SSP = c("SSP2-4.5", "SSP2-4.5", "SSP5-8.5", "SSP5-8.5"),
  Period = c("2050s", "2070s", "2050s", "2070s")
)

suffix <- "atlas_1981-2022_30s"

speciesList <- c(
  "alouatta_palliata",
  "anacardium_excelsum",
  "dendrobates_auratus",
  "rhizophora_mangle"
)

# -----------------------------
# Funciones
# -----------------------------

prep_raster <- function(r, zones, target_res = 0.01) {
  
  zones_r <- project(zones, crs(r))
  
  r <- crop(r, zones_r)
  
  template <- rast(
    ext(zones_r),
    resolution = target_res,
    crs = crs(r)
  )
  
  r <- resample(r, template, method = "near")
  r <- mask(r, zones_r)
  
  r
}

zone_raster <- function(r, zones) {
  
  zones_r <- project(zones, crs(r))
  zones_r$ZONE_ID <- zones$ZONE_ID
  
  rasterize(
    zones_r,
    r[[1]],
    field = "ZONE_ID",
    touches = TRUE
  )
}

zonal_stats_maxent <- function(r, zones, adm_lookup, stat_name, species, scenario, period) {
  
  rz <- zone_raster(r, zones)
  
  z_mean <- as.data.table(zonal(r, rz, fun = "mean", na.rm = TRUE))
  z_sd   <- as.data.table(zonal(r, rz, fun = "sd", na.rm = TRUE))
  z_min  <- as.data.table(zonal(r, rz, fun = "min", na.rm = TRUE))
  z_max  <- as.data.table(zonal(r, rz, fun = "max", na.rm = TRUE))
  
  setnames(z_mean, c("ZONE_ID", "mean_value"))
  setnames(z_sd,   c("ZONE_ID", "sd_value"))
  setnames(z_min,  c("ZONE_ID", "min_value"))
  setnames(z_max,  c("ZONE_ID", "max_value"))
  
  out <- Reduce(
    function(x, y) merge(x, y, by = "ZONE_ID", all = TRUE),
    list(z_mean, z_sd, z_min, z_max)
  )
  
  out <- merge(out, adm_lookup, by = "ZONE_ID", all.x = TRUE)
  
  out[, species := species]
  out[, variable := stat_name]
  out[, SSP := scenario]
  out[, Period := period]
  
  setcolorder(
    out,
    c("species", "variable", "SSP", "Period",
      "ZONE_ID", "Provincia", "Distrito", "Corregimiento",
      "mean_value", "sd_value", "min_value", "max_value")
  )
  
  out
}

# -----------------------------
# Loop principal
# -----------------------------

all_stats <- list()

for (spID in speciesList) {
  
  message("Procesando especie: ", spID)
  
  outName <- file.path(outFolder, paste0("sp-", spID))
  
  # Línea base
  base_file <- file.path(
    outName,
    "crossval",
    paste0(
      toupper(substr(spID, 1, 1)),
      substr(spID, 2, nchar(spID)),
      "_",
      suffix,
      "_avg.asc"
    )
  )
  
  if (file.exists(base_file)) {
    
    r_base <- rast(base_file) * 100
    r_base <- prep_raster(r_base, mask_adm1, target_res = 0.01)
    
    all_stats[[paste(spID, "baseline", sep = "_")]] <- zonal_stats_maxent(
      r = r_base,
      zones = mask_adm1,
      adm_lookup = adm_lookup,
      stat_name = "suitability",
      species = spID,
      scenario = "current",
      period = "1981-2022"
    )
    
  } else {
    warning("No existe línea base: ", base_file)
  }
  
  # Futuros y cambios
  for (prj in projectionList) {
    
    prj_info <- projection_lookup[projection == prj]
    
    # Suitability futura
    fut_file <- file.path(
      outName,
      "projections",
      "averages",
      paste0(spID, "_", prj, "_EMN.asc")
    )
    
    if (file.exists(fut_file)) {
      
      r_fut <- rast(fut_file) * 100
      r_fut <- prep_raster(r_fut, mask_adm1, target_res = 0.01)
      
      all_stats[[paste(spID, prj, "suitability", sep = "_")]] <- zonal_stats_maxent(
        r = r_fut,
        zones = mask_adm1,
        adm_lookup = adm_lookup,
        stat_name = "suitability",
        species = spID,
        scenario = prj_info$SSP,
        period = prj_info$Period
      )
    } else {
      warning("No existe futuro: ", fut_file)
    }
    
    # Cambio futuro
    chg_file <- file.path(
      outName,
      "projections",
      "changes",
      paste0(spID, "_", prj, "_EMN.asc")
    )
    
    if (file.exists(chg_file)) {
      
      r_chg <- rast(chg_file) * 100
      r_chg <- prep_raster(r_chg, mask_adm1, target_res = 0.01)
      
      r_chg[r_chg < -100] <- -100
      r_chg[r_chg > 100] <- 100
      
      all_stats[[paste(spID, prj, "change", sep = "_")]] <- zonal_stats_maxent(
        r = r_chg,
        zones = mask_adm1,
        adm_lookup = adm_lookup,
        stat_name = "change",
        species = spID,
        scenario = prj_info$SSP,
        period = prj_info$Period
      )
    } else {
      warning("No existe cambio: ", chg_file)
    }
  }
}

stats_maxent_corregimiento <- rbindlist(all_stats, fill = TRUE)

fwrite(
  stats_maxent_corregimiento,
  file.path(oDir, "maxent_stats_corregimiento.csv")
)