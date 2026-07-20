library(terra)
library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(ggplot2)

# Directorios
bslDir <- "X:/Climate change scenarios/CMIP6_LATAM"
oDir   <- "Z:/1.Data/Results/climate/02_climate_change/gtm_indices"
tmpDir <- "Z:/1.Data/Results/climate/02_climate_change/gtm_daily"

dir.create(oDir, recursive = TRUE, showWarnings = FALSE)
dir.create(tmpDir, recursive = TRUE, showWarnings = FALSE)

# Parámetros
varList   <- c("pr", "hurs", "rsds", "sfcWind", "tasmax", "tasmin")
sspList   <- c( "ssp245", "ssp585")
# sspList   <- c("ssp585")
modelList <- c("ACCESS-ESM1-5", "EC-Earth3", "INM-CM5-0", "MPI-ESM1-2-HR", "MRI-ESM2-0")
# modelList <- c("MRI-ESM2-0")
years     <- 2026:2080

maskFile <- "Z:/1.Data/Results/climate/00_admin_data/gtm/cuenca_salinas_gtm_mun.shp"

ref_file <- list.files(
  file.path(bslDir, "pr", sspList[1], modelList[1]),
  pattern = "\\.nc$",
  full.names = TRUE
)[1]

crs_ref <- crs(rast(ref_file))

mask_adm1 <- project(vect(maskFile), crs_ref)
target_res <- 0.01
mask_adm1$ZONE_ID <- 1:nrow(mask_adm1)

adm_lookup <- data.table(
  ZONE_ID = mask_adm1$ZONE_ID,
  AdmLvl1 = mask_adm1$DEPTO,
  AdmLvl2 = mask_adm1$CODIGO,
  AdmLvl3 = mask_adm1$MUNICIPIO
)


#########################################
### Indices por cada GCM ###
#########################################
read_nc_year <- function(var, ssp, model, yr) {
  
  fdir <- file.path(bslDir, var, ssp, model)
  
  f <- list.files(
    fdir,
    pattern = paste0("^", var, "_day_", model, "_", ssp, ".*_", yr, "_v2\\.0\\.nc$"),
    full.names = TRUE
  )
  
  if (length(f) == 0) {
    warning("No existe archivo: ", var, " ", ssp, " ", model, " ", yr)
    return(NULL)
  }
  
  rast(f[1])
}

crop_to_aoi <- function(r, var, ssp, model, yr, zones) {
  
  if (is.null(r)) return(NULL)
  
  out_dir <- file.path(tmpDir, ssp, model, var)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_file <- file.path(
    out_dir,
    paste0(var, "_", ssp, "_", model, "_", yr, "_aoi_001deg.tif")
  )
  
  if (file.exists(out_file)) {
    return(rast(out_file))
  }
  
  zones_r <- project(zones, crs(r))
  
  # Recortar al área de interés
  r_crop <- crop(r, zones_r)
  
  # Crear plantilla fina a 0.01 grados
  template <- rast(
    ext(zones_r),
    resolution = target_res,
    crs = crs(r)
  )
  
  # Interpolar de 0.25° a 0.01°
  r_res <- resample(r_crop, template, method = "near")
  
  # Enmascarar al área de interés
  r_res <- mask(r_res, zones_r)
  
  writeRaster(
    r_res,
    out_file,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW", "TILED=YES")
  )
  
  rast(out_file)
}

make_zone_raster <- function(r, zones) {
  
  zones_r <- project(zones, crs(r))
  zones_r$ZONE_ID <- zones$ZONE_ID
  
  rasterize(
    zones_r,
    r[[1]],
    field = "ZONE_ID",
    touches = TRUE
  )
}

spell_stats <- function(x, min_len = 5) {
  
  r <- rle(x)
  spells <- r$lengths[r$values == TRUE]
  spells <- spells[spells >= min_len]
  
  c(
    n_spells = length(spells),
    mean_duration = ifelse(length(spells) > 0, mean(spells), 0),
    max_duration = ifelse(length(spells) > 0, max(spells), 0)
  )
}

calc_vpd <- function(tmean, hurs) {
  
  es <- 0.6108 * exp((17.27 * tmean) / (tmean + 237.3))
  ea <- es * (hurs / 100)
  
  es - ea
}

process_one_year <- function(ssp, model, yr) {
  
  message("Procesando: ", ssp, " | ", model, " | ", yr)
  
  pr     <- crop_to_aoi(read_nc_year("pr", ssp, model, yr), "pr", ssp, model, yr, mask_adm1)
  hurs   <- crop_to_aoi(read_nc_year("hurs", ssp, model, yr), "hurs", ssp, model, yr, mask_adm1)
  rsds   <- crop_to_aoi(read_nc_year("rsds", ssp, model, yr), "rsds", ssp, model, yr, mask_adm1)
  wspd   <- crop_to_aoi(read_nc_year("sfcWind", ssp, model, yr), "sfcWind", ssp, model, yr, mask_adm1)
  tasmax <- crop_to_aoi(read_nc_year("tasmax", ssp, model, yr), "tasmax", ssp, model, yr, mask_adm1)
  tasmin <- crop_to_aoi(read_nc_year("tasmin", ssp, model, yr), "tasmin", ssp, model, yr, mask_adm1)
  
  if (
    is.null(pr) |
    is.null(hurs) |
    is.null(rsds) |
    is.null(wspd) |
    is.null(tasmax) |
    is.null(tasmin)
  ) {
    return(NULL)
  }
  
  nd <- nlyr(hurs)
  
  names(pr)     <- paste0("pr_", 1:nd)
  names(hurs)   <- paste0("hurs_", 1:nd)
  names(rsds)   <- paste0("rsds_", 1:nd)
  names(wspd)   <- paste0("sfcWind_", 1:nd)
  names(tasmax) <- paste0("tasmax_", 1:nd)
  names(tasmin) <- paste0("tasmin_", 1:nd)
  
  stk <- c(pr, hurs, rsds, wspd, tasmax, tasmin)
  rz <- make_zone_raster(pr, mask_adm1)
  
  z <- terra::zonal(stk, rz, fun = "mean", na.rm = TRUE)
  z <- as.data.table(z)
  setnames(z, 1, "ZONE_ID")
  
  dates <- seq(
    as.Date(paste0(yr, "-01-01")),
    as.Date(paste0(yr, "-12-31")),
    by = "day"
  )[1:nd]
  
  make_var_dt <- function(z, var, dates) {
    
    cols <- grep(paste0("^", var, "_"), names(z), value = TRUE)
    
    out <- melt(
      z[, c("ZONE_ID", cols), with = FALSE],
      id.vars = "ZONE_ID",
      variable.name = "layer",
      value.name = var
    )
    
    out[, day_id := as.integer(sub(paste0("^", var, "_"), "", layer))]
    out[, date := dates[day_id]]
    out[, c("layer", "day_id") := NULL]
    
    out
  }
  
  df <- make_var_dt(z, "pr", dates)
  df <- merge(df, make_var_dt(z, "hurs", dates), by = c("ZONE_ID", "date"))
  df <- merge(df, make_var_dt(z, "rsds", dates), by = c("ZONE_ID", "date"))
  df <- merge(df, make_var_dt(z, "sfcWind", dates), by = c("ZONE_ID", "date"))
  df <- merge(df, make_var_dt(z, "tasmax", dates), by = c("ZONE_ID", "date"))
  df <- merge(df, make_var_dt(z, "tasmin", dates), by = c("ZONE_ID", "date"))
  
  df <- merge(df, adm_lookup, by = "ZONE_ID")
  
  df[, SSP := ssp]
  df[, Model := model]
  df[, year := lubridate::year(date)]
  df[, month := lubridate::month(date)]
  df[, tmean := (tasmax + tasmin) / 2]
  df[, vpd := calc_vpd(tmean, hurs)]
  df[, dry := pr <= 1]
  df[, wet := pr > 1]
  
  indices_monthly_correg <- df[, {
    
    dry_s <- spell_stats(dry)
    wet_s <- spell_stats(wet)
    
    .(
      total_prec = sum(pr, na.rm = TRUE),
      mean_hurs = mean(hurs, na.rm = TRUE),
      mean_rsds = mean(rsds, na.rm = TRUE),
      mean_wspd = mean(sfcWind, na.rm = TRUE),
      mean_tasmax = mean(tasmax, na.rm = TRUE),
      mean_tasmin = mean(tasmin, na.rm = TRUE),
      mean_tmean = mean(tmean, na.rm = TRUE),
      mean_vpd = mean(vpd, na.rm = TRUE),
      max_vpd = max(vpd, na.rm = TRUE),
      total_dry_days = sum(dry, na.rm = TRUE),
      total_wet_days = sum(wet, na.rm = TRUE),
      dry_n_spells = dry_s["n_spells"],
      dry_mean_duration = dry_s["mean_duration"],
      dry_max_duration = dry_s["max_duration"],
      wet_n_spells = wet_s["n_spells"],
      wet_mean_duration = wet_s["mean_duration"],
      wet_max_duration = wet_s["max_duration"],
      wet_spell_intensity = ifelse(sum(wet, na.rm = TRUE) > 0, mean(pr[wet], na.rm = TRUE), 0)
    )
    
  }, by = .(SSP, Model, AdmLvl1, AdmLvl2, AdmLvl3, year, month)]
  
  indices_annual_correg <- indices_monthly_correg[, .(
    annual_prec = sum(total_prec, na.rm = TRUE),
    mean_hurs = mean(mean_hurs, na.rm = TRUE),
    mean_rsds = mean(mean_rsds, na.rm = TRUE),
    mean_wspd = mean(mean_wspd, na.rm = TRUE),
    mean_tasmax = mean(mean_tasmax, na.rm = TRUE),
    mean_tasmin = mean(mean_tasmin, na.rm = TRUE),
    mean_tmean = mean(mean_tmean, na.rm = TRUE),
    mean_vpd = mean(mean_vpd, na.rm = TRUE),
    max_vpd = max(max_vpd, na.rm = TRUE),
    annual_dry_days = sum(total_dry_days, na.rm = TRUE),
    annual_wet_days = sum(total_wet_days, na.rm = TRUE),
    annual_dry_spells = sum(dry_n_spells, na.rm = TRUE),
    annual_wet_spells = sum(wet_n_spells, na.rm = TRUE),
    mean_dry_duration = mean(dry_mean_duration, na.rm = TRUE),
    mean_wet_duration = mean(wet_mean_duration, na.rm = TRUE),
    mean_wet_intensity = mean(wet_spell_intensity, na.rm = TRUE),
    max_dry_spell = max(dry_max_duration, na.rm = TRUE),
    max_wet_spell = max(wet_max_duration, na.rm = TRUE)
  ), by = .(SSP, Model, AdmLvl1, AdmLvl2, AdmLvl3, year)]
  
  indices_monthly_distrito <- indices_monthly_correg[, .(
    total_prec = mean(total_prec, na.rm = TRUE),
    mean_hurs = mean(mean_hurs, na.rm = TRUE),
    mean_rsds = mean(mean_rsds, na.rm = TRUE),
    mean_wspd = mean(mean_wspd, na.rm = TRUE),
    mean_tasmax = mean(mean_tasmax, na.rm = TRUE),
    mean_tasmin = mean(mean_tasmin, na.rm = TRUE),
    mean_tmean = mean(mean_tmean, na.rm = TRUE),
    mean_vpd = mean(mean_vpd, na.rm = TRUE),
    max_vpd = max(max_vpd, na.rm = TRUE),
    total_dry_days = mean(total_dry_days, na.rm = TRUE),
    total_wet_days = mean(total_wet_days, na.rm = TRUE),
    dry_n_spells = mean(dry_n_spells, na.rm = TRUE),
    dry_mean_duration = mean(dry_mean_duration, na.rm = TRUE),
    dry_max_duration = max(dry_max_duration, na.rm = TRUE),
    wet_n_spells = mean(wet_n_spells, na.rm = TRUE),
    wet_mean_duration = mean(wet_mean_duration, na.rm = TRUE),
    wet_max_duration = max(wet_max_duration, na.rm = TRUE),
    wet_spell_intensity = mean(wet_spell_intensity, na.rm = TRUE),
    n_corregimientos = .N
  ), by = .(SSP, Model, AdmLvl1, AdmLvl2, year, month)]
  
  indices_annual_distrito <- indices_monthly_distrito[, .(
    annual_prec = sum(total_prec, na.rm = TRUE),
    mean_hurs = mean(mean_hurs, na.rm = TRUE),
    mean_rsds = mean(mean_rsds, na.rm = TRUE),
    mean_wspd = mean(mean_wspd, na.rm = TRUE),
    mean_tasmax = mean(mean_tasmax, na.rm = TRUE),
    mean_tasmin = mean(mean_tasmin, na.rm = TRUE),
    mean_tmean = mean(mean_tmean, na.rm = TRUE),
    mean_vpd = mean(mean_vpd, na.rm = TRUE),
    max_vpd = max(max_vpd, na.rm = TRUE),
    annual_dry_days = sum(total_dry_days, na.rm = TRUE),
    annual_wet_days = sum(total_wet_days, na.rm = TRUE),
    annual_dry_spells = sum(dry_n_spells, na.rm = TRUE),
    annual_wet_spells = sum(wet_n_spells, na.rm = TRUE),
    mean_dry_duration = mean(dry_mean_duration, na.rm = TRUE),
    mean_wet_duration = mean(wet_mean_duration, na.rm = TRUE),
    mean_wet_intensity = mean(wet_spell_intensity, na.rm = TRUE),
    max_dry_spell = max(dry_max_duration, na.rm = TRUE),
    max_wet_spell = max(wet_max_duration, na.rm = TRUE),
    n_corregimientos = mean(n_corregimientos, na.rm = TRUE)
  ), by = .(SSP, Model, AdmLvl1, AdmLvl2, year)]
  
  list(
    monthly_correg = indices_monthly_correg,
    annual_correg = indices_annual_correg,
    monthly_distrito = indices_monthly_distrito,
    annual_distrito = indices_annual_distrito
  )
}

for (ssp in sspList) {
  
  for (model in modelList) {
    
    out_mon_correg <- list()
    out_ann_correg <- list()
    out_mon_dist <- list()
    out_ann_dist <- list()
    
    for (yr in years) {
      
      res <- process_one_year(ssp, model, yr)
      
      if (is.null(res)) next
      
      out_mon_correg[[as.character(yr)]] <- res$monthly_correg
      out_ann_correg[[as.character(yr)]] <- res$annual_correg
      out_mon_dist[[as.character(yr)]] <- res$monthly_distrito
      out_ann_dist[[as.character(yr)]] <- res$annual_distrito
    }
    
    fwrite(
      rbindlist(out_mon_correg, fill = TRUE),
      file.path(oDir, paste0("indices_", ssp, "_", model, "_2026_2080_corregimiento_mon.csv"))
    )
    
    fwrite(
      rbindlist(out_ann_correg, fill = TRUE),
      file.path(oDir, paste0("indices_", ssp, "_", model, "_2026_2080_corregimiento_ann.csv"))
    )
    
    fwrite(
      rbindlist(out_mon_dist, fill = TRUE),
      file.path(oDir, paste0("indices_", ssp, "_", model, "_2026_2080_distrito_mon.csv"))
    )
    
    fwrite(
      rbindlist(out_ann_dist, fill = TRUE),
      file.path(oDir, paste0("indices_", ssp, "_", model, "_2026_2080_distrito_ann.csv"))
    )
  }
}






#########################################
### Ensemble por SSP para los índices ###
### Eliminando filas incompletas antes ###
#########################################

index_cols_monthly <- c(
  "total_prec",
  "mean_hurs",
  "mean_rsds",
  "mean_wspd",
  "mean_tasmax",
  "mean_tasmin",
  "mean_tmean",
  "mean_vpd",
  "max_vpd",
  "total_dry_days",
  "total_wet_days",
  "dry_n_spells",
  "dry_mean_duration",
  "dry_max_duration",
  "wet_n_spells",
  "wet_mean_duration",
  "wet_max_duration",
  "wet_spell_intensity"
)

index_cols_annual <- c(
  "annual_prec",
  "mean_hurs",
  "mean_rsds",
  "mean_wspd",
  "mean_tasmax",
  "mean_tasmin",
  "mean_tmean",
  "mean_vpd",
  "max_vpd",
  "annual_dry_days",
  "annual_wet_days",
  "annual_dry_spells",
  "annual_wet_spells",
  "mean_dry_duration",
  "mean_wet_duration",
  "mean_wet_intensity",
  "max_dry_spell",
  "max_wet_spell"
)

read_and_clean_indices <- function(files, index_cols) {
  
  files <- files[file.exists(files)]
  
  if (length(files) == 0) {
    warning("No se encontraron archivos para ensemble.")
    return(NULL)
  }
  
  dt <- rbindlist(lapply(files, fread), fill = TRUE)
  
  n_before <- nrow(dt)
  
  # Eliminar filas incompletas en las columnas usadas para el ensemble
  dt <- dt[complete.cases(dt[, ..index_cols])]
  
  n_after <- nrow(dt)
  
  message("Filas originales: ", n_before)
  message("Filas después de eliminar incompletas: ", n_after)
  message("Filas eliminadas: ", n_before - n_after)
  
  if (nrow(dt) == 0) {
    warning("Después de eliminar filas incompletas no quedan datos válidos.")
    return(NULL)
  }
  
  dt
}

ensemble_stats <- function(dt, group_cols, index_cols) {
  
  dt <- as.data.table(dt)
  
  dt[, lapply(.SD, mean, na.rm = TRUE),
     by = group_cols,
     .SDcols = index_cols]
}

for (ssp in sspList) {
  
  message("Calculando ensemble: ", ssp)
  
  #######################################
  # AdmLvl3 mensual
  #######################################
  
  files_correg_mon <- file.path(
    oDir,
    paste0("indices_", ssp, "_", modelList, "_2026_2080_corregimiento_mon.csv")
  )
  
  dt_correg_mon <- read_and_clean_indices(files_correg_mon, index_cols_monthly)
  
  if (!is.null(dt_correg_mon)) {
    
    ens_correg_mon <- ensemble_stats(
      dt_correg_mon,
      group_cols = c("SSP", "AdmLvl1", "AdmLvl2", "AdmLvl3", "year", "month"),
      index_cols = index_cols_monthly
    )
    
    ens_correg_mon[, Model := "ensemble"]
    setcolorder(
      ens_correg_mon,
      c("SSP", "Model", "AdmLvl1", "AdmLvl2", "AdmLvl3", "year", "month")
    )
    
    fwrite(
      ens_correg_mon,
      file.path(oDir, paste0("indices_", ssp, "_ensemble_2026_2080_corregimiento_mon.csv"))
    )
  }
  
  
  #######################################
  # AdmLvl3 anual
  #######################################
  
  files_correg_ann <- file.path(
    oDir,
    paste0("indices_", ssp, "_", modelList, "_2026_2080_corregimiento_ann.csv")
  )
  
  dt_correg_ann <- read_and_clean_indices(files_correg_ann, index_cols_annual)
  
  if (!is.null(dt_correg_ann)) {
    
    ens_correg_ann <- ensemble_stats(
      dt_correg_ann,
      group_cols = c("SSP", "AdmLvl1", "AdmLvl2", "AdmLvl3", "year"),
      index_cols = index_cols_annual
    )
    
    ens_correg_ann[, Model := "ensemble"]
    setcolorder(
      ens_correg_ann,
      c("SSP", "Model", "AdmLvl1", "AdmLvl2", "AdmLvl3", "year")
    )
    
    fwrite(
      ens_correg_ann,
      file.path(oDir, paste0("indices_", ssp, "_ensemble_2026_2080_corregimiento_ann.csv"))
    )
  }
  
  
  #######################################
  # AdmLvl2 mensual
  #######################################
  
  files_dist_mon <- file.path(
    oDir,
    paste0("indices_", ssp, "_", modelList, "_2026_2080_distrito_mon.csv")
  )
  
  dt_dist_mon <- read_and_clean_indices(files_dist_mon, index_cols_monthly)
  
  if (!is.null(dt_dist_mon)) {
    
    ens_dist_mon <- ensemble_stats(
      dt_dist_mon,
      group_cols = c("SSP", "AdmLvl1", "AdmLvl2", "year", "month"),
      index_cols = index_cols_monthly
    )
    
    ens_dist_mon[, Model := "ensemble"]
    setcolorder(
      ens_dist_mon,
      c("SSP", "Model", "AdmLvl1", "AdmLvl2", "year", "month")
    )
    
    fwrite(
      ens_dist_mon,
      file.path(oDir, paste0("indices_", ssp, "_ensemble_2026_2080_distrito_mon.csv"))
    )
  }
  
  
  #######################################
  # AdmLvl2 anual
  #######################################
  
  files_dist_ann <- file.path(
    oDir,
    paste0("indices_", ssp, "_", modelList, "_2026_2080_distrito_ann.csv")
  )
  
  dt_dist_ann <- read_and_clean_indices(files_dist_ann, index_cols_annual)
  
  if (!is.null(dt_dist_ann)) {
    
    ens_dist_ann <- ensemble_stats(
      dt_dist_ann,
      group_cols = c("SSP", "AdmLvl1", "AdmLvl2", "year"),
      index_cols = index_cols_annual
    )
    
    ens_dist_ann[, Model := "ensemble"]
    setcolorder(
      ens_dist_ann,
      c("SSP", "Model", "AdmLvl1", "AdmLvl2", "year")
    )
    
    fwrite(
      ens_dist_ann,
      file.path(oDir, paste0("indices_", ssp, "_ensemble_2026_2080_distrito_ann.csv"))
    )
  }
}




#########################################
### Completar corregimientos faltantes ###
### en archivos ensemble                ###
#########################################

fix_correg_ensemble <- function(file, time_cols, index_cols) {
  
  norm_key <- function(x) {
    x <- trimws(as.character(x))
    x <- ifelse(is.na(x), NA_character_, x)
    x <- ifelse(grepl("^[0-9]+$", x), sub("^0+", "", x), x)
    x <- ifelse(x == "", NA_character_, x)
    x
  }
  
  if (!file.exists(file)) {
    warning("No existe archivo: ", file)
    return(NULL)
  }
  
  ens <- fread(file)
  
  if (nrow(ens) == 0) {
    warning("Archivo vacío: ", file)
    return(NULL)
  }
  
  # Verificar que existan las columnas necesarias
  needed_cols <- c(
    "SSP", "Model", "AdmLvl1", "AdmLvl2", "AdmLvl3",
    time_cols,
    index_cols
  )
  
  missing_cols <- setdiff(needed_cols, names(ens))
  
  if (length(missing_cols) > 0) {
    stop("Faltan columnas en el archivo: ", paste(missing_cols, collapse = ", "))
  }
  
  # Si el archivo ya quedó totalmente vacío en índices, detener
  if (all(sapply(ens[, ..index_cols], function(x) all(is.na(x))))) {
    stop(
      "El archivo ya tiene todos los índices en NA. ",
      "Regenera/restaura el ensemble original antes de corregirlo."
    )
  }
  
  # Tabla completa de corregimientos desde maskFile / adm_lookup
  adm_full <- unique(adm_lookup[, .(
    AdmLvl1,
    AdmLvl2,
    AdmLvl3
  )])
  
  adm_full[, Provincia_key := norm_key(AdmLvl1)]
  adm_full[, Distrito_key := norm_key(AdmLvl2)]
  adm_full[, Corregimiento_key := norm_key(AdmLvl3)]
  
  ens[, Provincia_key := norm_key(AdmLvl1)]
  ens[, Distrito_key := norm_key(AdmLvl2)]
  ens[, Corregimiento_key := norm_key(AdmLvl3)]
  
  # Mantener nombres administrativos originales del maskFile
  adm_full[, AdmLvl1 := as.character(AdmLvl1)]
  adm_full[, AdmLvl2 := as.character(AdmLvl2)]
  adm_full[, AdmLvl3 := as.character(AdmLvl3)]
  
  ens[, AdmLvl1 := as.character(AdmLvl1)]
  ens[, AdmLvl2 := as.character(AdmLvl2)]
  ens[, AdmLvl3 := as.character(AdmLvl3)]
  
  # Combinaciones temporales existentes
  time_dt <- unique(
    ens[, c("SSP", "Model", time_cols), with = FALSE]
  )
  
  # Crear grilla completa:
  # cada año/mes x todos los corregimientos del maskFile
  time_dt[, tmp_join := 1]
  adm_full[, tmp_join := 1]
  
  full_grid <- merge(
    time_dt,
    adm_full,
    by = "tmp_join",
    allow.cartesian = TRUE
  )
  
  full_grid[, tmp_join := NULL]
  time_dt[, tmp_join := NULL]
  adm_full[, tmp_join := NULL]
  
  join_cols <- c(
    "SSP",
    "Model",
    "Provincia_key",
    "Distrito_key",
    "Corregimiento_key",
    time_cols
  )
  
  # Datos originales por corregimiento
  ens_values <- ens[, c(join_cols, index_cols), with = FALSE]
  
  # Promedio del distrito
  dist_mean <- ens[, lapply(.SD, mean, na.rm = TRUE),
                   by = c("SSP", "Model", "Provincia_key", "Distrito_key", time_cols),
                   .SDcols = index_cols]
  
  # Promedio de la provincia
  prov_mean <- ens[, lapply(.SD, mean, na.rm = TRUE),
                   by = c("SSP", "Model", "Provincia_key", time_cols),
                   .SDcols = index_cols]
  
  # Promedio global por SSP y tiempo
  global_mean <- ens[, lapply(.SD, mean, na.rm = TRUE),
                     by = c("SSP", "Model", time_cols),
                     .SDcols = index_cols]
  
  # Unir valores originales
  out <- merge(
    full_grid,
    ens_values,
    by = join_cols,
    all.x = TRUE
  )
  
  # Unir respaldo distrito
  out <- merge(
    out,
    dist_mean,
    by = c("SSP", "Model", "Provincia_key", "Distrito_key", time_cols),
    all.x = TRUE,
    suffixes = c("", "_dist")
  )
  
  # Unir respaldo provincia
  out <- merge(
    out,
    prov_mean,
    by = c("SSP", "Model", "Provincia_key", time_cols),
    all.x = TRUE,
    suffixes = c("", "_prov")
  )
  
  # Unir respaldo global
  out <- merge(
    out,
    global_mean,
    by = c("SSP", "Model", time_cols),
    all.x = TRUE,
    suffixes = c("", "_global")
  )
  
  # Relleno jerárquico:
  # corregimiento original -> distrito -> provincia -> global
  for (col in index_cols) {
    
    col_dist <- paste0(col, "_dist")
    col_prov <- paste0(col, "_prov")
    col_glob <- paste0(col, "_global")
    
    out[is.na(get(col)), (col) := get(col_dist)]
    out[is.na(get(col)), (col) := get(col_prov)]
    out[is.na(get(col)), (col) := get(col_glob)]
  }
  
  # Eliminar columnas auxiliares
  aux_cols <- c(
    paste0(index_cols, "_dist"),
    paste0(index_cols, "_prov"),
    paste0(index_cols, "_global"),
    "Provincia_key",
    "Distrito_key",
    "Corregimiento_key"
  )
  
  aux_cols <- intersect(aux_cols, names(out))
  out[, c(aux_cols) := NULL]
  
  # Orden final
  key_cols <- c(
    "SSP",
    "Model",
    "AdmLvl1",
    "AdmLvl2",
    "AdmLvl3",
    time_cols
  )
  
  setcolorder(out, c(key_cols, index_cols))
  setorder(out, AdmLvl1, AdmLvl2, AdmLvl3, year)
  
  if ("month" %in% names(out)) {
    setorder(out, AdmLvl1, AdmLvl2, AdmLvl3, year, month)
  }
  
  # Diagnóstico
  n_original <- nrow(ens)
  n_final <- nrow(out)
  n_added <- n_final - n_original
  
  na_remaining <- sum(is.na(out[, ..index_cols]))
  
  message("Archivo corregido: ", basename(file))
  message("Filas originales: ", n_original)
  message("Filas finales: ", n_final)
  message("Filas agregadas: ", n_added)
  message("Valores NA restantes en índices: ", na_remaining)
  
  
  # Orden final
  if ("month" %in% names(out)) {
    
    setorder(
      out,
      year,
      month,
      AdmLvl1,
      AdmLvl2,
      AdmLvl3
    )
    
  } else {
    
    setorder(
      out,
      year,
      AdmLvl1,
      AdmLvl2,
      AdmLvl3
    )
    
  }
  
  # Reordenar columnas
  key_cols <- c(
    "SSP",
    "Model",
    "AdmLvl1",
    "AdmLvl2",
    "AdmLvl3",
    time_cols
  )
  
  setcolorder(out, c(key_cols, index_cols))

  file_out <- sub("\\.csv$", "_completed.csv", file)
  
  fwrite(out, file_out)
  
  message("Archivo original conservado: ", basename(file))
  message("Archivo corregido escrito como: ", basename(file_out))
    
  invisible(out)
}

for (ssp in sspList) {
  
  fix_correg_ensemble(
    file = file.path(
      oDir,
      paste0("indices_", ssp, "_ensemble_2026_2080_corregimiento_mon.csv")
    ),
    time_cols = c("year", "month"),
    index_cols = index_cols_monthly
  )
  
  fix_correg_ensemble(
    file = file.path(
      oDir,
      paste0("indices_", ssp, "_ensemble_2026_2080_corregimiento_ann.csv")
    ),
    time_cols = c("year"),
    index_cols = index_cols_annual
  )
}


#########################################
### Índices en raster mensual y anual ###
#########################################

rasterIndexDir <- file.path(oDir, "rasters")
dir.create(rasterIndexDir, recursive = TRUE, showWarnings = FALSE)

calc_vpd_raster <- function(tmean, hurs) {
  es <- 0.6108 * exp((17.27 * tmean) / (tmean + 237.3))
  ea <- es * (hurs / 100)
  es - ea
}

write_index_raster <- function(r, out_file) {
  writeRaster(
    r,
    out_file,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW", "TILED=YES")
  )
}

process_grid_indices_year <- function(ssp, model, yr) {

  message("Índices raster: ", ssp, " | ", model, " | ", yr)

  pr     <- crop_to_aoi(read_nc_year("pr", ssp, model, yr), "pr", ssp, model, yr, mask_adm1)
  hurs   <- crop_to_aoi(read_nc_year("hurs", ssp, model, yr), "hurs", ssp, model, yr, mask_adm1)
  rsds   <- crop_to_aoi(read_nc_year("rsds", ssp, model, yr), "rsds", ssp, model, yr, mask_adm1)
  wspd   <- crop_to_aoi(read_nc_year("sfcWind", ssp, model, yr), "sfcWind", ssp, model, yr, mask_adm1)
  tasmax <- crop_to_aoi(read_nc_year("tasmax", ssp, model, yr), "tasmax", ssp, model, yr, mask_adm1)
  tasmin <- crop_to_aoi(read_nc_year("tasmin", ssp, model, yr), "tasmin", ssp, model, yr, mask_adm1)

  if (
    is.null(pr) |
    is.null(hurs) |
    is.null(rsds) |
    is.null(wspd) |
    is.null(tasmax) |
    is.null(tasmin)
  ) return(NULL)

  nd <- nlyr(pr)

  dates <- seq(
    as.Date(paste0(yr, "-01-01")),
    as.Date(paste0(yr, "-12-31")),
    by = "day"
  )[1:nd]

  months <- lubridate::month(dates)

  tmean <- (tasmax + tasmin) / 2
  vpd <- calc_vpd_raster(tmean, hurs)

  dry <- pr <= 1
  wet <- pr > 1

  out_base <- file.path(rasterIndexDir, ssp, model, as.character(yr))
  dir.create(out_base, recursive = TRUE, showWarnings = FALSE)

  for (m in 1:12) {

    idx <- which(months == m)

    total_prec <- sum(pr[[idx]], na.rm = TRUE)
    mean_hurs <- mean(hurs[[idx]], na.rm = TRUE)
    mean_rsds <- mean(rsds[[idx]], na.rm = TRUE)
    mean_wspd <- mean(wspd[[idx]], na.rm = TRUE)
    mean_tasmax <- mean(tasmax[[idx]], na.rm = TRUE)
    mean_tasmin <- mean(tasmin[[idx]], na.rm = TRUE)
    mean_tmean <- mean(tmean[[idx]], na.rm = TRUE)
    mean_vpd <- mean(vpd[[idx]], na.rm = TRUE)
    max_vpd <- max(vpd[[idx]], na.rm = TRUE)
    total_dry_days <- sum(dry[[idx]], na.rm = TRUE)
    total_wet_days <- sum(wet[[idx]], na.rm = TRUE)

    write_index_raster(total_prec, file.path(out_base, paste0("total_prec_", yr, "_", sprintf("%02d", m), ".tif")))
    write_index_raster(mean_hurs, file.path(out_base, paste0("mean_hurs_", yr, "_", sprintf("%02d", m), ".tif")))
    write_index_raster(mean_rsds, file.path(out_base, paste0("mean_rsds_", yr, "_", sprintf("%02d", m), ".tif")))
    write_index_raster(mean_wspd, file.path(out_base, paste0("mean_wspd_", yr, "_", sprintf("%02d", m), ".tif")))
    write_index_raster(mean_tasmax, file.path(out_base, paste0("mean_tasmax_", yr, "_", sprintf("%02d", m), ".tif")))
    write_index_raster(mean_tasmin, file.path(out_base, paste0("mean_tasmin_", yr, "_", sprintf("%02d", m), ".tif")))
    write_index_raster(mean_tmean, file.path(out_base, paste0("mean_tmean_", yr, "_", sprintf("%02d", m), ".tif")))
    write_index_raster(mean_vpd, file.path(out_base, paste0("mean_vpd_", yr, "_", sprintf("%02d", m), ".tif")))
    write_index_raster(max_vpd, file.path(out_base, paste0("max_vpd_", yr, "_", sprintf("%02d", m), ".tif")))
    write_index_raster(total_dry_days, file.path(out_base, paste0("total_dry_days_", yr, "_", sprintf("%02d", m), ".tif")))
    write_index_raster(total_wet_days, file.path(out_base, paste0("total_wet_days_", yr, "_", sprintf("%02d", m), ".tif")))
  }

  annual_prec <- sum(pr, na.rm = TRUE)
  annual_hurs <- mean(hurs, na.rm = TRUE)
  annual_rsds <- mean(rsds, na.rm = TRUE)
  annual_wspd <- mean(wspd, na.rm = TRUE)
  annual_tasmax <- mean(tasmax, na.rm = TRUE)
  annual_tasmin <- mean(tasmin, na.rm = TRUE)
  annual_tmean <- mean(tmean, na.rm = TRUE)
  annual_vpd <- mean(vpd, na.rm = TRUE)
  annual_max_vpd <- max(vpd, na.rm = TRUE)
  annual_dry_days <- sum(dry, na.rm = TRUE)
  annual_wet_days <- sum(wet, na.rm = TRUE)

  write_index_raster(annual_prec, file.path(out_base, paste0("annual_prec_", yr, ".tif")))
  write_index_raster(annual_hurs, file.path(out_base, paste0("mean_hurs_", yr, "_ann.tif")))
  write_index_raster(annual_rsds, file.path(out_base, paste0("mean_rsds_", yr, "_ann.tif")))
  write_index_raster(annual_wspd, file.path(out_base, paste0("mean_wspd_", yr, "_ann.tif")))
  write_index_raster(annual_tasmax, file.path(out_base, paste0("mean_tasmax_", yr, "_ann.tif")))
  write_index_raster(annual_tasmin, file.path(out_base, paste0("mean_tasmin_", yr, "_ann.tif")))
  write_index_raster(annual_tmean, file.path(out_base, paste0("mean_tmean_", yr, "_ann.tif")))
  write_index_raster(annual_vpd, file.path(out_base, paste0("mean_vpd_", yr, "_ann.tif")))
  write_index_raster(annual_max_vpd, file.path(out_base, paste0("max_vpd_", yr, "_ann.tif")))
  write_index_raster(annual_dry_days, file.path(out_base, paste0("annual_dry_days_", yr, ".tif")))
  write_index_raster(annual_wet_days, file.path(out_base, paste0("annual_wet_days_", yr, ".tif")))
}

for (ssp in sspList) {
  for (model in modelList) {
    for (yr in years) {
      process_grid_indices_year(ssp, model, yr)
    }
  }
}




#########################################################
### Promedios multianuales raster - ensemble de GCMs  ###
#########################################################

rasterIndexDir <- file.path(oDir, "rasters")
dir.create(rasterIndexDir, recursive = TRUE, showWarnings = FALSE)

multiDir <- file.path(rasterIndexDir, "ensemble_multianual")
dir.create(multiDir, recursive = TRUE, showWarnings = FALSE)

periods <- list(
  "2050s" = 2041:2060,
  "2070s" = 2061:2080
)

monthly_indices <- c(
  "total_prec",
  "mean_hurs",
  "mean_rsds",
  "mean_wspd",
  "mean_tasmax",
  "mean_tasmin",
  "mean_tmean",
  "mean_vpd",
  "max_vpd",
  "total_dry_days",
  "total_wet_days"
)

annual_indices <- c(
  "annual_prec",
  "mean_hurs",
  "mean_rsds",
  "mean_wspd",
  "mean_tasmax",
  "mean_tasmin",
  "mean_tmean",
  "mean_vpd",
  "max_vpd",
  "annual_dry_days",
  "annual_wet_days"
)

write_mean_raster <- function(files, out_file) {

  files <- files[file.exists(files)]

  if (length(files) == 0) {
    warning("No hay archivos para: ", out_file)
    return(NULL)
  }

  r <- rast(files)
  r_mean <- mean(r, na.rm = TRUE)

  writeRaster(
    r_mean,
    out_file,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW", "TILED=YES")
  )
}

for (ssp in sspList) {

  for (prd in names(periods)) {

    yrs <- periods[[prd]]

    out_ssp_dir <- file.path(multiDir, ssp, prd)
    dir.create(out_ssp_dir, recursive = TRUE, showWarnings = FALSE)

    message("Calculando ensemble multianual: ", ssp, " | ", prd)

    # Mensuales
    for (idx in monthly_indices) {

      for (m in 1:12) {

        files <- c()

        for (model in modelList) {
          files <- c(
            files,
            file.path(
              rasterIndexDir,
              ssp,
              model,
              yrs,
              paste0(idx, "_", yrs, "_", sprintf("%02d", m), ".tif")
            )
          )
        }

        out_file <- file.path(
          out_ssp_dir,
          paste0(idx, "_", ssp, "_ensemble_", prd, "_", sprintf("%02d", m), ".tif")
        )

        write_mean_raster(files, out_file)
      }
    }

    # Anuales
    for (idx in annual_indices) {

      files <- c()

      for (model in modelList) {

        if (idx %in% c("annual_prec", "annual_dry_days", "annual_wet_days")) {
          files <- c(
            files,
            file.path(
              rasterIndexDir,
              ssp,
              model,
              yrs,
              paste0(idx, "_", yrs, ".tif")
            )
          )
        } else {
          files <- c(
            files,
            file.path(
              rasterIndexDir,
              ssp,
              model,
              yrs,
              paste0(idx, "_", yrs, "_ann.tif")
            )
          )
        }
      }

      out_file <- file.path(
        out_ssp_dir,
        paste0(idx, "_", ssp, "_ensemble_", prd, "_ann.tif")
      )

      write_mean_raster(files, out_file)
    }
  }
}


