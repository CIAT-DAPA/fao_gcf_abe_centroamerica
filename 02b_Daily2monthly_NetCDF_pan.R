library(terra)
library(ncdf4)

base_dir <- "Z:/1.Data/Results/climate/02_climate_change/pan_raw"

vars <- c("Pr", "tmax", "tmin")
scenarios <- c("ssp245", "ssp585")
periods <- c("2041_2060", "2061_2080", "2081_2100")

for (ssp in scenarios) {
  for (per in periods) {
    for (var in vars) {
      
      in_dir <- file.path(base_dir, "daily", var, ssp, per, "Netcdf")
      out_dir <- file.path(base_dir, "monthly", var, ssp, per, "Netcdf")
      
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      
      files <- list.files(
        in_dir,
        pattern = "\\.nc$",
        full.names = TRUE
      )
      
      if (length(files) == 0) {
        message("No files found: ", in_dir)
        next
      }
      
      for (f in files) {
        
        message("Processing: ", var, " | ", ssp, " | ", per, " | ", basename(f))
        
        r <- rast(f)
        dates <- time(r)
        
        if (is.null(dates)) {
          warning("No time dimension found in: ", f)
          next
        }
        
        ym <- format(as.Date(dates), "%Y-%m")
        
        if (var == "Pr") {
          r_mon <- tapp(r, index = ym, fun = sum, na.rm = TRUE)
        } else {
          r_mon <- tapp(r, index = ym, fun = mean, na.rm = TRUE)
        }
        
        time(r_mon) <- as.Date(paste0(unique(ym), "-15"))
        
        out_name <- basename(f)
        out_name <- gsub("_day_", "_mon_", out_name)
        #out_name <- gsub("\\.nc$", "_monthly.nc", out_name)
        
        out_file <- file.path(out_dir, out_name)
        
        writeCDF(
          r_mon,
          filename = out_file,
          overwrite = TRUE
        )
      }
    }
  }
}





library(terra)

base_dir <- "Z:/1.Data/Results/climate/02_climate_change/pan_raw"

vars <- c("Pr", "tmax", "tmin")
scenarios <- c("ssp245", "ssp585")
periods <- c("2041_2060", "2061_2080", "2081_2100")

for (ssp in scenarios) {
  for (per in periods) {
    for (var in vars) {
      
      in_dir <- file.path(base_dir, "monthly", var, scn, prd, "Netcdf")
      out_dir <- file.path(base_dir, "climatology", var, scn, prd, "Netcdf")
      

      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- list.files(in_dir, pattern = "\\.nc$", full.names = TRUE)
      
      
      
      if (length(files) == 0) {
        
        message("No files found: ", in_dir)
        
        next
        
      }
      
      
      
      for (f in files) {
        
        
        
        message("Processing climatology: ", var, " | ", scn, " | ", prd, " | ", basename(f))
        
        
        
        r <- rast(f)
        
        dates <- time(r)
        
        
        
        if (is.null(dates)) {
          
          warning("No time dimension found in: ", f)
          
          next
          
        }
        
        
        
        months <- as.numeric(format(as.Date(dates), "%m"))
        
        
        
        # Climatología mensual multianual
        
        r_clim <- tapp(r, index = months, fun = mean, na.rm = TRUE)
        
        
        
        names(r_clim) <- paste0(var, "_", sprintf("%02d", 1:12))
        
        
        
        out_name <- basename(f)
        
        out_name <- gsub("_monthly\\.nc$", "_climatology.nc", out_name)
        
        out_name <- gsub("\\.nc$", "_climatology.nc", out_name)
        
        
        
        out_file <- file.path(out_dir, out_name)
        
        
        
        writeCDF(
          
          r_clim,
          
          filename = out_file,
          
          overwrite = TRUE
          
        )
        
      }
      
    }
    
  }
  
}







library(terra)

base_dir <- "Z:/1.Data/Results/climate/02_climate_change/pan_raw"
out_base <- "Z:/1.Data/Results/climate/02_climate_change/hnd_2_16min"

vars <- c("Pr", "tmax", "tmin")
scenarios <- c("ssp245", "ssp585")
periods <- c("2041_2060", "2061_2080", "2081_2100")

var_out <- c(
  "Pr" = "prec",
  "tmax" = "tmax",
  "tmin" = "tmin"
)

scn_out <- c(
  "ssp245" = "ssp_245",
  "ssp585" = "ssp_585"
)

prd_out <- c(
  "2041_2060" = "2050s",
  "2061_2080" = "2070s",
  "2081_2100" = "2090s"
)

get_model <- function(x) {
  nm <- basename(x)
  parts <- strsplit(nm, "_")[[1]]
  model <- parts[3]
  model <- tolower(gsub("-", "_", model))
  return(model)
}

# 1. Climatologías mensuales para prec, tmax, tmin
for (var in vars) {
  for (scn in scenarios) {
    for (prd in periods) {
      
      in_dir <- file.path(base_dir, "monthly", var, scn, prd, "Netcdf")
      files <- list.files(in_dir, pattern = "\\.nc$", full.names = TRUE)
      
      if (length(files) == 0) next
      
      for (f in files) {
        
        message("Processing: ", var, " | ", scn, " | ", prd, " | ", basename(f))
        
        r <- rast(f)
        dates <- time(r)
        months <- as.numeric(format(as.Date(dates), "%m"))
        
        r_clim <- tapp(r, index = months, fun = mean, na.rm = TRUE)
        
        model <- get_model(f)
        out_dir <- file.path(out_base, scn_out[scn], model, prd_out[prd])
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        
        for (m in 1:12) {
          out_file <- file.path(out_dir, paste0(var_out[var], "_", m, ".tif"))
          writeRaster(r_clim[[m]], out_file, overwrite = TRUE)
        }
      }
    }
  }
}

# 2. Calcular tmean = (tmax + tmin) / 2
for (scn in scenarios) {
  for (prd in periods) {
    
    tmax_dir <- file.path(base_dir, "monthly", "tmax", scn, prd, "Netcdf")
    tmin_dir <- file.path(base_dir, "monthly", "tmin", scn, prd, "Netcdf")
    
    tmax_files <- list.files(tmax_dir, pattern = "\\.nc$", full.names = TRUE)
    tmin_files <- list.files(tmin_dir, pattern = "\\.nc$", full.names = TRUE)
    
    if (length(tmax_files) == 0 | length(tmin_files) == 0) next
    
    for (fmax in tmax_files) {
      
      model <- get_model(fmax)
      fmin <- tmin_files[sapply(tmin_files, get_model) == model]
      
      if (length(fmin) == 0) {
        warning("No tmin found for model: ", model)
        next
      }
      
      message("Calculating tmean: ", scn, " | ", prd, " | ", model)
      
      rmax <- rast(fmax)
      rmin <- rast(fmin[1])
      
      dates <- time(rmax)
      months <- as.numeric(format(as.Date(dates), "%m"))
      
      tmean_daily <- (rmax + rmin) / 2
      tmean_clim <- tapp(tmean_daily, index = months, fun = mean, na.rm = TRUE)
      
      out_dir <- file.path(out_base, scn_out[scn], model, prd_out[prd])
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      
      for (m in 1:12) {
        out_file <- file.path(out_dir, paste0("tmean_", m, ".tif"))
        writeRaster(tmean_clim[[m]], out_file, overwrite = TRUE)
      }
    }
  }
}