library(raster)

in_dir <- "Z:/1.Data/Results/climate/01_baseline/pan/atlas_1981-2022_30s"
msk <- raster("Z:/1.Data/Results/climate/00_admin_data/pan_msk_30s.tif")
files <- list.files(in_dir, pattern = "\\.nc$", full.names = TRUE)

# ##
# writeRaster(
#   raster("Z:/1.Data/Results/climate/02_climate_change/pan_30s/ssp_245/access_cm2/2050s/prec_1.tif") * 0 + 1,
#   filename = "Z:/1.Data/Results/climate/00_admin_data/pan_msk_30s.tif",
#   format = "GTiff",
#   overwrite = TRUE
# )


var_names <- c(
  "pr"      = "prec",
  "tasmin"  = "tmin",
  "tasmax"  = "tmax",
  "rsds"    = "rsds",
  "hurs"    = "hurs",
  "sfcWind" = "wspd",
  "ETP"     = "etp"
)

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

for (f in files) {
  
  message("Processing: ", basename(f))
  
  r <- stack(f)
  nms <- names(r)
  
  # Detect variable from filename
  var_in <- names(var_names)[sapply(names(var_names), function(v) grepl(v, basename(f), ignore.case = FALSE))]
  
  if (length(var_in) == 0) {
    warning("Variable not recognized: ", basename(f))
    next
  }
  
  var_out <- var_names[var_in[1]]
  
  # Extract monthly layers
  idx <- match(months, nms)
  
  if (any(is.na(idx))) {
    warning("Missing monthly layers in: ", basename(f))
    next
  }
  
  r_months <- r[[idx]]
  
  # Write one tif per month
  for (m in 1:12) {

    rr <- r_months[[m]]

    # Ajustar a la máscara
    rr <- crop(rr, msk)
    rr <- mask(rr, msk)

    out_file <- file.path(
      in_dir,
      paste0(var_out, "_", m, ".tif")
    )

    writeRaster(
      rr,
      filename = out_file,
      format = "GTiff",
      overwrite = TRUE
    )

  }
}


#-------------------------------------------------------
# Calculate Tmean = (Tmax + Tmin) / 2
#-------------------------------------------------------

for (m in 1:12) {
  
  f_tmax <- file.path(in_dir, paste0("tmax_", m, ".tif"))
  f_tmin <- file.path(in_dir, paste0("tmin_", m, ".tif"))
  
  if (file.exists(f_tmax) & file.exists(f_tmin)) {
    
    tmax <- raster(f_tmax)
    tmin <- raster(f_tmin)
    
    tmean <- (tmax + tmin) / 2
    
    writeRaster(
      tmean,
      filename = file.path(in_dir, paste0("tmean_", m, ".tif")),
      format = "GTiff",
      overwrite = TRUE
    )
  }
}
