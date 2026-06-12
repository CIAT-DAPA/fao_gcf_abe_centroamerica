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