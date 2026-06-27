library(terra)
library(data.table)

ecoDir <- "Z:/1.Data/Results/climate/03_ecosistems"
oDir <- file.path(ecoDir, "tables")
dir.create(oDir, recursive = TRUE, showWarnings = FALSE)

maskFile <- "Z:/1.Data/Process/Info_Inputs_SWAT/Panama/Tonosi_La_Villa/Division_administrativa/Tonosi_la_Villa_corregimientos.shp"

hz_files <- list.files(
  ecoDir,
  pattern = "^holdridge",
  full.names = TRUE
)

hz_files <- hz_files[!dir.exists(hz_files)]

print(hz_files)

holdridge_classes <- data.table(
  Holdridge = 1:12,
  Holdridge_name = c(
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
)

extract_holdridge_stats <- function(hz_file) {
  
  message("Procesando: ", basename(hz_file))
  
  hz <- rast(hz_file)
  hz <- round(hz)
  
  mask_adm1 <- vect(maskFile)
  mask_adm1 <- project(mask_adm1, crs(hz))
  mask_adm1$ZONE_ID <- 1:nrow(mask_adm1)
  
  adm_lookup <- data.table(
    ZONE_ID = mask_adm1$ZONE_ID,
    Provincia = mask_adm1$Provincia,
    Distrito = mask_adm1$Distrito,
    Corregimiento = mask_adm1$Corregimie
  )
  
  zones <- rasterize(
    mask_adm1,
    hz,
    field = "ZONE_ID",
    touches = TRUE
  )
  
  v <- as.data.table(values(c(hz, zones)))
  setnames(v, c("Holdridge", "ZONE_ID"))
  
  v <- v[!is.na(Holdridge) & !is.na(ZONE_ID)]
  
  if (nrow(v) == 0) {
    warning("Sin celdas válidas para: ", basename(hz_file))
    return(NULL)
  }
  
  out <- v[, .N, by = .(ZONE_ID, Holdridge)]
  setnames(out, "N", "n_cells")
  
  out[, pct_area := 100 * n_cells / sum(n_cells), by = ZONE_ID]
  
  out <- merge(out, adm_lookup, by = "ZONE_ID", all.x = TRUE)
  out <- merge(out, holdridge_classes, by = "Holdridge", all.x = TRUE)
  
  out[, Scenario := tools::file_path_sans_ext(basename(hz_file))]
  
  out[]
}

stats_all <- rbindlist(
  lapply(hz_files, extract_holdridge_stats),
  fill = TRUE
)

fwrite(
  stats_all,
  file.path(oDir, "holdridge_corregimientos_area_pct.csv")
)