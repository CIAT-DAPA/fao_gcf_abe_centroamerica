library(dplyr)
library(readr)
library(raster)
library(data.table)
library(tidyr)

setwd("C:/_scripts/fao_gcf_abe_centroamerica/maxent")
source("000.createChullBuffer.R")
source("002.selectBackgroundArea_mod.R")

# -----------------------------
# Inputs
# -----------------------------
inputDir <- "Z:/1.Data/Results/climate/04_species"
iDir <- "Z:/1.Data/Results/climate/04_species/occurrence_files"
iDir_back <- "Z:/1.Data/Results/climate/04_species/background_selection"
NADir <- "Z:/1.Data/Results/climate/04_species/native-areas/asciigrid"
dem_file <- "Z:/1.Data/Results/climate/04_species/masks/hnd_dem.tif"
bio_dir  <- "Z:/1.Data/Results/climate/01_baseline/hnd/average_v2"  
backFilesDir <- "Z:/1.Data/Results/climate/04_species/background_selection"
msk <- "Z:/1.Data/Results/climate/04_species/masks/hnd_mask.asc"
country <- "hnd"

#specie = "ceroxylon_quindiuense" 
speciesList <- c(
  "alouatta_palliata",
  "amazona_autumnalis",
  "ceiba_pentandra",
  "enterolobium_cyclocarpum",
  "liquidambar_styraciflua",
  "pharomachrus_mocinno"
)
buffDist <- 50000


for(specie in speciesList){
  
  occ_file <- paste0(iDir, "/", specie, ".csv")
  # -----------------------------
  # 1. Tabla simple: species, longitude, latitude
  # -----------------------------
  occ <- fread(occ_file)
  occ_simple <- occ %>%
    transmute(
      species = species,
      lon = decimalLongitude,
      lat = decimalLatitude,
      type = "pres"
    ) %>%
    filter(
      !is.na(lon),
      !is.na(lat)
    ) %>%
    distinct()
  write_csv(occ_simple, paste0(iDir, "/", specie, "_pres.csv"))
  
  # -----------------------------
  # Convertir a SpatialPoints
  # -----------------------------
  pts <- occ_simple
  coordinates(pts) <- ~ lon + lat
  proj4string(pts) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  # -----------------------------
  # 2. Extraer altitud desde DEM
  # -----------------------------
  dem <- raster(dem_file)
  # reproyectar puntos si DEM tiene otro CRS
  if (!compareCRS(pts, dem)) {
    pts_dem <- spTransform(pts, CRS(projection(dem)))
  } else {
    pts_dem <- pts
  }
  occ_alt <- occ_simple %>%
    mutate(
      altitude = raster::extract(dem, pts_dem)
    )
  write_csv(occ_alt, paste0(iDir, "/", specie, "_alt.csv"))
  
  
  # -----------------------------
  # 3. Extraer bioclimáticos actuales
  # -----------------------------
  bio_files <- list.files(
    bio_dir,
    pattern = "^bio_[0-9]+\\.asc$",
    full.names = TRUE
  )
  bio_stack <- stack(bio_files)
  # ordenar por bio1, bio2, ..., bio19 si aplica
  bio_files <- bio_files[order(as.numeric(gsub("\\D", "", basename(bio_files))))]
  bio_stack <- stack(bio_files)
  names(bio_stack) <- paste0("bio_", 1:nlayers(bio_stack))
  # reproyectar puntos si bioclimáticos tienen otro CRS
  if (!compareCRS(pts, bio_stack)) {
    pts_bio <- spTransform(pts, CRS(projection(bio_stack)))
  } else {
    pts_bio <- pts
  }
  bio_vals <- raster::extract(bio_stack, pts_bio)
  occ_bio <- bind_cols(
    occ_alt,
    as.data.frame(round(bio_vals, digits = 1))
  ) %>%
    drop_na()
  
  write_csv(occ_bio, paste0(iDir, "/", specie, "_swd.csv"))
  
  otp <- chullBuffer(inputDir, paste(iDir, "/", specie, "_swd.csv", sep=""), paste(NADir, "/", specie, sep=""), buffDist, country)
  otp <- selectBack(paste(iDir, "/", specie, "_swd.csv", sep=""), paste0(backFilesDir, "/", specie, "_background.csv"), msk, backFilesDir)
  
  # -----------------------------
  # 4. Extraer bioclimáticos background
  # -----------------------------
  bio_files <- list.files(
    bio_dir,
    pattern = "^bio_[0-9]+\\.asc$",
    full.names = TRUE
  )
  # leer CSV de background
  back_file <- paste0(iDir_back, "/", specie, "_background.csv")
  pts_df <- read_csv(back_file, show_col_types = FALSE)
  pts_df <- pts_df %>%
    filter(!is.na(longitude), !is.na(latitude))
  
  pts <- pts_df
  coordinates(pts) <- ~ longitude + latitude
  proj4string(pts) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  bio_stack <- stack(bio_files)
  # ordenar por bio1, bio2, ..., bio19 si aplica
  bio_files <- bio_files[order(as.numeric(gsub("\\D", "", basename(bio_files))))]
  bio_stack <- stack(bio_files)
  names(bio_stack) <- paste0("bio_", 1:nlayers(bio_stack))
  
  # reproyectar puntos si bioclimáticos tienen otro CRS
  if (!compareCRS(pts, bio_stack)) {
    pts_bio <- spTransform(pts, CRS(projection(bio_stack)))
  } else {
    pts_bio <- pts
  }
  bio_vals <- raster::extract(bio_stack, pts_bio)
  
  # unir background + valores bio
  back_bio <- bind_cols(
    pts_df,
    as.data.frame(round(bio_vals, digits = 1))
  ) %>%
    drop_na()
  
  write_csv(back_bio, paste0(iDir_back, "/", specie, "_background_swd.csv"))
  
}


