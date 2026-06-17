
selectBack <- function(occFile, outBackName, msk, backFilesDir) {
  require(raster)

  globZones <- raster(msk) * 0 +1
  spData <- read.csv(occFile)
  coords <- coordinates(globZones)
  coord_pres <- spData[,2:3]
  pos_pres <- cellFromXY(globZones, coord_pres)
  unos <- which(globZones[] == 1)
  
  pos_pres <- pos_pres[!is.na(pos_pres)]
  pos_unos_pres <- which(unos %in% pos_pres)
  
  coords_extrac <- coords[unos, ]
  
  if (length(pos_unos_pres) > 0) {
    coords_extrac <- coords_extrac[-pos_unos_pres, ]
  }
  
  n_back <- min(10000, nrow(coords_extrac))
  
  coords_final <- coords_extrac[
    sample(1:nrow(coords_extrac), n_back),
  ]
  
  coords_final <- data.frame(
    species = "background",
    longitude = coords_final[,1],
    latitude = coords_final[,2]
  )
  
  write.csv(
    coords_final,
    outBackName,
    quote = FALSE,
    row.names = FALSE
  )
  
}