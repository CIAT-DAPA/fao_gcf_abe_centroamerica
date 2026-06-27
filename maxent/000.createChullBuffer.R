require(raster)

require(sp)

require(geosphere)

source("000.zipWrite.R")

chullBuffer <- function(inDir, occFile, outFolder, buffDist, country) {
  
  
  
  if (!file.exists(outFolder)) {
    
    dir.create(outFolder, recursive = TRUE)
    
  }
  
  
  
  # Reading occurrences
  
  cat("Reading occurrences \n")
  
  occ <- read.csv(occFile)
  
  
  
  # Asegurar nombres lon/lat
  
  names(occ) <- tolower(names(occ))
  
  
  
  if (!all(c("lon", "lat") %in% names(occ))) {
    
    stop("occFile must contain columns named lon and lat")
    
  }
  
  
  
  occ <- occ[!is.na(occ$lon) & !is.na(occ$lat), ]
  
  
  
  # Create convex hull
  
  cat("Creating the convex hull \n")
  
  ch <- occ[chull(cbind(occ$lon, occ$lat)), c("lon", "lat")]
  
  chClosed <- rbind(ch, ch[1, ])
  
  
  
  # Buffer convex hull vertices
  
  cat("Buffering the convex hull \n")
  
  
  
  bearings <- 0:359
  
  
  
  buff_pts <- do.call(rbind, lapply(1:nrow(ch), function(i) {
    
    
    
    pts <- geosphere::destPoint(
      
      p = c(ch$lon[i], ch$lat[i]),
      
      b = bearings,
      
      d = buffDist
      
    )
    
    
    
    data.frame(
      
      lon = pts[, 1],
      
      lat = pts[, 2]
      
    )
    
  }))
  
  
  
  # Convex hull of buffered points
  
  hull_id <- chull(buff_pts$lon, buff_pts$lat)
  
  hull_buff <- buff_pts[hull_id, ]
  
  hull_buff <- rbind(hull_buff, hull_buff[1, ])
  
  
  
  # Transform to polygon and raster
  
  cat("Transforming to polygons \n")
  
  
  
  msk <- raster(file.path(inDir, "masks", paste0(country, "_mask.asc")))
  
  
  
  pol <- SpatialPolygons(
    
    list(
      
      Polygons(
        
        list(Polygon(as.matrix(hull_buff[, c("lon", "lat")]))),
        
        ID = "1"
        
      )
      
    ),
    
    proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs")
    
  )
  
  
  
  # Reproject polygon if needed
  
  if (!is.na(projection(msk)) && !compareCRS(pol, msk)) {
    
    pol <- spTransform(pol, CRS(projection(msk)))
    
  }
  
  
  
  pa <- rasterize(pol, msk, field = 1)
  
  
  
  cat("Final calculations \n")
  
  
  
  pa[!is.na(pa[])] <- 1
  
  pa[is.na(pa[]) & msk[] == 1] <- 0
  
  pa[is.na(msk[])] <- NA
  
  
  
  cat("Writing output \n")
  
  
  
  paName <- zipWrite(pa, outFolder, "narea.asc.gz")
  
  
  
  return(pa)
  
}