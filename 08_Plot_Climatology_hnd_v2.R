# Author: Carlos Navarro
# UNIGIS 2022
# Purpose: Plot WorldClim v2.1 (baseline) in facet maps by months and seasons

# Load libraries
require(rasterVis)
require(raster)
require(latticeExtra)
library(sf)
library(sp)
library(grid)

# Set params
bDir <- "X:/1.Data/Results/climate/01_baseline/hnd/average_v2"
oDir <- "X:/1.Data/Results/climate/01_baseline/hnd/evaluation_watersheed"
varList <- c("prec", "tmax", "tmin", "tmean")
id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
maskFile <- "X:/1.Data/Results/climate/00_admin_data/hnd/basin1.shp"

mask <- as(
  st_read(maskFile, quiet = TRUE),
  "Spatial"
)

####################################################
## Spatial preprocessing, Crop, Mask, Reproject if needed
####################################################

prep_map <- function(r){
  
  shp <- mask
  
  if(!compareCRS(r, shp)){
    shp <- spTransform(shp, crs(r))
  }
  
  r <- crop(r, extent(shp))
  r <- mask(r, shp)
  
  list(
    raster = r,
    mask = shp
  )
  
}

setwd(bDir)
if (!file.exists(oDir)) {dir.create(oDir)}


############################
#### 01 Plots by months  ####
#############################

for (var in varList){
  
  stk <- stack(paste0(bDir, "/", var, "_", 1:12, ".tif"))
  
  mp <- prep_map(stk)
  stk_crop <- mp$raster
  
  if (var == "prec"){
    
    stk_crop[which(stk_crop[]>800)]=800
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
    
    zvalues <- seq(0, 800, 50) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("lightgoldenrodyellow", "skyblue1", "blue", "darkblue", "darkmagenta"))(length(zvalues)-1) # Set new colors
    myTheme$strip.border$col = "white" # Eliminate frame from maps
    myTheme$axis.line$col = 'white' # Eliminate frame from maps
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))
    
  } else {
    
    # stk_crop <- stk_crop / 10
    stk_crop[which(stk_crop[] < -5 )]= -5
    stk_crop[which(stk_crop[] > 45 )]= 45
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
    zvalues <- seq(-5, 45, 5)
    # zvalues <- c(-8, -4, 0, 4, 8, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 36)
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(c("darkblue", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    myTheme$strip.border$col = "white"
    myTheme$axis.line$col = 'white'
    # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  
    
  }
  
  # Save to file
  tiff(paste(oDir, "/plot_monthly_clim_", var, ".tif", sep=""), width=2000, height=1200, pointsize=8, compression='lzw',res=200)
  
  print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), layout=c(4, 3), xlab="", ylab="", par.settings = myTheme, 
                  colorkey = list(space = "bottom", width=1.2, height=1)
                  ) 
        + layer(sp.polygons(mp$mask, lwd=0.8))
        )
  
  dev.off()
  
} 


#############################
#### 02 Plots by seasons ####
#############################

# Set params
id <- c("djf","mam","jja","son","ann")
id_mod <- c("DEF","MAM","JJA","SON","ANUAL")

for (var in varList){
  
  stk <- stack(paste0(bDir, "/", var, "_", id, ".tif"))
  
  mp <- prep_map(stk)
  stk_crop <- mp$raster
  
  if (var == "prec"){
    
    stk_crop[which(stk_crop[]>2000)]=2000
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- toupper(id_mod)
    
    zvalues <- seq(0, 2000, 100) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("khaki1", "skyblue1", "blue", "darkblue", "darkmagenta"))(length(zvalues)-1) # Set new colors
    myTheme$strip.border$col = "white" # Eliminate frame from maps
    myTheme$axis.line$col = 'white' # Eliminate frame from maps
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))  
    
    
    
  } else {
    
    stk_crop[which(stk_crop[]< -5 )]= -5
    stk_crop[which(stk_crop[]> 45 )]= 45
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- id_mod
    zvalues <- seq(-5, 45, 5)
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(c("darkblue", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    myTheme$strip.border$col = "white"
    myTheme$axis.line$col = 'white'
    # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  
    
  }
  
  # Version panel 2x2
  
  
  # version original (panel 2x2)
  
  tiff(
    paste(oDir, "/plot_seasonal_clim_", var, ".tif", sep=""),
    width=1800,
    height=1300,
    pointsize=8,
    compression="lzw",
    res=200
  )
  
  print(
    levelplot(
      plot[[1:4]],
      at = zvalues,
      scales = list(draw=FALSE),
      layout = c(2,2),
      xlab="",
      ylab="",
      par.settings = myTheme,
      colorkey = list(
        space="bottom",
        width=1.2,
        height=1,
        labels=list(cex=1.2)
      )
    ) +
      layer(sp.polygons(mp$mask, lwd=0.8))
  )
  
  dev.off()
  
  # Version panel 5x1
  
  tiff(paste(oDir, "/plot_seasonal_clim_", var, "_v2.tif", sep=""), width=2600, height=700, pointsize=8, compression='lzw',res=200)
  
  print(levelplot(plot, at = zvalues, 
                  scales = list(draw=FALSE), 
                  layout=c(5, 1),
                  xlab="", 
                  ylab="", 
                  par.settings = myTheme, 
                  # margin=F,
                  colorkey = list(space = "bottom", width=1.2, height=1, labels=list(cex=1.2)))
        + layer(sp.polygons(mp$mask))
        )
   
  dev.off()
  
} 


##########################
#### 03 Annual Plots  ####
##########################

id <- c("ann")
id_mod <-c("ANUAL")

for (var in varList){
  
  stk <- stack(paste0(bDir, "/", var, "_", id, ".tif"))
  
  mp <- prep_map(stk)
  stk_crop <- mp$raster
  
  if (var == "prec"){
    
    stk_crop[which(stk_crop[]>5000)]=5000
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- toupper(id_mod)
    
    zvalues <- seq(0, 5000, 200) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("khaki1", "skyblue1", "blue", "darkblue", "darkmagenta"))(length(zvalues)-1) # Set new colors
    myTheme$strip.border$col = "white" # Eliminate frame from maps
    myTheme$axis.line$col = 'white' # Eliminate frame from maps
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))  
    
    
  } else {
    
    stk_crop[which(stk_crop[] < -5 )] = -5
    stk_crop[which(stk_crop[] > 45 )] = 45
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- id_mod
    zvalues <- seq(-5, 45, 5)
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(c("darkblue", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    myTheme$strip.border$col = "white"
    myTheme$axis.line$col = 'white'
    # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  
    
  }
  
  tiff(paste(oDir, "/plot_annual_clim_", var, ".tif", sep=""), width=1800, height=1200, pointsize=8, compression='lzw',res=200)
  
  print(levelplot(plot, at = zvalues, 
                  scales = list(draw=FALSE), 
                  # layout=c(2, 2), 
                  xlab="", 
                  ylab="", 
                  par.settings = myTheme, 
                  margin=F,
                  colorkey = list(space = "bottom", width=1.2, height=1, labels=list(cex=1.2)))
        + layer(sp.polygons(mp$mask))
  )
  
  dev.off()
  
} 
