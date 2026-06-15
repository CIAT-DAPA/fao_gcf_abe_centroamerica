# Author: Carlos Navarro
# UNIGIS 2022
# Purpose: Plot WorldClim v2.1 (baseline) in facet maps by months and seasons

#############################
#### 01 Plots by months  ####
#############################

# Load libraries
require(rasterVis)
require(maptools)
require(rgdal)
require(raster)
require(maptools)
require(latticeExtra)
library(terra)
library(sf)

# Set params
bDir <- "Z:/1.Data/Results/climate/01_baseline/pan/atlas_1981-2022_30s"
oDir <- "Z:/1.Data/Results/climate/01_baseline/pan/evaluation"
varList <- c("prec", "tmax", "tmin", "tmean")
id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
#mask <- readOGR("Z:/1.Data/Results/climate/00_admin_data/pan/Cuenca126.shp")
#mask_adm1 <- readOGR("S:/admin_boundaries/shp_files/CRI_adm/CRI1.shp")
crs_ref <- crs(raster(paste0(bDir, "/prec_1.tif")))

mask <- as(project(vect("Z:/1.Data/Process/Info_Inputs_SWAT/Panama/Tonosi_La_Villa/Division_administrativa/Tonosi_la_Villa_corregimientos.shp"), crs_ref), "Spatial")
mask_adm1 <- as(project(vect("Z:/1.Data/Process/Info_Inputs_SWAT/Panama/Tonosi_La_Villa/Division_administrativa/Tonosi_la_Villa_corregimientos.shp"), crs_ref), "Spatial")
#mask <- as(project(vect("Z:/1.Data/Results/climate/00_admin_data/pan/Cuenca126.shp"), crs_ref), "Spatial")
#mask_adm1 <- as(project(vect("Z:/1.Data/Results/climate/00_admin_data/pan/SubCuencas_126.shp"), crs_ref), "Spatial")

setwd(bDir)
if (!file.exists(oDir)) {dir.create(oDir)}


# ##Prepare files for database
# for (var in varList){
#   
#   stk_crop <- stack(paste0(bDir, "/", var, "_", 1:12, ".tif"))
#   stk_crop <- mask(crop(stk_crop, extent(mask)), mask)
#   
#   for(i in 1:12){
#     writeRaster(stk_crop[[i]], paste0(oDir, "/", var, "_", i, ".tif"))
#     
#   }
# }
# 
# id <- c("djf", "mam", "jja", "son", "ann")
# id_mod <-c("DEF", "MAM", "JJA", "SON", "ANUAL")
# 
# for (var in varList){
#   
#   for (s in id){
#     
#     stk_crop <- stack(paste0(bDir, "/", var, "_", s, ".tif"))
#     
#     if (s=="ann"){
#       stk_crop <- projectRaster(stk_crop, crs=raster(paste0(bDir, "/", var, "_1.tif")))
#       stk_crop <- resample(stk_crop, raster(paste0(oDir, "/", var, "_1.tif")))
#     }
#     
#     stk_crop <- mask(crop(stk_crop, extent(mask)), mask)
#     writeRaster(stk_crop, paste0(oDir, "/", var, "_", s, ".tif"))
#     
#   }
# 
# }



#############################
#### 01 Plots by months ####
#############################
id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

for (var in varList){
  
  stk_crop <- stack(paste0(bDir, "/", var, "_", 1:12, ".tif"))
  stk_crop <- mask(crop(stk_crop, extent(mask)), mask)

  if (var == "prec"){
    
    stk_crop[which(stk_crop[]>600)]=600
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
    
    zvalues <- seq(0, 600, 50) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("lightgoldenrodyellow", "skyblue1", "blue", "darkblue", "darkmagenta"))(length(zvalues)-1) # Set new colors
    myTheme$strip.border$col = "white" # Eliminate frame from maps
    myTheme$axis.line$col = 'white' # Eliminate frame from maps
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))
    
  } else if (var == "dtr"){
    
    stk_crop[which(stk_crop[] < -4 )]= -4
    stk_crop[which(stk_crop[] > 14 )]= 14
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
    
    zvalues <- seq(-4, 14, 2) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("darkblue", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    myTheme$strip.border$col = "white" # Eliminate frame from maps
    myTheme$axis.line$col = 'white' # Eliminate frame from maps
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))
    
  } else {
    
    # stk_crop <- stk_crop / 10
    stk_crop[which(stk_crop[] < 0 )]= 0
    stk_crop[which(stk_crop[] > 40 )]= 40
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
    zvalues <- seq(0, 40, 2)
    # zvalues <- c(-8, -4, 0, 4, 8, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 36)
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(c("darkblue", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    myTheme$strip.border$col = "white"
    myTheme$axis.line$col = 'white'
    # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  
    
  }
  
  # Save to file
  tiff(paste(oDir, "/plot_monthly_clim_", var, ".tif", sep=""), width=1600, height=800, pointsize=8, compression='lzw',res=200)
  
  print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), layout=c(6, 2), xlab="", ylab="", par.settings = myTheme, 
                  colorkey = list(space = "bottom", width=1.2, height=1)
                  ) 
        + layer(sp.polygons(mask_adm1, lwd=0.8))
        )
  
  dev.off()
  
} 


#############################
#### 02 Plots by seasons ####
#############################

# Load libraries
require(raster)
require(rasterVis)
require(maptools)
require(rgdal)
library(grid)

# Set params
id <- c("djf", "mam", "jja", "son") #, "ann")
id_mod <-c("DEF", "MAM", "JJA", "SON") #, "ANUAL")

for (var in varList){
  
  stk_crop <- stack(paste0(bDir, "/", var, "_", id, ".tif"))
  stk_crop <- mask(crop(stk_crop, extent(mask)), mask)
  
  if (var == "prec"){
    
    stk_crop[which(stk_crop[]>1600)]=1600
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- toupper(id_mod)
    
    zvalues <- seq(0, 1600, 100) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("khaki1", "skyblue1", "blue", "darkblue", "darkmagenta"))(length(zvalues)-1) # Set new colors
    myTheme$strip.border$col = "white" # Eliminate frame from maps
    myTheme$axis.line$col = 'white' # Eliminate frame from maps
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))
    
  } else if (var == "dtr"){
    
    stk_crop[which(stk_crop[] < -4 )]= -4
    stk_crop[which(stk_crop[] > 14 )]= 14
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- id_mod
    
    zvalues <- seq(-4, 14, 2) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("darkblue", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    myTheme$strip.border$col = "white" # Eliminate frame from maps
    myTheme$axis.line$col = 'white' # Eliminate frame from maps
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))
    
  } else {
    
    # stk_crop <- stk_crop / 10
    stk_crop[which(stk_crop[] < 0 )]= 0
    stk_crop[which(stk_crop[] > 40 )]= 40
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- id_mod
    zvalues <- seq(0, 40, 2)
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(c("darkblue", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    myTheme$strip.border$col = "white"
    myTheme$axis.line$col = 'white'
    # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))
    
  }
  
  tiff(paste(oDir, "/plot_seasonal_clim_", var, ".tif", sep=""), width=1400, height=600, pointsize=8, compression='lzw',res=200)
  
  print(levelplot(plot, at = zvalues, 
                  scales = list(draw=FALSE), 
                  #names.attr=rep("", length(id)), 
                  layout=c(4, 1),
                  xlab="",
                  ylab="",
                  par.settings = myTheme,
                  # margin=F,
                  colorkey = list(space = "bottom", width=1.2, height=1, labels=list(cex=1)))
        + layer(sp.polygons(mask_adm1, lwd=0.8))
        )
   
  
  dev.off()
  
} 


##########################
#### 03 Annual Plots  ####
##########################

id <- c("ANN")
id_mod <-c("ANUAL")

setwd(bDir)
if (!file.exists(oDir)) {dir.create(oDir)}

for (var in varList){
  
  stk_crop <- stack(paste0(bDir, "/", var, "_", id, ".tif"))
  stk_crop <- mask(crop(stk_crop, extent(mask)), mask)
  
  if (var == "prec"){
    
    stk_crop[which(stk_crop[]>3000)]=3000
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- toupper(id_mod)
    
    zvalues <- seq(0, 3000, 200) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("khaki1", "skyblue1", "blue", "darkblue", "darkmagenta"))(length(zvalues)-1) # Set new colors
    myTheme$strip.border$col = "white" # Eliminate frame from maps
    myTheme$axis.line$col = 'white' # Eliminate frame from maps
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))  
    
    
  } else if (var == "dtr"){
    
    stk_crop[which(stk_crop[] < -4 )]= -4
    stk_crop[which(stk_crop[] > 14 )]= 14
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
    
    zvalues <- seq(-4, 14, 2) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("darkblue", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    myTheme$strip.border$col = "white" # Eliminate frame from maps
    myTheme$axis.line$col = 'white' # Eliminate frame from maps
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))
    
  } else {
    
    stk_crop[which(stk_crop[] < 0 )]= 0
    stk_crop[which(stk_crop[] > 40 )]= 40
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
    zvalues <- seq(0, 40, 2)
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(c("darkblue", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    myTheme$strip.border$col = "white"
    myTheme$axis.line$col = 'white'
    # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  
    
  }
  
  tiff(paste(oDir, "/plot_annual_clim_", var, ".tif", sep=""), width=1000, height=1200, pointsize=8, compression='lzw',res=200)
  
  print(levelplot(plot, at = zvalues, 
                  scales = list(draw=FALSE), 
                  # layout=c(2, 2), 
                  xlab="", 
                  ylab="", 
                  par.settings = myTheme, 
                  margin=F,
                  colorkey = list(space = "bottom", width=1.2, height=1, labels=list(cex=1.2)))
        + layer(sp.polygons(mask, lwd=0.8))
  )
  
  dev.off()
  
} 

