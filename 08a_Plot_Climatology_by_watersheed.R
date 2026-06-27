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
library(grid)

# Set params
bDir <- "Z:/1.Data/Results/climate/01_baseline/dom/wcl_v21_2_5min"
oDir <- "Z:/1.Data/Results/climate/01_baseline/dom/evaluation_watersheed"
varList <- c("tmax", "tmin", "tmean", "prec")
# varList <- c("etp", "hurs", "rsds", "wspd") 
id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
seasons <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)
crs_ref <- crs(raster(paste0(bDir, "/prec_1.tif")))
#mask <- readOGR("Z:/1.Data/Results/climate/00_admin_data/pan/Cuenca126.shp")
#mask_adm1 <- readOGR("S:/admin_boundaries/shp_files/CRI_adm/CRI1.shp")
mask <- as(project(vect("Z:/1.Data/Results/climate/00_admin_data/gadm41_DOM_0.shx"), crs_ref), "Spatial")
mask_adm1 <- as(project(vect("Z:/1.Data/Results/climate/00_admin_data/gadm41_DOM_1.shx"), crs_ref), "Spatial")
# mask <- as(project(vect("Z:/1.Data/Results/climate/00_admin_data/dom/guayubin_mao_wgs84.shp"), crs_ref), "Spatial")
# mask_adm1 <- as(project(vect("Z:/1.Data/Results/climate/00_admin_data/dom/guayubin_mao_wgs84.shp"), crs_ref), "Spatial")
# mask_rs <- "Z:/1.Data/Results/climate/00_admin_data/dom/guayubin_mao_wgs84.tif"

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
# for (var in varList){  
#   
#   # Loop throught seasons
#   for (i in 1:length(seasons)){
#     
#     #baseline 
#     if (!file.exists(paste(bDir,'/', var, "_", names(seasons[i]), '.tif',sep=''))){
#       
#       cat("Seasonal calcs baseline \n")
#       
#       # Load averages files 
#       iAvg <- stack(paste(bDir,'/', var, "_", 1:12, ".tif",sep=''))
#       
#       if (var == "prec"){
#         sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){sum(x,na.rm=any(!is.na(x)))})
#       } else {
#         sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
#       }
#       writeRaster(sAvg, paste(bDir,'/', var, "_", names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T)
#     }
#   }
# }

#############################
#### 01 Plots by months #####
#############################

for (var in varList){
  
  stk_crop <- stack(paste0(bDir, "/", var, "_", 1:12, ".tif"))
  # stk_crop <- resample(stk_crop, raster(mask_rs), method="bilinear")
  stk_crop <- mask(crop(stk_crop, extent(mask)))

  if (var == "prec"){
    
    stk_crop[which(stk_crop[]>600)]=600
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
    
    zvalues <- seq(0, 600, 50) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("lightgoldenrodyellow", "skyblue1", "blue", "darkblue", "darkmagenta"))(length(zvalues)-1) # Set new colors
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))
    
  } else if (var == "dtr"){
    
    stk_crop[which(stk_crop[] < -4 )]= -4
    stk_crop[which(stk_crop[] > 14 )]= 14
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
    
    zvalues <- seq(-4, 14, 2) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("darkblue", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))

  } else if (var == "rsds"){
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
    
    zvalues <- seq(120, 280, by = 10) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme=rasterTheme(region=brewer.pal('Oranges', n=9))  
    
  } else if (var == "etp"){
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
    
    zvalues <- seq(100, 200, by = 10) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme=rasterTheme(region=brewer.pal('GnBu', n=9))   
    
  } else if (var == "hurs"){
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
    
    zvalues <- seq(60, 100, by = 5) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme=rasterTheme(region=brewer.pal('PuBuGn', n=9))

  } else if (var == "wspd"){
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
    
    zvalues <- seq(0, 12, by = 1)
    myTheme <- BuRdTheme()
    myTheme=rasterTheme(region=brewer.pal('Greens', n=9))  
    
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
    # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  
    
  }
  
  myTheme$strip.border$col = "white" # Eliminate frame from maps
  myTheme$axis.line$col = 'white' # Eliminate frame from maps
  
  # Save to file
  tiff(paste(oDir, "/plot_monthly_clim_", var, ".tif", sep=""), width=2000, height=1200, pointsize=8, compression='lzw',res=200)
  
  print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), layout=c(4, 3), xlab="", ylab="", par.settings = myTheme, 
                  colorkey = list(space = "bottom", width=1.2, height=1)
                  ) 
        + layer(sp.polygons(mask_adm1, lwd=0.8))
        )
  
  dev.off()
  
} 


#############################
#### 02 Plots by seasons ####
#############################

# Set params
id <- c("djf", "mam", "jja", "son", "ann") ##if annual, include 5x1 plots in levelplot and increase width
id_mod <-c("DEF", "MAM", "JJA", "SON", "ANUAL")

for (var in varList){
  
  stk_crop <- stack(paste0(bDir, "/", var, "_", id, ".tif"))
  # stk_crop <- resample(stk_crop, raster(mask_rs), method="bilinear")
  stk_crop <- mask(crop(stk_crop, extent(mask)))
  
  if (var == "prec"){
    
    stk_crop[which(stk_crop[]>2400)]=2400
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- toupper(id_mod)
    
    zvalues <- seq(0, 2400, 100) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("khaki1", "skyblue1", "blue", "darkblue", "darkmagenta"))(length(zvalues)-1) # Set new colors
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))
    
  } else if (var == "dtr"){
    
    stk_crop[which(stk_crop[] < -4 )]= -4
    stk_crop[which(stk_crop[] > 14 )]= 14
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- id_mod
    
    zvalues <- seq(-4, 14, 2) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("darkblue", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))
    
  } else if (var == "rsds"){
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- id_mod
    
    zvalues <- seq(120, 280, by = 10) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme=rasterTheme(region=brewer.pal('Oranges', n=9))  
    
  } else if (var == "etp"){
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- id_mod
    
    zvalues <- seq(100, 200, by = 10) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme=rasterTheme(region=brewer.pal('GnBu', n=9))   
    
  } else if (var == "hurs"){
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- id_mod
    
    zvalues <- seq(60, 100, by = 5) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme=rasterTheme(region=brewer.pal('PuBuGn', n=9))
    
  } else if (var == "wspd"){
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- id_mod
    
    zvalues <- seq(0, 12, by = 1)
    myTheme <- BuRdTheme()
    myTheme=rasterTheme(region=brewer.pal('Greens', n=9))  
    
  } else {
    
    # stk_crop <- stk_crop / 10
    stk_crop[which(stk_crop[] < 0 )]= 0
    stk_crop[which(stk_crop[] > 40 )]= 40
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- id_mod
    zvalues <- seq(0, 40, 2)
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(c("darkblue", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))
    
  }
  
  myTheme$strip.border$col = "white" # Eliminate frame from maps
  myTheme$axis.line$col = 'white' # Eliminate frame from maps
    
  tiff(paste(oDir, "/plot_seasonal_clim_", var, "_v2.tif", sep=""), width=2400, height=800, pointsize=8, compression='lzw',res=200)
  
  print(levelplot(plot, at = zvalues, 
                  scales = list(draw=FALSE), 
                  #names.attr=rep("", length(id)), 
                  layout=c(5, 1),
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
  
  plot <- setZ(stk_crop, id)
  names(plot) <- id
  
  if (var == "prec"){
    
    stk_crop[which(stk_crop[]>2500)]=2500
    
    zvalues <- seq(0, 2500, 100) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("khaki1", "skyblue1", "blue", "darkblue", "darkmagenta"))(length(zvalues)-1) # Set new colors
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))  

  } else if (var == "dtr"){
    
    stk_crop[which(stk_crop[] < -4 )]= -4
    stk_crop[which(stk_crop[] > 14 )]= 14
    
    zvalues <- seq(-4, 14, 2) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("darkblue", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))
    
  } else if (var == "rsds"){
    
    zvalues <- seq(120, 280, by = 10) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme=rasterTheme(region=brewer.pal('Oranges', n=9))  
    
  } else if (var == "etp"){
    
    zvalues <- seq(100, 200, by = 10) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme=rasterTheme(region=brewer.pal('GnBu', n=9))   
    
  } else if (var == "hurs"){
    
    zvalues <- seq(60, 100, by = 5) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme=rasterTheme(region=brewer.pal('PuBuGn', n=9))
    
  } else if (var == "wspd"){

    zvalues <- seq(0, 12, by = 1)
    myTheme <- BuRdTheme()
    myTheme=rasterTheme(region=brewer.pal('Greens', n=9))  
    
  } else {
    
    stk_crop[which(stk_crop[] < 0 )]= 0
    stk_crop[which(stk_crop[] > 40 )]= 40

    zvalues <- seq(0, 40, 2)
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(c("darkblue", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  
    
  }
  
  myTheme$strip.border$col = "white" # Eliminate frame from maps
  myTheme$axis.line$col = 'white' # Eliminate frame from maps
  
  tiff(paste(oDir, "/plot_annual_clim_", var, ".tif", sep=""), width=500, height=600, pointsize=8, compression='lzw',res=200)
  
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

