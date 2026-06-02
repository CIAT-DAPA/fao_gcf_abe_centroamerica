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

# Set params
bDir <- "D:/cenavarro/msc_gis_thesis/01_baseline/wcl_v21_30s"
oDir <- "D:/cenavarro/bid_mag_pasar/climate_change/baseline"
varList <- c("prec", "tmax", "tmin", "tmean")
id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
#mask <- readOGR("D:/cenavarro/msc_gis_thesis/00_admin_data/CAMEXCA_adm0.shp")
mask <- readOGR("S:/admin_boundaries/shp_files/CRI_adm/CRI0.shp")
mask_adm1 <- readOGR("S:/admin_boundaries/shp_files/CRI_adm/CRI1.shp")

setwd(bDir)
if (!file.exists(oDir)) {dir.create(oDir)}


##Prepare files for database
for (var in varList){
  
  stk_crop <- stack(paste0(bDir, "/", var, "_", 1:12, ".tif"))
  stk_crop <- mask(crop(stk_crop, extent(mask)), mask)
  
  for(i in 1:12){
    writeRaster(stk_crop[[i]], paste0(oDir, "/", var, "_", i, ".tif"))
    
  }
}

id <- c("djf", "mam", "jja", "son", "ann")
id_mod <-c("DEF", "MAM", "JJA", "SON", "ANUAL")

for (var in varList){
  
  for (s in id){
    
    stk_crop <- stack(paste0(bDir, "/", var, "_", s, ".tif"))
    
    if (s=="ann"){
      stk_crop <- projectRaster(stk_crop, crs=raster(paste0(bDir, "/", var, "_1.tif")))
      stk_crop <- resample(stk_crop, raster(paste0(oDir, "/", var, "_1.tif")))
    }
    
    stk_crop <- mask(crop(stk_crop, extent(mask)), mask)
    writeRaster(stk_crop, paste0(oDir, "/", var, "_", s, ".tif"))
    
  }

}



#############################
#### 01 Plots by months ####
#############################
id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

for (var in varList){
  
  stk_crop <- stack(paste0(oDir, "/", var, "_", 1:12, ".tif"))
  stk_crop <- mask(crop(stk_crop, extent(mask)), mask)

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
  tiff(paste(oDir, "/_plot/plot_monthly_clim_", var, ".tif", sep=""), width=1600, height=800, pointsize=8, compression='lzw',res=200)
  
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
id <- c("djf", "mam", "jja", "son", "ann")
id_mod <-c("DEF", "MAM", "JJA", "SON", "ANUAL")

for (var in varList){
  
  stk_crop <- stack(paste0(bDir, "/", var, "_", id, ".tif"))
  stk_crop <- mask(crop(stk_crop, extent(mask)), mask)
  
  if (var == "prec"){
    
    stk_crop[which(stk_crop[]>6000)]=6000
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- toupper(id_mod)
    
    zvalues <- seq(0, 6000, 100) # Define limits
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
  
  tiff(paste(oDir, "/plot_seasonal_clim_", var, "_and_annual.tif", sep=""), width=1800, height=600, pointsize=8, compression='lzw',res=200)
  
  print(levelplot(plot, at = zvalues, 
                  scales = list(draw=FALSE), 
                  names.attr=rep("", length(id)), 
                  layout=c(5, 1), 
                  main=list(paste(c("         DEF", "       MAM", "     JJA", 
                                    "       SON", "        ANUAL"), 
                                  sep=""),side=1,line=0.5, cex=0.8),
                  xlab="", 
                  ylab="", 
                  par.strip.text=list(cex=0),
                  par.settings = myTheme, 
                  # margin=F,
                  colorkey = list(space = "bottom", width=1.2, height=1))
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
    
    stk_crop[which(stk_crop[]>4000)]=4000
    
    plot <- setZ(stk_crop, id_mod)
    names(plot) <- toupper(id_mod)
    
    zvalues <- seq(0, 4000, 200) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("khaki1", "skyblue1", "blue", "darkblue", "darkmagenta"))(length(zvalues)-1) # Set new colors
    myTheme$strip.border$col = "white" # Eliminate frame from maps
    myTheme$axis.line$col = 'white' # Eliminate frame from maps
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))  
    
    
  } else {
    
    stk_crop[which(stk_crop[] < -5 )] = -5
    stk_crop[which(stk_crop[] > 45 )] = 45
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
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
        + layer(sp.polygons(mask, lwd=0.8))
  )
  
  dev.off()
  
} 



## Seasonal calcs and Tmean calcuation
seasons <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)

for (var in varList){
  
  # Seasonal calcs
  iAvg <- stack(paste(bDir,'/', var, "_", 1:12, ".tif",sep=''))
  # Loop throught seasons
  for (i in 1:length(seasons)){
      cat("Calcs ", var, names(seasons[i]), "\n")
      if (var == "prec"){
        sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){sum(x,na.rm=any(!is.na(x)))})
      } else {
        sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
      }
      writeRaster(sAvg, paste(bDir,'/', var, "_", names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T)
    }
  }
  
  
  ## Tmean calculation
  
  for (mth in 1:12){
    if (!file.exists(paste(bDir, "/tmean_", mth,".tif", sep=""))) {
      tmin <- raster(paste0(bDir, "/tmin_",mth, ".tif"))
      tmax <- raster(paste0(bDir, "/tmax_",mth, ".tif"))
      tmean <- (tmax + tmin)/2
      writeRaster(tmean, paste0(bDir, "/tmean_",mth, ".tif"), format="GTiff", overwrite=T)
    }
  }
  
  
  # Seasonal calcs Tmean
  iAvg <- stack(paste(bDir,'/', "tmean", "_", 1:12, ".tif",sep=''))
  
  # Loop throught seasons
  for (i in 1:length(seasons)){
    if (!file.exists(paste(bDir,'/', "tmean", "_", names(seasons[i]), '.tif',sep=''))){
      cat("Calcs ", "tmean", names(seasons[i]), "\n")
      sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
      writeRaster(sAvg, paste(bDir,'/', "tmean", "_", names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T)
    }
  }
