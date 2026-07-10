# Carlos Navarro 
# Purpose: Plot anomalies from CMIP6 data

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
library(RColorBrewer)

sspList <- c("ssp_245", "ssp_585") #"ssp_126", "ssp_370"
#sspList <- c("ssp_585")
baseDir <- "Z:/1.Data/Results/climate/02_climate_change/hnd_watershed_30s_anom_ens"
perList <- c("2050s", "2070s") #, "2090s") #,"2030s", )
varList <- c("prec", "tmin", "tmax", "tmean") 
id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
crs_ref <- crs(raster("Z:/1.Data/Results/climate/02_climate_change/hnd_watershed_30s_anom_ens/ssp_245/2050s/prec_1_avg.tif"))
mask <- as(project(vect("Z:/1.Data/Process/Info_Inputs_SWAT/Honduras/Choluteca/Division_Administrativa/Choluteca_adm2.shp"), crs_ref), "Spatial")
mask_adm1 <- as(project(vect("Z:/1.Data/Process/Info_Inputs_SWAT/Honduras/Choluteca/Division_Administrativa/Choluteca_adm2.shp"), crs_ref), "Spatial")
mask_rs <- "Z:/1.Data/Results/climate/00_admin_data/hnd/hnd_watersheed_msk_30s"
resample <- F
oDir <- "Z:/1.Data/Results/climate/02_climate_change/hnd_evaluations"
metrics <- c("avg") #, "q25", "q75")
suffix <- "ws"
if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}


######################################
#### 01 Plots anomalies by months ####
######################################

for (ssp in sspList) {
  
  for (per in perList){
    
    for (var in varList){
      
      ### Average, q25, q75 pots
      
      for (metric in metrics){
        
        stk <- stack(paste0(baseDir, "/", ssp, "/", per, "/", var, "_", 1:12, "_", metric, ".tif"))
        stk <- mask(crop(stk, extent(mask)), mask)
        if(resample == T){stk <- resample(stk, raster(mask_rs), method="bilinear")}
        stk <- mask(crop(stk, extent(mask)), mask)
        
        oPlot <- paste(oDir, "/plot_monthly_anom_", var, "_", ssp, "_ensemble_", per, "_", metric, "_", suffix, ".tif", sep="")
        
        if (!file.exists(oPlot)) {
          
          if (var == "prec"){
            
            stk_crop <- stk * 100
            stk_crop[stk_crop > 60] = 60
            stk_crop[stk_crop < (-60)] = (-60)
            
            plot <- setZ(stk_crop, id)
            names(plot) <- id
            
            zvalues <- seq(-60, 60, 5) # Define limits
            myTheme <- BuRdTheme() # Define squeme of colors
            # myTheme$regions$col=colorRampPalette(c("#b2182b", "#d6604d", "#f4a582", "#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac", "#053061"))(length(zvalues)-1) # Set new colors
            myTheme <- rasterTheme(region=brewer.pal('RdBu', n=9))
            myTheme$strip.border$col = "white" # Eliminate frame from maps
            myTheme$axis.line$col = 'white' # Eliminate frame from maps
            
          } else {
            
            stk_crop <- stk
            stk_crop[stk_crop > 5 ] = 5
            stk_crop[stk_crop < 0 ] = 0
            
            
            plot <- setZ(stk_crop, id)
            names(plot) <- id
            
            zvalues <- seq(0, 5, 0.2)
            myTheme <- BuRdTheme()
            myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))
            myTheme$strip.border$col = "white"
            myTheme$axis.line$col = 'white'
            
            
          } 
          
          # Save to file
          tiff(oPlot, width=1600, height=800, pointsize=8, compression='lzw',res=200)
          print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), layout=c(6, 2), xlab="", ylab="", par.settings = myTheme, 
                          colorkey = list(space = "bottom", width=1.2, height=1)
          ) 
          + latticeExtra::layer(sp.polygons(mask_adm1, lwd=0.8))
          )
          
          dev.off()
          
        }
      }
      
      
      ## Standard deviation plot
      
      stkStd <- stack(paste0(baseDir, "/", ssp, "/", per, "/", var, "_", 1:12, "_std.tif"))
      stkStd <- mask(crop(stkStd, extent(mask)), mask)
      if(resample == T){stkStd <- resample(stkStd, raster(mask_rs), method="bilinear")}
      stkStd <- mask(crop(stkStd, extent(mask)), mask)
      oPlot <- paste(oDir, "/plot_monthly_anom_", var, "_", ssp, "_ensemble_", per, "_std", "_", suffix, ".tif", sep="")
      
      if (!file.exists(oPlot)) {
        
        if (var == "prec"){
          
          stk_crop <- stkStd * 120
          stk_crop[stk_crop > 120] = 120
          
          plot <- setZ(stk_crop, id)
          names(plot) <- id
          
          zvalues <- seq(0, 120, 5) # Define limits
          myTheme <- BuRdTheme() # Define squeme of colors
          
          
        } else {
          
          stk_crop <- stkStd
          stk_crop[stk_crop > 2 ] = 2
          
          plot <- setZ(stk_crop, id)
          names(plot) <- id
          
          zvalues <- seq(0, 2, 0.1)
          myTheme <- BuRdTheme()

        } 
        
        myTheme <- rasterTheme(region=brewer.pal('Greys', n=9))
        myTheme$strip.border$col = "white" # Eliminate frame from maps
        myTheme$axis.line$col = 'white' # Eliminate frame from maps
        
        # Save to file
        tiff(oPlot, width=1600, height=800, pointsize=8, compression='lzw',res=200)
        
        print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), layout=c(6, 2), xlab="", ylab="", par.settings = myTheme, 
                        colorkey = list(space = "bottom", width=1.2, height=1)
        ) 
        + latticeExtra::layer(sp.polygons(mask_adm1, lwd=0.8))
        )
        
        dev.off()
        
      }
      
    }
    
  }
  
}


#######################################
#### 02 Plots anomalies by seasons ####
#######################################

seasons <- c("djf", "mam", "jja", "son", "ann")
id <- rep(c("DEF ", "MAM", "JJA", "SON", "ANUAL"), length(perList))

for (ssp in sspList) {
  
  perSeas <- expand.grid(seasons, perList)
  
  for (var in varList){
    
    ### Average, q25, q75 pots
    
    for (metric in metrics){
      
      stk <- stack(paste0(baseDir, "/", ssp, "/", perSeas[,2], "/", var, "_", perSeas[,1], "_", metric,".tif"))
      stk <- mask(crop(stk, extent(mask)), mask)
      # if(resample == T){stk <- resample(stk, raster(mask_rs), method="bilinear")}
      # stk <- mask(crop(stk, extent(mask)), mask)
      
      oPlot <- paste(oDir, "/plot_season_anom_", var, "_", ssp, "_", metric, "_", suffix, ".tif", sep="")

      if (!file.exists(oPlot)) {
        
        if (var == "prec"){
          
          stk_crop <- stk * 100
          stk_crop[stk_crop > 40] = 40
          stk_crop[stk_crop < (-40)] = (-40)
          
          plot <- setZ(stk_crop, id)
          names(plot) <- id
          
          zvalues <- seq(-40, 40, 2) # Define limits
          myTheme <- BuRdTheme() # Define squeme of colors
          myTheme <- rasterTheme(region=brewer.pal('RdBu', n=9))
          myTheme$strip.border$col = "white" # Eliminate frame from maps
          myTheme$axis.line$col = 'white' # Eliminate frame from maps
          
        } else {
          
          stk_crop <- stk
          stk_crop[stk_crop < 0 ] = 0
          stk_crop[stk_crop > 5 ] = 5
          
          plot <- setZ(stk_crop, id)
          names(plot) <- id
          
          zvalues <- seq(0, 5, 0.2)
          # zvalues <- c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6)
          myTheme <- BuRdTheme()
          myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))
          myTheme$strip.border$col = "white"
          myTheme$axis.line$col = 'white'
          
          
        } 
        
        
        tiff(oPlot, width=1600, height=1000, compression='lzw',res=200)
        
        print(levelplot(plot, at = zvalues,  
                        scales = list(draw=FALSE), 
                        names.attr=rep("", length(id)), 
                        layout=c(5, 2), 
                        main=list(paste(c("         DEF", "       MAM", "     JJA", 
                                          "       SON", 
                                          "   ANUAL"), 
                                        sep=""),side=1,line=0.5, cex=0.8, font = 1 ),
                        xlab="", 
                        ylab=list(paste(rev(perList), sep="        "), line=1, cex=0.8), 
                        par.strip.text=list(cex=0),
                        par.settings = myTheme, 
                        colorkey = list(space = "bottom", width=1.2, height=1)
        )
        + latticeExtra::layer(sp.polygons(mask_adm1, lwd=0.8))
        )
        
        dev.off()
        
      }
      
    }
    
    
    ## Standar deviation
    
    stk <- stack(paste0(baseDir, "/", ssp, "/", perSeas[,2], "/", var, "_", perSeas[,1], "_std.tif"))
    stk <- mask(crop(stk, extent(mask)), mask)
    # if(resample == T){stk <- resample(stk, raster(mask_rs), method="bilinear")}
    # stk <- mask(crop(stk, extent(mask)), mask)
    
    oPlot <- paste(oDir, "/plot_season_anom_", var, "_", ssp, "_std", "_", suffix, ".tif", sep="")
    
    if (!file.exists(oPlot)) {
      
      if (var == "prec"){
        
        stk_crop <- stk * 100
        stk_crop[stk_crop > 60] = 60

        plot <- setZ(stk_crop, id)
        names(plot) <- id
        
        zvalues <- seq(0, 60, 5) # Define limits

        
      } else {
        
        stk_crop <- stk
        stk_crop[stk_crop > 2 ] = 2
        
        plot <- setZ(stk_crop, id)
        names(plot) <- id
        
        zvalues <- seq(0, 2, 0.1)
        
      } 
      
      myTheme <- BuRdTheme() # Define squeme of colors
      myTheme <- rasterTheme(region=brewer.pal('Greys', n=9))
      myTheme$strip.border$col = "white" # Eliminate frame from maps
      myTheme$axis.line$col = 'white' # Eliminate frame from maps
      
      tiff(oPlot, width=1600, height=1000, compression='lzw',res=200)
      
      print(levelplot(plot, at = zvalues,  
                      scales = list(draw=FALSE), 
                      names.attr=rep("", length(id)), 
                      layout=c(5, 2), 
                      main=list(paste(c("         DEF", "       MAM", "     JJA", 
                                        "       SON", 
                                        "   ANUAL"), 
                                      sep=""),side=1,line=0.5, cex=0.8, font = 1),
                      xlab="", 
                      ylab=list(paste(rev(perList), sep="        "), line=1, cex=0.8), 
                      par.strip.text=list(cex=0),
                      par.settings = myTheme, 
                      colorkey = list(space = "bottom", width=1.2, height=1)
      )
      + latticeExtra::layer(sp.polygons(mask_adm1, lwd=0.8))
      )
      
      dev.off()
      
    }
    
    
  }
  
}

