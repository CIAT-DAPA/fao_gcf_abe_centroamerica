# Carlos Navarro 
# UNIGIS 2020A
# Purpose: Plot anomalies from CMIP6 data


# Load libraries
require(rasterVis)
require(maptools)
require(rgdal)
require(raster)
require(maptools)
require(latticeExtra)
require(RColorBrewer)


######################################
#### 01 Plots anomalies by months ####
######################################

# sspList <- c("ssp_126", "ssp_245", "ssp_585") #"ssp_370"
sspList <- c("ssp_126")
baseDir <- "D:/cenavarro/msc_gis_thesis/02_climate_change/camexca_2_5min_anom_ens"
# baseDir <- "D:/cenavarro/msc_gis_thesis/02_climate_change/camexca_2_5min_anom"
perList <- c("2030s", "2050s", "2070s") #, "2090s")
varList <- c("prec", "tmin", "tmax", "tmean") 
id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
mask <- readOGR("D:/cenavarro/msc_gis_thesis/00_admin_data/CAMEXCA_adm0.shp")
oDir <- "D:/cenavarro/msc_gis_thesis/02_climate_change/evaluations/plots"
metrics <- c("avg", "q25", "q75")

if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}

for (ssp in sspList) {
  
  for (per in perList){
    
    for (var in varList){
      
      ### Average, q25, q75 pots
      
      for (metric in metrics){
        
        stk <- stack(paste0(baseDir, "/", ssp, "/", per, "/", var, "_", 1:12, "_", metric, ".tif"))
        stk <- mask(crop(stk, extent(mask)), mask)
        oPlot <- paste(oDir, "/plot_monthly_anom_", var, "_", ssp, "_ensemble_", per, "_", metric, ".tif", sep="")
        
        
        if (!file.exists(oPlot)) {
          
          if (var == "prec"){
            
            stk_crop <- stk * 100
            stk_crop[stk_crop > 50] = 50
            stk_crop[stk_crop < (-50)] = (-50)
            
            plot <- setZ(stk_crop, id)
            names(plot) <- id
            
            zvalues <- seq(-50, 50, 5) # Define limits
            myTheme <- BuRdTheme() # Define squeme of colors
            # myTheme$regions$col=colorRampPalette(c("#b2182b", "#d6604d", "#f4a582", "#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac", "#053061"))(length(zvalues)-1) # Set new colors
            myTheme <- rasterTheme(region=brewer.pal('RdBu', n=9))
            myTheme$strip.border$col = "white" # Eliminate frame from maps
            myTheme$axis.line$col = 'white' # Eliminate frame from maps
            
          } else {
            
            stk_crop <- stk
            stk_crop[stk_crop > 5 ] = 5
            
            plot <- setZ(stk_crop, id)
            names(plot) <- id
            
            zvalues <- seq(0, 5, 0.5)
            # zvalues <- c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6)
            myTheme <- BuRdTheme()
            # myTheme$regions$col=colorRampPalette(c("white", "snow","yellow","orange", "red", "darkred"))(length(zvalues)-1)
            myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))
            myTheme$strip.border$col = "white"
            myTheme$axis.line$col = 'white'
            
            
          } 
          
          # Save to file
          tiff(oPlot, width=2000, height=1200, pointsize=8, compression='lzw',res=200)
          print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), layout=c(4, 3), xlab="", ylab="", par.settings = myTheme, 
                          colorkey = list(space = "bottom", width=1.2, height=1)
          ) 
          + layer(sp.polygons(mask, lwd=0.7))
          )
          dev.off()
          
        }
      }
      
      
      ## Standard deviation plot
      
      stkStd <- stack(paste0(baseDir, "/", ssp, "/", per, "/", var, "_", 1:12, "_std.tif"))
      stkStd <- mask(crop(stkStd, extent(mask)), mask)
      oPlot <- paste(oDir, "/plot_monthly_anom_", var, "_", ssp, "_ensemble_", per, "_std.tif", sep="")
      
      if (!file.exists(oPlot)) {
        
        if (var == "prec"){
          
          stk_crop <- stkStd * 100
          stk_crop[stk_crop > 40] = 40
          
          plot <- setZ(stk_crop, id)
          names(plot) <- id
          
          zvalues <- seq(0, 40, 5) # Define limits
          myTheme <- BuRdTheme() # Define squeme of colors
          
          
        } else {
          
          stk_crop <- stkStd
          stk_crop[stk_crop > 2 ] = 2
          
          plot <- setZ(stk_crop, id)
          names(plot) <- id
          
          zvalues <- seq(0, 2, 0.1)
          # zvalues <- c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6)
          myTheme <- BuRdTheme()
          
          
        } 
        
        myTheme <- rasterTheme(region=brewer.pal('Greys', n=9))
        myTheme$strip.border$col = "white" # Eliminate frame from maps
        myTheme$axis.line$col = 'white' # Eliminate frame from maps
        
        # Save to file
        tiff(oPlot, width=2000, height=1200, pointsize=8, compression='lzw',res=200)
        print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), layout=c(4, 3), xlab="", ylab="", par.settings = myTheme, 
                        colorkey = list(space = "bottom", width=1.2, height=1)
        ) 
        + layer(sp.polygons(mask, lwd=0.7))
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
varList <- c("prec") 
# sspList <- c("ssp_126", "ssp_245", "ssp_585") #"ssp_370"
sspList <- c("ssp_585")

for (ssp in sspList) {
  
  perSeas <- expand.grid(seasons, perList)
  
  for (var in varList){
    
    ### Average, q25, q75 pots
    
    for (metric in metrics){
      
      stk <- stack(paste0(baseDir, "/", ssp, "/", perSeas[,2], "/", var, "_", perSeas[,1], "_", metric,".tif"))
      stk <- mask(crop(stk, extent(mask)), mask)
      oPlot <- paste(oDir, "/plot_season_anom_", var, "_", ssp, "_", metric, ".tif", sep="")
      
      if (!file.exists(oPlot)) {
        
        if (var == "prec"){
          
          stk_crop <- stk * 100
          stk_crop[stk_crop > 30] = 30
          stk_crop[stk_crop < (-30)] = (-30)
          
          plot <- setZ(stk_crop, id)
          names(plot) <- id
          
          zvalues <- seq(-30, 30, 5) # Define limits
          myTheme <- BuRdTheme() # Define squeme of colors
          myTheme <- rasterTheme(region=brewer.pal('RdBu', n=9))
          myTheme$strip.border$col = "white" # Eliminate frame from maps
          myTheme$axis.line$col = 'white' # Eliminate frame from maps
          
        } else {
          
          stk_crop <- stk
          stk_crop[stk_crop > 5 ] = 5
          
          plot <- setZ(stk_crop, id)
          names(plot) <- id
          
          zvalues <- seq(0, 5, 0.5)
          # zvalues <- c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6)
          myTheme <- BuRdTheme()
          myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))
          myTheme$strip.border$col = "white"
          myTheme$axis.line$col = 'white'
          
          
        } 
        
        
        tiff(oPlot, width=3000, height=1300, compression='lzw',res=200)
        
        print(levelplot(plot, at = zvalues,  
                        scales = list(draw=FALSE), 
                        names.attr=rep("", length(id)), 
                        layout=c(5, 3), 
                        main=list(paste(c("         DEF", "         MAM", "         JJA", 
                                          "           SON", 
                                          "          ANUAL"), 
                                        sep=""),side=1,line=0.5, cex=0.8),
                        xlab="", 
                        ylab=list(paste(rev(perList), sep="        "), line=1, cex=0.9, fontface='bold'), 
                        par.strip.text=list(cex=0),
                        par.settings = myTheme, 
                        colorkey = list(space = "bottom", width=1.2, height=1)
        )
        + layer(sp.polygons(mask, lwd=0.8))
        )
        
        dev.off()
        
      }
      
    }
    
    
    ## Standar deviation
    
    stk <- stack(paste0(baseDir, "/", ssp, "/", perSeas[,2], "/", var, "_", perSeas[,1], "_std.tif"))
    stk <- mask(crop(stk, extent(mask)), mask)
    oPlot <- paste(oDir, "/plot_season_anom_", var, "_", ssp, "_std.tif", sep="")
    
    if (!file.exists(oPlot)) {
      
      if (var == "prec"){
        
        stk_crop <- stk * 100
        stk_crop[stk_crop > 30] = 30

        plot <- setZ(stk_crop, id)
        names(plot) <- id
        
        zvalues <- seq(0, 30, 5) # Define limits

        
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
      
      tiff(oPlot, width=3000, height=1300, compression='lzw',res=200)
      
      print(levelplot(plot, at = zvalues,  
                      scales = list(draw=FALSE), 
                      names.attr=rep("", length(id)), 
                      layout=c(5, 3), 
                      main=list(paste(c("         DEF", "         MAM", "         JJA", 
                                        "           SON", 
                                        "          ANUAL"), 
                                      sep=""),side=1,line=0.5, cex=0.8),
                      xlab="", 
                      ylab=list(paste(rev(perList), sep="        "), line=1, cex=0.9, fontface='bold'), 
                      par.strip.text=list(cex=0),
                      par.settings = myTheme, 
                      colorkey = list(space = "bottom", width=1.2, height=1)
      )
      + layer(sp.polygons(mask, lwd=0.8))
      )
      
      dev.off()
      
    }
    
    
  }
  
}

