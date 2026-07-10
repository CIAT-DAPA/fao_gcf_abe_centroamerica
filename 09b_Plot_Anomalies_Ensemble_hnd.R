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
library(terra)
library(sf)

######################################
#### 01 Plots anomalies by months ####
######################################

sspList <- c("ssp_245", "ssp_585") #"ssp_126", "ssp_370"
# sspList <- c("ssp_585")
baseDir <- "Z:/1.Data/Results/climate/02_climate_change/hnd_watershed_30s_anom_ens"
perList <- c("2050s") #, "2070s") #, "2090s") #,"2030s", )
varList <- c("prec", "tmin", "tmax", "tmean") 
# varList <- c("etp", "hurs", "rsds", "wspd") 
id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
# mask <- readOGR("D:/cenavarro/msc_gis_thesis/00_admin_data/CAMEXCA_adm0.shp")
# mask <- readOGR("D:/cenavarro/msc_gis_thesis/00_admin_data/by_country/CRI_adm0.shp")
crs_ref <- crs(raster("Z:/1.Data/Results/climate/02_climate_change/pan_30s_anom_ens/ssp_585/2050s/etp_1.tif"))
mask <- as(project(vect("Z:/1.Data/Results/climate/00_admin_data/hnd/basin1.shp"), crs_ref), "Spatial")
mask_adm1 <- as(project(vect("Z:/1.Data/Results/climate/00_admin_data/hnd/basin1.shp"), crs_ref), "Spatial")
oDir <- "Z:/1.Data/Results/climate/02_climate_change/hnd_evaluations"
metrics <- c("avg") #, "q25", "q75")

if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}

for (ssp in sspList){
  
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
            stk_crop[stk_crop > 60] = 60
            stk_crop[stk_crop < (-60)] = (-60)
            
            plot <- setZ(stk_crop, id)
            names(plot) <- id
            
            zvalues <- seq(-60, 60, 5) # Define limits
            myTheme <- BuRdTheme() # Define squeme of colors
            # myTheme$regions$col=colorRampPalette(c("#b2182b", "#d6604d", "#f4a582", "#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac", "#053061"))(length(zvalues)-1) # Set new colors
            myTheme <- rasterTheme(region=brewer.pal('RdBu', n=9))

          } else if (var == "hurs") { #%
            
            stk_crop <- stk 
            stk_crop[stk_crop > 5] = 5
            stk_crop[stk_crop < (-5)] = (-5)
            
            plot <- setZ(stk_crop, id)
            names(plot) <- id
            
            zvalues <- seq(-5, 5, 0.2) # Define limits
            myTheme <- BuRdTheme() # Define squeme of colors
            myTheme <- rasterTheme(region=brewer.pal('BrBG', n=9))
            
          } else if (var == "rsds") { 
            
            stk_crop <- stk
            stk_crop[stk_crop > 60] = 60
            stk_crop[stk_crop < -60] = -60
            
            plot <- setZ(stk_crop, id)
            names(plot) <- id
            
            zvalues <- seq(-60, 60, 5) # Define limits
            myTheme <- BuRdTheme() # Define squeme of colors
            myTheme <- rasterTheme(region=rev(brewer.pal('PuOr', n=9)))
            
          } else if (var == "etp") { 
            
            stk_crop <- stk
            stk_crop[stk_crop > 12] = 12
            stk_crop[stk_crop < 0] = 0
            
            plot <- setZ(stk_crop, id)
            names(plot) <- id
            
            zvalues <- seq(0, 12, 1) # Define limits
            myTheme <- BuRdTheme() # Define squeme of colors
            myTheme <- rasterTheme(region=rev(brewer.pal('GnBu', n=9)))
            
          } else if (var == "wspd") {
            
            stk_crop <- stk
            stk_crop[stk_crop > 100] = 100
            stk_crop[stk_crop < (-100)] = (-100)
            
            zvalues <- seq(-100, 100, 10) # Define limits
            myTheme <- BuRdTheme() # Define squeme of colors
            myTheme <- rasterTheme(region=brewer.pal('PRGn', n=9))
            
          } else {
            
            stk_crop <- stk
            stk_crop[stk_crop > 5 ] = 5
            stk_crop[stk_crop < 0 ] = 0
            
            
            plot <- setZ(stk_crop, id)
            names(plot) <- id
            
            zvalues <- seq(0, 5, 0.2)
            # zvalues <- c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6)
            myTheme <- BuRdTheme()
            # myTheme$regions$col=colorRampPalette(c("white", "snow","yellow","orange", "red", "darkred"))(length(zvalues)-1)
            myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))
            
          } 
          
          myTheme$strip.border$col = "white" # Eliminate frame from maps
          myTheme$axis.line$col = 'white' # Eliminate frame from maps
          
          # Save to file
          tiff(oPlot, width=1600, height=800, pointsize=8, compression='lzw',res=200)
          print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), layout=c(6, 2), xlab="", ylab="", par.settings = myTheme, 
                          colorkey = list(space = "bottom", width=1.2, height=1)
          ) 
          + layer(sp.polygons(mask_adm1, lwd=0.8))
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
          # zvalues <- c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6)
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
        + layer(sp.polygons(mask_adm1, lwd=0.8))
        )

        dev.off()
        
      }
      
    }
    
  }
  
}



#######################################
#### 02 Plots anomalies by seasons ####
#######################################

seasons_dic <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)
seasons <- c("djf", "mam", "jja", "son", "ann")

id <- rep(c("DEF ", "MAM", "JJA", "SON", "ANUAL"), length(perList))

for (ssp in sspList) {
  
  for (per in perList){
    
    for (var in varList){
      
      ### Average, q25, q75 pots
      ot_dir_gcm <- paste0(baseDir, "/", ssp, "/", per)
      iAvg <- stack(paste(ot_dir_gcm,'/', var, "_", 1:12, "_", metric,".tif",sep=''))
      
      for (i in 1:length(seasons_dic)){
        if (!file.exists(paste(ot_dir_gcm,'/', var, "_", names(seasons_dic[i]), "_", metric,".tif",sep=''))){
          cat("Calcs ", ssp, per, var, names(seasons[i]), "\n")
          if (var == "prec"){
            sAvg = calc(iAvg[[c(seasons_dic[i], recursive=T)]],fun=function(x){sum(x,na.rm=any(!is.na(x)))})
          } else {
            sAvg = calc(iAvg[[c(seasons_dic[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
          }
          writeRaster(sAvg, paste(ot_dir_gcm,'/', var, "_", names(seasons_dic[i]), "_", metric,".tif",sep=''),format="GTiff", overwrite=T)
        }
      }
      
      
    }
  }
  
  perSeas <- expand.grid(seasons, perList)
  
  for (var in varList){
    
    for (metric in metrics){
      
      stk <- stack(paste0(baseDir, "/", ssp, "/", perSeas[,2], "/", var, "_", perSeas[,1], "_", metric,".tif"))
      stk <- mask(crop(stk, extent(mask)), mask)
      oPlot <- paste(oDir, "/plot_season_anom_", var, "_", ssp, "_", metric, ".tif", sep="")
      
      if (!file.exists(oPlot)) {
        
        if (var == "prec"){
          
          stk_crop <- stk * 100
          stk_crop[stk_crop > 60] = 60
          stk_crop[stk_crop < (-60)] = (-60)
          
          plot <- setZ(stk_crop, id)
          names(plot) <- id
          
          zvalues <- seq(-60, 60, 5) # Define limits
          myTheme <- BuRdTheme() # Define squeme of colors
          myTheme <- rasterTheme(region=brewer.pal('RdBu', n=9))
          myTheme$strip.border$col = "white" # Eliminate frame from maps
          myTheme$axis.line$col = 'white' # Eliminate frame from maps
          
        } else if (var == "hurs") { #%
          
          stk_crop <- stk 
          stk_crop[stk_crop > 5] = 5
          stk_crop[stk_crop < (-5)] = (-5)
          
          plot <- setZ(stk_crop, id)
          names(plot) <- id
          
          zvalues <- seq(-5, 5, 0.2) # Define limits
          myTheme <- BuRdTheme() # Define squeme of colors
          myTheme <- rasterTheme(region=brewer.pal('BrBG', n=9))
          
        } else if (var == "rsds") { 
          
          stk_crop <- stk
          stk_crop[stk_crop > 60] = 60
          stk_crop[stk_crop < -60] = -60
          
          plot <- setZ(stk_crop, id)
          names(plot) <- id
          
          zvalues <- seq(-60, 60, 5) # Define limits
          myTheme <- BuRdTheme() # Define squeme of colors
          myTheme <- rasterTheme(region=rev(brewer.pal('PuOr', n=9)))
          
        } else if (var == "etp") { 
          
          stk_crop <- stk
          stk_crop[stk_crop > 12] = 12
          stk_crop[stk_crop < 0] = 0
          
          plot <- setZ(stk_crop, id)
          names(plot) <- id
          
          zvalues <- seq(0, 12, 1) # Define limits
          myTheme <- BuRdTheme() # Define squeme of colors
          myTheme <- rasterTheme(region=rev(brewer.pal('GnBu', n=9)))
          
        } else if (var == "wspd") {
          
          stk_crop <- stk
          stk_crop[stk_crop > 100] = 100
          stk_crop[stk_crop < (-100)] = (-100)
          
          zvalues <- seq(-100, 100, 10) # Define limits
          myTheme <- BuRdTheme() # Define squeme of colors
          myTheme <- rasterTheme(region=brewer.pal('PRGn', n=9))
          
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

        } 
        
        myTheme$strip.border$col = "white"
        myTheme$axis.line$col = 'white'
        
        tiff(oPlot, width=1600, height=600, compression='lzw',res=200)
        
        print(levelplot(plot, at = zvalues,  
                        scales = list(draw=FALSE), 
                        names.attr=rep("", length(id)), 
                        layout=c(5, 1), 
                        main=list(paste(c("         DEF", "       MAM", "     JJA", 
                                          "       SON", 
                                          "   ANUAL"), 
                                        sep=""),side=1,line=0.5, cex=0.8),
                        xlab="", 
                        ylab=list(paste(rev(perList), sep="        "), line=1, cex=0.9, fontface='bold'), 
                        par.strip.text=list(cex=0),
                        par.settings = myTheme, 
                        colorkey = list(space = "bottom", width=1.2, height=1)
        )
        + layer(sp.polygons(mask_adm1, lwd=0.8))
        )
        
        dev.off()
        
      }
      
    }
    
    
    ## Standard deviation

    stk <- stack(paste0(baseDir, "/", ssp, "/", perSeas[,2], "/", var, "_", perSeas[,1], "_std.tif"))
    stk <- mask(crop(stk, extent(mask)), mask)
    oPlot <- paste(oDir, "/plot_season_anom_", var, "_", ssp, "_std.tif", sep="")

    if (!file.exists(oPlot)) {

      if (var == "prec"){

        stk_crop <- stk * 100
        stk_crop[stk_crop > 120] = 120

        plot <- setZ(stk_crop, id)
        names(plot) <- id

        zvalues <- seq(0, 120, 5) # Define limits


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

      tiff(oPlot, width=1600, height=900, compression='lzw',res=200)

      print(levelplot(plot, at = zvalues,
                      scales = list(draw=FALSE),
                      names.attr=rep("", length(id)),
                      layout=c(5, 2),
                      main=list(paste(c("         DEF", "       MAM", "     JJA",
                                        "       SON",
                                        "   ANUAL"),
                                      sep=""),side=1,line=0.5, cex=0.8),
                      xlab="",
                      ylab=list(paste(rev(perList), sep="        "), line=1, cex=0.9, fontface='bold'),
                      par.strip.text=list(cex=0),
                      par.settings = myTheme,
                      colorkey = list(space = "bottom", width=1.2, height=1)
      )
      + layer(sp.polygons(mask_adm1, lwd=0.8))
      )

      dev.off()

    }
    
    
  }
  
}

