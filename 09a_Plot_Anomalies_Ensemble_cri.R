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
baseDir <- "C:/Users/cenavarro/Workspace/msc_gis_thesis/02_climate_change/camexca_2_5min_anom_ens"
perList <- c("2050s", "2070s") #, "2090s") #,"2030s", )
varList <- c("prec", "tmin", "tmax", "tmean") 
id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
crs_ref <- crs(raster("Z:/1.Data/Results/climate/02_climate_change/cri_2_5min_anom_ens/prec_ssp_126_season_avg.tif"))
# mask <- as(project(vect("Z:/1.Data/Results/climate/00_admin_data/dom/gadm41_DOM_0.shp"), crs_ref), "Spatial")
# mask_adm1 <- as(project(vect("Z:/1.Data/Results/climate/00_admin_data/dom/gadm41_DOM_1.shp"), crs_ref), "Spatial")
mask <- as(project(vect("Z:/1.Data/Process/Info_Inputs_SWAT/Costa_Rica/Tempisque/Division_Administativa/cuenca_distritos_wgs84.shp"), crs_ref), "Spatial")
mask_adm1 <- as(project(vect("Z:/1.Data/Process/Info_Inputs_SWAT/Costa_Rica/Tempisque/Division_Administativa/cuenca_distritos_wgs84.shp"), crs_ref), "Spatial")
mask_rs <- "Z:/1.Data/Results/climate/01_baseline/cri/wcl_v21_30s/prec_1.tif"
resample <- T
oDir <- "Z:/1.Data/Results/climate/02_climate_change/cri_evaluations"
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
            stk_crop[stk_crop > 20] = 20
            stk_crop[stk_crop < (-20)] = (-20)
            
            plot <- setZ(stk_crop, id)
            names(plot) <- id
            
            zvalues <- seq(-20, 20, 2) # Define limits
            myTheme <- BuRdTheme() # Define squeme of colors
            # myTheme$regions$col=colorRampPalette(c("#b2182b", "#d6604d", "#f4a582", "#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac", "#053061"))(length(zvalues)-1) # Set new colors
            myTheme <- rasterTheme(region=brewer.pal('RdBu', n=9))
            myTheme$strip.border$col = "white" # Eliminate frame from maps
            myTheme$axis.line$col = 'white' # Eliminate frame from maps
            
          } else {
            
            stk_crop <- stk
            stk_crop[stk_crop > 4 ] = 4
            stk_crop[stk_crop < 0 ] = 0
            
            
            plot <- setZ(stk_crop, id)
            names(plot) <- id
            
            zvalues <- seq(0, 4, 0.2)
            myTheme <- BuRdTheme()
            myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))
            myTheme$strip.border$col = "white"
            myTheme$axis.line$col = 'white'
            
            
          } 
          
          # Save to file
          tiff(oPlot, width=1200, height=800, pointsize=8, compression='lzw',res=200)
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
        tiff(oPlot, width=1200, height=800, pointsize=8, compression='lzw',res=200)
        
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
      
      stk <- stack(paste0(baseDir, "/", var, "_", ssp, "_season_", metric,".tif"))
      stk_res <- resample(stk, raster(mask_rs), method = "bilinear")
      stk_ann <- stack(paste0(baseDir, "/", var, "_", gsub("_", "", ssp), "_ann.tif")) 
      stk_ann_res <- resample(stk_ann, raster(mask_rs), method = "bilinear")
      if (var == "prec"){stk_ann_res = stk_ann_res / 100}
      
      stk <- stack(
        stk_res[[1:4]],
        stk_ann_res[[1]],
        stk_res[[5:8]],
        stk_ann_res[[2]]
      )
      
      stk <- mask(crop(stk, extent(mask)), mask)
      # if(resample == T){stk <- resample(stk, raster(mask_rs), method="bilinear")}
      # stk <- mask(crop(stk, extent(mask)), mask)
      
      oPlot <- paste(oDir, "/plot_season_anom_", var, "_", ssp, "_", metric, "_", suffix, ".tif", sep="")

      if (!file.exists(oPlot)) {
        
        if (var == "prec"){
          
          stk_crop <- stk * 100
          stk_crop[stk_crop > 20] = 20
          stk_crop[stk_crop < (-20)] = (-20)
          
          plot <- setZ(stk_crop, id)
          names(plot) <- id
          
          zvalues <- seq(-20, 20, 2) # Define limits
          myTheme <- BuRdTheme() # Define squeme of colors
          myTheme <- rasterTheme(region=brewer.pal('RdBu', n=9))
          myTheme$strip.border$col = "white" # Eliminate frame from maps
          myTheme$axis.line$col = 'white' # Eliminate frame from maps
          
        } else {
          
          stk_crop <- stk
          stk_crop[stk_crop < 0 ] = 0
          stk_crop[stk_crop > 4 ] = 4
          
          plot <- setZ(stk_crop, id)
          names(plot) <- id
          
          zvalues <- seq(0, 4, 0.2)
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
    
    # 
    # ## Standard deviation
    # 
    # stk <- stack(paste0(baseDir, "/", ssp, "/", perSeas[,2], "/", var, "_", perSeas[,1], "_std.tif"))
    # stk <- mask(crop(stk, extent(mask)), mask)
    # if(resample == T){stk <- resample(stk, raster(mask_rs), method="bilinear")}
    # stk <- mask(crop(stk, extent(mask)), mask)
    # 
    # oPlot <- paste(oDir, "/plot_season_anom_", var, "_", ssp, "_std", "_", suffix, ".tif", sep="")
    # 
    # if (!file.exists(oPlot)) {
    #   
    #   if (var == "prec"){
    #     
    #     stk_crop <- stk * 100
    #     stk_crop[stk_crop > 60] = 60
    # 
    #     plot <- setZ(stk_crop, id)
    #     names(plot) <- id
    #     
    #     zvalues <- seq(0, 60, 5) # Define limits
    # 
    #     
    #   } else {
    #     
    #     stk_crop <- stk
    #     stk_crop[stk_crop > 2 ] = 2
    #     
    #     plot <- setZ(stk_crop, id)
    #     names(plot) <- id
    #     
    #     zvalues <- seq(0, 2, 0.1)
    #     
    #   } 
    #   
    #   myTheme <- BuRdTheme() # Define squeme of colors
    #   myTheme <- rasterTheme(region=brewer.pal('Greys', n=9))
    #   myTheme$strip.border$col = "white" # Eliminate frame from maps
    #   myTheme$axis.line$col = 'white' # Eliminate frame from maps
    #   
    #   tiff(oPlot, width=2400, height=1000, compression='lzw',res=200)
    #   
    #   print(levelplot(plot, at = zvalues,  
    #                   scales = list(draw=FALSE), 
    #                   names.attr=rep("", length(id)), 
    #                   layout=c(5, 2), 
    #                   main=list(paste(c("         DEF", "       MAM", "     JJA", 
    #                                     "       SON", 
    #                                     "   ANUAL"), 
    #                                   sep=""),side=1,line=0.5, cex=0.8, font = 1),
    #                   xlab="", 
    #                   ylab=list(paste(rev(perList), sep="        "), line=1, cex=0.8), 
    #                   par.strip.text=list(cex=0),
    #                   par.settings = myTheme, 
    #                   colorkey = list(space = "bottom", width=1.2, height=1)
    #   )
    #   + latticeExtra::layer(sp.polygons(mask_adm1, lwd=0.8))
    #   )
    #   
    #   dev.off()
    #   
    # }
    
    
  }
  
}

