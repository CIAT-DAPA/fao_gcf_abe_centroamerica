# Carlos Navarro 
# CIAT - CCAFS
# May 2018


######################################
#### 01 Plots anomalies by months ####
######################################

# Load libraries
require(rasterVis)
require(maptools)
require(rgdal)
require(raster)
require(maptools)
require(latticeExtra)
require(RColorBrewer)

sspList <- c("ssp_126", "ssp_245", "ssp_370", "ssp_585")
# sspList <- c("ssp_585")
baseDir <- "D:/cenavarro/msc_gis_thesis/02_climate_change/camexca_2_5min_anom"
perList <- c("2030s", "2050s", "2070s", "2090s")
varList <- c("prec", "tmin", "tmax", "tmean") 
id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
mask <- readOGR("D:/cenavarro/msc_gis_thesis/00_admin_data/CAMEXCA_adm0.shp")
oDir <- "D:/cenavarro/msc_gis_thesis/02_climate_change/camexca_2_5min_anom/_plot"


if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}

for (ssp in sspList) {
  
  gcmList <- list.dirs(paste0(baseDir, "/", ssp), recursive = FALSE, full.names = FALSE)
  
  for (gcm in gcmList){
    
    for (per in perList){
      
      for (var in varList){
        
        oPlot <- paste(oDir, "/plot_monthly_anom_", var, "_", ssp, "_", gcm, "_", per, ".tif", sep="")
        if (!file.exists(oPlot)) {
          
          stk <- stack(paste0(baseDir, "/", ssp, "/", gcm, "/", per, "/", var, "_", 1:12, ".tif"))
          stk <- mask(crop(stk, extent(mask)), mask)
          
          if (var == "prec"){
            
            stk_crop <- stk * 100
            stk_crop[stk_crop > 100] = 100
            stk_crop[stk_crop < (-100)] = (-100)
            
            plot <- setZ(stk_crop, id)
            names(plot) <- id
            
            zvalues <- seq(-100, 100, 10) # Define limits
            myTheme <- BuRdTheme() # Define squeme of colors
            # myTheme$regions$col=colorRampPalette(c("#b2182b", "#d6604d", "#f4a582", "#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac", "#053061"))(length(zvalues)-1) # Set new colors
            myTheme <- rasterTheme(region=brewer.pal('RdBu', n=9))
            myTheme$strip.border$col = "white" # Eliminate frame from maps
            myTheme$axis.line$col = 'white' # Eliminate frame from maps
            
          } else {
            
            stk_crop <- stk
            stk_crop[stk_crop > 6 ] = 6
            
            plot <- setZ(stk_crop, id)
            names(plot) <- id
            
            zvalues <- seq(0, 6, 0.5)
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
          # + layer(sp.polygons(mask, lwd=0.8))
          )
          dev.off()
          
        }
        
        
        cat(paste(">. ", ssp, gcm, per, var, " plotted\n"))
        
        
      }
      
    }
    
  }
  
}


