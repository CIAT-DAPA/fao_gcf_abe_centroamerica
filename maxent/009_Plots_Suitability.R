# Author: Carlos Navarro
# UNIGIS 2023
# Purpose: Plot suitability changes from EcoCrop runs and uncertainties

##############################
###### Maxent Plots  ########
##############################

# Load libraries
require(raster)
require(rasterVis)
require(maptools)
require(latticeExtra)
require(RColorBrewer)
require(stringr)
library(terra)

# Set params
inputDir <- "Z:/1.Data/Results/climate/04_species"
#spID <- "cedrela_montana"
outFolder <- paste(inputDir, "/mxe_outputs", sep="")
crs_ref <- crs(raster("Z:/1.Data/Results/climate/01_baseline/pan/atlas_1981-2022_30s/prec_1.tif"))
mask_adm1 <- as(project(vect("Z:/1.Data/Process/Info_Inputs_SWAT/Panama/Tonosi_La_Villa/Division_administrativa/Tonosi_la_Villa_corregimientos.shp"), crs_ref), "Spatial")
mask_adm2 <- mask_adm1
projectionList <- c("ssp_245/2050s","ssp_245/2070s","ssp_585/2050s", "ssp_585/2070s")
id <- c("2050 SSP2-4.5", "2050s SSP5-8.5",
        "2070 SSP2-4.5", "2070s SSP5-8.5"
)
sspLsMod <- c("SSP2-4.5", "SSP5-8.5")
yearLs <- c("2050s", "2070s")
projectionList <- gsub("/", "_", projectionList)
suffix <- "atlas_1981-2022_30s"
oDir <- paste0(outFolder, "/_plots")
if (!file.exists(oDir)) {dir.create(oDir)}

speciesList <- c(
  "alouatta_palliata",
  "ateles_geoffroyi",
  "ara_macao",
  "anacardium_excelsum",
  # "dalbergia_retusa",
  "dendrobates_auratus",
  # "leopardus_pardalis",
  "rhizophora_mangle"
)

for(spID in speciesList){
  
  outName <- paste0(outFolder, "/sp-", spID)

  suitH <- raster(paste(outName, "/crossval/", paste0(toupper(substr(spID, 1, 1)), substr(spID, 2, nchar(spID))), "_", suffix , "_avg.asc", sep="")) *100
  suitH_crop <- mask(crop(suitH, extent(mask_adm1)), mask_adm1)
  
  id_mod <-c("CUR")
  plot <- setZ(suitH_crop, id_mod)
  names(plot) <- toupper(id_mod)
  
  zvalues <- seq(0, 100, 10) # Define limits
  myTheme <- BuRdTheme() # Define squeme of colors
  # myTheme$regions$col=colorRampPalette(c("khaki1", "skyblue1", "blue", "darkblue", "darkmagenta"))(length(zvalues)-1) # Set new colors
  myTheme=rasterTheme(region=brewer.pal('BuGn', n=9))  
  myTheme$strip.border$col = "white" # Eliminate frame from maps
  myTheme$axis.line$col = 'white' # Eliminate frame from maps

  
  tiff(paste(oDir, "/suit_", spID, ".tif", sep=""), width=800, height=800, pointsize=8, compression='lzw',res=100)
  
  print(levelplot(plot, at = zvalues, 
                  scales = list(draw=FALSE), 
                  # layout=c(2, 2), 
                  xlab="", 
                  ylab="", 
                  par.settings = myTheme, 
                  margin=F,
                  colorkey = list(space = "bottom", width=1.2, height=1, labels=list(cex=1.2)))
        + latticeExtra::layer(sp.polygons(mask_adm2, lwd = 0.8))
  )
  
  dev.off()
  
  suitC <- stack(paste0(outName, "/projections/changes/",spID, "_", projectionList,  "_EMN",".asc")) *100
  suitC_crop <- mask(crop(suitC, extent(mask_adm1)), mask_adm1)
  
  # Set limits
  suitC_crop[which(suitC_crop[]< (-100))] = (-100)
  suitC_crop[which(suitC_crop[]> 100) ] = 100
  
  # Plot settings
  plot <- setZ(suitC_crop, id)
  names(plot) <- id
  zvalues <- seq(-100, 100, 10)
  myTheme <- BuRdTheme()
  myTheme$regions$col=colorRampPalette(c("darkred", "red", "orange", "snow", "yellowgreen", "forestgreen", "darkolivegreen"))(length(zvalues)-1)
  myTheme$strip.border$col = "white"
  myTheme$axis.line$col = 'white'
  
  # Plot via levelplot
  tiff(paste(oDir, "/suitchg_", spID, "_changes.tif", sep=""), width=1000, height=1000, pointsize=8, compression='lzw',res=100)
  
  print(levelplot(plot, at = zvalues,  
                  scales = list(draw=FALSE), 
                  names.attr=c(sspLsMod, rep("", length(id)-2)),
                  layout=c(2, 2), 
                  main="",
                  xlab="",
                  ylab=list(paste(rev(c(yearLs)), sep=""),side=1,line=0.5, cex=1),
                  # par.strip.text=list(cex=0),
                  par.settings = myTheme, 
                  colorkey = list(space = "bottom", width=1.2, height=1)
  )
  + latticeExtra::layer(sp.polygons(mask_adm2, lwd = 0.8))
  
  )
  
  dev.off()

  # ## Thresholded
  # suitH <- raster(paste(outName, "/projections/summarize/", spID, "_bsl_EMN_PR.asc", sep=""))
  # suitH_crop <- mask(crop(suitH, extent(mask_adm1)), mask_adm1)
  # 
  # suitC <- stack(paste0(outName, "/projections/changes/",spID, "_", projectionList,  "_EMN_PR",".asc")) *100
  # suitC_crop <- mask(crop(suitC, extent(mask_adm1)), mask_adm1)
  # 
  # # Set limits
  # suitC_crop[which(suitC_crop[]< (-100))] = (-100)
  # suitC_crop[which(suitC_crop[]> 100) ] = 100
  # 
  # # Plot settings
  # plot <- setZ(suitC_crop, id)
  # names(plot) <- id
  # zvalues <- seq(-100, 100, 10)
  # myTheme <- BuRdTheme()
  # myTheme$regions$col=colorRampPalette(c("darkred", "red", "orange", "snow", "yellowgreen", "forestgreen", "darkolivegreen"))(length(zvalues)-1)
  # myTheme$strip.border$col = "white"
  # myTheme$axis.line$col = 'white'
  # 
  # # Plot via levelplot
  # tiff(paste(oDir, "/suitchg_", spID, "_changes_pr.tif", sep=""), width=1000, height=800, pointsize=8, compression='lzw',res=100)
  # 
  # print(levelplot(plot, at = zvalues,  
  #                 scales = list(draw=FALSE), 
  #                 names.attr=c(sspLsMod, rep("", length(id)-3)),
  #                 layout=c(3, 2), 
  #                 main="",
  #                 xlab="",
  #                 ylab=list(paste(rev(c(yearLs)), sep=""),side=1,line=0.5, cex=1),
  #                 # par.strip.text=list(cex=0),
  #                 par.settings = myTheme, 
  #                 colorkey = list(space = "bottom", width=1.2, height=1)
  # )
  # + latticeExtra::layer(sp.polygons(mask_adm2, lwd = 0.8))
  # 
  # )
  # 
  # dev.off()
}
