### Author : Carlos Navarro c.e.navarro@cgiar.org
### Date : Jan 2016

library(raster)
library(terra)
library(sp)

####################
#### 02 Future  ####
####################

bDir <- "Z:/1.Data/Results/climate/02_climate_change/dom_2_5min_anom_ens"

pts <- vect("Z:/1.Data/Process/Info_Inputs_SWAT/Republica_Dominicana/Cambio_Climatico/Centroides_Subcuencas.shp")

oDir <- "Z:/1.Data/Process/Info_Inputs_SWAT/Republica_Dominicana/Cambio_Climatico"

varList <- c("prec", "tmin", "tmax", "tmean")
varListMod <- c("pr", "tn", "tx", "tm")

sspList <- c("ssp_245", "ssp_585")
perList <- c("2050s", "2070s")
ext <- "tif"

ref <- "Z:/1.Data/Results/climate/02_climate_change/dom_2_5min_anom_ens/ssp_245/2050s/prec_1_avg.tif"

pts <- project(pts, crs(rast(ref)))

# Convertir a Spatial para seguir usando raster::extract()
pts <- as(pts, "Spatial")

for (ssp in sspList) {
  
  for (per in perList) {
    
    for (var in 1:length(varList)) {
      
      # Stack by variables
      stk <- stack(paste0(
        bDir, "/", ssp, "/", per, "/", 
        varList[var], "_", 1:12, "_avg.", ext
      ))
      
      # point shapefile
      vals <- round(extract(stk, pts), 4)
      
      ssp_col <- gsub("_", "", ssp)
      
      colnames(vals) <- paste0(
        varListMod[var],
        substring(ssp_col, nchar(ssp_col) - 1),
        "_",
        substr(per, nchar(per) - 2, nchar(per)),
        1:12
      )
      
      pts@data <- cbind(pts@data, vals)
    }
  }
}

writeVector(
  vect(pts),
  filename = file.path(oDir, "Centroides_Subcuencas_vals.shp"),
  overwrite = TRUE
)