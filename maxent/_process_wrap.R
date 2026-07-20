setwd("C:/_scripts/fao_gcf_abe_centroamerica/maxent")

# speciesList <- c(
#   "alouatta_palliata",
#   "amazona_autumnalis",
#   "ceiba_pentandra",
#   "enterolobium_cyclocarpum",
#   "liquidambar_styraciflua",
#   "pharomachrus_mocinno"
# )

# ## Guatemala
# speciesList <- c(
#   "abies_guatemalensis",
#   "pharomachrus_mocinno",
#   "pinus_ayacahuite",
#   "plectrohyla_guatemalensis",
#   "nasua_narica",
#   "quercus_peduncularis"
# )

## Costa Rica
speciesList <- c(
  "alouatta_palliata",
  "crocodylus_acutus",
  "enterolobium_cyclocarpum",
  "jabiru_mycteria",
  "pachira_aquatica",
  "rhizophora_mangle"
)


## 0 - Asegurar que esté el dem y la máscara
dem <- raster("S:/observed/gridded_products/srtm/srtm_v41_30s.tif")
mask <- raster("Z:/1.Data/Results/climate/04_species/masks/cri_mask.tif")
crs_ref <- raster("Z:/1.Data/Results/climate/02_climate_change/gtm_2_5min_anom_ens/ssp_245/2050s/prec_1_avg.tif")
mask <- as(project(vect("Z:/1.Data/Process/Info_Inputs_SWAT/Costa_Rica/Tempisque/Division_Administativa/cuenca_distritos_wgs84.shp"), crs(crs_ref)), "Spatial")
if (!compareCRS(dem, mask)) {dem <- projectRaster(dem, crs = crs(mask), method = "bilinear")}
dem_msk <- mask(crop(dem, mask), mask)
writeRaster(dem_msk,"Z:/1.Data/Results/climate/04_species/masks/cri_dem.tif", format = "GTiff", overwrite = TRUE)

crs_ref[!is.na(crs_ref[])] <- 1 
writeRaster(mask, filename = "Z:/1.Data/Results/climate/04_species/masks/gtm_mask.asc", format = "ascii", overwrite = TRUE)
writeRaster(mask * 0 + 1, filename = "Z:/1.Data/Results/climate/04_species/masks/cri_mask.asc", format = "ascii", overwrite = TRUE)

## 1- Correr 001.extractClimates.R

## 2- Correr el modelo
source("005.modelingApproach.R")
inputDir <- "Z:/1.Data/Results/climate/04_species"
inCurClimDir <- "Z:/1.Data/Results/climate/01_baseline/cri/wcl_v21_30s"
inProjClimDir <- "Z:/1.Data/Results/climate/02_climate_change/cri_2_5min_ens" 
projectionList <- c("ssp_245/2050s","ssp_245/2070s","ssp_585/2050s", "ssp_585/2070s")
country <- "cri"
java <- "C:/Program Files/Java/jre-1.8/bin/java.exe"
# spID <- "anacardium_excelsum"
OSys <-	"nt"
#otp <- theEntireProcess(spID, OSys, inputDir)
for(spID in speciesList){
  otp <- theEntireProcess(spID, OSys, inputDir, inCurClimDir, inProjClimDir, projectionList, java, country)
}

## 3 - Sumarizar
source("006.summarizeProjectionThresholding.R")
# spID <- "rhizophora_mangle"
inputDir <- "Z:/1.Data/Results/climate/04_species"
outFolder <- paste(inputDir, "/mxe_outputs", sep="")
NADir <- paste(inputDir, "/native-areas/asciigrid", sep="")
projectionList <- c("ssp_245/2050s","ssp_245/2070s","ssp_585/2050s", "ssp_585/2070s")
suffix <- "wcl_v21_30s"
for(spID in speciesList){
  outName <- paste(outFolder, "/sp-", spID, sep="")
  otp <- summarize(spID, inputDir, outFolder, outName, NADir, projectionList, suffix)
}


## 4 - 007.boxplotsCrossvalidation

## 4- 010_Stats_Suitability
