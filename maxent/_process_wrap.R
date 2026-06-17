setwd("C:/_scripts/fao_gcf_abe_centroamerica/maxent")

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

## 0 - Asegurar que esté el dem y la máscara
mask <- raster("Z:/1.Data/Results/climate/00_admin_data/pan_msk_30s.tif")
mask[!is.na(mask[])] <- 1
writeRaster(mask, filename = "Z:/1.Data/Results/climate/04_species/masks/pan_mask.asc", format = "ascii", overwrite = TRUE)

dem_file <- "Z:/1.Data/Process/Info_Inputs_SWAT/Panama/Tonosi_La_Villa/DEM/dem_Cuenca.tif"
dem <- raster(dem_file)
if (!compareCRS(dem, mask)) {dem <- projectRaster(dem, crs = crs(mask), method = "bilinear")}
dem_rs <- resample(dem, mask, method = "bilinear")
dem_msk <- mask(dem_rs, mask)
writeRaster(dem_msk,"Z:/1.Data/Results/climate/04_species/masks/pan_dem.tif", format = "GTiff", overwrite = TRUE)

## 1- Correr 001.extractClimates.R

## 2- Correr el modelo
source("005.modelingApproach.R")
inputDir <- "Z:/1.Data/Results/climate/04_species"
inCurClimDir <- "Z:/1.Data/Results/climate/01_baseline/pan/atlas_1981-2022_30s" 
inProjClimDir <- "Z:/1.Data/Results/climate/02_climate_change/pan_30s_ens" 
projectionList <- c("ssp_245/2050s","ssp_245/2070s","ssp_585/2050s", "ssp_585/2070s")
java <- "C:/Program Files/Java/jre-1.8/bin/java.exe"
# spID <- "anacardium_excelsum"
OSys <-	"nt"
#otp <- theEntireProcess(spID, OSys, inputDir)
for(spID in speciesList){
  otp <- theEntireProcess(spID, OSys, inputDir, inCurClimDir, inProjClimDir, projectionList, java)
}

## 3 - Sumarizar
source("006.summarizeProjectionThresholding.R")
spID <- "rhizophora_mangle"
inputDir <- "Z:/1.Data/Results/climate/04_species"
outFolder <- paste(inputDir, "/mxe_outputs", sep="")
NADir <- paste(inputDir, "/native-areas/asciigrid", sep="")
projectionList <- c("ssp_245/2050s","ssp_245/2070s","ssp_585/2050s", "ssp_585/2070s")
suffix <- "atlas_1981-2022_30s"
for(spID in speciesList){
  outName <- paste(outFolder, "/sp-", spID, sep="")
  otp <- summarize(spID, inputDir, outFolder, outName, NADir, projectionList, suffix)
}

