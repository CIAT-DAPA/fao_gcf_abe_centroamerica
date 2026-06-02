#Julian Ramirez
#University of Leeds / CIAT
#2011


repoDir <- "D:/_scripts/cc-crop-impacts-central-america"
srcDir <- paste(repoDir, "/baseline_comparison", sep="")

setwd(srcDir)

#################################################################################
#################################################################################
#GCM vs. WCL grids (RAIN, TMEAN)
#################################################################################
#################################################################################

library(rgdal)

source("compareRasterRaster.R")

mDataDir <- "D:/cenavarro/msc_gis_thesis/01_baseline"

oDir <- paste(mDataDir, "/skill_gcm", sep="")
if (!file.exists(oDir)) {
	dir.create(oDir)
	}

md <- paste(mDataDir, "/gcm_data/1970_2000", sep="")
gcmList <- list.files(md)
cat(gcmList)
cd <- paste(mDataDir, "/wcl_v21_2_5min", sep="")
# shd <- "D:/cenavarro/msc_gis_thesis/00_admin_data"
shd <- "D:/cenavarro/msc_gis_thesis/00_admin_data/by_country"

# cList <- c("CAMEXCA")
# cList <- c("BHS", "BLZ", "CRI", "CUB", "DOM", "GTM", "HND", "HTI", "JAM", "NIC", "PAN", "PRI", "SLV", "NIC", "PAN", "PRI", "SLV")
cList <- c("MEX")
mam <- paste(oDir, "/wcl-vs-gcm/MAM", sep="")
son <- paste(oDir, "/wcl-vs-gcm/SON", sep="")
jja <- paste(oDir, "/wcl-vs-gcm/JJA", sep="")
djf <- paste(oDir, "/wcl-vs-gcm/DJF", sep="")
ann <- paste(oDir, "/wcl-vs-gcm/ANNUAL",sep="")


# ### Resample WLC to 1deg
# listWCL <-  list.files(paste0(cd), recursive = F, full.names = T,pattern = paste0(".tif"))
# mask1deg <- raster(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, res=1, vals=1)
# mask1deg_reg <- crop(mask1deg, extent(raster(listWCL[1])))
# oWclDir <- paste(mDataDir, "/wcl_v2_1deg", sep="")
# if (!file.exists(oWclDir)) {dir.create(oWclDir)}
# for (i in 1:length(listWCL)){
#   writeRaster(resample(raster(listWCL[i]), mask1deg_reg), paste0(oWclDir, "/", basename(listWCL[i])))
# }


if (!file.exists(mam)) {
	dir.create(mam, recursive = T)
	}
if (!file.exists(son)) {
	dir.create(son, recursive = T)
	}
if (!file.exists(jja)) {
	dir.create(jja, recursive = T)
	}
if (!file.exists(djf)) {
	dir.create(djf, recursive = T)
	}
if (!file.exists(ann)) {
	dir.create(ann, recursive = T)
	}
	
for (ctry in cList) {
	for (mod in gcmList) {
		for (vr in c("prec", "tmean")) {
			cat("Processing", ctry, mod, vr, "\n")
			if (vr == "prec") {dv <- F} else {dv <- T}
			outp <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=mam, vn=vr, divide=dv, ext=".tif", country=ctry, monthList=c(3,4,5), verbose=T)
			outp <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=son, vn=vr, divide=dv, ext=".tif", country=ctry, monthList=c(9,10,11), verbose=T)
			outp <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=jja, vn=vr, divide=dv, ext=".tif", country=ctry, monthList=c(6,7,8), verbose=T)
			outp <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=djf, vn=vr, divide=dv, ext=".tif", country=ctry, monthList=c(12,1,2), verbose=T)
			outp <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=ann, vn=vr, divide=dv, ext=".tif", country=ctry, monthList=c(1:12), verbose=T)
		}
	}
}



#################################################################################
#################################################################################
#GCM vs. WCL weather stations (RAIN,TMEAN)
#################################################################################
#################################################################################

library(rgdal)

repoDir <- "D:/_scripts/cc-crop-impacts-central-america"
srcDir <- paste(repoDir, "/baseline_comparison", sep="")

setwd(srcDir)
source("compareWSRaster.R")

mDataDir <- "D:/cenavarro/msc_gis_thesis/01_baseline"

oDir <- paste(mDataDir, "/skill_gcm", sep="")
if (!file.exists(oDir)) {
  dir.create(oDir)
}

md <- paste(mDataDir, "/gcm_data/1970_2000", sep="")
gcmList <- list.files(md)
cat(gcmList)
cd <- paste(mDataDir, "/ghcn_data", sep="")
# shd <- "D:/cenavarro/msc_gis_thesis/00_admin_data"
shd <- "D:/cenavarro/msc_gis_thesis/00_admin_data/by_country"

# cList <- c("CAMEXCA")
# cList <- c("BHS", "BLZ", "CRI", "CUB", "DOM", "GTM", "HND", "HTI", "JAM", "NIC", "PAN", "PRI", "SLV", "NIC", "PAN", "PRI", "SLV")
cList <- c("MEX")

mam <- paste(oDir, "/whst-vs-gcm/MAM", sep="")
son <- paste(oDir, "/whst-vs-gcm/SON", sep="")
jja <- paste(oDir, "/whst-vs-gcm/JJA", sep="")
djf <- paste(oDir, "/whst-vs-gcm/DJF", sep="")
ann <- paste(oDir, "/whst-vs-gcm/ANNUAL",sep="")
if (!file.exists(mam)) {dir.create(mam, recursive = T)}
if (!file.exists(son)) {dir.create(son)}
if (!file.exists(jja)) {dir.create(jja)}
if (!file.exists(djf)) {dir.create(djf)}
if (!file.exists(ann)) {dir.create(ann)}

for (ctry in cList) {
  for (mod in gcmList) {
    for (vr in c("tmean", "prec")) {
      cat("Processing", ctry, mod, vr, "\n")
      if (vr == "prec") {dv <- F} else {dv <- T}
      outp <- compareWSR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(3,4,5), outDir=mam, verbose=T)
      outp <- compareWSR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(9,10,11), outDir=son, verbose=T)
      outp <- compareWSR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(6,7,8), outDir=jja, verbose=T)
      outp <- compareWSR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(12,1,2), outDir=djf, verbose=T)
      outp <- compareWSR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(1:12), outDir=ann, verbose=T)
    }
  }
}


#################################################################################
#################################################################################
#Summarise metrics at the country level
#################################################################################
#################################################################################

#Summaries
source("summariseComparisons.R")

bDir <- "D:/cenavarro/msc_gis_thesis/01_baseline/skill_gcm" 
variableLs <- c("prec", "tmean")

# isoALL <- c("CAMEXCA", "BHS", "BLZ", "CRI", "CUB", "DOM", "GTM", "HND", "HTI", "JAM", "MEX", "NIC", "PAN", "PRI", "SLV")
# dataset <- "wcl"
isoALL <- c("CAMEXCA", "BHS", "CRI", "CUB", "DOM", "GTM", "HND", "JAM", "MEX", "NIC", "PAN", "SLV")
dataset <- "whst"

for (variable in variableLs){
  metricsSummary(bDir, dataset, variable)
  dataSummary(bDir, dataset, variable)
}


f.dir <- "D:/cenavarro/msc_gis_thesis/01_baseline/skill_gcm/_summaries"
generateBoxplots(fd=f.dir)
for (vn in variableLs) {
  for (dset in c("whst", "wcl")) {
    for (prd in c("ANNUAL","DJF","JJA","MAM","SON")) {
      if (file.exists(paste(f.dir,"/",vn,"-",dset,"-vs-gcm-summaryMetrics.csv",sep=""))) {
          createColoured(fDir=f.dir, variable=vn, dataset=dset, month="total", period=prd, metric="R2.FORCED")
        }
    }
  }
}
