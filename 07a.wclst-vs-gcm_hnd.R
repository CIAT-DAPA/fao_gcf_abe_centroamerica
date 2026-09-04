# library(rgdal)

repoDir <- "C:/_scripts/usaid_hnd/03_climate_change"
srcDir <- paste(repoDir, "/baseline_comparison", sep="")

setwd(srcDir)
source("compareWSRaster.R")


#################################################################################
#################################################################################
#GCM vs. WCL weather stations (RAIN,TMEAN)
#################################################################################
#################################################################################
mDataDir <- "Z:/1.Data/Results/climate/01_baseline/hnd/skill_gcm"

oDir <- paste(mDataDir, sep="")
if (!file.exists(oDir)) {
  dir.create(oDir)
}

md <- paste("W:/GLOBAL/Climate/gcm/CMIP6/raw/averages/1981_2010")
gcmList <- list.files(md)

cd <- "Z:/1.Data/Results/climate/01_baseline/hnd/combined"
shd <- "Z:/1.Data/Results/climate/00_admin_data/hnd"

cList <- c("HND")
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
		for (vr in c("tmean", "prec", "tmin", "tmax")) {
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
