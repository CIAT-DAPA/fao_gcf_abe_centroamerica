library(rgdal)

repoDir <- "C:/_scripts/usaid_hnd/03_climate_change"
srcDir <- paste(repoDir, "/baseline_comparison", sep="")

setwd(srcDir)
source("compareRasterRaster.R")

# #################################################################################
# #################################################################################
# #GCM vs. WCL grids (DTR)
# #################################################################################
# #################################################################################
# mDataDir <- "E:/GCM_Performance"
# md <- paste(mDataDir, "/gcm-data/20C3M/1961_1990", sep="")
# gcmList <- list.files(md)[-c(1,19)]
# cd <- paste(mDataDir, "/wcl-data", sep="")
# shd <- "Z:/1.Data/Results/climate/00_admin_data/hnd"
# 
# cList <- c("COL")
# jja <- paste(mDataDir, "/results/wcl-vs-gcm/JJA", sep="")
# djf <- paste(mDataDir, "/results/wcl-vs-gcm/DJF", sep="")
# ann <- paste(mDataDir, "/results/wcl-vs-gcm/ANNUAL",sep="")
# for (ctry in cList) {
#   for (mod in gcmList) {
#     cat("Processing", ctry, mod, "dtr \n")
# 		outp <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=jja, vn="dtr", divide=T, ext=".asc", country=ctry, monthList=c(6,7,8), verbose=T)
# 		outp <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=djf, vn="dtr", divide=T, ext=".asc", country=ctry, monthList=c(12,1,2), verbose=T)
# 		outp <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=ann, vn="dtr", divide=T, ext=".asc", country=ctry, monthList=c(1:12), verbose=T)
# 	}
# }

#################################################################################
#################################################################################
#GCM vs. WCL grids (RAIN, TMEAN)
#################################################################################
#################################################################################
mDataDir <- "Z:/1.Data/Results/climate/01_baseline/hnd/skill_gcm"

oDir <- paste(mDataDir, sep="")
if (!file.exists(oDir)) {
  dir.create(oDir)
}

md <- paste("W:/GLOBAL/Climate/gcm/CMIP6/raw/averages/1981_2010")
gcmList <- list.files(md)
cat(gcmList)
cd <- paste("Z:/1.Data/Results/climate/01_baseline/hnd/average_v2", sep="")
shd <- "Z:/1.Data/Results/climate/00_admin_data/hnd"

cList <- c("HND")
mam <- paste(oDir, "/wcl-vs-gcm/MAM", sep="")
son <- paste(oDir, "/wcl-vs-gcm/SON", sep="")
jja <- paste(oDir, "/wcl-vs-gcm/JJA", sep="")
djf <- paste(oDir, "/wcl-vs-gcm/DJF", sep="")
ann <- paste(oDir, "/wcl-vs-gcm/ANNUAL",sep="")

if (!file.exists(mam)) {
	dir.create(mam)
	}
if (!file.exists(son)) {
	dir.create(son)
	}
if (!file.exists(jja)) {
	dir.create(jja)
	}
if (!file.exists(djf)) {
	dir.create(djf)
	}
if (!file.exists(ann)) {
	dir.create(ann)
	}
	
for (ctry in cList) {
	for (mod in gcmList) {
		for (vr in c("tmean", "prec")) {
			cat("Processing", ctry, mod, vr, "\n")
			if (vr == "prec") {dv <- F} else {dv <- T}
			outp <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=mam, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(3,4,5), verbose=T)
			outp <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=son, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(9,10,11), verbose=T)
			outp <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=jja, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(6,7,8), verbose=T)
			outp <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=djf, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(12,1,2), verbose=T)
			outp <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=ann, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(1:12), verbose=T)
		}
	}
}

