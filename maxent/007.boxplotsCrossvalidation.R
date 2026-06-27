###################################
###Cross Validation Performance####
###############AUC#################
###################################
inputDir <- "Z:/1.Data/Results/climate/04_species"
speciesList <- c(
  "amazona_ventralis",
  "cyclura_cornuta",
  "juniperus_gracilior",
  "leuenbergeria_quisqueyana",
  "magnolia_pallescens",
  "solenodon_paradoxus"
)
outFolder <- paste(inputDir, "/mxe_outputs", sep="")
oDir <- paste0(outFolder, "/_plots")

# bDir="D:/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica_swd/crossval"
# oDir="D:/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica_swd/metrics"
# id <- read.csv(paste(bDir, "/maxentResults.csv", sep=""))

for(spID in speciesList){
  
  outName <- paste0(outFolder, "/sp-", spID)
  bDir <- paste0(outName, "/crossval")
  id <- read.csv(paste(bDir, "/maxentResults.csv", sep=""))

  tiff(paste(oDir, "/Hist_test_AUC_", spID, ".tif", sep=""),width=600, height=600,pointsize=8,compression='lzw',res=150)
  hist(id$Test.AUC,breaks=10,xlim=c(round(min(id$Test.AUC)-0.1,digits =1),1),xlab="AUC (dec)",ylab="Frecuency",main=NA)
  dev.off()
  
  tiff(paste(oDir, "/Hist_training_AUC", spID, ".tif", sep=""),width=600, height=600,pointsize=8,compression='lzw',res=150)
  hist(id$Training.AUC,breaks=10,xlim=c(round(min(id$Training.AUC)-0.1,digits =1),1),xlab="AUC (dec)",ylab="Frecuency",main=NA)
  dev.off()
}