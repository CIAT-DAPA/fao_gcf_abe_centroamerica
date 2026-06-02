# Author: Carlos Navarro
# UNIGIS 2023
# Purpose: Plot uncertainties in climate change projections 


###########################
#### 01 Across RCP/GCM ####
###########################

# Load libraries
require(raster)
require(rasterVis)
require(maptools)
require(rgdal)
require(ggplot2)

sspLs <- c("ssp_126", "ssp_245","ssp_585")
oDir <- "D:/cenavarro/msc_gis_thesis/02_climate_change/evaluations/statistics"
oStat <- "D:/cenavarro/msc_gis_thesis/02_climate_change/evaluations/statistics/climate_stats_by_season.csv"
ctrList <- c("BHS", "BLZ", "CRI", "CUB", "DOM", "GTM", "HND", "HTI", "JAM", "NIC", "PAN", "PRI", "SLV", "MEX")

anomVals <- read.csv(oStat, header=T)
anomVals <- anomVals[anomVals$ssp != "current", ] 
anomVals[anomVals == "2030s"] <- 2030
anomVals[anomVals == "2050s"] <- 2050
anomVals[anomVals == "2070s"] <- 2070

anomVals_pr <- anomVals[anomVals$Variable == "prec",]
anomVals_tm <- anomVals[anomVals$Variable == "tmean",]

for(ssp in sspLs){
  
  ## Prec
  # ssp <- sspLs[1]
  anomVals_pr_ssp <- anomVals_pr[anomVals_pr$ssp == ssp, ]
  
  anomVals_pr_avg <- anomVals_pr_ssp[anomVals_pr_ssp$Stat == "mean", ]
  anomVals_pr_p25 <- anomVals_pr_ssp[anomVals_pr_ssp$Stat == "q25", ]
  anomVals_pr_p75 <- anomVals_pr_ssp[anomVals_pr_ssp$Stat == "q75", ]
  
  anomVals_pr_sts <- data.frame(cbind(rbind(cbind(SEASON="DEF", PERIOD=anomVals_pr_avg$Period, ZONE=anomVals_pr_avg$Zone, MEAN=anomVals_pr_avg$djf), 
                                            cbind(SEASON="MAM", PERIOD=anomVals_pr_avg$Period, ZONE=anomVals_pr_avg$Zone, MEAN=anomVals_pr_avg$mam),
                                            cbind(SEASON="JJA", PERIOD=anomVals_pr_avg$Period, ZONE=anomVals_pr_avg$Zone, MEAN=anomVals_pr_avg$jja),
                                            cbind(SEASON="SON", PERIOD=anomVals_pr_avg$Period, ZONE=anomVals_pr_avg$Zone, MEAN=anomVals_pr_avg$son),
                                            cbind(SEASON="ANN", PERIOD=anomVals_pr_avg$Period, ZONE=anomVals_pr_avg$Zone, MEAN=anomVals_pr_avg$ann)),
                                      P25=c(anomVals_pr_p25$djf, anomVals_pr_p25$mam, anomVals_pr_p25$jja, anomVals_pr_p25$son, anomVals_pr_p25$ann),
                                      P75=c(anomVals_pr_p75$djf, anomVals_pr_p75$mam, anomVals_pr_p75$jja, anomVals_pr_p75$son, anomVals_pr_p75$ann))
  )
  
  
  anomVals_pr_sts$MEAN = as.numeric(anomVals_pr_sts$MEAN)*100
  anomVals_pr_sts$P25 = as.numeric(anomVals_pr_sts$P25)*100
  anomVals_pr_sts$P75 = as.numeric(anomVals_pr_sts$P75)*100
  anomVals_pr_sts$PERIOD = as.numeric(anomVals_pr_sts$PERIOD)
  anomVals_pr_sts$ZONE <- ctrList
  
  PlotP <- paste0(oDir, "/uncertainties_prec_", ssp, ".tif")
  
  p <- ggplot() +
    geom_line(data=anomVals_pr_sts, aes(x=PERIOD, y=MEAN, colour="band", group=1), linewidth=1) +
    # geom_point(data=anomVals_pr_sts, aes(x=PERIOD, y=MEAN, colour="band", group=1), size=1) +
    geom_ribbon(data=anomVals_pr_sts, aes(x=PERIOD, ymin=P25, ymax=P75, fill="navyblue", group=1), alpha=0.5, linewidth=0.2) +
    geom_hline(yintercept=0, linetype="dashed", color = "black", linewidth=0.1) + 
    theme(panel.background = element_rect(fill = 'gray97', linetype = 2, linewidth = 0.2), legend.title=element_blank(), axis.text.x = element_text(size = 8, angle = 90, vjust=0.5)) +
    # theme_bw() +
    guides(fill=guide_legend(title=NULL), color=guide_legend(title=NULL), alpha=FALSE) +
    scale_color_manual("", values="royalblue4") +
    scale_fill_manual("", values="royalblue4") +
    scale_x_continuous(breaks = seq(2030, 2070, 20), ) +
    theme(legend.position="none") +
    ylim(-40, 20) + 
    facet_grid( ZONE ~ factor(SEASON, levels=c('DEF', 'MAM', 'JJA', 'SON', 'ANN'))) +
    labs(x="Periodos ", y="Anomalia (%)")
  
  tiff(PlotP, width=600, height=1800, pointsize=8, compression='lzw',res=150)
  plot(p)
  dev.off()
  
  
  
  
  ## Tmean
  # ssp <- sspLs[1]
  anomVals_tm_ssp <- anomVals_tm[anomVals_tm$ssp == ssp, ]
  
  anomVals_tm_avg <- anomVals_tm_ssp[anomVals_tm_ssp$Stat == "mean", ]
  anomVals_tm_p25 <- anomVals_tm_ssp[anomVals_tm_ssp$Stat == "q25", ]
  anomVals_tm_p75 <- anomVals_tm_ssp[anomVals_tm_ssp$Stat == "q75", ]
  
  anomVals_tm_sts <- data.frame(cbind(rbind(cbind(SEASON="DEF", PERIOD=anomVals_tm_avg$Period, ZONE=anomVals_tm_avg$Zone, MEAN=anomVals_tm_avg$djf), 
                                            cbind(SEASON="MAM", PERIOD=anomVals_tm_avg$Period, ZONE=anomVals_tm_avg$Zone, MEAN=anomVals_tm_avg$mam),
                                            cbind(SEASON="JJA", PERIOD=anomVals_tm_avg$Period, ZONE=anomVals_tm_avg$Zone, MEAN=anomVals_tm_avg$jja),
                                            cbind(SEASON="SON", PERIOD=anomVals_tm_avg$Period, ZONE=anomVals_tm_avg$Zone, MEAN=anomVals_tm_avg$son),
                                            cbind(SEASON="ANN", PERIOD=anomVals_tm_avg$Period, ZONE=anomVals_tm_avg$Zone, MEAN=anomVals_tm_avg$ann)),
                                      P25=c(anomVals_tm_p25$djf, anomVals_tm_p25$mam, anomVals_tm_p25$jja, anomVals_tm_p25$son, anomVals_tm_p25$ann),
                                      P75=c(anomVals_tm_p75$djf, anomVals_tm_p75$mam, anomVals_tm_p75$jja, anomVals_tm_p75$son, anomVals_tm_p75$ann))
  )
  
  
  anomVals_tm_sts$MEAN = as.numeric(anomVals_tm_sts$MEAN)
  anomVals_tm_sts$P25 = as.numeric(anomVals_tm_sts$P25)
  anomVals_tm_sts$P75 = as.numeric(anomVals_tm_sts$P75)
  anomVals_tm_sts$PERIOD = as.numeric(anomVals_tm_sts$PERIOD)
  anomVals_tm_sts$ZONE <- ctrList
  
  PlotP <- paste0(oDir, "/uncertainties_tmean_", ssp, ".tif")
  
  p <- ggplot() +
    geom_line(data=anomVals_tm_sts, aes(x=PERIOD, y=MEAN, colour="band", group=1), linewidth=1) +
    # geom_point(data=anomVals_tm_sts, aes(x=PERIOD, y=MEAN, colour="band", group=1), size=1) +
    geom_ribbon(data=anomVals_tm_sts, aes(x=PERIOD, ymin=P25, ymax=P75, fill="firebrick4", group=1), alpha=0.5, linewidth=0.2) +
    geom_hline(yintercept=2, linetype="dashed", color = "black", linewidth=0.1) +
    theme(panel.background = element_rect(fill = 'gray97', linetype = 2, linewidth = 0.2), legend.title=element_blank(), axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5)) +
    # theme_bw() +
    guides(fill=guide_legend(title=NULL), color=guide_legend(title=NULL), alpha=FALSE) +
    scale_color_manual("", values="firebrick") +
    scale_fill_manual("", values="firebrick") +
    scale_x_continuous(breaks = seq(2030, 2070, 20), ) +
    theme(legend.position="none") +
    ylim(0, 5) + 
    facet_grid( ZONE ~ factor(SEASON, levels=c('DEF', 'MAM', 'JJA', 'SON', 'ANN'))) +
    labs(x="Periodos ", y="Anomalia (C)")
  
  tiff(PlotP, width=600, height=1800, pointsize=8, compression='lzw',res=150)
  plot(p)
  dev.off()
  
}

