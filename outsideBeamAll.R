library(data.table)
library(magrittr)
library(dplyr)

#create the potential cable swap site list according to percentage outside beam and percentage outside 180
###input 1: tempFileDir
###input 2: binThreshold: remove the samples whose total bin number is below binThreshold
###output: 1. filterSiteList.csv
###output: 2. filterCellList.csv
###output: 3. threshold.csv


outsideBeamList = function(tempFileDir, binThreshold){
  
  max2 = function(x){
    temp <- sort(x, decreasing = T)[2]
    return(temp)
  }
  
  alldata <- fread(tempFileDir, na.strings = "N/A") 
  result <- try(sigmaThr <- alldata[1, sigmaThreshold])
  if(class(result) == "try-error"){
    sigmaThr  <- 90
  } 

  
  alldata <- na.omit(alldata[,1:17])
  alldata <- alldata[totalBins > binThreshold]
  #remove duplicate
  alldata <- alldata[,.(totalBins = max(totalBins, na.rm = T),
                          percentOutsideBeamwidth = min(percentOutsideBeamwidth, na.rm = T),
                          percentOutside180beamwidth = min(percentOutside180beamwidth, na.rm = T)),
                     by = .(Site, Sector, PCI, DL_EARFCN)]
  alldata$postCode <- substr(alldata$Sector,1,3)
  alldata$SiteFreq <- paste(substr(alldata$Site,3,8), alldata$DL_EARFCN, sep = "_")
  
  
  siteList <- alldata[,.(cellCount = .N),
                      by = .(SiteFreq)]
  
  siteList <- siteList[cellCount > 1]
  #remove one-cell sites
  alldata <- alldata[SiteFreq %in% siteList$SiteFreq]
  #calculate the 2 sigma and 3 sigma value on postcode and freq level
  alldata.site.3sigma <- alldata[,.(OB_Avg = mean(percentOutsideBeamwidth, na.rm = T) %>% round(.,1),
                                    OB_Sigma = sd(percentOutsideBeamwidth, na.rm = T) %>% round(.,1),
                                    OB_2sigmaThreshold = min(mean(percentOutsideBeamwidth, na.rm = T) + 2*sd(percentOutsideBeamwidth, na.rm = T), sigmaThr, na.rm = T) %>% round(.,1),
                                    OB_3sigmaThreshold = min(mean(percentOutsideBeamwidth, na.rm = T) + 3*sd(percentOutsideBeamwidth, na.rm = T), sigmaThr, na.rm = T) %>% round(.,1),
                                    OB180_Avg = mean(percentOutside180beamwidth, na.rm = T) %>% round(.,1),
                                    OB180_Sigma = sd(percentOutside180beamwidth, na.rm = T) %>% round(.,1),
                                    O180_2sigmaThreshold = min(mean(percentOutside180beamwidth, na.rm = T) + 2*sd(percentOutside180beamwidth, na.rm = T), sigmaThr, na.rm = T) %>% round(.,1),
                                    O180_3sigmaThreshold = min(mean(percentOutside180beamwidth, na.rm = T) + 3*sd(percentOutside180beamwidth, na.rm = T), sigmaThr, na.rm = T) %>% round(.,1)
                                    ),
                                 by=.(postCode, DL_EARFCN)]
  #join the data on site-freq level and the corresponding 3 sigma and 2 sigma values
  sectorList <- alldata.site.3sigma[alldata, on = .(postCode, DL_EARFCN)]
  sectorList$check_OB_2Sigma <- ifelse(sectorList$percentOutsideBeamwidth > sectorList$OB_2sigmaThreshold, 1, 0)
  sectorList$check_O180_2Sigma <- ifelse(sectorList$percentOutside180beamwidth > sectorList$O180_2sigmaThreshold, 1, 0)
  sectorList$check_OB_3Sigma <- ifelse(sectorList$percentOutsideBeamwidth > sectorList$OB_3sigmaThreshold, 1, 0)
  sectorList$check_O180_3Sigma <- ifelse(sectorList$percentOutside180beamwidth > sectorList$O180_3sigmaThreshold, 1, 0)
  #2 sigma and
  sectorList$check_2Sigma_and <- sectorList$check_OB_2Sigma * sectorList$check_O180_2Sigma
  #3 sigma or
  sectorList$check_3Sigma_or <- pmin(sectorList$check_OB_3Sigma + sectorList$check_O180_3Sigma,1)
  sectorList <- sectorList[,c("postCode", "SiteFreq", "PCI" ,"OB_2sigmaThreshold",
                              "O180_2sigmaThreshold", "OB_3sigmaThreshold", "O180_3sigmaThreshold", "totalBins",
                              "percentOutsideBeamwidth", "percentOutside180beamwidth", "check_OB_2Sigma", 
                              "check_O180_2Sigma", "check_OB_3Sigma", "check_O180_3Sigma", "check_2Sigma_and", 
                              "check_3Sigma_or")]
  
  # sectorList.agg <- sectorList[,.(OB_2nd_max = max2(percentOutsideBeamwidth), OB180_2nd_max = max2(percentOutside180beamwidth),
  #                                 sum2sigma = sum(check_2Sigma_and), sum3sigma = sum(check_3Sigma_or)),
  #                              by=.(postCode, SiteFreq)]
  # 
  sectorList.agg <- sectorList[,.(OB_2nd_max = max2(percentOutsideBeamwidth), OB180_2nd_max = max2(percentOutside180beamwidth),
                                  sum2sigma = sum(check_2Sigma_and), sum3sigmaOB = sum(check_OB_3Sigma), sum3sigmaO180 = sum(check_O180_3Sigma)),
                               by=.(postCode, SiteFreq)]
  #filter list with 2 cells per site satisfying 2 sigma and and 3 sigma or criteria
  sectorList.agg <- sectorList.agg[sum2sigma > 1 | sum3sigmaOB > 1 |  sum3sigmaO180 > 1]
  
  if(nrow(sectorList.agg) > 0){
    temp <- strsplit(sectorList.agg$SiteFreq, split = "_") %>% 
      unlist() %>% matrix(ncol=2, byrow=T) %>% as.data.table()
    colnames(temp) <- c("eNBID", "DL_EARFCN")
    temp$DL_EARFCN <- as.integer(temp$DL_EARFCN)
    
    sectorList.agg <- cbind(sectorList.agg, temp)
    sectorList.agg <- alldata.site.3sigma[sectorList.agg, .(postCode, eNBID, DL_EARFCN, OB_2nd_max, OB_2sigmaThreshold, OB_3sigmaThreshold, 
                                                            OB180_2nd_max, O180_2sigmaThreshold, O180_3sigmaThreshold), 
                                          on=.(postCode, DL_EARFCN)]
    sectorList.agg$check_OB_2Sigma <- ifelse(sectorList.agg$OB_2nd_max > sectorList.agg$OB_2sigmaThreshold, 1, 0)
    sectorList.agg$check_OB_3Sigma <- ifelse(sectorList.agg$OB_2nd_max > sectorList.agg$OB_3sigmaThreshold, 1, 0)
    sectorList.agg$check_O180_2Sigma <- ifelse(sectorList.agg$OB180_2nd_max > sectorList.agg$O180_2sigmaThreshold, 1, 0)
    sectorList.agg$check_O180_3Sigma <- ifelse(sectorList.agg$OB180_2nd_max > sectorList.agg$O180_3sigmaThreshold, 1, 0)
  } else{
    sectorList.agg <- "no site exceeds filter criteria"
    sectorList.agg <- as.data.table(sectorList.agg)
    colnames(sectorList.agg)[1] <- "result"
  }
  
  
  #output data
  fwrite(sectorList.agg, "e:/rf/percOutsideBeam/ftpTest/temp/filterSiteList.csv")
  fwrite(sectorList, "e:/rf/percOutsideBeam/ftpTest/temp/filterCellList.csv")
  fwrite(alldata.site.3sigma, "e:/rf/percOutsideBeam/ftpTest/temp/thresholds.csv")
}

