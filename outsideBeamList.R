library(data.table)
library(magrittr)

#create the potential cable swap site list according to percentage outside beam and percentage outside 180
###input: tempFileDir
###output: 1. filterSiteList.csv
###output: 2. filterCellList.csv


outsideBeamList = function(tempFileDir){
  
  max2 = function(x){
    temp <- sort(x, decreasing = T)[2]
  }
  
  alldata <- fread(tempFileDir, na.strings = "N/A") %>% na.omit()
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
  # calculate the 2nd max percentage of outside beam and percentage of outside 180 on site-freq level
  alldata.site <- alldata[,.(max2OB = max2(percentOutsideBeamwidth),
                             max2O180 = max2(percentOutside180beamwidth)),
                          by = .(SiteFreq,postCode, DL_EARFCN)]
  #calculate the 2 sigma and 3 sigma value on postcode and freq level
  alldata.site.3sigma <- alldata.site[,.(OB_2sigmaThreshold = min(mean(max2OB, na.rm = T) + 2*sd(max2OB, na.rm = T), 90, na.rm = T), 
                                         O180_2sigmaThreshold = min(mean(max2O180, na.rm = T) + 2*sd(max2O180, na.rm = T), 90, na.rm = T),
                                         OB_3sigmaThreshold = min(mean(max2OB, na.rm = T) + 3*sd(max2OB, na.rm = T), 90, na.rm = T), 
                                         O180_3sigmaThreshold = min(mean(max2O180, na.rm = T) + 3*sd(max2O180, na.rm = T), 90, na.rm = T)
                                         ), 
                                      by=.(postCode, DL_EARFCN)]
  #join the data on site-freq level and the corresponding 3 sigma and 2 sigma values
  sectorList <- alldata.site.3sigma[alldata.site, on = .(postCode, DL_EARFCN)]
  sectorList <- sectorList[,.(postCode, DL_EARFCN, SiteFreq, max2OB, max2O180, 
                              OB_2sigmaThreshold, O180_2sigmaThreshold, OB_3sigmaThreshold, O180_3sigmaThreshold)]
  ### Filter Sector List with the intersection of percentage outside beam and percentage outside 180 > 2 sigma
  filterSiteList <- sectorList[max2O180 > O180_2sigmaThreshold & max2OB > OB_2sigmaThreshold]
  filterCellList <- alldata[SiteFreq %in% filterSiteList$SiteFreq]
  filterCellList <- alldata.site.3sigma[filterCellList, on =.(postCode, DL_EARFCN)]
  
  fwrite(filterSiteList, "e:/rf/percOutsideBeam/ftpTest/temp/filterSiteList.csv")
  fwrite(filterCellList, "e:/rf/percOutsideBeam/ftpTest/temp/filterCellList.csv")
  fwrite(alldata.site.3sigma, "e:/rf/percOutsideBeam/ftpTest/temp/thresholds.csv")
}

