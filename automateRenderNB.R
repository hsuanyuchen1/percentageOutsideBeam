cat(format(Sys.time(), usetz = T),"\n", file = "e:/rf/percOutsideBeam/ftpTest/perceObLog.txt", append = T)
library(rmarkdown)
library(magrittr)

source("e:/rf/percOutsideBeam/ftpTest/rCode/outsideBeamList.R")

landingZone <- "f:/FTP Data/percOutsideBeam/landingZone/"
temp <- "E:/rf/percOutsideBeam/ftpTest/temp/"
output <- "f:/FTP Data/percOutsideBeam/Output/"
tempCSV <- "e:/rf/percOutsideBeam/ftpTest/temp/All.csv"
filterSiteList <- "e:/rf/percOutsideBeam/ftpTest/temp/filterSiteList.csv"
filterCellList <- "e:/rf/percOutsideBeam/ftpTest/temp/filterCellList.csv"

fileList <- list.files(landingZone, full.names = T, pattern = ".csv")
cat(fileList, "\n",file = "e:/rf/percOutsideBeam/ftpTest/perceObLog.txt", append = T)

if (length(fileList > 0)) {
  for (tfile in fileList) {
    #copy the uploaded csv to temp folder and rename it as "All.csv"
    file.copy(tfile, paste0(temp, "All.csv"))
    #remove the uploaded csv
    file.remove(tfile)  
    #file <- try(rmarkdown::render(notebook))
    try(outsideBeamList(tempCSV))
    #if the notebook cannot be rendered, and then create an error msg
    if (class(file) == "try-error") {
      writeLines(text = paste0(basename(tfile), " has incorrect file format"),
                 paste0(landingZone, "Error_", basename(tfile) %>%
                          tools::file_path_sans_ext(), ".txt"))
    }else {
      #if success, then rename the filterSiteList csv with uploaded file name and move it to output folder
      file.copy(filterSiteList, paste0(output, 
                                 basename(tfile) %>% 
                                   tools::file_path_sans_ext(),
                                 "_siteList.csv"))
      #remove the filterSiteList from temp folder
      file.remove(filterSiteList)
      
      #if success, then rename the filterCellList csv with uploaded file name and move it to output folder
      file.copy(filterCellList, paste0(output, 
                                       basename(tfile) %>% 
                                         tools::file_path_sans_ext(),
                                       "_CellList.csv"))
      #remove the filterCellList from temp folder
      file.remove(filterCellList)
      
      
    }
    #remove the csv from temp folder
    file.remove(tempCSV)
      
  }
    
}
