mainPercOB = function(){
  cat("\n",format(Sys.time(), usetz = T), " ")
  library(rmarkdown)
  library(magrittr)
  
  source("e:/rf/percOutsideBeam/ftpTest/rCode/outsideBeamAll.R")
  
  binThreshold = 10
  landingZone <- "f:/FTP Data/percOutsideBeam/landingZone/"
  temp <- "E:/rf/percOutsideBeam/ftpTest/temp/"
  output <- "f:/FTP Data/percOutsideBeam/Output/"
  succeeded <- "f:/FTP Data/percOutsideBeam/succeeded/"
  tempCSV <- "e:/rf/percOutsideBeam/ftpTest/temp/All.csv"
  filterSiteList <- "e:/rf/percOutsideBeam/ftpTest/temp/filterSiteList.csv"
  filterCellList <- "e:/rf/percOutsideBeam/ftpTest/temp/filterCellList.csv"
  thresholds <- "e:/rf/percOutsideBeam/ftpTest/temp/thresholds.csv"
  
  fileList <- list.files(landingZone, full.names = T, pattern = ".csv")
  if (length(fileList) == 0){cat("No data")}
  
  
  if (length(fileList > 0)) {
    for (tfile in fileList) {
      cat(tfile, "=>")
      #copy the uploaded csv to temp folder and rename it as "All.csv"
      file.copy(tfile, paste0(temp, "All.csv"))
        
      #file <- try(rmarkdown::render(notebook))
      try(outsideBeamList(tempCSV, binThreshold))
      #if the notebook cannot be rendered, and then create an error msg
      if (class(file) == "try-error") {
        cat(class(file))
        writeLines(text = paste0(basename(tfile), " has incorrect file format"),
                   paste0(landingZone, "Error_", basename(tfile) %>%
                            tools::file_path_sans_ext(), ".txt"))
      }else {
        #if succeed, move the original csv to succeeded folder
        file.copy(tfile, paste0(succeeded, basename(tfile)))
        #remove the uploaded csv
        file.remove(tfile)
        #if succeed, then rename the filterSiteList csv with uploaded file name and move it to output folder
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
        #if success, then rename the thresholds csv with uploaded file name and move it to output folder
        file.copy(thresholds, paste0(output, 
                                         basename(tfile) %>% 
                                           tools::file_path_sans_ext(),
                                         "_Thresholds.csv"))
        #remove the filterCellList from temp folder
        file.remove(thresholds)
        cat("Success! ")
        
      }
      #remove the csv from temp folder
      file.remove(tempCSV)
      
    }
    
  }
  
}




