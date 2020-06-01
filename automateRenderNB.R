library(rmarkdown)
library(magrittr)

landingZone <- "f:/FTP Data/percOutsideBeam/landingZone/"
temp <- "E:/rf/percOutsideBeam/ftpTest/temp/"
output <- "f:/FTP Data/percOutsideBeam/Output/"
tempHtml <- "e:/rf/percOutsideBeam/ftpTest/rCode/PercOutSideBeam.nb.html"
tempCSV <- "e:/rf/percOutsideBeam/ftpTest/temp/All.csv"
notebook <- "e:/rf/percOutsideBeam/ftpTest/rCode/PercOutSideBeam.Rmd"

fileList <- list.files(landingZone, full.names = T)

if (length(fileList > 0)) {
  for (tfile in fileList) {
    #check if the uploaded data is in csv format. Only process csv data
    if (substr(tfile, nchar(tfile)-2, nchar(tfile)) == "csv") {
      #copy the uploaded csv to temp folder and rename it as "All.csv"
      file.copy(tfile, paste0(temp, "All.csv"))
      #remove the uploaded csv
      file.remove(tfile)
      #render r notebook to html file
      Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
      file <- try(rmarkdown::render(notebook))
      #if the notebook cannot be rendered, and then create an error msg
      if (class(file) == "try-error") {
        writeLines(text = paste0(basename(tfile), " has incorrect file format"),
                   paste0(landingZone, "Error_", basename(tfile) %>%
                            tools::file_path_sans_ext(), ".txt"))
      }else {
        #if render success, thne rename the html with uploaded file name and move it to output folder
        file.copy(tempHtml, paste0(output, 
                                   basename(tfile) %>% 
                                     tools::file_path_sans_ext(),
                                   ".html"))
        #remove the temp html file
        file.remove(tempHtml)
        file.remove(tempCSV)
      }
      #remove the csv from temp folder
      
      
    }
    
  }
}
