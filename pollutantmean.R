pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  
  # List only the files of the requested monitors from the indicated directory
  monitorFilesList<-list.files(directory)[id]
  setwd(directory)
  if(pollutant=="sulfate"){poll<-2}else{poll<-3}
  
  # populate a vector with measures from all the requested files, remove NAs first
  x<-numeric()
  for(i in 1:length(monitorFilesList)) {
    measureList<-scan(monitorFilesList[i], skip=1, sep=",", what=list("", 1.0, 1.0, 1), quiet=TRUE)
    x<-c(x,measureList[[poll]][!is.na(measureList[[poll]])])
  }
  
  # return mean with 3 decimal places
  setwd("./..")
  round(mean(x), digits = 3)
}