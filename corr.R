corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  # List the files of the requested monitors from the indicated directory
  monitorFilesList<-list.files(directory)
  setwd(directory)
  nrObs<-length(monitorFilesList)
  
  # populate a vector with measures from all the completed cases
  co = numeric()
  for(i in 1:nrObs) {
    measureList<-scan(monitorFilesList[i], skip=1, sep=",", what=list("", 1.0, 1.0, 1), quiet=TRUE)
    completeCasesCount<-sum(!is.na(measureList[[2]]*measureList[[3]]))
    if(threshold<completeCasesCount) {
      co<-c(co, cor(measureList[[2]], measureList[[3]], use="pairwise.complete.obs"))
    }
  }
  
  # return data frame
  setwd("./..")
  co
}