complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  # List only the files of the requested monitors from the indicated directory
  monitorFilesList<-list.files(directory)[id]
  setwd(directory)
  nrObs<-length(monitorFilesList)
  
  # populate a dataframe with measures from all the requested files, remove NAs first
  allID<-rep(1, nrObs)
  allNobs<-rep(1, nrObs)
  complete<-data.frame(allID, allNobs)
  colnames(complete)<-c("id","nobs")
  for(i in 1:nrObs) {
    measureList<-scan(monitorFilesList[i], skip=1, sep=",", what=list("", 1.0, 1.0, 1), quiet=TRUE)
    completeCasesCount<-sum(!is.na(measureList[[2]]*measureList[[3]]))
    sensorID<- max(measureList[[4]])
    complete[i,1]<-sensorID
    complete[i,2]<-completeCasesCount
  }
  
  # return data frame
  setwd("./..")
  complete
}