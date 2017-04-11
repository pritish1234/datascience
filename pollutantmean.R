pollutantmean<- function(directory, pollutant, id){
  datab<-read.csv(paste(directory,id,".csv",sep = ""))
  if(pollutant == "sulphate"){
    pollutantcleaned<- datab$sulfate[!is.na(datab$sulfate)]
  }
  else if(pollutant == "nitrate"){
    pollutantcleaned<- datab$nitrate[!is.na(datab$nitrate)]
  }
  mean(pollutantcleaned)
}

complete<- function(directory, id){
  datab<-read.csv(paste(directory,id,".csv",sep = ""))
  num<-length(!is.na(complete.cases(datab)))
  num
}

corr <- function(directory, threshold = 0) {
 tcorr <- function(fname) {
    data <- read.csv(file.path(directory, fname))
    nobs <- sum(complete.cases(data))
    if (nobs > threshold) {
      return (cor(data$nitrate, data$sulfate, use="complete.obs"))
    }
  }
  tcorrs <- sapply(list.files(directory), tcorr) #get all correlations + NULLs
  tcorrs <- unlist(tcorrs[!sapply(tcorrs, is.null)]) #remove NULLs
  return (tcorrs)
}

elt<- function(matrix,row,col)
{
  matrix[row,col]
}
