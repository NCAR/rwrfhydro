#' Make timeslices from active USGS data files. 
#' 
#' @examples
#' realTimeFiles <- list.files(pattern='huc.*.RData', 
#'                             path='~/usgsStreamData/realTimeData', 
#'                             full.names=TRUE)
#' outPath = '~/usgsStreamData/timeSliceData/'
#' MkUsgsTimeSlice( realTimeFiles, outPath )
MkUsgsTimeSlice <- function( realTimeFiles, outPath, nearestMin=5,
                             varianceFunction=NULL){
  ## sort realTimeFiles by their dates? not sure this is necessary.
  
  ## get back a list of output files for each input file
  ## this operation *cannot* be parallelized. 
  ## 
    
  inOutList <- plyr::llply(realTimeFiles[1], SliceHucFile, 
                           outPath=outPath, nearest=nearestMin, 
                           varianceFunction=varianceFunction)
  
}

SliceHucFile <- function(file, outPath, nearest,
                         varianceFunction) {
  load(file)
  #uniquePosix <- unique(data$dateTime)
  ## this operation CAN be parallelized
  
  data$dateTime <- RoundMinutes(data$dateTime, nearest=nearest)
  print(table(data$dateTime))
  outList <- plyr::ddply(data, plyr::.(dateTime), 
                         WriteNcTimeSlice, 
                         outPath, varianceFunction, 
                         .parallel=(foreach::getDoParWorkers() > 1 ) )
  outList
}


#' Round a POSIXct time to the nearest mth minute.
#' @examples
#' POSIXct <- structure(c(1429296352.92171, 1429296366.12118, 1429296497.81269), class = c("POSIXct", "POSIXt"))
#' RoundMinutes(POSIXct, nearest=5)
RoundMinutes <- function(POSIXct, nearest=5) {
  nearestInv <- 1./nearest
  theMin <- as.numeric(format(POSIXct,'%M')) + as.numeric(format(POSIXct,'%S'))/60
  roundMin <- (round(theMin * nearestInv) / nearestInv)
  diffMin <- roundMin - theMin
  POSIXct + lubridate::seconds(round(diffMin*60))   
}


