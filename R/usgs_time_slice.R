#' Make timeslices for data assimilation from realTimeData
#' 
#' @examples
#' realTimeFiles <- list.files(pattern='huc.*.RData', 
#'                             path='~/usgsStreamData/realTimeData', 
#'                             full.names=TRUE)
#' outPath = '~/usgsStreamData/timeSliceData/'
#' MkDaTimeSlice( realTimeFiles, outPath )

#' allMins <- c()
#' for(rr in realTimeFiles){
#'  load(rr)
#'  allMins <- 
#'  #c(allMins, as.numeric(format(data$dateTime,'%M')))
#'  c(allMins, as.numeric(format(lubridate(data$dateTime,'%M')))
#'  (round(59 * .2) / .2) %% 60
#' }
#' table(allMins)

  

MkDaTimeSlice <- function( realTimeFiles, outPath ){
  ## sort realTimeFiles by their dates? not sure this is necessary.
  
  ## get back a list of output files for each input file
  ## this operation *cannot* be parallelized. 
  ## 
  
  inOutList <- plyr::llply(realTimeFiles[1], SliceHucFile, outPath=outPath)
  
}

SliceHucFile <- function(file, outPath) {
  load(file)
  #uniquePosix <- unique(data$dateTime)
  ## this operation can be parallelized
  
  
  outList <- plyr::ddply(data, plyr::.(dateTime), 
                         WriteTimeSlice, 
                         .parallel=(foreach::getDoParWorkers() > 1 ) )
  outList
}


WriteTimeSlice <- function(dfByPosix, outPath) {
  
  str(dfByPosix)
  if(unique())
  file <- TimeSliceFileName()
  ## does the file exist
  if(file.exists()) {
    
  } else {
    
  }
  stop()
}

#' @examples
#' POSIXct <- structure(c(1429296352.92171, 1429296366.12118, 1429296497.81269), class = c("POSIXct", "POSIXt"))
RoundMinutes <- function(POSIXct, nearest=5) {
  nearestInv <- 1./nearest
  POSIXct <- lubridate::round_date(POSIXct,'minute')
  theMin <- as.numeric(format(POSIXct,'%M'))
  roundMin <- (round(theMin * nearestInv) / nearestInv)
  diffMin <- roundMin - theMin
  POSIXct + lubridate::minutes(diffMin)   
}

