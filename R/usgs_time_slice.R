#' Make timeslices from active USGS discharge data files. 
#' 
#' @examples
#' realTimeFiles <- list.files(pattern='huc.*.RData', 
#'                             path='~/usgsStreamData/realTimeData', 
#'                             full.names=TRUE)[1:3]
#' outPath = '~/usgsStreamData/timeSliceData/'
#' MkUsgsTimeSlice( realTimeFiles, outPath )
MkUsgsTimeSlice <- function( realTimeFiles, outPath, nearestMin=5,
                             varianceFunction=NULL){

  stop()
  ## get all the active data from the specified files
  RetActData <- function(file) {
    load(file)
    data <- subset(data, !is.na(X_00060_00011))
    data$queryTime <- attr(data,'queryTime')
    data
  }
  allData <- plyr::ldply(NamedList(realTimeFiles), RetActData)  
  names(allData)[1] <- 'queryFileName'
  
  ## QC Active data
  ## filter by product codes
  ## filter duplicate observations per indiv time
  ## convert units. 

  # write it out to files. 
  ## files are written to the nearest Nth minute.
  allData$dateTime <- RoundMinutes(allData$dateTime, nearest=nearestMin)
  print(table(allData$dateTime))

  ## output must be in cms (m3/s)
  allData$discharge.cms <- allData$X_00060_00011 * cfs2cms
  
  ## remove unused variables for speed/memory
  allData <- within(allData, {agency_cd <- tz_cd  <- X_00060_00011 <- NULL})
  ## maybe use queryFileName in global attributes... ? a record of what query times 
  ## contributed the data file to be written? for now, nullify it.
  allData$queryFileName <- NULL
  
  dum <- WriteNcTimeSlice(subset(allData, dateTime == allData$dateTime[1]))
                                   
  outList <- plyr::ddply(allData, plyr::.(dateTime), 
                         WriteNcTimeSlice, 
                         outPath, varianceFunction, 
                         .parallel=(foreach::getDoParWorkers() > 1 ), .inform=TRUE )
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


