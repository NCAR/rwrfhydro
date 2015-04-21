#' Make timeslices from active USGS discharge data files. 
#' 
#' @param realTimeFiles Character vector of active RData format files to be processed. 
#' @param outPath Character, the directory path where ncdf files are to be written. 
#' @param nearestMin Numeric, the time resolution to which the observation times are rounded 
#' and the netcdf timeslice files are to be written. Must evenly divide 60. 
#' @param oldestTime POSIXct, the date BEFORE which data will be ignored.
#' @param qcFunction Function, used to apply quality control procedures to the timeslice in 
#' metric units. Note that this QC can only use information from the current time. QC 
#' procedures involving the temporal domain will be applied elsewhere. 
#' @param varianceFunction Function, used to derive the observation variance from 
#' a dataframe with the following columns: \code{site_no}, \code{dateTime}, \code{code}, 
#' @param processedInputPath Character, a directory path to which proccessed input files are 
#' moved upon successful processing to ncdf timeslice files. 
#' \code{queryTime}, and \code{discharge.cms}. The function accepts the dataframe and 
#' returns the data frame with the new \code{variance} column. 
#' @return A dataframe with two columns: \code{POSIXct} and \code{filename} which given the
#' time of the timeslice and the corresponding file name with full path. 
#' @examples
#' realTimeFiles <- list.files(pattern='huc.*.RData', 
#'                             path='~/usgsStreamData/realTimeData', 
#'                             full.names=TRUE)
#' outPath = '~/usgsStreamData/timeSliceData/'
#' library(doMC)
#' registerDoMC(4)

#' ret1 <- MkUsgsTimeSlice( realTimeFiles[1:21], outPath=outPath, 
#'                          oldest=as.POSIXct('2015-04-15 00:00:00', tz='UTC') )
#' nrow(ret1)
#' 
#' ret1 <- MkUsgsTimeSlice( realTimeFiles[1:21], outPath=outPath )
#' nrow(ret1)
#' ncdump(ret1$V1[230])
#' ret2 <- MkUsgsTimeSlice( realTimeFiles[22:42], outPath=outPath )
#' ncdump(ret1$V1[230])
#' 
#' ret1 <- MkUsgsTimeSlice( realTimeFiles, outPath=outPath, 
#'                         oldest=as.POSIXct('2015-04-15 00:00:00', tz='UTC') )
#' ncdump(ret1[1,2])
MkUsgsTimeSlice <- function( realTimeFiles, outPath, 
                             nearestMin=5, 
                             oldestTime=NULL,
                             qcFunction, 
                             varianceFunction, 
                             processedInputPath){

  ## get all the active data from the specified files
  GetActiveData <- function(file) {
    load(file)
    data$queryTime <- attr(data,'queryTime')
    data <- if(is.null(oldestTime)) {
      subset(data, !is.na(X_00060_00011))  ## remove missing data.
    } else {
      if(!('POSIXct' %in% class(oldestTime))) 
        warning("The oldestTime argument must be POSIXct class.", immediate.=TRUE)
      subset(data, !is.na(X_00060_00011) & dateTime >= oldestTime)  ## remove missing data.
    }
    data
  }
  allData <- plyr::ldply(NamedList(realTimeFiles), GetActiveData, 
                         .parallel=(foreach::getDoParWorkers() > 1 ))  
  
  ## transform the time "granularity"
  ## files are written to the nearest Nth minute.
  allData$dateTime <- RoundMinutes(allData$dateTime, nearest=nearestMin)
  if(any(is.na(allData$dateTime))) warning("NAs in dateTime", immediate.=TRUE)
  #print(table(allData$dateTime))
  
  ## discharge units: output must be in cms (m3/s)
  allData$discharge.cms <- allData$X_00060_00011 * cfs2cms
  
  ## change the code name
  allData <- plyr::rename(allData, c('X_00060_00011_cd'='code') )

  ## move missing codes to blanks?
  allData$code[which(is.na(allData$code))] <- '-'

  ## remove unused variables for speed/memory
  allData <- within( allData, {.id <- agency_cd <- tz_cd  <- X_00060_00011 <- NULL})

  ## QC Active data
  ## filter by product codes
  ## filter duplicate observations per indiv time
  ## convert units. 
#  allData <- qcFunction(allData)  
  
  ## apply the variance function, fudging for now.
#  allData <- varainceFunction(allData)
  allData$variance <- allData$discharge.cms * .1 

  ## for testing outside of plyr
  #WriteNcTimeSlice(subset(allData, dateTime == allData$dateTime[1]), outPath = outPath)
  
  ## "slice" the dataframe by time and pass for writing to ncdf. 
  ## this can be done in parallel.
  outList <- plyr::ddply(allData, plyr::.(dateTime), 
                         WriteNcTimeSlice, 
                         outPath, 
                         .parallel=(foreach::getDoParWorkers() > 1 ) ) #, .inform=TRUE )
  names(outList) <- c('POSIXct', 'file')
  outList  
}

#' Round a POSIXct time to the nearest mth minute.
#' 
#' @param POSIXct, POSIXct class vector. 
#' @param nearest, Numeric, the time resolution to which the passed times are rounded. 
#' Must evenly divide 60. 
#' @return A vector of POSIXct class corresponding to the input, rounded to the nearest 
#' \code{nearest} minutes.
#' @examples
#' print(time <- Sys.time()+lubridate::minutes(1:10))
#' RoundMinutes(time, nearest=5)
RoundMinutes <- function(POSIXct, nearest=5) {
  if((60 %% nearest) != 0) 
    warning(paste0("The nearest argument (passed: ",nearestMin,
                   ") is mean to divide 60 with no remainder."),
            immediate.=TRUE)
  nearestInv <- 1./nearest
  theMin <- as.numeric(format(POSIXct,'%M')) + as.numeric(format(POSIXct,'%S'))/60
  roundMin <- (round(theMin * nearestInv) / nearestInv)
  diffMin <- roundMin - theMin
  #lubridate::round_date(POSIXct,'minute') + lubridate::seconds(round(diffMin*60))
  lubridate::round_date(POSIXct,'minute') + lubridate::minutes(round(diffMin))
}