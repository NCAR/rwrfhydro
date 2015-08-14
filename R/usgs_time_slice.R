##===================================================================

#' Make timeslices from active USGS discharge data files.
#' 
#' @param realTimeFiles Character vector of active RData format files to be
#'   processed.
#' @param outPath Character, the directory path where ncdf files are to be
#'   written.
#' @param nearestMin Numeric, the time resolution to which the observation times
#'   are rounded and the netcdf timeslice files are to be written. Must evenly
#'   divide 60.
#' @param oldestTime POSIXct, the date BEFORE which data will be ignored.
#' @param qcFunction Function, used to apply quality control procedures to the
#'   timeslice in metric units. Note that this QC can only use information from
#'   the current time. QC procedures involving the temporal domain will be
#'   applied elsewhere.
#' @param varianceFunction Function, used to derive the observation variance
#'   from a dataframe with the following columns: \code{site_no},
#'   \code{dateTime}, \code{code}, \code{queryTime}, and \code{discharge.cms}.
#'   The function accepts the dataframe and returns the data frame with the new
#'   \code{variance} column.
#' @return A dataframe with two columns: \code{POSIXct} and \code{filename}
#'   which given the time of the timeslice and the corresponding file name with
#'   full path.
#' @examples
#' \dontrun{
#' realTimeFiles <- list.files(pattern='huc.*.RData', 
#'                             path='~/usgsStreamData/realTimeData', 
#'                             full.names=TRUE)
#' outPath = '~/usgsStreamData/timeSliceData/'
#' library(doMC)
#' registerDoMC(4)
#' 
#' ## A first test
#' ret1 <- MkUsgsTimeSlice( realTimeFiles[1:21], outPath=outPath, 
#'                          oldest=as.POSIXct('2015-04-15 00:00:00', tz='UTC') )
#' nrow(ret1)
#' 
#' ## delete the files and see how many more are created without the oldestTime set
#' unlink(ret1$file)
#' ret1 <- MkUsgsTimeSlice( realTimeFiles[1:21], outPath=outPath )
#' nrow(ret1)  ## quite a few more files. 
#' ncdump(ret1$file[230])  ## 27 stations
#' ret2 <- MkUsgsTimeSlice( realTimeFiles[22:42], outPath=outPath )
#' ncdump(ret1$file[230])  ## 58 stations
#' 
#' ## new experiment
#' unlink(unique(c(ret1$file, ret2$file)))
#' ret1 <- MkUsgsTimeSlice( realTimeFiles, outPath=outPath, nearest=60,
#'                         oldest=as.POSIXct('2015-04-15 00:00:00', tz='UTC'))
#' nStn <- 
#'  plyr::ldply(NamedList(ret1$file), 
#'       function(ff) { nc <- ncdump(ff, quiet=TRUE)
#'                      data.frame(nStn=nc$dim$stationId$len,
#'                                 time=as.POSIXct('1970-01-01 00:00:00',tz='UTC') + 
#'                                      nc$dim$time$vals,
#'                                 nUniqueStn = length(unique(nc$dim$stationId$vals)) )},
#'              .parallel=TRUE)
#' library(ggplot2)
#' ggplot(nStn, aes(x=time,y=nStn)) + geom_point(color='red')
#' 
#' 
#' ###############################
#' ## "re-"process on saudi
#   library(rwrfhydro)
#   realTimeFiles <- list.files(pattern='huc.*.RData', 
#                               path='~/usgsStreamData/realTimeData/', 
#                               full.names=TRUE)
#   # apparently not the problem
#   #whRealTimeFileGt0Bytes <- which(file.size(realTimeFiles) !=0)
#   
#   #realTimeFiles <- tail(realTimeFiles,21*7*24*4)
#   #realTimeFiles <- realTimeFiles[941:945]
#   whRT <- grep('2015-05-22', realTimeFiles)[1]
#   realTimeFiles <- realTimeFiles[whRT:length(realTimeFiles)]
#   
#   outPath = '~/usgsStreamData/timeSliceData/'
#   library(doMC)
#   registerDoMC(8)
#   
#    #I'm worried about using too much memory, when I run this on all 
#   #previously collected data, so break up the problem
#    chunkSize <- 1000
#    chunkDf <- data.frame( ind = 0:(length(realTimeFiles) %/% chunkSize) )
#    chunkDf <- within(chunkDf, { start = (ind)*chunkSize+1
#                                 end   = pmin( (ind+1)*chunkSize, length(realTimeFiles)) } )
#    
#  for (ii in 1:nrow(chunkDf) ) {
#    print(ii)
#    ret1 <- MkUsgsTimeSlice( realTimeFiles[chunkDf$start[ii]:chunkDf$end[ii]], 
#                             outPath=outPath, nearest=15,
#                             oldest=as.POSIXct('2015-04-15 00:00:00', tz='UTC')
#                           )
#    }
#   
#' 
#' 
#' ## end dontrun }  
#' @family usgs
#' @concept usgs
#' @export
MkUsgsTimeSlice <- function( realTimeFiles, outPath, 
                             nearestMin=5, 
                             oldestTime=NULL,
                             qcFunction, 
                             varianceFunction ){

  ## get all the active data from the specified files
  GetActiveData <- function(file) {
    tryLoad <- try(load(file))
    if(class(tryLoad)=="try-error") return(NULL)
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
  
  ##needs to happen
  allData$site_no <- formatC(allData$site_no, width=15) #width for ncdf output
  
  ## transform the time "granularity"
  ## files are written to the nearest Nth minute.
  if(any(is.na(allData$dateTime))) warning("NAs in dateTime", immediate.=TRUE)
  allData$dateTimeRound <- RoundMinutes(allData$dateTime, nearest=nearestMin)
  if(any(is.na(allData$dateTimeRound))) warning("NAs in dateTimeRound", immediate.=TRUE)
  allData$dateTimeRound <- format(allData$dateTimeRound, '%Y-%m-%d_%H:%M:%S', tz='UTC')
  #print(table(allData$dateTime))
  
  ## discharge units: output must be in cms (m3/s)
  allData$discharge.cms <- allData$X_00060_00011 * cfs2cms
  
  ## change the code name
  allData <- plyr::rename(allData, c('X_00060_00011_cd'='code') )

  ## move missing codes to blanks?
  allData$code[which(is.na(allData$code))] <- '-'
  allData$code    <- formatC(allData$code,    width=4)  ## width for ncdf output
  
  ## remove unused variables for speed/memory
  allData <- within( allData, {.id <- agency_cd <- tz_cd  <- X_00060_00011 <- NULL})

  ## QC Active data
  ## filter by product codes
  ## filter duplicate observations per indiv time
  ## convert units. 
#  allData <- qcFunction(allData)  
  
  ## apply the variance function, fudging for now.
#  allData <- varainceFunction(allData)
#  allData$variance <- allData$discharge.cms * .1 

  ## "slice" the dataframe by time and pass for writing to ncdf. 
  ## this can be done in parallel.
  ## for testing outside of plyr:
  ## WriteNcTimeSlice(subset(allData, dateTimeRound == allData$dateTimeRound[1]), outPath = outPath)
  outList <- plyr::ddply(allData, plyr::.(dateTimeRound), 
                         WriteNcTimeSlice, 
                         outPath, 
                         nearestMin,
                         .parallel=(foreach::getDoParWorkers() > 1 ), .inform=TRUE )
                         
  if(length(outList)) names(outList) <- c('POSIXct', 'file')
  outList  
}

##===================================================================

#' Round a POSIXct time to the nearest mth minute.
#' 
#' @param POSIXct, POSIXct class vector.
#' @param nearest, Numeric, the time resolution to which the passed times are
#'   rounded. Must evenly divide 60.
#' @return A vector of POSIXct class corresponding to the input, rounded to the
#'   nearest \code{nearest} minutes.
#' @examples
#' print(time <- Sys.time()+lubridate::seconds(0:30))
#' RoundMinutes(time, nearest=1)
#' @keywords internal
#' @export
RoundMinutes <- function(POSIXct, nearest=5) {
  if((60 %% nearest) != 0) 
    warning(paste0("The nearest argument (passed: ",nearestMin,
                   ") is mean to divide 60 with no remainder."),
            immediate.=TRUE)
  nearestInv <- 1./nearest
  theMin <- as.numeric(format(POSIXct,'%M')) + as.numeric(format(POSIXct,'%S'))/60
  floorDiff <- theMin-floor(theMin)
  whFloor <- which(floorDiff < .5)
  roundMin <- (ceiling(theMin * nearestInv) / nearestInv)
  roundMin[whFloor] <- (floor(theMin * nearestInv) / nearestInv)[whFloor]
  diffMin <- roundMin - theMin
  #lubridate::round_date(POSIXct,'minute') + lubridate::seconds(round(diffMin*60))
  lubridate::round_date(POSIXct,'minute') + lubridate::minutes(round(diffMin))
}

##===================================================================
QcUsgsTimeSlice <- function() {

  # CODES FROM 
  # http://water.usgs.gov/GIS/metadata/usgswrd/XML/nwis_surface_water.xml

  #Instantaneous Value Qualification Code (uv_rmk_cd) 
  #  e     The value has been edited or estimated by USGS personnel
  #  A     The value is affected by ice at the measurement site.
  #  B     The value is affected by backwater at the measurement site.
  #  R     The rating is undefined for this value
  #  &     This value is affected by unspecified reasons.
  #  K     The value is affected by  instrument calibration drift.
  #  X     The value is erroneous. It will not be used.
  #  <     The Value is known to be less than reported value
  #  >     The value is known to be greater than reported value
  #  E     The value was computed from an estimated value
  #  F     The value was modified due to automated filtering.
  
  ## map the instanataneous qualifier to an action to be taken
  ivQualifCdMap <- c(
      'e' = 'ok',           # The value has been edited or estimated by USGS personnel
      'A' = 'remove',       # The value is affected by ice at the measurement site.
      'B' = 'remove',       # The value is affected by backwater at the measurement site.
      'R' = 'xtra.uncert',  # The rating is undefined for this value
      '&' = 'remove',       # This value is affected by unspecified reasons.
      'K' = 'remove',       # The value is affected by  instrument calibration drift.
      'X' = 'remove',       # The value is erroneous. It will not be used.
      '<' = '',             # The Value is known to be less than reported value
      '>' = '',             # The value is known to be greater than reported value
      'E' = '',             # The value was computed from an estimated value
      'F' = ''              # The value was modified due to automated filtering.
  )
  
  
  #Instantaneous and Daily Value Status Codes
  #  Ssn    Parameter monitored seasonally
  #  Ice    Ice affected
  #  Pr     Partial-record site
  #  Rat    Rating being developed or revised
  #  Eqp    Equipment malfunction
  #  Fld    Flood damage
  #  Dis    Data-collection discontinued
  #  Dry    Dry
  #  --     Parameter not determined
  #  Mnt    Maintenance in progress
  #  ZFl    Zero flow
  #  ***    Temporarily unavailable
   
  ivStatusCdMap <- c(
    'Ssn' = '',     #   Parameter monitored seasonally
    'Ice' = '',     #   Ice affected
    'Pr ' = '',     #   Partial-record site
    'Rat' = '',     #   Rating being developed or revised
    'Eqp' = '',     #   Equipment malfunction
    'Fld' = '',     #   Flood damage
    'Dis' = '',     #   Data-collection discontinued
    'Dry' = '',     #   Dry
    '-- ' = '',     #   Parameter not determined
    'Mnt' = '',     #   Maintenance in progress
    'ZFl' = '',     #   Zero flow
    '***' = ''      #   Temporarily unavailable
  )
  
}
