## reprocess on saudi

if (FALSE) {
  ##/opt/R-3.2.0/bin/R
options(warn=1)
devtools::load_all('~/R/jlm_lib/rwrfhydro')
options(warn=2)

realTimeFiles <- list.files(pattern='huc.*.RData', 
                               path='~/usgsStreamData/realTimeData', 
                            full.names=TRUE)

realTimeFiles <- list.files(pattern='2015-11-24_0[1-2].*.huc.*.RData', 
                               path='~/usgsStreamData/realTimeData', 
                               full.names=TRUE)


#realTimeFiles <- tail(realTimeFiles,21*7*24*4)
#realTimeFiles <- tail(realTimeFiles,24*4)
outPath = '~/usgsStreamData/timeSliceData15MinQC/'
library(doMC)
registerDoMC(1)  
#I'm worried about using too much memory, when I run this on all 
#previously collected data, so break up the problem
chunkSize <- 500
chunkDf <- data.frame( ind = 0:(length(realTimeFiles) %/% chunkSize) )
chunkDf <- within(chunkDf, { start = (ind)*chunkSize+1
                             end   = pmin( (ind+1)*chunkSize, length(realTimeFiles)) } )
    
hostname <- system("hostname", intern=TRUE)
hostnum <- as.integer(substr(hostname, nchar(hostname), 99))
hostnum
for (ii in 1:nrow(chunkDf) ) {
##for (ii in 199:nrow(chunkDf) ) {
#for (ii in ((hostnum-1)*100+(1:100)) )  {
  print(chunkDf$start[ii])
  ret1 <- MkUsgsTimeSlice( realTimeFiles[chunkDf$start[ii]:chunkDf$end[ii]], 
                          outPath=outPath, nearest=15,
                          oldest=as.POSIXct('2015-04-15 00:00:00', tz='UTC')
                          )
}

}
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
    theTry <- try(load(file))
    if(class(theTry)=='try-error' | class(data)=='function') {
      print(file)
      return(NULL)
    }
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
  allData$quality <- QcUsgsTimeSlice(allData)  

  ## apply the variance function, fudging for now.
#  allData <- varainceFunction(allData)
#  allData$variance <- allData$discharge.cms * .1 

  ## "slice" the dataframe by time and pass for writing to ncdf. 
  ## this can be done in parallel.
  ## for testing outside of plyr:
  ##WriteNcTimeSlice(subset(allData, dateTimeRound == allData$dateTimeRound[1]), outPath = outPath,
  ##                 sliceResolution = nearestMin)
  ##stop()
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
QcUsgsTimeSlice <- function(dataDf) {
  ## comments base on current version of code from Zhengtao and Oubeid.
  ##
  ##%STATUS_CODES =
  ##  (
  ##   '--'  => 'Parameter not determined',
  ##   '***' => 'Temporarily unavailable',
  ##   'Bkw' => 'Flow affected by backwater',
  ##   'Dis' => 'Data-collection discontinued',
  ##   'Dry' => 'Dry',
  ##   'Eqp' => 'Equipment malfunction',
  ##   'Fld' => 'Flood damage',
  ##   'Ice' => 'Ice affected',
  ##   'Mnt' => 'Maintenance in progress',
  ##   'Pr'  => 'Partial-record site',
  ##   'Rat' => 'Rating being developed or revised',
  ##   'Ssn' => 'Parameter monitored seasonally',
  ##   'ZFl' => 'Zero flow',
  ##  );

  ##Also "approval" codes applicable to both UV and DV data:
  ##%DATA_AGING_CODES =
  ##  (
  ##   'P' => ['0', 'P', 'Provisional data subject to revision.'],
  ##   'A' => ['0', 'A', 'Approved for publication -- Processing and review
  ##completed.'],
  ##  );
  
  ##There are also unit value quality codes the vast majority of which you should never see.  The ones in red below "may" be the only one you can expect to see in the public view:
  ##
  ##%UV_REMARKS =
  ##  (
  ##   # -- Threshold flags
  ##   'H' => ['1', 'H', 'Value exceeds "very high" threshold.'],
  ##   'h' => ['1', 'h', 'Value exceeds "high" threshold.'],
  ##   'l' => ['1', 'l', 'Value exceeds "low" threshold.'],
  ##   'L' => ['1', 'L', 'Value exceeds "very low" threshold.'],
  ##   'I' => ['1', 'I', 'Value exceeds "very rapid increase" threshold.'],
  ##   'i' => ['1', 'i', 'Value exceeds "rapid increase" threshold.'],
  ##   'd' => ['1', 'd', 'Value exceeds "rapid decrease" threshold.'],
  ##   'D' => ['1', 'D', 'Value exceeds "very rapid decrease" threshold.'],
  ##   'T' => ['1', 'T', 'Value exceeds "standard difference" threshold.'],
  ##
  ##   # -- Source Flags
  ##   'o' => ['1', 'o', 'Value was observed in the field.'],
  ##   'a' => ['1', 'a', 'Value is from paper tape.'],
  ##   's' => ['1', 's', 'Value is from a DCP.'],
  ##   '~' => ['1', '~', 'Value is a system interpolated value.'],
  ##   'g' => ['1', 'g', 'Value recorded by data logger.'],
  ##   'c' => ['1', 'c', 'Value recorded on strip chart.'],
  ##   'p' => ['1', 'p', 'Value received by telephone transmission.'],
  ##   'r' => ['1', 'r', 'Value received by radio transmission.'],
  ##   'f' => ['1', 'f', 'Value received by machine readable file.'],
  ##   'z' => ['1', 'z', 'Value received from backup recorder.'],
  
  ##   # -- Processing Status Flags
  ##   '*' => ['1', '*', 'Value was edited by USGS personnel.'],
  
  ##   # -- Other Flags (historical UVs only)
  ##   'S' => ['1', 'S', 'Value could be a result of a stuck recording instrument.'],
  ##   'M' => ['1', 'M', 'Redundant satellite value doesnt match original value.'],
  ##   'Q' => ['1', 'Q', 'Value was a satellite random transmission.'],
  ##   'V' => ['1', 'V', 'Value was an alert value.'],
  
  ##   # -- UV remarks
  ##   '&' => ['1', '&', 'Value was computed from affected unit values by unspecified reasons.'],
  ##   '<' => ['0', '<', 'Actual value is known to be less than reported value.'],
  ##   '>' => ['0', '>', 'Actual value is known to be greater than reported value.'],
  ##   'C' => ['1', 'C', 'Value is affected by ice at the measurement site.'],
  ##   'B' => ['1', 'B', 'Value is affected by backwater at the measurement site.'],
  ##   'E' => ['0', 'e', 'Value was computed from estimated unit values.'],
  ##   'e' => ['0', 'e', 'Value has been estimated.'],
  ##   'F' => ['1', 'F', 'Value was modified due to automated filtering.'],
  ##   'K' => ['1', 'K', 'Value is affected by instrument calibration drift.'],
  ##   'R' => ['1', 'R', 'Rating is undefined for this value.'],
  ##   'X' => ['1', 'X', 'Value is erroneous and will not be used.'],
  ##  );

  ## Notes from Oubeid:: 
  ## I think we should set the ?e? (estimated) to 50% since we don?t know how the estimation was done,
  ## set the ?&? to 25%, and may be the ?*? to 5% since the data could be incomplete. The remainder
  ## set to 0 for the time being. That should wrap up this QC at this stage. We can always revisit
  ## them in the future as we progress.

  ## I made contact today with the USGS staff on location here and inform them that I will visit them
  ## next week for question about the rating curve gages. May be we can visit them together.

  ## Let?s stick with a weight of ?0? then for ?***?. I don?t have a lot of confidence on the data
  ## appended with that code.

  ## Apparently the code can consist of 2 parts separated by whitespace
print(table(dataDf$code))
  dataDf$code <- dataDf$qualifier1 <- trimws(dataDf$code)
  dataDf$qualifier2 <- ''
  wh2Qual <- grep(' ',dataDf$code)
  dataDf$qualifier1[wh2Qual] <- plyr::laply(strsplit(dataDf$code[wh2Qual],' '),'[[',1)
  dataDf$qualifier2[wh2Qual] <- plyr::laply(strsplit(dataDf$code[wh2Qual],' '),'[[',2)

  ## assume the worst and then recover what fits out mental model of this chaos.
  ## JLM: a different assumption off the bat.
  dataDf$quality <- dataDf$discharge.cms * 0
  
  ## Recover quality for valid/sane flow range
  ## JLM this deviates from their script
  ## in their script they consider 0 a valid flow, but I'd argue otherwise...
  ## why would any streams with no flow be gaged?? Then again, drought. IT's a really dicey value,
  ## JLM also limit maximum flows to approx twice what I believe is the largest gaged flow
  ## on MS river *2 
  ## http://nwis.waterdata.usgs.gov/nwis/peak?site_no=07374000&agency_cd=USGS&format=html 
  ## baton rouge 1945: 1,473,000cfs=41,711cms
  ## multiply it roughly by 2
  isValidFlow <- dataDf$discharge.cms > 0 & dataDf$discharge.cms < 90000

  wh100 <- which(isValidFlow                       &
                 dataDf$qualifier1 %in% c("A","P") &
                 dataDf$qualifier2 == ''           )
  if(length(wh100)) dataDf$quality[wh100] <- 100

  wh50 <- which(isValidFlow                       &
                dataDf$qualifier1 %in% c("A","P") &
                dataDf$qualifier2 == 'e'          )
  if(length(wh50)) dataDf$quality[wh50] <- 50

  wh25 <- which(isValidFlow                       &
                dataDf$qualifier1 %in% c("A","P") &
                dataDf$qualifier2 == '&'          )
  if(length(wh25)) dataDf$quality[wh25] <- 25

  wh05 <- which(isValidFlow                       &
                dataDf$qualifier1 %in% c("A","P") &
                dataDf$qualifier2 == '*'          )
  if(length(wh05)) dataDf$quality[wh05] <- 5

  ## dont need isValidFlow in this since zero can be applied elsewhere
  wh0  <- which(!(dataDf$qualifier1 %in% c("A","P"))        |
                !(dataDf$qualifier2 %in% c('e','&','*','')) )
  if(length(wh0)) dataDf$quality[wh0] <- 0

  dataDf$quality
}



##===================================================================
## collect / gather / cull the station ids from the timeslices
## so that we have a comprehensive list of what is coming in.

if(FALSE) {
devtools::load_all('~/R/jlm_lib/rwrfhydro')
files <- list.files(path="~/usgsStreamData/timeSliceData/", pattern='2015', full=TRUE)
cullFile <- CullUsgsGageIds(files, cullFile='~/usgsStreamData/culledUsgsGageIdsNOTQC.rsav')

devtools::load_all('~/R/jlm_lib/rwrfhydro')
files <- list.files(path="~/usgsStreamData/timeSliceData15MinQC/", pattern='2015-04', full=TRUE)
cullFile <- CullUsgsGageIds(files)
}

CullUsgsGageIds <- function(files,
                            goBackNDays = 2,
                            cullFile = '~/usgsStreamData/culledUsgsGageIds.rsav') {
  if(file.exists(cullFile)) load(cullFile)

  ## subset the files in time
  if(exists('lastTimeSlice')) {    
    lastTimeSliceDate <- as.POSIXct(plyr::laply(strsplit(basename(lastTimeSlice),'\\.'), '[[', 1),
                                    format='%Y-%m-%d_%H:%M:%S', tz='UTC')
    fileDates <- as.POSIXct(plyr::laply(strsplit(basename(files),'\\.'), '[[', 1),
                            format='%Y-%m-%d_%H:%M:%S', tz='UTC')
    whSinceLast <- which(fileDates >= (lastTimeSliceDate - goBackNDays * (24*60*60) ))
    files <- files[whichSinceLast]
  }
  lastTimeSlice <- tail(files,1)
                         
  if(!exists('culledGageIds')) culledGageIds = c()

  for(ff in files)
    culledGageIds <- union(culledGageIds, ncdump(ff, 'stationId', quiet=TRUE) )
  
  save(culledGageIds, lastTimeSlice, file=cullFile)
  cullFile
}
