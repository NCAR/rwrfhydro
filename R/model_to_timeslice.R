
#' Process model CHANOBS to observation timeSlice format for nudging DA. 
#' 
#' @param files Character vector path/file to CHANOBS files. 
#' @param sliceResolutionMin Integer must be <= 60 and evenly divide 60.
#' @param outputDir Character the directory where the ouput files are to be written
#' 
#' @examples 
#' \dontrun{
#'   files <- list.files('~/WRF_Hydro/testChanobs/', pattern = 'CHANOBS_DOMAIN', full.names=TRUE)
#'   sliceResolutionMin <- 12
#'   outputDir <- '~/WRF_Hydro/testChanobs/timeSliceTest/'
#'   ChanObsToTimeSlice(files, sliceResolutionMin, outputDir) 
#' } 
#' @keywords manip IO
#' @concept nudging 
#' @family nudging
#' @export
ChanObsToTimeSlice <- function(files, sliceResolutionMin, outputDir, na.rm=FALSE, parallel=FALSE) {
  
  if(sliceResolutionMin > 60 | (60 %% sliceResolutionMin)!=0 )
      warning('silceResolution is too large OR does not divide 60 evenly.', immediate.=TRUE)
  
  fileBase <- basename(files)
  fileDf <- data.frame(files=files, stringsAsFactors=FALSE)  
  
  GetChanObs <- function(chobsFile, code=100) {
    chobs <- as.data.frame(GetNcdfFile(chobsFile, quiet=TRUE)[,c('station_id', 'time_observation', 'streamflow')])
    if(na.rm) chobs <- subset(chobs, !is.na(streamflow))
    
    ## the times are butchered in the file for restarts. This is how it should work but it does NOT.
    #ncid <- ncdf4::nc_open(chobsFile)
    #origin <- substr(ncid$var$time_observation$units, 15, 30)
    #tz <- substr(ncid$var$time_observation$units, 32, 34)
    #ncdf4::nc_close(ncid)
    #chobs$time_observation <- as.POSIXct(origin, tz=tz) + chobs$time_observation
    ## heres the temp fix: just use the file name
    chobs$time_observation <-
      as.POSIXct(substr(basename(chobsFile),1,12), format='%Y%m%d%H%M', tz='UTC')
    
    ## Need these names
    ## \code{site_no}, \code{dateTime}, \code{code}, \code{queryTime}, \code{discharge.cms}
    renamer <- c('station_id'='site_no', 'time_observation'='dateTime', 'streamflow'='discharge.cms')
    chobs <- plyr::rename(chobs, renamer)
    chobs$queryTime <- lubridate::with_tz(file.mtime(chobsFile), tz='UTC')
    chobs$code <- code
    chobs
  }
  
  MkTimeSlice <- function(sliceDf) {
    chanObsSlice <- plyr::ldply(as.list(sliceDf$files), GetChanObs)
    chanObsSlice$dateTimeRound <- format(RoundMinutes(chanObsSlice$dateTime, nearest=sliceResolutionMin),
                                         '%Y-%m-%d_%H:%M:%S')
    WriteNcTimeSlice(chanObsSlice, outputDir, sliceResolutionMin) 
  }
  
  plyr::dlply(fileDf, plyr::.(files), MkTimeSlice, .parallel=parallel)
}

