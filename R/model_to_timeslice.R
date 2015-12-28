#' Process model *CHANOBS* to observation timeSlice format for nudging DA. 
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
#' @export
ChanObsToTimeSlice <- function(files, sliceResolutionMin, outputDir) {
  
  if(sliceResolutionMin > 60 | (60 %% sliceResolutionMin)!=0 )
      warning('silceResolution is too large OR does not deivide 60 evenly.', immediate.=TRUE)
  
  fileBase <- basename(files)
  ## get the times for all files
  fileTimes <- as.POSIXct(substr(fileBase,1,12), format='%Y%m%d%H%M', tz='UTC')
  fileRoundTimes <- RoundMinutes(fileTimes)
    
  fileDf <- data.frame(file=files, time=fileTimes, dateTimeRound=fileRoundTimes, stringsAsFactors=FALSE)  
  
  GetChanObs <- function(chobsFile, code=1) {
    chobs <- as.data.frame(GetNcdfFile(chobsFile, quiet=TRUE)[,c('station_id', 'time_observation', 'streamflow')])
    #'\code{site_no}, \code{dateTime}, \code{code}, \code{queryTime}, \code{discharge.cms}, 
    
    ncid <- ncdf4::nc_open(chobsFile)
    origin <- substr(ncid$var$time_observation$units, 15, 30)
    tz <- substr(ncid$var$time_observation$units, 32, 34)
    ncdf4::nc_close(ncid)
    chobs$time_observation <- as.POSIXct(origin, tz=tz) + chobs$time_observation
    
    renamer <- c('station_id'='site_no', 'time_observation'='dateTime', 'streamflow'='discharge.cms')
    chobs <- plyr::rename(chobs, renamer)
    chobs$queryTime <- file.mtime(chobsFile)
    chobs$code <- code
    chobs
  }
  
  MkTimeSlice <- function(sliceDf) {
    chanObsSlice <- plyr::ldply(as.list(sliceDf$file), GetChanObs)
    chanObsSlice$dateTimeRound <- format(sliceDf$dateTimeRound[1], '%Y-%m-%d_%H:%M:%S')
    WriteNcTimeSlice(chanObsSlice, outputDir, sliceResolutionMin) 
  }
  
  plyr::dlply(fileDf, plyr::.(dateTimeRound), MkTimeSlice)
}



# #' frxstPtsToTimeSlice
# #' 
# #' 
# #' @examples 
# files <- list.files('/Users/jamesmcc/WRF_Hydro/testFrxstPts/', pattern='frxst', full.names=TRUE)
# data <- plyr::ldply(NamedList(files), ReadFrxstPts)
# ggplot2::ggplot(data, ggplot2::aes(x=POSIXct, y=q_cms, color=.id)) +
#   ggplot2::geom_line() +
#   ggplot2::facet_wrap(~st_id, ncol=1, scales='free_y') +
#   ggplot2::theme_bw()










