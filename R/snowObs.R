#' Get and unpack National Water Center SWE and snow depth 
#' point observations for a given hour.
#' 
#' \code{getNWCSnowPoints} Get and unpack National Water Center
#' snow observations for a given hour and unpack the data. Data
#' is stored in text file format.
#' 
#' @param datePOSIXct the date in POSIXct format for which the
#'  data is desired. The format needs to be YYYY-MM-DD HH:MM:SS.
#' @param outDir The directory where the data is to be archived.
#'  This directory is checked to see if the data exist when
#'  overwrite=FALSE
#' @param overwrite when false: If depth and SWE files exist on
#'  disk, don't grab the zip file. \cr When false: If the zip
#'  file exists on disk but depth and SWE files don't, just unzip
#'  the zip files. \cr When true: Pull new zip and overwrite any
#'  existing files with the same date.
#' @param quiet logical passed to curl , to show it's progress.
#' @return Logical was the file "got".
#' @examples
#' NWCSnowGot <- getNWCSnowPoints(as.POSIXct('2015-02-01 12:00:00'),
#'  outDir = '/home/karsten/snow_obs')
#' @keywords IO
#' @concept OBSERVATION NWC
#' @family OBSERVATION
#' @export
getNWCSnowPoints <- function(datePOSIXct, outDir='.', overwrite=FALSE,
                             quiet=TRUE){
  #Date parameters
  dYYYYMMDDHH <- strftime(datePOSIXct, "%Y%m%d%H")
  date2 <- strftime(datePOSIXct, "%Y-%m-%d %H:%M:%S")
  
  #SWE zip file on NWC FTP
  sweFile0 <- paste0('swe_',dYYYYMMDDHH,'.txt.gz')
  #Snow depth file on NWC FTP
  depthFile0 <- paste0('snowdepth_',dYYYYMMDDHH,'.txt.gz')
  
  #Go to the correct directory for archiving the data
  origDir <- getwd()
  setwd(outDir)

  #SWE Data-----------------------------------------
  #If gz SWE or snow depth files exists on system, skip download unless told
  #to override
  if( (file.exists(sweFile0) | file.exists(depthFile0)) & !overwrite) return(0)
  
  if(!file.exists(sweFile0) | overwrite){
    theURL <- paste0('ftp://ftp.nohrsc.noaa.gov/pub/data/snow/',
                     sweFile0)
    if(!quiet) print(paste('Pulling SWE Obs for ', date2))
    try(curl::curl_download(theURL, sweFile0, quiet=quiet))
    if(!file.exists(sweFile0)){
      warning(paste0('Error: File not obtained via FTP: ', sweFile0))
      setwd(origDir)
      return(FALSE)
    }
  }
  
  #Snow Depth Data-----------------------------------
  #Already checked for either file existence earlier, will not
  #repeat here
  if(!file.exists(depthFile0) | overwrite){
    theURL <- paste0('ftp://ftp.nohrsc.noaa.gov/pub/data/snow/',
                     depthFile0)
    if(!quiet) print(paste('Pulling Snow Depth Obs for ', date2))
    try(curl::curl_download(theURL, depthFile0, quiet=quiet))
    if(!file.exists(depthFile0)){
      warning(paste0('Error: File not obtained via FTP: ', depthFile0))
      setwd(origDir)
      return(FALSE)
    }
  }
  setwd(origDir)
  TRUE
}