#' Read Ameriflux data into R dataframe.
#' 
#' \code{ReadAmeriflux} reads pre-downloaded Ameriflux BASE data table (BASE data 
#' product CSV file) and creates a dataframe.
#' 
#' \code{ReadAmeriflux} reads Ameriflux BASE data product CSV file and outputs a 
#' dataframe with consistent date and data columns for use with other rwrfhydro tools.
#' 
#' @param inFile Path to the CSV file (e.g., "AMF_US-NR1_BASE_HH_9-1.csv").
#' @param tz The time zone for the site (generally Ameriflux data is in local time).
#' @param useEndTime By default, a POSIXct column will be defined based on the 
#' "TIMESTAMP_START" column. If you prefer to use "TIMESTAMP_END", set this to TRUE.
#' @return dataframe
#' @examples
#' \dontrun{
#' amf.NR <- ReadAmeriflux('~/wrfhydroTestCases/Fourmile_Creek_testcase_v2.0/OBS/AMF_US-NR1_BASE_HH_9-1.csv',
#'                         tz='America/Denver')
#' }
#' @keywords IO
#' @concept Ameriflux
#' @family Ameriflux
#' @export
ReadAmeriflux <- function(inFile, tz, useEndTime=FALSE) {
    
    tmpData <- read.table(inFile, sep = ",", skip=2, header=TRUE,
                          na.strings=c(-6999,-9999), strip.white=T, 
                          stringsAsFactors=FALSE)
    tmpData$POSIXct <- as.POSIXct(as.character(tmpData$TIMESTAMP_START),
                                  format="%Y%m%d%H%M", tz=tz)
    tmpData$wy <- CalcWaterYear(tmpData$POSIXct)
    return(tmpData)
    
}

#' Read standard-format NetCDF data downloaded from Ameriflux
#' 
#' \code{ReadAmerifluxNCOldFormat} reads Ameriflux data table (Level 2 standardized
#' NetCDF file) and creates a dataframe. OLDER FORMAT ONLY - NO LONGER SUPPORTED. 
#' 
#' \code{ReadAmerifluxNCOldFormat} reads an Ameriflux Level 2 standardized NetCDF file
#' and outputs a dataframe with consistent date and data columns for use with
#' other rwrfhydro tools. OLDER FORMAT ONLY - NO LONGER SUPPORTED. Please use 
#' ReadAmeriflux for the latest BASE format support.
#' 
#' @param pathFluxData The full pathname to the flux time series NetCDF file as
#'   downloaded from an Ameriflux data server (OLD FORMAT).
#' @param timeZone The time zone for the flux site. Time zone name must be
#'   R-friendly for your current OS. See:
#'   \url{http://stat.ethz.ch/R-manual/R-devel/library/base/html/timezones.html}. 
#'   Provide this OR a UTC offset. Note that Ameriflux data is (allegedly!) in local
#'   time without daylight savings. 
#' @param utcOffset The UTC offset for the flux site's local time. These should be in 
#' positive or negative hours (e.g., -7 for "America/Denver" with no DST shift). 
#' These values will supercede any provided time zones. See tzLookup for a list of 
#' time zone UTC offsets.
#' @return A dataframe containing the Ameriflux data.
#'   
#' @examples
#' ## Takes a NetCDF file downloaded from the ORNL Amerifux website for US-NC2
#' ## (North Carolina Loblolly Pine) and returns a dataframe.
#' 
#' \dontrun{
#' obsFlux30min.usnc2 <- 
#'   ReadAmerifluxNCOldFormat("../OBS/FLUX/AMF_USNC2_2005_L2_WG_V003.nc", utcOffset=-5)
#' }
#' @keywords IO
#' @concept dataGet
#' @family obsDataReads
#' @export
ReadAmerifluxNCOldFormat <- function(pathFluxData, timeZone=NULL, utcOffset=NULL) {
    if ((is.null(utcOffset)) & (is.null(timeZone))) {
        stop("No time zone or UTC offset provided. Please determine the site's local time zone and re-run the tool.")
    }
    ncFile <- ncdf4::nc_open(pathFluxData)
    nc <- ncFile$nvars
    nr <- ncFile$var[[1]]$varsize
    outDf <- as.data.frame(matrix(nrow=nr, ncol=nc))
    ncVarList <- list()
    for (i in 1:nc ) {
        ncVar <- ncFile$var[[i]]
        ncVarList[i] <- ncVar$name
        outDf[,i] <- ncdf4::ncvar_get( ncFile, ncVar )
    }
    colnames(outDf) <- ncVarList
    ncdf4::nc_close(ncFile)
    if (is.null(utcOffset)) {
        outDf$POSIXct <- as.POSIXct( paste(as.character(outDf$YEAR), as.character(outDf$DOY),
                                           as.character(ifelse(substr(outDf$HRMIN,1,
                                                                      nchar(outDf$HRMIN)-2)=='', 
                                                               "00", substr(outDf$HRMIN,1,
                                                                            nchar(outDf$HRMIN)-2))),
                                           as.character(substr(outDf$HRMIN,
                                                               nchar(outDf$HRMIN)-1,
                                                               nchar(outDf$HRMIN))), sep="-"),
                                     format="%Y-%j-%H-%M", tz=timeZone )
    } else {
        outDf$POSIXct <- as.POSIXct( paste(as.character(outDf$YEAR), as.character(outDf$DOY),
                                           as.character(ifelse(substr(outDf$HRMIN,1,
                                                                      nchar(outDf$HRMIN)-2)=='', 
                                                               "00", substr(outDf$HRMIN,1,
                                                                            nchar(outDf$HRMIN)-2))),
                                           as.character(substr(outDf$HRMIN,
                                                               nchar(outDf$HRMIN)-1,
                                                               nchar(outDf$HRMIN))), sep="-"),
                                     format="%Y-%j-%H-%M", tz="UTC" ) - (utcOffset*3600)
    }
    outDf$wy <- CalcWaterYear(outDf$POSIXct)
    outDf
}

#' Read standard-format CSV data downloaded from Ameriflux
#' 
#' \code{ReadAmerifluxCSVOldFormat} reads Ameriflux data table (Level 2 standardized CSV
#' file) and creates a dataframe. OLDER FORMAT ONLY - NO LONGER SUPPORTED.
#' 
#' \code{ReadAmerifluxCSVOldFormat} reads an Ameriflux Level 2 standardized CSV file and
#' outputs a dataframe with consistent date and data columns for use with other
#' rwrfhydro tools. OLDER FORMAT ONLY - NO LONGER SUPPORTED. Please use 
#' ReadAmeriflux for the latest BASE format support.
#' 
#' @param pathFluxData The full pathname to the flux time series CSV file as
#'   downloaded from an Ameriflux data server. (OLD FORMAT)
#' @param timeZone The time zone for the flux site. Time zone name must be
#'   R-friendly for your current OS. See:
#'   \url{http://stat.ethz.ch/R-manual/R-devel/library/base/html/timezones.html}. 
#'   Provide this OR a UTC offset. Note that Ameriflux data is (allegedly!) in local
#'   time without daylight savings. 
#' @param utcOffset The UTC offset for the flux site's local time. These should be in 
#' positive or negative hours (e.g., -7 for "America/Denver" with no DST shift). 
#' These values will supercede any provided time zones. See tzLookup for a list of 
#' time zone UTC offsets.
#' @return A dataframe containing the Ameriflux data.
#'   
#' @examples
#' ## Takes a CSV file downloaded from the ORNL Amerifux website for US-NR1 (Niwot Ridge)
#' ## and returns a dataframe.
#' 
#' \dontrun{
#' obsFlux30min.usnr1 <- 
#'   ReadAmerifluxCSV("../OBS/FLUX/AMF_USNR1_2013_L2_GF_V008.csv", utcOffset=-7)
#' }
#' @keywords IO
#' @concept dataGet
#' @family obsDataReads
#' @export
ReadAmerifluxCSVOldFormat <- function(pathFluxData, timeZone=NULL, utcOffset=NULL) {
    if ((is.null(utcOffset)) & (is.null(timeZone))) {
        stop("No time zone or UTC offset provided. Please determine the site's local time zone and re-run the tool.")
    }
    outDf <- read.table(pathFluxData, sep=",", skip=20, na.strings=c(-6999,-9999), 
                        strip.white=T)
    outDf.head <- read.table(pathFluxData, sep=",", skip=17, nrows=1, strip.white=T)
    colnames(outDf) <- as.matrix(outDf.head)[1,]
    if (is.null(utcOffset)) {
        outDf$POSIXct <- as.POSIXct( paste(as.character(outDf$YEAR), as.character(outDf$DOY),
                                           as.character(ifelse(substr(outDf$HRMIN,1,nchar(outDf$HRMIN)-2)=='', 
                                                               "00", substr(outDf$HRMIN,1,nchar(outDf$HRMIN)-2))),
                                           as.character(substr(outDf$HRMIN,nchar(outDf$HRMIN)-1,
                                                               nchar(outDf$HRMIN))), sep="-"),
                                     format="%Y-%j-%H-%M", tz=timeZone )
    } else {
        outDf$POSIXct <- as.POSIXct( paste(as.character(outDf$YEAR), as.character(outDf$DOY),
                                           as.character(ifelse(substr(outDf$HRMIN,1,
                                                                      nchar(outDf$HRMIN)-2)=='', 
                                                               "00", substr(outDf$HRMIN,1,
                                                                            nchar(outDf$HRMIN)-2))),
                                           as.character(substr(outDf$HRMIN,nchar(outDf$HRMIN)-1,
                                                               nchar(outDf$HRMIN))), sep="-"),
                                     format="%Y-%j-%H-%M", tz="UTC" ) - (utcOffset*3600)
    }
    outDf$wy <- CalcWaterYear(outDf$POSIXct)
    outDf
}

#' Ameriflux site metadata
#' 
#' Metadata for the Ameriflux sites (as of June 2015) including lat/lon and timezone.
#' 
#' @usage
#'  amfMeta
#'  
#' @format
#' data.frame:  166 obs. of  11 variables:
#' \describe{
#'  \item{name}{Site name}
#'  \item{id_txt}{Site ID (used to access datasets)}
#'  \item{state}{State where site is located}
#'  \item{cntry}{Country where site is located}
#'  \item{lon}{Site longitude}
#'  \item{lat}{Site latitude}
#'  \item{tz_us}{Time zone for US sites only}
#'  \item{tz_glob}{General time zones for all sites (no daylight savings)}
#'  \item{UTC_offset}{UTC offset w/o daylight savings}
#'  \item{UTC_DST_offset}{UTC offset with daylight savings}
#'  \item{UTC_offset_int}{UTC offset w/o daylight savings in integer format 
#'  (for manual time adjustments to UTC)}
#' }
#' 
#' @family Ameriflux
#' @concept data
#' @keywords data
"amfMeta"

#' Time zone lookup table
#' 
#' Lookup table to match time zones to UTC offsets.
#' 
#' @usage
#'  tzLookup
#'  
#' @format
#' data.frame:  539 obs. of  3 variables:
#' \describe{
#'  \item{TZ}{Time zone name}
#'  \item{UTC_offset}{UTC offset w/o daylight savings}
#'  \item{UTC_DST_offset}{UTC offset with daylight savings}
#' }
#' 
#' @section Citation:
#' Originator: Paul Eggert/IANA \cr
#' Publication_Date: 20140325 \cr
#' Title: Time Zone Database \cr
#' List of time zones in the tz database release 2014b
#' Online_Linkage: \link[http://en.wikipedia.org/wiki/List_of_tz_database_time_zones]{
#' http://en.wikipedia.org/wiki/List_of_tz_database_time_zones} \cr
#' Online_Linkage: \link[http://www.iana.org/time-zones]{http://www.iana.org/time-zones} \cr
#' @family Ameriflux
#' @concept data
#' @keywords data
"tzLookup"


