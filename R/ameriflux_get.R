#' Get Ameriflux data for specified sites.
#' 
#' \code{GetAmeriflux} downloads Ameriflux data tables (Level 2 standardized CSV
#' files) and creates a dataframe. Based on Koen Hufkens's Ameriflux download tool.
#' 
#' \code{GetAmeriflux} downloads Ameriflux Level 2 standardized CSV files for
#' specified sites and outputs a dataframe with consistent date and data columns 
#' for use with other rwrfhydro tools. Borrowed generously from Koen Hufkens's
#' Ameriflux download tool: 
#' \link[https://bitbucket.org/khufkens/ameriflux-download-tool]{https://bitbucket.org/
#' khufkens/ameriflux-download-tool}
#' 
#' @param siteIDs A single site ID or vector of site IDs to download and process. Site 
#' IDs should match the standardized AmerifluxIDs (e.g., "US-NR1" for Niwot Ridge). 
#' See amfMeta$id_txt for a list of site IDs (as of June 2015).
#' @param gaps (DEFAULT=TRUE) Boolean flag whether to download data with gaps 
#' (TRUE) or with gaps filled (FALSE). Gap-filled data is not available for all 
#' sites.
#' @param timeZones (OPTIONAL) A single time zone or vector of time zones for each  
#' site provided in the siteIDs. Note that Ameriflux timestamps are (allegedly!)
#' in local time WITHOUT daylight savings time adjustments. If neither timeZones nor
#' utcOffsets are provided, the tool will attempt to lookup the site in the master
#' database (amfMeta) and get a UTC adjustment from there. These offsets were
#' estimated based on site lat/lon coordinates and are provided with a 
#' use-at-your-own-risk(!) caveat.
#' @param utcOffsets (OPTIONAL) A single UTC offset or vector of UTC offsets for each 
#' site provided in the siteIDs. These should be in positive or negative hours 
#' (e.g., -7 for "America/Denver" with no DST shift). These values will supercede 
#' any provided time zones. See tzLookup for a list of time zone UTC offsets. If 
#' neither timeZones nor utcOffsets are provided, the tool will attempt to lookup the 
#' site in the master database (amfMeta) and get a UTC adjustment from there. These 
#' offsets were estimated based on site lat/lon coordinates and are provided with a 
#' use-at-your-own-risk(!) caveat.
#' @param startYr (OPTIONAL) The first year to download files for.
#' @param endYr (OPTIONAL) The last year to download files for.
#' @return dataframe
#' @examples
#' amf.An <- GetAmeriflux(c("US-An1","US-An2","US-An3"), startYr=2009, endYr=2010)
#' @keywords IO
#' @concept Ameriflux
#' @family Ameriflux
#' @export

GetAmeriflux <- function(siteIDs, gaps=TRUE, 
                         timeZones=NULL, utcOffsets=NULL, 
                         startYr=NULL, endYr=NULL) {

  server="ftp://cdiac.ornl.gov/pub/ameriflux/data/Level2/Sites_ByID/"
  server_unix="ftp://cdiac.ornl.gov/pub/ameriflux/data/Level2/"
  
  # initialize dataframe
  data <- data.frame()
  
  # download file directory
  tmp <- RCurl::getURL(server)
  tmp <- read.table(textConnection(tmp), sep = ",", stringsAsFactor=FALSE)
  tmp <- tmp$V1
  tmp[grep(".txt",tmp)] <- NA
  tmp[grep(".doc",tmp)] <- NA
  tmp <- na.omit(tmp)
  # tease out site names and correct link locations
  # use server_unix as download.file() doesn't handle symlinks!
  fileLocs <- paste(server_unix, unlist(lapply(tmp, function(x) substring(x,70,200))), sep="") 
  siteNames <- unlist(lapply(tmp, function(x) substring(x,57,62)))
      
  # download a list of all files to download
  for (i in 1:length(siteIDs)) {
    siteID <- siteIDs[i]
    print(paste("Site:", siteID))
    siteLoc <- which(siteIDs[i] == siteNames)  
    if (gaps==TRUE) {
      siteDir <- paste(fileLocs[siteLoc],"/with_gaps/",sep='')    
    } else {
      siteDir <- paste(fileLocs[siteLoc],"/gap_filled/",sep='')
    }
    # Lookup timezone
    tz <- NULL
    uo <- NULL
    if (!(is.null(utcOffsets[i]))) {
      uo <- utcOffsets[i]
    } else if (!(is.null(timeZones[i]))) {
      tz <- timeZones[i]
    } else {
      uo <- amfMeta$UTC_offset_int[amfMeta$id_txt==siteID]
      if (length(uo)==0) {
        print("No time zone provided and site not included in existing database. Please determine the site's local (no DST) time zone and re-run the tool.")
        next
        }
    }
    print(paste("Time zone: tz=", ifelse(is.null(tz), "NA", tz), ", UTC offset=", ifelse(is.null(uo), "NA", uo))) 
    # Query for files
    fluxFiles <- tryCatch(suppressWarnings(RCurl::getURL(siteDir)), 
                          error=function(cond) {message(cond); return(NA)}) 
    if (is.na(fluxFiles)) {
      print(paste("No files found for site", siteID))
      next
    }
    # Process file list
    fluxFiles <- read.table(textConnection(fluxFiles), sep = ",", stringsAsFactor=FALSE)
    fluxFiles <- fluxFiles$V1
    fluxFiles <- fluxFiles[grep('*.csv',fluxFiles)]
    fluxFiles <- unlist(lapply(fluxFiles, function(x,...) substring(x,57,300)))
    fluxFiles <- data.frame(file=fluxFiles, yr=0, stringsAsFactors=FALSE)
    for (j in 1:nrow(fluxFiles)) {
      fluxFiles$yr[j]<-as.integer(unlist(strsplit(fluxFiles$file[j], split="_"))[3])
    }
    if (!(is.null(startYr)) & !(is.null(endYr))) {
      fluxFiles <- subset(fluxFiles, fluxFiles$yr>=startYr & fluxFiles$yr<=endYr)
    }
    if (nrow(fluxFiles)==0) {
      print(paste("No files found for site", siteID, "within the year range", startYr, "to", endYr))
      next
    }
    for (f in 1:nrow(fluxFiles)) {
      print(paste("  - processing year:", fluxFiles$yr[f]))
      # compile filename to download
      fluxFileLoc <- paste(siteDir, fluxFiles$file[f], sep="")
      # download and process data
      tmp <- RCurl::getURL(fluxFileLoc)
      tmpData <- read.table(textConnection(tmp), sep = ",", skip=20, na.strings=c(-6999,-9999), strip.white=T, stringsAsFactors=FALSE)
      tmpHead <- read.table(textConnection(tmp), sep = ",", skip=17, nrows=1, strip.white=T, stringsAsFactors=FALSE)
      colnames(tmpData)<-as.matrix(tmpHead)[1,]
      if (is.null(uo)) {
        tmpData$POSIXct <- as.POSIXct( paste(as.character(tmpData$YEAR), as.character(tmpData$DOY),
                                  as.character(ifelse(substr(tmpData$HRMIN,1,nchar(tmpData$HRMIN)-2)=='', "00", substr(tmpData$HRMIN,1,nchar(tmpData$HRMIN)-2))),
                                  as.character(substr(tmpData$HRMIN,nchar(tmpData$HRMIN)-1,nchar(tmpData$HRMIN))), sep="-"),
                                  format="%Y-%j-%H-%M", tz=tz )
      } else {
        tmpData$POSIXct <- as.POSIXct( paste(as.character(tmpData$YEAR), as.character(tmpData$DOY),
                                  as.character(ifelse(substr(tmpData$HRMIN,1,nchar(tmpData$HRMIN)-2)=='', "00", substr(tmpData$HRMIN,1,nchar(tmpData$HRMIN)-2))),
                                  as.character(substr(tmpData$HRMIN,nchar(tmpData$HRMIN)-1,nchar(tmpData$HRMIN))), sep="-"),
                                  format="%Y-%j-%H-%M", tz="UTC" ) - (uo*3600)
      }
      tmpData$wy <- CalcWaterYear(tmpData$POSIXct)
      tmpData$site_id <- siteID
      data <- plyr::rbind.fill(data, tmpData)
    } # end for flux files 
  } # end for sites
  return(data)
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
#'  \item{UTC_offset_int}{UTC offset w/o daylight savings in integer format (for manual time adjustments to UTC)}
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
#' Online_Linkage: \link[http://en.wikipedia.org/wiki/List_of_tz_database_time_zones]{http://en.wikipedia.org/wiki/List_of_tz_database_time_zones} \cr
#' Online_Linkage: \link[http://www.iana.org/time-zones]{http://www.iana.org/time-zones} \cr
#' @family Ameriflux
#' @concept data
#' @keywords data
"tzLookup"
  

