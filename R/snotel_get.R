#' Get SNOTEL data for specified sites.
#' 
#' \code{GetSnotel} downloads SNOTEL data tables and creates a dataframe.
#' 
#' \code{GetSnotel} downloads SNOTEL for
#' specified sites and outputs a dataframe with consistent date and data columns 
#' for use with other rwrfhydro tools.
#' 
#' @param siteIDs A single site ID or vector of site IDs to download and process. Site IDs 
#' should match the standardized SNOTEL IDs (integers). See snotelMeta for a list of 
#' site IDs (as of June 2015).
#' @param report (DEFAULT="STAND") Type of report to generate. Options (from NRCS)
#' are: \itemize{ \item "STAND" (standard snotel) \item "SOIL" (soil temp and moisture)
#' \item "SCAN" (standard scan) \item "ALL" (all) \item "WEATHER" (atmospheric) }
#' @param series (DEFAULT="Daily") Time series to generate. Options (from NRCS)
#' are: \itemize{ \item "Daily" \item "Hourly" \item "Hour:HH" (a single hour specified 
#' by HH) }
#' @param intervaltype (DEFAULT="Historic") Options (from NRCS)
#' are: \itemize{ \item "Historic" \item "Current" }
#' @param current (DEFAULT="DAY") Only used if intervaltype="Current". Options (from NRCS)
#' are: \itemize{ \item "DAY" \item "WEEK" \item "YEAR" \item "WATERYEAR" }
#' @param duration (DEFAULT="WY") Only used if intervaltype="Historic". Options
#' are: \itemize{ \item "CY" (calendar year) \item "WY" (water year) }
#' @param startYr (OPTIONAL) The first year to download files for.
#' @param endYr (OPTIONAL) The last year to download files for.
#' @param month (OPTIONAL) The month to download files for.
#' @param day (OPTIONAL) The day of month to download files for.
#' @param quiet (OPTIONAL) Whether to suppress output (TRUE=suppress output; 
#' DEFAULT=FALSE).
#' @return dataframe
#' @examples
#' \dontrun{
#'   sno.santafe <- GetSnotel(c("921","922"), series="Daily", startYr=2009, endYr=2010)
#'   sno.niwot <- GetSnotel(c("663"), series="Hourly", startYr=2009, month=4)
#'   sno.santafe <- GetSnotel(c("921","922"), intervaltype="Current", current="WEEK")
#' } #dontrun
#' @keywords IO
#' @concept SNOTEL
#' @family SNOTEL
#' @export

GetSnotel <- function(siteIDs, 
                      report="STAND", series="Daily",
                      intervaltype="Historic",
                      current="DAY", 
                      duration="WY", 
                      startYr=NULL, endYr=NULL,
                      month=NULL, day=NULL,
                      quiet=FALSE) {
  
  # test: siteIDs="663"; report="STAND"; series="Hourly"; intervaltype="Historic"; 
  # current="DAY"; duration="WY"; startYr=NULL; endYr=NULL; month=NULL; day=NULL
  # test: siteIDs=c("663","922"); report="STAND"; series="Hourly"; intervaltype="Historic"; 
  # current="DAY"; duration="WY"; startYr=2005; endYr=2015; month=NULL; day=NULL
  #"http://www.wcc.nrcs.usda.gov/nwcc/view?intervalType=$INTERVALTYPE+&report=$REPORT&
  #timeseries=$SERIES&format=copy&sitenum=$STATION&year=$YEAR&month=$MONTH&day=$DAY"
  
  # Setup
  server="http://www.wcc.nrcs.usda.gov/nwcc/"
  # initialize dataframe
  data <- data.frame()
  years <- c()
  # All SNOTEL timestamps are PST (UTC-8, no daylight savings)
  uo <- -8
  # Parse arguments for URL
  if (is.null(endYr)) endYr <- startYr
  if (!is.null(startYr)) years <- seq(startYr, endYr, 1)
  if (!is.null(month)) if (nchar(month)==1) month <- paste0("0",month)
  if (!is.null(day)) if (nchar(day)==1) day <- paste0("0",day)
  if (is.null(month)) month <- duration
  if (intervaltype=="Current") years <- format(Sys.Date(), "%Y")
      
  # Loop through sites and years
  for (i in 1:length(siteIDs)) {
    siteID <- siteIDs[i]
    if (!quiet) print(paste("Site:", siteID))
    for (j in 1:length(years)) {
      if (intervaltype=="Current") {
        if (!quiet) print(paste("  - processing current", current))
        # Form URL
        baseurl <- paste0(server, 
                          "view?intervalType=", intervaltype, 
                          "+&report=", report,
                          "&timeseries=", series,
                          "&format=copy&sitenum=", siteID,
                          "&interval=", current)
      } else {
        if (!quiet) print(paste("  - processing year:", years[j]))
        # Form URL
        baseurl <- paste0(server, 
                      "view?intervalType=", intervaltype, 
                      "+&report=", report,
                      "&timeseries=", series,
                      "&format=copy&sitenum=", siteID,
                      "&year=", years[j],
                      "&month=", month,
                      "&day=", day)
      }
      # Download data   
      tmp <- RCurl::getURL(baseurl)
      tmpData <- read.table(textConnection(tmp), sep = ",", header=TRUE, skip=2, 
                          strip.white=T, stringsAsFactors=FALSE,
                          na.strings=c(-99.9))
      tmpData$X <- NULL
      tmpHead <- read.table(textConnection(tmp), sep = "\t", nrows=1, strip.white=T, 
                            stringsAsFactors=FALSE)
      # Process time. Note that we are using "Date" not "POSIXct" format for daily data
      if (length(grep("Hour", series))>0) {
        tmpData$POSIXct <- as.POSIXct( paste(as.character(tmpData$Date), 
                                             as.character(tmpData$Time), sep=" "),
                                  format="%Y-%m-%d %H:%M", tz="UTC" ) - (uo*3600)
        tmpData$wy <- CalcWaterYear(tmpData$POSIXct)
      } else {
        # Using this timezone because it is UTC-8 with and without daylight savings, 
        # which matches SNOTEL PST timestamp, although this is just to get the WY correct
        # so doesn't actually matter as long as we are consistent...
        tmpData$Date <- as.Date(as.character(tmpData$Date), format="%Y-%m-%d")
        tmpData$wy <- CalcWaterYear(as.POSIXct(format(tmpData$Date, 
                                                      tz="America/Metlakatla"), 
                                               tz="America/Metlakatla"))
        }
      if ("Site.Id" %in% names(tmpData)) {
        names(tmpData)[which(names(tmpData)=="Site.Id")]<-"site_id"}
      # Do unit conversions where necessary
      if ("WTEQ.I.1..in." %in% names(tmpData)) {
        tmpData$SWE_mm <- sapply(tmpData$WTEQ.I.1..in., FUN=function(x) max(x*25.4, 0.0))
        tmpData$SWE_mm <- as.numeric(as.character(tmpData$SWE_mm))
      }
      if ("PREC.I.1..in." %in% names(tmpData)) {
        tmpData$CumPrec_mm <- as.numeric(as.character(tmpData$PREC.I.1..in.)) * 25.4
        tmpData$Prec_mm[2:nrow(tmpData)] <- diff(tmpData$CumPrec_mm)
        tmpData$Prec_mm[tmpData$Prec_mm<0] <- 0.0
        }
      if ("SNWD.I.1..in." %in% names(tmpData)) {
        tmpData$SNWD_mm <- sapply(tmpData$SNWD.I.1..in., FUN=function(x) max(x*25.4, 0.0))
        tmpData$SNWD_mm <- as.numeric(as.character(tmpData$SNWD_mm))
      }
      # Rename temperature fields (already in ok units)
      if ("TOBS.I.1..degC." %in% names(tmpData)) {
        names(tmpData)[which(names(tmpData)=="TOBS.I.1..degC.")]<-"Tobs_C"}
      if ("TMAX.D.1..degC." %in% names(tmpData)) {
        names(tmpData)[which(names(tmpData)=="TMAX.D.1..degC.")]<-"Tmax_C"}
      if ("TMIN.D.1..degC." %in% names(tmpData)) {
        names(tmpData)[which(names(tmpData)=="TMIN.D.1..degC.")]<-"Tmin_C"}
      if ("TAVG.D.1..degC." %in% names(tmpData)) {
        names(tmpData)[which(names(tmpData)=="TAVG.D.1..degC.")]<-"Tavg_C"}
      # Tack on to end of master dataset
      data <- plyr::rbind.fill(data, tmpData)
      } # end for years 
    } # end for sites
    return(data)
}


#' SNOTEL site metadata
#' 
#' Metadata for the SNOTEL sites (as of June 2015) including lat/lon.
#' 
#' @usage
#'  snotelMeta
#'  
#' @format
#' data.frame:  854 obs. of  11 variables:
#' \describe{
#'  \item{ntwk}{Observation network}
#'  \item{state}{State where site is located}
#'  \item{site_name}{Site name}
#'  \item{site_id}{Site integer ID}
#'  \item{start}{Start date for the observation record at the site}
#'  \item{lat}{Site latitude}
#'  \item{lon}{Site longitude}
#'  \item{elev}{Site elevation in feet}
#'  \item{county}{County where site is located}
#'  \item{huc}{HUC name where site is locted}
#'  \item{huc_id}{HUC ID where site is located}
#' }
#' 
#' @family SNOTEL
#' @concept data
#' @keywords data
"snotelMeta"

