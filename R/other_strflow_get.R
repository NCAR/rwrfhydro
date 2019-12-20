#' Get CO DWR data for specified sites.
#' 
#' \code{GetCoDwrData} downloads and processes CO DWR data from web feed.
#' 
#' \code{GetCoDwrData} downloads Colorado Department of Water Resources data for
#' specified sites and outputs a dataframe with consistent date and data columns 
#' for use with other rwrfhydro tools.
#' 
#' @param siteIDs A single site ID or vector of site IDs to download and process.  
#' Site IDs should match the standardized CO DWR IDs (e.g., "RIOALACO" for RIO  
#' GRANDE RIVER AT ALAMOSA).
#' @param paramCodes A single or list of parameter codes to download. CO DWR
#' parameters include (list not inclusive) GAGE_HT, DISCHRG, AIRTEMP, COND, DO,  
#' DO_SAT, PH, PRECIP, TDS, TURBID, WATTEMP, WATTEMP2.
#' @param timeInt Time interval to download. Options are "raw", "hourly", or "daily".
#' (DEFAULT="raw"). Note that the hourly and daily options are means.
#' @param startDate (OPTIONAL) The start date to download for (format "mm/dd/yy").
#' @param endDate (OPTIONAL) The end date to download for (format "mm/dd/yy").
#' @return dataframe
#' @examples
#' \dontrun{}
#' str.RIO <- GetCoDwrData(c("RIOSFKCO", "RIOWAGCO","RIODELCO"), 
#'            paramCodes=c("GAGE_HT", "DISCHRG"), timeInt="hourly", 
#'            startDate="06/01/14", endDate="06/30/14")
#' @keywords IO
#' @concept Streamflow
#' @family Streamflow
#' @export

GetCoDwrData <- function(siteIDs, paramCodes, timeInt=1, 
                         startDate=NULL, endDate=NULL) {
  # Setup
  server <- "http://www.dwr.state.co.us/SurfaceWater/data/export_tabular.aspx?"
  timeList <- list(raw=1, hourly=2, daily=3)
  # Initialize dataframe
  data <- data.frame()
  for (siteID in siteIDs) {
    # The DWR server seems to fail when more than 2 parameters given, 
    # so we'll loop if more than 2.
    i <- 1
    while (i <= length(paramCodes)) {
      # Build URL
      fullurl <- paste0(server, "ID=", siteID, "&MTYPE=", 
                        paste(paramCodes[i:min(i+1, length(paramCodes))], collapse=","),
                      "&INTERVAL=", timeList[[timeInt]],
                      "&START=", startDate, "&END=", endDate)
      # Get data and parse (tab-delim)
      tmp <- read.delim(fullurl, comment.char="#", sep="\t", 
                        stringsAsFactors=FALSE,
                        na.strings=c("B","Bw","Dis","E","Eqp","Ice","M","na",
                                     "nf","Prov","Rat","S","Ssn","wtr op","----"))
      if (length(grep("DOCTYPE.html.PUBLIC",names(tmp))) > 0) {
        print(fullurl)
        stop("Error returned from the DWR server.")
        }
      if (i==1) datatmp <- tmp
      else datatmp <- plyr::join(datatmp, tmp, by=c("Station","Date.Time"))
      i <- i+2
      }
    data <- plyr::rbind.fill(data, datatmp)
  }
  # Set POSIXct
  data$POSIXct <- as.POSIXct(format(as.POSIXct(data$Date.Time, format="%Y-%m-%d %H:%M", 
                                               tz="America/Denver"),
                        tz="UTC"), tz="UTC")
  data <- subset(data, !is.na(data$POSIXct))
  data$wy <- CalcWaterYear(data$POSIXct)
  # Convert units
  if ("GAGE_HT..ft." %in% names(data)) data$ht_m <- data$GAGE_HT..ft. * 0.3048
  if ("DISCHRG..cfs." %in% names(data)) data$q_cms <- data$DISCHRG..cfs. * (0.3048^3)
  if ("AIRTEMP...F." %in% names(data)) data$airtemp_C <- (data$AIRTEMP...F. - 32) * 5/9
  if ("PRECIP..inches." %in% names(data)) data$cumprec_mm <- data$PRECIP..inches. * 25.4
  if ("STORAGE..AF." %in% names(data)) data$stor_m3 <- data$STORAGE..AF. * 43560 * (0.3048^3)
  return(data)
}

  

