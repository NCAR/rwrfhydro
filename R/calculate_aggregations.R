#' Aggregate hourly precipitation data into daily values to be
#' comparable to GHCN daily data.
#' 
#' This function inputs the hourly forcing / model output data and aggregate then to daily.
#' GHCN gauges report data usually at 7:00 AM, 
#' however, that is not the case for all gauges. The reporting time may 
#' even differ for the same gauge but different elements. One can obtain the 
#' report time using \code{GetGhcn2}. 
#' 
#' @param sg A dataframe of the selectedGauges, it should have a column called siteIds,
#' which refer to the name of the station. 
#' siteIds should match the the standardized GHCN IDs (for example : ACW00011604).
#' See \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt}
#' for a list of siteIds.
#' It also should have a timeZone column or if not available it should have latitude 
#' and longitude which will be used whithin the function to find the timeZone.

#' 
#' @param prcp A dataframe of hourly data. It should have at least three columns:  
#' POSIXct, statArg, and DEL_ACCPRCP. POSIXct has the time in UTC, statArg has the 
#' site ID, and DEL_ACCPRCP is the depth of rainfall for that hour. The DEL_ACCPRCP  
#' column will be with the same name if you use GetMultiNcdf, ReshapeMultiNcdf and
#' CalcNoahmpFluxes to get the precipitation from WRF-Hydro output files. 
#' Otherwise, rename the your datafram accordingly to have POSIXct, DEL_ACCPRCP and statArg.
#' 
#' @param reportTime A vector of reportTime matching with sg$siteIds. 
#' reportTime is a unique reporting time for each elelment of each gauge.
#' It may vary for different elements for a single gauge. 
#' The report time values are retrived when reading the daily GHCN data 
#' using  \code{GetGhcn2}. Note, use 700 wherever the report time is missing. 
#' It will be overwritten if sg$reportTime exits.
#' 
#' @param parallel Logical (DEFAULT=FALSE)
#' 
#' @return A dataframe having 4 columns, the Daily precipitation comparable with daily GHCN data. 
#' @examples
#' \dontrun{
#' #ADD EXAMPLE HERE
#' }
#' @keywords manip
#' @concept GHCN
#' @family GHCN
#' @import data.table
#' @export

# To substitude the subroutine in rwrfhydro
CalcDailyGhcn <- function(sg, prcp, reportTime = 700, parallel = FALSE){
  
  # convert all the factor columns to character
  prcp <- as.data.frame(rapply(prcp, as.character, classes="factor", how="replace"), 
                        stringsAsFactors = FALSE)
  sg <- as.data.frame(rapply(sg, as.character, classes="factor", how="replace"),
                      stringsAsFactors = FALSE)
  
  # check for the columns which should exists in sg and prcp
  if(!(('siteIds') %in% names(sg))) {
    warning('siteIds should exists in sg dataframe.')
    return(NULL)
  }
  
  if(!all(c('POSIXct','statArg','DEL_ACCPRCP') %in% names(prcp))) {
    warning('POSIXct, statArg, DEL_ACCPRCP should exist in prcp dataframe.')
    return(NULL)
  }
  
  # Find the time zone using GetTimeZone for each gauge location
  
  if (!("timeZone" %in% colnames(sg))) {
    if (!all(c('latitude','longitude') %in% names(sg))){
      warning('sg dataframe should have latitude and logitude in order to GetTimeZone')
      return(NULL)
    }else{
      sg <- GetTimeZone(sg)
    }
  }
  
  timeZoneList <- as.list(as.character(unique(sg$timeZone)))
  names(timeZoneList) <- (timeZoneList)
  
  # Find the time difference between the UTC and the Local Standard Time using Time Zone
  offset1 <- lapply(timeZoneList, function(timeZoneX) {
    offset1 <- tzLookup$UTC_offset[tzLookup$TZ == timeZoneX]
  })
  diffTime <-as.difftime(gsub("[+-]","", offset1), format = "%H:%M") *ifelse(grepl("^-", offset1),-1, 1)
  names(diffTime) <- names(offset1)
  
  TZ <- sg$timeZone
  names(TZ) <- (sg$siteIds)
  
  # add time zone to the prcp dataframe
  prcp$timeZone <- TZ[as.character(prcp$statArg)]
  
  # remove the gauges where they do not have any time zone (They fall outside the tz polygon)
  prcp <- subset(prcp , !is.na(prcp$timeZone))
  
  # Add the local standard time to prcp
  prcp$lstTime <- as.POSIXct(prcp$POSIXct, tz = "UTC") + diffTime[as.character(prcp$timeZone)]
  
  # Check if there is a reportTime in sg columns or not. If there is not any, 
  # it will use the reportTime vector feeded to the function. 
  # This is to add the GHCN-DAY to the data
  
  if ("reportTime" %in% colnames(sg)) {
    sg$reportTime[which(sg$reportTime == "" |
                          is.na(sg$reportTime))] <- 700
    TZ <- sg$reportTime
    names(TZ) <- (sg$siteIds)
    prcp$reportTime <- TZ[as.character(prcp$statArg)]
    prcp$ghcnDay <-
      as.Date(trunc(prcp$lstTime + as.difftime((
        23 - as.numeric(as.character(prcp$reportTime)) / 100
      ), units = "hours"),"days"))
  } else {
    prcp$ghcnDay <-
      as.Date(trunc(prcp$lstTime + as.difftime((
        23 - as.numeric(as.character(reportTime)) / 100
      ), units = "hours"),"days"))
  }
  prcp <- prcp[,which(names(prcp) %in% c("ghcnDay","statArg","DEL_ACCPRCP"))]
  
  # Convert the dataframe to data.table to do the daily aggregation faster
  # We keep the number of hours used to calculate the daily data (summation)
  
  if (is.element('data.table', installed.packages()[,1])){
    prcp <- data.table::as.data.table(prcp)
   dailyData <-data.table:::`[.data.table`(prcp, ,.(dailyPrcp = sum(DEL_ACCPRCP),numberOfDataPoints = .N), by = .(ghcnDay,statArg))
    return(as.data.frame(dailyData))
  }else{
    warning('If install package data.table, it would be super fast to aggregate')
    dailyData<-plyr::ddply(prcp, c("ghcnDay","statArg"), 
                           plyr::summarise, 
                           dailyPrcp=sum(DEL_ACCPRCP[DEL_ACCPRCP], na.rm=TRUE),
                           .parallel=parallel)
    return(dailyData)
  }
}
