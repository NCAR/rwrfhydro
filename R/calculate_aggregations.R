#' To aggregate model output hourly precipitation data into daily values 
#' comparable to GHCN daily data.
#' 
#' This function inputs the model output hourly data and aggregate the  
#' hourly data into daily info. GHCN gauges report data usually at 7:00 AM, 
#' however, that is not the case for all gauges. The reporting time may 
#' even differ for the same gauge but different elements. One can obtain the 
#' report time using \code{GetGhcn2}. 
#' 
#' @param sg A dataframe of the selectedGauges, it should have at least one column
#' with name of siteIds.
#' siteIds should match the the standardized GHCN IDs (for example : ACW00011604).
#' See \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt}
#' for a list of siteIds.
#' 
#' @param prcp A dataframe of hourly data. It should have at least three columns:  
#' POSIXct, statArg, and DEL_ACCPRCP. POSIXct has the time in UTC, statArg has the 
#' site ID, and DEL_ACCPRCP is the depth of rainfall for that hour. The DEL_ACCPRCP  
#' column will be with the same name if you use GetMultiNcdf, ReshapeMultiNcdf and
#' CalcNoahmpFluxes to get the precipitation from WRF-Hydro output files.
#' 
#' @param reportTime A vector of reportTime matching with sg$siteIds. 
#' reportTime is a unique reporting time for each elelment of each gauge.
#' It may vary for different elemenets for a single gauge. 
#' The report time values are retrived when reading the daily GHCN data 
#' using  \code{GetGhcn2}. Note, use 700 wherever the report time is missing. 
#' It will be overwritten if sg$reportTime exits.
#' 
#' @param parallel Logical (DEFAULT=FALSE)
#' 
#' @return Daily precipitation comparable with daily GHCN data. 
#' @examples
#' \dontrun{
#' #ADD EXAMPLE HERE
#' }
#' @keywords manip
#' @concept GHCN
#' @family GHCN
#' @export

CalcDailyGhcn<-function(sg, prcp, reportTime=700, parallel=FALSE){
  
  # Add timeZone if missing
  if (!("timeZone" %in% colnames(sg))) sg<-GetTimeZone(sg) 
  
  timeZoneList<-as.list(as.character(unique(sg$timeZone)))
  names(timeZoneList)<-(timeZoneList)
  offset1<-lapply(timeZoneList, function(timeZoneX) {
    offset1<-tzLookup$UTC_offset[tzLookup$TZ == timeZoneX]
  })
  
  # Add the time zone column to prcp data
  TZ<-sg$timeZone
  names(TZ)<-(sg$siteIds)
  prcp$timeZone<-TZ[as.character(prcp$statArg)]
  prcp$lstTime<-as.POSIXct(prcp$POSIXct)+as.difftime(gsub("[+-]","",offset1[as.character(prcp$timeZone)]),format ="%H:%M") * ifelse(grepl("^-",offset1[as.character(prcp$timeZone)]),-1,1)
  
  # add a day index to each hour compatible with GHCN time stamp 
  if ("reportTime" %in% colnames(sg)){
    # Add the report time column to prcp data 
    TZ<-sg$reportTime
    names(TZ)<-(sg$siteIds)
    prcp$reportTime<-TZ[as.character(prcp$statArg)]
    
    prcp$ghcnDay<-as.Date(trunc(prcp$lstTime+as.difftime((23-as.numeric(prcp$reportTime)/100),units="hours"),"days"))
  }else{
    prcp$ghcnDay<-as.Date(trunc(prcp$lstTime+as.difftime((23-as.numeric(reportTime)/100),units="hours"),"days"))
  }
  
  # calculate the daily precip form hourly prcp
  dailyData<-plyr::ddply(prcp, c("ghcnDay","statArg"), 
                         plyr::summarise, 
                         dailyPrcpMod=sum(DEL_ACCPRCP[DEL_ACCPRCP>=0], na.rm=TRUE),
                         .parallel=parallel)
  
}