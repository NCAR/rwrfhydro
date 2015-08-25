#' To aggregate the hourly data into daily coparable to GHCN daily data.
#' 
#' This function inputs the hourly data and aggregate the hourly data into daily info. 
#' GHCN gauges report data usually at 7:00 AM, hoowever, taht is not the case for all
#' gauges. The reporting time may even differ for the same gauge but different elements.
#' One can obtain the report time when using \code{GetGhcn2}. 
#' 
#' @param sg A dataframe of the selectedGauges, it should have at least two columns with names 
#' of siteIds and reportTime.
#' SiteIds should match the the standardized GHCN IDs (for example : ACW00011604).
#' See \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt}
#' for a list of siteIds.
#' reportTime is a unique reporting time for each gauges which may vary for different elemenets
#' for a single gauge. The report time values are retrived when reading the daily GHCN data 
#' using  \code{GetGhcn2}. Note, use 7 for wherever the report time is missing. 
#' 
#' @param prcp A dataframe of hourly data. It should have at least two columns of POSIXct, 
#' statArg, and DEL_ACCPRCP.POSIXct has the timing at UTC. statArg has the siteIds, and 
#' DEL_ACCPRCP is the depth of rainfall since wrf_hydro report the accumulated rainfall, 
#' one extra step is required to calculate the depth of rainfall for an interval.
#' The column will be with the same name if one use GetMultiNcdf, ReshapeMultiNcdf and
#' CalcNoahmpFluxes to get the precipitation from WRF_hydro files.
#' 
#' @param reportTime A vector of reportTime matching with sg$siteIds, or a number 
#' such as 7 is all gauges report at the same time. You can leave it NULL is sg has a 
#' column of reportTime.
#' 
#' @return Daily precipitation comparable with daily GHCN data. 
#' 

CalcDailyGhcn<-function(sg,prcp,reportTime=700){
  
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
  
  # Add the report time column to prcp data 
  TZ<-sg$reportTime
  names(TZ)<-(sg$reportTime)
  prcp$reportTime<-TZ[as.character(prcp$statArg)]
 
  # add a day index to each hour compatible with GHCN time stamp 
  if (!is.null(reportTime)) {
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