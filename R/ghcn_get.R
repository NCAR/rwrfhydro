#' Read the daily GHNC metadata and select gauges based on user criteria.
#'
#' \code{SelectGhcnGauges} is designed to create a dataframe of the selected 
#' gauges based on the user's criteria. The dataframe stores important information  
#' about each gauge such as country, network type, stationID, lat/lon, 
#' elevation, state, description, GSN flag, HCN/CRN flag, WMO ID and 
#' siteIds=[country,ntework,stationID]
#'
#' @section Selection criteria : Selection can be based on the following 
#'  criteria: \enumerate{ \item A list of countries. For the full list
#'  of countries refer to 
#'  \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-countries.txt},
#'  example: countryCode=c("US","UK")
#'  
#'  \item A list of states (in this case the country will be 
#'  automatically set to US). For the full list of states refer to 
#'  \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-states.txt},
#'  example: states=c("OK","TX")
#'  
#'  \item A specific type of the network. For a list of network codes, 
#'  please refer to \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt} The 
#'  options are as follows: \itemize{ \item  0 = unspecified (station 
#'  identified by up to eight alphanumeric characters) \item  1 = Community 
#'  Collaborative Rain, Hail,and Snow (CoCoRaHS) based identification number. To
#'  ensure consistency with GHCN Daily, all numbers in the original 
#'  CoCoRaHS IDs have been left-filled to make them all four digits long. In 
#'  addition, the characters "-" and "_" have been removed to ensure that the 
#'  IDs do not exceed 11 characters when preceded by "US1". For example, the 
#'  CoCoRaHS ID "AZ-MR-156" becomes "US1AZMR0156" in GHCN-Daily \item  C = U.S. 
#'  Cooperative Network identification number (last six characters of the 
#'  GHCN-Daily ID) \item  E = Identification number used in the ECA&D 
#'  non-blended dataset \item  M = World Meteorological Organization ID (last 
#'  five characters of the GHCN-Daily ID) \item  N = Identification number used 
#'  in data supplied by a National Meteorological or Hydrological Center \item R
#'  = U.S. Interagency Remote Automatic Weather Station (RAWS) identifier \item 
#'  S = U.S. Natural Resources Conservation Service SNOwpack TELemtry (SNOTEL) 
#'  station identifier \item  W = WBAN identification number (last five 
#'  characters of the GHCN-Daily ID) 
#'  \item example: networkCode=c("C","1") } 
#'  \item Based on a domain. If domain is true, then you need min and max latitude and 
#'  logitude for the enclosing rectangle. }
#' @param countryCode A vector of desired countries, e.g., 
#'  countryCode=c("US","UK").
#' @param networkCode A vector of desired network type, e.g., for COOP and CoCoRaHS 
#'  use networkCode=c("C","1").
#' @param states A vector of desired states if country is US, e.g., states=c("OK","TX").
#' @param domain Logical. Set to TRUE if you want to cut over a rectangle domain. 
#'  (DEAFULT=FALSE)
#' @param minLat,maxLat,minLon,maxLon Numerics defining the boundary of the 
#'  rectangle if domain=TRUE.
#' @param fileAdd Provide the address to the daily GHCN ghcnd-states.txt. Default
#'  is "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt".
#' @return A dataframe of the selected gauges based on the user's criteria containing
#'  the following fields: country, network type, stationID, lat/lon, elevation, 
#'  state, description, GSN flag, HCN/CRN flag, WMO ID and 
#'  siteIds=[country,ntework,stationID]
#'  
#' @examples
#' \dontrun{
#' countryCodeList=c("US")
#' networkCodeList=c("1")
#' statesList=c("WY")
#' selectedGauges<-SelectGhcnGauges(countryCode=countryCodeList,
#'                                  networkCode=networkCodeList,
#'                                  states=statesList)
#' }
#' @keywords IO
#' @concept GHCN
#' @family GHCN
#' @export

SelectGhcnGauges <- function(countryCode=NULL,networkCode=NULL,states=NULL,
                             domain=FALSE,minLat=NULL,maxLat=NULL,minLon=NULL,
                             maxLon=NULL,fileAdd=NULL) {
# Setup
  if (is.null(fileAdd)) fileAdd <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"

  selectedGauges <- read.fwf(fileAdd,widths=c(2,1,8,-1,8,-1,9,-1,6,-1,2,-1,30,-1,3,-1,3,-1,4),
                 colClasses=c(rep("character",3),rep("numeric",3),rep("character",5)),comment.char = "")
  names(selectedGauges)<- c('country','network','stationID','latitude','longitude',
                          'elevation','state','name','GSNflag','HCN/CRNflag','WMOID')

  if (!is.null(countryCode)) selectedGauges<-subset(selectedGauges,selectedGauges$country %in% as.list(countryCode))
  if (!is.null(networkCode)) selectedGauges<-subset(selectedGauges,selectedGauges$network %in% as.list(networkCode))
  if (!is.null(states)) selectedGauges<-subset(selectedGauges,selectedGauges$state %in% as.list(states))
  if (isTRUE(domain)) selectedGauges<-subset(selectedGauges,selectedGauges$latitude > minLat & selectedGauges$latitude < maxLat & selectedGauges$longitude > minLon & selectedGauges$longitude < maxLon)

  selectedGauges$siteIds<-paste0(selectedGauges$country,selectedGauges$network,selectedGauges$stationID)
  return(selectedGauges)
}

#' Get GHCN data for specified siteIds.
#'
#' \code{\link{GetGhcn}} downloads the daily GHCN (Global Historic Climatology Network)
#'  data for each site in siteIds and creates a dataframe containing four fields: siteIds, date, 
#'  daily GHCN value and the qFlag. If there are many gauges, then 
#'  \code{\link{GetGhcn2}} would be much faster.
#'
#' @param siteIds A single site ID or vector of site IDs to download and process. 
#' SiteIds should match the standardized GHCN IDs (for example : ACW00011604).
#' See \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt}
#' for a list of siteIds.
#' @param elements A character vector defining what type of observation you 
#' are interested in. There are five core elements as well as a number of 
#' additional elements. The five core elements are:
#' \describe{
#' \item{PRCP}{ = Precipitation (tenths of mm)}
#' \item{SNOW}{ = Snowfall (mm)}
#' \item{SNWD}{ = Snow depth (mm)}
#' \item{MAX}{ = Maximum temperature (tenths of degrees C)}
#' \item{TMIN}{ = Minimum temperature (tenths of degrees C)}
#' }
#' For the full list of elemenst refer to 
#' \url{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}
#' @param startDate,endDate Date.
#' @param parallel Logical (DEFAULT=FALSE)
#' @param fileAdd Address to the url containg all the daily GHCN data,
#' default is \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/all/}
#'
#' @return A dataframe containing the date, correponding daily GHCN value,
#'  and the qFlag value.
#' 
#' @examples
#' \dontrun{
#' siteIds=c("ACW00011604","AJ000037579","AJ000037883","ASN00005095")
#' startDate="1949/02/01"
#' endDate="1949/10/01"
#' element="PRCP"
#' obsPrcp<-GetGhcn(siteIds,element,startDate,endDate,parallel=FALSE)
#' }
#'
#' # Or you could use the results of SelectGhcnGauges:
#' \dontrun{
#' countryCodeList=c("US")
#' networkCodeList=c("1")
#' statesList=c("WY")
#' selectedGauges<-SelectGhcnGauges(countryCode=countryCodeList,
#'                                  networkCode=networkCodeList,
#'                                  states=statesList)
#' obsPrcp<-GetGhcn(selectedGauges$siteIds,element,startDate,endDate,parallel=FALSE)
#' }
#' @keywords IO
#' @concept GHCN
#' @family GHCN
#' @export

GetGhcn <- function(siteIds,elements,startDate=NULL,endDate=NULL,parallel=FALSE,
                    fileAdd=NULL) {

  # Setup
  if (is.null(fileAdd)) fileAdd="http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/all/"

  getSite <- function(siteId) {
    urlAdd=paste0(fileAdd,siteId,'.dly') 
  
    tmp <- tryCatch(suppressWarnings(read.fwf(urlAdd,widths=c(11,4,2,4,rep(c(5,1,1,1),31)),
                                              colClass=c("character","integer","integer","factor",
                                                         rep(c("integer","factor","factor","factor"),31)))), 
                          error=function(cond) {message(cond); return(NA)}) 
      
     # Reading the whole data in the page
       tmp <- read.fwf(urlAdd,widths=c(11,4,2,4,rep(c(5,1,1,1),31)),
                       colClass=c("character","integer","integer","factor",
                                  rep(c("integer","factor","factor","factor"),31)))
       tmp<-tmp[tmp[4]==element,]

       dayflag<-c('day','mFlag','qFlag','sFlag')
       dayflag<-paste0(dayflag,rep(1:31,each=4))
       names(tmp)<- c('siteId','year','month','element',dayflag)
       
       whDay<-grep("day",names(tmp))
       whqFlag<-grep("qFlag",names(tmp))
       dayMeltDf <- reshape2::melt(tmp[,c(1:3,whDay)], id=c(names(tmp)[1:3]))
       dayMeltDf$qFlag <- reshape2::melt(tmp[,c(1:3,whqFlag)], id=c(names(tmp)[1:3]))$value
       
       changeDayName <- c(1:31)
       names(changeDayName) <- paste0('day',1:31)
       names(dayMeltDf)[4]<-'dayOfMonth'
       dayMeltDf$dayOfMonth <- changeDayName[dayMeltDf$dayOfMonth]
       dayMeltDf$datetime <- as.Date(paste0(dayMeltDf$year,'/', dayMeltDf$month,'/', dayMeltDf$dayOfMonth)) # add a date class column to the datafarme
       dayMeltDf<-dayMeltDf[complete.cases(dayMeltDf$datetime),] #Removes the null dates like 31 day of feb does not exist
       dayMeltDf<-subset(dayMeltDf,select=c("datetime","value","qFlag"))
       
       #Subset based on the provided date here to avoid reading lengthy observation into memory
       if ((!is.null(startDate)) & (!is.null(endDate))) {
         dayMeltDf<-subset(dayMeltDf, datetime > as.Date(startDate) & datetime < as.Date(endDate))
       }
       return(dayMeltDf)
  }

  siteIdsList <- as.list(siteIds)
  names(siteIdsList) <- siteIds
  data<-plyr::ldply( siteIdsList, getSite, .parallel=parallel)
  
  if (nrow(data)== 0) {
    return("warning: there is no available observation for the specified period")
  }else{
    if (element=="PRCP") data$value<-data$value/10
    names(data)<-c("siteIds","Date","dailyGhcn","qFlag")
    return(data)
  }
}


#' Get GHCN data for specified siteIds.
#'
#' \code{\link{GetGhcn2}} downloads the daily GHCN (Global Historic Climatology Network)
#'  data for each site in siteIds and creates a dataframe containing the fields: siteIds, date, 
#'  daily GHCN value, mFlag, qFlag, sFlag and reportTime. This is a faster function compared
#'  to \code{\link{GetGhcn}} if you have many sites.
#'  
#' @param siteIds A single site ID or vector of site IDs to download and process. 
#' SiteIds should match the standardized GHCN IDs (for example : ACW00011604).
#' See \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt}
#' for a list of siteIds.
#' @param elements A character vector defining what type of observation you 
#' are interested in. There are five core elements as well as a number of 
#' additional elements. The five core elements are:
#' \describe{
#' \item{PRCP}{ = Precipitation (tenths of mm)}
#' \item{SNOW}{ = Snowfall (mm)}
#' \item{SNWD}{ = Snow depth (mm)}
#' \item{MAX}{ = Maximum temperature (tenths of degrees C)}
#' \item{TMIN}{ = Minimum temperature (tenths of degrees C)}
#' }
#' For the full list of elemenst refer to 
#' \url{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}
#' @param startDate,endDate Date.
#' @param parallel Logical (DEFAULT=FALSE)
#' @param fileAdd Address to the url containg the csv files of daily GHCN by year. 
#' default is \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/}
#'
#' @return A dataframe containing the siteIds, date, daily GHCN, element,sFlag, qFlag, 
#'  mFlag and reportTime.
#' If the element is PRCP, then divide the numbers by 10 to convert to mm.
#' For more information on possible outcomes of flags and their meaning, refer to
#' \url{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt} 
#'
#' @examples
#' \dontrun{
#' siteIds=c("ACW00011604","AJ000037579","AJ000037883","ASN00005095")
#' startDate="2000/02/01"
#' endDate="2002/10/01"
#' element="PRCP"
#' obsPrcp<-GetGhcn2(siteIds,element,startDate,endDate,parallel=FALSE)
#' }
#'
#' # Or you could use the results of SelectGhcnGauges:
#' \dontrun{
#' countryCodeList=c("US")
#' networkCodeList=c("1")
#' statesList=c("WY")
#' selectedGauges<-SelectGhcnGauges(countryCode=countryCodeList,
#'                                  networkCode=networkCodeList,
#'                                  states=statesList)
#' obsPrcp<-GetGhcn2(selectedGauges$siteIds,element,startDate,endDate,parallel=FALSE)
#' }
#' @keywords IO
#' @concept GHCN
#' @family GHCN
#' @export

GetGhcn2 <- function(siteIDs, elements, startDate, endDate, parallel=FALSE,
                    fileAdd=NULL) {
 if (is.null(fileAdd)) fileAdd="ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/"
 # get all the years in the time period
 years<-as.list(seq(lubridate::year(as.Date(startDate)),lubridate::year(as.Date(endDate))))
 dat<-plyr::ldply(years, function(year) {
    temp <- tempfile()  
    download.file(paste0(fileAdd,year,".csv.gz"),temp, mode="wb")
    dat <- read.csv(gzfile(temp), header = FALSE,
                  col.names = c("siteIds","date","element","value","mFlag","qFlag","sFlag","reportTime"),
                  colClasses = c(rep("character",3),"numeric",rep("character",4)))
    unlink(temp)
  
    # subset based on siteIds, element, date
    dat$date <- as.Date(as.character(dat$date),"%Y%m%d")
    dat <- subset(dat,dat$siteIds %in% siteIDs)
    dat <- subset(dat,dat$element %in% elements)
    dat <- subset(dat,dat$date >= as.Date(startDate) & dat$date <= as.Date(endDate))
    },.parallel=parallel)

 if ("PRCP" %in% elements) {
    dat$value <- ifelse(dat$element == "PRCP", dat$value/10, dat$value)
 }
 return(dat)
}

