## Atomic functionalities desired:
## 1. FindUsgsStns :identify stn from [ lat lon, stnid, huc? ]
## 1.1 mapUsgsStns
## 2. CheckLocalUsgsLib: identify data in local library, has it's own metadata file
## 3. GetUsgsStn: get usgs data
## 4. Convert units.
## 4. UpdateLocalUsgsLib: update local library. update from end vs fill holes. 
## 5. calculate variance information: examine rating curve information. 
## 6. output for dart

## testing input info

stnLon <- 254.67374999999998408
stnLat <- 40.018666670000001773
within <- .10
huc8 <- 10190005
##===============================================================================

FindUsgsStn <- function(stnLon=NULL, stnLat=NULL, within=NULL,
                        huc=NULL, siteType='Stream', onlyUsgs=TRUE ) {
  argList <- list()
  argList$siteType <- siteType
  # lat/lon
  if( !is.null(stnLon) && !is.null(stnLat) && !is.null(within) ) {
    if(stnLon>180.) stnLon <- stnLon-360
    argList$long   <- as.character(stnLon)
    argList$lat    <- as.character(stnLat)
    argList$within <- as.character(within) 
  }
  # huc
  if (!is.null(huc)) argList$huc <- as.character(huc)
  
  whatSites <- do.call( dataRetrieval::whatWQPsites, argList)
  if(onlyUsgs)
    whatSites <- whatSites[grep('USGS-',whatSites$MonitoringLocationIdentifier),]
  whatSites
}

## test 1 - pull al 
b <- FindUsgsStn(huc=huc8)
## Issues:
## * there is repeat information here.
## * there are "stations" from non-usgs entities, are those available.
## * there USGS station codes of different lengths. 
## seems that it's difficult to figure out what might be available/useful.

## test 2
bb <- FindUsgsStn(stnLon=stnLon, stnLat=stnLat, within=within)
## this nails it because the search radius is so localized. 
## if you know what you want, it seems you can get it. 

GetUsgsStn <- function( usgsStns, pCode=c(00060), startDate='', endDate='', tz='')

  ## parse the sation ids out
  grep('', bb$MonitoringLocationIdentifier,)

## get the data.
readNWISuv(
         }
