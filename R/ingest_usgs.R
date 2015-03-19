## This whole file continues to be a work in progress.
## Atomic functionalities desired:
## 1. FindUsgsStns :identify stn from [ lat lon, stnid, huc? ]
## 1.1 mapUsgsStns
## 2. CheckLocalUsgsLib: identify data in local library, has it's own metadata file
##      HUC.pCode.startDate.endDate.
## 3. GetUsgsStn: get usgs data
## 4. Convert units.
## 4. UpdateLocalUsgsLib: update local library. update from end vs fill holes. 
## 5. calculate variance information: examine rating curve information. 
## 6. output for dart

#' Discover USGS stations using huc8 code or lat/lon/radius.
#' \code{FindUsgsStns} wraps dataRetrieval::whatNWISsites with common options for finding USGS gauges. 
#' See the dataRetrieval package for more deails on what else could be passed or how this could be modified. 
#' @param stnLon optional
#' @param stnLat optional
#' @param within optional, goes with stnLon and stnLat and specifies a search radius in decimal miles.
#' @param huc optional EIGHT digit HUC code. 
#' @param siteType the type of USGS site to look for. 
#' @param hasDataTypeCd the kind of data of interest (iv=instantaneous, dv=daily, etc.)
#' @param siteOutput how much detail to return 
#' @examples
#' stnDf <- FindUsgsStns(huc='10190005')
#' @export
FindUsgsStns <- function(stnLon=NULL, stnLat=NULL, within=NULL,
                         huc=NULL, 
                         siteType='ST', hasDataTypeCd='iv',
                         siteOutput='expanded') {
  argList <- list()
  argList$siteType       <- siteType
  argList$hasDataTypeCd  <- hasDataTypeCd
  argList$siteOutput     <- siteOutput
  # lat/lon
  if( !is.null(stnLon) && !is.null(stnLat) && !is.null(within) ) {
    if(stnLon>180.) stnLon <- stnLon-360
    argList$long   <- as.character(stnLon)
    argList$lat    <- as.character(stnLat)
    argList$within <- as.character(within) 
  }
  # huc
  if (!is.null(huc)) argList$huc <- as.character(huc)
  #print(argList)
  do.call( dataRetrieval::whatNWISsites, argList)
}

#' Get all the USGS streamgage information within a HUC8. 
#' \code{GetUsgsHucData} gets all the USGS streamgage information within a HUC8. If an
#' output path is supplied, an existing database is examined for existing records and
#' the data retrieved from the USGS only extends this and is also save in the data base. 
GetUsgsHucData <- function(huc8, outPath=NULL) {

  stns <- FindUsgsStns(huc=huc8)
  meta <- dataRetrieval::whatNWISdata(stns$site_no, service = "uv")

  if(!is.null(outPath)) {
    metaFileName <- paste0(huc8,'.meta.RData')
    dataFileName <- paste0(huc8,'.data.RData')
    ## do the HUC files exist
    metaFilePath <- list.files(outPath, pattern=metaFileName) 
    dataFilePath <- list.files(outPath, pattern=dataFileName)
    
    if(metaFilePath != "" & dataFilePath != "") {          ## Both files exist: appending
      load(metaFilePath)
      metaVar <- paste0('meta.',huc8[1])
      metaData <- get(metaVar)
      ## if so, what are the stations and what are their end dates?
      ## if station present: startDate=oldEndDate+1second, endDate=today, tz=''  
      meta$startDate <- plyr::ddply(meta, plyr::.(parm_cd), 
                                    function(df) meta)
      meta$endDate   <- 
    } else if (metaFilePath == "" & dataFilePath == "") {  ## Neither exists: creating    
      meta$startDate <- meta$endDate <- ''    
    } else {                                               ## One / two files exists: error.
      theExister    <- if(metaFilePath == "") metaFilePath else paste0(outpath,'/',dataFileName)
      theNonExister <- if(metaFilePath != "") paste0(metaFileName,'/',metaFileName) else dataFilePath
      warning( paste(theExister,    'does not exist while \n',
                     theNonExister, 'does. Please investigate.'), immediate.=TRUE )
    }
    
    ## if station NOT present: startDate='', endDate='', tz=''
 
  } else {
    meta$startDate <- meta$endDate <- ''    
  }
  
  meta$tz <- ''
  
  #' Simply a wrapper for a plyr::dlply call to this . 
  GetUsgsStn <- function( pCodeDf, startDate='', endDate='', tz='') {
      dataRetrieval::readNWISuv(pCodeDf$site_no, pCodeDf$parm_cd, 
                                startDate=startDate,  endDate=endDate, tz=tz)
  }

  ## this some times fails for various reasons. should I wrap these various retrievals in try()?
  out <- plyr::dlply(meta, plyr::.(parm_cd), GetUsgsStn) 
  
  if(!is.null(outPath)) SaveHuc(out,path=outPath)
  
  out ## this should be the full record?
}


SaveHuc <- function(out, path=) {
  
  ## files are simply named after the huc
  ## HHHHHHHH.data.RData
  ## HHHHHHHH.meta.RData
  hucMetaAll <- plyr::llply(NamedList(names(out)), 
                            function(huc.prod) ExtractHucMeta(out[[huc.prod]]) )

  ##What is the HUC?
  allHuc <- unlist(plyr::llply(out, function(df) attr(df, 'siteInfo')$hucCd))
  ## make sure there's only one HUC.
  if(!all(allHuc == allHuc[1])) warning('Not all stations in the same HUC, though they should be.', immediate.=TRUE)
  
  metaFileName <- paste0(allHuc[1],'.meta.RData')
  dataFileName <- paste0(allHuc[1],'.data.RData')
  
  ## do the HUC files exist
  metaFilePath <- list.files(outPath, pattern=metaFileName) 
  dataFilePath <- list.files(outPath, pattern=dataFileName)
  ## name the output variables to be used in the files.
  metaVar <- paste0('meta.',allHuc[1])
  dataVar <- paste0('data.',allHuc[1])
    
  if(metaFilePath != "" & dataFilePath != "") {          ## Both files exist: appending
    stop('write this code')
  } else if (metaFilePath == "" & dataFilePath == "") {  ## Neither exists: creating
    assign(metaVar, hucMetaAll)
    assign(dataVar, out)
    save(metaVar, metaFilePath)
    save(dataVar, dataFilePath)  
    rm(list=c(metaVar,dataVar))
  } else {                                               ## One / two files exists: error.
    theExister    <- if(metaFilePath == "") metaFilePath else paste0(outpath,'/',dataFileName)
    theNonExister <- if(metaFilePath != "") paste0(metaFileName,'/',metaFileName) else dataFilePath
    warning( paste(theExister,    'does not exist while \n',
                   theNonExister, 'does. Please investigate.'), immediate.=TRUE )
  }
  
}
  

#' Grab the meta data to be stored in a HUC database for each huc. 
#' \code{ExtractHucMeta} extracts the meta data to stash in a HUC database file for each HUC.
#' It removes the atributes from the data, gathers them, supplements siteInfo with
#' startTime and endTime to assist appending the records. 
#' @param hucProdDf The 
#' 
ExtractHucMeta <- function(hucProdDf) {
  hucMeta <- list( siteInfo      = attr(hucProdDf, 'siteInfo'), 
                   variableInfo  = attr(hucProdDf, 'variableInfo'), 
                   statisticInfo = attr(hucProdDf, 'statisticInfo') )
  startTime <- plyr::ldply(hucMeta$siteInfo$site_no, 
                           function(site) min(subset(hucProdDf, site_no == site)$dateTime) )[,1]
  endTime   <- plyr::ldply(hucMeta$siteInfo$site_no, 
                           function(site) max(subset(hucProdDf, site_no == site)$dateTime) )[,1]
  hucMeta$siteInfo$startTime <- lubridate::with_tz(startTime,tz='UTC') 
  hucMeta$siteInfo$endTime   <- lubridate::with_tz(endTime,  tz='UTC')
  hucMeta
}  