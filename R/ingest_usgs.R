## This whole file continues to be a work in progress.
## Atomic functionalities desired:
## 1. FindUsgsStns :identify stn from [ lat lon, stnid, huc? ]
## 1.1 mapUsgsStns
## 2. CheckLocalUsgsLib: identify data in local library
## 3. GetUsgsStn: get usgs data
## 4. Convert units.
## 4. UpdateLocalUsgsLib: update local library. update from end vs fill holes. 
## 5. calculate variance information: examine rating curve information. 
## 6. output for dart
## 7. update metadata using files in same directory?
## 8. how to query the DB? lat lon, station code, huc? have a first layer which queries NWIS?

#==============================================================================================
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
                         huc8=NULL, 
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
  if (!is.null(huc8)) argList$huc <- as.character(huc8)
  #print(argList)
  do.call( dataRetrieval::whatNWISsites, argList)
}


#==============================================================================================
#' Get all the USGS streamgage information within a HUC8. 
#' \code{GetUsgsHucData} gets all the USGS streamgage information within a HUC8. If an
#' output path is supplied, an existing database is examined for existing records and
#' the data retrieved from the USGS only extends the available and is saved in the data base.
#' Could eventually use startDate and endDate to tweak this for calls which dont care about archival.
#' @examples
#' bldrHucData    <- GetUsgsHucData(huc='10190005')
#' satilpaHucData <- GetUsgsHucData(huc='03160203')
#' vansHucData    <- GetUsgsHucData(huc='03020104')
#' @export
GetUsgsHucData <- function(huc8, outPath=NULL, 
                           metaDBFileName='usgsDataRetrieval.metaDatabase.RData' ) {

  stns <- FindUsgsStns(huc=huc8)
  meta <- dataRetrieval::whatNWISdata(stns$site_no, service = "uv")
  meta$tz <- ''  #not currently used
  
  ## are we outputting or not?
  if(!is.null(outPath)) {
    metaFilePath <- list.files(outPath, pattern=metaDBFileName) 
    dataFileName <- paste0(huc8,'.data.RData')
    dataFilePath <- list.files(outPath, pattern=dataFileName)
    if(!length(metaFilePath) & length(dataFilePath))
      warning( paste0('A data file exists (',dataFilePath,') but cannoth find the metadata (',
                     metaFilePath,'). Please investigate.'), immediate.=TRUE )
  } else {  
    ## is.null(outPath), not writing to a database
    meta$startDate <- meta$endDate <- ''    
    metaFilePath <- dataFilePath <- ''
  }
  
  ## Both files exist: identify existing data at each station and only get data past those dates
  if(metaFilePath != "" & dataFilePath != "") {        
    warning('this case has not been fully programmed yet', immediate.=TRUE)
    load(metaFilePath)
    ## if so, what are the stations and what are their end dates?
    ## if station present: startDate=oldEndDate+1second, endDate=today, tz=''  
    #a general purpose query function is needed here.
    meta$startDate <- plyr::ddply(meta, plyr::.(parm_cd), 
                                  function(df) meta)
    #meta$endDate   <- 
  } else {
    ## else = the cases (!meta & !data) and (meta & !data) dont need to query the data.
    meta$startDate <- meta$endDate <- ''
  }
    
  ## Actually get the data.
  ## Some times fails for various reasons, eventually wrap these various retrievals in try()?
  if(AllSame(meta$startDate) && AllSame(meta$endDate)) {
    # This is the case where all station record updates are exactly the same in time.
    out <- plyr::dlply(meta, plyr::.(parm_cd), GetUsgsIvProduct) 
  } else {
    # This is the case where station record updates are different over time. 
    outStnIndiv <- plyr::dlply(meta, plyr::.(parm_cd), 
                               function(df) plyr::dlply(df, plyr::.(site_no), GetUsgsIvProduct))
    outData <- plyr::llply(outStnIndiv, function(param) 
                          plyr::ldply(param, function(site) site$data) )
    ## A helper 
    GetAtt <- function(attName) plyr::llply(outStnIndiv, function(param, ...) 
                                            plyr::ldply(param, function(site, ...) site$meta[[attName]] ) )
    siteInfo <- GetAtt('siteInfo')
    variableInfo<- GetAtt('variableInfo')
    statisticInfo<- GetAtt('statisticInfo')
    outMeta <- plyr::llply(NamedList(names(outData)), 
                           function(prod) list(siteInfo = siteInfo[[prod]],
                                               variableInfo = variableInfo[[prod]],
                                               statisticInfo = statisticInfo[[prod]]) )
   ## make the variableInfo and statisticInfo match exactly by collapsing it.
   homogenInfo <- function(prod,info) 
     unlist(plyr::daply(outMeta[[prod]][[info]], plyr::.(), plyr::colwise(AllSame) ))[-1]
   for (prod in c(names(outMeta))) {
     for (info in (c('variableInfo','statisticInfo'))) {
       if(all(homogenInfo(prod,info), na.rm=TRUE))
         outMeta[[prod]][[info]]<-outMeta[[prod]][[info]][1,-1]
     }
   }   
   out <- list(data=outData, meta=outMeta)
  }

  if(!is.null(outPath)) SaveHuc(out,path=outPath)
  out ## this should be the full record?  
}


#==============================================================================================
#' Save the output of GetUsgsHucData to an archive
#' \code{SaveHucData} take a list returned by GetUsgsHucData, create or append to an existing archive, and update
#' metadata file. Input list is grouped by HUC codes where each HUC has data and meta lists. The output files 
#' are written to outPath with the following format HHHHHHHH.data.RData where HHHHHHHH is the HUC8 code. The 
#' name of the metadata file is configurable. The metadata file contains metadata for all files in the outPath. 
#' files <- SaveHucData(GetUsgsHucData(huc='10190005'), outPath='/Users/jamesmcc/streamflow/OBS') ##boulder, CO
#' files <- SaveHucData(GetUsgsHucData(huc='03160203'), outPath='/Users/jamesmcc/streamflow/OBS') ##satilpa, AL
#' files <- SaveHucData(GetUsgsHucData(huc='03020104'), outPath='/Users/jamesmcc/streamflow/OBS') ##vans, NC
#' @export
SaveHucData <- function(out, outPath, 
                        metaDBFileName='usgsDataRetrieval.metaDatabase.RData', 
                        overwriteHucDataFile=FALSE) {
  ##What the HUC?
  allHuc <- unlist(plyr::llply(out, function(ll) ll$meta$siteInfo$hucCd))
  ## make sure there's only one HUC.
  if(!AllSame(allHuc)) warning('Not all stations in the same HUC, though they should be.', immediate.=TRUE)
  huc=as.character(allHuc[1])
  dataFileName <- paste0(huc[1],'.data.RData')
  
  ## do the HUC files exist
  metaFilePath <- list.files(outPath, pattern=metaDBFileName) 
  dataFilePath <- list.files(outPath, pattern=dataFileName)
  if(overwriteHucDataFile) dataFilePath <- character(0)
  if(!length(metaFilePath) & length(dataFilePath))
    warning( paste0('A data file exists (',dataFilePath,') but cannoth find the metadata (',
                    metaFilePath,'). Please investigate.'), immediate.=TRUE )
  
  ## prepare the data for output.
  ## name the output variables to be used in the files.
  dataVar <- paste0('data.',huc[1])
  assign(dataVar, out)
  outHucMetaDB <- out
  for (name in names(outHucMetaDB)) outHucMetaDB[[name]]$data <- NULL
  str(outHucMetaDB)
  
  if(length(metaFilePath)) {
    # loads metaDB
    load(paste0(outPath,'/',metaDBFileName))
    ## have to append to existing metadata
    if(huc %in% names(metaDB) & !overwriteHucDataFile) {
      ## getHucData
      prevHucMetaDB <- metaDB[[huc]]
      ## merge
      ## lots of issues, go through site info site by site and expand times.
      ## force end and beginning times to match? how to avoid holes in the timeseries?
    } else {
      metaDB[[huc]] <- outHucMetaDB
    }  
  } else {
    ## there is no existing meta data
    metaDB <- list()
    metaDB[[huc]] <- outHucMetaDB
  }
  save(list='metaDB', file=paste0(outPath,'/',metaDBFileName))

  if(length(dataFilePath)) {
    stop('write this code')
    ## merge the data
    ## the updated meta need to be put into the output
  } 
  save(list=dataVar, file=paste0(outPath,'/',dataFileName))  
 
  list(dataFile=paste0(outPath,'/',dataFileName), metaDBFile=paste0(outPath,'/',metaDBFileName))
}
  

#==============================================================================================
#' For an indivudal product, grab the HUC metadata to be stored in the database. 
#' \code{ExtractHucMeta} extracts the metadata to stash in a usgsDataRetrieval database for an individual product.
#' It gathers the attributes returned by (returned from dataRetrieval::readNWISuv) , supplements siteInfo with
#' startTime and endTime for each station in UTC to assist appending the records. 
#' @param hucProdDf
#' @return list of metadata for a 
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


#==============================================================================================
#' For a single product, get instantaneous USGS data and separate data and metadata/attributes into a list.
#' \code{GetUsgsIvProduct} gets instantaneous USGS data for a single product code
#' and separates the returned dataframe with attributes from dataRetrieval::readNWISuv into a list with 
#' separate data and metadata/attributes. The meta data are updated with startTime and endTime information.
#' @param prodDf is a dataframe returned by dataRetrieval::whatNWISdata(stns$site_no, service = "uv")
#' subet to an individual product code.
#' @return list(data=,meta=) 
GetUsgsIvProduct <- function( prodDf ) {  
  #pCodeDf <- subset(meta, parm_cd == '00065') ## testing
  prodData <- dataRetrieval::readNWISuv(prodDf$site_no, prodDf$parm_cd, 
                                        startDate=prodDf$startDate[1],  
                                        endDate=prodDf$endDate[1], 
                                        tz=prodDf$tz[1])
  prodMeta <- ExtractHucMeta(prodData)
  ## the subset simply removes the attributes
  list(data=subset(prodData,site_no %in% prodData$site_no), meta=prodMeta)
}


#==============================================================================================
#' Given a USGS site code, return its HUC8
#' \code{GetSiteHuc} returns a HUC8 given a USGS site name. 
#' @param site Character USGS site number.
#' @return character HUC8
#' @export
GetSiteHuc <- function(site) readNWISsite(as.character(site))$huc


#==============================================================================================
#' Find the products available for a given site in the local database.
#' \code{QuerySiteProd} finds the products available for a given site in the local database.
#' @param site Character USGS site number.
#' @param path Character The path to the database.
#' @param metaDBFileName Character The name of the metadata file.
#' @examples
#' QuerySiteProd('06730500', '~/streamflow/OBS/')
#' @export
QuerySiteProd <- function(site, path='.', 
                          metaDBFileName='usgsDataRetrieval.metaDatabase.RData') {
  ## loads metaDB
  load(paste0(path,'/',metaDBFileName))  
  subset(QuerySiteInfo('site_no'), site_no == site)$product
}

#==============================================================================================
#' Find the name(site id) for a given site(name)  in the local database.
#' \code{QuerySiteName} returns the name (site id) for a given site ID (name) in the local database.
#' @param site Character USGS site number or name.
#' @param path Character The path to the database.
#' @retSiteId Logical return ID (name if FALSE) This is only exposed in case there are issues, should work by default.
#' @param metaDBFileName Character The name of the metadata file.
#' @examples
#' QuerySiteName('06730500', '~/streamflow/OBS/')
#' QuerySiteName('BOULDER CREEK AT MOUTH NEAR LONGMONT, CO', '~/streamflow/OBS/')
#' @export
QuerySiteName <- function(site, path='.', 
                          retSiteId=is.na(try(as.numeric(site),silent=TRUE)), 
                          metaDBFileName='usgsDataRetrieval.metaDatabase.RData') {
  ## loads metaDB
  load(paste0(path,'/',metaDBFileName))  
  if(retSiteId) {
    subset(QuerySiteInfo(c('site_no','station_nm')), station_nm == site)[1,'site_no']
  } else {
    subset(QuerySiteInfo(c('site_no','station_nm')), site_no == site)[1,'station_nm']
  }
}


#==============================================================================================
#' Returns the desired information from the database file.
#' \code{QuerySiteInfo} gets the specified info from the local database.
#' @param info Character vector, information fields in HUC$prod$meta$SiteInfo$info
#' @examples 
#' QuerySiteInfo(c('station_nm','site_no'))
QuerySiteInfo <- function(info) {
  out <- reshape2::melt(plyr::llply(metaDB, function(huc) 
                                    plyr::llply(huc, function(prod) prod$meta$siteInfo[info])), id=info)
  names(out) <- c(info, 'product', 'HUC8')
  out
}


#==============================================================================================
#' Returns the data for a given site from local database.
#' \code{QuerySiteData} gets the specified data from the local database.
#' @param site Character USGS site number.
#' @param product Character USGS product code number.
#' @param path Character path to the database.
#' @examples 
#' p='~/streamflow/OBS/'
#' dataOrodell <- QuerySiteData(QuerySiteName("FOURMILE CREEK AT ORODELL, CO", p), '00060', p)
QuerySiteData <- function(site, product, path='.'){
  if(!any(QuerySiteProd(site, path) == product)) {
    warning(paste("No product",product,"at site",site,"."))
    return(NULL)
  }
  huc <- subset(QuerySiteInfo(c('site_no')), product == product & site_no == site)$HUC8[1]
  load(paste0(path,'/',huc,'.data.RData'))
  productData <- get(paste0('data.',huc))[[product]]
  rm(list=paste0('data.',huc))
  ret <- subset(productData$data, site_no == site )
  ret <- within(ret, {site_no <- NULL; agency_cd <- NULL})
  attr(ret, 'siteInfo') <- subset(productData$meta$siteInfo, site_no == site)
  attr(ret, 'variableInfo') <- productData$meta$variableInfo
  attr(ret, 'statisticInfo') <- productData$meta$statisticInfo
  ret
}


#==============================================================================================
#' Pretty USGS site data makes nice headers and optionally converts to metric. 
#' \code{PrettySiteData} beautifies the data frame 
#' @param data Dataframe from QuerySiteData
#' @param metric Logical
#' @param metricOnly Logical
#' @examples 
#' p='~/streamflow/OBS/'
#' dataOrodell <- QuerySiteData(QuerySiteName("FOURMILE CREEK AT ORODELL, CO", p), '00060', p)
PrettySiteData <- function(data, metric=metricOnly, metricOnly=FALSE) {
 1 
}
