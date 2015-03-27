## This file continues to be a work in progress.
## Atomic functionalities desired:
## XX 1. FindUsgsStns :identify stn from [ lat lon, stnid, huc? ] 
## 1.1 mapUsgsStns
## XX 2. CheckLocalUsgsLib: identify data in local library        
## XX 3. GetUsgsStn: get usgs data
## XX 4. Convert units.
## 4. UpdateLocalUsgsLib: update local library. update from end vs fill holes. 
## 5. calculate variance information: examine rating curve information. 
## 7. update metadata using files in same directory?
## XX 8. how to query the DB? lat lon, station code, huc? have a first layer which queries NWIS?

## notes:
## 1. might be better to set global variables for DB path and metaDBFileName than to set as options in nearly every
##    function.

#==============================================================================================
#' Discover USGS stations using huc8 code or lat/lon/radius.
#' 
#' \code{FindUsgsStns} wraps \code{dataRetrieval::whatNWISsites} with common options for finding USGS gauges. 
#' See the dataRetrieval package for more deails on what else could be passed or how this could be modified. 
#' One improvement here would be to order results by proximity to supplied lat/lon and filter to nClosest.
#' @param stnLon optional
#' @param stnLat optional
#' @param within optional, goes with stnLon and stnLat and specifies a search radius in decimal degrees.
#' @param huc8 optional EIGHT digit HUC code. 
#' @param siteType the type of USGS site to look for. 
#' @param hasDataTypeCd the kind of data of interest (iv=instantaneous, dv=daily, etc.)
#' @keywords IO
#' @concept dataGet usgsStreamObs
#' @family streamObs
#' @examples
#' stnDf <- FindUsgsStns(huc='10190005')
#' stnDf <- FindUsgsStns(huc=c('10190005','03160203'))
#' stnDf <- FindUsgsStns(stnLon=254.67374999999998408,stnLat=40.018666670000001773,within=.001)
#' stnDf <- FindUsgsStns(stnLon=c(254.67374999999998408,-87.747224700000004),
#'                       stnLat=c(40.018666670000001773, 31.864042489999999),within=.001)
#' @export
FindUsgsStns <- function(stnLon=NULL, stnLat=NULL, within=NULL,
                         huc8=NULL, siteType='ST', hasDataTypeCd='iv') {

  if(all(is.null(c(stnLon,stnLat,huc8)))) {
    warning('FindUsgsStns requires either both stnLon and stnLat or huc8 to be set. Returning.')
    return(NULL)
  }  
  if(length(stnLon)!=length(stnLat)) {
    warning('The stnLon and stnLat arguments to FindUsgsStns must have same length. Returning.')
    return(NULL)
  }

  vecDf <- FormalsToDf(FindUsgsStns)
  plyr::mdply(vecDf, FindUsgsStns.atomic)
}

#' @keywords internal
#' @concept usgsStreamObs dataGet
#' @family streamObs
#' @export
FindUsgsStns.atomic <- function(stnLon=NULL, stnLat=NULL, within=NULL,
                                huc8=NULL, siteType='ST', hasDataTypeCd='iv') {
  argList <- list()
  argList$siteType       <- siteType
  argList$hasDataTypeCd  <- hasDataTypeCd
  # lat/lon
  if( !is.null(stnLon) && !is.null(stnLat) && !is.null(within) ) {
    if(length(whGt180<- which(stnLon>180.))) stnLon[whGt180] <- stnLon[whGt180]-360
    argList$bBox <- as.character(paste(as.character(format(stnLon-within,nsmall=7)),
                                       as.character(format(stnLat-within,nsmall=7)),
                                       as.character(format(stnLon+within,nsmall=7)),
                                       as.character(format(stnLat+within,nsmall=7)), sep=',' ))
  }
  # huc
  if (!is.null(huc8)) argList$huc <- as.character(huc8)
  #print(argList)
  do.call(dataRetrieval::whatNWISsites, argList)
}


#==============================================================================================
#' Get all the USGS streamgage information within a HUC8. 
#' 
#' \code{GetUsgsHucData} gets all the USGS streamgage information within a HUC8. If an
#' output path is supplied, an existing database is examined for existing records and
#' the data retrieved from the USGS only extends the available and is saved in the data base.
#' Could eventually use startDate and endDate to tweak this for calls which dont care about archival.
#' @param huc8 Character The eight-digit HUC code. 
#' @param outPath Character The path to the database directory
#' @param metaDBFileName Character The name of the database metadata file.
#' @examples
#' bldrHucData    <- GetUsgsHucData(huc='10190005')
#' satilpaHucData <- GetUsgsHucData(huc='03160203')
#' vansHucData    <- GetUsgsHucData(huc='03020104')
#' @keywords IO
#' @concept dataGet usgsStreamObs
#' @family streamObs
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
    LoadMetaDB(path=metaFilePath, metaDBFileName=metaDBFileName)
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
    foo<-GetUsgsIvProduct(subset(meta, parm_cd=='00060'))
    
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
#' Save the output of GetUsgsHucData to an archive.
#' 
#' \code{SaveHucData} take a list returned by \code{GetUsgsHucData}, create or append to an existing archive, and update
#' metadata file. Input list is grouped by HUC codes where each HUC has data and meta lists. The output files 
#' are written to outPath with the following format HHHHHHHH.data.RData where HHHHHHHH is the HUC8 code. The 
#' name of the metadata file is configurable. The metadata file contains metadata for all files in the outPath. 
#' @param hucData List Returned from GetUsgsHucData.
#' @param outPath Character The path to the database directory
#' @param metaDBFileName Character The name of the database metadata file.
#' @param overwriteHucDataFile Logical Replace/overwrite the existing data file for the HUC with the current data.
#' @examples
#' files <- SaveHucData(GetUsgsHucData(huc='10190005'), outPath='/Users/jamesmcc/streamflow/OBS') ##boulder, CO
#' files <- SaveHucData(GetUsgsHucData(huc='03160203'), outPath='/Users/jamesmcc/streamflow/OBS') ##satilpa, AL
#' files <- SaveHucData(GetUsgsHucData(huc='03020104'), outPath='/Users/jamesmcc/streamflow/OBS') ##vans, NC
#' @keywords IO database
#' @concept dataGet usgsStreamObs
#' @family streamObs
#' @export
SaveHucData <- function(hucData, outPath, 
                        metaDBFileName='usgsDataRetrieval.metaDatabase.RData', 
                        overwriteHucDataFile=FALSE) {
  ##What the HUC?
  allHuc <- unlist(plyr::llply(hucData, function(ll) ll$meta$siteInfo$huc_cd))
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
  assign(dataVar, hucData)
  outHucMetaDB <- hucData
  for (name in names(outHucMetaDB)) outHucMetaDB[[name]]$data <- NULL
  #str(outHucMetaDB)
  
  if(length(metaFilePath)) {
    # loads metaDB
    LoadMetaDB(path=outPath, metaDBFileName=metaDBFileName)
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
#' For an indivudal product, Improve site metadata to be stored in the database and file. 
#' 
#' \code{ImproveHucMeta} extracts the metadata to stash in a usgsDataRetrieval database for an individual product.
#' It gathers the attributes returned by \code{dataRetrieval::readNWISuv} , supplements siteInfo with
#' startTime and endTime for each station in UTC to assist appending the records. 
#' @param hucProdDf dataframe from dataRetrieval::readNWISuv
#' @return list of metadata for a 
#' @keywords IO internal
#' @concept dataGet usgsStreamObs
#' @family streamObs
#' @export
ImproveHucMeta <- function(hucProdDf) {
  
  hucMeta <- list( siteInfo      = attr(hucProdDf, 'siteInfo'),
                   variableInfo  = attr(hucProdDf, 'variableInfo'), 
                   statisticInfo = attr(hucProdDf, 'statisticInfo') )  
  
  meta2 <- dataRetrieval::readNWISdata(site=unique(hucProdDf$site_no),                                        
                                       hasDataTypeCd="iv", 
                                       siteOutput='expanded', service = "site")  
  
  if(FALSE) {
    ## THis is a test block to discover what is in common / repeated between the siteInfo which
    ## comes with hucProdDf and the more extended metadata in meta2 (which cant tell y)
    m<-merge.data.frame(meta2,hucMeta$siteInfo, by='site_no', all=TRUE, sort=TRUE)
    repeats <- data.frame( x=c('agency_cd.x','county_cd','dec_lat_va.x','dec_long_va', 'huc_cd',
                               'site_tp_cd','state_cd','station_nm.x','tz_cd','well_depth_va'), 
                           y=c('agency_cd.y','countyCd', 'dec_lat_va.y','dec_lon_va','hucCd', 
                               'siteTypeCd','stateCd','station_nm.y','timeZoneAbbreviation','hole_depth_va'), 
                           stringsAsFactors=FALSE)
    checkMatch <- plyr::ddply(repeats, plyr::.(x), 
                              function(df) {print(df$x); print(df$y);
                                            print(m[[df$x]]); print(m[[df$y]]);
                                            data.frame(match=all(m[[df$x]]==m[[df$y]]))} )
    ## cant be sure well_depth_va and hole_depth_va are the same.
    ## countyCd and county_cd are the same but countyCd is less accurate as it contains state code too.
  }
  
  ## check all the x names in x
  hucMeta$siteInfo$countyCd <- NULL
  xNames<-c('site_no',     'agency_cd',   'dec_lat_va',   'dec_long_va',  'huc_cd',
            'site_tp_cd',  'state_cd',     'station_nm',   'tz_cd')
  yNames<-c('site_no',     'agency_cd',   'dec_lat_va',   'dec_lon_va',   'hucCd', 
            'siteTypeCd',  'stateCd',      'station_nm',   'timeZoneAbbreviation')
  all(match(xNames, names(meta2)))
  all(match(yNames, names(hucMeta$siteInfo)))
  
  m2 <- merge.data.frame(meta2,hucMeta$siteInfo, by.x=xNames, by.y=yNames, sort=TRUE, all=TRUE)
  
  hucMeta$siteInfo <- m2
  
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
#' 
#' \code{GetUsgsIvProduct} gets instantaneous USGS data for a single product code
#' and separates the returned dataframe with attributes from \code{dataRetrieval::readNWISuv} into a list with 
#' separate data and metadata/attributes. The meta data are updated with startTime and endTime information.
#' @param prodDf is a dataframe returned by \code{dataRetrieval::whatNWISdata(stns$site_no, service = "uv")}
#' subet to an individual product code.
#' @return list(data=,meta=) 
#' @keywords IO internal
#' @concept dataGet usgsStreamObs
#' @family streamObs
#' @export
GetUsgsIvProduct <- function( prodDf ) {  
  prodData <- dataRetrieval::readNWISuv(prodDf$site_no, prodDf$parm_cd, 
                                        startDate=prodDf$startDate[1],  
                                        endDate=prodDf$endDate[1], 
                                        tz=prodDf$tz[1])
  prodMeta <- ImproveHucMeta(prodData)
  ## the subset simply removes the attributes
  list(data=subset(prodData,site_no %in% prodData$site_no), meta=prodMeta)
}


#==============================================================================================
#' Given a USGS site code, return its HUC8.
#' 
#' \code{GetSiteHuc} returns a HUC8 given a USGS site name. 
#' @param site Character USGS site number.
#' @return character HUC8
#' @examples
#' GetSiteHuc(FindUsgsStns(stnLon=254.67374999999998408,stnLat=40.018666670000001773,within=.001)$site_no)
#' @keywords IO 
#' @concept dataGet usgsStreamObs
#' @family streamObs
#' @export
GetSiteHuc <- function(site) dataRetrieval::readNWISsite(as.character(site))$huc


#==============================================================================================
#' Find the products available for a given site in the local database.
#' 
#' \code{QuerySiteProd} finds the products available for a given site in the local database.
#' @param site Character USGS site number.
#' @param path Character The path to the database.
#' @param metaDBFileName Character The name of the metadata file.
#' @examples
#' QuerySiteProd('06730500', '~/streamflow/OBS/')
#' @keywords database
#' @concept dataMgmt usgsStreamObs
#' @family streamObs
#' @export
QuerySiteProd <- function(site, path='.', 
                          metaDBFileName='usgsDataRetrieval.metaDatabase.RData') {
  ## loads metaDB
  LoadMetaDB(path=path, metaDBFileName=metaDBFileName)
  subset(QuerySiteInfo('site_no', path, metaDBFileName), site_no %in% site)$product
}


#' General query/get for NWIS instantaneous values.
#' @examples
#' haveOro <- QueryHaveSite('06727500', path='~/wrfHydroTestCases/usgsDb', retData=TRUE)
#' @export
QueryHaveSite <- function(site, path='.', 
                          metaDBFileName='usgsDataRetrieval.metaDatabase.RData',
                          get=FALSE, retData=FALSE) {
  vecDf <- FormalsToDf(QueryHaveSite)
  ret <- plyr::mlply(vecDf, QueryHaveSite.atomic)
  names(ret) <- site
  ret
}
  
QueryHaveSite.atomic <- function(site, path='.', 
                                 metaDBFileName='usgsDataRetrieval.metaDatabase.RData',
                                 get=FALSE, retData=FALSE) {
  have <- 
    as.logical(length(QuerySiteProd(site, path=path, 
                                     metaDBFileName=metaDBFileName)))
  if(have & !retData) return(have)
  
  if(get) {
    siteHuc8 <- GetSiteHuc(stnDf$site_no)
    ## do we have this HUC but not the site?
    haveHuc <- any(QuerySiteInfo('huc_cd')$huc_cd == site)
    if(haveHuc) {
      yesOrNo <-
        readline(prompt=paste0('The site was not found but the data for the HUC already exists. ',
                              'This currently requires re-retrieving the entire HUC and ',
                              'overwriting the existing data in the database. Overwrite? (y/n):'))
      overwrite <- yesOrNo == 'y'
      if(!overwrite) return(have)
    }
    files<-
      SaveHucData(GetUsgsHucData(huc8 = siteHuc8), outPath = dbPath, 
                  overwriteHucDataFile = overwrite)
    have <- file.exists(files[[1]])
    return(have)
  }
  
  if(have & retData)
    have <- QuerySiteData(site, 
                          product=QuerySiteProd(site, path=dbPath,metaDBFileName=metaDBFileName), 
                          path=dbPath)
  
  return(have)
}

#==============================================================================================
#' Find the name(site id) for a given site(name)  in the local database.
#' 
#' \code{QuerySiteName} returns the name (site id) for a given site ID (name) in the local database.
#' @param site Character USGS site number or name.
#' @param path Character The path to the database.
#' @param retSiteId Logical return ID (name if FALSE) This is only exposed in case there are issues, 
#'        should work by default.
#' @param metaDBFileName Character The name of the metadata file.
#' @return Character Site name or number.
#' @examples
#' QuerySiteName('06730500', '~/streamflow/OBS/')
#' QuerySiteName('BOULDER CREEK AT MOUTH NEAR LONGMONT, CO', '~/streamflow/OBS/')
#' @keywords database
#' @concept dataMgmt usgsStreamObs
#' @family streamObs
#' @export
QuerySiteName <- function(site, path='.', 
                          retSiteId=tryCatch(is.na(as.numeric(site)), 
                                             warning=function(w) TRUE, 
                                             error=function(e) TRUE), 
                          metaDBFileName='usgsDataRetrieval.metaDatabase.RData') {
  ## loads metaDB
  LoadMetaDB(path=path, metaDBFileName=metaDBFileName)
  if(retSiteId) {
    subset(QuerySiteInfo(c('site_no','station_nm'),path,metaDBFileName), 
           station_nm %in% site)[1,'site_no']
  } else {
    subset(QuerySiteInfo(c('site_no','station_nm'),path,metaDBFileName), 
           site_no %in% site)[1,'station_nm']
  }
}


#==============================================================================================
#' Returns the desired information from the database file.
#' 
#' \code{QuerySiteInfo} gets the specified info from the local database.
#' @param info Character vector, information fields in \code{HUC$prod$meta$SiteInfo$info}.
#' @param path Character The path to the database directory.
#' @param metaDBFileName Character The name of the metadata file.
#' @return dataframe of requested info with all available HUC and product codes.
#' @examples 
#' QuerySiteInfo(c('station_nm','site_no'), path='~/streamflow/OBS/')
#' @keywords database
#' @concept dataMgmt usgsStreamObs
#' @family streamObs
#' @export
QuerySiteInfo <- function(info=NULL, path='.', 
                          metaDBFileName='usgsDataRetrieval.metaDatabase.RData') {
  LoadMetaDB(path=path, metaDBFileName=metaDBFileName)
  if(is.null(info)) {
    infoVec <- print(names(metaDB[[1]][[1]]$meta$siteInfo))
    whInfo <- as.numeric(readline(prompt='Enter a vector of info to return:'))
    info <- infoVec[whInfo]
  }
  out <- reshape2::melt(plyr::llply(metaDB, function(huc) 
                                    plyr::llply(huc, function(prod) prod$meta$siteInfo[info])), id=info)
  names(out) <- c(info, 'product', 'HUC8')
  out
}


#==============================================================================================
#' Returns the data for given sites from local database.
#' 
#' \code{QuerySiteData} gets the specified data from the local database.
#' @param site Character USGS site number or vector of numbers.
#' @param product Character USGS product code number.
#' @param path Character path to the database.
#' @param metaDBFileName Character The name of the metadata file.
#' @return dataframe of data with pertinent attributes.
#' @examples 
#' p='~/streamflow/OBS/'
#' dataOrodell <- QuerySiteData(QuerySiteName("FOURMILE CREEK AT ORODELL, CO", p), '00060', p)
#' siteInfo<-QuerySiteInfo(c('station_nm','site_no','stateCd'), path=p)
#' dataCO <- QuerySiteData(subset(siteInfo, stateCd=='08' & product=='00060')$site_no, '00060', p)
#' dataMultiHuc <- QuerySiteData(c('06730500','02084557'),'00060',p)
#' dataMultiHuc <- QuerySiteData(c('06730500','02084557'),c('00060','00065'),p)
#' @keywords database
#' @concept dataMgmt usgsStreamObs
#' @family streamObs
#' @export
QuerySiteData <- function(site, product, path='.',
                          metaDBFileName='usgsDataRetrieval.metaDatabase.RData'){
  vecDf <- FormalsToDf(QuerySiteData)
  ret <- plyr::mlply(vecDf, QuerySiteData.scalar)
  names(ret) <- vecDf$site
  ret
}
  

#==============================================================================================
QuerySiteData.scalar <- function(site, product, path='.',
                                 metaDBFileName='usgsDataRetrieval.metaDatabase.RData'){
  if(!any(QuerySiteProd(site, path=path, metaDB=metaDBFileName) == product)) {
    warning(paste("No product",product,"at site",site,"."))
    return(NULL)
  }
  huc <- subset(QuerySiteInfo(c('site_no'), path=path, metaDBFileName=metaDBFileName), 
                product == product & site_no %in% site)$HUC8[1]
  load(paste0(path,'/',huc,'.data.RData'))
  productData <- get(paste0('data.',huc))[[product]]
  rm(list=paste0('data.',huc))
  ret <- subset(productData$data, site_no %in% site )
  ret <- within(ret, {agency_cd <- NULL})  ## this may or may not be desirable in the long run.
  attr(ret, 'siteInfo')      <- subset(productData$meta$siteInfo, site_no %in% site)
  attr(ret, 'variableInfo')  <- productData$meta$variableInfo
  attr(ret, 'statisticInfo') <- productData$meta$statisticInfo
  ret
}

#==============================================================================================
#' Pretty USGS site data makes nice headers and optionally converts to metric. 
#' 
#' \code{PrettySiteData} beautifies the data frame 
#' @param data Dataframe from QuerySiteData
#' @param metric Logical
#' @param metricOnly Logical
#' @param tz Character The timezone for the POSIXct dataTime variable to be returned. 
#' @param na.rm Logical Remove all missing observations?
#' @return dataframe similar to input with improved names and/or metric variables.
#' @examples 
#' p='~/streamflow/OBS/'
#' dataOrodell <- PrettySiteData(QuerySiteData(QuerySiteName("FOURMILE CREEK AT ORODELL, CO", p), 
#'                                             '00060', p), metricOnly=TRUE)
#' @keywords manip
#' @concept dataMgmt usgsStreamObs
#' @family streamObs
#' @export
PrettySiteData <- function(data, tz='UTC', metric=metricOnly, metricOnly=TRUE, 
                           na.rm=TRUE) {
  if(class(data)[1]=='data.frame')
    return(PrettySiteData.df(data, tz='UTC', metric=metric, metricOnly=metricOnly, na.rm=na.rm))
  if(class(data)[1]=='list')
    return(plyr::llply(data, PrettySiteData.df, tz='UTC', metric=metric, metricOnly=metricOnly, na.rm=na.rm))
}

PrettySiteData.df <- function(data, tz='UTC', metric=metricOnly, metricOnly=TRUE, 
                              na.rm=TRUE) {
  whNames <- TransUsgsProdStat(names(data),whichIn=TRUE)
  prettyNames <- TransUsgsProdStat(names(data)[whNames])
  names(data)[whNames] <- prettyNames
  attr(data,'variableInfo') <- attr(data,'statisticInfo') <- NULL
  
  if(tz != 'UTC') data$dateTime <- lubridate::with_tz(data$dateTime, tz=tz)
  data$tz_cd <- NULL
  
  if('Discharge (cfs)' %in% prettyNames) {
    attr(data,'variables') <- c('Discharge (cfs)')
    attr(data,'codes') <- c('Discharge code')
  }
  if('Stage (ft)'      %in% prettyNames) {
    attr(data,'variables') <- c('Stage (feet)')
    attr(data,'codes') <- c('Stage code')
  }
  
  if(metric) {
    if('Discharge (cfs)' %in% prettyNames) {
      data$`Discharge (cms)` <- data$`Discharge (cfs)`*cfs2cms
      if(metricOnly) {
        data$`Discharge (cfs)` <- NULL
        attr(data,'variables') <- c('Discharge (cms)')
      } else attr(data,'variables') <- c('Discharge (cfs)','Discharge (cms)')
    }
    
    if('Stage (ft)' %in% prettyNames) {
      data$`Stage (m)` <- data$`Stage (ft)`*feet2meters
      if(metricOnly) {
        data$`Stage (ft)` <- NULL
        attr(data,'variables') <- c('Stage (meters)')
      } else attr(data,'variables') <- c('Stage (feet)','Stage (meters)')
    }
  }
  
  if(na.rm & length(whMiss <- which(is.na(data[,attr(data,'variables')[1]]))) ) {
    data <- data[-whMiss,]
  }
    
  attr(data,'class') <- c('prettyUsgs', 'data.frame')
  data
}


##============================================================================================
#' Load the metadata for the USGS streamflow database.
#' 
#' \code{LoadMetaDB} Load the metadata for the USGS streamflow database.
#' @param path Character path to the meta DB. 
#' @param metaDBFileName Character name of the meta DB. 
#' @param envir Envrionment where it is to be loaded. 
#' @return Character the name of the variables loaded with the file (invisible).
#' @keywords internal database
#' @concept dataMgmt usgsStreamObs
#' @family streamObs
#' @export
LoadMetaDB <- function(path='.', 
                       metaDBFileName='usgsDataRetrieval.metaDatabase.RData', 
                       envir=globalenv()) {
  load(paste0(path,'/',metaDBFileName), envir=envir)  
}


##============================================================================================
#' Translate USGS product/stat codes to something readable (and vice versa).
#' 
#' \code{TransUsgsProdStat} Translate USGS product/stat codes to something readable (and vice versa) 
#' using a lookup table. 
#' @param names Character product_stat codes (e.g. 'X_00060_00011') or their translation 
#' (e.g. 'Discharge (cfs)') separated with an underscore. 
#' @param whichIn Logical
#' @return if whichIn==FALSE : Character of the translation.
#'         if whichIn==TRUE  : Integer index which passed names are in the table. 
#' @keywords database internal
#' @concept dataMgmt usgsStreamObs
TransUsgsProdStat <- function(names, whichIn=FALSE) {
  ## Elsewhere these rely on a single set of parentheses around the units.
  prodStatLookup <- c( X_00060_00011    ='Discharge (cfs)',   ##instantaneous is 00011 but not worth saying IMO
                       X_00060_00011_cd ='Discharge code',
                       X_00065_00011    ='Stage (ft)',
                       X_00065_00011_cd ='Stage code' )
  code2Name <- any(names %in% names(prodStatLookup))
  name2code <- any(names %in% prodStatLookup)
  ## mixed
  if (code2Name && name2code)
    warning('Names argument to TransUsgsProdStat contains both names and codes', 
            immediate.=TRUE)
  
  ## could be all missing but not mixed.
  if(whichIn) {
    if(code2Name) theMatch <- match(names,names(prodStatLookup))
    if(name2code) theMatch <- match(names,prodStatLookup)
    return(which(!is.na(theMatch)))
  } 
  
  ## all missing
  if (!code2Name && !name2code) 
    warning('Names argument to TransUsgsProdStat contains neither names nor codes', 
            immediate.=TRUE)
  
  ## some missing
  if(!all(names %in% names(prodStatLookup)) | !all(names %in% names(prodStatLookup)))
      warning(paste('There are codes or names passed to TransUsgsProdStat which are not in the lookup table.',
                    'You can use the argument whichIn to determine which passed character as in the lookup table.',
                    'If there are entries you would like to add to the lookup tabble,',
                    'please do so and contribute your additions.'),
              immediate.=TRUE)
  
  if(code2Name) return( prodStatLookup[names])
  if(name2code) return( prodStatLookup[match(names,prodStatLookup)] )
}


##============================================================================================
#' Plot USGS site data which has been prettied with PrettySiteData.
#' 
#' \code{PlotPrettyData} plots USGS site data which has been prettied with \code{PrettySiteData}.
#' @param prettyUsgs dataframe returned from PrettySiteData
#' @param plot Logical to plot before returning or not.
#' @param errInnerQntl The inner quantile of the error estimate to be plotted with error bars. For example, 
#'        the "68-95-99.7 rule" where specifying these as the errInnerQntl would display 1, 2, and 3 
#'        standard deviations, respectively.
#' @return A function(closure) with arguments controlling the look of its graphical output. It's actual
#'         return value is a list of ggplot2 object which can be custom manipulated. 
#' @examples
#' # See vignette "Collect USGS stream observations and build a local database" for examples.
#' @keywords hplot
#' @concept plot usgsStreamObs
#' @family streamObs
#' @export
PlotPrettyData <- function(prettyUsgs, plot=TRUE, errInnerQntl=.995) {
  if(!('prettyUsgs' %in% class(prettyUsgs))) {
      warning('The data argument to PlotPrettyData is not of class prettyUsgs. Returning.')
      return(NULL)
  }
  variables <- attr(prettyUsgs,'variables')
  codes     <- attr(prettyUsgs,'codes')
  variances <- attr(prettyUsgs,'variances')
  stDevs    <- attr(prettyUsgs,'st.devs.')
  errVars   <- c(variances,stDevs)
  
  ## if there are errVars, only plot a single variable with errors. 
  if(length(variables)>1 && length(errVars)) {
    errUnits <- plyr::laply(strsplit(errVars,'[(^)]'), '[[', 2)
    if(length(errVars)==1) {
      theVar <- variables[grep(errUnits,variables)]
      theErr <- errVars
    } else {
      varsWErr <- variables[plyr::laply(errUnits, grep, variables)]
      whVar <- readline(prompt=paste0('Please select a single variable to plot with error bars: \n',
                                      paste(1:length(varsWErr),varsWErr, sep=': ', collapse=' \n'),' \n'))
      theVar <- variables[as.numeric(whVar)]
      theErr <- errVars[grep( strsplit(theVar,'[(^)]')[[1]][2], errUnits )]
    }
    prettyUsgs <- prettyUsgs[,c("dateTime","site_no", codes, theVar, theErr)]
    variables <- theVar
    errVars   <- theErr
  }
    
  plotData  <- reshape2::melt(prettyUsgs, id=c("dateTime","site_no", codes, errVars))
  
  if (length(errVars)) {
    ## x==qnorm(pnorm(x,0,1),0,1) ## my reminder i wish were in the R documentation.
    obsSd <- plotData[[theErr]]
    if(length(variances)) obsSd <- sqrt(obsSd)
    plotData$err <- -1*qnorm( ((1-errInnerQntl)/2), mean=0, sd=obsSd )
    timePlot <- ggplot2::ggplot(plotData, ggplot2::aes(x=dateTime, y=value, 
                                                       ymin=value-err, ymax=value+err) )
  } else {
    timePlot <- ggplot2::ggplot(plotData, ggplot2::aes(x=dateTime, y=value))
  }
  timePlot <- timePlot + ggplot2::theme_bw()
  
  multiSite <- if(length(unique(plotData$site_no))>1) TRUE else FALSE
  multiVar  <- if(length(variables)>1)                TRUE else FALSE
  if( multiSite &  multiVar) 
    timePlot <- timePlot + ggplot2::facet_grid(site_no~variable, scale='free_xy')
  if( multiSite & !multiVar) 
    timePlot <- timePlot + ggplot2::facet_wrap(site_no~variable, scale='fixed', ncol=1)
  if(!multiSite &  multiVar) 
    timePlot <- timePlot + ggplot2::facet_wrap(site_no~variable, scale='free_y', ncol=length(variables))
  
  ## more stuff to be added here.
  OutFunc <- function(plot=TRUE, yLog=FALSE, pointSize=1, pointColor='black', errColor='red') {
    if(yLog) timePlot <- timePlot + ggplot2::scale_y_log10()
    timePlot <- timePlot + ggplot2::geom_point(size=pointSize, color=pointColor) 
    if (length(errVars)) timePlot <- timePlot + ggplot2::geom_errorbar(color=errColor)
    if(plot) print(timePlot)
    invisible(timePlot)
  }
    
  if(plot) OutFunc()
  invisible(OutFunc)
}

##============================================================================================
#' Subset prettyUsgs objects.
#' 
#' \code{subset.prettyUsgs} subsets prettyUsgs objects and retains their attributes.
#' @param prettyUsgs A dataframe of class \code{c("prettyUsgs", "data.frame")} returned from \code{PrettySiteData}
#' @param ... additional arguments to subset.data.frame
#' @return A dataframe of class c("prettyUsgs", "data.frame")
#' # See vignette "Collect USGS stream observations and build a local database" for examples.
#' @keywords manip
#' @concept dataMgmt usgsStreamObs
#' @family streamObs
#' @export
subset.prettyUsgs <- function(prettyUsgs, ... ) {
 class     <- attr(prettyUsgs, 'class')
 variables <- attr(prettyUsgs, 'variables')
 codes     <- attr(prettyUsgs, 'codes')
 variances  <- attr(prettyUsgs, 'variances')
 stDevs     <- attr(prettyUsgs, 'st.devs.')
 attr(prettyUsgs, 'class') <- 'data.frame'
 subPretty <- subset(prettyUsgs, ...)
 attr(subPretty, 'class')      <- class
 attr(subPretty, 'variables')  <- variables
 attr(subPretty, 'codes')      <- codes
 attr(subPretty, 'variances')  <- variances
 attr(subPretty, 'st.devs.')   <- stDevs
 subPretty
}
