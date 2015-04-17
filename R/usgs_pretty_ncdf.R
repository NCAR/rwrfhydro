
if(FALSE){
  ## look at platoro example
  poNcid <- ncdf4::nc_open('~/Desktop/ALL_PLATORO_DATA.nc')
  names(poNcid)
  names(poNcid$dim)
  
  ##bring in some pretty data.
  
  GetMultiSiteData <- function(site, product) {
    pDf <- PrettySiteData(QuerySiteData(site=site, product=product, path=dbPath))
    pDf$variable <- attr(pDf, 'variables')[1]
    renames <- c('value', 'code')
    names(renames) <- c(attr(pDf,'variables')[1], attr(pDf,'codes')[1])
    print(renames)
    pDf <- plyr::rename(pDf, renames)
    pDf
  }
  
  dbPath='~/wrfHydroTestCases/usgsDb/'
  theSites <- c('06724970','06730200')
  theProds <- c('00060','00065')

  collatedAbsc <- data.frame(site=rep(theSites, each=length(theProds)), product=theProds, 
                                      stringsAsFactors=FALSE)
  siteData <- plyr::mdply(collatedAbsc, GetMultiSiteData, .inform=TRUE)
  
  
  ## would be nice to have both station_id and station_names in here
  
}


## For now there variable length (ragged) arrays are not supported in the ncdf4 package. After  
## emailing Pierce, who welcomded the addition, I sketched how this might be done and we may 
## do it.
## For now, we will have to create one file per station*product. If returning the file name
## as success, that's a plyr::daply. 

#' @export
MkNcPrettyUsgs <- function(prettyDf, outPath='.') {
  
  ## break up by site*product until we get ragged arrays. 
  
  varList = list()
  varList[[1]] <- list( name='streamflow',
                        longname='Precipitation Multiplier',
                        units='-',
                        precision = 'double',
                        missing = -9999,
                        dimensionList = list(scalar=list(name='scalar',values=1,
                                                         units='-', unlimited=FALSE,
                                                         create_dimvar=FALSE)),
                        data = 1:1 )
  
  #// global attributes:
  #  :featureType = "timeSeries" ;
  #:Conventions = "CF-1.6" ;
  
  #' globalAttList <- list()
  #' globalAttList[[1]] <- list(name='Restart_Time',value='2012-07-05_00:00:00', precision="text")
  #' globalAttList[[2]] <- list(name='Some reall atts',value='#$%^!!', precision="text" )
  dum <- MkNcdf( varList, globalAttList, '~/testHistoricalStreamData.nc' )
  
  
}