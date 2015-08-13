
## this code is for development of WriteNcPrettyUsgs
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
WriteNcPrettyUsgs <- function(prettyDf, outPath='.') {
  
  ## break up by site*product until we get ragged arrays. 
  
  varList = list()
  varList[[1]] <- list( name='streamflow',
                        longname='Precipitation Multiplier',
                        units='-',
                        precision = 'float',
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
  dum <- MkNcdf( varList, globalAttList=globalAttList, filename='~/testHistoricalStreamData.nc' )
  
  
}


#' Write a USGS discharge timeslice to a netcdf file. 
#' 
#' @param dfByPosix   Dataframe, a data frame with the following columns: 
#' \code{site_no}, \code{dateTime}, \code{code}, \code{queryTime}, \code{discharge.cms}, 
#' and \code{variance} where dateTime is the same for the entire dataframe. 
#' @param outPath     Character, the file name with path for the output netcdf file. 
#' @examples
#' ## See \link{MkUsgsTimeSlice}.
#' @keywords internal
#' @export
WriteNcTimeSlice <- function(dfByPosix, outPath, sliceResolution) {
    
    dateTimeRound <- dfByPosix$dateTimeRound   ## this is a string
    dfByPosix$dateTimeRound <- NULL
    fileName <- TimeSliceFileName(dateTimeRound[1], sliceResolution)
    outFileName <- paste0(outPath,'/',fileName)

    ## does the file exist?
    if(file.exists(outFileName)) {
      print("file exists: merging... ")
      ## one cant change random rows of a netcdf file, only slabs.
      ## seems more efficient to read in the old file and merge them old with the 
      ## new and sort it out and then overwrite the file.       
      dfByPosix <- rbind(dfByPosix, ReadNcTimeSlice(outFileName))
    }

    ## could have multiple of the same station at a given time. 
    ## simly take the most recent. if it got this far it has had some qc
    dfByPosix <- plyr::ddply(dfByPosix, plyr::.(site_no),
                             function(df) df[which.max(df$queryTime)[1],])
    
# you'd have to know the rounding time to get this right... checking 
# for times outside the window of this file.
#     if(length(unique(format(dfByPosix$dateTime, '%Y-%m-%d_%H:%M:%S')))>1) {
#       print(nrow(dfByPosix))
#       print(dateTimeRound)
#       print(dfByPosix$dateTime)
#       print('Times in ')
#       stop() #print(dfByPosix$dateTime)
#     }
    


    ## need to set the missing value used by ncdf4? i think it's NA by default
    dimensionList <-
     list(  # n.b. the dimension order: z,y,x,t
       stationIdInd=list(name='stationIdInd',
                         units='', 
                         values=1:as.numeric(length(dfByPosix$site_no)),
                         unlimited=TRUE,
                         create_dimvar=FALSE),
       
       stationIdStrLen=list(name='stationIdStrLen',
                       units='', 
                       values=1:15,
                       unlimited=FALSE,
                       create_dimvar=FALSE),
       
       codeStrLen=list(name='codeStrLen',
                       units='', 
                       values=1:4,
                       unlimited=FALSE,
                       create_dimvar=FALSE),
       
       timeStrLen=list(name='timeStrLen',
                       units='', 
                       values=1:19,
                       unlimited=FALSE,
                       create_dimvar=FALSE)
     )
     
    
    varList = list()
    varList[[1]] <- 
      list( name='stationId',
            longname='USGS station identifer of length 15',
            units='',
            precision = 'char',
            #missing = ,
            dimensionList=dimensionList[c('stationIdStrLen','stationIdInd')],
            data = dfByPosix$site_no )
    
    varList[[2]] <- 
      list( name='time',
            longname='YYYY-MM-DD_HH:mm:ss UTC',
            units='UTC',
            precision = 'char',
            #missing = ,
            dimensionList=dimensionList[c('timeStrLen','stationIdInd')],
            data = format(dfByPosix$dateTime, '%Y-%m-%d_%H:%M:%S') )

    varList[[3]] <- 
      list( name='discharge',
            longname='Discharge.cubic_meters_per_second',
            units='m^3/s',
            precision = 'float',
            #missing = ,
            dimensionList=dimensionList[c('stationIdInd')],
            data = dfByPosix$discharge.cms )
    
    varList[[4]] <- 
      list( name='discharge_quality',
            longname='Discharge quality 0 to 100 to be scaled by 100.',
            units='-',
            precision = 'short',
            multfactor='.01',
            #missing = ,
            dimensionList=dimensionList[c('stationIdInd')],
            data = dfByPosix$code )
    
    varList[[5]] <- 
      list( name='queryTime',
            longname='queryTime',
            units='seconds since 1970-01-01 00:00:00 local TZ',
            precision = 'float',
            #missing = ,
            dimensionList=dimensionList[c('stationIdInd')],
            data = as.integer(dfByPosix$queryTime) )
  
    globalAttList <- list()
    globalAttList[[1]] <- list(name='fileUpdateTime',
                               value=format(Sys.time(),'%Y-%m-%d_%H:%M:%S',tz='UTC'), precision="text" )
    globalAttList[[2]] <- list(name='sliceCenterTime',
                               value=dateTimeRound[1], precision="text" )  ## already a string
    globalAttList[[3]] <- list(name='sliceTimeResolution',
                               value=formatC(sliceResolution, width=2), precision="text" )
    
    MkNcdf( varList, globalAttList=globalAttList, 
            filename=paste0(outPath,'/',fileName), 
            overwrite=TRUE )
}

##====================================================================================
TimeSliceFileName <- function(POSIXctOrChr, sliceResolution) {
  if(class(POSIXctOrChr)[1] == 'POSIXct') {
      paste0(format(POSIXct,'%Y-%m-%d_%H:%M:%S'), 
             '.', formatC(sliceResolution, width=2, flag='0'),'min', 
             '.usgsTimeSlice.ncdf')
  } else paste0(POSIXctOrChr,
                '.', formatC(sliceResolution, width=2, flag='0'),'min',
                '.usgsTimeSlice.ncdf')
}
##====================================================================================
#'
#' Read a USGS discharge data timeslice from a netcdf file. 
#' 
#' This is kind of the inverse of WriteNcTimeSlice to be used in extending existing
#' timeslices on file.
#' @param file, the ncdf file to read.
#' @examples
#' \dontrun{
#' sliceFiles <- list.files('~/usgsStreamData/timeSliceData/','.*', full.names=TRUE)
#' ReadNcTimeSlice(tail(sliceFiles,1))
#' }
#' @keywords internal
#' @export
ReadNcTimeSlice <- function(file) {  
  ncRead <- ncdf4::nc_open(file)

  varNames <- names(ncRead$var)
  sliceDf <- as.data.frame(plyr::llply(NamedList(varNames),
                           function(nn) ncdf4::ncvar_get(ncRead,nn)), stringsAsFactors=FALSE)
  
  dimNames <- setdiff(names(ncRead$dim),c('codeStrLen', 'timeStrLen', 
                                          'stationIdStrLen', 'stationIdInd'))
  for(dd in dimNames) {
    sliceDf[[dd]] <- ncdf4::ncvar_get(ncRead,dd)
  }
  ncdf4::nc_close(ncRead)
  
  sliceDf$queryTime <- as.POSIXct('1970-01-01 00:00:00',tz='UTC') + sliceDf$queryTime
  sliceDf$time <- as.POSIXct(sliceDf$time, 
                                 format='%Y-%m-%d_%H:%M:%S', tz='UTC')
  sliceDf <- plyr::rename(sliceDf, c("discharge"="discharge.cms",
                                     "time"="dateTime",
                                     "discharge_code"="code",
                                     "stationId"="site_no"))
  
  sliceDf
}
  