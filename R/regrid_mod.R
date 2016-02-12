#' High level call to regrid data to WRF-Hydro domain.
#' 
#' \code{regrid} High level call to regrid data.
#' 
#' @param dataIn the stack of matrices of data to be regridded.
#' @param latIn 2D array of latitude values of the center 
#'  of each pixel cell of the data being regridded.
#' @param lonIn 2D array of longitude values of the center
#'  of each pixel cell of the data being regridded.
#' @param geoFile geogrid file of WRF-Hydro domain.
#' @param method integer of regridding. Options are 2 - "conserve",
#'  1 - "bilinear", 3 - "nneighbor".
#' @param wghtFile NetCDF weight file that contains regridding 
#' weight arrays to be passed to ESMF for sparse matrix multiplication.
#' @param ndvSrc Missing value of source data being regridded. 
#' used as a mask value during regridding calls.
#' @return dataOut the stack of regridded matrices.
#' @examples
#' \dontrun{
#' dataOut <- regrid(hrrrDSWRF,latIn,lonIn,'geo_em.d01.nc',1,'./wghts_hrrr.Rdata',
#'                   9999)
#' }
#'
#' @keywords io geospatial regrid ESMF
#' @concept regrid geospatial  
#' @family regrid
#' @useDynLib rwrfhydro
#' @export
regrid <- function(dataIn,latIn,lonIn,geoFile,method,wghtFile,ndvSrc){
  #Check for existence of geogrid file
  if(!file.exists(geoFile)){
    stop(paste0('ERROR: Geogrid file: ',geoFile,' not found.'))
  }
  
  #Check for existence of weight file
  if(!file.exists(wghtFile)){
    stop(paste0('ERROR: Weight file: ',wghtFile,' not found.'))
  }
  
  #Check for valid method integer variable.
  if((method != 1) & (method != 3) & (method != 5)){
    stop(paste0(method,' Not a valid integer method value.'))
  }

  #Open geogrid file and extract 2D lat/lon 
  ncid <- ncdf4::nc_open(geoFile)
  latGeo <- ncdf4::ncvar_get(ncid,'XLAT_M')
  lonGeo <- ncdf4::ncvar_get(ncid,'XLONG_M')
  nxGeo <- dim(latGeo)[[1]]
  nyGeo <- dim(latGeo)[[2]] 
  nxGeo <- as.integer(nxGeo)
  nyGeo <- as.integer(nyGeo)
  ncdf4::nc_close(ncid)

  #Establish number of slices and cast Fortran variables
  nSteps <- dim(dataIn)[[4]]
  nxIn <- dim(dataIn)[[1]]
  nyIn <- dim(dataIn)[[2]]
  nFTimes <- dim(dataIn)[[3]]
  nxIn <- as.integer(nxIn)
  nyIn <- as.integer(nyIn)
  nSteps <- as.integer(nSteps)
  nFTimes <- as.integer(nFTimes)
  method <- as.integer(method)
  error <- as.integer(0)
  ndvSrc <- as.numeric(ndvSrc)

  #Cast all longitude values to degrees east values
  ind <- which(lonIn < 0.0)
  lonIn[ind] <- lonIn[ind] + 360.0
  ind <- which(lonGeo < 0.0)
  lonGeo[ind] <- lonGeo[ind] + 360
  
  #Calculate corner lat/lon coordinates for each pixel cell if 'conserve'
  #was chosen. This is necessary for that regridding method.
  if(method == 5){
    latInCorners <- array(-9999.0,c(nxIn,nyIn,4))
    lonInCorners <- array(-9999.0,c(nxIn,nyIn,4))
    latGeoCorners <- array(-9999.0,c(nxGeo,nyGeo,4))
    lonGeoCorners <- array(-9999.0,c(nxGeo,nyGeo,4))
    dataTemp <- .Fortran('calcLatLonCorners',nxIn,nyIn,nxGeo,nyGeo,
                         latIn,lonIn,
                         latGeo,lonGeo,latInCorners,lonInCorners,
                         latGeoCorners,lonGeoCorners,error)
    error <- dataTemp[[13]]
    if(error != 0){
      stop(paste0('ERROR: calcLatLonCorners returned an exist status of: ',error))
    }
    latInCorners <- dataTemp[[9]]
    lonInCorners <- dataTemp[[10]]
    latGeoCorners <- dataTemp[[11]]
    lonGeoCorners <- dataTemp[[12]]
  }
  
  #Establish regridded array
  dataOut <- array(ndvSrc,c(nxGeo,nyGeo,nFTimes,nSteps))
  
  #Open weight file and pull out regridding weight arrays
  weightList <- readWghtFile(wghtFile,nxIn,nyIn,nxGeo,nyGeo)
  fctLen <- as.integer(weightList$fctLen)
  factorList <- as.array(weightList$factorList)
  factorIndexList <- as.array(weightList$factorIndexList)
  
  #Call Fortran shared object. Call special regrid for 'conserve' method given
  #it requires additional arguments.
  if(method == 5){
    #dataTemp <- .Fortran('regrid_conserve',nxIn,nyIn,nFTimes,nSteps,
    #                     dataIn,nxGeo,nyGeo,dataOut,
    #                     fctLen,factorList,
    #                     factorIndexList,ndvSrc,error)
    error <- dataTemp[[13]]
    if(error != 0){
      stop(paste0('ERROR: regridConserve returned exit status of: ',error))
    }
    dataOut <- dataTemp[[8]]
  } else {
    dataTemp <- .Fortran('regrid',nxIn,nyIn,nFTimes,nSteps,
                         dataIn,nxGeo,nyGeo,dataOut,
                         fctLen,factorList,factorIndexList,
                         ndvSrc,error)

    error <- dataTemp[[13]]
    if(error == -99){
      print('ERROR: Rwrfhydro built without regridding support.')
      warning('ERROR: Please rebuild package with ESMF and GRIB API libraries.')
    } else if(error != 0){
      stop(paste0('ERROR: regrid returned exist status of: ',error))
    }
    dataOut <- dataTemp[[8]]
  }
  
  return(dataOut)

}

#' High level call to generate regridding weight NetCDF file.
#' 
#' \code{genWghtFile} High level call to generate regridding 
#' weight file.
#' 
#' @param geoFile geogrid file of WRF-Hydro domain.
#' @param nxIn Integer number of columns of source data.
#' @param nyIn Integer number of rows of source data.
#' @param latSrc Grid of center-stagger latitude values for source data.
#' @param lonSrc Grid of center-stagger longitude values for soure data.
#' @param method Integer of regridding method. Options are 2 - "conserve",
#'  1 - "bilinear", 3 - "nneighbor". 
#' @param srcDummy Array of sample source data to be used in generation of
#' weights. This will be used to generate the mask.
#' @param ndv Source NDV value used in masking.
#' @param wghtFile Output weight file to be generated.
#' @examples
#' \dontrun{
#' genWghtFile('./geo_em.d02.nc',nxIn,nyIn,latGRIB,lonGRIB,1,dataIn[,,],
#' 9999,'./wght_hrrr_rio_grande.nc')
#' }
#' 
#' @keywords regrid ESMF geospatial io
#' @concept regrid geospatial
#' @family regrid
#' @useDynLib rwrfhydro
#' @export
genWghtFile <- function(geoFile,nxIn,nyIn,latSrc,lonSrc,method,srcDummy,
                        ndv,wghtFile){
  
  #Check for existence of geogrid file
  if(!file.exists(geoFile)){
    stop(paste0('ERROR: Geogrid file: ',geoFile,' not found.'))
  }
  
  #Check for valid method integer variable.
  if((method != 1) & (method != 3) & (method != 5)){
    stop(paste0(method,' Not a valid integer method value.'))
  }
  
  #Open geogrid file and extract 2D lat/lon 
  ncid <- ncdf4::nc_open(geoFile)
  latGeo <- ncdf4::ncvar_get(ncid,'XLAT_M')
  lonGeo <- ncdf4::ncvar_get(ncid,'XLONG_M')
  nxGeo <- dim(latGeo)[[1]]
  nyGeo <- dim(latGeo)[[2]] 
  nxGeo <- as.integer(nxGeo)
  nyGeo <- as.integer(nyGeo)
  ncdf4::nc_close(ncid)
  
  #Establish variables for generation of weights.
  nxIn <- as.integer(nxIn)
  nyIn <- as.integer(nyIn)
  method <- as.integer(method)
  ndv <- as.numeric(ndv)
  fctLen <- as.integer(0)
  lTest1 <- nxIn*nyIn*5
  lTest2 <- nxGeo*nyGeo*5
  if(lTest1 > lTest2){
    fctLenTemp <- as.integer(lTest1)
  } else {
    fctLenTemp <- as.integer(lTest2)
  }

  factIndTemp <- array(-9999,c(2,fctLenTemp)) #-9999 because a positive 9999
  #is a possible index value
  factTemp <- array(ndv,c(fctLenTemp))
  mskArray <- array(ndv,c(nxIn,nyIn))  
  error <- as.integer(0)
  
  dataTemp <- .Fortran('generate_weights',nxIn,nyIn,nxGeo,nyGeo,
                       fctLenTemp,srcDummy,latSrc,lonSrc,latGeo,lonGeo,
                       method,ndv,fctLen,factTemp,factIndTemp,
                       error)
  
  error <- dataTemp[[16]]
  if(error == -99){
    print('ERROR: Rwrfhydro built without regridding support.')
    warning('ERROR: Please rebuild package with ESMF and GRIB API libraries.')
  } else if(error != 0){
    stop(paste0('ERROR: generate_weights returned an exit status of: ',error))
  }
  fctLen <- dataTemp[[13]]
  factTemp <-dataTemp[[14]]
  factIndTemp <- dataTemp[[15]]
  
  #place weight arrays into proper arrays.
  factorList <- array(ndv,c(fctLen))
  factorIndexList <- array(ndv,c(2,fctLen))
  factorList[1:fctLen] <- factTemp[1:fctLen]
  factorIndexList[1,] <- factIndTemp[1,1:fctLen]
  factorIndexList[2,] <- factIndTemp[2,1:fctLen]

  #Save weight arrays and lat/lon info as Rdata file. Originally was 
  #NetCDF, but discovered a bug in the code writing large arrays.
  save(latSrc,lonSrc,latGeo,lonGeo,factorList,factorIndexList,file=wghtFile)
  
}

#' Open weight file and extract weight arrays used for ESMF regridding.
#' 
#' \code{readWghtFile} Open weight file and extract important weight arrays.
#' 
#' @param wghtFile Weight file opened for extract.
#' @param nxSrc Integer number of columns of source data.
#' @param nySrc Integer number of rows of source data.
#' @param nxDst Integer number of columns of destination data.
#' @param nyDst Integer number of rows of destination data.
#' @return list with regridding weight arrays
#' @examples
#' \dontrun{
#' list <- readWghtFile('./hrrr_wghts_rio_grande.nc',
#' nxIn,nyIn,nxGeo,nyGeo)
#' }
#' @keywords regrid, geospatial, ESMF
#' @concept regrid, ESMF
#' @family regrid ESMF
#' @export
readWghtFile <- function(wghtFile,nxSrc,nySrc,nxDst,nyDst){
  #Check to make sure weight file exists
  if(!file.exists(wghtFile)){
    stop(paste0('ERROR: weight file: ',wghtFile,' not found.'))
  }
  
  #Open weight file
  load(wghtFile)
  
  if((dim(latSrc)[1] != nxSrc) | (dim(latSrc)[2] != nySrc)){
    stop('ERROR: Row/Column mistmatch between source data and weight file.')
  }
  if((dim(latGeo)[1] != nxDst) | (dim(latGeo)[2] != nyDst)){
    stop('ERROR: Row/Column mismatch between destination data and weight file.')
  }
  
  #Determine length of weight arrays
  fctLen <- dim(factorList)[1]
  
  #Create output list to hold data
  outList <- list("factorList" = factorList, "factorIndexList" = factorIndexList,
                  "fctLen" = fctLen)
   
  return(outList)
  
}
