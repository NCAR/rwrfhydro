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
#' @param ndvSrc Missing value of source data being regridded. 
#' used as a mask value during regridding calls.
#' @return dataOut the stack of regridded matrices.
#' @examples
#' \dontrun{
#' dataOut <- regrid(hrrrDSWRF,latIn,lonIn,'geo_em.d01.nc',1)
#' }
#'
#' @keywords io geospatial regrid ESMF
#' @concept regrid geospatial  
#' @family regrid
#' @useDynLib rwrfhydro
#' @export
regrid <- function(dataIn,latIn,lonIn,geoFile,method,ndvSrc){
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
    print(latInCorners[1,1,1])
    print(latInCorners[1,2,1])
    print(latInCorners[2,1,1])
    print(latInCorners[2,2,1])
  }
  
  #Establish regridded array
  dataOut <- array(ndvSrc,c(nxGeo,nyGeo,nFTimes,nSteps))

  #Call Fortran shared object. Call special regrid for 'conserve' method given
  #it requires additional arguments.
  if(method == 5){
    dataTemp <- .Fortran('regrid_conserve',nxIn,nyIn,nFTimes,nSteps,
                         dataIn,nxGeo,
                         nyGeo,dataOut,latIn,lonIn,latGeo,lonGeo,
                         latInCorners,lonInCorners,latGeoCorners,
                         lonGeoCorners,ndvSrc,error)
    error <- dataTemp[[18]]
    if(error != 0){
      stop(paste0('ERROR: regridConserve returned exit status of: ',error))
    }
    dataOut <- dataTemp[[8]]
    #The regridding behavior in Fortran adds the missing value to all 
    #values. Subrtract the missing value to get the corret grid.
    #indTemp <- which(dataOut != ndvSrc)
    #dataOut[indTemp] <- dataOut[indTemp] - ndvSrc
  } else {
    dataTemp <- .Fortran('regrid',nxIn,nyIn,nFTimes,nSteps,
                         dataIn,nxGeo,
                         nyGeo,dataOut,latIn,lonIn,latGeo,lonGeo,method,
                         ndvSrc,error)

    error <- dataTemp[[15]]
    if(error != 0){
      stop(paste0('ERROR: regrid returned exist status of: ',error))
    }
    dataOut <- dataTemp[[8]]
    #The regridding behavior in Fortran adds the missing value to all 
    #values. Subrtract the missing value to get the corret grid.
    #indTemp <- which(dataOut != ndvSrc)
    #dataOut[indTemp] <- dataOut[indTemp] - ndvSrc
  }
  
  return(dataOut)

}
