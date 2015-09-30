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
#' @param Integer method of regridding. Options are 2 - "conserve",
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

  #Establish number of slices and cast Fortran variables
  nSteps <- dim(dataIn)[[3]]
  nxIn <- dim(dataIn)[[1]]
  nyIn <- dim(dataIn)[[2]]
  nxIn <- as.integer(nxIn)
  nyIn <- as.integer(nyIn)
  nSteps <- as.integer(nSteps)
  method <- as.integer(method)
  error <- as.integer(0)
  ndvSrc <- as.integer(ndvSrc)

  #Cast all longitude values to degrees east values
  ind <- which(lonIn < 0.0)
  lonIn[ind] <- lonIn[ind] + 360.0
  ind <- which(lonGeo < 0.0)
  lonGeo[ind] <- lonGeo[ind] + 360
  
  #Establish regridded array
  dataOut <- array(-9999,c(nxGeo,nyGeo,nSteps))

  #Call Fortran shared object
  dataTemp <- .Fortran('regrid',nxIn,nyIn,nSteps,dataIn,nxGeo,
                nyGeo,dataOut,latIn,lonIn,latGeo,lonGeo,method,
                ndvSrc,error)

  error <- dataTemp[[14]]
  if(error != 0){
    stop(paste0('ERROR: regrid returned exist status of: ',error))
  }
  dataOut <- dataTemp[[7]]

  return(dataOut)

}
