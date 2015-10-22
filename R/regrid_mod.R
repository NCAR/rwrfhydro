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
#' dataOut <- regrid(hrrrDSWRF,latIn,lonIn,'geo_em.d01.nc',1,'./wghts_hrrr.nc',
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
    dataTemp <- .Fortran('regrid_conserve',nxIn,nyIn,nFTimes,nSteps,
                         dataIn,nxGeo,nyGeo,dataOut,
                         fctLen,factorList,
                         factorIndexList,ndvSrc,error)
    error <- dataTemp[[13]]
    if(error != 0){
      stop(paste0('ERROR: regridConserve returned exit status of: ',error))
    }
    dataOut <- dataTemp[[8]]
  } else {
    dataTemp <- .Fortran('regrid',nxIn,nyIn,nFTimes,nSteps,
                         dataIn,nxGeo,nyGeo,dataOut,method,
                         fctLen,factorList,factorIndexList,
                         ndvSrc,error)

    error <- dataTemp[[14]]
    if(error != 0){
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
  factIndTemp <- array(-9999,c(2,(nxIn*nyIn))) #-9999 because a positive 9999
  #is a possible index value
  factTemp <- array(ndv,c(nxIn*nyIn))
  mskArray <- array(ndv,c(nxIn,nyIn))  
  error <- as.integer(0)
  
  dataTemp <- .Fortran('generate_weights',nxIn,nyIn,nxGeo,nyGeo,
                       srcDummy,latSrc,lonSrc,latGeo,lonGeo,
                       method,ndv,fctLen,factTemp,factIndTemp,
                       error)
  
  error <- dataTemp[[15]]
  fctLen <- dataTemp[[12]]
  factTemp <-dataTemp[[13]]
  factIndTemp <- dataTemp[[14]]
  if(error != 0){
    stop(paste0('ERROR: generate_weights returned an exit error of: ',error))
  }
  
  #place weight arrays into proper arrays.
  factorList <- array(ndv,c(fctLen))
  factorIndexList <- array(ndv,c(2,fctLen))
  factorList[1:fctLen] <- factTemp[1:fctLen]
  factorIndexList[1,] <- factIndTemp[1,1:fctLen]
  factorIndexList[2,] <- factIndTemp[2,1:fctLen]
  
  #Dimension definition
  ncdim1 <- ncdf4::ncdim_def(name='src_west_east',units='',vals=1:nxIn,
                   create_dimvar=FALSE)
  ncdim2 <- ncdf4::ncdim_def(name='src_south_north',units='',vals=1:nyIn,
                   create_dimvar=FALSE)
  ncdim3 <- ncdf4::ncdim_def(name='dst_west_east',units='',vals=1:nxGeo,
                   create_dimvar=FALSE)
  ncdim4 <- ncdf4::ncdim_def(name='dst_south_north',units='',vals=1:nyGeo,
                   create_dimvar=FALSE)
  ncdim5 <- ncdf4::ncdim_def(name='factor_dim_1',units='',vals=1:fctLen,
                   create_dimvar=FALSE)
  ncdim6 <- ncdf4::ncdim_def(name='factor_dim_2',units='',vals=1:2,
                   create_dimvar=FALSE)
  
  
  #Define NetCDF variables
  varid1 <- ncdf4::ncvar_def('latSrc','Degrees',list(ncdim1,ncdim2),
                   ndv,prec="float")
  varid2 <- ncdf4::ncvar_def('lonSrc','Degrees',list(ncdim1,ncdim2),
                   ndv,prec="float")
  varid3 <- ncdf4::ncvar_def('latDst','Degrees',list(ncdim3,ncdim4),
                   ndv,prec="float")
  varid4 <- ncdf4::ncvar_def('lonDst','Degrees',list(ncdim3,ncdim4),
                   ndv,prec="float")
  varid5 <- ncdf4::ncvar_def('factorList','-',ncdim5,ndv,prec="float")
  varid6 <- ncdf4::ncvar_def('factorIndexList','-',list(ncdim6,ncdim5),
                   -9999,prec="float")
  
  defVarList <- list(varid1,varid2,varid3,varid4,varid5,varid6)
  
  #Open NetCDF file for writing
  ncid <- ncdf4::nc_create(wghtFile, defVarList, force_v4=TRUE)
  
  #Global attributes
  ncdf4::ncatt_put(ncid, 0, 'Title', 
                 'Regrid Weights for ESMF Regridding in rwrfhydro',
                 prec='text')
  ncdf4::ncatt_put(ncid, 0, 'source_missing_value', ndv, prec='double')
  
  #Variable attributes
  ncdf4::ncatt_put(ncid, varid1, 
                 'description', 'Source Center-Stagger Latitude',
                 prec='text')
  ncdf4::ncatt_put(ncid, varid1, 'stagger','Center',prec='text')
  ncdf4::ncatt_put(ncid, varid2, 
                 'description', 'Source Center-Stagger Longitude',
                 prec='text')
  ncdf4::ncatt_put(ncid, varid2, 'stagger','Center',prec='text')
  ncdf4::ncatt_put(ncid, varid3, 
                 'description', 'Destination Center-Stagger Latitude',
                 prec='text')
  ncdf4::ncatt_put(ncid, varid3, 'stagger','Center',prec='text')
  ncdf4::ncatt_put(ncid, varid4, 
                 'description', 'Destination Center-Stagger Longitude',
                 prec='text')
  ncdf4::ncatt_put(ncid, varid4, 'stagger','Center',prec='text')
  ncdf4::ncatt_put(ncid, varid5, 
                 'description', 'FactorList weight array for ESMF',
                 prec='text')
  ncdf4::ncatt_put(ncid, varid6, 
                 'description', 'FactorIndexList weight array for ESMF',
                 prec='text')
  
  #Put data into variables
  ncdf4::ncvar_put(ncid,varid1,latSrc)
  ncdf4::ncvar_put(ncid,varid2,lonSrc)
  ncdf4::ncvar_put(ncid,varid3,latGeo)
  ncdf4::ncvar_put(ncid,varid4,lonGeo)
  ncdf4::ncvar_put(ncid,varid5,factorList)
  ncdf4::ncvar_put(ncid,varid6,factorIndexList)
  
  #Close NetCDF file
  ncdf4::nc_close(ncid)
  
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
  ncId <- ncdf4::nc_open(wghtFile)
  
  #Read in lat data and double check dimensions make sense
  latSrc <- ncdf4::ncvar_get(ncId,'latSrc')
  latDst <- ncdf4::ncvar_get(ncId,'latDst')
  
  if((dim(latSrc)[1] != nxSrc) | (dim(latSrc)[2] != nySrc)){
    stop('ERROR: Row/Column mistmatch between source data and weight file.')
  }
  if((dim(latDst)[1] != nxDst) | (dim(latDst)[2] != nyDst)){
    stop('ERROR: Row/Column mismatch between destination data and weight file.')
  }
  
  #Pull regrid weight arrays
  factorList <- ncdf4::ncvar_get(ncId,'factorList')
  factorIndexList <- ncdf4::ncvar_get(ncId,'factorIndexList')
  fctLen <- dim(factorList)[1]
  
  #Close weight file
  ncdf4::nc_close(ncId)
  
  #Create output list to hold data
  outList <- list("factorList" = factorList, "factorIndexList" = factorIndexList,
                  "fctLen" = fctLen)
   
  return(outList)
  
}