#' High level call to regrid data to WRF-Hydro domain.
#' 
#' \code{Regrid} High level call to regrid data.
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
#' dataOut <- Regrid(hrrrDSWRF,latIn,lonIn,'geo_em.d01.nc',1,'./wghts_hrrr.Rdata',
#'                   9999)
#' }
#'
#' @keywords io geospatial regrid ESMF
#' @concept regrid geospatial  
#' @family regrid
#' @useDynLib rwrfhydro
#' @export
Regrid <- function(dataIn,latIn,lonIn,geoFile,method,wghtFile,ndvSrc){
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
  weightList <- ReadWghtFile(wghtFile,nxIn,nyIn,nxGeo,nyGeo)
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
    rm(dataIn)
  }
  
  return(dataOut)

}

#' High level call to generate regridding weight NetCDF file.
#' 
#' \code{GenWghtFile} High level call to generate regridding 
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
#' GenWghtFile('./geo_em.d02.nc',nxIn,nyIn,latGRIB,lonGRIB,1,dataIn[,,],
#' 9999,'./wght_hrrr_rio_grande.nc')
#' }
#' 
#' @keywords regrid ESMF geospatial io
#' @concept regrid geospatial
#' @family regrid
#' @useDynLib rwrfhydro
#' @export
GenWghtFile <- function(geoFile,nxIn,nyIn,latSrc,lonSrc,method,srcDummy,
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
#' \code{ReadWghtFile} Open weight file and extract important weight arrays.
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
ReadWghtFile <- function(wghtFile,nxSrc,nySrc,nxDst,nyDst){
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

#' Generate a UGRID NetCDF file necessary for regridding to unstructured meshes. 
#' Format of the file follows the UGRID convention.
#' 
#' \code{GenUgridFile} Generate UGRID NetCDF file based on polygon shapefile. 
#' 
#' @param shpPath Path to the polygon shapefile.
#' @param uGridPath Path to UGRID file to be created
#' 
#' @examples 
#' \dontrun{
#' GenUgridFile('/d4/karsten/geospatial/nhd.shp','/d4/karsten/geospatial/nhd_UGRID.nc')
#' }
#' @keywords regrid, geospatial, ESMF, UGRID
#' @concept regrid, ESMF
#' @family regrid, ESMF
#' @export
GenUgridFile <- function(shpPath,uGridPath){
  ndv <- -9999
  # Check to make sure file exists
  if(!file.exists(shpPath)){
    stop(paste0('ERROR: Shape file: ',shpPath,' not found.'))
  }
  
  # Parse shapefile path for reading purposes
  split1 <- strsplit(shpPath,'/')
  print(split1)
  shpFile <- split1[[1]][length(split1[[1]])]
  print(shpFile)
  shpFile <- strsplit(shpFile,'\\.')[[1]][1]
  print(shpFile)
  pathIn <- '/'
  for (i in 1:(length(split1[[1]])-1)){
    pathIn <- paste0(pathIn,split1[[1]][i],'/')
    print(pathIn)
  }
  # Read in shapefile
  shpIn <- rgdal::readOGR(pathIn,shpFile)
  numPoly <- length(shpIn@polygons)
  
  numVert <- 0
  nodeSum <- 0
  # Loop through and calculate the maximum number of vertices associated with the polygons
  # Also sum up the total number of vertices associated with the shapefiles
  for (i in 1:numPoly){
    coordTmp <- shpIn@polygons[[i]]@Polygons[[1]]@coords
    check <- dim(coordTmp)[1]
    if (check > numVert){
      numVert = check
    }
    nodeSum <- nodeSum + check
  }
  
  # Establish arrays to hold output data
  latOut <- array(ndv,dim=c(nodeSum))
  lonOut <- array(ndv,dim=c(nodeSum))
  conOut <- array(ndv,dim=c(numVert,numPoly))
  
  print(nodeSum)
  print(numPoly)
  print(numVert)
  # Loop through and place lat/lon values for each vertex into
  # output array
  count <- 1
  for (i in 1:numPoly){
    coordTmp <- shpIn@polygons[[i]]@Polygons[[1]]@coords
    lenTmp <- dim(coordTmp)[1]
    for (j in 1:lenTmp){
      latOut[count] <- coordTmp[j,2]
      lonOut[count] <- coordTmp[j,1]
      conOut[j,i] <- count
      count <- count + 1
    }
  }
  
  # Create output NetCDF file
  dim1 <- ncdf4::ncdim_def('node','',1:nodeSum,unlim=FALSE,create_dimvar=FALSE )
  dim2 <- ncdf4::ncdim_def('nele','',1:numPoly,unlim=FALSE,create_dimvar=FALSE )
  dim3 <- ncdf4::ncdim_def('max_node','',1:numVert,unlim=FALSE,create_dimvar=FALSE )
  
  var1 <- ncdf4::ncvar_def('fvcom_mesh','',list(),NULL,prec='integer')
  var2 <- ncdf4::ncvar_def('nv','',list(dim3,dim2),ndv,prec='integer')
  var3 <- ncdf4::ncvar_def('lon','degrees_east',list(dim1),ndv,prec='float')
  var4 <- ncdf4::ncvar_def('lat','degrees_north',list(dim1),ndv,prec='float')
  
  id <- ncdf4::nc_create(uGridPath,list(var1,var2,var3,var4))
  
  ncdf4::ncatt_put(id,var1,'standard_name','mesh_topology')
  ncdf4::ncatt_put(id,var1,'dimension',2)
  ncdf4::ncatt_put(id,var1,'node_coordinates','lon lat')
  ncdf4::ncatt_put(id,var1,'face_node_connectivity','nv')
  
  ncdf4::ncatt_put(id,var2,'standard_name','face_node_connectivity')
  ncdf4::ncatt_put(id,var2,'start_index',1)
  
  ncdf4::ncatt_put(id,var3,'standard_name','longitude')
  ncdf4::ncatt_put(id,var4,'standard_name','latitude')
  
  ncdf4::ncvar_put(id,var2,conOut)
  ncdf4::ncvar_put(id,var3,lonOut)
  ncdf4::ncvar_put(id,var4,latOut)
  
  ncdf4::nc_close(id)
}

#' High level call to generate regridding weight file.
#' 
#' \code{GenWghtFileMesh} High level call to generate a regridding
#' weight file for going from cartesian to an unstructured mesh.
#' 
#' @param geoFile geogrid file of WRF-Hydro domain.
#' @param uGridFile UGRID file representing unstructured mesh grid.
#' @param nxIn Integer number of columns of source data.
#' @param nyIn Integer number of rows of source data.
#' @param latSrc Grid of center-stagger latitude values for source data.
#' @param lonSrc Grid of center-stagger longitude values for source data.
#' @param method Integer of regridding method. Options are 2 - "conserve",
#' 1 - "bilinear", 3 - "neighbor"
#' @param srcDummy Array of sample source data to be used in generation of
#' weights. This will be used to generate the mask. 
#' @param ndv Source NDV value used in masking.
#' @param wghtFile Output weight file to be generated.
#' @examples 
#' \dontrun{
#' GenWghtFileMesh('./geo_em.d02.nc','./UGRID_NHD.nc',nxIn,nyIn,latGRIB,
#' lonGRIB,1,dataIn[,,],9999,'./wght_hrrr_NHD.Rdata')
#' }
#' 
#' @keywords regrid ESMF geospatial io
#' @concept regrid geospatial
#' @family regrid
#' @useDynLib rwrfhydro
#' @export
GenWghtFileMesh <- function(geoFile,uGridFile,nxIn,nyIn,latSrc,lonSrc,method,
                            srcDummy,ndv,wghtFile){
  
  #Check for the existence of geogrid file
  if(!file.exists(geoFile)){
    stop(paste0('ERROR: Geogrid file: ',geoFile,' not found.'))
  }
  #Check for the existence of UGRID file
  if(!file.exists(uGridFile)){
    stop(paste0('ERROR: UGRID file: ',uGridFile,' not found.'))
  }
  
  uLen <- nchar(uGridFile)
  
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
  
  #Open UGRID file and pull dimensions of polygons. This
  #is used to better determine allocating a sufficient length
  #for the weight arrays
  ncid <- ncdf4::nc_open(uGridFile)
  nvVar <- ncdf4::ncvar_get(ncid,'nv')
  print(dim(nvVar))
  numPoly <- dim(nvVar)[2]
  ncdf4::nc_close(ncid)
  
  #Establish variables for generation of weights.
  nxIn <- as.integer(nxIn)
  nyIn <- as.integer(nyIn)
  method <- as.integer(method)
  ndv <- as.numeric(ndv)
  fctLen <- as.integer(0)
  lTest1 <- nxIn*nyIn*5
  lTest2 <- numPoly*5
  print(lTest1)
  print(lTest2)
  if(lTest1 > lTest2){
    fctLenTemp <- as.integer(lTest1)
  } else {
    fctLenTemp <- as.integer(lTest2)
  }
  
  factIndTemp <- array(-9999,c(2,fctLenTemp)) #-9999 becase a positive 9999
  #is a possible index value.
  factTemp <- array(ndv,c(fctLenTemp))
  mskArray <- array(ndv,c(nxIn,nyIn))
  error <- as.integer(0)
  
  print(nxIn)
  print(nyIn)
  print(fctLenTemp)
  print(method)
  print(ndv)
  print(uLen)
  print(uGridFile)
  dataTemp <- .Fortran('generate_weights_mesh',nxIn,nyIn,fctLenTemp,
                       srcDummy,latSrc,lonSrc,method,
                       ndv,uLen,uGridFile,fctLen,factTemp,factIndTemp,
                       error)
  
  error <- dataTemp[[14]]
  if(error == -99){
    print('ERROR: Rwrfhydro built without regridding support.')
    warning('ERROR: Please rebuild package with ESMF and GRIB API libraries.')
  } else if(error == 49){ # This error means ESMF was not built with NetCDF, which is necessary
                          # to read in the UGRID file during the mesh creation in ESMF
    stop(paste0('ERROR: ESMF was not built with NetCDF. This is necessary for mesh creation.'))
  } else if(error != 0){
    stop(paste0('ERROR: generate_weights returned an exit status of: ',error))
  }
  fctLen <- dataTemp[[11]]
  factTemp <- dataTemp[[12]]
  factIndTemp <- dataTemp[[15]]
  
  #Place weight arrays into proper arrays.
  factorList <- array(ndv,c(fctLen))
  factorIndexList <- array(ndv,c(2,fctLen))
  factorList[1:fctLen] <- factTemp[1:fctLen]
  factorIndexList[1,] <- factIndTemp[1,1:fctLen]
  factorIndexList[2,] <- factIndTemp[2,1:fctLen]
  
  #Save weight arrays and lat/lon infor as Rdata file. Originally was
  #NetCDF, but discovered a bug in the code writing large arrays.
  save(latSrc,lonSrc,latGeo,lonGeo,factorList,factorIndexList,file=wghtFile)

}