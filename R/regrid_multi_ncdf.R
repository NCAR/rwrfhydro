#' Extraction and regridding of NetCDF data for multiple files for a 
#' given variable.
#'
#' \code{regridNcdf}: For a list of NetCDF files, extract and regrid
#' a variable WRF-Hydro domain.
#'
#' @param var The GRIB variable being regridded
#' @param files The files vector list
#' @param latVar The latitude variable from the source files.
#' @param lonVar The longitude variable from the source files.
#' @param wghtFile The weight regridding file. If it does not exist,
#' it will be created.
#' @param geoFile The Geogrid LSM file needed for regridding. 
#' @param method Method of regridding. Current acceptable values are
#' "bilinear"
#' @return A list containing the regridded stack.
#' @keywords internal
#' @concept dataRegrid
#' @family regridMultiNcdf
#' @export 
regridNcdf <- function(var,files,latVar,lonVar,wghtFile,geoFile,method){
  #Check for existence of GeoFile
  if(!file.exists(geoFile)){
    stop(paste0('ERROR: ',geoFile,' not found.'))
  }

  #Check for a valid method
  if(method != "bilinear"){
    stop(paste0(method," regridding is not currently supported in this function."))
  }
  
  #Cast integer value for regridding method. This value will be used in the 
  #Fortran shared object to determine proper ESMF calls.
  if(method == "bilinear"){
    methodInt <- 1
  }
  #Calculate number of time steps
  nSteps <- length(files)

  #Extract lat/lon value from first file in list. This function 
  #is assuming all files are on the same grid. If they aren't
  #please split files up by grid type.

  fileLL <- files[1]
  #Open file
  ncIdLL <- ncdf4::nc_open(fileLL)
  varLLList <- names(ncIdLL$var)

  #Get lat/lon variables
  latVarTmp <- ncdf4::ncvar_get(ncIdLL,latVar)
  lonVarTmp <- ncdf4::ncvar_get(ncIdLL,lonVar)
  
  #Get number of time steps from variable of interest.
  if(var %in% varLLList){
    varTemp <- ncdf4::ncvar_get(ncIdLL,var)
    if(length(dim(varTemp)) == 2){
      numFTimes <- 1
    }
    if(length(dim(varTemp)) > 3){
      stop(paste0('ERROR: NetCDF variable has ',length(dim(varTemp)),'. Only
                  dimension lenghts of 2 or 3 supported within each file.'))
    }
    if(length(dim(varTemp)) == 3){
      numFTimes <- dim(varTemp)[3]
    }
    #Extract some meta-data about the variable
    attDf <- ncdf4::ncatt_get(ncIdLL,var)
    if("units" %in% names(attDf)){
      units <- attDf$units
    }
    if("long_name" %in% names(attDf)){
      longName <- attDf$long_name
    } else {
      longName <- ""
    }
    if("longName" %in% names(attDf)){
      longName <- attDf$longName
    }
    if("_FillValue" %in% names(attDf)){
      ndvSrc <- ncdf4::ncatt_get(ncIdLL,var,'_FillValue')$value
    } else {
      stop('ERROR: _FillValue not found.')
    }
  } else {
    stop('ERROR: ',var,' not found in NetCDF file.')
  }   

  #Close NetCDF file
  ncdf4::nc_close(ncIdLL)

  #Determine lat/lon grid information. Function assumes data is read in as
  #(col/row) major. This implies that grid is conforming to normal standards
  #If it's not, this function will return an error.
  #Function also assumes data is read south to north and west to east
  llDims <- length(dim(latVarTmp))

  if(llDims == 2){
    latArray <- latVarTmp
    lonArray <- lonVarTmp
    nRowIn <- length(latVarTmp[1,])
    nColIn <- length(lonVarTmp[,1])
  }
  if(llDims == 1){
    latArrayTemp <- latVarTmp
    lonArrayTemp <- lonVarTmp
    nRowIn <- length(latVarTmp)
    nColIn <- length(lonVarTmp)
    #Cast to 2D arrays for regridding purposes
    latArray <- array(ndvSrc,c(nColIn,nRowIn))
    lonArray <- array(ndvSrc,c(nColIn,nRowIn))
    for(r in 1:nRowIn){
      lonArray[,r] <- lonArrayTemp
    }
    for(c in 1:nColIn){
      latArray[c,] <- latArrayTemp
    }
  }
 
  #Sanity check
  if((latArray[1,1] == latArray[1,2]) | (lonArray[1,1] == lonArray[2,1])){
    stop('ERROR: Lat/lon array is uniform. Wrong latitude values provided.')
  }
  if((nRowIn == 0) | (nColIn == 0)){
    stop('ERROR: Lat or Lon array has been returned as 0 length.')
  }
  if(latArray[nColIn,nRowIn] < latArray[1,1]){
    snFlag <- 0
  } else {
    snFlag <- 1
  }
  if(lonArray[nColIn,nRowIn] < lonArray[1,1]){
    stop('ERROR: Grid is east to west. Please reverse order.')
  }

  #Flip data if south-north flag is not 1. Data is read north-south
  if(snFlag == 0){
    latArray <- flipLR(latArray)
    lonArray <- flipLR(lonArray)
  }
  #Establish input data stack
  dataIn <- array(ndvSrc,c(nColIn,nRowIn,numFTimes,nSteps))

  #Loop through NetCDF files, extract data for given variable
  for(step in 1:length(files)){
    file <- files[step]

    #Open file
    ncId <- ncdf4::nc_open(file)
    varListTemp <- names(ncId$var)
    
    if(var %in% varListTemp){ 
      dataTemp <- ncdf4::ncvar_get(ncId,var)
      #Flip if read in north-south
      for(fTime in 1:numFTimes){
        if(numFTimes == 1){
          dataGrid <- as.matrix(dataTemp[,])
        } else {
          dataGrid <- as.matrix(dataTemp[,,fTime])
        }
        if(snFlag == 0){
          dataGrid <- flipLR(dataGrid)
        }
        #Check to see if valid data was read in.
        if(length(which(dataGrid != ndvSrc)) == 0){
          stop(paste0('ERROR: NetCDF variable ',var,' returned no valid values.'))
        }
        dataIn[,,fTime,step] <- dataGrid
      }
    } else {
      stop(paste0('ERROR: variable ',var,' not found in ',file))
    }
  }

  #Set NA values to ndvSrc as they are not read in by the NetCDF
  #library, but set to NA This is needed for the regridding routine.
  dataIn[which(is.na(dataIn))] <- ndvSrc

  #Check for the existence of weight file. If it's not present, create one
  #using ESMF.
  if(!file.exists(wghtFile)){
    #Note we are passing the first slice of NetCDF data. This is used 
    #in conjunction with NDV information to generate a mask during the
    #weight generation.
    srcDummy <- array(ndvSrc,c(nColIn,nRowIn))
    srcDummy[,] <- dataIn[,,1,1]
  
    genWghtFile(geoFile,nColIn,nRowIn,latArray,lonArray,methodInt,srcDummy,
                ndvSrc,wghtFile)

    #Double check to make sure file was created.
    if(!file.exists(wghtFile)){
      stop(paste0('ERROR: genWghtFile failed to create: ',wghtFile))
    }
  }

  #Regrid data stack
  dataOut <- regrid(dataIn,latArray,lonArray,geoFile,methodInt,wghtFile,ndvSrc)

  nxOut <- dim(dataOut)[1]
  nyOut <- dim(dataOut)[2]
  outList <- list(name=var,
                  longname=longName,
                  units=units,
                  precision = 'double',
                  missing=ndvSrc,
                  dimensionList =
                  list(
                        x=list(name='west-east',values=1:nxOut),
                        y=list(name='south-north',values=1:nyOut),
                        t=list(name='time',values=1:numFTimes),
                        f=list(name='files',values=1:length(files))),
                  data=dataOut,
                  nativeFiles=files)

  outList

}

  
#' The regridMultiNcdf for getting variables out of individual files.
#' 
#' \code{regridMultiNcdfVar}: For a group of NetCDF files, extract
#' and regrid a list of variables to a WRF-Hydro domain.
#'
#' @param varInd The variable index.
#' @param varList The variable list.
#' @param files The files vector.
#' @param latVar The latitude variable
#' @param lonVar The longitude variable
#' @param wghtFile The weight file used for regridding.
#' @param geoFile The Geogrid LSM file for regridding.
#' @param method Method of regridding. Current acceptable values are
#' "bilinear"
#' @return A list
#' @keywords internal
#' @keywords dataRegrid
#' @family regridMultiNcdf
#' @export 
regridMultiNcdfVar <- function(varInd, varList, files, latVar, lonVar,
                               wghtFile,geoFile, method){

  #Check for existence of GeoFile
  if(!file.exists(geoFile)){
    stop(paste0('ERROR: ',geoFile,' not found.'))
  }

  outList <- regridNcdf(var = varList[[varInd]],
                        files = files,
                        latVar = latVar,
                        lonVar = lonVar,
                        wghtFile = wghtFile,
                        geoFile = geoFile,
                        method=method)


  outList
}


#' The regridMultiNcdf for individual file groups.
#'
#' \code{regridMultiNcdfFile} : For this file group, regrid
#' NetCDF data spread over multiple files. Typically called by 
#' regridMultiNcdf.
#' 
#' @param fileInd The index of the file group.
#' @param fileList The list of the file group.
#' @param varList The variable list.
#' @param latList The latitude variable list.
#' @param lonList The longitude variable list.
#' @param wghtList The weight file list.
#' @param geoFile The geofile needed for regridding.
#' @param method Method of regridding. Current acceptable values are 
#' 'bilinear'.
#' @return A list
#' @keywords internal
#' @keywords dataRegrid
#' @keywords regridMultiNcdf
#' @export
regridMultiNcdfFile <- function(fileInd,fileList,varList,latList,
                                lonList,wghtList,geoFile,method){
  #Check for existence of GeoFile
  if(!file.exists(geoFile)){
    stop(paste0('ERROR: ',geoFile,' not found.'))
  }
  #Enforce collation at the variable-level level: (for this file group)
  #each variable has a co-located level index.
  if (length(varList) != length(fileList) |
      length(fileList) != length(wghtList) | 
      length(fileList) != length(latList) |
      length(fileList) != length(lonList)){
    stop(paste0("for file set ",names(fileList)[fileInd]," (#",fileInd,") ",
                "The variable and file lists must be collated: their
                lengths do not match."))
  }
  if (all(names(varList) != names(fileList)) |
      all(names(fileList) != names(wghtList)) |
      all(names(fileList) != names(latList)) |
      all(names(fileList) != names(lonList))){
    stop(paste0("For file set ",names(fileList)[fileInd]," (#",fileInd,") ",
                "The variable and level lists must be collated: their
                names do not match."))
  }
  varInd <- 1:length(varList[[fileInd]])

  outList <- plyr::llply(varInd,regridMultiNcdfVar,
                         varList = varList[[fileInd]],
                         files = fileList[[fileInd]],
                         latVar = latList[[fileInd]][[1]],
                         lonVar = lonList[[fileInd]][[1]],
                         wghtFile = wghtList[[fileInd]][[1]],
                         geoFile = geoFile,
                         method = method)
 
  names(outList) <- names(varList[[fileInd]])
  outList
}


#' Regrid multiple variables across various NetCDF file groups to the 
#' WRF-Hydro domain.
#'
#' \code{RegridMultiNcdf} is designed to regrid multiple variables across
#' multiple NetCDF files. Two collated lists specify 1) file groups,
#' and 2) variables for each file group. The names of the lists must match.
#' In addition, a Geo LSM file must be presented for regridding to the WRF-Hydro
#' domain. NetCDF input files must have center lat/lon values for the entire
#' grid. Also, a regridding method must be provided to specify how ESMF will regrid
#' the data. Valid options currently include "bilinear". See examples for details.
#'
#' @param fileList The list of file groups. Names must match those in the
#'   other lists.
#' @param varList The list of NetCDF variables for each file group. 
#'   Names must match fileList.
#' @param latList The list of latitude variables for each file group.
#' @param lonList The list of longitude variables for each file group.
#' @param wghtList The list of weight files for each file group.
#' @param geoFile The path to the Geo LSM file used for regridding. 
#' @param method Method of regridding. Current acceptable values are
#'   "bilinear"
#' @return A list containing regridded data along with meta-data.
#'
#' @examples
#' ##This example shows opening multiple SNODAS NetCDF conus files containing
#' ##snow depth and SWE for regridding.
#' \dontrun{
#' snodasPath <- '/d4/karsten/data/SNODAS'
#' NCFiles <- list.files(path=snodasPath, pattern='SNODAS_CONUS_', full.names=TRUE)
#' #fileList <- these are the groups of files.
#' fileList <- list(SNODASList1 = NCFiles)
#' #varList <- Define which variables are desired for each file group.
#' NCVars <- list(SWE='SNEQV', DEPTH='SNOWH')
#' varList <- list(SNODASList1 = NCVars)
#' latVars <- list(lat='latitude')
#' latList <- list(SNODASList1 = latVars)
#' lonVars <- list(lon='longitude')
#' lonList <- list(SNODASList1 = lonVars)
#' wghtFiles <- list(wght1 = "./wght_snodas_ioc_1km.nc")
#' wghtList <- list(SNODASList1 = wghtFiles
#' geoFile <- '/d4/karsten/geospatial/geo_em.d01.nc'
#' regridData <- regridMultiNcdf(fileList=fileList,varList=varList,latList=latList,lonList=lonList,
#'               wghtList=wghtList,geoFile=geoFile,'bilinear')
#'
#' }
#' @export 
regridMultiNcdf <- function(fileList,varList,latList,lonList,wghtList,geoFile,method){
  #Check for existence of GeoFile
  if(!file.exists(geoFile)){
    stop(paste0('ERROR: ',geoFile,' not found.'))
  }
  ##Only do collated lists. Collation check at teh file-variable-level level.
  if (length(varList) != length(fileList) | 
      length(fileList) != length(wghtList) |
      length(fileList) != length(latList) |
      length(fileList) != length(lonList)){
    stop("The input lists must be collated: their lengths do not match.")
  }
  if (all(names(varList) != names(fileList)) |
      all(names(fileList) != names(wghtList)) |
      all(names(fileList) != names(latList)) |
      all(names(fileList) != names(lonList))){
    stop("The input lists must be collated: their names do not match.")
  }
  fileInd <- 1:length(fileList)
 
  outList <- plyr::llply(fileInd,regridMultiNcdfFile,
                         varList=varList,
                         fileList=fileList,
                         latList=latList,
                         lonList=lonList,
                         wghtList=wghtList,
                         geoFile=geoFile,
                         method=method)
  
  names(outList) <- names(fileList)
  outList
} 

