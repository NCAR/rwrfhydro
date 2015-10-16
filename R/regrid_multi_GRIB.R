#' Extraction and regridding of GRIB data for multiple files for a given variable.
#' 
#' \code{RegridGRIB}: For a list of GRIB files, extract and regrid a variable
#' of a given vertical level to a WRF-Hydro domain.
#' 
#' @param files The files vector list
#' @param var The GRIB variable being regridded
#' @param levType Level type to extract GRIB data. Examples of levType would
#' be "surface", "cloudTop", "heightAboveGround"
#' @param lev The vertical level being regridded
#' @param geoFile The Geogrid LSM file needed for regridding.
#' @param method Method of regridding. Current acceptable values are 
#' "bilinear"
#' @return A dataframe containing the regridded stack.
#' @keywords internal
#' @concept dataRegrid
#' @family regridMultiGRIB
#' @export
RegridGRIB <- function(files,var,levType,lev,geoFile,method){
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
  #Calculate number of file steps
  nSteps <- length(files)
  
  #Extract metadata from the first GRIB file in the list. This information
  #will be used to establish input arrays into the regridding call.
  fileInit <- files[1]
  if(!file.exists(fileInit)){
    stop(paste0('ERROR: ',fileInit,' not found.'))
  }
  projection <- GRIBgridType(fileInit)
  geospatialDf <- GRIBgeospatial(projection,fileInit)
  
  #Extract number of 'forecast times per variable. For most cases, this
  #will be 1.
  numFTimes <- GRIBNumForecastTimes(fileInit,var,levType,lev)
  
  latLonGRIB <- GRIBLatLon(fileInit,geospatialDf$NX,geospatialDf$NY)
  latGRIB <- latLonGRIB[,,1]
  lonGRIB <- latLonGRIB[,,2]
  
  #Flip data if south-north flag is not 1. Data is read north-south
  if(geospatialDf$SNFLAG == 0){
    latGRIB <- flipLR(latGRIB)
    lonGRIB <- flipLR(lonGRIB)
  }
  
  #Establish input data stack
  dataIn <- array(-9999.0,c(geospatialDf$NX,geospatialDf$NY,numFTimes,nSteps))
  
  #Establish date arrays
  cycleOut <- data.frame(NA,matrix(nrow=numFTimes,ncol=nSteps))
  verOut <- data.frame(NA,matrix(nrow=numFTimes,ncol=nSteps))
  bVerOut <- data.frame(NA,matrix(nrow=numFTimes,ncol=nSteps))
  
  #Loop through files and extract GRIB data for parent GRIB domain
  for(step in 1:length(files)){
    cycleOut[,step] <- as.POSIXct('1900-01-01')
    verOut[,step] <- as.POSIXct('1900-01-01')
    bVerOut[,step] <- as.POSIXct('1900-01-01')
    
    file <- files[step]
    dataTemp <- extractGRIBGrid(file,var,levType,lev,geospatialDf$NX,
                                geospatialDf$NY,numFTimes=numFTimes)
    #Flip if read in north-south
    for(fTime in 1:numFTimes){
      dataGrid <- as.matrix(dataTemp$data[,,fTime])
      if(geospatialDf$SNFLAG == 0){
        dataGrid <- flipLR(dataGrid)
      }
      dataIn[,,fTime,step] <- dataGrid
      cycleOut[fTime,step] <- dataTemp$cycle[fTime]
      verOut[fTime,step] <- dataTemp$verTime[fTime]
      bVerOut[fTime,step] <- dataTemp$begVerTime[fTime]
    }
    #Establish metadata from 1st time step
    if(step == 1){
      longName <- dataTemp$longName
      units <- dataTemp$units
      ndv <- dataTemp$ndv
    }
    #Place date/time information into output arrays
  }
  #Regrid data stack
  dataOut <- regrid(dataIn,latGRIB,lonGRIB,geoFile,methodInt,ndv)
  
  nxOut <- dim(dataOut)[1]
  nyOut <- dim(dataOut)[2]
  outList <- list(name=var,
                  longname=longName,
                  units=units,
                  precision = 'double',
                  missing=ndv,
                  dimensionList = 
                  list(
                          x=list(name='west_east',values=1:nxOut),
                          y=list(name='south_north',values=1:nyOut),
                          t=list(name='time',values=1:numFTimes),
                          f=list(name='files',values=1:length(files))),
                  data=dataOut,
                  cyclePOSIXct=cycleOut,
                  verifPOSIXct=verOut,
                  begVerifPOSIXct=bVerOut,
                  nativeFiles=files)
  
  outList
  
}

#' The RegridMultiGRIB for getting variables out of individual files.
#' 
#' \code{RegridMultiGRIBVar}: For a group of GRIB files, extract
#' and regrid a list of variables (for a specified level) to a WRF-Hydro domain.
#' 
#' @param varInd The variable index.
#' @param levTypeList The list of level types for each variable.
#' @param levList The vertical level list.
#' @param varList The variable list.
#' @param files The files vector.
#' @param geoFile The Geogrid LSM file for regridding.
#' @param method Method of regridding. Current acceptable values are 
#' "bilinear"
#' @return A dataframe
#' @keywords internal
#' @keywords dataRegrid
#' @family regridMultiGRIB
#' @export 
RegridMultiGRIBVar <- function(varInd, varList, levTypeList, levList, files,
                               geoFile,method){
  #Check for existence of GeoFile
  if(!file.exists(geoFile)){
    stop(paste0('ERROR: ',geoFile,' not found.'))
  }
  #Length of levList and varList should be the same. A vertical level for
  #each variable in the GRIB file MUST be identified.
  if(length(varList) != length(levList)){
    stop("Length of the variable and levList are not equal.")
  }
  if(length(varList) != length(levTypeList)){
    stop("Length of the variable and levTypeList are not equal.")
  }
 
  #outList <- plyr::llply(ind, RegridGRIB,
  outList <-  RegridGRIB(var = varList[[varInd]],
                         levType = levTypeList[[varInd]],
                         lev = levList[[varInd]],
                         geoFile = geoFile,
                         files = files,
                         method=method)
  outList
}





#' The RegridMultiGRIB for individual file groups.
#' 
#' \code{RegridMultiGRIBFile} : For this file group, regrid 
#' Regrid GRIB data spread over multiple files. Typically called
#' by RegridMultiGRIB.
#' 
#' @param fileInd The index of the file group.
#' @param fileList The list of the file group.
#' @param varList The variable list.
#' @param levTypeList The level type list.
#' @param levList The vertical level list.
#' @param geoFile The geofile needed for regridding.
#' @param method Method of regridding. Current acceptable values are 
#' "bilinear".
#' @return A dataframe
#' @keywords internal
#' @keywords dataRegrid
#' @keywords regridMultiGRIB
#' @export
RegridMultiGRIBFile <- function(fileInd, fileList, varList, levTypeList,
                                levList,geoFile,method){
  #Check for existence of GeoFile
  if(!file.exists(geoFile)){
    stop(paste0('ERROR: ',geoFile,' not found.'))
  }
  #Enforce collation at the variable-level level: (for this file group)
  #each variable has a co-located level index.
  if (length(varList) != length(levList)){
    stop(paste0("for file set ",names(fileList)[fileInd]," (#",fileInd,") ",
                "The variable and level lists must be collated: their 
                lengths do not match."))
  }
  if (length(levTypeList) != length(varList)){
    stop(paste0("for file set ",names(fileList)[fileInd]," (#",fileInd,") ",
                "The variable and level type lists must be collated: their
                lengths do not match."))
  }
  if (all(names(varList) != names(levList))){
    stop(paste0("For file set ",names(fileList)[fileInd]," (#",fileInd,") ",
                "The variable and level lists must be collated: their 
                names do not match."))
  }
  if (all(names(levTypeList) != names(varList))){
    stope(paste0("For file set ",names(fileList)[fileInd]," (#",fileInd,") ",
                 "The variable and level type lists must be collated: their
                 names do not match."))
  }
  varInd <- 1:length(varList[[fileInd]])
  
  outList <- plyr::llply(varInd, RegridMultiGRIBVar,
                         varList = varList[[fileInd]],
                         levTypeList = levTypeList[[fileInd]],
                         levList = levList[[fileInd]],
                         files = fileList[[fileInd]],
                         geoFile = geoFile,
                         method=method)
  
  outList
} 



#' Regrid multiple variables across various GRIB file groups to the
#' WRF-Hydro domain.
#' 
#' \code{RegridMultiGRIB} is designed to regrid multiple variables across
#' multiple GRIB files. Four collated lists specify 1) file groups,
#' 2) variables for each file group, 3) Level types for each variable group,
#' and 4) levels for each variable group. 
#' The names of the lists must match. 
#' In addition, a Geo LSM file must be presented for regridding to the
#' WRF-Hydro domain. Also, a regridding method must be provided to specify
#' how ESMF will regrid the data. Valid options currently include "bilinear".
#' See examples for details.
#' 
#' @param fileList The list of file groups. Names must match those in 
#'   the other lists.
#' @param varList The list of GRIB variables for each file group.
#'   Names must match fileList.
#' @param levTypeList The list of level types for each variable.
#' @param levList The list of GRIB levels for each variable group.
#'   Names must match fileList.
#' @param geoFile The path to the Geo LSM file used for regridding.
#' @param method Method of regridding. Current acceptable values are 
#' "bilinear".
#' @return A dataframe containing regridded data along with meta-data.
#' 
#' @examples
#' # This example shows extracting temperature, pressure, and downward
#' # shortwave radiation data at the surface from High Resolution Rapid
#' # Refresh GRIB data files.
#' \dontrun{
#' hrrrPath <- '/d4/karsten/data/HRRR/20151001'
#' GRIBFiles <- list.files(path=hrrrPath,pattern='hrrr.t00z.wrfnatf',
#'                         full.names=TRUE)
#' #fileList <- These are the groups of files.
#' fileList <- list(GRIBList1 = GRIBFiles )
#' #varList <- Define which variables are desired for each file group.
#' GRIBVars <- list(TEMPERATURE='2t',PRES='pres',DSWRF='dwsrf')
#' varList <- list(GRIBList1 = GRIBVars )
#' #levelTypeList <- Define type of vertical levels for each variables.
#' GRIBLevTypes <- list(levType='heightAboveGround,levType='surface',
#'                      levType='surface')
#' levTypeList <- list(GRIBList1 = GRIBLevTypes)
#' #levelList <- Define which vertical levels for each variable.
#' GRIBLevels <- list(level=0, level=0, level=0)
#' levelList <- list(GRIBList1 = GRIBLevels )
#' geoFile <- '/d4/karsten/geospatial/geo_em.d01.nc'
#' regridData <- regridMultiGRIB(fileList=fileList,varList=variableList,
#'                               levList=levelList,geoFile=geoFile,'bilinear')
#'                               
#' }
#' @export
RegridMultiGRIB <- function(fileList,varList,levTypeList,levList,
                            geoFile,method){
  #Check for existence of GeoFile
  if(!file.exists(geoFile)){
    stop(paste0('ERROR: ',geoFile,' not found.'))
  }
  ##Only do collated lists. Collation check at the file-variable-level level.
  if (length(varList) != length(levList) |
      length(varList) != length(fileList) |
      length(levList) != length(fileList) |
      length(levTypeList) != length(fileList)){
    stop("The input lists must be collated: their lengths do not match.")
  }
  if (all(names(varList) != names(fileList)) |
      all(names(varList) != names(levList)) |
      all(names(levList) != names(fileList)) |
      all(names(levTypeList) != names(fileList))){
    stop("The input lists must be collated: their names do not match.")
  }
  fileInd <- 1:length(fileList)
  
  outList <- plyr::llply(fileInd,RegridMultiGRIBFile,
                         varList=varList,
                         levTypeList=levTypeList,
                         levList=levList,
                         fileList=fileList,
                         geoFile=geoFile,
                         method=method)
  outList
}
