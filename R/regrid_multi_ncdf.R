##' Extraction and regridding of NetCDF data for multiple files for a 
##' given variable.
##'
##' \code{regridNcdf}: For a list of NetCDF files, extract and regrid
##' a variable of a given vertical level to a WRF-Hydro domain.
##'
##' @param files The files vector list
##' @param var The GRIB variable being regridded
##' @param geoFile The Geogrid LSM file needed for regridding. 
##' @param method Method of regridding. Current acceptable values are
##' "bilinear"
##' @return A list containing the regridded stack.
##' @keywords internal
##' @concept dataRegrid
##' @family regridMultiNcdf
##' @export 
#regridNcdf <- function(files,var,geoFile,method){
#  #Check for existence of GeoFile
#  if(!file.exists(geoFile)){
#    stop(paste0('ERROR: ',geoFile,' not found.'))
#  }

#  #Check for a valid method
#  if(method != "bilinear"){
#    methodInd <- 1
#  }
#  #Calculate number of time steps
#  nSteps <- length(files)

#  #Extract lat/lon value from first file in list. This function 
#  #is assuming all files are on the same grid. If they aren't
#  #please split files up by grid type.

#  fileLL <- files[1]
#  #Open file
#  ncIdLL <- ncdf4::nc_open(fileLL)
#  varLLList <- names(ncIDLL$var)
 
#  #Deal with all possibilities of lat/lon var names
#  if(("LAT" %in% varLLList) & ("LON" %in% varLLList)){
#    latVar <- ncdf4::ncvar_get(ncIdLL,"LAT")
#    lonVar <- ncdf4::ncvar_get(ncIdLL,"LON")
#  } else if(("lat" %in% varLLList) & ("lon" %in% varLLList)){
#    latVar <- ncdf4::ncvar_get(ncIdLL,"lat")
#    lonVar <- ncdf4::ncvar_get(ncIdLL,"lon")
#  } else if(("XLAT" %in% varLLList) & ("XLONG" %in% varLLList)){
#    latVar <- ncdf4::ncvar_get(ncIdLL,"XLAT")
#    lonVar <- ncdf4::ncvar_get(ncIdLL,"XLONG")
#  } else if(("XLAT_M" %in% varLLList) & ("XLONG_M" %in% varLLList)){
#    latVar <- ncdf4::ncvar_get(ncIdLL,"XLAT_M")
#    lonVar <- ncdf4::ncvar_get(ncIdLL,"XLONG_M")
#  } else if(("Lat" %in% varLLList) & ("Lon" %in% varLLList)){
#    latVar <- ncdf4::ncvar_get(ncIdLL,"Lat")
#    lonVar <- ncdf4::ncvar_get(ncIdLL,"Lon")
#  } else if(("Latitude" %in% varLLList) & ("Longitude" %in% varLLList)){
#    latVar <- ncdf4::ncvar_get(ncIdLL,"Latitude")
#    lonVar <- ncdf4::ncvar_get(ncIdLL,"Longitude")
#  } else {
#    warning('ERROR: No lat/lon variables found in NetCDF file')
#    warning('ERROR: Acceptable values are: LAT/LON, lat/lon, XLAT/XLONG,
#            XLAT_M/XLONG_M, Lat/Lon, Latitude/Longitude')
#    stop(0)
#   } 

#  #Close NetCDF file
#  ncdf4::nc_close(ncIdLL)

#  #Determine lat/lon grid information. Function assumes data is read in as
#  #(col/row) major. This implies that grid is conforming to normal standards
#  #If it's not, this function will return an error.
#  #Function also assumes data is read south to north and west to east
#  llDims <- length(dim(latVar))
#  if(llDims == 2){
#    latArray <- latVar[1,]
#    lonArray <- lonVar[,1]
#  } else {
#    latArray <- latVar
#    lonArray <- lonVar
#  }
#  nRowIn <- length(latArray)
#  nColIn <- length(lonArray)
#  #Sanity check
#  if((latArray[1] == latArray[2]) | (lonArray[1] == lonArray[2])){
#    stop('ERROR: Lat/lon array is uniform. Wrong latitude values provided.')
#  }
#  if((nRowIn == 0) | (nColIn == 0)){
#    stop('ERROR: Lat or Lon array has been returned as 0 length.')
#  }
#  if(latArray[nRowIn] < latArray[1]){
#    stop('ERROR: Grid is north to south. Please reverse order.')
#  }
#  if(lonArray[nColIn] < lonArray[1]){
#    stop('ERROR: Grid is east to west. Please reverse order.')
#  }

#  #Establish input data stack
#  dataIn <- array(-9999.0,c(nColIn,nRowIn,nSteps))

#  #Loop through NetCDF files, extract data for given variable
#  for(step in 1:length(files)){
#    file <- files[step]

#    #Open file
#    ncId <- ncdf4::nc_open(file)
#    varListTemp <- names(ncId$var)

#    dataIn[,,step] <- ncdf4::ncvar_get(ncId,var)
#    ndv <- -9999.0
#  }

#  #Regrid data stack
#  dataOut <- regrid(dataIn,latArray,lonArray,geoFile,methodInt,ndv)

#  nxOut <- dim(dataOut)[1]
#  nyOut <- dim(dataOut)[2]
#  outList <- list(name=var,
#                  longname='test',
#                  units='-',
#                  precision = 'double',
#                  missing=ndv,
#                  dimensionList =
#                  list(
#                        x=list(name='west-east',values=1:nxOut),
#                        y=list(name='south-north',values=1:nyOut)),
#                  data=dataOut)
#
#  outList
#
#}

  
##' The regridMultiNcdf for getting variables out of individual files.
##' 
##' \code{regridMultiNcdfVar}: For a group of NetCDF files, extract
##' and regrid a list of variables to a WRF-Hydro domain.
##'
##' @param varInd The variable index.
##' @param varList The variable list.
##' @param files The files vector.
##' @param geoFile The Geogrid LSM file for regridding.
##' @param method Method of regridding. Current acceptable values are
##' "bilinear"
##' @return A list
##' @keywords internal
##' @keywords dataRegrid
##' @family regridMultiNcdf
##' @export 
#regridMultiNcdfVar <- function(varInd, varList, levTypeList, files,
#                               geoFile, method){
#
#  #Check for existence of GeoFile
#  if(!file.exists(geoFile)){
#    stop(paste0('ERROR: ',geoFile,' not found.'))
#  }
#  
#  outList <- plyr::llply(files, regridNcdf,
#                         var = varList[[varInd]],
#                         geoFile = geoFile,
#                         method = method)
#  outList
#}


##' The regridMultiNcdf for individual file groups.
##'
##' \code{regridMultiNcdfFile} : For this file group, regrid
##' NetCDF data spread over multiple files. Typically called by 
##' regridMultiNcdf.
##' 
##' @param fileInd The index of the file group.
##' @param fileList The list of the file group.
##' @param varList The variable list.
##' @param geoFile The geofile needed for regridding.
##' @param method Method of regridding. Current acceptable values are 
##' 'bilinear'.
##' @return A list
##' @keywords internal
##' @keywords dataRegrid
##' @keywords regridMultiNcdf
##' @export
#regridMultiNcdf <- function(fileInd,fileList,varList,geoFile,method){
#  #Check for existence of GeoFile
#  if(!file.exists(geoFile)){
#    stop(paste0('ERROR: ',geoFile,' not found.'))
#  }
#  #Enforce collation at the variable-level level: (for this file group)
#  #each variable has a co-located level index.
#  if (length(varList) != length(fileList)){
#    stop(paste0("for file set ",names(fileList)[fileInd]," (#",fileInd,") ",
#                "The variable and file lists must be collated: their
#                lengths do not match."))
#  }
#  if (all(names(varList) != names(fileList))){
#    stop(paste0("For file set ",names(fileList)[fileInd]," (#",fileInd,") ",
#                "The variable and level lists must be collated: their
#                names do not match."))
#  }
#  varInd <- 1:length(varList[[fileInd]])

#  outList <- plyr::llply(varInd,regridMultiNcdfVar,
#
#                         varList = varList[[fileInd]],
#                         files = fileList,
#                         geoFile = geoFile,
#                         method = method)
# 
#  outList
#}


##' Regrid multiple variables across various NetCDF file groups to the 
##' WRF-Hydro domain.
##'
##' \code{RegridMultiNcdf} is designed to regrid multiple variables across
##' multiple NetCDF files. Two collated lists specify 1) file groups,
##' and 2) variables for each file group. The names of the lists must match.
##' In addition, a Geo LSM file must be presented for regridding to the WRF-Hydro
##' domain. NetCDF input files must have center lat/lon values for the entire
##' grid. Also, a regridding method must be provided to specify how ESMF will regrid
##' the data. Valid options currently include "bilinear". See examples for details.
##'
##' @param fileList The list of file groups. Names must match those in the
##'   other lists.
##' @param variableList The list of GRIB variables for each file group. 
##'   Names must match fileList.
##' @param geoFile The path to the Geo LSM file used for regridding. 
##' @param method Method of regridding. Current acceptable values are
##'   "bilinear"
##' @return A list containing regridded data along with meta-data.
##'
##' @examples
##' #This example shows opening multiple SNODAS NetCDF conus files containing
##' #snow depth and SWE for regridding.
##' \dontrun{
##' snodasPath <- '/d4/karsten/data/SNODAS'
##' NCFiles <- list.files(path=snodasPath,pattern='SNODAS_CONUS_',full.names=TRUE)
##' #fileList <- these are the groups of files.
##' fileList <- list(SNODASList1 = NCFiles)
##' #varList <- Define which variables are desired for each file group.
##' NCVars <- list(SWE='SNEQV',DEPTH='SNOWH')
##' varList <- list(SNODASList1 = NCVars)
##' geoFile <- '/d4/karsten/geospatial/geo_em.d01.nc'
##' regridData <- regridMultiNcdf(fileList=fileList,varList=varList,geoFile=geoFile,
##'               'bilinear')
##'
##' }
##' @export 
#regridMultiNcdf <- function(fileList,varList,geoFile,method){
#  #Check for existence of GeoFile
#  if(!file.exists(geoFile)){
#    stop(paste0('ERROR: ',geoFile,' not found.'))
#  }
#  ##Only do collated lists. Collation check at teh file-variable-level level.
#  if (length(varList) != length(fileList)){
#    stop("The input lists must be collated: their lengths do not match.")
#  }
#  if (all(names(varList) != names(fileList))){
#    stop("The input lists must be collated: their names do not match.")
#  }
#  fileInd <- 1:length(fileList)
#
#  outList <- plyr::llply(fileInd,regridMultiNcdfFile,
#                         varList=varList,
#                         fileList=fileList,
#                         geoFile=geoFile,
#                         method=method)
#  outList
#} 

