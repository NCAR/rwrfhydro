#' Extract GRIB grid type using ECMWF GRIB_API.
#' 
#' \code{GRIBgridType} Obtain grid projection from GRIB file for a given file.
#' ECMWF GRIB_API INSTALLATION ON YOUR SYSTEM IS REQUIRED.
#' 
#' @param fileIn The GRIB file of interest.
#' @return gridType The GRIB projection
#' @examples
#' gridType <- GRIBgridType('rap.t01z.awp130bgrbf16.grib2')
#' 
#' @keywords io geospatial GRIB
#' @concept geospatial GRIB
#' @family io GRIB
#' @useDynLib rwrfhydro
#' @export
GRIBgridType <- function(fileIn){
  #Check for GRIB file existence
  if(!file.exists(fileIn)){
    warning(paste0('ERROR: GRIB file: ',fileIn,' not found.'))
    return(FALSE)
  }
 
  #Establish variables
  error <- as.integer(0)
  projection <- "" #Need to establish empty character string to pass to Fortran
  fLen <- as.integer(nchar(fileIn)) #Fortran needs to know character lengths
  
  #Get projection 
  data <- .Fortran("grib_grid_type",fLen,fileIn,
                   projection,error)
  
  error <- data[[4]]
  if(error != 0){
    stop(paste0('ERROR: grib_grid_type returned exist status of: ',error))
  }
  
  projection <- data[[3]]
  strTemp <- stringr::str_split(projection, " ") #Fortran can't pass trimmed character array 
  projection <- strTemp[[1]][1]
  
  return(projection)
}

#' Use projection information to extract geospatial information from a GRIB file.
#' 
#' \code{GRIBgeospatial} Extract relevant geospatial information associated with a 
#' GRIB file based on it's projection
#' 
#' @param proj character string specifying projection. Can be obtained from 
#'  GRIBgridType.
#' @param fileIn character string specifying GRIB file to pull geospatial info
#'  from.
#' @return GRIBgeo data frame with relevant geospatial information
#' @examples
#' GRIBgeo <- GRIBgeospatial('lambert','rap.t01z.awp130bgrbf16.grib2')
#' 
#' @keywords io geospatial
#' @concept GRIB geospatial
#' @family io geospatial GRIB
#' @export
GRIBgeospatial <- function(proj,fileIn){
  #Check for existence of GRIB file
  if(!file.exists(fileIn)){
    stop(paste0('ERROR: GRIB file: ',fileIn,' not found'))
  }
  
  #Call projection-specific functions to obtain relevant information
  if(proj == "lambert"){
    GRIBgeo <- GRIBgeospatialLambert(fileIn)
  } else if (proj == "regular_gg"){
    GRIBgeo <- GRIBgeospatialGauss(fileIn)
  } else if (proj == "regular_ll"){
    GRIBgeo <- GRIBgeospatialLL(fileIn)
  } else if (proj == "polar_stereographic"){
    GRIBgeo <- GRIBgeospatialPolar(fileIn)
  } else {
    stop(paste0('ERROR: GRIBgeospatial currently not compatible with
                 projection: ',proj))
  }
  
  return(GRIBgeo)
}

#' Extract Lambert Conformal geospatial parameters from GRIB file.
#' 
#' \code{GRIBgeospatialLambert} Pull Lambert Conformal geospatial 
#' parameters from GRIB file.
#' 
#' @param fileIn Character string specifying GRIB file to extract 
#'   information from.
#' @return GRIBgeo data frame containing relevant geospatial info.
#' @examples
#' GRIBgeo <- GRIBgeospatialLambert('rap.t01z.awp130bgrbf16.grib2')
#' 
#' @keywords metadata
#' @concept geospatial GRIB
#' @family geospatial GRIB
#' @useDynLib rwrfhydro
#' @export
GRIBgeospatialLambert <- function(fileIn){

  #Establish data frame and variables
  fLen <- as.integer(nchar(fileIn))
  nxTemp <- as.integer(0)
  nyTemp <- as.integer(0)
  dxTemp <- as.integer(0) #Meters
  dyTemp <- as.integer(0) #Meters
  lat1Temp <- as.numeric(0.0) #LL Latitude - degrees
  lon1Temp <- as.numeric(0.0) #LL Longitude - degrees
  lonVTemp <- as.numeric(0.0) #Origin Longitude - degrees
  latDTemp <- as.numeric(0.0) #Origin Latitude - degrees
  latin1Temp <- as.numeric(0.0) #Standard Parallel Latitude 1 - degrres
  latin2Temp <- as.numeric(0.0) #Standard Parallel Latitude 2 - degrees
  snTemp <- as.integer(0) #Flag to indicate if grid is read in NS or SN
                          #1 - Data read SN
                          #0 - Data read NS
  errorTemp <- as.integer(0)
  
  #Create data frame to hold output
  GRIBgeo <- data.frame(matrix(NA,nrow=1,ncol=11))
  names(GRIBgeo) <- c("NX","NY","DX","DY","LAT1","LON1","LONV",
                      "LATD","LATIN1","LATIN2","SNFLAG")
  
  #Call Fortran shared object
  dataTemp <- .Fortran('get_lambert_grid_meta',fLen, fileIn,nxTemp,
                       nyTemp,dxTemp,dyTemp,lat1Temp,lon1Temp,
                       lonVTemp,latDTemp,latin1Temp,latin2Temp,
                       snTemp,errorTemp)
  
  error <- dataTemp[[14]]
  if(error != 0){
    stop(paste0('ERROR: get_lambert_grid_meta returned an exit
                 status of: ',error))
  }
  
  GRIBgeo$NX <- dataTemp[[3]]
  GRIBgeo$NY <- dataTemp[[4]]
  GRIBgeo$DX <- dataTemp[[5]]
  GRIBgeo$DY <- dataTemp[[6]]
  GRIBgeo$LAT1 <- dataTemp[[7]]
  GRIBgeo$LON1 <- dataTemp[[8]]
  GRIBgeo$LONV <- dataTemp[[9]]
  GRIBgeo$LATD <- dataTemp[[10]]
  GRIBgeo$LATIN1 <- dataTemp[[11]]
  GRIBgeo$LATIN2 <- dataTemp[[12]]
  GRIBgeo$SNFLAG <- dataTemp[[13]]
  
  return(GRIBgeo)
}
#' Extract regular Gaussian geospatial parameters from GRIB file.
#'
#' \code{GRIBgeospatialGaus} Pull regular gaussian geospatial parameters
#' from GRIB file.
#'
#' @param fileIn Character string specifying GRIB file to extract 
#' information from.
#' @return GRIBgeo data frame containing relevant geospatial info.
#' @examples
#' GRIBgeo <- GRIBgeospatialGaus('gfs.t00z.f01.grib2')
#'
#' @keywords metadata
#' @concept geospatial GRIB
#' @family geospatial GRIB
#' @useDynLib rwrfhydro
#' @export
GRIBgeospatialGauss <- function(fileIn){
  
  #Establish data frame and variables
  fLen <- as.integer(nchar(fileIn))
  nxTemp <- as.integer(0)
  nyTemp <- as.integer(0)
  dxTemp <- as.numeric(0.0) #Degrees
  lat1Temp <- as.numeric(0.0) #LL Latitude - degrees
  lon1Temp <- as.numeric(0.0) #LL Longitude - degrees
  lat2Temp <- as.numeric(0.0) #UR Latitude - degrees
  lon2Temp <- as.numeric(0.0) #UR Longitude - degrees
  snTemp <- as.integer(0) #Flag to indicate if grid is read in NS or SN
                          #1 - Data read SN
                          #0 - Data read NS
  errorTemp <- as.integer(0)
  
  #Create data frame to hold output
  GRIBgeo <- data.frame(matrix(NA,nrow=1,ncol=8))
  names(GRIBgeo) <- c("NX","NY","DX","LAT1","LON1","LAT2",
                      "LON2","SNFLAG")
  
  #Call Fortran shared object
  dataTemp <- .Fortran('get_gauss_grid_meta',fLen,fileIn,nxTemp,
                       nyTemp,dxTemp,lat1Temp,lon1Temp,
                       lat2Temp,lon2Temp,snTemp,errorTemp)
  
  error <- dataTemp[[11]]
  if(error != 0){
    stop(paste0('ERROR: get_gauss_meta returned an exit status
                 of: ',error))
  }
  
  GRIBgeo$NX <- dataTemp[[3]]
  GRIBgeo$NY <- dataTemp[[4]]
  GRIBgeo$DX <- dataTemp[[5]]
  GRIBgeo$LAT1 <- dataTemp[[6]]
  GRIBgeo$LON1 <- dataTemp[[7]]
  GRIBgeo$LAT2 <- dataTemp[[8]]
  GRIBgeo$LON2 <- dataTemp[[9]]
  GRIBgeo$SNFLAG <- dataTemp[[10]]
  
  return(GRIBgeo)
}
#' Extract lat/lon projection parameters from GRIB file.
#'
#' \code{GRIBgeospatialLL} Pull lat/lon projection geospatial 
#' parameters from GRIB file.
#'
#' @param fileIn Character string specifying GRIB file to extract 
#' information from.
#' @return GRIBgeo data frame containing relevant geospatial info.
#' @examples
#' GRIBgeo <- GRIBgeospatialLL('nldas.force0125A.t00z.f01.grib')
#' 
#' @keywords metadata
#' @concept geospatial GRIB
#' @family geospatial GRIB
#' @useDynLib rwrfhydro
#' @export
GRIBgeospatialLL <- function(fileIn){
  
  #Establish data frame and variables
  fLen <- as.integer(nchar(fileIn))
  nxTemp <- as.integer(0)
  nyTemp <- as.integer(0)
  dxTemp <- as.numeric(0.0) #Degrees
  dyTemp <- as.numeric(0.0) #Degrees
  lat1Temp <- as.numeric(0.0) #LL Latitude - degrees
  lon1Temp <- as.numeric(0.0) #LL Longitude - degrees
  lat2Temp <- as.numeric(0.0) #UR Latitude - degrees
  lon2Temp <- as.numeric(0.0) #UR Longitude - degrees
  snTemp <- as.integer(0) #Flag to indicate if grid is read in NS or SN
                          #1 - Data read SN
                          #0 - Data read NS
  errorTemp <- as.integer(0)
  
  #Create data frame to hold output
  GRIBgeo <- data.frame(matrix(NA,nrow=1,ncol=9))
  names(GRIBgeo) <- c("NX","NY","DX","DY","LAT1","LON1","LAT2",
                      "LON2","SNFLAG")
  
  #Call Fortran shared object
  dataTemp <- .Fortran('get_ll_grid_meta',fLen,fileIn,nxTemp,
                       nyTemp,dxTemp,dyTemp,lat1Temp,lon1Temp,
                       lat2Temp,lon2Temp,snTemp,errorTemp)
  
  error <- dataTemp[[12]]
  if(error != 0){
    stop(paste0('ERROR: get_ll_meta returned an exit status
                 of: ',error))
  }
  
  GRIBgeo$NX <- dataTemp[[3]]
  GRIBgeo$NY <- dataTemp[[4]]
  GRIBgeo$DX <- dataTemp[[5]]
  GRIBgeo$DY <- dataTemp[[6]]
  GRIBgeo$LAT1 <- dataTemp[[7]]
  GRIBgeo$LON1 <- dataTemp[[8]]
  GRIBgeo$LAT2 <- dataTemp[[9]]
  GRIBgeo$LON2 <- dataTemp[[10]]
  GRIBgeo$SNFLAG <- dataTemp[[11]]
  
  return(GRIBgeo)
}
#' Extract Polar Stereographic projection parameters from GRIB file.
#'
#' \code{GRIBgeospatialPolar} Pull Polar Stereographic geospatial
#' parameters from GRIB file.
#'
#' @param fileIn Character string specifying GRIB file to extract 
#' information from.
#' @return GRIBgeo data frame containing relevant geospatial info.
#' @examples
#' GRIBgeo <- GRIBgeospatialPolar('stage4.t00z.f01.grib')
#'
#' @keywords metadata
#' @concept geospatial GRIB
#' @family geospatial GRIB
#' @useDynLib rwrfhydro
#' @export
GRIBgeospatialPolar <- function(fileIn){
  
  #Establish data frame and variables
  fLen <- as.integer(nchar(fileIn))
  nxTemp <- as.integer(0)
  nyTemp <- as.integer(0)
  dxTemp <- as.integer(0) #Meters
  dyTemp <- as.integer(0) #Meters
  lat1Temp <- as.numeric(0.0) #LL Latitude - degrees
  lon1Temp <- as.numeric(0.0) #LL Longitude - degrees
  lonVTemp <- as.numeric(0.0) #Orientation Longitude - degrees
  snTemp <- as.integer(0) #Flag to indicate if grid is read in NS or SN
                          #1 - Data read SN
                          #0 - Data read NS
  errorTemp <- as.integer(0)
  
  #Create data frame to hold output
  GRIBgeo <- data.frame(matrix(NA,nrow=1,ncol=8))
  names(GRIBgeo) <- c("NX","NY","DX","DY","LAT1","LON1","LONV",
                      "SNFLAG")
  
  #Call Fortran shared object
  dataTemp <- .Fortran('get_polar_grid_meta',fLen,fileIn,nxTemp,
                       nyTemp,dxTemp,dyTemp,lat1Temp,lon1Temp,
                       lonVTemp,snTemp,errorTemp)
  
  error <- dataTemp[[11]]
  if(error != 0){
    stop(paste0('ERROR: get_polar_meta returned an exit status
                of: ',error))
  }
  
  GRIBgeo$NX <- dataTemp[[3]]
  GRIBgeo$NY <- dataTemp[[4]]
  GRIBgeo$DX <- dataTemp[[5]]
  GRIBgeo$DY <- dataTemp[[6]]
  GRIBgeo$LAT1 <- dataTemp[[7]]
  GRIBgeo$LON1 <- dataTemp[[8]]
  GRIBgeo$LONV <- dataTemp[[9]]
  GRIBgeo$SNFLAG <- dataTemp[[10]]
  
  return(GRIBgeo)
}
#' Extract grid of GRIB data given a variable name and level.
#' 
#' \code{extractGRIBGrid} Extract grid of GRIB data.
#' 
#' @param fileIn GRIB file to be opened for reading.
#' @param var GRIB variable to be extracted.
#' @param levType Level type to extract GRIB data. Examples of 
#' levType are "surface", "heightAboveGround","cloudTop"
#' @param level GRIB level to be extracted.
#' @param nx Integer number of GRIB data columns.
#' @param ny Integer number of GRIB data rows.
#' @param numFTimes Optional number of forecast times in 
#' GRIB file. If not specified, defaults to 1. 
#' @return dataOut List of metadata and data to user.
#'  
#' @examples
#' datOut <- extractGRIBGrid('rap.t01z.awp130bgrbf16.grib2','acpcp',
#'            'surface',0,451,337)
#'             
#' @keywords IO GRIB
#' @concept GRIB
#' @family IO
#' @useDynLib rwrfhydro
#' @export
extractGRIBGrid <- function(fileIn,var,levType,level,nx,ny,numFTimes=1){
  #Check for existence of GRIB file
  if(!file.exists(fileIn)){
    stop(paste0('ERROR: GRIB file: ',fileIn,' not found'))
  }
  
  #Establish variables
  l1 <- as.integer(nchar(fileIn))
  l2 <- as.integer(nchar(var))
  l3 <- as.integer(nchar(levType))
  nxTemp <- as.integer(nx)
  nyTemp <- as.integer(ny) 
  levelTemp <- as.integer(level)
  longName <- "" #Initiate strings as empty before passing to Fortran
  units <- ""
  dateCycleYYYYMMDD <- as.integer(array(-9999,c(numFTimes)))
  dateHHMM <- as.integer(array(-9999,c(numFTimes)))
  bStep <- as.integer(array(-9999,c(numFTimes)))
  eStep <- as.integer(array(-9999,c(numFTimes)))
  stepRange <- character(length=numFTimes*4)
  l4 <- as.integer(0) #These will be updated for string lengths in Fortran
  l5 <- as.integer(0)
  ndv <- as.numeric(-9999.0)
  error <- as.integer(0)
  
  metaDF <- data.frame(matrix(NA,nrow=1,ncol=3))
  names(metaDF) <- c("LONGNAME","UNITS","NDV")
  
  #Establish matrix to hold grid
  gridOut <- array(ndv,c(nx,ny,numFTimes))
  
  #Call Fortran shared object
  dataTemp <- .Fortran('grib_grid_extract',l1,fileIn,l2,var,nxTemp,
                       nyTemp,gridOut,l3,levType,levelTemp,longName,
                       units,ndv,l4,l5,numFTimes,dateCycleYYYYMMDD,
                       dateHHMM,bStep,eStep,error)
  
  error <- dataTemp[[21]]
  dateCycleYYYYMMDD <- dataTemp[[17]]
  dateHHMM <- dataTemp[[18]]
  bStep <- dataTemp[[19]]
  eStep <- dataTemp[[20]]
  l4 <- dataTemp[[14]]
  l5 <- dataTemp[[15]]
  gridOut <- dataTemp[[7]]
  if(error != 0){
    stop(paste0('ERROR: grib_grid_extract returned exist status of: ',error))
  }
  
  #Check for valid values. If entire grid is ndv, then GRIB API failed
  #to properly find variable
  if(length(which(gridOut != ndv)) == 0){
    stop(paste0('ERROR: Variable ',var,' Returned no data. Please check your GRIB parameters'))
  }
  #Calculate date-time information based on cycle/steprange information.
  dateCycle <- as.POSIXct(array(NA,c(numFTimes)))
  dateVer <- as.POSIXct(array(NA,c(numFTimes)))
  dateBVer <- as.POSIXct(array(NA,c(numFTimes)))
  
  for (fStep in 1:numFTimes){
    #Cycle time
    dateTempYYYYMMDD <- dateCycleYYYYMMDD[fStep]
    dateTempYYYYMMDD <- as.Date(toString(dateTempYYYYMMDD),format="%Y%m%d")
    dateTempYYYYMMDD <- as.POSIXct(dateTempYYYYMMDD)
    hourTemp <- floor(dateHHMM[fStep]/100)
    dTempPOSIXct <- dateTempYYYYMMDD + hourTemp*3600
    dateCycle[fStep] <- dTempPOSIXct
    #Verification time (I.E. cycle time + forecast hour)
    dateVer[fStep] <- dTempPOSIXct + eStep[fStep]*3600
    dateBVer[fStep] <- dTempPOSIXct + bStep[fStep]*3600
  }
  #Create list to return to the user
  strTemp <- stringr::str_split(dataTemp[[11]], "")
  lNameStr <- stringr::str_c(strTemp[[1]][1:l4],sep=" ",collapse="")
  strTemp <- stringr::str_split(dataTemp[[12]],"")
  unitsStr <- stringr::str_c(strTemp[[1]][1:l5],sep=" ",collapse="")
  
  dataOut <- list(data=dataTemp[[7]],ndv=dataTemp[[13]],longName=lNameStr,
                  units=unitsStr,cycle=dateCycle,begVerTime=dateBVer,
                  verTime=dateVer)
  
  return(dataOut)
  
}
#' Extract lat/lon values as a grid from a GRIB file.
#'
#' \code{GRIBLatLon} Extract lat/lon grids from GRIB file.
#'
#' @param fileIn GRIB file to extract values from.
#' @param nx number of columns of GRIB data.
#' @param ny number of rows of GRIB data.
#' @return dataOut 3D grid (nx,ny,2) of lat/lon values. 
#'  (,,1) is latitude, (,,2) is longitude.
#' @examples
#' latLon <- GRIBLatLon('hrrr.grib2',1200,800)
#' @keywords GRIB geospatial
#' @family geospatial
#' @family metadata
#' @useDynLib rwrfhydro
#' @export
GRIBLatLon <- function(fileIn,nx,ny){
  #Check for existence of GRIB file.
  if(!file.exists(fileIn)){
    warning(paste0('ERROR: GRIB file: ',fileIn,' not found.'))
    return(FALSE)
  }

  #Establish lat/lon out grid. -9999.0 will be default missing value.
  latLon <- array(-9999.0,c(nx,ny,2))
  latTemp <- matrix(nrow=nx,ncol=ny)
  lonTemp <- matrix(nrow=nx,ncol=ny)
  latTemp[,] <- -9999.0
  lonTemp[,] <- -9999.0

  len1 <- as.integer(nchar(fileIn))
  nx <- as.integer(nx)
  ny <- as.integer(ny)
  error <- as.integer(0)

  #Call Fortran shared object'
  dataTemp <- .Fortran('grib_get_lat_lon',len1,fileIn,nx,ny,
                       latTemp,lonTemp,error)
  
  error <- dataTemp[[7]]
  if(error != 0){
    stop(paste0('ERROR: grib_get_lat_lon returned exit status of: ',error))
  }

  latLon[,,1] <- dataTemp[[5]]
  latLon[,,2] <- dataTemp[[6]]

  return(latLon)

}

#' Extract number of forecast times for a particular variable from
#' a GRIB file.
#'
#' \code{GRIBNumForecastTimes} Extract number of forecast times
#' from a GRIB file.
#'
#' @param fileIn GRIB file to extract times from.
#' @param var Variable of interest.
#' @param levType Level type variable resides on.
#' @param lev Level variable resides on.
#' @return numTimes Number of variable steps in file.
#' @keywords GRIB IO
#' @family metadata
#' @useDynLib rwrfhydro
#' @export
GRIBNumForecastTimes <- function(fileIn,var,levType,lev){
  #Check for existence of GRIB file.
  if(!file.exists(fileIn)){
    stop(paste0('ERROR: GRIB file: ',fileIn,' not found.'))
  }
  
  #Establish variables to be passed to Fortran
  len1 <- as.integer(nchar(fileIn))
  len2 <- as.integer(nchar(var))
  len3 <- as.integer(nchar(levType))
  lev <- as.integer(lev)
  numTimes <- as.integer(0)
  error <- as.integer(0)
  
  #Call Fortran shared object
  dataTemp <- .Fortran('grib_get_steps',len1,fileIn,len2,var,
                       len3,levType,lev,numTimes,error)
  
  error <- dataTemp[9]
  if(error != 0){
    stop(paste0('ERROR: grib_get_steps returned exit status of: ',error))
  } 
  
  numTimes <- dataTemp[[8]]
  if(numTimes == 0){
    stop(paste0('ERROR: variable ',var,' found 0 steps in the GRIB file.'))
  } 
  
  return(numTimes)
  
}
