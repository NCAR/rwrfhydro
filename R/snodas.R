#' Get and unpack the SNODAS snow depth and SWE tarball for given dates.
#' 
#' \code{GetSnodasDepthSweDate} Get and unpack the SNODAS snow depth and SWE
#' tarball for a given date.
#' 
#' @param datePOSIXct The date in POSIXct format for which data is desired. Only
#'   whole days matter.
#' @param outputDir   The directory where the data are to be archived. This
#'   directory is checked to see if the data exist when overwrite=FALSE
#' @param overwrite   When false: If the depth and SWE files exist on disk, dont
#'   grab the tarball. \cr When false: If the tarball exists on disk but depth
#'   and SWE files dont, just unpack the tarball. \cr When true: Pull new
#'   tarball and overwrite any existing files with the same date.
#' @param quiet       Passed to curl, to show it's progress (typcially too fast
#'   to matter).
#' @param parallel    Logical Defaults to (foreach::getDoParWorkers>1), so if
#'   you've set up parallelization it is automatically used.
#' @return Logical was the file "got"?
#' @examples
#' snodasGot <- GetSnodasDepthSweDate(as.POSIXct('2015-02-28'),
#'                                    outputDir = path.expand('~'))
#' @keywords IO
#' @concept SNODAS
#' @family SNODAS
#' @export
GetSnodasDepthSweDate <- function(datePOSIXct, outputDir='.', overwrite=FALSE, 
                                  quiet=TRUE, 
                                  parallel=(foreach::getDoParWorkers()>1) ){
  
  ## This atomic function gets called below. 
  GetSnodasDepthSweDate.atomic <- function(datePOSIXct, outputDir='.', overwrite=FALSE, 
                                           quiet=TRUE) {
    # date parameters
    yy <- format(datePOSIXct, c("%Y")); mm <- format(datePOSIXct, c("%m"))
    mon <- format(datePOSIXct, c("%h")); dd <- format(datePOSIXct, c("%d"))
    
    # depthProdId <- '1036',  sweProdId <- '1034'
    # Can construct the filenames. Calling this "0" incase they dont match the
    # names in the tarball.
    depthFile0<- paste0(outputDir,'/','us_ssmv1',
                        '1036tS__T0001TTNATS',yy,mm,dd,'05HP001.dat.gz')
    sweFile0  <- paste0(outputDir,'/','us_ssmv1',
                        '1034tS__T0001TTNATS',yy,mm,dd,'05HP001.dat.gz')
    
    ## If either of the depth or SWE files exist, bail out unless told to overwrite.
    if( (file.exists(depthFile0) | file.exists(sweFile0)) & !overwrite) return(0)
    
    # Go to the correct directory for arciving the data
    origDir <- getwd()
    setwd(outputDir)
    
    # theFile is the tarball
    theFile <- paste0('SNODAS_',yy,mm,dd,'.tar')
    
    # if theFile (tarball) exists, then skip downloading unless told to overwrite
    if(!file.exists(theFile) | overwrite) {
      theUrl <- paste0('ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/',
                       yy,'/',mm,'_',mon,'/',theFile)
      if(!quiet) print(paste('SNODAS: ', datePOSIXct))
      try(curl::curl_download(theUrl, theFile, quiet=quiet))
      if(!file.exists(theFile)) {
        warning(paste0('Error: File not obtained via FTP: ',theFile))
        setwd(origDir)
        return(FALSE)
      }
    }
    
    # unpack tarball to depth and SWE. name the tmpDir by date so dates can be done in parallel.
    tmpDir <- paste0('tmp',yy,mm,dd)
    if(file.exists(tmpDir)) unlink(tmpDir, recursive=TRUE)
    untar(theFile, exdir=tmpDir)
    sweFile <- list.files(path = tmpDir, pattern=glob2rx('*ssmv11034tS*.dat.gz'))  # SWE
    file.copy(paste0(tmpDir,'/',sweFile), sweFile0)
    depthFile <- list.files(path = tmpDir, pattern=glob2rx('*ssmv11036tS*.dat.gz'))  # depth
    file.copy(paste0(tmpDir,'/',depthFile), depthFile0)
    unlink(c(tmpDir, theFile), recursive=TRUE) 
    Sys.chmod(c(depthFile0, sweFile0), mode='0777', use_umask=FALSE)
    
    setwd(origDir)
    TRUE
  }
  
  ## FormalsToDf handles vector arguments and passes collated combos 
  ## to the atomic function 
  vecDf <- FormalsToDf(GetSnodasDepthSweDate)
  vecDf <- vecDf[,-which(names(vecDf) %in% c('parallel'))] ## exclude formals not passed to atomic
  ret <- plyr::mlply(vecDf, GetSnodasDepthSweDate.atomic, .parallel=parallel)
  names(ret) <- datePOSIXct
  if(length(ret)==1) ret <- ret[[1]]
  ret
}


#' Read snodas snow depth and SWE into memory for a given date.
#'
#' \code{ReadSnodasDepthSweDate} Read (from local disk into memory) the SNODAS snow depth and SWE for a given date. 
#' 
#' @param datePOSIXct  The date in POSIXct format for which data is desired. Only whole days matter. 
#' @param outputDir    The directory where the data are archived.
#' @return A list with a SWE and a depth matrix. 
#' @examples
#' snodasGot <- GetSnodasDepthSweDate(as.POSIXct('2015-02-28'),
#'                                    outputDir = path.expand('~'))
#' if(snodasGot) snodasList <- ReadSnodasDepthSweDate(as.POSIXct('2015-02-28'),
#'                                    outputDir = path.expand('~'))
#' unlink(path.expand('~/us_ssmv1103*.dat.gz'))
#' @keywords manip
#' @concept SNODAS
#' @family SNODAS
#' @export
ReadSnodasDepthSweDate <- function(datePOSIXct, outputDir='.') {
  # date parameters
  yy <- format(datePOSIXct, c("%Y")); mm <- format(datePOSIXct, c("%m"))
  mon <- format(datePOSIXct, c("%h")); dd <- format(datePOSIXct, c("%d"))
 
  # depthProdId <- '1036',  sweProdId <- '1034'
  depthFile0 <- paste0(outputDir,'/','us_ssmv1',
                      '1036tS__T0001TTNATS',yy,mm,dd,'05HP001.dat.gz')
  sweFile0   <- paste0(outputDir,'/','us_ssmv1',
                      '1034tS__T0001TTNATS',yy,mm,dd,'05HP001.dat.gz')

  # SNODAS spatial reference and scaling
  nCol <- 6935
  nRow <- 3351 #columns and rows number:masked version of contiguous US
  dataScaleFactor  <-  1000  #multiply the data this amount, both depth and SWE

  depthCon  <- gzcon(file(depthFile0, "rb"))
  depthData <- readBin(depthCon, integer(), n=nRow*nCol, size=2,
                       signed=TRUE, endian='big')/dataScaleFactor
  close(depthCon)
  
  sweCon  <- gzcon(file(sweFile0, "rb"))
  sweData <- readBin(sweCon, integer(), n=nRow*nCol, size=2,
                     signed=TRUE, endian='big')/dataScaleFactor
  
  #Convert to mm
  indTemp <- which(sweData >= 0.0)
  sweData[indTemp] <- sweData[indTemp]*1000.0
  indTemp <- which(depthData >= 0.0)
  depthData[indTemp] <- depthData[indTemp]*1000.0
  
  close(sweCon)

  # I've seen corrupted files which dont have the proper length depth.
  # In this case, I should delete the files and hope a future update fixes the problem.
  # e.g.: /dirt/scratch/snodas/2009/us_ssmv11036tS__T0001TTNATS2009061305HP001.dat.gz
  if(length(depthData)!=(nRow*nCol) | length(sweData)!=(nRow*nCol)) {
    warning(paste0('Error: At least one file corrupted and both deleted:', depthFile , sweFile, sep=' '))
    system(paste0('rm ',depthFile))
    system(paste0('rm ',sweFile))
    return(NULL)
  }

  #Rotation is necessary as the data will be passed into NetCDF as (c,r). When it's 
  #Written out, it will be listed as (r,c) in the NetCDF file.
  list(datePOSIXct = datePOSIXct,
       depth.m = RotateCw(matrix(depthData, ncol=nCol, nrow=nRow, byrow=TRUE)),
       swe.m   = RotateCw(matrix(sweData,   ncol=nCol, nrow=nRow, byrow=TRUE)),
       nRowNative = nRow, nColNative = nCol)}  
  
  
#' Write output of ReadSnodasDepthSweDate to netcdf.
#'
#' \code{PutSnodasNcdf} Put the output of ReadSnodasDepthSweDate into a netcdf file. 
#' 
#' @param snodasList The output of ReadSnodasDepthSweDate. 
#' @param outputDir Character. The directory path where the output files are to be written. 
#' @return Success if the filename which is (SNODAS_YYYYMMDD.nc), otherwise NULL.
#' @examples
#' \dontrun{
#' snodasGot <- GetSnodasDepthSweDate(as.POSIXct('2015-02-28'))
#' if(snodasGot) snodasList <- ReadSnodasDepthSweDate(as.POSIXct('2015-02-28'))
#' PutSnodasNcdf(snodasList)
#' }
# @TODO make output, particularly time dim, cf compliant.
#' @keywords IO
#' @concept SNODAS
#' @family SNODAS
#' @export
PutSnodasNcdf <- function(snodasList, outputDir='.') {
  ## make it a vanilla date... 
  theDate <- as.POSIXct(format(snodasList$datePOSIXct,'%Y-%m-%d'),'UTC')
  varList = list()
  varList[[1]] <- list( name='SNEQV', #Name to be consistent with LDASOUT files
                       longname='Snow water equivalent',
                       units='mm',
                       precision = 'double',
                       missing = min(snodasList$swe.m),
                       dimensionList =
                       list(
                            x=list(name='Longitude',values=1:snodasList$nColNative,
                                units='Degrees East', unlimited=FALSE,
                                create_dimvar=FALSE),
                            y=list(name='Latitude',values=1:snodasList$nRowNative,
                              units='Degrees North', unlimited=FALSE,
                              create_dimvar=FALSE),
                            t=list(name='Time',values=as.numeric(theDate),
                              units='POSIXct', unlimited=TRUE,
                              create_dimvar=TRUE)
                            ),
                       data = snodasList$swe.m ) 

  varList[[2]] <- list( name='SNOWH', #Name to be consistent with LDASOUT files
                       longname='Snow depth',
                       units='mm',
                       precision = 'double',
                       missing = min(snodasList$depth.m),
                       dimensionList =
                       list(
                            x=list(name='Longitude',values=1:snodasList$nColNative,
                              units='Degrees East', unlimited=FALSE,
                              create_dimvar=FALSE),
                            y=list(name='Latitude',values=1:snodasList$nRowNative,
                              units='Degrees North', unlimited=FALSE,
                              create_dimvar=FALSE),
                            t=list(name='Time',values=as.numeric(theDate),
                              units='POSIXct', unlimited=TRUE,
                              create_dimvar=TRUE)
                            ),
                       data = snodasList$depth.m )
  
  globalAttList <- list()
  globalAttList[[1]] <- list(name='Time',value='2012-07-05_00:00:00', precision="text")
  globalAttList[[2]] <- list(name='POSIXct Origin',value='1970-01-01 00:00.00 UTC', precision="text")
  
  outFile <- paste0(outputDir,'/','SNODAS_',format(snodasList$datePOSIXct,'%Y%m%d'),'.nc')
  MkNcdf( varList, globalAttList=globalAttList, filename=outFile )
}  


#' Calculate the SNODAS coordinates.
#'
#' \code{CalcSnodasCoords} Calculate the SNODAS coordinates. 
#' 
#' @return A list of lon and lat
#' @examples
#' snodasCoords <- CalcSnodasCoords()
#' @keywords manip
#' @concept SNODAS
#' @family SNODAS
#' @export
CalcSnodasCoords <- function() {
  nCol=6935
  nRow=3351 #columns and rows number:masked version of contiguous US
  minX= -124.729166666662
  maxX= -66.9416666666642
  minY= 24.9499999999990
  maxY= 52.8749999999978
  res <- 0.00833333333333300
  benchX <- -124.729166666662
  benchY <- 52.8708333333312
  xSeq <- seq(minX+(res/2),maxX-(res/2),length.out=nCol)
  ySeq <- seq(minY+(res/2),maxY-(res/2),length.out=nRow)  
  deltaX <- unique(diff(xSeq))[1] # same to 13 decimal places.
  deltaY <- unique(diff(xSeq))[1] # delta x and y are equal.  
  xCoords <- RotateCw(matrix( xSeq, nrow=nRow, ncol=nCol, byrow=TRUE))
  yCoords <- RotateCw(matrix( rev(ySeq), nrow=nRow, ncol=nCol, byrow=FALSE))
  list(Lon=xCoords, Lat=yCoords, nColNative=nCol, nRowNative=nRow)
}

#' Put the SNODAS coordinates into a netcdf file.
#'
#' \code{PutSnodasCoordsNcdf} Put the output of CalcSnodasCoords into a netcdf file. 
#' @return Success if the filename (which is SNODAS_Coordinates.nc), otherwise NULL.
#' @examples
#' outFile <- PutSnodasCoordsNcdf()
#' ncdump(outFile)
#' unlink(outFile)
#' @keywords IO
#' @concept SNODAS
#' @family SNODAS
#' @export
PutSnodasCoordsNcdf <- function() {
  snodasCoords <- CalcSnodasCoords()
  varList = list()
  varList[[1]] <- list( name='Lon',
                       longname='Longitude',
                       units='Deg East',
                       precision = 'double',
                       missing = -9999,
                       dimensionList =
                       list(
                            x=list(name='Longitude',values=1:snodasCoords$nColNative,
                              units='Degrees East', unlimited=FALSE,
                              create_dimvar=FALSE),
                            y=list(name='Latitude',values=1:snodasCoords$nRowNative,
                              units='Degrees North', unlimited=FALSE,
                              create_dimvar=FALSE)
                            ),
                       data = snodasCoords$Lon ) 

  varList[[2]] <- list( name='Lat',
                       longname='Latitude',
                       units='Deg North',
                       precision = 'double',
                       missing = -9999,
                       dimensionList =
                       list(
                            x=list(name='Longitude',values=1:snodasCoords$nColNative,
                              units='Degrees East', unlimited=FALSE,
                              create_dimvar=FALSE),
                            y=list(name='Latitude',values=1:snodasCoords$nRowNative,
                              units='Degrees North', unlimited=FALSE,
                              create_dimvar=FALSE)
                            ),
                       data = snodasCoords$Lat ) 
    
  globalAttList <- list()
  globalAttList[[1]] <- list(name='Time',value='Timeless', precision="text")
  
  MkNcdf( varList, globalAttList=globalAttList, 
          filename='SNODAS_Coordinates.nc' )
}
#' Pull SNODAS snowdepth and SWE time series for a given lat/lon pair.
#' 
#' \code{GetSnodasPointTs} Pull SWE and snow depth values for a specific
#' point for a given time period.
#' 
#' @param bDatePOSIXct The date in POSIXct format for the beginning of
#'  the time series.
#' @param eDatePOSIXct The date in POSIXct format for the ending of
#'  the time series.
#' @param snodasDir The directory containing SNODAS NetCDF files.
#' @param lat The latitude of the point of interest to pull data for.
#' @param lon The longitude of the point of interest to pull data for.
#' @param quiet logical to print updates to screen. Default is TRUE
#' @return Data frame with time series of SWE and snow depth data.
#' @examples
#' snodasExtracted <- GetSnodasPointTs(as.POSIXct('2014-12-01'),as.POSIXct('2015-04-01'),
#'                                     snodasDir='/d4/karsten/snodas',lat=39.9,
#'                                     lon=-105.1)
#' @keywords IO
#' @concept SNODAS
#' @family SNODAS
#' @export
GetSnodasPointTs <- function(bDatePOSIXct,eDatePOSIXct,snodasDir,lat,lon,quiet=TRUE){
  #First, calculate SNODAS lat/lon coordinates
  snodasCoords <- CalcSnodasCoords()
  latMin <- snodasCoords$Lat[which.min(snodasCoords$Lat)]
  lonMin <- snodasCoords$Lon[which.min(snodasCoords$Lon)]
  latMax <- snodasCoords$Lat[which.max(snodasCoords$Lat)]
  lonMax <- snodasCoords$Lon[which.max(snodasCoords$Lon)]
  res <- 0.00833333333333300
  
  #Sanity check on lat/lon
  if((lat < latMin) | (lat > latMax)){
    warning("Error: Provided latitude should range from 0-90.")
    return(0)
  }
  if((lon < lonMin) | (lon > lonMax)){
    warning("Error: Provided longitude should range from -180.0 to 0.0")
    return(0)
  }
  
  #Perform date analysis to determine time difference between beginning ending dates
  dUnits <- "days"
  diff1 <- difftime(eDatePOSIXct,bDatePOSIXct,units = dUnits)
  nSteps <- diff1 <- as.numeric(diff1)
  dSec <- diff1*24*3600
  dt <- 24*3600
  
  data <- data.frame()
  
  #Calculate SNODAS x,y coordinates using lat/lon pair
  rowInd <- floor((lat - (latMin - (res/2.0)))/res) + 1
  colInd <- floor((lon - (lonMin - (res/2.0)))/res) + 1
  
  for (step in 0:(nSteps - 1)){
    #establish date of current time step
    dCurrent <- bDatePOSIXct + dt*step
    
    if(quiet == FALSE){
        print(paste0('Extracting SNODAS for: ',strftime(dCurrent,"%Y-%m-%d")))
    }
    
    #Establish #SNODAS file name
    snodasFile <- paste0(snodasDir,'/SNODAS_',strftime(dCurrent,"%Y%m%d"),'.nc')
    
    if(!file.exists(snodasFile)){
      warning('Error: SNODAS file: ',snodasFile,' not found.')
      return(0)
    }
    
    #Open NetCDF file
    nc <- ncdf4::nc_open(snodasFile)
    #Sanity check
    varNames <- names(nc$var)
    if((varNames[1] != "SNEQV") & (varNames[2] != "SNOWH")){
      warning("Error: Unexpected variables found in: ",snodasFile)
      return(0)
    }
    
    #Extract fill value
    fillValueSWE <- ncdf4::ncatt_get(nc, varid="SNEQV", attname="_FillValue")
    fillValueSD <- ncdf4::ncatt_get(nc, varid="SNOWH", attname="_FillValue")
    
    if(fillValueSWE[1] != TRUE){
      warning("Error: FillValue for SWE not found.")
      return(0)
    }
    if(fillValueSD[1] != TRUE){
      warning("Error: FillValue for snow depth not found.")
      return(0)
    }
    #Pull data out
    sweData <- ncdf4::ncvar_get(nc, varid="SNEQV", start=c(colInd,rowInd,1), count=c(1,1,1))
    sdData <- ncdf4::ncvar_get(nc, varid="SNOWH", start=c(colInd,rowInd,1), count=c(1,1,1))
    
    #Check for missing values
    if(sweData == fillValueSWE[2]){
      sweData <- NA
    }else{
      #Convert to mm
      sweData <- sweData
    }
    if(sdData == fillValueSD[2]){
      sdData <- NA
    }else{
      #Convert to mm
      sdData <- sdData
    }
    
    #Close NetCDF file
    ncdf4::nc_close(nc)
    
    #Assign values to a temporary data frame
    #Note SNEQV is snow water equivalent and SNOWH is snow depth. This is to 
    #match the LSM output format. 
    dfTemp <- data.frame(POSIXct = dCurrent, SNEQV = sweData, SNOWH = sdData, 
                         units = "mm")
    
    #Merge with existing data frame
    data <- plyr::rbind.fill(data,dfTemp)
  } # end for time loop
  return(data)
} 