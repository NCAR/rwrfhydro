#' GetSnodasDepthSweDate
#'
#' \code{GetSnodasDepthSweDate} Get and unpack the SNODAS snow depth and SWE tarball for a given date. 
#' 
#' @param datePOSIXct The date in POSIXct format for which data is desired. Only whole days matter. 
#' @param outputDir   The directory where the data are to be archived. This directory is checked to see if the
#'                    data exist when overwrite=FALSE
#' @param overwrite   When false: If the depth and SWE files exist on disk, dont grab the tarball. \cr
#'                    When false: If the tarball exists on disk but depth and SWE files dont, just unpack the tarball. \cr
#'                    When true: Pull new tarball and overwrite any existing files with the same date.
#' @param quiet       Passed to curl, to show it's progress (typcially too fast to matter).
#' @return Success or failure. 
#' @examples
#' snodasGot <- GetSnodasDepthSweDate(as.POSIXct('2015-02-28'))
#' @export
GetSnodasDepthSweDate <- function(datePOSIXct, outputDir='.', overwrite=FALSE, quiet=TRUE) {
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
      return(0)
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
  1
}


#' ReadSnodasDepthSweDate
#'
#' \code{ReadSnodasDepthSweDate} Read (from local disk into memory) the SNODAS snow depth and SWE for a given date. 
#' 
#' @param datePOSIXct The date in POSIXct format for which data is desired. Only whole days matter. 
#' @param outputDir         The directory where the data are archived.
#' @return A list with a SWE and a depth matrix. 
#' @examples
#' snodasGot <- GetSnodasDepthSweDate(as.POSIXct('2015-02-28'))
#' if(snodasGot) snodasList <- ReadSnodasDepthSweDate(as.POSIXct('2015-02-28'))
#' @export
ReadSnodasDepthSweDate <- function(datePOSIXct, outputDir='.') {
  # date parameters
  yy <- format(datePOSIXct, c("%Y")); mm <- format(datePOSIXct, c("%m"))
  mon <- format(datePOSIXct, c("%h")); dd <- format(datePOSIXct, c("%d"))
 
  # depthProdId <- '1036',  sweProdId <- '1034'
  depthFile0<- paste0(outputDir,'/','us_ssmv1',
                      '1036tS__T0001TTNATS',yy,mm,dd,'05HP001.dat.gz')
  sweFile0  <- paste0(outputDir,'/','us_ssmv1',
                      '1034tS__T0001TTNATS',yy,mm,dd,'05HP001.dat.gz')

  # SNODAS spatial reference and scaling
  nCol <- 6935
  nRow <- 3351 #columns and rows number:masked version of contiguous US
  dataScaleFactor  <-  1000  #multiply the data this amount, both depth and SWE

  depthCon <- gzcon(file(depthFile0, "rb"))
  depthData <- readBin(depthCon, integer(), n=nRow*nCol, size=2,
                       signed=TRUE, endian='big')/dataScaleFactor
  close(depthCon)
  
  sweCon <- gzcon(file(sweFile0, "rb"))
  sweData <- readBin(sweCon, integer(), n=nRow*nCol, size=2,
                     signed=TRUE, endian='big')/dataScaleFactor
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

  list(datePOSIXct=datePOSIXct,
       depth.mm= RotateCw(matrix(depthData, ncol=nCol, nrow=nRow, byrow=TRUE)),
       swe.mm  = RotateCw(matrix(sweData,   ncol=nCol, nrow=nRow, byrow=TRUE)) ) 
}


#' PutSnodasNcdf
#'
#' \code{PutSnodasNcdf} Put the output of ReadSnodasDepthSweDate into a netcdf file. 
#' 
#' @param snodasList The output of ReadSnodasDepthSweDate. 
#' @return Success if the filename which is (SNODAS_YYYYMMDD.nc), otherwise NULL.
#' @examples
#' snodasGot <- GetSnodasDepthSweDate(as.POSIXct('2015-02-28'))
#' if(snodasGot) snodasList <- ReadSnodasDepthSweDate(as.POSIXct('2015-02-28'))
#' PutSnodasNcdf(snodasList)
#' @export
PutSnodasNcdf <- function(snodasList) {
  ## make it a vanilla date... 
  theDate <- as.POSIXct(format(snodasList$datePOSIXct,'%Y-%m-%d'),'UTC')
  varList = list()
  varList[[1]] <- list( name='SWE',
                       longname='Snow water equivalent',
                       units='mm',
                       precision = 'double',
                       missing = min(snodasList$swe.mm),
                       dimensionList =
                       list(
                            y=list(name='Latitude',values=1:nrow(snodasList$swe.mm),
                              units='Degrees North', unlimited=FALSE,
                              create_dimvar=FALSE),
                            x=list(name='Longitude',values=1:ncol(snodasList$swe.mm),
                              units='Degrees East', unlimited=FALSE,
                              create_dimvar=FALSE),
                            t=list(name='Time',values=as.numeric(theDate),
                              units='POSIXct', unlimited=TRUE,
                              create_dimvar=TRUE)
                            ),
                       data = snodasList$swe.mm ) 

  varList[[2]] <- list( name='snowDepth',
                       longname='Snow depth',
                       units='mm',
                       precision = 'double',
                       missing = min(snodasList$depth.mm),
                       dimensionList =
                       list(
                            y=list(name='Latitude',values=1:nrow(snodasList$depth.mm),
                              units='Degrees North', unlimited=FALSE,
                              create_dimvar=FALSE),
                            x=list(name='Longitude',values=1:ncol(snodasList$depth.mm),
                              units='Degrees East', unlimited=FALSE,
                              create_dimvar=FALSE),
                            t=list(name='Time',values=as.numeric(theDate),
                              units='POSIXct', unlimited=TRUE,
                              create_dimvar=TRUE)
                            ),
                       data = snodasList$depth.mm )

  globalAttList <- list()
  globalAttList[[1]] <- list(name='Time',value='2012-07-05_00:00:00', precision="text")
  globalAttList[[2]] <- list(name='POSIXct Origin',value='1970-01-01 00:00.00 UTC', precision="text")
  
  outFile <- paste0('SNODAS_',format(snodasList$datePOSIXct,'%Y%m%d'),'.nc')
  MkNcdf( varList, globalAttList, outFile )
}  


#' CalcSnodasCoords
#'
#' \code{CalcSnodasCoords} Get and unpack the SNODAS snow depth and SWE tarball for a given date. 
#' 
#' @return A list of lon and lat
#' @examples
#' snodasCoords <- CalcSnodasCoords()
#' @export
CalcSnodasCoords <- function() {
  nCol=6935
  nRow=3351 #columns and rows number:masked version of contiguous US
  minX= -124.733749999999
  maxX= -66.9420833333342
  minY= 24.9495833333335
  maxY= 52.8745833333323 
  res <- 0.00833333333333300
  benchX <- -124.729583333332
  benchY <- 52.8704166666657  
  xSeq <- seq(minX+(res/2),maxX-(res/2),length.out=nCol)
  ySeq <- seq(minY+(res/2),maxY-(res/2),length.out=nRow)  
  deltaX <- unique(diff(xSeq))[1] # same to 13 decimal places.
  deltaY <- unique(diff(xSeq))[1] # delta x and y are equal.  
  xCoords <- RotateCw(matrix( xSeq, nrow=nRow, ncol=nCol, byrow=TRUE))
  yCoords <- RotateCw(matrix( rev(ySeq), nrow=nRow, ncol=nCol, byrow=FALSE))
  list(Lon=xCoords, Lat=yCoords)
}

#' PutSnodasCoordsNcdf
#'
#' \code{PutSnodasCoordsNcdf} Put the output of CalcSnodasCoords into a netcdf file. 
#' @return Success if the filename (which is SNODAS_Coordinates.nc), otherwise NULL.
#' @examples
#' PutSnodasCoordsNcdf()
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
                            y=list(name='Latitude',values=1:nrow(snodasCoords$Lon),
                              units='Degrees North', unlimited=FALSE,
                              create_dimvar=FALSE),
                            x=list(name='Longitude',values=1:ncol(snodasCoords$Lon),
                              units='Degrees East', unlimited=FALSE,
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
                            y=list(name='Latitude',values=1:nrow(snodasCoords$Lat),
                              units='Degrees North', unlimited=FALSE,
                              create_dimvar=FALSE),
                            x=list(name='Longitude',values=1:ncol(snodasCoords$Lat),
                              units='Degrees East', unlimited=FALSE,
                              create_dimvar=FALSE)
                            ),
                       data = snodasCoords$Lat ) 
    
  globalAttList <- list()
  globalAttList[[1]] <- list(name='Time',value='Timeless', precision="text")
  
  MkNcdf( varList, globalAttList, 'SNODAS_Coordinates.nc' )
}
