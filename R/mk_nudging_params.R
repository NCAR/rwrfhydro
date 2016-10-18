##' Create a parameter file for nudging.
##' 
##' @param gageId Character vector of gage identifiers also used in the timeslice files.
##' @param R Numeric vector same length as gageId which describes the radius of influence at each gage.
##' @param G Numeric vector same length as gageId which describes the nudging amplitude at each gage.
##' @param tau Numeric vector same length as gageId which describes the size of the temporal half-window in minutes.
##' @param qThresh Numeric [length(gageId), 12, nThresh] threshhold of flow for acf selection.
##' @param expCoeff     Numeric [length(gageId), 12, nThresh+1] actually the denominator in exp(-y/b).
##' @param outFile Character path/file to the desired output file
##' @param overwrite Logical, overwrite outFile if it already exists?
##' @param rmBlankGages Take out gages where the name is blank?
##' @examples 
##' \dontrun{
##'   ## Once and future CONUS example snippett
##'   gageParams <- read.csv('~/ncar/WRF_Hydro/DOMAIN_library/CONUS/nhdRtIntersect.csv',
##'                          colClasses = 'character' )
##'                          
##'   ## Boulder creek domain PMO example
##'   gageParams <- data.frame( gageId = paste0('g',formatC(1:403,width=3,flag='0')), stringsAsFactors=FALSE)
##'   gageParams$R=0
##'   gageParams$G=1
##'   gageParams$tau=20
##'   MkNudgingParams(gageId=gageParams$gageId, R=gageParams$R, 
##'                   G=gageParams$G, tau=gageParams$tau, 
##'                   outFile='~/WRF_Hydro/Col_Bldr_Creek/PMO/nudgingParams.PMOallGages.nc', 
##'                   overwrite=TRUE)
##' # FRNG example
##' ## The existing params file
##' if(FALSE) {
##' devtools::load_all()
##' ## existing parameter file to change/extend
##' paramPath <- '~/WRF_Hydro/FRNG_NHD/RUN/prstTestFeb2016/DOMAIN/'
##' frnParams <- paste0(paramPath,'nudgingParams.rwValid5.nc')
##' gageParamId <- ncdump(frnParams,'stationId',q=TRUE)
##' gageParamR <- ncdump(frnParams,'R',q=TRUE)
##' gageParamG <- ncdump(frnParams,'G',q=TRUE)
##' gageParamTau <- ncdump(frnParams,'tau',q=TRUE)
##' nGages=length(gageParamId)
##'
##' ## the existing parameter file had blanks, remove them.
##' options(warn=1)
##' MkNudgingParams(gageId=gageParamId, R=gageParamR,
##' G=gageParamG, tau=gageParamTau,
##' outFile=paste0(paramPath,'nudgingParams.rwValid5.rmBlanks.nc'),
##' overwrite=TRUE, rmBlankGages=TRUE)
##'
##'
##' ## Uniform tiny coefficient, simulate not having these parameters.
##' nGages <- length(gageParamId)
##' gageParamQThresh1 <- array(rep(c(1000000),each=nGages*12),dim=c(nGages,12,1))
##' gageExpCoeff1 <- array(rep(c(1e-38),each=nGages*12),dim=c(nGages,12,2))
##' print(
##' MkNudgingParams(gageId=gageParamId, R=gageParamR,
##' G=gageParamG, tau=gageParamTau,
##' qThresh=gageParamQThresh1,
##' expCoeff=gageExpCoeff1,
##' outFile=paste0(paramPath,'nudgingParams.rwValid5.rmBlanksPrstTinyExp.nc'),
##' overwrite=TRUE, rmBlankGages=TRUE)
##' )
##'
##' ## In this example the exponent only depends on threshold, not location nor month.
##' ## The first threshold is negative, so only coefficients for the second threshold index should be
##' ## applied and in this case will be tiny, so again have no effect.
##' nGages <- length(whGages <- which(trimws(gageParamId) != '' ))
##' #nGages <- length(gageParamId)
##' gageParamQThresh1 <- array(c(-100),dim=c(nGages,12,1))
##' ## *** THIS IS THE CORRECT WAY OF FILLING THE ARRAY : KISS ***
##' gageExpCoeff1 <- array(c(1e-38,120),dim=c(nGages,12,2))
##' print(
##' MkNudgingParams(gageId=gageParamId[whGages], R=gageParamR[whGages],
##' G=gageParamG[whGages], tau=gageParamTau[whGages],
##' qThresh=gageParamQThresh1,
##' expCoeff=gageExpCoeff1,
##' outFile=paste0(paramPath,'nudgingParams.rwValid5.rmBlanksPrst1ThreshMed.nc'),
##' overwrite=TRUE, rmBlankGages=TRUE)
##' )
##' ## CONUS
##' ## In this example the exponent only depends on threshold, not location nor month.
##' ## The first threshold is negative, so only coefficients for the second threshold index should be
##' ## applied and in this case will be tiny, so again have no effect.
##' paramPath <- '~/WRF_Hydro/TESTING/TEST_FILES/CONUS/2015-12-04_20:08:06.b8e1e01c4cc2/STD/NUDGING/'
##' frnParams <- paste0(paramPath,'nudgingParams.nc')
##' gageParamId <- ncdump(frnParams,'stationId',q=TRUE)
##' gageParamR <- ncdump(frnParams,'R',q=TRUE)
##' gageParamG <- ncdump(frnParams,'G',q=TRUE)
##' gageParamTau <- ncdump(frnParams,'tau',q=TRUE)
##' nGages=length(gageParamId)
##'
##' rmGages <- c("       05059500", "       05054000", "       05082500", "       10108400",
##' "       09183600", "       08387550", "       08158930", "       06470500",
##' "       06768000", "       05427943", "       05551675", "       03298135",
##' "       03352953", "       07083200", "       07358284", "       02458450",
##' "       02310678", "       02302010", "       02237700", "       02299472",
##' "     0212467595", "     0214676115", "       02011460", "       01446776",
##' "       12306500", "       11119750", "       10257549", "       11122010",
##' "       11276600", formatC('', width=15) )
##'
##' nGages <- length(whGages <- which(!(gageParamId %in% rmGages)))
##' #nGages <- length(gageParamId)
##' gageParamQThresh1 <- array(c(-100),dim=c(nGages,12,1))
##'
##' ## *** THIS IS THE CORRECT WAY OF FILLING THE ARRAY : KISS ***
##' gageExpCoeff1 <- array(c(1e-38,120),dim=c(nGages,12,2))
##' print(
##' MkNudgingParams(gageId=gageParamId[whGages], R=gageParamR[whGages],
##' G=gageParamG[whGages], tau=gageParamTau[whGages],
##' qThresh=gageParamQThresh1,
##' expCoeff=gageExpCoeff1,
##' outFile=paste0(paramPath,'nudgingParams.conusPstActive.nc'),
##' overwrite=TRUE, rmBlankGages=TRUE)
##' )
##'
##'
##' #array(rep(c(.5,.8),each=4*12),dim=c(4,12,2))
##' }
##' 
##' ## the existing parameter file had blanks, remove them.
##' options(warn=1)
##' MkNudgingParams(gageId=gageParamId, R=gageParamR, 
##'                 G=gageParamG, tau=gageParamTau, 
##'                 outFile=paste0(paramPath,'nudgingParams.rwValid5.rmBlanks.nc'),
##'                 overwrite=TRUE, rmBlankGages=TRUE)
##' 
##' 
##' ## In this example the exponent only depends on threshold, not location nor month.
##' gageParamqThresh1 = array(rep(c(.5,.8),each=4*12),dim=c(4,12,2))
##' 
##' MkNudgingParams(gageId=gageParams$gageId, R=gageParams$R, 
##'                 G=gageParams$G, tau=gageParams$tau, 
##'                 outFile=paste0(paramPath,'nudgingParams.rwValid5.persistence.nc'),
##'                 overwrite=TRUE, rmBlankGages=TRUE)
##' }  #dontrun
##' @keywords manip IO
##' @concept dataMgmt nudging
##' @family nudging
##' @export
MkNudgingParams <- function(gageId, R, G, tau,
                            qThresh=NULL,
                            expCoeff=NULL,
                            outFile, overwrite=FALSE,
                            rmBlankGages=TRUE) {

  if(file.exists(outFile) & !overwrite) {
    warning(paste0('File ',outFile,' exists and overwrite=FALSE, returning.'))
    return('')
  }
  
  # A constructor, sorta. easy way to check the inputs.
  gageId <- formatC(gageId, width=15)
  gageParams <- data.frame(gageId=gageId, R=R, G=G, tau=tau, stringsAsFactors = FALSE)
  
  whBlank <- which(trimws(gageParams$gageId)=='')
  if(length(whBlank)) {
    warning("Blanks found in the input gage list")
    if(rmBlankGages) {
      warning('Removing blank gages from entries of all variables')
      gageParams <- gageParams[-whBlank,]
      if(!is.null(qThresh) | !is.null(expCoeff)) {
        qThresh   <-  qThresh[-whBlank, , , drop=FALSE]
        expCoeff  <- expCoeff[-whBlank, , , drop=FALSE]
      }
    }
  }
  
  ## need to set the missing value used by ncdf4? i think it's NA by default
  dimensionList <-
    list(  # n.b. the dimension order: z,y,x,t
         stationIdInd=list(name='stationIdInd',
                           units='', 
                           values=1:nrow(gageParams),
                           unlimited=TRUE,
                           create_dimvar=FALSE),
      
         stationIdStrLen=list(name='stationIdStrLen',
                              units='', 
                              values=1:15,
                              unlimited=FALSE,
                              create_dimvar=FALSE)
         )
  
  if(!is.null(qThresh) | !is.null(expCoeff)) {
    if(is.null(qThresh) | is.null(expCoeff)) {
      warning("Both thresh and expCoeff must be defined if one of them is. Returning")
      return(NULL)
    }
    if(!all((dim(qThresh)+c(0,0,1)) == dim(expCoeff) )) {
      warning("Variables qThresh and expCoeff must have the same dimensions. Returning")
      return(NULL)
    }
    if(dim(expCoeff)[1] != nrow(gageParams)) {
      warning("First dimension of expCoeff & qThresh must equal length of gageId")
      if(length(whBlank) & rmBlankGages) {
        warning(paste0('Removing blank gages may have resulted in dimension ',
                       'problems with qThresh and expCoeff.'))
        return(NULL)
      }
    }
    if(dim(expCoeff)[2] != 12){
      warning("Second dimension of expCoeff & qThresh must be of length 12 (months).")
      return(NULL)
    }
      
    dimensionList[[3]] <- list(name='monthInd',
                             units='month', 
                             values=1:12,
                             unlimited=FALSE,
                             create_dimvar=FALSE)
    
    dimensionList[[4]] <- list(name='threshInd',
                               units='m^3/s', 
                               values=1:(dim(qThresh)[3]),
                               unlimited=FALSE,
                               create_dimvar=FALSE)

    dimensionList[[5]] <- list(name='threshCatInd',
                               units='m^3/s', 
                               values=1:(dim(expCoeff)[3]),
                               unlimited=FALSE,
                               create_dimvar=FALSE)

    names(dimensionList)[c(3,4,5)] <- c('monthInd','threshInd','threshCatInd')
  }

  
  varList = list()
  varList[[1]] <- 
    list( name='stationId',
          longname='USGS station identifer',
          units='-',
          precision = 'char',
          #missing = ,
          dimensionList=dimensionList[c('stationIdStrLen','stationIdInd')],
          data = gageParams$gageId )
  
  varList[[2]] <- 
    list( name='R',
          longname='Radius of influence in meters',
          units='meters',
          precision = 'float',
          #missing = ,
          dimensionList=dimensionList[c('stationIdInd')],
          data = gageParams$R )
  
  varList[[3]] <- 
    list( name='G',
          longname='Amplitude of nudging',
          units='-',
          precision = 'float',
          #missing = ,
          dimensionList=dimensionList[c('stationIdInd')],
          data = gageParams$G )
  
  varList[[4]] <- 
    list( name='tau',
          longname='Time tapering parameter half window size in minutes',
          units='minutes',
          precision = 'float',
          #missing = ,
          dimensionList=dimensionList[c('stationIdInd')],
          data = gageParams$tau )

  if(!is.null(qThresh) | !is.null(expCoeff)) {

    varList[[5]] <- 
      list( name='qThresh',
           longname='Discharge threshold category',
           units='m^3/s',
           precision = 'float',
           ##missing = ,
           dimensionList=dimensionList[c('threshInd','monthInd','stationIdInd')],
           #dimensionList=dimensionList[c('stationIdInd','monthInd','threshInd')],
           data = qThresh )

    varList[[6]] <- 
      list( name='expCoeff',
           longname='Coefficient b in denominator e^(-dt/b)',
           units='minutes',
           precision = 'float',
           ##missing = ,
           dimensionList=dimensionList[c('threshCatInd','monthInd','stationIdInd')],
           #dimensionList=dimensionList[c('stationIdInd','monthInd','threshCatInd')],
           data = expCoeff )
    
  }

  MkNcdf(varList, filename=outFile, overwrite=overwrite)
  
  ncdump(outFile)
  
  invisible(outFile)
}
