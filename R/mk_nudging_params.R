#' Create a parameter file for nudging.
#' 
#' @param gageId Character vector of gage identifiers also used in the timeslice files.
#' @param R Numeric vector same length as gageId which describes the radius of influence at each gage.
#' @param G Numeric vector same length as gageId which describes the nudging amplitude at each gage.
#' @param tau Numeric vector same length as gageId which describes the size of the temporal half-window in minutes.
#' @param outFile Character path/file to the desired output file
#' @param overwrite Logical, overwrite outFile if it already exists?
#' @examples 
#' \dontrun{
#'   ## once and future CONUS example snippett
#'   gageParams <- read.csv('~/ncar/WRF_Hydro/DOMAIN_library/CONUS/nhdRtIntersect.csv',
#'                          colClasses = 'character' )
#'                          
#'   ## Boulder creek domain PMO example
#'   gageParams <- data.frame( gageId = paste0('g',formatC(1:403,width=3,flag='0')), stringsAsFactors=FALSE)
#'   gageParams$R=0
#'   gageParams$G=1
#'   gageParams$tau=20
#'   MkNudgingParams(gageId=gageParams$gageId, R=gageParams$R, 
#'                   G=gageParams$G, tau=gageParams$tau, 
#'                   outFile='~/WRF_Hydro/Col_Bldr_Creek/PMO/nudgingParams.PMOallGages.nc', 
#'                   overwrite=TRUE)
#' }
#' @keywords manip IO
#' @concept dataMgmt nudging
#' @family nudging
#' @export
MkNudgingParams <- function(gageId, R, G, tau, outFile, overwrite=FALSE) {

  if(file.exists(outFile) & !overwrite) {
    warning(paste0('File ',outFile,' exists and overwrite=FALSE, returning.'))
    return('')
  }
  
  # A constructor, sorta. easy way to check the inputs.
  gageId <- formatC(gageId, width=15)
  gageParams <- data.frame(gageId=gageId, R=R, G=G, tau=tau, stringsAsFactors = FALSE) 
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
  
  MkNcdf(varList, filename=outFile, overwrite=overwrite)
  
  ncdump(outFile)
  
  invisible(outFile)
}
