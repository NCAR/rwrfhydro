
## Possible approaches
## 1. Take LDASIN file and list of variables + operator + array values (integer? w add_offset and scale_fctor?)
## Could also do this with a geogrid file.

##' Create modle forcing adjustment files using LDASIN file.
##'
##' \code{MkFrcAdjFromLdasin} Takes an list (see examples) and creates at netcdf file
##' with a specified name which can be used to perturb WRF-Hydro forcing variables.
##' 
##' @param frcAdjL        List, the variable list. See structure in examples. 
##' @param file           character, the path/filename.nc of the netcdf file to be written.
##' @return Returns the filename invisibly, if successful.
##' @examples
##' \dontrun{
##' # Example 1 - 1 variable
##' theFile <- '/glade/p/work/jamesmcc/WRF_Hydro/domains/tuolumne/FORCING/2007100100.LDASIN_DOMAIN1'
##' ## Note: I've Abandoned compression with scale factor and add offset for the time being.
##' frcAdjList <-
##'   list(LDASIN=theFile,
##'        RAINRATE=list(variable='RAINRATE',
##'                      operator=tolower('x'),
##'                      data=2,  ## could be an array
##'                      ##scale_factor=.001, ## discouraged due to model uncompression issues
##'                      ##add_offset=4.0,    ##  "  "
##'                      upperBound=0.014,    ## the limit imposed in the model = 500mm/hr = .1388889
##'                      lowerBound=0.0
##'                     )
##'       )
##' ## Notes: high rain rates crash the model. 500mm/hr is the limit in the model
##' ##        but uniform rain of lesser magnitudes can crash the model.
##' OpTransform <- c(`+`='.add', `x`='.mult')
##' outFile <- paste0('/glade/p/work/jamesmcc/forcing_spatial_adj/test.frcAdj/',
##'                   'frcAdj',
##'                   OpTransform[frcAdjList$RAINRATE$operator],
##'                   '.data.',  frcAdjList$RAINRATE$data,
##'                   '.scale.',frcAdjList$RAINRATE$scale_factor,
##'                   '.add.',  frcAdjList$RAINRATE$add_offset,
##'                   '.upBnd.', frcAdjList$RAINRATE$upperBound,
##'                   '.loBnd.', frcAdjList$RAINRATE$lowerBound,
##'                   '.nc')
##' unlink(outFile)
##' zz <- MkFrcAdjFromLdasin(frcAdjList, outFile)
##' theData <- GetNcdfFile(zz)
##' summary(as.vector(theData[[1]]))
##' }
##' @concept ncdf
##' @family ncdf
##' @export
MkFrcAdjFromLdasin <- function(frcAdjL, file){
  whFile <- which(names(frcAdjL)=='LDASIN')
  if(!length(whFile)) {
    warning('No LDASIN variable specifying the file')
    return(invisible(NULL))
  }
  ldasinFile <- frcAdjL[[whFile]]
  if(!file.exists(ldasinFile)){
    warning('The LDASIN file does NOT exist')
    return(invisible(NULL))
  }
  
  ldasinFile <- frcAdjL[[whFile]]
  theNcdump <- ncdump(ldasinFile, q=TRUE)
  
  MkVarListFromLdasin <- function(ll, theFile) {

    outList <-
      list(name     = ll$variable,
           operator = tolower(ll$operator),
           dimensionList = list(  # n.b. the dimension order: charlen,z,y,x,t
             south_north=list(name='south_north',
                              values=1:theNcdump$dim$south_north$len,
                              unlimited=FALSE,
                              create_dimvar=FALSE),
             west_east=list(name='west_east',
                              values=1:theNcdump$dim$west_east$len,
                              unlimited=FALSE,
                              create_dimvar=FALSE)
             ## TIME?
             ))


    data = ncdump(theFile, ll$variable, q=TRUE)*0 + ll$data## just get the dims
    if(length(ll$add_offset))    {
      data <- data-ll$add_offset
      outList$add_offset <- ll$add_offset
    }
    if(length(ll$scale_factor))  {
      data <- data/ll$scale_factor
      outList$scale_factor <- ll$scale_factor
    }

    outList$precision <- ll$precision
    if(length(ll$add_offset) | length(ll$scale_factor))  {
      outList$data <- as.integer(data)
      #outList$precision <- 'integer'
    } else {
      outList$data <- data
      #outList$precision <- ll$precision
    }
      
    ## add optional bounds
    if(length(ll$upperBound)) outList$upperBound <- ll$upperBound
    if(length(ll$lowerBound)) outList$lowerBound <- ll$lowerBound
    ## optional units & longname
    outList$units <- if(length(ll$units)) ll$units else '-'
    outList$longname <- if(length(ll$longname)) ll$longname else '-'
    outList
  }

  mkNcdfList <- 
    plyr::llply(frcAdjL[-whFile], MkVarListFromLdasin, theFile=ldasinFile)
  
  dum <- MkNcdf( mkNcdfList, filename=file )
}  


