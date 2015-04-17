#' List based creation of netcdf files. 
#'
#' \code{MkNcdf} List based creation of netCDF files. Works for most cases. 
#' 
#' @param varList        The variable list. See structure in examples. 
#' @param globalAttList  The global attribute list. See structure in examples. 
#' @param filename       The name of the netcdf file to be created. 
#' @return Returns the filename if successful.
#' @examples
#' # Example 1
#' varList = list()
#' varList[[1]] <- list( name='precipMult',
#'                       longname='Precipitation Multiplier',
#'                       units='-',
#'                       precision = 'double',
#'                       missing = -9999,
#'                       dimensionList = list(scalar=list(name='scalar',values=1,
#'                                              units='-', unlimited=FALSE,
#'                                              create_dimvar=FALSE)),
#'                       data = 1:1 )
#' varList[[2]] <- list( name='precipMult22',
#'                       longname='Precipitation Multiplier22',
#'                       units='-',
#'                       precision = 'double',
#'                       missing = -9999,
#'                       dimensionList =
#'                           list(  # n.b. the dimension order: z,y,x,t
#'                                 y=list(name='y',values=2.5+((0:6)*5),
#'                                        units='lat', unlimited=FALSE,
#'                                        create_dimvar=TRUE),
#'                                 x=list(name='x',values=c(10,20),
#'                                        units='lon', unlimited=FALSE,
#'                                        create_dimvar=TRUE)
#'                                ),
#'                      data = matrix( 1:7, nrow=7, ncol=2 ) )
#' globalAttList <- list()
#' globalAttList[[1]] <- list(name='Restart_Time',value='2012-07-05_00:00:00', precision="text")
#' globalAttList[[2]] <- list(name='Some reall atts',value='#$%^!!', precision="text" )
#' dum <- MkNcdf( varList, globalAttList, 'test3.nc' )
#' @export
MkNcdf <- function( varList, globalAttList, filename ) {
  ## Define all the variables with their proper dimensions.
  doDefVar <- function( var ) { ## var is a list
    ## Handle the dimensions. 
    doDimDef <- function(dim) {
      if(!dim$create_dimvar) {
        dim$units=''  ## required if this condition.
        dim$values=1:length(dim$values)          
      }
      ncdf4::ncdim_def(name=dim$name, units=dim$units,
                       vals=dim$values, unlim=dim$unlim,
                       create_dimvar=dim$create_dimvar)
    }
    dimList <- plyr::llply( var$dimensionList, doDimDef)

    ncdf4::ncvar_def(var$name, var$units, dimList, var$missing,
                     longname=var$longname, prec=var$precision)
  }
  defVarList <- plyr::llply(varList, doDefVar)
   
  ## Create a netCDF file with this variable
  ncnew <- ncdf4::nc_create( filename, defVarList )  

  ## Put the vars into the file
  doPutVar <- function(var)
    ncdf4::ncvar_put( ncnew, var$name, var$data) #, start=1, count=1 )
  dum <- plyr::llply( varList, doPutVar )
  
  ## global attributes
  doPutGlobalAtts <- function( att )
    ncdf4::ncatt_put( ncnew, 0, att$name, att$value, prec=att$precision )
  dum <- plyr::llply( globalAttList, doPutGlobalAtts )
  
  ## Close
  ncdf4::nc_close(ncnew)
  ## Return the filename for reference.
  filename
}  
