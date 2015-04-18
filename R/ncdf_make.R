#' List based creation of netcdf files. 
#'
#' \code{MkNcdf} List based creation of netCDF files. Works for most cases. 
#' Creates netcdf version 4 files by default. 
#' 
#' @param varList        The variable list. See structure in examples. 
#' @param globalAttList  The global attribute list. See structure in examples. 
#' @param filename       The name of the netcdf file to be created. 
#' @return Returns the filename if successful.
#' @examples
#' # Example 1 - Basic write. 
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
#' dum <- MkNcdf( varList, globalAttList, '~/test1.nc' )
#' 
#' #Example 2 - append to an existing file's variable.
#' varList1 = list()
#' varList1[[1]] <- list( name='precipMult22',
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
#'                                        units='lon', unlimited=TRUE,
#'                                        create_dimvar=TRUE)
#'                                ),
#'                      data = matrix( 1:7, nrow=7, ncol=2 ) )
#' 
#' varList2 = list()
#' varList2[[1]] <- list( name='precipMult22',
#'                       longname='Precipitation Multiplier22',
#'                       units='-',
#'                       precision = 'double',
#'                       missing = -9999,
#'                       dimensionList =
#'                           list(  # n.b. the dimension order: z,y,x,t
#'                                 y=list(name='y',values=2.5+((0:6)*5),
#'                                        units='lat', unlimited=FALSE,
#'                                        create_dimvar=TRUE),
#'                                 x=list(name='x',values=c(30),
#'                                        units='lon', unlimited=TRUE,
#'                                        create_dimvar=TRUE)
#'                                ),
#'                      data = matrix( 11:17, nrow=7, ncol=1 ) )
#'
#' globalAttList <- list()
#' globalAttList[[1]] <- list(name='Some reall atts',value='#$%^!!', precision="text" )
#' 
#' dum <- MkNcdf( varList1, globalAttList, '~/test2.nc', over=TRUE )
#' dum <- MkNcdf( varList2, globalAttList, '~/test2.nc' )
#' 
#' @export
MkNcdf <- function( varList, globalAttList, filename, overwrite=FALSE, 
                    force_v4=TRUE ) {   

  fileExists <- file.exists(filename)
  names(varList) <- plyr::laply(varList,'[[', 'name')
  
  if(!fileExists | overwrite) {
  
    append <- FALSE
    
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
    
    ## there's no "noclobber" option to nc_create, so you have to do 
    ## this manually else get an occasional errlr
    if(fileExists) unlink(filename)
    ncid <- ncdf4::nc_create(filename, defVarList, force_v4=force_v4)  
  
  } else {  #above is starting from scratch, below is appending
      
    append <- TRUE
    ncid <- ncdf4::nc_open(filename, write=TRUE, readunlim=TRUE)  
    
    ## for unlimdims
    newVarsUnlim <- plyr::laply(names(varList), 
                                function(vv) ncid$var[[vv]]$unlim)
    if(!all(newVarsUnlim)) 
      warning(paste0('Some variables to append to ', filename,
                     '\n do not have unlimited dimensions in the original file.'),
              immediate. = TRUE)    
    
  }

  ## Put the vars into the file
  doPutVar <- function(var) {
    if(append) {
      start <- ncid$var[[var$name]]$varsize + plyr::laply(ncid$var[[var$name]]$dim,'[[', 'unlim')
      count <- 
        #plyr::laply(varList[[var$name]]$dimensionList, function(dd) dd$unlimited) *     ##unlimited?
        plyr::laply(varList[[var$name]]$dimensionList, function(dd) length(dd$values))  #+ ##length of new data
        #!(plyr::laply(varList[[var$name]]$dimensionList, function(dd) dd$unlimited) )    ##need a 1 for limited dimensions
    } else {
      count <- ncid$var[[var$name]]$varsize
      start <- count*0 + 1
    }
    ncdf4::ncvar_put( ncid, var$name, var$data, start=start, count=count )
  }
  dum <- plyr::llply( varList, doPutVar )
  
  ## global attributes
  doPutGlobalAtts <- function( att )
    ncdf4::ncatt_put( ncid, 0, att$name, att$value, prec=att$precision )
  dum <- plyr::llply( globalAttList, doPutGlobalAtts )
  
  ## Close
  ncdf4::nc_close(ncid)
  ## Return the filename for reference.
  filename
}  
