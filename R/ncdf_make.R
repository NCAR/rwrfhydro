#' List based creation of netcdf files. 
#'
#' \code{MkNcdf} List based creation of netCDF files. Works for most cases. 
#' Creates netcdf version 4 files by default. Appends to unlimited dimensions
#' in existing files. 
#' 
#' @param varList        List, the variable list. See structure in examples. 
#' @param globalAttList  List, the global attribute list. See structure in examples. 
#' @param filename       Character, the name of the netcdf file to be created. 
#' @param overwrite      Logical, overwrite (i.e. clobber in netcdf parlance) existing 
#' file? Otherwise, the file will be appended if existing and correct (unlimited) dims 
#' are extended. 
#' @param force_v4       Logical, make netcdf version 4 files. 
#' @return Returns the filename invisibly, if successful.
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
#'                           list(  # n.b. the dimension order: charlen,z,y,x,t
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
#' 
#' outFile1 <- path.expand('~/test1.nc')
#' dum <- MkNcdf( varList, globalAttList=globalAttList, filename=outFile1 )
#' ncdump(outFile1)
#' unlink(outFile1)
#' 
#' #Example 2 - append to an existing file's variable.
#' varList1 = list()
#' varList1[[1]] <- list( name='precipMult22',
#'                       longname='Precipitation Multiplier22',
#'                       units='-',
#'                       precision = 'double',
#'                       missing = -9999,
#'                       dimensionList =
#'                           list(  # n.b. the dimension order: charlen,z,y,x,t
#'                                 y=list(name='y',values=2.5+((0:6)*5),
#'                                        units='lat', unlimited=TRUE,
#'                                        create_dimvar=TRUE),
#'                                 x=list(name='x',values=c(10,20),
#'                                        units='lon', unlimited=FALSE,
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
#'                           list(  # n.b. the dimension order: charlen,z,y,x,t
#'                                 y=list(name='y',values=99,
#'                                        units='lat', unlimited=TRUE,
#'                                        create_dimvar=TRUE),
#'                                 x=list(name='x',values=c(10,20),
#'                                        units='lon', unlimited=FALSE,
#'                                        create_dimvar=TRUE)
#'                                ),
#'                      data =  c(1:2)+10 )
#'
#' globalAttList <- list()
#' globalAttList[[1]] <- list(name='Some reall atts',value='#$%^!!', precision="text" )
#' 
#' outFile2 <- path.expand('~/test2.nc')
#' MkNcdf( varList1, globalAttList=globalAttList, 
#'                filename='~/test2.nc', over=TRUE )
#' ncdump(outFile2)
#' unlink(outFile2)
#' @concept ncdf
#' @family ncdf
#' @export
MkNcdf <- function( varList, filename, 
                    globalAttList=NULL,
                    overwrite=FALSE, 
                    force_v4=TRUE ) {   

  names(varList) <- plyr::laply(varList,'[[', 'name')
  
  fileExists <- file.exists(filename)
  
  ## Create/overwrite the file or append to an existing file?
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
      dimList <- plyr::llply( var$dimensionList, doDimDef )
      
      ncdf4::ncvar_def(var$name, var$units, dimList, var$missing,
                       longname=var$longname, prec=var$precision)
    }
    
    defVarList <- plyr::llply(varList, doDefVar)    
    
    ## there's no "noclobber" option to nc_create, so you have to do 
    ## this manually else get an occasional error.
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
  for(varName in names(varList)) {
    var <- varList[[varName]]
    if(append) {
      ## start: at the first non-unlim indices in the next unlimited dim.
      start <- (ncid$var[[var$name]]$varsize * 
                 plyr::laply(ncid$var[[var$name]]$dim,'[[', 'unlim')) + 1
      ## count: how much data was passed in?
      count <- plyr::laply(varList[[var$name]]$dimensionList, 
                           function(dd) length(dd$values))  

      ## have to redefine the dimension?
      varDimInfo <- ncid$var[[var$name]]$dim
      names(varDimInfo) <- plyr::laply(varDimInfo, '[[', 'name')
      unlimDimName <- subset(plyr::ldply(varDimInfo,'[[','unlim'),`[[`)$.id
      ncdf4::ncvar_put( ncid, unlimDimName, 
                        var$dimensionList[[unlimDimName]]$values,
                        start=varDimInfo[[unlimDimName]]$len+1, 
                        count=length(var$dimensionList[[unlimDimName]]$values))
      ncdf4::ncvar_put( ncid, var$name, var$data, 
                        start=start, count=count)
                        #verbose=TRUE)
    } else {
      count <- ncid$var[[var$name]]$varsize
      start <- count*0 + 1
      ncdf4::ncvar_put( ncid, var$name, var$data, start=start, count=count )
    }
   
  }
  
  
  ## global attributes
  if(!is.null(globalAttList)) {
    doPutGlobalAtts <- function( att )
      ncdf4::ncatt_put( ncid, 0, att$name, att$value, prec=att$precision )
    dum <- plyr::llply( globalAttList, doPutGlobalAtts )
  }
  
  ## Close
  ncdf4::nc_close(ncid)
  ## Return the filename for reference.
  invisible(filename)
}  

##=========================================================================================================
#' Emulate ncdump -h on OSX where ncdump might not be availabe. 
#' 
#' I just hacked print.ncdf4 just to make it look more like unix output. 
#' 
#' @param file Character, the file to inspect. 
#' @param variable Character, a variable to return. 
#' @param quiet Logical, suppress the 'meta' dump?
#' @return If variable is not set, the meta object \code{ncdf4::nc_open(file)} is returned. If \code{variable}
#' is set, its values are returned. 
#' @example See \link{\code{MKNcdf}}.
#' @concept ncdf 
#' @family ncdf 
#' @export
ncdump <-function(file, variable, quiet=FALSE) {

  nc <- ncdf4::nc_open(file)
  
  if(!quiet) {
  is_netcdf_v4 = (nc$format == 'NC_FORMAT_NETCDF4')
  is_GMT       = ifelse( nc$is_GMT, ' ( GMT format )', '' )
  is_safemode  = ifelse( nc$safemode, ' ( SAFE MODE ON )', '' )
  
  cat(paste0("File: ", nc$file, "\n( ", nc$format, " )", is_GMT,  is_safemode, ":\n"))
  
  indent <- '    '
  
  cat(paste0("dimensions (",nc$ndims,"):\n"))
  if( nc$ndims > 0 ) {
    for( i in 1:nc$ndims ) {
      if( nc$dim[[i]]$unlim ) {
        cat(paste0(indent,nc$dim[[i]]$name," = UNLIMITED ; // (",nc$dim[[i]]$len,' currently)\n' ))
      } else {
        cat(paste0(indent,nc$dim[[i]]$name," = ",nc$dim[[i]]$len,' ; \n' ))
      }
    }
  }

  totVars <- nc$nvars + sum(plyr::laply(nc$dim, function(dd) dd$create_dimvar))
  cat(paste0("variables (",totVars,"):\n"))
  
  ## dimension variables
  if( nc$ndims > 0 ) {
    for( i in 1:nc$ndims ) {
      atts <- ncdf4::ncatt_get( nc, nc$dim[[i]]$name )
      natts <- length(atts)
      if( natts > 0 ) {
        cat(paste0(indent,typeof(nc$dim[[2]]$vals),' ',nc$dim[[i]]$name,"(",nc$dim[[i]]$name,') ; \n' ))
        nms <- names( atts )
        for( ia in 1:natts ) 
          cat(paste0(indent,indent,nc$dim[[i]]$name,':',nms[ia], ' = "', atts[[ia]], '"\n' ))
      }
    }
  }  
  
  if( nc$nvars > 0 ) {
    for( i in 1:nc$nvars ) {
      nd <- nc$var[[i]]$ndims
      dimstring <- '('
      if( nd > 0 ) {
        for( j in nd:1 ) {
          dimstring <- paste(dimstring,nc$var[[i]]$dim[[j]]$name,sep='')
          if( j > 1 )
            dimstring <- paste(dimstring,',',sep='')
        }
      }
      dimstring <- paste(dimstring,') ',sep='')
      
      chunk_tag = ''
      compress_tag = ''
      if( is_netcdf_v4 ) {
        
        #----------------------------
        # Handle chunking information
        #----------------------------
        if( is.null(nc$var[[i]]$storage) || nc$var[[i]]$storage == 1 )
          chunk_tag = "" #  (Contiguous storage)"
        else
        {
          chunk_tag = "  (Chunking: ["
          for( j in 1:nd ) {
            chunk_tag = paste( chunk_tag, nc$var[[i]]$chunksizes[j], sep='' )
            if( j < nd )
              chunk_tag = paste( chunk_tag, ",", sep='' )
          }
          chunk_tag = paste( chunk_tag, "])", sep='' )
        }
        
        #---------------------------------------
        # Handle shuffle/compression information
        #---------------------------------------
        is_shuffle  = (nc$var[[i]]$shuffle == 1)
        is_compress = (!is.na(nc$var[[i]]$compression))
        if( (!is_shuffle) && (!is_compress))  
          compress_tag = ""
        else if( is_shuffle && (!is_compress))
          compress_tag = "(Compression: shuffle)"
        else if( (!is_shuffle) && is_compress )
          compress_tag = paste("(Compression: level ", nc$var[[i]]$compression, ")", sep='' )
        else
          compress_tag = paste("(Compression: shuffle,level ", nc$var[[i]]$compression, ")", sep='' )
      }
      cat(paste0(indent, nc$var[[i]]$prec, ' ', nc$var[[i]]$name, dimstring, chunk_tag, "  ", compress_tag, ' ; \n' ))
      atts <- ncdf4::ncatt_get( nc, nc$var[[i]]$name )
      natts <- length(atts)
      if( natts > 0 ) {
        nms <- names( atts )
        for( ia in 1:natts ) 
          cat(paste0(indent,indent,nc$var[[i]]$name,':',nms[ia], ' = "', atts[[ia]], '" ;\n' ))
      }
    }
  }
  
  
  #--------------------------
  # Now get global attributes
  #--------------------------
  atts <- ncdf4::ncatt_get( nc, 0 )
  natts <- length(atts)
  if( natts > 0 ) {
    cat(paste0('\n// global attributes (',natts,'):\n'))
    nms <- names( atts )
    for( ia in 1:natts ) 
      cat(paste0(indent,':',nms[ia], ' = "', atts[[ia]], '"\n' ))
  }
  } ## !quiet
  
  ret <- if(!missing(variable)) ncdf4::ncvar_get(nc,variable) else nc
  
  ncdf4::nc_close(nc)
  invisible(ret)
}


