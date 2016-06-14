##=========================================================================================================
#' Get variables from a ncdf file
#' 
#' This saves some annoying typing. 
#'
#' @param file Character, the file to inspect. 
#' @param variables Character vector, optional. The variables to return. If not specified, all variables are returned.
#' @param exclude Logical, exclude the specified variables and return all others.
#' @param quiet Logical, suppress the 'meta' dump?
#' @param flip2D Logical, apply a vertical flip to 2D variables? (E.g. WRF Hydro geo grids)
#' @return A list or a dataframe (if all variables are 1D of the same length.)
#' @examples
#' \dontrun{
#' conn <- GetNcdfFile('~/wrfHydroTestCases/Fourmile_Creek/CHANNEL_CONNECTIVITY.nc')
#'  conn <- GetNcdfFile('~/wrfHydroTestCases/Fourmile_Creek/CHANNEL_CONNECTIVITY.nc', var='lambert_conformal_conic', exc=TRUE)
#' }
#' @concept ncdf 
#' @family ncdf 
#' @export
GetNcdfFile <- function(file, variables, exclude=FALSE, quiet=FALSE, flip2D=TRUE){
  
  if(!file.exists(file)) warning(paste0('The file ', file, 'does not exist.'), immediate. = TRUE)

  if(!quiet) ncdump(file)
    
  nc <- ncdf4::nc_open(file)
  
  # Deal with variables asked for
  varsInFile <- names(nc$var)
  dimVarsInFile <- names(nc$dim)
  whDimVarsVals <- plyr::laply(nc$dim, '[[', 'create_dimvar')
  if(any(whDimVarsVals)) varsInFile <- c(dimVarsInFile[whDimVarsVals], varsInFile)
  
  returnVars <- 
  if(!missing(variables)) {
    varsNotInFile <- setdiff(variables, varsInFile)
    if(length(varsNotInFile)) 
      warning(paste0('The following variables were not found in the file', paste(varsNotInFile, collapse=', ')))
    if(!exclude) intersect(variables, varsInFile) else setdiff(varsInFile, variables)
  } else varsInFile
  
  doGetVar <- function(theVar) ncdf4::ncvar_get(nc, varid=theVar)
  outList <- plyr::llply(NamedList(returnVars), doGetVar)

  doGetVarAtt <- function(theVar) ncdf4::ncatt_get( nc, varid=theVar )
  attList <- plyr::llply(NamedList(returnVars), doGetVarAtt)
  
  natts <- nc$natts
  if( natts  > 0 ) attList$global <- ncdf4::ncatt_get( nc, 0 )

  ncdf4::nc_close(nc)
  
  nDims <- plyr::laply(outList, function(ll) length(dim(ll)))
  
  if(flip2D & any(nDims==2)){
    wh2D <- which(nDims==2)
    for(ww in wh2D) outList[[ww]] <- FlipUD(outList[[ww]])
  }
    
  if( !(all(nDims==nDims[1])) | !(all(nDims==1)) ) return(outList)
  
  vecLen <- plyr::laply(outList[-10], length)
  if( all(vecLen==vecLen[1]) ) outList <- as.data.frame(outList)

  if( natts > 0 ) attributes(outList) <- c(attributes(outList), attList)
  
  outList
  
}

##=========================================================================================================
#' Emulate ncdump -h and -v.
#' 
#' I just hacked print.ncdf4 just to make it look more like unix output. 
#'
#' @param file Character, the file to inspect. 
#' @param variable Character, a variable to return. 
#' @param quiet Logical, suppress the 'meta' dump?
#' @return If variable is not set, the meta object \code{ncdf4::nc_open(file)} is returned. If \code{variable}
#' is set, its values are returned. 
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
          cat(paste0(indent,typeof(nc$dim[[i]]$vals),' ',nc$dim[[i]]$name,"(",nc$dim[[i]]$name,') ; \n' ))
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


