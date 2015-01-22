# This code was pulled from
# ~/R/jlm_lib/general/timeseriesMultiNcdf.r
# /home/jamesmcc/R/jlm_lib/general/parseIndexArg.r
# on hydro-c1 where there may remain other relevant code.

## the premise is that the timeseries is strewn over multiple files and the 
## time is either a global attribute or in the variable 'Times'

#=====================================================================
ParseIndexArg <- function( index, dimSize ) {

  ## Deal with various index possibilities. 
  ## Dont accept functions/closures, i want their names so I can use that information.
  ## Convert character strings to the associated closure/function, or die trying.

  CharToFunction <- function(theInput) {
    if(is.function(theInput)) {
      ncdf4::nc_close(ncid)
      warning("This routine does not accept functions directly, only their names",
              "(for the same reason, I cant tell you the name of the offending function).")
      return(NULL)
    }
    
    if(typeof(index)=='character') {
      if(is.function(try(get(theInput)))) {
        return(get(theInput))
      } else {
        ncdf4::nc_close(ncid)
        warning('Could not find a function with that name (could be a scoping issue).')
      }
    }
    theInput
  } ## end charToFunction

  ##=============================================
  ## default values. 
  dataStart=NULL
  dataEnd=NULL
  statFunc=NULL
  statChar=NULL
  
  if(is.list(index)) {
    if(!all(c('start','end','stat') %in% names(index))) {
      ncdf4::nc_close(ncid)
      warning('List index has inapproprite names.')
      return(NULL)
    }
    dataStart <- index[['start']]
    dataEnd <- index[['end']]
    statFunc <- CharToFunction(index[['stat']])
    statChar <- index[['stat']]
  } else if(is.character(index)) {
    statFunc <- CharToFunction(index)
    statChar <- index
    dataEnd <- dimSize
    dataStart <- dataEnd*0+1
  } else if(is.function(index)) {    
    statFunc <- CharToFunction(index)  ## this will throw error
  } else if(class(index)=='numeric') {
    dataStart <- dataEnd <- index
  } else {
    ncdf4::nc_close(ncid)
    warning('Inapproriate type (',typeof(index),
            ') for argument index or could not find a function with that name.')
    return(NULL)
  }
  
  list(dataStart=dataStart, dataEnd=dataEnd,
       statFunc=statFunc, statChar=statChar )
}
#=====================================================================


##=====================================================================
#' Return data from a netcdf file.
#'
#' \code{GetFileStat} opens a netcdf file, extracts specified indices for
#' a variable, and may apply a specified statistic. 
#'
#' @param theFile The file to open. 
#' @param variable TODO
#' @param index TODO 
#' @param ... Further arguments to be passsed to a statistic. 
#' @return A dataframe with columns TODO

#' @examples
#' This is to do.
GetFileStat <- function(theFile, variable, index, ...) {

  if(!file.exists(theFile)) {
    warning('No such file: ',theFile)
    return(NULL)
  }
  ncid <- ncdf4::nc_open(theFile)
  
  if(!(variable %in% names(ncid$var))) {
    ncdf4::nc_close(ncid)
    warning('No such variable ',variable,' in ',theFile,'.')
    return(NULL)
  }

  ## get the time.
  if('times' %in% tolower(names(ncid$var))) { ## noah/noahMP style variable Times
    varNames <- names(ncid$var)
    timeChr <- varNames[which(tolower(names(ncid$var))=='times')]
    time <-ncdf4::ncvar_get(ncid, timeChr)
    time <- as.POSIXct(sub('_',' ',time), tz='UTC') ## wrf hydro times are UTC 
  }

  possibleTimeNames <- c('Restart_Time','time_coverage_end')
  whTimeName <- which(possibleTimeNames %in% names(ncdf4::ncatt_get( ncid, 0 )))
  if(length(whTimeName)) {
    time <- ncdf4::ncatt_get( ncid, 0 )[possibleTimeNames[whTimeName]]
    time <- as.POSIXct(sub('_',' ',time), tz='UTC') ## wrf hydro is UTC
  } 
  if(is.function(time)) stop('Time dimension not found in file')
  
  dimSize <- ncid$var[[variable]]$size

  ## Deal with various index possibilities. 
  ## Dont accept functions/closures, i want their names so I can use that information.
  ## Convert character strings to the associated closure/function, or die trying.
  indexList <- ParseIndexArg(index, dimSize)
  dataStart <- indexList$dataStart
  dataEnd <- indexList$dataEnd
  statFunc <- indexList$statFunc
  statChar <- indexList$statChar
  
  ## sanity check the dimensions
  if( length(dataStart)!=length(dataEnd) | length(dataStart)!=length(dimSize) |
  any(dataStart < 1) | any(dataStart > dimSize) |
     any(dataEnd   < 1) | any(dataEnd   > dimSize) |
     any(dataStart > dataEnd) ) {
    ncdf4::nc_close(ncid)
    stop("Error in passed index or its dimensions, variable has dimensions: ",
            paste(dimSize,collapse=', '))
    return(NULL)
  }
  
  dataCount <- dataEnd-dataStart+1
  data <- ncdf4::ncvar_get(ncid, variable, start=dataStart, count=dataCount)
  ncdf4::nc_close(ncid)

  outDf <- if(!is.null(statFunc))
              data.frame( do.call(statFunc, list(data, ...)) ) else data.frame(data)
  
  names(outDf) <- c(variable)
  outDf$POSIXct <- time
  outDf$inds <-paste( paste(dataStart,dataEnd,sep=':'), collapse=',' )
  if(is.null(statChar)) statChar <- '-'
  outDf$stat <- statChar
  outDf
}
# end: getFileStat
#=====================================================================

#=====================================================================
#' The GetMultiNcdf for getting variables out of individual files.
#'
#' \code{GetMultiNcdfVariable}: For a file, get all variables.
#' Typically calls GetFileStat to get the individual variables.
#' Typically called by GetMultiNcdfFile.
#'
#' @param varInd The variable index. 
#' @param indexList The variable list. 
#' @param variableList The variable list. 
#' @param files The files vector.
#' @param parallel Logical, this is the .parallel argument of plyr functions.
#' @return A dataframe
#'

GetMultiNcdfVariable <- function(varInd, indexList,
                                 variableList, files, parallel=FALSE) {
  outDf <- plyr::ldply(files, GetFileStat,
                       variableList[[varInd]], indexList[[varInd]],
                       .parallel=parallel) 
  outDf <- reshape2::melt(outDf, c('POSIXct','inds','stat') )
  outDf$variableGroup <- names(variableList)[varInd]
  outDf
}
#=====================================================================


#=====================================================================
#' The GetMultiNcdf for individual file groups.
#'
#' \code{GetMultiNcdfFile}: For this file group, get each files variable.
#' Typically calls GetMultiNcdfVariable to then get the individual files.
#' Typically called by GetMultiNcdf.
#'
#' @param filesInd The index of the file groups.
#' @param filesList The list of the file groups.
#' @param variableList The variable list.
#' @param indexList The index list.
#' @param parallel Logical, this is the .parallel argument of plyr functions.
#' @return A dataframe

GetMultiNcdfFile <- function(filesInd, filesList,
                             variableList, indexList, parallel=FALSE) {
  ## Enforce collation at the variable-index level: (for this file) each variable
  ## has to have a collated index.
  if (length(variableList) != length(indexList) ) 
    stop(paste0("For file set ", names(filesList)[filesInd], " (#",filesInd,") ",
                "The variable and index lists must be collated: their lengths do not match."))
  if (all(names(variableList) != names(indexList)) )
    stop(paste0("For file set ", names(filesList)[filesInd], " (#",filesInd,") ",
                "The variable and index lists must be collated: their names do not match."))
  varInd <- 1:length(variableList[[filesInd]])
  outDf <- plyr::ldply(varInd, GetMultiNcdfVariable,
                       variableList=variableList[[filesInd]],
                       indexList=indexList[[filesInd]],
                       files=filesList[[filesInd]], parallel=parallel)
  outDf$fileGroup <- names(filesList)[filesInd]
  outDf
}
#=====================================================================

#=====================================================================
#' Get WRF Hydro output/restart timeseries spread over multiple files.
#'
#' \code{GetMultiNcdf} is designed to get *all* your output/restart data which
#' are spread over multiple files. Three collated lists specify
#' 1) file groups, 2) variables for each file group, and 3) indices or statistics
#' for each variable in each file group. The names of the lists must match. See 
#' examples for details. 
#'
#' @param filesList The list of file groups. Names must match those in the other lists. 
#' @param variableList The list of variables for each file group. Names must match filesList.
#' @param indexList The list of indices or statistics to be applied to each variable.
#' @param parallel Logical, this is the .parallel argument of plyr functions.
#' Parallelization is at the file level (not file group).Typcially we achieve
#' parallelization using the DoMC package. See examples. 
#' @return A dataframe in an awesome format (, Aubrey!).
#'
#' @examples
#' This is to do.
#' @export
GetMultiNcdf <- function(filesList, variableList, indexList, parallel=FALSE) {
  ## Only do collated lists. Collation check at the file-variable level. 
  if (length(variableList) != length(indexList) |
      length(variableList) != length(filesList) ) 
    stop("The input lists must be collated: their lengths do not match.")
  if (all(names(variableList) != names(indexList)) |
      all(names(variableList) != names(filesList)) ) 
    stop("The input lists must be collated: their names do not match.")
  ## Due to some internal "deficiencies" of plyr, I find it's better to loop
  ## on index. This results in more coherent output.
  fileInd <- 1:length(filesList)
  outDf <- plyr::ldply(fileInd, GetMultiNcdfFile,
                       variableList=variableList,
                       indexList=indexList,
                       filesList=filesList,
                       parallel=parallel)
  outDf
}
#=====================================================================
