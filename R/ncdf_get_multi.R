#=====================================================================
ParseIndexArg <- function( index, dimSize, ncid ) {

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
  statArg=NULL
  
  if(is.list(index)) {
    if(!all(c('start','end','stat') %in% names(index))) {
      ncdf4::nc_close(ncid)
      warning('List index has inappropriate names.')
      return(NULL)
    }
    dataStart <- index[['start']]
    dataEnd <- index[['end']]
    statFunc <- CharToFunction(index[['stat']])
    statChar <- index[['stat']]
    if ('arg' %in% names(index)) {
      statArg <- index[['arg']]
    }
  } else if(is.character(index)) {
    statFunc <- CharToFunction(index)
    statChar <- index
    dataEnd <- dimSize
    dataStart <- dataEnd*0+1
  } else if(is.function(index)) {    
    statFunc <- CharToFunction(index)  ## this will throw error
  } else if(class(index)=='numeric' | class(index)=='integer') {
    dataStart <- dataEnd <- index
  } else {
    ncdf4::nc_close(ncid)
    warning('Inappropriate type (',typeof(index),
            ') for argument index or could not find a function with that name.')
    return(NULL)
  }
  
  list(dataStart=dataStart, dataEnd=dataEnd,
       statFunc=statFunc, statChar=statChar, statArg=statArg )
}


##=====================================================================

#' Open a netcdf file, extract specified indices for a variable, optionally
#' apply a specified statistic.
#' 
#' \code{GetFileStat} opens a netcdf file, extracts specified indices for a
#' variable, and may apply a specified statistic.
#' 
#' @param theFile The file to open.
#' @param variable TODO
#' @param index TODO
#' @param env The environment where the stat function lives
#' @param ... Further arguments to be passsed to a statistic.
#' @return A dataframe with columns TODO

#' @examples
#' #This is to do.
#' @keywords internal
#' @concept dataGet
#' @family getMultiNcdf
#' @export
GetFileStat <- function(theFile, variable, index, env=parent.frame(), parallel=FALSE, ...) {

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
  if(is.function(time)) warning('Time dimension not found in file, therefore the file name is returned in POSIXct')
  
  dimSize <- ncid$var[[variable]]$size

  ApplyIndex <- function(i=NULL, index, label="-") {
    ## Deal with various index possibilities. 
    ## Dont accept functions/closures, i want their names so I can use that information.
    ## Convert character strings to the associated closure/function, or die trying.
    if (!is.null(i)) {
      index <- index[[i]]
      label <- label[i]
    }
    indexList <- ParseIndexArg(index, dimSize)
    dataStart <- indexList$dataStart
    dataEnd <- indexList$dataEnd
    statFunc <- indexList$statFunc
    statChar <- indexList$statChar
    statArg <- indexList$statArg
    ## sanity check the dimensions
    #print(paste("i=", i, "label=", label, "dataStart=", length(dataStart), "dataEnd=", length(dataEnd), "dimSize=", length(dimSize)))
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
    outDf <- if(!is.null(statFunc))
              data.frame( do.call(statFunc, append(list(data), statArg), envir=env) ) else data.frame(data)
  
    names(outDf) <- c(variable)
    if (!is.function(time)) outDf$POSIXct <- time else outDf$POSIXct <- theFile
    outDf$inds <-paste( paste(dataStart,dataEnd,sep=':'), collapse=',' )
    if(is.null(statChar)) statChar <- '-'
    outDf$stat <- statChar
    outDf$statArg <- label
    outDf
    } # end: ApplyIndex
  
  if (is.list(index[[1]])) {
    i <- 1:length(index)
    outDf <- plyr::ldply(i, ApplyIndex, index, label=names(index),
                         .parallel=parallel)
  } else {
    outDf <- ApplyIndex(i=NULL, index)
  }
  ncdf4::nc_close(ncid)
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
#' @param env The environment where the stat function lives
#' @param parallel Logical, this is the .parallel argument of plyr functions.
#' @return A dataframe
#' @keywords internal
#' @concept dataGet
#' @family getMultiNcdf
#' @export
GetMultiNcdfVariable <- function(varInd, indexList,
                                 variableList, files, env=parent.frame(), parallel=FALSE) {
  outDf <- plyr::ldply(files, GetFileStat,
                       variableList[[varInd]], indexList[[varInd]],
                       env=env, .parallel=parallel) 
  outDf <- reshape2::melt(outDf, c('POSIXct','inds','stat','statArg') )
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
#' @param env The environment where the stat function lives
#' @param parallel Logical, this is the .parallel argument of plyr functions.
#' @return A dataframe
#' @keywords internal
#' @concept dataGet
#' @family getMultiNcdf
#' @export
GetMultiNcdfFile <- function(filesInd, filesList,
                             variableList, indexList, env=parent.frame(), parallel=FALSE) {
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
                       files=filesList[[filesInd]], env=env, parallel=parallel)
  outDf$fileGroup <- names(filesList)[filesInd]
  outDf
}
#=====================================================================

#=====================================================================

#' Get WRF Hydro output/restart (scalar) timeseries spread over multiple files.
#' 
#' \code{GetMultiNcdf} is designed to get *all* your output/restart data which 
#' are spread over multiple files. Three collated lists specify 1) file groups,
#' 2) variables for each file group, and 3) indices or statistics for each
#' variable in each file group. The names of the lists must match. See examples
#' for details. While the routine can read and summarize raster data at each
#' time via specificied statistics, it only returns scalar timeseries. (It may
#' be possible to extend to return both scalar and raster data if there's 
#' demand.)
#' 
#' @param filesList The list of file groups. Names must match those in the other
#'   lists.
#' @param variableList The list of variables for each file group. Names must
#'   match filesList.
#' @param indexList The list of indices or statistics to be applied to each
#'   variable.
#' @param env The environment where the stat function lives
#' @param parallel Logical, this is the .parallel argument of plyr functions. 
#'   Parallelization is at the file level (not file group).Typcially we achieve 
#'   parallelization using the DoMC package. See examples.
#' @return A dataframe (in an awesome format).
#'   
#' @examples
#' # This example only shows data for 3 dates, because of limitation of package data.
#' # Find the package data directory on your machine
#' \dontrun{
#' tcPath <- '~/wrfHydroTestCases/'
#' fcPath <- paste0(tcPath,'Fourmile_Creek/')
#' dataPath <- paste0(fcPath,'/RUN.RTTESTS/OUTPUT_CHRT_DAILY/')
#' fileList - These are the groups of files.
#' lsmFiles <- list.files(path=dataPath, pattern='LDASOUT_DOMAIN', full.names=TRUE)
#' hydroFiles <- list.files(path=dataPath, pattern='HYDRO_RST', full.names=TRUE)
#' fileList <- list( lsm=lsmFiles, hydro=hydroFiles)
#' 
#' # varList - Define which variables are desired for each file group.
#' lsmVars   <- list(TRAD='TRAD', SWE='SNEQV')
#' ## smc1-4 will correspond to the vertical layers.
#' hydroVars <- list(streamflow='qlink1', smc1='sh2ox', smc2='sh2ox', 
#'                   smc3='sh2ox', smc4='sh2ox')
#' # Note that the outer names collate with fileList.
#' variableList <- list(lsm=lsmVars, hydro=hydroVars)
#' 
#' # indexList - Define what indices/stats are desired for each variable.
#' # Note that only scalars can be returned for each entry. Spatial fields can 
#' # be summarized via statistics. 
#' # Show how to define your own useful stats to use.
#' # For basin average and max we need the basin mask (this is a non-standard
#' # field in the fine grid file).
#' basinMask <- ncdump(paste0(fcPath,'DOMAIN/hydro_OrodellBasin_100m.nc'), 
#'                     'basn_msk_geogrid')
#' nc_close(fineGridNc)
#' basAvg <- function(var) sum(basinMask*var)/sum(basinMask)
#' basMax <- function(var) max(ceiling(basinMask)*var)
#' basinKm2 <- sum(basinMask)  ## just asking about the total area of the basin.
#' 
#' # Note that the list names at this level collate with the variable names
#' # in VarList. You are responsible for entering the correct indices. Note
#' # that these are reverse order from what is shown in "ncdump -h".
#' lsmInds   <- list(TRAD=list(start=c(1,1,1), end=c(21,7,1), stat='basAvg'),
#'                   SNEQV=list(start=c(1,1,1), end=c(21,7,1), stat='basMax'))
#' hydroInds <- list(qlink1=1,
#'                   smc1=list(start=c(1,1,1), end=c(21,7,1), stat='basAvg'),
#'                   smc2=list(start=c(1,1,2), end=c(21,7,2), stat='basAvg'),
#'                   smc3=list(start=c(1,1,3), end=c(21,7,3), stat='basAvg'),
#'                   smc4=list(start=c(1,1,4), end=c(21,7,4), stat='basAvg') )
#' indexList <- list( lsm=lsmInds, hydro=hydroInds)
#' 
#' library(doMC)   ## Showing parallelization, which is at the file level within
#' registerDoMC(3) ## each file grous; pointless to be longer than your timeseries.
#' fileData <- GetMultiNcdf(file=fileList,var=variableList, ind=indexList,
#'                          parallel=TRUE)
#' 
#' # plot
#' # the lsm and hyro output for this spinup were at different times... 
#' library(ggplot2)
#' library(scales)
#' ggplot( fileData, aes(x=POSIXct, y=value, color=fileGroup)) +
#'   geom_line() + geom_point() +
#'   facet_wrap(~variableGroup, scales='free_y', ncol=1) +
#'   scale_x_datetime(breaks = date_breaks("5 days")) + theme_bw()
#' }
#' @export
GetMultiNcdf <- function(filesList, variableList, indexList, env=parent.frame(), parallel=FALSE) {
  ## Only do collated lists. Collation check at the file-variable level. 
  if (length(variableList) != length(indexList) |
      length(variableList) != length(filesList) ) 
    stop("The input lists must be collated: their lengths do not match.")
  if (all(names(variableList) != names(indexList)) |
      all(names(variableList) != names(filesList)) ) 
    stop("The input lists must be collated: their names do not match.")
  ## Due to some internal "deficiencies" of plyr, I find it's better to loop
  ## on index. This results in more coherent output.
  #print("Starting")
  fileInd <- 1:length(filesList)
  outDf <- plyr::ldply(fileInd, GetMultiNcdfFile,
                       variableList=variableList,
                       indexList=indexList,
                       filesList=filesList,
                       env=env, parallel=parallel)
  outDf
}
#=====================================================================
