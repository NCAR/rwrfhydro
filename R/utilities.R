##=============================================================================
#' Simplifed loading of rwrfhydro data included with the package.
#' 
#' \code{GetPkgRawDataPath} is a simplified wrapper (for system.file) for
#' loading external rwrfhydro data included with the package.
#' @param theFile The external data file to load (this is in dirrefent places
#'   before, rwrfhydro/inst/extdata, and after build, rwrfhydro/).
#' @return The full path to the file.
#' @examples
#' GetPkgRawDataPath('gagesII_all.csv')
#' @keywords internal
#' @export
GetPkgRawDataPath <- function(theFile='') system.file("extdata", theFile, package = "rwrfhydro")

##=============================================================================

#' Standardize lon to (-180,180].
#' 
#' \code{StdLon} Standardizes longitude to (-180,180]
#' 
#' @param x The numeric objeect to be standardized.
#' @return The standardized object.
#' @examples
#' StdLon(0:360)
#' @keywords internal
#' @export
StdLon <- function(x) {
  x[which(x>180.)] <- x[which(x>180.)]-360.
  x
}

#' Expand limits by some amount or proportionally to their difference.
#' 
#' \code{PadRange} Takes limits and expands them by some amount or
#' proportionally to their difference.
#' @param limits A vector of length 2, an initial range, to be expanded.
#' @param delta An amount to add(subtract) from the upper(lower) limit.
#' @param diffMult A fraction of the passed range (\code{limits}) to use as
#'   \code{delta}.
#' @examples
#' PadRange(c(0,100))
#' PadRange(c(0,100), delta=.1)
#' PadRange(c(0,100), diffMult=.1)
#' @keywords internal
#' @export
PadRange <- function(limits, delta=diffMult*diff(limits), diffMult=.05) {
  ## someday throw error if length(limits)>2
  limits+c(-delta,delta)
}


#' Rotate a matrix clock-wise.
#' 
#' \code{RotateCw} Rotates a matrix clock-wise. 
#' @param matrix A matrix.
#' @examples
#' x <- matrix(1:9, 3)
#' x
#' RotateCw(x)
#' RotateCw(RotateCw(x))
#' @keywords internal
#' @export
RotateCw <- function(matrix) t(apply(matrix, 2, rev))

#' Rotate a matrix counter-clock-wise.
#' 
#' \code{RotateCcw} Rotates a matrix counter-clock-wise. 
#' @param matrix A matrix. 
#' @examples
#' x <- matrix(1:9, 3)
#' x
#' RotateCcw(x)
#' RotateCcw(RotateCcw(x))
#' @keywords internal
#' @export
RotateCcw <- function(matrix) apply(matrix, 1, rev)

#' Flip a matrix upside down.
#' 
#' \code{FlipUD} Flips a matrix upside down.
#' @param matrix A matrix.
#' @examples
#' x <- matrix(1:9, 3)
#' x
#' FlipUD(x)
#' @keywords internal
#' @export
FlipUD <- function(matrix) apply(matrix,2,rev)

#' Flip a matrix from left to right.
#' 
#' \code{flipLR} Flips a matrix from left to to right.
#' @param matrix A matrix.
#' @examples
#' x <- matrix[1:9,3]
#' x
#' flipLR(x)
#' @keywords internal
#' @export
flipLR <- function(matrix) t(apply(matrix,1,rev))

#' Flip a matrix from left to right.
#' 
#' \code{flipLR} Flips a matrix from left to to right.
#' @param matrix A matrix.
#' @examples
#' x <- matrix[1:9,3]
#' x
#' flipLR(x)
#' @keywords internal
#' @export
flipLR <- function(matrix) t(apply(matrix,1,rev))

#' Translate (i.e. invert) timezones to the so calle Olson names used by
#' POSIXct.
#' 
#' Translate formatted timezones codes to the so-called "Olson names" used by
#' POSIXct. \code{TransTz} translates the formatted timezone codes (incl those
#' from USGS) to Olson Names.
#' @param tz The timezone to be translated.
#' @examples
#' as.POSIXct('2012-01-01')
#' as.POSIXct('2012-01-01', tz='US/Pacific')
#' format(as.POSIXct('2012-01-01', tz='US/Pacific'),'%Z')
#' TransTz(format(as.POSIXct('2012-01-01', tz='US/Pacific'),'%Z'))
#' lubridate::with_tz(as.POSIXct('2012-01-01'),
#'                    TransTz(format(as.POSIXct('2012-01-01', tz='US/Pacific'),'%Z')))
#' @keywords internal
#' @export
TransTz <- function(tz) {
  olson <- c(EDT ="US/Eastern",  EST ="US/Eastern",
             MDT ="US/Mountain", MST ="US/Mountain",
             PDT ="US/Pacific",  PST ="US/Pacific",
             CDT ="US/Central",  CST ="US/Central",
             AKDT="US/Alaska",   AKST="US/Alaska",
             HADT="US/Hawaii",   HAST="US/Hawaii" )[tz]
  # This is the full list of remaining US Olson names, given in R by OlsonNames()
  # "US/Aleutian", "US/Arizona", "US/East-Indiana", "US/Indiana-Starke",
  # "US/Michigan", "US/Pacific-New", "US/Samoa"
  if(any(is.na(olson))) warning('The supplied timezone code, ', tz,
                                ', is not covered by the cases programmed ',
                                'in TransTz (in read_observations.R). Please notify us or ',
                                'fix, commit, and send a pull request. Thanks!',
                                immediate.=TRUE)
  olson
}  

#' Returns the water year or the day of water year for a given POSIXct.
#' 
#' \code{CalcWaterYear} Returns the water year or the day of water year for a
#' given POSIXct.
#' @param POSIXct is a POSIXct variable.
#' @param dayOf signals if you want to get back the day of the water year
#'   instead of the water year.
#' @examples
#' CalcWaterYear(as.POSIXct(c("2011-09-30", "2011-10-01"), tz='US/Pacific'))
#' CalcWaterYear(as.POSIXct(c("2011-09-30", "2011-10-01"), tz='US/Pacific'), dayOf=TRUE)
#' @keywords internal
#' @export
CalcWaterYear <- function(POSIXct, dayOf=FALSE) {
  if (class(POSIXct)[1]!='POSIXct') {
    warning("Input is not of class POSIXct, returning NAs.")
    return( POSIXct*NA )
  }
  y <- as.numeric(format(POSIXct,'%Y'))
  m <- as.numeric(format(POSIXct,'%m'))
  y[which(m>=10)] <- y[which(m>=10)]+1
  ## if only the water year is required
  if (!dayOf) return(y)
  ## if the day of the water year is desired:
  d <- as.numeric(format(POSIXct,'%d'))
  first <- as.POSIXct( paste0(y-1,'-10-01'), format='%Y-%m-%d', tz='UTC')
  POSIXctUTC <- as.numeric(as.POSIXct(format(POSIXct,'%Y-%m-%d')))
  doyWY <- round((as.numeric(POSIXctUTC)-as.numeric(first))/60/60/24) + 1
  doyWY
}


#' Calculate standard date breaks.
#' 
#' \code{CalcDates} calculates standard date breaks.
#' Calculate standard date breaks.
#' @param x The input dataframe.
#' @return The input dataframe with date columns added.
#' @keywords internal
#' @export
CalcDates <- function (x) {
    x$day <- as.integer(format(x$POSIXct,"%d"))
    x$month <- as.integer(format(x$POSIXct,"%m"))
    x$year <- as.integer(format(x$POSIXct,"%Y"))
    x$wy <- ifelse(x$month >= 10, x$year + 1, x$year)
    x$yd <- as.integer(format(x$POSIXct,"%j"))
    x$wyd <- CalcWaterYear(x$POSIXct, dayOf=TRUE)
    x
}


#' Calculate mean with forced NA removal.
#' 
#' \code{CalcMeanNarm} calculates a mean with forced NA removal.
#' Read a vector and calculate the mean with forced NA removal.
#' @param x The vector of values.
#' @return The mean.
#' @keywords internal
#' @export
CalcMeanNarm <- function(x) {
    mean(x, na.rm=TRUE)
    }

#' Calculate mean with enforced minimum valid value.
#' 
#' \code{CalcMeanMinrm} calculates a mean with an enforced minimum valid value.
#' Read a vector and calculate the mean with all values below
#' a specified minimum value set to NA (and therefore ignored).
#' @param x The vector of values.
#' @param minValid The minimum valid value.
#' @return The mean.
#' @examples
#' x <- c(1,2,-1e+20,3)
#' mean(x) # yields -2.5e+19
#' CalcMeanMinrm(x, 0) # yields 2
#' @keywords internal
#' @export
CalcMeanMinrm <- function(x, minValid=-1e+30) {
    x[which(x<minValid)]<-NA
    mean(x, na.rm=TRUE)
    }

#' Calculate cumulative sum with forced NA=0.
#' 
#' \code{CumsumNa} calculates a cumulative sum with NAs converted to 0s.
#' Read a vector and calculate the cumulative sum with NAs converted to 0s.
#' @param x The vector of values.
#' @return The cumulative sum vector.
#' @keywords internal
#' @export
CumsumNa <- function(x) {
    x[which(is.na(x))] <- 0
    return(cumsum(x))
}

#' Calculate Nash-Sutcliffe Efficiency.
#' 
#' \code{Nse} calculates the Nash-Sutcliffe Efficiency.
#' Calculate the Nash-Sutcliffe Efficiency for vectors
#' of modelled and observed values.
#' @param m The vector of modelled values.
#' @param o The vector of observed values.
#' @return The Nash-Sutcliffe Efficiency.
#' @keywords internal
#' @export
Nse <- function (m, o) {
    err1 <- sum((m - o)^2, na.rm=T)
    err2 <- sum((o - mean(o, na.rm=T))^2, na.rm=T)
    ns <- 1 - (err1/err2)
    ns
}

#' Calculate Log Nash-Sutcliffe Efficiency.
#' 
#' \code{NseLog} calculates the Log Nash-Sutcliffe Efficiency.
#' Calculate the Nash-Sutcliffe Efficiency for vectors
#' of log-transformed modelled and observed values.
#' @param m The vector of modelled values.
#' @param o The vector of observed values.
#' @return The Log Nash-Sutcliffe Efficiency.
#' @keywords internal
#' @export
NseLog <- function (m, o) {
    m <- log(m + 1e-04)
    o <- log(o + 1e-04)
    err1 <- sum((m - o)^2, na.rm=T)
    err2 <- sum((o - mean(o, na.rm=T))^2, na.rm=T)
    ns <- 1 - (err1/err2)
    ns
}


#' Calculate root mean squared error.
#' 
#' \code{Rmse} calculates the root mean squared error.
#' Calculate the root mean squared error for vectors
#' of modelled and observed values.
#' @param m The vector of modelled values.
#' @param o The vector of observed values.
#' @return The root mean squared error.
#' @keywords internal
#' @export
Rmse <- function (m, o) {
    err <- sum((m - o)^2, na.rm=T)/(min(sum(!is.na(m)),sum(!is.na(o))))
    rmserr <- sqrt(err)
    rmserr
}

#' Calculate normalized root mean squared error.
#' 
#' \code{RmseNorm} calculates the normalized root mean squared error.
#' Calculate the normalized root mean squared error for vectors
#' of modelled and observed values.
#' @param m The vector of modelled values.
#' @param o The vector of observed values.
#' @return The nrmalized root mean squared error.
#' @keywords internal
#' @export
RmseNorm <- function (m, o) {
    err <- sum((m - o)^2, na.rm=T)/(min(sum(!is.na(m)),sum(!is.na(o))))
    rmserr <- sqrt(err) / ( max(o, na.rm=T) - min(o, na.rm=T) ) * 100
    rmserr
}

#' Calculate center-of-mass.
#' 
#' \code{CalcCOM} calculates the time step of center of mass.
#' Calculate the time step when the center-of-mass of
#' a time series of values occurs.
#' @param x The time series vector.
#' @return The center-of-mass time step.
#' @keywords internal
#' @export
CalcCOM <- function (x) {
    cuml.x <- as.data.frame(CumsumNa(x)/sum(x, na.rm=T))
    colnames(cuml.x) <- c("x")
    cuml.x$ts <- seq(from = 1, to = length(cuml.x$x))
    tmp <- subset(cuml.x, cuml.x$x > 0.5)
    ts <- tmp$ts[1]
    ts
}

#' "Flatten" the output from GetMultiNcdf
#' 
#' \code{ReshapeMultiNcdf} flattens the output from GetMultiNcdf.
#' Take the output dataframe from GetMultiNcdf and reshape the dataframe
#' for ease of use in other functions.
#' @param myDf The output dataframe from GetMultiNcdf.
#' @return The reshaped output dataframe.
#' @keywords utilities internal
#' @export
ReshapeMultiNcdf <- function(myDf) {
    newDF <- subset(myDf[,c("POSIXct","stat","statArg")], myDf$variableGroup==unique(myDf$variableGroup)[1])
    for (i in unique(myDf$variableGroup)) {
        newDF[,i] <- subset(myDf$value, myDf$variableGroup==i)
        }
    newDF$wy <- ifelse(as.numeric(format(newDF$POSIXct,"%m"))>=10,
                       as.numeric(format(newDF$POSIXct,"%Y"))+1,
                       as.numeric(format(newDF$POSIXct,"%Y")))
    newDF
}

#' Create and or name a list with its entries.
#' 
#' \code{NamedList} creates a list with names equal to its entries. 
#' @param theNames Vector to be coerced to character.
#' @return List with names equal to entries.
#' @examples 
#' NamedList(1:5)
#' @keywords manip
#' @export
NamedList <- function(theNames) {
  theList <- as.list(theNames)
  names(theList)<- theNames
  theList
}


#' Are all vector entries the same/identical.
#' 
#' \code{AllSame} check if all vector entries are same/identical.
#' @param x A vector.
#' @param na.rm Logical Remove NAs from output?
#' @return Logical
#' @examples 
#' AllSame( 1:5 )
#' AllSame( 0*(1:5) )
#' @keywords internal
#' @export
AllSame <- function(x, na.rm=FALSE) all(x==x[which(!is.na(x))[1]], na.rm=na.rm)


# coversion constants
cfs2cms <- 0.0283168466  
feet2meters <- 0.30480

#' Get a package's metadata fields and associated entries.
#' 
#' \code{GetPkgMeta} Get metadata fields and associated entries from a package's
#' documentation (e.g. "keyword" or "concepts".)
#' @param meta Character the metadata field.
#' @param package Character The package to query for metadata.
#' @param quiet Logical Do not print summary to screen.
#' @param keyword Character A specific keyword to look for.
#' @param concept Character A specific concept to look for.
#' @param listMetaOnly Logical Just return the meta categories (without
#'   functons)?
#' @param byFunction Character Vector of functions for which concepts and
#'   keywords are desired.
#' @return List of metadata fields in alphabetical order with corresponding
#'   entries.
#' @examples 
#' GetPkgMeta()
#' GetPkgMeta('keyword', package='ggplot2')
#' GetPkgMeta(concept = 'foo', key='hplot' )
#' print( GetPkgMeta(concept = c('dataMgmt','foo'), key=c('foo','hplot') , quiet=TRUE))
#' str( GetPkgMeta(concept = c('dataMgmt','foo'), key=c('foo','hplot') , quiet=TRUE))
#' str( GetPkgMeta(concept = c('dataMgmt','DART'), key=c('internal','hplot') , quiet=TRUE))
#' GetPkgMeta(byFunction=c('MkDischargeVariance','SaveHucData'))
#' @keywords utilities
#' @export
GetPkgMeta <- function(meta=c('concept','keyword'), package='rwrfhydro',  
                       keyword=NULL, concept=NULL, quiet=FALSE, listMetaOnly=FALSE, 
                       byFunction='') {
  ## if specifying keywords or concepts, gather them into a vector conKey
  if(!is.null(keyword)  | !is.null(concept)) {
    nameNull <- 
      function(n) tryCatch(ifelse(is.null(get(n)),NULL,n),warning=function(w) {})
    meta <- c(nameNull('concept'),nameNull('keyword'))
    conKey <- c(concept, keyword)
  }
  out <- plyr::llply(NamedList(meta), GetPkgMeta.scalar, package=package, byFunction=byFunction)
  if(!is.null(keyword) | !is.null(concept)) {
    out <- plyr::llply(out, function(mm) mm[which(names(mm) %in% conKey)])
  }
  if(byFunction[1]!=''){  
    ## GetPkgMeta.scalar returns lists organized by meta (can only apparently search on keywords not function names)
    ## so do the "inversion" here (seems like there might be a more elegant way, but .Rd_get_metadata is vague)
    out[which(!as.logical(plyr::laply(out, length)))] <- NULL
    if(!length(out)) return(NULL)
    out <- reshape2::melt(out)
    out$value <- as.character(out$value)
    out <- plyr::dlply(out, plyr::.(L2), function(ss) plyr::dlply(ss, plyr::.(L1), function(zz) zz$value))
    for (oo in names(out)) { 
      attr(out[[oo]], 'split_type') <- NULL
      attr(out[[oo]], 'split_labels') <- NULL
      attr(out[[oo]], 'meta') <- oo
      attr(out[[oo]], 'package') <- package
      attr(out[[oo]], 'class') <- c('pkgMeta', class(out[[oo]]))
    }
    attr(out, 'split_type') <- NULL
    attr(out, 'split_labels') <- NULL    
  }
  out <- out[which(as.logical(unlist(plyr::llply(out,length))))]
  if(listMetaOnly) out <- plyr::llply(out, function(ll) { for(cc in names(ll)) ll[[cc]] <- c(''); ll} )
  attr(out,'class') <- c('pkgMeta', class(out))
  if(!quiet) print(out)
  invisible(out)
}

GetPkgMeta.scalar <- function(meta='concept', package='rwrfhydro', byFunction='') {
  l1 <- plyr::llply(tools::Rd_db(package), tools:::.Rd_get_metadata, meta)
  l2 <- l1[as.logical(plyr::laply(l1, length))]  ## remove empties
  if(!length(l2)) return(NULL)
  l3 <- plyr::llply(l2, function(ll) ll[which(as.logical(nchar(ll)))] ) ## remove blanks
  ulStrsplit <- function(...) unlist(strsplit(...))
  l4 <- plyr::llply(l3, ulStrsplit, ' ') ## parse individual keywords
  names(l4) <- plyr::laply(strsplit(names(l3),'\\.Rd'),'[[',1)  ## remove .Rd from function doc names.
  out <- if(byFunction[1]!='') 
    l4[byFunction] else plyr::dlply(reshape2::melt(l4), plyr::.(value), function(dd) dd$L1 )
  out <- out[sort(names(out))]
  attr(out, 'split_type') <- NULL
  attr(out, 'split_labels') <- NULL
  attr(out, 'meta') <- meta
  attr(out, 'package') <- package
  attr(out, 'class') <- c('pkgMeta', class(out))
  invisible(out)
}

#' @export
print.pkgMeta  <- function(pkgMeta) {
  PrintAtomic <- function(atom) {
    meta <- attr(atom,'meta')
    package <- attr(atom,'package')
    anS <- if(grepl('s$',meta)) '' else 's'
    if(!grepl('keyword|concept',meta)) anS <- '()'
    pkgSep <- if(grepl('keyword|concept',meta)) ' ' else ': '
    cat('\n')
    cat('-----------------------------------',sep='\n')
    cat(paste0(package,pkgSep,meta,anS), sep='\n')
    cat('-----------------------------------',sep='\n')
    for (ii in (1:length(atom))) {      
      if(atom[[ii]][1]!='') {
        cat('* ',names(atom)[ii],':\n', sep='')
        writeLines(strwrap(paste('   ',atom[[ii]],collapse=' '),prefix='   '))
        cat('\n')
      } else cat(names(atom)[ii],'\n', sep='')
    }
    invisible(1)
  }
  plyr::llply(pkgMeta, PrintAtomic)
  invisible(pkgMeta)
}


#' @export
`[.pkgMeta` <- function(x,i,...) {
  meta <- attr(x, "meta")
  package <- attr(x, "package")
  class <- attr(x, "class")  
  attr(x, 'class') <- 'list'
  x <- `[`(x,i,...)
  attr(x, "meta") <- meta
  attr(x, "package") <- package
  attr(x, "class")  <- class
  x
}


#' Handle vector arguments to functions in a collated fashion.
#' 
#' \code{FormalsToDf} is called inside a function where some formal arguments may have
#' been supplied as vectors. \code{FormalsToDf} constructs a dataframe from the
#' arguments which can then be passed to plyr::mlply (or similar, e.g. mdply) to
#' return a list of results.  The assumption is that all vector arguments are
#' collated: 1) NULL arguments are dropped; 2) all arguments of length > 1 all have
#' the same length and are collated. 
#' @param meta Character the metadata field.
#' @param package Character The package to query for metadata.
#' @return Dataframe
#' @examples 
#' ## A stupid example where true vectorization is possible
#' myF.atomic <- function(x,y,z=NULL) if(is.null(z)) x+y else x+y+z  
#' myF <- function(x,y,z=NULL) { 
#'   col <- FormalsToDf(myF) 
#'   plyr::maply(col, myF.atomic, .expand=FALSE)
#'   }
#' myF.atomic(x=11:13,y=1:3)
#' myF(x=11:13,y=1:3)
#' @keywords utilities internal
#' @export
FormalsToDf <- function(theFunc, envir=parent.frame()) {
  ## can only have formals of two different lengths other than zero.
  ## find the length of the formals
  theFormals <- names(formals(theFunc))
  formalLens <- plyr::laply(theFormals, function(ff) length(get(ff, envir=envir)))
  formalLensGt1 <- formalLens[which(formalLens>1)]
  nLenGt1 <- length(unique(formalLensGt1))
  if(nLenGt1>1) {
    warning('Formals are not collated as required.')
    return(NULL)
  }
  theFormals <- theFormals[which(formalLens!=0)]
  formalLens <- formalLens[which(formalLens!=0)]
  sortFormals <- theFormals[sort(formalLens, index=TRUE, decreasing=TRUE)$ix]
  for (ff in sortFormals) {
    if (ff==sortFormals[1]) {
      df <- data.frame(get(ff, envir=envir))
      names(df) <- ff
    } else df[ff] <- get(ff, envir=envir)
  }
  df  
}

#' Calculate number of days in a month.
#' 
#' \code{CalcMonthDays} calculates the number of days in a month.
#' Calculate the number of days in the month specified
#' by the given month and year.
#' @param mo The month.
#' @param yr The year.
#' @return The day count.
#' @keywords utilities internal
#' @export
CalcMonthDays <- function(mo, yr) {
  #m <- format(date, format="%m")
  res<-c()
  for (i in 1:length(mo)) {
    date <- as.Date(paste0(yr[i], "-", mo[i], "-01"), format="%Y-%m-%d")
    while (as.integer(format(date, format="%m")) == as.integer(mo[i])) {
      date <- date + 1
    }
    res[i] <- as.integer(format(date - 1, format="%d"))
  }
  return(res)
}

#' Calculate date object from POSIXct time
#' 
#' \code{CalcDateTrunc} takes a POSIXct object and outputs
#' a corresponding date object. POSIXct times are truncated
#' to a date, not rounded (e.g., 03/15/2014 23:00 will 
#' become 03/15/2014).
#' @param timePOSIXct Time in POSIXct format
#' @param timeZone Time zone (DEFAULT="UTC")
#' @return Date object
#' @keywords utilities internal
#' @export
CalcDateTrunc <- function(timePOSIXct, timeZone="UTC") {
  timeDate <- as.Date(trunc(as.POSIXct(format(timePOSIXct, tz=timeZone), tz=timeZone), "days"))
  return(timeDate)
}

#' List objects with more detailed metadata
#' 
#' \code{LsObjects} lists objects with more detailed info on type, size,
#' etc. Taken from
## http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
#' Petr Pikal, David Hinds, Dirk Eddelbuettel, Tony Breyal.
#' @param pos           position
#' @param pattern       pattern
#' @param order.by      ordering variable
#' @param decreasing    decreasing order
#' @param head          head
#' @param n             n
#' @return dataframe
#' @keywords utilities internal
#' @export
LsObjects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(format(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

#' Shorthand call for LsObjects.
#' 
#' \code{lsOS} Shorthand call for LsObjects
#' @param n   n
#' @return    dataframe 
#' @keywords utilities internal
#' @export
lsOS <- function(..., n=10) {
  LsObjects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

#' Calculate a running mean
#' 
#' \code{CalcRunningMean} takes series and calculates
#' a running mean over specified number of records
#' @param x Vector of values
#' @param n Number of records to use
#' @param sides Whether to use both sides (2=past and 
#' future) or just one side (1=past only).
#' @param ts Flag whether or not to return a time
#' series object or just a vector (DEFAULT=FALSE,
#' which returns a vector)
#' @return Vector of moving averages
#' @keywords utilities internal
#' @export
CalcRunningMean <- function(x, n, sides=2, ts=FALSE) {
  out <- filter(x, rep(1/n,n), sides=sides, method="convolution")
  if (!ts) out <- unclass(out)
  return(out)
}

#' Fill outliers based on change between steps
#' 
#' \code{FillOutliers} is a simple utility
#' to fill outliers in a time series. Values that are 
#' more than some threshold change from the previous
#' time step are replaced by the value from that
#' previous time step. 
#' @param x Vector of values
#' @param thresh Threshold for change between timesteps
#' (in absolute value of x units)
#' @return Vector of values with outliers replaced by
#' previous value
#' @keywords utilities internal
#' @export
FillOutliers <- function(x, thresh) {
  for (i in 2:(length(x)-1)) {
    if (!is.na(x[i]) & !is.na(x[i-1])) del <- (x[i]-x[i-1])
    if (abs(del) > thresh) x[i] <- x[i-1]
    }
  return(x)
}

#' Convert long-ish integers to characters without mutilation
#' 
#' \code{AsCharLongInt} is simply a usage of format.
#' @param x Vector of values
#' @return Vector of characters.
#' @keywords utilities internal
#' @export
AsCharLongInt <- function(vec) {
  allIntegers <- all((vec %% 1) == 0)
  if(!allIntegers)
    warning("Non-integer values supplied to AsCharLongInt")
  if(any(abs(vec) > 2^.Machine$double.digits))
    warning("Values passed to AsCharLongInt exceed bounds of representable integers in R") 
  format((vec), trim=TRUE, nsmall=0, scientific=0,digits=16)
}

