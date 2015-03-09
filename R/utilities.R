#' GetPkgDataPath
#' 
#' \code{GetPkgData} is a simplified wrapper (for system.file) for loading external rwrfhydro data included with the package.
#' 
#' @param theFile The external data file to load (this is in dirrefent places before, rwrfhydro/inst/extdata, and after build, rwrfhydro/). 
#' @return The full path to the file.
#' @examples
#' GetPkgDataPath('Fourmile_test_case_AD.hydro_OrodellBasin_100m_8.nc')
#' @export
GetPkgDataPath <- function(theFile='') system.file("extdata", theFile, package = "rwrfhydro")

#' StdLon
#' 
#' \code{StdLon} Standardizes longitude to (-180,180]
#' 
#' @param x The numeric objeect to be standardized.
#' @return The standardized object.
#' @examples
#' StdLon(0:360)
#' @export
StdLon <- function(x) {
  x[which(x>180.)] <- x[which(x>180.)]-360.
  x
}

#' PadRange
#' 
#' \code{PadRange} Takes limits and expands them by some amount or proportionally to their difference.
#' @param limits A vector of length 2, an initial range, to be expanded.
#' @param delta An amount to add(subtract) from the upper(lower) limit.
#' @param diffMult A fraction of the passed range (\code{limits}) to use as \code{delta}.
#' @examples
#' PadRange(c(0,100))
#' PadRange(c(0,100), delta=.1)
#' PadRange(c(0,100), diffMult=.1)
PadRange <- function(limits, delta=diffMult*diff(limits), diffMult=.05) {
  ## someday throw error if length(limits)>2
  limits+c(-delta,delta)
}


#' RotateCw
#' 
#' \code{RotateCw} Rotates a matrix clock-wise. 
#' @param matrix A matrix.
#' @examples
#' x <- matrix(1:9, 3)
#' x
#' RotateCw(x)
#' RotateCw(RotateCw(x))
RotateCw <- function(matrix) t(apply(matrix, 2, rev))

#' RotateCcw
#' 
#' \code{RotateCcw} Rotates a matrix counter-clock-wise. 
#' @param matrix A matrix. 
#' @examples
#' x <- matrix(1:9, 3)
#' x
#' RotateCcw(x)
#' RotateCcw(RotateCcw(x))
RotateCcw <- function(matrix) apply(matrix, 1, rev)


#' CalcWaterYear
#' 
#' \code{CalcWaterYear} Returns the water year or the day of water year for a given POSIXct.  
#' @param POSIXct is a POSIXct variable.
#' @param dayOf signals if you want to get back the day of the water year instead of the water year.
#' @examples
#' CalcWaterYear(as.POSIXct(c("2011-09-30", "2011-10-01"), tz='US/Pacific'))
#' CalcWaterYear(as.POSIXct(c("2011-09-30", "2011-10-01"), tz='US/Pacific'), dayOf=TRUE)
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
 
