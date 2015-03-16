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

#' Calculate standard date breaks.
#' \code{CalcDates} calculates standard date breaks.
#' Calculate standard date breaks.
#' @param x The input dataframe.
#' @return The input dataframe with date columns added.
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
#' \code{CalcMeanNarm} calculates a mean with forced NA removal.
#' Read a vector and calculate the mean with forced NA removal.
#' @param x The vector of values.
#' @return The mean.
CalcMeanNarm <- function(x) {
    mean(x, na.rm=TRUE)
    }

#' Calculate mean with enforced minimum valid value.
#' \code{CalcMeanMinrm} calculates a mean with an enforced minimum valid value.
#' Read a vector and calculate the mean with all values below
#' a specified minimum value set to NA (and therefore ignored).
#' @param x The vector of values.
#' @param minValid The minimum valid value.
#' @return The mean.
CalcMeanMinrm <- function(x, minValid=-1e+30) {
    x[which(x<minValid)]<-NA
    mean(x, na.rm=TRUE)
    }

#' Calculate cumulative sum with forced NA=0.
#' \code{CumsumNa} calculates a cumulative sum with NAs converted to 0s.
#' Read a vector and calculate the cumulative sum with NAs converted to 0s.
#' @param x The vector of values.
#' @return The cumulative sum vector.
CumsumNa <- function(x) {
    x[which(is.na(x))] <- 0
    return(cumsum(x))
}

#' Calculate Nash-Sutcliffe Efficiency.
#' \code{Nse} calculates the Nash-Sutcliffe Efficiency.
#' Calculate the Nash-Sutcliffe Efficiency for vectors
#' of modelled and observed values.
#' @param m The vector of modelled values.
#' @param o The vector of observed values.
#' @return The Nash-Sutcliffe Efficiency.
Nse <- function (m, o) {
    err1 <- sum((m - o)^2, na.rm=T)
    err2 <- sum((o - mean(o, na.rm=T))^2, na.rm=T)
    ns <- 1 - (err1/err2)
    ns
}

#' Calculate Log Nash-Sutcliffe Efficiency.
#' \code{NseLog} calculates the Log Nash-Sutcliffe Efficiency.
#' Calculate the Nash-Sutcliffe Efficiency for vectors
#' of log-transformed modelled and observed values.
#' @param m The vector of modelled values.
#' @param o The vector of observed values.
#' @return The Log Nash-Sutcliffe Efficiency.
NseLog <- function (m, o) {
    m <- log(m + 1e-04)
    o <- log(o + 1e-04)
    err1 <- sum((m - o)^2, na.rm=T)
    err2 <- sum((o - mean(o, na.rm=T))^2, na.rm=T)
    ns <- 1 - (err1/err2)
    ns
}

#' Calculate root mean squared error.
#' \code{Rmse} calculates the root mean squared error.
#' Calculate the root mean squared error for vectors
#' of modelled and observed values.
#' @param m The vector of modelled values.
#' @param o The vector of observed values.
#' @return The root mean squared error.
Rmse <- function (m, o) {
    err <- sum((m - o)^2, na.rm=T)/(min(sum(!is.na(m)),sum(!is.na(o))))
    rmserr <- sqrt(err)
    rmserr
}

#' Calculate normalized root mean squared error.
#' \code{RmseNorm} calculates the normalized root mean squared error.
#' Calculate the normalized root mean squared error for vectors
#' of modelled and observed values.
#' @param m The vector of modelled values.
#' @param o The vector of observed values.
#' @return The nrmalized root mean squared error.
RmseNorm <- function (m, o) {
    err <- sum((m - o)^2, na.rm=T)/(min(sum(!is.na(m)),sum(!is.na(o))))
    rmserr <- sqrt(err) / ( max(o, na.rm=T) - min(o, na.rm=T) ) * 100
    rmserr
}

#' Calculate center-of-mass.
#' \code{CalcCOM} calculates the time step of center of mass.
#' Calculate the time step when the center-of-mass of
#' a time series of values occurs.
#' @param x The time series vector.
#' @return The center-of-mass time step.
CalcCOM <- function (x) {
    cuml.x <- as.data.frame(CumsumNa(x)/sum(x, na.rm=T))
    colnames(cuml.x) <- c("x")
    cuml.x$ts <- seq(from = 1, to = length(cuml.x$x))
    tmp <- subset(cuml.x, cuml.x$x > 0.5)
    ts <- tmp$ts[1]
    ts
}

#' "Flatten" the output from GetMultiNcdf
#' \code{ReshapeMultiNcdf} flattens the output from GetMultiNcdf.
#' Take the output dataframe from GetMultiNcdf and reshape the dataframe
#' for ease of use in other functions.
#' @param myDf The output dataframe from GetMultiNcdf.
#' @return The reshaped output dataframe.
ReshapeMultiNcdf <- function(myDf) {
    newDF <- subset(myDf[,c("POSIXct","stat")], myDf$variableGroup==unique(myDf$variableGroup)[1])
    for (i in unique(myDf$variableGroup)) {
        newDF[,i] <- subset(myDf$value, myDf$variableGroup==i)
        }
    newDF$wy <- ifelse(as.numeric(format(newDF$POSIXct,"%m"))>=10,as.numeric(format(newDF$POSIXct,"%Y"))+1,as.numeric(format(newDF$POSIXct,"%Y")))
    newDF
}
