#' GetPkgDataPath
#' 
#' \code{GetPkgData} is a simplified wrapper (for system.file) for loading external rwrfhydro data included with the package.
#' 
#' @param theFile The external data file to load (this is in dirrefent places before, rwrfhydro/inst/extdata, and after build, rwrfhydro/). 
#' @return The full path to the file.
#' @examples
#' GetPkgDataPath('Fourmile_test_case_AD.hydro_OrodellBasin_100m_8.nc')
#' #' @export
GetPkgDataPath <- function(theFile) system.file("extdata", theFile, package = "rwrfhydro")


# internal
## Standardize longitudes
StdLon <- function(x) {
  x[which(x>180.)] <- x[which(x>180.)]-360.
  x
}

# internal
## Pad a range.
padRange <- function(limits, delta=diffMult*diff(limits), diffMult=.05) limits+c(-delta,delta)
