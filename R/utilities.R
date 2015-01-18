#' GetPkgDataPath
#' 
#' \code{GetPkgData} is a simplified wrapper (for system.file) for loading external rwrfhydro data included with the package.
#' 
#' @param file The external data file to load (this is in dirrefent places before, rwrfhydro/inst/extdata, and after build, rwrfhydro/). 
#' @return The full path to the file.
#' @examples
#' GetPkgDataPath('Fourmile_test_case_AD.hydro_OrodellBasin_100m_8.nc')
#' #' @export
GetPkgDataPath <- function(theFile) system.file("extdata", theFile, package = "rwrfhydro")