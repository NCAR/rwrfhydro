#' Get NLDAS2 forcings or model outputs.
#' 
#' \code{GetNldas2} downloads NLDAS-2 grids and creates a raster stack.
#' 
#' \code{GetNldas2} downloads NLDAS-2 for specified time period
#' and outputs a raster stack.
#' 
#' @param prodName Product to download. Options are: \itemize{
#' \item "FORA" (primary forcings)
#' \item "FORB" (secondary forcings)
#' \item "MOS" (Mosaic model output)
#' \item "NOAH" (Noah model output)
#' \item "VIC" (VIC model output)}
#' @param timeInt Time interval. Options are: \itemize{
#' \item "H" (hourly)
#' \item "M" (monthly)
#' \item "MC" (monthly climatology)}
#' @param outDir Output directory. This directory will be checked to see if the file
#' already exists.
#' @param startDate Date to start downloading data
#' @param endDate Date to end downloading data
#' @param quiet Flag whether or not to output messages (DEFAULT=TRUE)
#' @return raster stack object
#' @examples
#' sno.santafe <- GetSnotel(c("921","922"), series="Daily", startYr=2009, endYr=2010)
#' sno.niwot <- GetSnotel(c("663"), series="Hourly", startYr=2009, month=4)
#' sno.santafe <- GetSnotel(c("921","922"), intervaltype="Current", current="WEEK")
#' @keywords IO
#' @concept NLDAS
#' @family NLDAS
#' @export

GetNldas2 <- function(prodName, timeInt, outDir, 
                      startDate, endDate, quiet=TRUE) {
  
  # Check arguments
  if (!(prodName %in% c("FORA","FORB","MOS","NOAH","VIC"))) {
    stop("Incorrect product name. Options are: FORA, FORB, MOS, NOAH, VIC.")}
  if (!(prodName %in% c("H","M","MC"))) {
    stop("Incorrect time interval. Options are: H, M, MC.")}
  
  # Setup
  inServer="ftp://hydro1.sci.gsfc.nasa.gov/data/s4pa/NLDAS/"
  # initialize dataframe
  data <- data.frame()
  fileList <- c()
  nameList <- c()
  # Parse arguments for URL
  startYr <- as.integer(format(startDate, "%Y", tz="UTC"))
  endYr <- as.integer(format(endDate, "%Y", tz="UTC"))
  years <- seq(startYr, endYr, 1)
  startMo <- as.integer(format(startDate, "%m", tz="UTC"))
  endMo <- as.integer(format(endDate, "%m", tz="UTC"))
  
  inDir <- paste0("NLDAS_", prodName, "0125_", timeInt, ".002")
  
  i <- 1
  for (iyr in years) {
    # Monthly files
    if (timeInt == "M") {
      imo_start <- ifelse(iyr == startYr, startMo, 1)
      imo_end <- ifelse(iyr == endYr, endMo, 12)
      months <- seq(imo_start, imo_end, 1)
      for (imo in months) {
        if (nchar(imo)==1) imo <- paste0("0", imo)
        inFile <- paste0("NLDAS_", prodName, "0125_", timeInt, ".A", iyr, imo, ".002.grb")
        inNcFile <- sub(".grb",".nc",inFile)
        inUrl <- paste0(inServer, "/", inDir, "/", iyr, "/", inFile)
        # Check if GRB or NC file exists locally
        existsGRB <- (file.exists(paste0(outDir, "/", inFile)) & 
                        file.info(paste0(outDir, "/", inFile))$size>0)
        existsNC <- (file.exists(paste0(outDir, "/", inNcFile)) & 
                       file.info(paste0(outDir, "/", inNcFile))$size>0)
        if ( !( existsGRB | existsNC ) ) {
          # File does not exist so download grib
          out <- tryCatch(curl::curl_download(inUrl, paste0(outDir, "/", inFile), quiet=quiet), 
                          error=function(cond) {message(cond); return(NA)})
          if (!is.na(out)) {
            # File sucessfully downloaded, so convert to netcdf
            system(paste0("ncl_convert2nc ", paste0(outDir, "/", inFile)))
            fileList[i] <- paste0(outDir, "/", inNcFile)
            nameList[i] <- paste0(iyr,imo)
            i <- i+1
          }
        }
        else {
          # File already exists locally
          # Check if netcdf exists and convert if not
          if ( !existsNC ) system(paste0("ncl_convert2nc ", paste0(outDir, "/", inFile)))
          fileList[i] <- paste0(outDir, "/", inNcFile)
          nameList[i] <- paste0(iyr,imo)
          i <- i+1
        }
      }
    }
  }
 return() 
}

