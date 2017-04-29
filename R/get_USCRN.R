#' Get the US Climate Reference Network data from the NCEP ftp server. 
#' 
#' \code{GetCRN} Given the information provided to the function will download the data from NCEP ftp serverr
#' and then subset the data to meet the criteria defined such as being in a domian or being in specific time period. 
#' Return a dataframe containing. 
#' 
#' The contiguous U.S. network of 114 stations was completed in 2008. 
#' There are two USCRN stations in Hawaii and deployment of a network of 29 stations in Alaska continues. 
#' The vision of the USCRN program is to maintain a sustainable high-quality climate observation network.
#' 
#' These data are provided in 4 different temporal resolution (subhourly, hourly, daily and monthly),
#' and depending on the temporal resolution, the variables provided changes. Below is a summary of all the 
#' variables in each temporal resolution and their units. 
#' 
#' subhourly data contains: \itemize{
#' \item chech this README.txt \url{ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/subhourly01/README.txt}
#' \item AIR_TEMPERATURE                Celsius
#' \item PRECIPITATION                  (mm)
#' \item SOLAR_RADIATION                (W/m^2)
#' \item SR_FLAG                        (X)
#' \item SURFACE_TEMPERATURE            (Celsius)
#' \item ST_TYPE                        (X)
#' \item ST_FLAG                        (X)
#' \item RELATIVE_HUMIDITY              (\%)
#' \item RH_FLAG                        (X)
#' \item SOIL_MOISTURE_5                (m^3/m^3)
#' \item SOIL_TEMPERATURE_5             (Celsius)
#' \item WETNESS                        (Ohms)
#' \item WET_FLAG                       (X)
#' \item WIND_1_5                       (m/s)
#' \item WIND_FLAG                      (X)
#' } 
#' 
#'  hourly data contains: \itemize{
#' \item chech this README.txt \url{ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/hourly02/README.txt}
#' \item T_CALC                         (Celsius)
#' \item T_HR_AVG                       (Celsius)
#' \item T_MAX                          (Celsius)
#' \item T_MIN                          (Celsius)
#' \item P_CALC                         (mm)
#' \item SOLARAD                        (W/m^2)
#' \item SOLARAD_FLAG                   (X)
#' \item SOLARAD_MAX                    (W/m^2)
#' \item SOLARAD_MAX_FLAG               (X)
#' \item SOLARAD_MIN                    (W/m^2)
#' \item SOLARAD_MIN_FLAG               (X)
#' \item SUR_TEMP_TYPE                  (X)
#' \item SUR_TEMP                       (Celsius)
#' \item SUR_TEMP_FLAG                  (X)
#' \item SUR_TEMP_MAX                   (Celsius)
#' \item SUR_TEMP_MAX_FLAG              (X)
#' \item SUR_TEMP_MIN                   (Celsius)
#' \item SUR_TEMP_MIN_FLAG              (X)
#' \item RH_HR_AVG                      (\%)
#' \item RH_HR_AVG_FLAG                 (X)
#' \item SOIL_MOISTURE_5                (m^3/m^3)
#' \item SOIL_MOISTURE_10               (m^3/m^3)
#' \item SOIL_MOISTURE_20               (m^3/m^3)
#' \item SOIL_MOISTURE_50               (m^3/m^3)
#' \item SOIL_MOISTURE_100              (m^3/m^3)
#' \item SOIL_TEMP_5                    (Celsius)
#' \item SOIL_TEMP_10                   (Celsius)
#' \item SOIL_TEMP_20                   (Celsius)
#' \item SOIL_TEMP_50                   (Celsius)
#' \item SOIL_TEMP_100                  (Celsius)
#' } 
#' 
#'  daily data contains: \itemize{
#' \item chech this README.txt \url{ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/daily01/README.txt}
#' \item T_DAILY_MAX                    (Celsius)
#' \item T_DAILY_MIN                    (Celsius)
#' \item T_DAILY_MEAN                   (Celsius)
#' \item T_DAILY_AVG                    (Celsius)
#' \item P_DAILY_CALC                   (mm)
#' \item SOLARAD_DAILY                  (MJ/m^2)
#' \item SUR_TEMP_DAILY_TYPE            (X)
#' \item SUR_TEMP_DAILY_MAX             (Celsius)
#' \item SUR_TEMP_DAILY_MIN             (Celsius)
#' \item SUR_TEMP_DAILY_AVG             (Celsius)
#' \item RH_DAILY_MAX                   (\%)
#' \item RH_DAILY_MIN                   (\%)
#' \item RH_DAILY_AVG                   (\%)
#' \item SOIL_MOISTURE_5_DAILY          (m^3/m^3)
#' \item SOIL_MOISTURE_10_DAILY         (m^3/m^3)
#' \item SOIL_MOISTURE_20_DAILY         (m^3/m^3)
#' \item SOIL_MOISTURE_50_DAILY         (m^3/m^3)
#' \item SOIL_MOISTURE_100_DAILY        (m^3/m^3)
#' \item SOIL_TEMP_5_DAILY              (Celsius)
#' \item SOIL_TEMP_10_DAILY             (Celsius)
#' \item SOIL_TEMP_20_DAILY             (Celsius)
#' \item SOIL_TEMP_50_DAILY             (Celsius)
#' \item SOIL_TEMP_100_DAILY            (Celsius)
#' } 
#' 
#'  monthly data contains: \itemize{
#' \item chech this README.txt \url{ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/monthly01/README.txt}
#' \item T_MONTHLY_MAX                  (Celsius)
#' \item T_MONTHLY_MIN                  (Celsius)
#' \item T_MONTHLY_MEAN                 (Celsius)
#' \item T_MONTHLY_AVG                  (Celsius)
#' \item P_MONTHLY_CALC                 (mm)
#' \item SOLRAD_MONTHLY_AVG             (MJ/m^2)
#' \item SUR_TEMP_MONTHLY_TYPE          (X)
#' \item SUR_TEMP_MONTHLY_MAX           (Celsius)
#' \item SUR_TEMP_MONTHLY_MIN           (Celsius)
#' \item SUR_TEMP_MONTHLY_AVG           (Celsius)
#' } 
#'  
#' 
#' @param gageNames Character vector of the gage names which is used in the file names (or any part of the file name) 
#' @param siteIds Characcter vector of the stations IDs so called WBANNO.
#' @param elements Character vector containing all elements required from the data. 
#' @param timestep Character (DEFAULT = "hourly"). It can be subhourly, hourly, daily and monthly
#' @param domain data.frame, containing four columns named lonmax, latmin, latmax, latmin. 
#' It will be used to subset the data to only those gauges falling into the rectangle domain
#' @param startDate POSIXct, ending date
#' @param endDate POSIXct, strating date
#' @param realTime Logical (DEFAULT = FALSE)
#' @param parallel Logical (DEFAULT = FALSE)
#' 
#' @return A data.table containing the US CRN data.
#' 
#' @examples
#' example 1: getting the precipition data at the subhourly time steps, 
#' for the first three months of 2016
#' startDate = as.POSIXct("2016-01-01 00:00:00", format= "%Y-%m-%d %H:%M:%S", tz = "UTC")
#' endDate   = as.POSIXct("2016-04-01 00:00:00", format= "%Y-%m-%d %H:%M:%S", tz = "UTC")
#' output <- GetCRN(elements = "P_CALC", timestep = "hourly", 
#' startDate = startDate, endDate= endDate)
#' 
#' # choose only gauge in the following domain
#' domain <- data.frame(minlon = -80, maxlon = -60, minlat = 30, maxlat = 50)
#' output <- GetCRN(elements = "P_CALC", timestep = "hourly", 
#' startDate = startDate, endDate= endDate, domain = domain)
#' 
#' # choose only gauges in the Colorado 
#' outputHourly <- GetCRN(gageNames = "CO", elements = "P_CALC", timestep = "hourly", 
#' startDate = startDate, endDate= endDate)
#' 
#' # get daily data only for basins in colorado
#' outputDaily <- GetCRN(gageNames = "CO", elements = "P_DAILY_CALC", timestep = "daily", 
#' startDate = startDate, endDate= endDate)
#' 
#' # get monthly data only for basins in colorado
#' outputMonthly <- GetCRN(gageNames = "CO", elements = "P_MONTHLY_CALC", timestep = "monthly", 
#' startDate = startDate, endDate= endDate)
#' 
#' # get subhourly data only for basins in colorado
#' outputsubhourly <- GetCRN(gageNames = "CO", elements = "PRECIPITATION", 
#' timestep = "subhourly", startDate = startDate, endDate= endDate)
#' 
#' # get hourly soik moisure data
#' outputHourly <- GetCRN(gageNames = "CO", elements = c("SOIL_MOISTURE_5", 
#' "SOIL_MOISTURE_10", "SOIL_MOISTURE_20", "SOIL_MOISTURE_50", "SOIL_MOISTURE_100"), 
#' timestep = "hourly")

#' @keywords IO
#' @concept USCRN
#' @family USCRN
#' @export

GetCRN <- function(gageNames = NULL, siteIds= NULL, elements = NULL, timestep = "hourly", domain = NULL, 
                   startDate = Sys.time() - 10*24*3600, endDate = Sys.time(), realTime = FALSE, parallel = FALSE){
  
  # These addresses are fixed and need to be changes if NCEP changes their serving address.
  if (timestep == "subhourly") url_uscrn = "ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/subhourly01/"
  if (timestep == "hourly")    url_uscrn = "ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/hourly02/"
  if (timestep == "daily")     url_uscrn = "ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/daily01/"
  if (timestep == "monthly")   url_uscrn = "ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/monthly01/"
  
  # Since the folders are arranged by yeas, so we need to find out which years shoul be processed.
  years <- seq(lubridate::year(startDate), lubridate::year(endDate))
  
  # reading the hear information which is distributed in a separate file
  headerInfo <- utils::read.table(paste0(url_uscrn,"/HEADERS.txt"), stringsAsFactors = FALSE) 
  
  if (timestep == "monthly") {
    urlPaths <- paste0(url_uscrn)
  } else {
    urlPaths <- paste0(url_uscrn,years,"/") 
  }
  
  # get the name of all the files under the requested years
  filenames <- foreach::"%do%"(foreach::foreach(url = urlPaths, .combine=function(...) data.table::rbindlist(list(...))), {
    #  print(url)
    filenames <- RCurl::getURL(url, dirlistonly = TRUE)
    filenames <- paste0(url,strsplit(filenames, "\r*\n")[[1]])  # split the name of the files
    return(filenames)
  })
  
  # To remove other files rather than the CRN files
  filenames <- unique(grep("CRN", filenames, value=TRUE))
  
  # if part of the file names has been given, find only those files and open those
  if (!is.null(gageNames)) filenames <- unique(grep(paste(gageNames,collapse="|"), filenames, value=TRUE))
  
  ######---------------------> 
  # function for reading the files according to the timeStep
  
  ReadTheFile <- function(file){
    
    if (timestep == "subhourly"){
      dat <- tryCatch(readr::read_fwf(file, 
                                      col_positions = readr::fwf_positions(c(1, 7, 16, 21, 30, 35, 42, 50, 58, 66, 74, 81, 83, 91, 93, 95, 101, 103, 111, 119,
                                                                             125, 127, 134),
                                                                           c(5, 14, 19, 28, 33, 40, 48, 56, 64, 72, 79, 81, 89, 91, 93, 99, 101, 109, 117, 123,
                                                                             125, 132, 134)),
                                      col_types = paste(c(rep('c', 6), rep('d', 5),'cdccdcdddcdc'), collapse = '')),
                      error=function(cond) {message(cond); return(NA)})                              
    } else  if (timestep == "hourly") {
      dat <- tryCatch(readr::read_fwf(file, 
                                      col_positions = readr::fwf_positions(c(1, 7, 16, 21, 30, 35, 42, 50, 58, 66, 74, 82, 90, 98, 105, 107, 114, 116, 123, 125,
                                                                             127, 135, 137, 145, 147, 155, 157, 163, 165, 173, 181, 189, 197, 205, 213, 221, 229, 237),
                                                                           c(5, 14, 19, 28, 33, 40, 48, 56, 64, 72, 80, 88, 96, 103, 105, 112, 114, 121, 123, 125,
                                                                             133, 135, 143, 145, 153, 155, 161, 163, 171, 179, 187, 195, 203, 211, 219, 227, 235, 243)),
                                      col_types = paste(c(rep('c', 6), rep('d', 8),'cdcdccdcdcdcdc', rep('d',10)), collapse = '')),
                      error=function(cond) {message(cond); return(NA)})        
    } else  if (timestep == "daily")   {
      dat <- tryCatch(readr::read_fwf(file, 
                                      col_positions = readr::fwf_positions(c(1, 7, 16, 23, 31, 39, 47, 55, 63, 71, 79, 88, 90, 98, 106, 114, 122, 130, 138, 146, 154,
                                                                             162, 170, 178, 186, 194, 202, 210),
                                                                           c(5, 14, 21, 29, 37, 45, 53, 61, 69, 77, 86, 88, 96, 104, 112, 120, 128, 136, 144, 152, 160, 
                                                                             168, 176, 184, 192, 200, 208, 216)),
                                      col_types = paste(c(rep('c', 3), rep('d', 8),'c', rep('d',16)), collapse = '')),
                      error=function(cond) {message(cond); return(NA)}) 
    } else  if (timestep == "monthly") {
      dat <- tryCatch(readr::read_fwf(file, 
                                      col_positions = readr::fwf_positions(c(1, 7, 14, 21, 31, 41, 49, 57, 65, 73, 81, 89, 91, 99, 107),
                                                                           c(5, 12, 19, 29, 39, 47, 55, 63, 71, 79, 87, 89, 97, 105, 113)),
                                      col_types = paste(c('cccddddddddcddd'), collapse = '')),
                      error=function(cond) {message(cond); return(NA)}) 
    } else {
      
      stop("the step could only be one of the followings: subhourly, hourly, daily, monthly")
      
    }
    names(dat) <- headerInfo[2,]
    
    # only those with the requested siteIds to be selected
    if (!is.null(siteIds)) dat <- subset(dat, WBANNO %in% siteIds )
    
    # only those in the requested domain to be selected
    if (timestep == "monthly") {
      if (!is.null(domain)) dat <- subset(dat, PRECISE_LONGITUDE >= domain$lonmin & PRECISE_LONGITUDE <= domain$lonmax 
                                          & PRECISE_LATITUDE >= domain$latmin & PRECISE_LATITUDE <= domain$latmax)
    } else {
      
      if (!is.null(domain)) dat <- subset(dat, LONGITUDE >= domain$lonmin & LONGITUDE <= domain$lonmax 
                                          & LATITUDE >= domain$latmin & LATITUDE <= domain$latmax)
    } 
    
    if (timestep %in% c("subhourly", "hourly")) {
      
      # only those in the time period requested should be selected
      dat$POSIXct <- as.POSIXct(paste0(dat$UTC_DATE, dat$UTC_TIME), format = "%Y%m%d%H%M", tz = "UTC")
      dat <- subset(dat, POSIXct >= startDate & POSIXct <= endDate)
      
      # only those elements requested to be returned
      if (!any(is.null(elements))) dat <- dat[, c(names(dat)[1:8],elements,"POSIXct")]
      
    } else if (timestep == "daily") {
      
      dat$Date <- as.Date(paste0(dat$LST_DATE), format = "%Y%m%d", tz = "UTC")
      dat <- subset(dat, Date >= as.Date(startDate) & Date <= as.Date(endDate))
      
      # only those elements requested to be returned
      if (!any(is.null(elements))) dat <- dat[, c(names(dat)[1:5],elements)]
      
    } else if (timestep == "monthly") {
      
      dat$Date <- as.Date(paste0(dat$LST_YRMO, "01"), format = "%Y%m%d", tz = "UTC")
      dat <- subset(dat, Date >= as.Date(startDate) & Date <= as.Date(endDate))
      
      # only those elements requested to be returned
      if (!any(is.null(elements))) dat <- dat[, c(names(dat)[1:5],elements)]
      
    }
    return(data.table::as.data.table(dat))
  }
  
  # The parallel option does not work properly, the FTP server sends and error
  if (parallel){
    
    datTotal <- foreach::"%dopar%"(foreach::foreach(file = filenames, .combine=function(...) data.table::rbindlist(list(...))), {
      print(paste0("reading this file :",file))
      dat <- ReadTheFile(file)
      return(dat)
    })
    
  } else {
    
    datTotal <- foreach::"%do%"(foreach::foreach(file = filenames, .combine=function(...) data.table::rbindlist(list(...))), {
      print(paste0("reading this file :",file))
      dat <- ReadTheFile(file)
      return(dat)
    })
    
  }
  return(datTotal)
}

