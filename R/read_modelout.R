#' Read WRF-Hydro standard-format forecast points output text file
#'
#' \code{ReadFrxstptsOut} reads in WRF-Hydro forecast points output text file.
#'
#' \code{ReadFrxstptsOut} reads a standard-format WRF-Hydro forecast points output text
#' file and creates a dataframe with consistent date and data columns for use with other
#' rwrfhydro tools.
#' 
#' @param pathOutfile The full pathname to the WRF-Hydro forecast points text file
#' (frxst_pts_out.txt).
#' @return A dataframe containing the forecast points output flow data.
#'
#' @examples
#' ## Take a forecast point output text file for an hourly model run of Fourmile Creek
#' ## and return a dataframe.
#'
#' modStr1h.mod1.fc <- ReadFrxstptsOut("../OUTPUT/frxst_pts_out.txt")
#' @export

ReadFrxstptsOut <- function(pathOutfile) {
    myobj <- read.table(pathOutfile, header=F, sep=",", colClasses=c("character","character","integer","numeric","numeric","numeric","numeric","numeric"), na.strings=c("********","*********","************"))
    colnames(myobj) <- c("secs","timest","st_id","st_lon","st_lat","q_cms","q_cfs","dpth_m")
    myobj$POSIXct <- as.POSIXct(as.character(myobj$timest), format="%Y-%m-%d %H:%M:%S", tz="UTC")
    myobj$wy <- ifelse(as.numeric(format(myobj$POSIXct,"%m"))>=10, as.numeric(format(myobj$POSIXct,"%Y"))+1, as.numeric(format(myobj$POSIXct,"%Y")))
myobj
}


#' Read WRF-Hydro standard-format groundwater output text file
#'
#' \code{ReadGwOut} reads in WRF-Hydro groundwater output text file.
#'
#' \code{ReadGwOut} reads a standard-format WRF-Hydro groundwater output text file
#' (GW_inflow.txt, GW_outflow.txt, or GW_zlev.txt) and creates a dataframe with consistent
#' date and data columns for use with other rwrfhydro tools.
#' 
#' @param pathOutfile The full pathname to the WRF-Hydro groundwater text file
#' (GW_inflow.txt, GW_outflow.txt, or GW_zlev.txt).
#' @return A dataframe containing the groundwater data.
#'
#' @examples
#' ## Take a groundwater outflow text file for an hourly model run of Fourmile Creek
#' ## and return a dataframe.
#'
#' modGWout1h.mod1.fc <- ReadGwOut("../OUTPUT/GW_outflow.txt")
#' @export

ReadGwOut <- function(pathOutfile) {
    myobj <- read.table(pathOutfile,header=F)
    if ( grepl("GW_zlev", pathOutfile) ) {
        colnames(myobj) <- c("basin","timest","zlev_mm")
        }
    else {
        colnames(myobj) <- c("basin","timest","q_cms")
        myobj$q_cfs <- myobj$q_cms/(0.3048^3)
        }
    myobj$POSIXct <- as.POSIXct(as.character(myobj$timest), format="%Y-%m-%d_%H:%M:%S",tz="UTC")
    myobj$wy <- ifelse(as.numeric(format(myobj$POSIXct,"%m"))>=10, as.numeric(format(myobj$POSIXct,"%Y"))+1, as.numeric(format(myobj$POSIXct,"%Y")))
    myobj
}
