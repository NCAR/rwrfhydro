#' Read standard-format text data table downloaded from USGS Water Data
#' 
#' \code{ReadUsgsGage} reads USGS data table (streamflow and/or stage) and puts
#' into a dataframe.
#' 
#' \code{ReadUsgsGage} reads a streamflow or stage time series data table
#' (standard USGS Water Data format) and outputs a dataframe with consistent
#' date and data columns for use with other rwrfhydro tools.
#' 
#' @param pathGageData The full pathname to the streamflow/stage time series
#'   text file as downloaded from USGS Water Data. The file should include the
#'   standard USGS header info and the data time series with standard USGS
#'   columns.
#' @param returnEnglish Logical: Return variables in english units (cfs and ft)?
#' @param returnMetric  Logical: Return variables in these units (cms and m)?
#' @param timeZone  (OPTIONAL) The time zone for the USGS gage data. Only
#'   necessary if tz code is NOT provided in the input file (e.g., daily
#'   streamflow gage data files). Time zone name must be R-friendly for your
#'   current OS. See:
#'   \url{http://stat.ethz.ch/R-manual/R-devel/library/base/html/timezones.html}
#' @return A dataframe containing the USGS flow and/or stage data.
#'   
#' @examples
#' ## Take a text file downloaded from the USGS Water Data website for 
#' ## 5-minute flow at Fourmile Creekand create a dataframe called
#' ## "obsStr5min.fc".
#' \dontrun{
#' obsStr5min.fc <- 
#'   ReadUsgsGage("../OBS/STRFLOW/5min_str_06727500_110401_140810.txt")
#' obsStr5min.fc <- 
#'   ReadUsgsGage("../OBS/STRFLOW/5min_str_06727500_110401_140810.txt, 
#'                returnEnglish=FALSE")
#' }
#' @keywords IO
#' @concept dataGet
#' @family obsDataReads
#' @export
ReadUsgsGage <- function(pathGageData, returnMetric=TRUE, 
                         returnEnglish=TRUE, timeZone=NULL) {

  outDf <- read.table(pathGageData, sep="\t", na.strings=c("","Rat","Mnt"),
                      stringsAsFactors=F, comment.char="#")
  outDf.head <- outDf[1,]
  outDf <- outDf[3:nrow(outDf),]

  cdlist <- list("00060"="q_cfs",
                 "00061"="qinst_cfs",
                 "00062"="reslev_ft",
                 "00064"="strlev_ft",
                 "00065"="ht_ft",
                 "00060_00003"="qmean_cfs",
                 "00060_00001"="qmax_cfs",
                 "00060_00002"="qmin_cfs",
                 "00065_00003"="htmean_ft",
                 "00065_00001"="htmax_ft",
                 "00065_00002"="htmin_ft")

  for (i in names(cdlist)) { # if grep dosent match, the zero index is set.
    indx <- grep(paste(i,"$",sep=""),outDf.head)
    outDf.head[indx] <- cdlist[[i]]
    indx <- grep(paste(i,"_cd",sep=""),outDf.head)
    outDf.head[indx] <- paste(cdlist[[i]],"_cd",sep="")
  }
  colnames(outDf) <- as.matrix(outDf.head)
  for (i in names(cdlist)) {
    if (cdlist[[i]] %in% colnames(outDf)) {
      outDf[,cdlist[[i]]] <- as.numeric(outDf[,cdlist[[i]]])
    }
  }
  
  if (is.null(timeZone)) {
    timeZone <- TransTz(outDf$tz_cd)
    # Since a gauge shouldnt move, there should only be one time zone
    if(!all(timeZone == timeZone[1]))
      warning(paste0("There should only be one timezone for a given gauge, ",
                   "please check the file: ",pathGageData))
    timeZone <- timeZone[1]
  }
  # instantaneous vs daily average values
  usgsTimeFmt <- if (grepl(" ",outDf$datetime[1])) {
     "%Y-%m-%d %H:%M" # instantaneous
  } else { "%Y-%m-%d" }  # daily avg

  outDf$POSIXct <- as.POSIXct(as.character(outDf$datetime), format=usgsTimeFmt, 
                              tz=timeZone)

  outDf$wy <- CalcWaterYear(outDf$POSIXct)
  #outDf$wy <- ifelse(as.numeric(format(outDf$POSIXct,"%m"))>=10,
  #                   as.numeric(format(outDf$POSIXct,"%Y"))+1,
  #                   as.numeric(format(outDf$POSIXct,"%Y")))

  if (returnMetric) {
    ftToM <- 0.3048
    if ("q_cfs"     %in% colnames(outDf))      outDf$q_cms  <- outDf$q_cfs*(ftToM^3)
    if ("qinst_cfs" %in% colnames(outDf))  outDf$qinst_cms  <- outDf$qinst_cfs*(ftToM^3)
    if ("qmean_cfs" %in% colnames(outDf))  outDf$qmean_cms  <- outDf$qmean_cfs*(ftToM^3)
    if ("qmin_cfs"  %in% colnames(outDf))   outDf$qmin_cms  <- outDf$qmin_cfs*(ftToM^3)
    if ("qmax_cfs"  %in% colnames(outDf))   outDf$qmax_cms  <- outDf$qmax_cfs*(ftToM^3)
    if ("reslev_ft" %in% colnames(outDf))   outDf$reslev_m  <- outDf$reslev_ft*(ftToM)
    if ("strlev_ft" %in% colnames(outDf))   outDf$strlev_m  <- outDf$strlev_ft*(ftToM)
    if ("ht_ft"     %in% colnames(outDf))       outDf$ht_m  <- outDf$ht_ft*(ftToM)
    if ("htmean_ft" %in% colnames(outDf))   outDf$htmean_m  <- outDf$htmean_ft*(ftToM)
    if ("htmin_ft"  %in% colnames(outDf))    outDf$htmin_m  <- outDf$htmin_ft*(ftToM)
    if ("htmax_ft"  %in% colnames(outDf))    outDf$htmax_m  <- outDf$htmax_ft*(ftToM)
  }

  ## dont return nothing!
  if(!returnEnglish & returnMetric) {
    englishCols <- which(grepl('_cfs$|cfs_cd$|_ft$|_ft_cd$',colnames(outDf)))
    outDf <- outDf[,-1*englishCols]
    ## problem is that this kills the codes, should translated the codes. 
  }
  
  outDf
}


#' Read standard-format text data table downloaded from CO DWR
#' 
#' \code{ReadCoDwrGage} reads CO DWR gage data table (streamflow and/or stage)
#' and puts into a dataframe.
#' 
#' \code{ReadCoDwrGage} reads a streamflow or stage time series data table
#' (standard CO DWR data format) and outputs a dataframe with consistent date
#' and data columns for use with other rwrfhydro tools.
#' 
#' @param pathGageData The full pathname to the streamflow/stage time series
#'   text file as downloaded from CO DWR. The file should include the standard
#'   CO DWR header info and the data time series with standard CO DWR columns.
#' @param returnEnglish Logical: Return variables in english units (cfs and ft)
#' @param returnMetric  Logical: Return variables in these units (cms and m)
#' @param timeZone  (OPTIONAL) The time zone for the gage data
#'   (DEFAULT="America/Denver", which is MST/MDT). Time zone name must be
#'   R-friendly for your current OS. See:
#'   \url{http://stat.ethz.ch/R-manual/R-devel/library/base/html/timezones.html}
#' @return A dataframe containing the flow and/or stage data.
#'   
#' @examples
#' ## Take a text file downloaded from the CO DWR website for 15-minute flow 
#' ## at Alamosa River and create a dataframe called "obsStr15min.alaterco".
#' 
#' \dontrun{
#' obsStr15min.alaterco <- 
#'   ReadCoDwrGage("../OBS/STRFLOW/ALATERCO_41715103512.txt")
#' obsStr15min.alaterco <- 
#'   ReadCoDwrGage("../OBS/STRFLOW/ALATERCO_41715103512.txt", returnEnglish=FALSE)
#' }
#' @keywords IO
#' @concept dataGet
#' @family obsDataReads
#' @export
ReadCoDwrGage <- function(pathGageData, returnMetric=TRUE, returnEnglish=TRUE, 
                          timeZone="America/Denver") {
  # Manually remove commented lines since cannot automate due to mismatch 
  # between column names and column count
  ncomm <- as.integer(system(paste('grep "#"', pathGageData, '| wc -l'), intern=TRUE))
  outDf <- read.table(pathGageData, sep="\t", stringsAsFactors=F, skip=ncomm+1,
                      na.strings=c("B","Bw","Dis","E","Eqp","Ice","M","na","nf","Prov",
                                   "Rat","S","Ssn","wtr op","----"))
  # Deal with header (not 1:1 with data columns)
  outDf.tmp <- read.table(pathGageData, sep="\t", na.strings=c(""),
                           stringsAsFactors=F, skip=ncomm, nrows=1)
  outDf.head <- c()
  outDf.units <- c()
  for (i in 1:ncol(outDf.tmp)) {
    outDf.head[i] <- unlist(strsplit(outDf.tmp[1,i], split=" ", fixed=TRUE))[1]
    outDf.units[i] <- unlist(strsplit(outDf.tmp[1,i], split=" ", fixed=TRUE))[2]
  }
  # Deal with duplicate "Date/Time" header. We need to know the index so 
  # we can remove it from "units" too.
  tmp<-grep("Date/Time", outDf.head, fixed=TRUE)
  outDf.head[tmp[2]]<-"datetime"
  outDf.head<-c(outDf.head[1:(tmp[1]-1)], outDf.head[(tmp[1]+1):length(outDf.head)])
  outDf.units<-c(outDf.units[1:(tmp[1]-1)], outDf.units[(tmp[1]+1):length(outDf.units)])
  colnames(outDf) <- outDf.head
  names(outDf.units)<-outDf.head
  attr(outDf, "units")<-outDf.units
  
  # Unit conversions
  if ("DISCHRG" %in% names(outDf)) {
      if (attributes(outDf)$units[["DISCHRG"]] == "(cfs)") {
          if (returnMetric) { outDf$q_cms <- outDf$DISCHRG * (0.3048^3) }
          if (returnEnglish) { outDf$q_cfs <- outDf$DISCHRG }
          }
      if (attributes(outDf)$units[["DISCHRG"]] == "(cms)") {
            if (returnMetric) { outDf$q_cms <- outDf$DISCHRG }
            if (returnEnglish) { outDf$q_cfs <- outDf$DISCHRG / (0.3048^3) }
          }
  }
  if ("GAGE_HT" %in% names(outDf)) {
    if (attributes(outDf)$units[["GAGE_HT"]] == "(ft)") {
      if (returnMetric) { outDf$ht_m <- outDf$GAGE_HT * (0.3048) }
      if (returnEnglish) { outDf$ht_ft <- outDf$GAGE_HT }
    }
    if (attributes(outDf)$units[["GAGE_HT"]] == "(m)") {
      if (returnMetric) { outDf$ht_m <- outDf$GAGE_HT }
      if (returnEnglish) { outDf$ht_ft <- outDf$GAGE_HT / (0.3048) }
    }
  }
  # Deal with time
  #if (is.null(timeZone)) { timeZone <- "MST" } 
  # assuming local time for all gages, no daylight savings?
  usgsTimeFmt <- if (grepl(" ",outDf$datetime[1])) {
    "%Y-%m-%d %H:%M" # instantaneous
  } else { "%Y-%m-%d" }  # daily avg
  outDf$POSIXct <- as.POSIXct(as.character(outDf$datetime), format=usgsTimeFmt, tz=timeZone)
  # Screen out NA times, since CO DWR leaves in null values for the 
  # missing daylight savings hour.
  outDf <- subset(outDf, !is.na(outDf$POSIXct))
  outDf$wy <- CalcWaterYear(outDf$POSIXct)
  
  outDf
}


#' Read standard-format NetCDF data downloaded from Ameriflux
#' 
#' \code{ReadAmerifluxNC} reads Ameriflux data table (Level 2 standardized
#' NetCDF file) and creates a dataframe.
#' 
#' \code{ReadAmerifluxNC} reads an Ameriflux Level 2 standardized NetCDF file
#' and outputs a dataframe with consistent date and data columns for use with
#' other rwrfhydro tools.
#' 
#' @param pathFluxData The full pathname to the flux time series NetCDF file as
#'   downloaded from an Ameriflux data server.
#' @param timeZone The time zone for the flux site. Time zone name must be
#'   R-friendly for your current OS. See:
#'   \url{http://stat.ethz.ch/R-manual/R-devel/library/base/html/timezones.html}. 
#'   Provide this OR a UTC offset. Note that Ameriflux data is (allegedly!) in local
#'   time without daylight savings. 
#' @param utcOffset The UTC offset for the flux site's local time. These should be in 
#' positive or negative hours (e.g., -7 for "America/Denver" with no DST shift). 
#' These values will supercede any provided time zones. See tzLookup for a list of 
#' time zone UTC offsets.
#' @return A dataframe containing the Ameriflux data.
#'   
#' @examples
#' ## Takes a NetCDF file downloaded from the ORNL Amerifux website for US-NC2
#' ## (North Carolina Loblolly Pine) and returns a dataframe.
#' 
#' \dontrun{
#' obsFlux30min.usnc2 <- 
#'   ReadAmerifluxNC("../OBS/FLUX/AMF_USNC2_2005_L2_WG_V003.nc", utcOffset=-5)
#' }
#' @keywords IO
#' @concept dataGet
#' @family obsDataReads
#' @export
ReadAmerifluxNC <- function(pathFluxData, timeZone=NULL, utcOffset=NULL) {
    if ((is.null(utcOffset)) & (is.null(timeZone))) {
      stop("No time zone or UTC offset provided. Please determine the site's local time zone and re-run the tool.")
    }
    ncFile <- ncdf4::nc_open(pathFluxData)
    nc <- ncFile$nvars
    nr <- ncFile$var[[1]]$varsize
    outDf <- as.data.frame(matrix(nrow=nr, ncol=nc))
    ncVarList <- list()
    for (i in 1:nc ) {
        ncVar <- ncFile$var[[i]]
        ncVarList[i] <- ncVar$name
        outDf[,i] <- ncdf4::ncvar_get( ncFile, ncVar )
    }
    colnames(outDf) <- ncVarList
    ncdf4::nc_close(ncFile)
    if (is.null(utcOffset)) {
      outDf$POSIXct <- as.POSIXct( paste(as.character(outDf$YEAR), as.character(outDf$DOY),
                                         as.character(ifelse(substr(outDf$HRMIN,1,
                                                                    nchar(outDf$HRMIN)-2)=='', 
                                                             "00", substr(outDf$HRMIN,1,
                                                                      nchar(outDf$HRMIN)-2))),
                                         as.character(substr(outDf$HRMIN,
                                                             nchar(outDf$HRMIN)-1,
                                                             nchar(outDf$HRMIN))), sep="-"),
                                   format="%Y-%j-%H-%M", tz=timeZone )
    } else {
      outDf$POSIXct <- as.POSIXct( paste(as.character(outDf$YEAR), as.character(outDf$DOY),
                                         as.character(ifelse(substr(outDf$HRMIN,1,
                                                                    nchar(outDf$HRMIN)-2)=='', 
                                                             "00", substr(outDf$HRMIN,1,
                                                                      nchar(outDf$HRMIN)-2))),
                                         as.character(substr(outDf$HRMIN,
                                                             nchar(outDf$HRMIN)-1,
                                                             nchar(outDf$HRMIN))), sep="-"),
                                   format="%Y-%j-%H-%M", tz="UTC" ) - (utcOffset*3600)
    }
    outDf$wy <- CalcWaterYear(outDf$POSIXct)
    outDf
}



#' Read standard-format CSV data downloaded from Ameriflux
#' 
#' \code{ReadAmerifluxCSV} reads Ameriflux data table (Level 2 standardized CSV
#' file) and creates a dataframe.
#' 
#' \code{ReadAmerifluxCSV} reads an Ameriflux Level 2 standardized CSV file and
#' outputs a dataframe with consistent date and data columns for use with other
#' rwrfhydro tools.
#' 
#' @param pathFluxData The full pathname to the flux time series CSV file as
#'   downloaded from an Ameriflux data server.
#' @param timeZone The time zone for the flux site. Time zone name must be
#'   R-friendly for your current OS. See:
#'   \url{http://stat.ethz.ch/R-manual/R-devel/library/base/html/timezones.html}. 
#'   Provide this OR a UTC offset. Note that Ameriflux data is (allegedly!) in local
#'   time without daylight savings. 
#' @param utcOffset The UTC offset for the flux site's local time. These should be in 
#' positive or negative hours (e.g., -7 for "America/Denver" with no DST shift). 
#' These values will supercede any provided time zones. See tzLookup for a list of 
#' time zone UTC offsets.
#' @return A dataframe containing the Ameriflux data.
#'   
#' @examples
#' ## Takes a CSV file downloaded from the ORNL Amerifux website for US-NR1 (Niwot Ridge)
#' ## and returns a dataframe.
#' 
#' \dontrun{
#' obsFlux30min.usnr1 <- 
#'   ReadAmerifluxCSV("../OBS/FLUX/AMF_USNR1_2013_L2_GF_V008.csv", utcOffset=-7)
#' }
#' @keywords IO
#' @concept dataGet
#' @family obsDataReads
#' @export
ReadAmerifluxCSV <- function(pathFluxData, timeZone=NULL, utcOffset=NULL) {
    if ((is.null(utcOffset)) & (is.null(timeZone))) {
      stop("No time zone or UTC offset provided. Please determine the site's local time zone and re-run the tool.")
    }
    outDf <- read.table(pathFluxData, sep=",", skip=20, na.strings=c(-6999,-9999), 
                        strip.white=T)
    outDf.head <- read.table(pathFluxData, sep=",", skip=17, nrows=1, strip.white=T)
    colnames(outDf) <- as.matrix(outDf.head)[1,]
    if (is.null(utcOffset)) {
      outDf$POSIXct <- as.POSIXct( paste(as.character(outDf$YEAR), as.character(outDf$DOY),
                        as.character(ifelse(substr(outDf$HRMIN,1,nchar(outDf$HRMIN)-2)=='', 
                                            "00", substr(outDf$HRMIN,1,nchar(outDf$HRMIN)-2))),
                        as.character(substr(outDf$HRMIN,nchar(outDf$HRMIN)-1,
                                            nchar(outDf$HRMIN))), sep="-"),
                        format="%Y-%j-%H-%M", tz=timeZone )
    } else {
      outDf$POSIXct <- as.POSIXct( paste(as.character(outDf$YEAR), as.character(outDf$DOY),
                                         as.character(ifelse(substr(outDf$HRMIN,1,
                                                                    nchar(outDf$HRMIN)-2)=='', 
                                                             "00", substr(outDf$HRMIN,1,
                                                                      nchar(outDf$HRMIN)-2))),
                                         as.character(substr(outDf$HRMIN,nchar(outDf$HRMIN)-1,
                                                             nchar(outDf$HRMIN))), sep="-"),
                                   format="%Y-%j-%H-%M", tz="UTC" ) - (utcOffset*3600)
      }
    outDf$wy <- CalcWaterYear(outDf$POSIXct)
    outDf
}

#' Download Stage IV and II data files
#' 
#' \code{GetStage4Files} downloads Stage IV and Stage II data files
#' 
#' \code{GetStage4Files} downloads Stage IV and Stage II data files from NCEP 
#' ftp site based on user configurations. The function downloads Stage IV or 
#' Stage II data from NCEP ftp site, unpacks them, and removes uncessary 
#' files (e.g., *.gif). Note for Stage II, only the multi-sensor ("ml") data 
#' files are kept while others are removed. Both Stage IV & Stage II datasets 
#' contain hourly, 6-hourly, and daily precipitation data.
#' 
#' @author Yuqiong.Liu@@noaa.gov (updated by aubreyd)
#' @param startDate start date + hour in the form of YYYYMMDDHH
#' @param endDate end date + hour in the form of YYYYMMDDHH
#' @param dataTyp name of the dataset: stage4 or stage2
#' @param dataDir directory to store the data, subdirectories YYYY/MM will be created
#' @param keepDownloads logical for whether to keep original download files;
#' if FALSE, dummy empty files will be used for tracking instead
#' to conserve space (DEFAULT=FALSE)
#' @param runParallel logical for running in parallel mode (must have a parallel
#' backend installed and registered (e.g., doMC or doParallel) (DEFAULT=FALSE)
#' @return NULL
#' #' @examples
#' ## Pull data for May 1-5, 2015.
#' \dontrun{
#' datadir <- "~/wrfHydroTestCases/OBS/Stage_II/"
#' GetStage4Files(startDate="2015050100", endDate="2015050500", 
#'                  dataTyp="stage2", dataDir=datadir, 
#'                  runParallel=FALSE)
#' }
#' @keywords IO
#' @concept dataGet
#' @family obsDataReads
#' @export
#' 
GetStage4Files <- function(startDate, endDate, 
                           dataTyp, dataDir, 
                           keepDownloads=FALSE,
                           runParallel=FALSE) {
    # template wget url string for StageII data 
    # ftp://ftp.emc.ncep.noaa.gov/mmb/precip/st2n4.arch/201505/ST2.20150501
    
    # template wget url string for StageIV data 
    # ftp://ftp.emc.ncep.noaa.gov/mmb/precip/st2n4.arch/201405/ST4.20140506
    
    urlStr <- "ftp://ftp.emc.ncep.noaa.gov/mmb/precip/st2n4.arch/" 
    
    # time period for data retrieval
    yr1 <- c(substr(startDate,1,4),substr(endDate,1,4))
    mo1 <- c(substr(startDate,5,6),substr(endDate,5,6))
    da1 <- c(substr(startDate,7,8),substr(endDate,7,8))
    hr1 <- c(substr(startDate,9,10),substr(endDate,9,10))
    
    # collect all the hours for data retrieval
    hours = seq(ISOdatetime(yr1[1],mo1[1],da1[1],hr1[1],0,0),
                ISOdatetime(yr1[2],mo1[2],da1[2],hr1[2],0,0),by="day")      
    
    # create directory if it does not exist
    if (substr(dataDir, nchar(dataDir), nchar(dataDir)) != "/") {
        dataDir <- paste0(dataDir, "/")
    }
    if (!file.exists(dataDir)) dir.create(dataDir, recursive=TRUE)
    dataDirRaw <- paste0(dataDir, "/RAW/")
    if (!file.exists(dataDirRaw)) dir.create(dataDirRaw, recursive=TRUE)
    
    # create sub function that we can loop with
    GetFiles4Loop <- function(hoursi, dataTyp.=dataTyp,
                              dataDir.=dataDir, dataDirRaw.=dataDirRaw) {
        # contruct the url for data retrieval
        print(paste0("Processing: ", hoursi))
        fname <-  paste0("ST", substr(dataTyp.,6,6),format(hoursi,".%Y%m%d"))
        dtstr <- unlist(strsplit(fname, split="[.]"))[2]
        dirSrc <- paste0(format(hoursi,"%Y%m/"))
        dirDst <- paste0(dataDir., format(hoursi,"%Y%m/"))
        
        if (!file.exists(paste0(dataDirRaw., fname))) {
            print("   File not in RAW directory. Downloading and uncompressing...")
            if (!file.exists(dirDst)) dir.create(dirDst,recursive=TRUE)
            url1 <- paste0(urlStr,dirSrc,fname)
            try(curl::curl_download(url1, paste0(dirDst, fname)))
            system(paste0("tar -xf ", dirDst, fname, ' -C ', dirDst), 
                   ignore.stdout = TRUE)
            if (keepDownloads) {
                system(paste0("mv ", dirDst, fname, " ", dataDirRaw., "/."))
            } else {
                system(paste0("touch ", dataDirRaw., fname))
                system(paste0("rm ", dirDst, fname))
            }
            filesToRm <- list.files(path=dirDst, 
                                    pattern=glob2rx(paste0("*.", dtstr, "*.gif")),
                                    full.names=TRUE)
            filesToRm <- c(filesToRm, list.files(path=dirDst, 
                                    pattern=glob2rx(paste0("*rd", dtstr, "*.gz")),
                                    full.names=TRUE))
            filesToRm <- c(filesToRm, list.files(path=dirDst, 
                                    pattern=glob2rx(paste0("*gg", dtstr, "*.gz")),
                                    full.names=TRUE))
            filesToRm <- c(filesToRm, list.files(path=dirDst, 
                                    pattern=glob2rx(paste0("*un", dtstr, "*.gz")),
                                    full.names=TRUE))
            if (length(filesToRm)>0) kill <- file.remove(filesToRm)
            system(paste0("gunzip ", dirDst, paste0("*", dtstr, "*.gz")))    
        } else {
            print("File already in directory. Skipping...")
        }
    }
               
    if (runParallel) {
        # PARALLEL VERSION
        foreach (i=1:length(hours)) %dopar% {
            GetFiles4Loop(hours[i], dataTyp, dataDir, dataDirRaw)
        }
    } else { 
        # NON-PARALLEL VERSION
        for (i in 1:length(hours)) {
            GetFiles4Loop(hours[i], dataTyp, dataDir, dataDirRaw)
        }
    }
}


