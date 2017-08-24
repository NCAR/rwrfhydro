#' Read WRF-Hydro standard-format forecast points output text file.
#'
#' \code{ReadFrxstPts} reads in WRF-Hydro forecast points output text file.
#'
#' \code{ReadFrxstPts} reads a standard-format WRF-Hydro forecast points output text
#' file and creates a dataframe with consistent date and data columns for use with other
#' rwrfhydro tools.
#' 
#' @param pathOutfile The full pathname to the WRF-Hydro forecast points text file
#' (frxst_pts_out.txt).
#' @param stIdType Character describing the variable type desired for the stn_id variable, 
#' defaults to "character" but can also be "integer".
#' @param adjPosix If the first timest in the file matches the model init time, then set this to TRUE.
#' @return A dataframe containing the forecast points output flow data. Note that POSIXct is the valid
#' time for the flow but timest is not, it is the LSM time prior to the flow at POSIXct.
#'
#' @examples
#' ## Take a forecast point output text file for an hourly model run of Fourmile Creek
#' ## and return a dataframe.
#' \dontrun{
#' modStr1h.mod1.fc <- ReadFrxstPts("../OUTPUT/frxst_pts_out.txt")
#' }
#' @keywords IO
#' @concept dataGet
#' @family modelDataReads
#' @export
ReadFrxstPts <- function(pathOutfile, stIdType='character', adjPosix=FALSE) {
    myobj <- read.table(pathOutfile, header=F, sep=",", 
                        colClasses=c("integer","character",stIdType,"numeric","numeric","numeric","numeric","numeric"), 
                        na.strings=c("********","*********","************"))
    colnames(myobj) <- c("secs","timest","st_id","st_lon","st_lat","q_cms","q_cfs","dpth_m")

    myobj$POSIXct <- as.POSIXct(as.character(myobj$timest), format="%Y-%m-%d %H:%M:%S", tz="UTC")
    # The above may not give the valid times of the flows.
    if(adjPosix)
      myobj$POSIXct <- myobj$POSIXct + lubridate::period(as.integer(myobj$secs[1]), 'seconds')
    
    myobj$wy <- ifelse(as.numeric(format(myobj$POSIXct,"%m"))>=10, as.numeric(format(myobj$POSIXct,"%Y"))+1, 
                       as.numeric(format(myobj$POSIXct,"%Y")))
    myobj
}


#' Read WRF-Hydro standard-format groundwater output text file.
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
#' \dontrun{
#' modGWout1h.mod1.fc <- ReadGwOut("../OUTPUT/GW_outflow.txt")
#' }
#' @keywords IO
#' @concept dataGet
#' @family modelDataReads
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
    myobj$POSIXct <- as.POSIXct(as.character(myobj$timest), 
                                format="%Y-%m-%d_%H:%M:%S",tz="UTC")
    myobj$wy <- ifelse(as.numeric(format(myobj$POSIXct,"%m"))>=10, 
                       as.numeric(format(myobj$POSIXct,"%Y"))+1, 
                       as.numeric(format(myobj$POSIXct,"%Y")))
    myobj
}


#' Read WRF-Hydro (w/NoahMP) LDASOUT data files and generate basin-wide mean water 
#' budget variables.
#'
#' \code{ReadLdasoutWb} reads in WRF-Hydro (w/NoahMP) LDASOUT files and outputs a 
#' time series of basin-wide mean variables for water budget calculations.
#'
#' \code{ReadLdasoutWb} reads standard-format WRF-Hydro (w/NoahMP) LDASOUT NetCDF 
#' files and calculates basin-wide mean values for each time step suitable for running 
#' basin water budget calculations.
#'
#' OUTPUT NoahMP LDASOUT water budget variables:
#' \itemize{
#'    \item ACCECAN: Mean accumulated canopy evaporation (mm)
#'    \item ACCEDIR: Mean accumulated surface evaporation (mm)
#'    \item ACCETRAN: Mean accumulated transpiration (mm)
#'    \item ACCPRCP: Mean accumulated precipitation (mm)
#'    \item CANICE: Mean canopy ice storage (mm)
#'    \item CANLIQ: Mean canopy liquid water storage (mm)
#'    \item SFCRNOFF: Mean surface runoff from LSM \emph{(meaningful for an 
#'              LSM-only run)} (mm)
#'    \item SNEQV: Mean snowpack snow water equivalent (mm)
#'    \item UGDRNOFF: Mean subsurface runoff from LSM \cr \emph{(meaningful 
#'              for an LSM-only run)} (mm)
#'    \item SOIL_M1: Mean soil moisture storage in soil layer 1 (top) (mm)
#'    \item SOIL_M2: Mean soil moisture storage in soil layer 2 (mm)
#'    \item SOIL_M3: Mean soil moisture storage in soil layer 3 (mm)
#'    \item SOIL_M4: Mean soil moisture storage in soil layer 4 (bottom) (mm)
#' }
#'
#' @param pathOutdir The full pathname to the output directory containing the 
#' LDASOUT files.
#' @param pathDomfile The full pathname to the high-res hydro domain NetCDF file 
#' used in the model run (for grabbing the basin mask).
#' @param mskvar The variable name in pathDomfile to use for the mask 
#' (DEFAULT="basn_msk").
#' @param basid The basin ID to use (DEFAULT=1)
#' @param aggfact The high-res (hydro) to low-res (LSM) aggregation factor 
#' (e.g., for a 100-m routing grid and a 1-km LSM grid, aggfact = 10)
#' @param pattern Pattern to match in the model output 
#' (DEFAULT=glob2rx('*LDASOUT_DOMAIN*'))
#' @param parallel Logical for running in parallel mode (must have a parallel
#' backend installed and registered (e.g., doMC or doParallel) (DEFAULT=FALSE)
#' @return A dataframe containing a time series of basin-wide mean water 
#' budget variables.
#'
#' @examples
#' ## Take an OUTPUT directory for a daily LSM timestep model run of Fourmile Creek and
#' ## create a new dataframe containing the basin-wide mean values for the major water
#' ## budget components over the time series.
#'
#' \dontrun{
#' library(doMC)
#' registerDoMC(3)
#' modLdasoutWb1d.mod1.fc <- 
#'   ReadLdasoutWb("../RUN.MOD1/OUTPUT", "../DOMAIN/Fulldom_hires_hydrofile_4mile.nc", 
#'                 parallel=TRUE)
#' }
#' @keywords IO univar ts
#' @concept dataGet
#' @family modelDataReads
#' @export
ReadLdasoutWb <- function(pathOutdir, pathDomfile, mskvar="basn_msk", 
                          basid=1, aggfact=10,
                          pattern=glob2rx('*LDASOUT_DOMAIN*'),
                          parallel=FALSE) {
    if (!is.null(pathDomfile)) {
       # Setup mask
       mskvar <- CreateBasinMask(pathDomfile, mskvar=mskvar, basid=basid, aggfact=aggfact)
       # Calculate basin area as a cell count
       basarea <- sum(mskvar)
    } else {
       # Create dummy mask of ones
       firstFile <- list.files(path=pathOutdir, pattern=pattern, full.names=TRUE)[1]
       ncid <- ncdf4::nc_open(firstFile)
       ylength <- ncid$dim[["south_north"]]$len
       xlength <- ncid$dim[["west_east"]]$len
       ncdf4::nc_close(ncid)
       mskvar <- matrix(1, xlength, ylength)
       basarea <- sum(mskvar)
    }
    # Setup basin mean function
    basin_avg <- function(myvar, minValid=-1e+30) {
        myvar[which(myvar<minValid)]<-NA
        sum(mskvar*myvar, na.rm=TRUE)/sum(mskvar, na.rm=TRUE)
        }
    basin.level1 <- list( start=c(1,1,1,1), end=c(dim(mskvar)[1],1,dim(mskvar)[2],1), 
                          stat='basin_avg', mskvar, env=environment() )
    basin.level2 <- list( start=c(1,2,1,1), end=c(dim(mskvar)[1],2,dim(mskvar)[2],1), 
                          stat='basin_avg', mskvar, env=environment() )
    basin.level3 <- list( start=c(1,3,1,1), end=c(dim(mskvar)[1],3,dim(mskvar)[2],1), 
                          stat='basin_avg', mskvar, env=environment() )
    basin.level4 <- list( start=c(1,4,1,1), end=c(dim(mskvar)[1],4,dim(mskvar)[2],1), 
                          stat='basin_avg', mskvar, env=environment() )
    basin.surf <-  list(start=c(1,1,1), end=c(dim(mskvar)[1],dim(mskvar)[2],1), 
                        stat='basin_avg', mskvar, env=environment())
    # Setup LDASOUT variables to use
    variableNames <- c('ACCECAN','ACCEDIR','ACCETRAN','ACCPRCP','CANICE','CANLIQ',
                       'SFCRNOFF','SNEQV', 'UGDRNOFF', 
                       rep('SOIL_M',4))
    ldasoutVars <- as.list( variableNames ) 
    names(ldasoutVars) <- c('ACCECAN','ACCEDIR','ACCETRAN','ACCPRCP','CANICE','CANLIQ',
                            'SFCRNOFF','SNEQV', 'UGDRNOFF', 
                            paste0("SOIL_M",1:4))
    ldasoutVariableList <- list( ldasout = ldasoutVars )
    # For each variable, setup relevant areas and levels to do averaging
    cell <-  basin.surf
    level1 <- basin.level1
    level2 <- basin.level2
    level3 <- basin.level3
    level4 <- basin.level4
    ldasoutInd <- list( cell, cell, cell, cell, cell, cell, 
                        cell, cell, cell, 
                        level1, level2, level3, level4 )
    names(ldasoutInd) <- names(ldasoutVars)
    ldasoutIndexList <- list( ldasout = ldasoutInd )
    # Run GetMultiNcdf
    ldasoutFilesList <- list( ldasout = list.files(path=pathOutdir, 
                                                   pattern=pattern, full.names=TRUE))
    ldasoutDF <- GetMultiNcdf(indexList=ldasoutIndexList, 
                                  variableList=ldasoutVariableList, 
                                  filesList=ldasoutFilesList, parallel=parallel )
    outDf <- ReshapeMultiNcdf(ldasoutDF)
    outDf <- CalcNoahmpFluxes(outDf)
    attr(outDf, "area_cellcnt") <- basarea
    outDf
}


#' Read WRF-Hydro (w/NoahMP) LDASOUT data files and generate basin-wide mean of all
#' variables.
#' 
#' \code{ReadLdasoutAll} reads in WRF-Hydro (w/NoahMP) LDASOUT files and outputs a time
#' series of basin-wide mean variables.
#' 
#' \code{ReadLdasoutAll} reads standard-format WRF-Hydro (w/NoahMP) LDASOUT NetCDF files
#' and calculates basin-wide mean values for each time step.
#' 
#' OUTPUT NoahMP LDASOUT variables: SEE NOAHMP DOCUMENTATION
#' 
#' @param pathOutdir The full pathname to the output directory containing the LDASOUT
#'   files.
#' @param pathDomfile The full pathname to the high-res hydro domain NetCDF file used in 
#'   the model run (for grabbing the basin mask).
#' @param mskvar The variable name in pathDomfile to use for the mask
#'   (DEFAULT="basn_msk").
#' @param basid The basin ID to use (DEFAULT=1)
#' @param aggfact The high-res (hydro) to low-res (LSM) aggregation factor (e.g., for a
#'   100-m routing grid and a 1-km LSM grid, aggfact = 10)
#' @param pattern Pattern to match in the model output
#'   (DEFAULT=glob2rx('*LDASOUT_DOMAIN*'))
#' @param parallel Logical for running in parallel mode (must have a parallel
#' backend installed and registered (e.g., doMC or doParallel) (DEFAULT=FALSE)
#' @return A dataframe containing a time series of basin-wide mean water budget variables.
#'   
#' @examples
#' ## Take an OUTPUT directory for a daily LSM timestep model run of Fourmile Creek and
#' ## create a new dataframe containing the basin-wide mean values for all variables
#' ## over the time series.
#' 
#' \dontrun{
#' library(doMC)
#' registerDoMC(3)
#' modLdasout1d.mod1.fc <- 
#'   ReadLdasoutAll("../RUN.MOD1/OUTPUT", "../DOMAIN/Fulldom_hires_hydrofile_4mile.nc", 
#'                 parallel=TRUE)
#' }
#' @keywords IO univar ts
#' @concept dataGet
#' @family modelDataReads
#' @export
ReadLdasoutAll <- function(pathOutdir, pathDomfile, mskvar="basn_msk", 
                          basid=1, aggfact=10,  
                          pattern=glob2rx('*LDASOUT_DOMAIN*'),
                          parallel=FALSE) {
    if (!is.null(pathDomfile)) {
        # Setup mask
        mskvar <- CreateBasinMask(pathDomfile, mskvar=mskvar, basid=basid, aggfact=aggfact)
        # Calculate basin area as a cell count
        basarea <- sum(mskvar)
    } else {
        # Create dummy mask of ones
        firstFile <- list.files(path=pathOutdir, pattern=pattern, full.names=TRUE)[1]
        ncid <- ncdf4::nc_open(firstFile)
        ylength <- ncid$dim[["south_north"]]$len
        xlength <- ncid$dim[["west_east"]]$len
        ncdf4::nc_close(ncid)
        mskvar <- matrix(1, xlength, ylength)
        basarea <- sum(mskvar)
    }
  # Setup basin mean function
  basin_avg <- function(myvar, minValid=-1e+30) {
    myvar[which(myvar<minValid)]<-NA
    sum(mskvar*myvar, na.rm=TRUE)/sum(mskvar, na.rm=TRUE)
  }
  basin.level1 <- list( start=c(1,1,1,1), end=c(dim(mskvar)[1],1,dim(mskvar)[2],1), 
                        stat='basin_avg', mskvar, env=environment() )
  basin.level2 <- list( start=c(1,2,1,1), end=c(dim(mskvar)[1],2,dim(mskvar)[2],1), 
                        stat='basin_avg', mskvar, env=environment() )
  basin.level3 <- list( start=c(1,3,1,1), end=c(dim(mskvar)[1],3,dim(mskvar)[2],1), 
                        stat='basin_avg', mskvar, env=environment() )
  basin.level4 <- list( start=c(1,4,1,1), end=c(dim(mskvar)[1],4,dim(mskvar)[2],1), 
                        stat='basin_avg', mskvar, env=environment() )
  basin.surf <-  list(start=c(1,1,1), end=c(dim(mskvar)[1],dim(mskvar)[2],1), 
                      stat='basin_avg', mskvar, env=environment())
  # Setup LDASOUT variables to use
  variableNames <- c('ACCECAN', 'ACCEDIR', 'ACCETRAN', 'ACCPRCP', 'ACSNOM', 
                     'ACSNOW', 'ALBEDO', 'APAR', 'CANICE', 'CANLIQ', 
                     'CH', 'CHB', 'CHB2', 'CHLEAF', 'CHUC', 
                     'CHV', 'CHV2', 'CM', 'COSZ', 'EAH', 
                     'ECAN', 'EDIR', 'EMISS', 'ETRAN', 'EVB', 
                     'EVC', 'EVG', 'FASTCP', 'FIRA', 'FSA', 
                     'FSNO', 'FVEG', 'FWET', 'GHB', 'GHV', 
                     'GPP', 'GRDFLX', 'HFX', 'IRB', 'IRC', 
                     'IRG', 'ISLTYP', 'ISNOW', 'IVGTYP', 'LAI', 
                     'LFMASS', 'LH', 'LWFORC', 'NEE', 'NPP', 
                     'PSN', 'Q2MB', 'Q2MV', 'QSNOW', 'RAINRATE', 
                     'RTMASS', 'SAG', 'SAI', 'SAV', 'SFCRNOFF', 
                     'SHB', 'SHC', 'SHG', 'SNEQV',
                     rep('SNICE',3), 
                     rep('SNLIQ',3), 
                     'SNOWH', 
                     rep('SNOW_T',3),
                     rep('SOIL_M',4), 
                     rep('SOIL_T',4), 
                     rep('SOIL_W',4), 
                     'STBLCP', 'STMASS', 'SWFORC', 'T2MB', 'T2MV', 
                     'TAH', 'TG', 'TGB', 'TGV', 'TR',
                     'TRAD', 'TV', 'UGDRNOFF', 'WA', 'WOOD', 
                     'WT', 
                     rep('ZSNSO_SN',3), 
                     'ZWT')
  ldasoutVars <- as.list( variableNames ) 
  names(ldasoutVars) <- c('ACCECAN', 'ACCEDIR', 'ACCETRAN', 'ACCPRCP', 'ACSNOM', 
                          'ACSNOW', 'ALBEDO', 'APAR', 'CANICE', 'CANLIQ', 
                          'CH', 'CHB', 'CHB2', 'CHLEAF', 'CHUC', 
                          'CHV', 'CHV2', 'CM', 'COSZ', 'EAH', 
                          'ECAN', 'EDIR', 'EMISS', 'ETRAN', 'EVB', 
                          'EVC', 'EVG', 'FASTCP', 'FIRA', 'FSA', 
                          'FSNO', 'FVEG', 'FWET', 'GHB', 'GHV', 
                          'GPP', 'GRDFLX', 'HFX', 'IRB', 'IRC', 
                          'IRG', 'ISLTYP', 'ISNOW', 'IVGTYP', 'LAI', 
                          'LFMASS', 'LH', 'LWFORC', 'NEE', 'NPP', 
                          'PSN', 'Q2MB', 'Q2MV', 'QSNOW', 'RAINRATE', 
                          'RTMASS', 'SAG', 'SAI', 'SAV', 'SFCRNOFF', 
                          'SHB', 'SHC', 'SHG', 'SNEQV',
                          paste0('SNICE',1:3), 
                          paste0('SNLIQ',1:3), 
                          'SNOWH', 
                          paste0('SNOW_T',1:3), 
                          paste0('SOIL_M',1:4), 
                          paste0('SOIL_T',1:4), 
                          paste0('SOIL_W',1:4), 
                          'STBLCP', 'STMASS', 'SWFORC', 'T2MB', 'T2MV', 
                          'TAH', 'TG', 'TGB', 'TGV', 'TR', 
                          'TRAD', 'TV', 'UGDRNOFF', 'WA', 'WOOD', 
                          'WT', 
                          paste0('ZSNSO_SN',1:3), 
                          'ZWT')
  ldasoutVariableList <- list( ldasout = ldasoutVars )
  # For each variable, setup relevant areas and levels to do averaging
  cell <-  basin.surf
  level1 <- basin.level1
  level2 <- basin.level2
  level3 <- basin.level3
  level4 <- basin.level4
  ldasoutInd <- list( cell, cell, cell, cell, cell, 
                      cell, cell, cell, cell, cell,
                      cell, cell, cell, cell, cell, 
                      cell, cell, cell, cell, cell,
                      cell, cell, cell, cell, cell, 
                      cell, cell, cell, cell, cell,
                      cell, cell, cell, cell, cell, 
                      cell, cell, cell, cell, cell,
                      cell, cell, cell, cell, cell, 
                      cell, cell, cell, cell, cell,
                      cell, cell, cell, cell, cell, 
                      cell, cell, cell, cell, cell,
                      cell, cell, cell, cell,
                      level1, level2, level3,
                      level1, level2, level3,
                      cell,
                      level1, level2, level3,
                      level1, level2, level3, level4,
                      level1, level2, level3, level4,
                      level1, level2, level3, level4,
                      cell, cell, cell, cell, cell, 
                      cell, cell, cell, cell, cell,
                      cell, cell, cell, cell, cell, 
                      cell,
                      level1, level2, level3,
                      cell )
  names(ldasoutInd) <- names(ldasoutVars)
  ldasoutIndexList <- list( ldasout = ldasoutInd )
  # Run GetMultiNcdf
  ldasoutFilesList <- list( ldasout = list.files(path=pathOutdir, 
                                                 pattern=pattern, full.names=TRUE))
  ldasoutDF <- GetMultiNcdf(indexList=ldasoutIndexList, 
                              variableList=ldasoutVariableList, 
                              filesList=ldasoutFilesList, parallel=parallel )
  outDf <- ReshapeMultiNcdf(ldasoutDF)
  outDf <- CalcNoahmpFluxes(outDf)
  attr(outDf, "area_cellcnt") <- basarea
  outDf
}

#' Read WRF-Hydro RTOUT data files and generate basin-wide mean water fluxes.
#'
#' \code{ReadRtout} reads in WRF-Hydro RTOUT files and outputs a time series of
#' basin-wide mean water fluxes for water budget.
#'
#' \code{ReadRtout} reads standard-format WRF-Hydro RTOUT NetCDF files and calculates
#' basin-wide mean values for each time step for water budget terms.
#'
#' OUTPUT RTOUT water budget variables:
#' \itemize{
#'    \item QSTRMVOLRT: Mean accumulated depth of stream channel inflow (mm)
#'    \item SFCHEADSUBRT: Mean depth of ponded water (mm)
#'    \item QBDRYRT: Mean accumulated flow volume routed outside of the domain 
#'              from the boundary cells (mm)
#' }
#'
#' @param pathOutdir The full pathname to the output directory containing the 
#' RTOUT files.
#' @param pathDomfile The full pathname to the high-res hydro domain NetCDF file 
#' used in the model run (for grabbing the basin mask).
#' @param mskvar The variable name in pathDomfile to use for the mask 
#' (DEFAULT="basn_msk").
#' @param basid The basin ID to use (DEFAULT=1)
#' @param parallel Logical for running in parallel mode (must have a parallel
#' backend installed and registered (e.g., doMC or doParallel) (DEFAULT=FALSE)
#' @param pattern The pattern to match for file ingest
#' (DEFAULT=glob2rx('*.RTOUT_DOMAIN*'))
#' @return A dataframe containing a time series of basin-wide mean water budget 
#' variables.
#'
#' @examples
#' ## Take an OUTPUT directory for an hourly routing timestep model run of 
#' ## Fourmile Creek (Basin ID = 1) and create a new dataframe containing the 
#' ## basin-wide mean values for the major water budget components over the 
#' ## time series.
#'
#' \dontrun{
#' library(doMC)
#' regsiterDoMC(3)
#' modRtout1h.mod1.fc <- 
#'   ReadRtout("../RUN.MOD1/OUTPUT", "../DOMAIN/Fulldom_hires_hydrofile_4mile.nc", 
#'             basid=1, parallel=TRUE)
#' }
#' @keywords IO univar ts
#' @concept dataGet
#' @family modelDataReads
#' @export
ReadRtout <- function(pathOutdir, pathDomfile, 
                      mskvar="basn_msk", basid=1, 
                      parallel=FALSE,
                      pattern=glob2rx('*.RTOUT_DOMAIN*')) {
    if (!is.null(pathDomfile)) {
        # Setup mask
        msk <- ncdf4::nc_open(pathDomfile)
        mskvar <- ncdf4::ncvar_get(msk,mskvar)
        # Subset to basinID
        mskvar[which(mskvar != basid)] <- 0.0
        mskvar[which(mskvar == basid)] <- 1.0
        # Reverse y-direction for N->S hydro grids to S->N
        mskvar <- mskvar[,order(ncol(mskvar):1)]
    } else {
        # Create dummy mask of ones
        firstFile <- list.files(path=pathOutdir, pattern=pattern, 
                                full.names=TRUE)[1]
        ncid <- ncdf4::nc_open(firstFile)
        ylength <- ncid$dim[["y"]]$len
        xlength <- ncid$dim[["x"]]$len
        ncdf4::nc_close(ncid)
        mskvar <- matrix(1, xlength, ylength)
    }
    
    # Setup mean functions
    basin_avg <- function(myvar, minValid=-1e+30) {
        myvar[which(myvar<minValid)]<-NA
        sum(mskvar*myvar, na.rm=TRUE)/sum(mskvar, na.rm=TRUE)
        }
    basin.surf <-  list(start=c(1,1,1), end=c(dim(mskvar)[1],dim(mskvar)[2],1), 
                        stat='basin_avg', mskvar, env=environment())
    # Get files
    rtoutFilesList <- list( rtout = list.files(path=pathOutdir, 
                                                   pattern=glob2rx('*.RTOUT_DOMAIN*'), 
                                                   full.names=TRUE))
    if (length(rtoutFilesList)==0) stop("No matching files in specified directory.")
    # Setup RTOUT variables to use
    variableNames <- c('QSTRMVOLRT', 'SFCHEADSUBRT', 'sfcheadsubrt',
                       'QBDRYRT', 'ZWATTABLRT', 'zwattablrt')
    fileVars <- names(ncdump(unlist(rtoutFilesList)[1], quiet=TRUE)$var)
    variableNames <- variableNames[variableNames %in% fileVars]
    rtoutVars <- as.list( variableNames )
    names(rtoutVars) <- variableNames
    rtoutVariableList <- list( rtout = rtoutVars )
    # For each variable, setup relevant areas and levels to do averaging
    cell <-  basin.surf
    rtoutInd <- list( cell, cell, cell, cell )
    names(rtoutInd) <- names(rtoutVars)
    rtoutIndexList <- list( rtout = rtoutInd )
    # Run GetMultiNcdf
    rtoutDF <- GetMultiNcdf(indexList=rtoutIndexList, 
                                  variableList=rtoutVariableList, 
                                  filesList=rtoutFilesList, parallel=parallel )
    outDf <- ReshapeMultiNcdf(rtoutDF)
    names(outDf)[names(outDf)=="sfcheadsubrt"] <- 'SFCHEADSUBRT'
    names(outDf)[names(outDf)=="zwattablrt"] <- 'ZWATTABLRT'
    outDf
}

#' Read WRF-Hydro CHRTOUT data files.
#'
#' \code{ReadChrtout} reads in WRF-Hydro CHRTOUT files and outputs a time series of
#' channel fluxes.
#'
#' \code{ReadChrtout} reads standard-format WRF-Hydro CHRTOUT NetCDF files and 
#' outputs a time series of channel fluxes.
#'
#' @param pathOutdir The full pathname to the output directory containing the 
#' RTOUT files.
#' @param idList Optional list of station IDs to import (must be consistent
#' with IDs as used in the specified idvar variable). 
#' @param gageList Optional list of gage IDs to import. Must provide a corresponding
#' route link file (used to map gage IDs to link IDs). Available only for reach-based
#' channel routing model runs.
#' @param rtlinkFile Optional path to the route link file. Available only for 
#' reach-based channel routing model runs.
#' @param parallel Logical for running in parallel mode (must have a parallel
#' backend installed and registered (e.g., doMC or doParallel) (DEFAULT=FALSE)
#' @param useDatatable Logical for utilizing the data.table package and 
#' outputting in data.table format (DEFAULT=TRUE)
#' @param gageOnly Logical for whether to bring in reaches with associated 
#' gage IDs only (vs. all reaches) (DEFAULT=TRUE)
#' @param pattern The pattern to match for file ingest
#' (DEFAULT=glob2rx('*.CHRTOUT_DOMAIN*'))
#' @param idvar The unique ID variable (DEFAULT="feature_id")
#' @return A datatable containing a time series of channel fluxes.
#'
#' @examples
#' ## Take an OUTPUT directory for an hourly routing timestep model run of 
#' ## the Front Range domain and create a new dataframe containing the channel
#' ## fluxes for two USGS gages on Fourmile Creek.
#'
#' \dontrun{
#' ReadChrtout('~/wrfHydroTestCases/FRN.REACH/OUTPUT', 
#'      gageList=c("06727500", "06730160"), 
#'      rtlinkFile="~/wrfHydroTestCases/FRN.REACH/DOMAIN/RouteLink.nc")
#' }
#' @keywords IO univar ts
#' @concept dataGet
#' @family modelDataReads
#' @export
ReadChrtout <- function(pathOutdir, 
                        idList=NULL,
                        gageList=NULL, rtlinkFile=NULL,
                        parallel=FALSE,
                        useDatatable=TRUE,
                        gageOnly=TRUE,
                        pattern=glob2rx('*.CHRTOUT_DOMAIN*'),
                        idvar="feature_id") {

    # Get files
    filesList <- list.files(path=pathOutdir, 
                                    pattern=pattern, 
                                    full.names=TRUE)
    if (length(filesList)==0) stop("No matching files in specified directory.")
    # Compile link list
    if (!is.null(rtlinkFile)) {
        rtLink <- ReadRouteLink(rtlinkFile)
        if (useDatatable) rtLink <- data.table(rtLink)
    }
    if (is.null(idList)) {
        if (exists("rtLink")) {
            if (is.null(gageList)) {
                if (gageOnly) {
                   if (useDatatable) {
                      rtLink <- rtLink[site_no != '',]
                   } else {
                      rtLink <- subset(rtLink, rtLink$site_no != '')
                   }
                }
            } else {
                if (useDatatable) {
                    rtLink <- rtLink[site_no %in% gageList,]
                } else {
                    rtLink <- subset(rtLink, rtLink$site_no %in% gageList)
                }
            }
            idList <- unique(rtLink$link)
        }
    }
    
    # Single file read function
    ReadFile4Loop <- function(file., useDatatable.=TRUE) {
        out <- GetNcdfFile(file., variables=c("time", "reference_time"), exclude=TRUE, quiet=TRUE)
        dtstr <- basename(file.)
        dtstr <- unlist(strsplit(dtstr, "[.]"))[1]
        dtstr <- as.POSIXct(dtstr, format="%Y%m%d%H%M", tz="UTC")
        out$POSIXct <- dtstr
        if (useDatatable.) out<-data.table(out)
        out
    }
    
    # Loop through all files
    outList <- list()
    if (parallel) {
        packageList <- ifelse(useDatatable, c("ncdf4","data.table"), c("ncdf4"))
        outList <- foreach(file=filesList, .packages = packageList, 
                           .combine=c) %dopar% {
            out <- ReadFile4Loop(file, useDatatable.=useDatatable)
            if (!is.null(idList)) {
                if (useDatatable) {
                    out <- out[get(idvar) %in% idList,]
                } else {
                    out <- subset(out, out[[idvar]] %in% idList)
                }
            }
            list(out)
        }
    } else {
        for (file in filesList) {
            out <- ReadFile4Loop(file)
            if (!is.null(idList)) {
                if (useDatatable) {
                    out <- out[get(idvar) %in% idList,]
                } else {
                    out <- subset(out, out[[idvar]] %in% idList)

                }
            }
            outList <- c(outList, list(out))
        }
    }
    if (useDatatable) {
        outDT <- data.table::rbindlist(outList)
    } else {
        outDT <- do.call("rbind", outList)
    }
    names(outDT)[names(outDT)=="streamflow"]<-"q_cms"
    names(outDT)[names(outDT)=="velocity"]<-"vel_ms"
    if (exists("rtLink")) {
        names(outDT)[names(outDT)==idvar]<-"link"

        if (useDatatable) {
            data.table::setkey(rtLink, "link")
            data.table::setkey(outDT, "link")
            outDT <- merge(outDT, rtLink[, c("link", "site_no"), with=FALSE], all.x=TRUE)
        } else {
            outDT <- plyr::join(outDT, rtLink[, c("link", "site_no")], by="link", type="left")
        }
    }
    outDT
}

#' Create a coarse-resolution basin mask grid.
#'
#' \code{CreateBasinMask} reads in a high-res domain file and outputs a resampled
#' weighted basin mask grid for generating LSM-grid statistics.
#'
#' \code{CreateBasinMask} reads in a high-res domain file and outputs a resampled
#' weighted basin mask grid for generating LSM-grid statistics. The output grid will
#' contain 1 for cells that are completely within the basin, 0 for cells that are
#' completely outside of the basin, and fractions (based on area) for cells that are 
#' partially within and partially outside of the basin.
#' 
#' @param ncfile The full pathname to the WRF-Hydro high-res routing domain file.
#' @param mskvar The variable name for the high-res basin mask (DEFAULT="basn_msk")
#' @param basid The basin ID to generate a mask file for (DEFAULT=1)
#' @param aggfact The aggregation factor for downsampling the high-res grid (e.g.,
#' aggfact=1 for going from a 100-m routing grid to a 1km geogrid) (DEFAULT=1)
#' @return A matrix containing the basin mask weights on the resampled grid.
#'
#' @examples
#' ## Take the high-res 100-m routing domain for Fourmile and generate a matrix of
#' ## area weights on the 1km geogrid domain.
#' \dontrun{
#' geoMsk <- 
#' CreateBasinMask("~/wrfHydroTestCases/Fourmile_Creek/DOMAIN/Fulldom_hydro_OrodellBasin_100m.nc",
#' aggFact=10)
#' }
#' @keywords IO
#' @concept dataGet
#' @family modelDataReads
#' @export
CreateBasinMask <- function(ncfile, mskvar="basn_msk", basid=1, aggfact=1) {
   # Setup mask
   nc <- ncdf4::nc_open(ncfile)
   basnmsk <- ncdf4::ncvar_get(nc, mskvar)
   ncdf4::nc_close(nc)
   # Subset to basinID
   basnmsk[which(basnmsk != basid)] <- 0.0
   basnmsk[which(basnmsk == basid)] <- 1.0
   # Reverse y-direction for N->S hydro grids to S->N
   basnmsk <- basnmsk[,order(ncol(basnmsk):1)]
   # Resample the high-res grid to the low-res LSM
   if (aggfact > 1) {
     basnmsk <- raster::as.matrix(raster::aggregate(raster::raster(basnmsk), 
                                                    fact=aggfact, fun=mean))
   }
   basnmsk
 }
 
 #' Read in WRF-Hydro route link file
 #' 
 #' \code{ReadRouteLink} is simply a usage of format.
 #' @param linkFile Full path to route link file
 #' @return Dataframe of route link data
 #' @keywords IO
 #' @concept dataGet
 #' @family modelDataReads
 #' @export
 ReadRouteLink <- function(linkFile) {
     rtLinks <- GetNcdfFile(linkFile, variables=c("time"), exclude=TRUE, quiet=TRUE)
     rtLinks$site_no <- stringr::str_trim(rtLinks$gages)
     rtLinks
 }
#' Read LAKEOUT files from gridded lake routing option and create data frame/table for selected lakes.
#' \code{ReadLakeout} reads in the WRF-Hydro LAKEOUT files from gridded lake routing: Community Release version.
#'
#' @param pathOutdir The full pathname to the WRF-Hydro lake output files.
#' @param lakeidList The list of lakeids to put in the output data table. These lakeids correspond to the lakes in the LAKEGRID, LAKEPARM and lakes.shp files that are
#' created in the routing files from the ArcGIS pre-processing tools.
#' @param parallel Logical for running in parallel mode (must have a parallel
#' backend installed and registered (e.g., doMC or doParallel) (DEFAULT=FALSE)
#' @param useDatatable Logical for utilizing the data.table package and
#' outputting in data.table format (DEFAULT=TRUE)
#' @param pattern Pattern to match in the model output (e.g. *LAKEOUT_DOMAIN1*)

#' @examples
#' # This function loops through LAKEOUT files in the pathOutdir and creates a data frame or 
#' # data table to store inflow, outflow, elevation and the station_id (lakeid).  The lakeids 
#' # correspond to the lakes in the LAKEGRID, LAKEPARM and lakes.shp files that are
#' # created in the routing files from the ArcGIS pre-processing tools.

#' \dontrun{
#' ReadLakeout('~/wrfHydroTestCases/FRN.REACH/OUTPUT',
#'      lakeidList=c("1", "5"))
#' }

 ReadLakeout <- function(pathOutdir=NULL,
                        lakeidList=NULL,
                        parallel=FALSE,
                        useDatatable=TRUE,
                        pattern=glob2rx('*.LAKEOUT_DOMAIN*')) {
  # Get files
  filesList <- list.files(path=pathOutdir,
                          pattern=pattern,
                          full.names=TRUE)
  if (length(filesList)==0) stop("No matching files in specified directory.")
  # Compile lake list
  firstFile <- filesList[1]
  ncid <- ncdf4::nc_open(firstFile)
  lakeCount <- ncid$dim[["station"]]$len
  ncdf4::nc_close(ncid)

  if (length(lakeCount)==0) stop("No lakes found.")

  # Single file read function
  ReadFile4Loop <- function(file., useDatatable.=TRUE) {
    out <- GetNcdfFile(file., variables=c("time"), exclude=TRUE, quiet=TRUE)
    dtstr <- basename(file.)
    dtstr <- unlist(strsplit(dtstr, "[.]"))[1]
    dtstr <- as.POSIXct(dtstr, format="%Y%m%d%H%M", tz="UTC")
    out$POSIXct <- dtstr
    if (useDatatable.) out<-data.table(out)
    out
  }
  # Loop through all files
  outList <- list()
  if (parallel) {
    packageList <- ifelse(useDatatable, c("ncdf4","data.table"), c("ncdf4"))
    outList <- foreach(file=filesList, .packages = packageList,
                       .combine=c) %dopar% {
                         out <- ReadFile4Loop(file)
                         if (!is.null(idList)) {
                           if (useDatatable) {
                             out <- out[trimws(station_id) %in% lakeidList,]
                           } else {
                             out <- subset(out, out$trimws(station_id) %in% lakeidList)
                           }
                         }
                         list(out)
                       }
  } else {
    for (file in filesList) {
      out <- ReadFile4Loop(file)
      if (!is.null(lakeidList)) {
        if (useDatatable) {
          out <- out[trimws(station_id) %in% lakeidList,]
        } else {
          out <- subset(out, out$trimws(station_id) %in% lakeidList)
        }
      }
      outList <- c(outList, list(out))
    }
  }
  if (useDatatable) {
    outDT <- data.table::rbindlist(outList)
    outDT<-outDT[,.SD,.SDcols=c("station_id","POSIXct","inflow","outflow","elevation")]
  } else {
    outDT <- do.call("rbind", outList)
    outDT<-subset(outDT,select=c("station_id","POSIXct","inflow","outflow","elevation"))
  }
  outDT
}

