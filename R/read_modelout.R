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
#' @return A dataframe containing the forecast points output flow data.
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
ReadFrxstPts <- function(pathOutfile) {
    myobj <- read.table(pathOutfile, header=F, sep=",", colClasses=c("character","character","integer","numeric","numeric","numeric","numeric","numeric"), na.strings=c("********","*********","************"))
    colnames(myobj) <- c("secs","timest","st_id","st_lon","st_lat","q_cms","q_cfs","dpth_m")
    myobj$POSIXct <- as.POSIXct(as.character(myobj$timest), format="%Y-%m-%d %H:%M:%S", tz="UTC")
    myobj$wy <- ifelse(as.numeric(format(myobj$POSIXct,"%m"))>=10, as.numeric(format(myobj$POSIXct,"%Y"))+1, as.numeric(format(myobj$POSIXct,"%Y")))
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
    myobj$POSIXct <- as.POSIXct(as.character(myobj$timest), format="%Y-%m-%d_%H:%M:%S",tz="UTC")
    myobj$wy <- ifelse(as.numeric(format(myobj$POSIXct,"%m"))>=10, as.numeric(format(myobj$POSIXct,"%Y"))+1, as.numeric(format(myobj$POSIXct,"%Y")))
    myobj
}


#' Read WRF-Hydro (w/NoahMP) LDASOUT data files and generate basin-wide mean water budget variables.
#'
#' \code{ReadLdasoutWb} reads in WRF-Hydro (w/NoahMP) LDASOUT files and outputs a time series of
#' basin-wide mean variables for water budget calculations.
#'
#' \code{ReadLdasoutWb} reads standard-format WRF-Hydro (w/NoahMP) LDASOUT NetCDF files and calculates
#' basin-wide mean values for each time step suitable for running basin water budget calculations.
#'
#' OUTPUT NoahMP LDASOUT water budget variables:
#' \itemize{
#'    \item ACCECAN: Mean accumulated canopy evaporation (mm)
#'    \item ACCEDIR: Mean accumulated surface evaporation (mm)
#'    \item ACCETRAN: Mean accumulated transpiration (mm)
#'    \item ACCPRCP: Mean accumulated precipitation (mm)
#'    \item CANICE: Mean canopy ice storage (mm)
#'    \item CANLIQ: Mean canopy liquid water storage (mm)
#'    \item SFCRNOFF: Mean surface runoff from LSM \emph{(meaningful for an LSM-only run)} (mm)
#'    \item SNEQV: Mean snowpack snow water equivalent (mm)
#'    \item UGDRNOFF: Mean subsurface runoff from LSM \cr \emph{(meaningful for an LSM-only run)} (mm)
#'    \item SOIL_M1: Mean soil moisture storage in soil layer 1 (top) (mm)
#'    \item SOIL_M2: Mean soil moisture storage in soil layer 2 (mm)
#'    \item SOIL_M3: Mean soil moisture storage in soil layer 3 (mm)
#'    \item SOIL_M4: Mean soil moisture storage in soil layer 4 (bottom) (mm)
#' }
#'
#' @param pathOutdir The full pathname to the output directory containing the LDASOUT files.
#' @param pathDomfile The full pathname to the high-res hydro domain NetCDF file used in
#' the model run (for grabbing the basin mask).
#' @param mskvar The variable name in pathDomfile to use for the mask (DEFAULT="basn_msk").
#' @param basid The basin ID to use (DEFAULT=1)
#' @param aggfact The high-res (hydro) to low-res (LSM) aggregation factor (e.g., for a 100-m
#' routing grid and a 1-km LSM grid, aggfact = 10)
#' @param ncores If multi-core processing is available, the number of cores to use (DEFAULT=1).
#' Must have doMC installed if ncores is more than 1.
#' @param pattern Pattern to match in the model output (DEFAULT=glob2rx('*LDASOUT_DOMAIN*'))
#' @return A dataframe containing a time series of basin-wide mean water budget variables.
#'
#' @examples
#' ## Take an OUTPUT directory for a daily LSM timestep model run of Fourmile Creek and
#' ## create a new dataframe containing the basin-wide mean values for the major water budget
#' ## components over the time series.
#'
#' \dontrun{
#' modLdasoutWb1d.mod1.fc <- 
#'   ReadLdasoutWb("../RUN.MOD1/OUTPUT", "../DOMAIN/Fulldom_hires_hydrofile_4mile.nc", 
#'                 ncores=16)
#' }
#' @keywords IO univar ts
#' @concept dataGet
#' @family modelDataReads
#' @export
ReadLdasoutWb <- function(pathOutdir, pathDomfile, mskvar="basn_msk", 
                          basid=1, aggfact=10, ncores=1, 
                          pattern=glob2rx('*LDASOUT_DOMAIN*')) {
    if (ncores > 1) {
        doMC::registerDoMC(ncores)
        }
    # Setup mask
    mskvar <- CreateBasinMask(pathDomfile, mskvar=mskvar, basid=basid, aggfact=aggfact)
    # Calculate basin area as a cell count
    basarea <- sum(mskvar)
    # Setup basin mean function
    basin_avg <- function(myvar, minValid=-1e+30) {
        myvar[which(myvar<minValid)]<-NA
        sum(mskvar*myvar, na.rm=TRUE)/sum(mskvar, na.rm=TRUE)
        }
    basin.level1 <- list( start=c(1,1,1,1), end=c(dim(mskvar)[1],1,dim(mskvar)[2],1), stat='basin_avg', mskvar, env=environment() )
    basin.level2 <- list( start=c(1,2,1,1), end=c(dim(mskvar)[1],2,dim(mskvar)[2],1), stat='basin_avg', mskvar, env=environment() )
    basin.level3 <- list( start=c(1,3,1,1), end=c(dim(mskvar)[1],3,dim(mskvar)[2],1), stat='basin_avg', mskvar, env=environment() )
    basin.level4 <- list( start=c(1,4,1,1), end=c(dim(mskvar)[1],4,dim(mskvar)[2],1), stat='basin_avg', mskvar, env=environment() )
    basin.surf <-  list(start=c(1,1,1), end=c(dim(mskvar)[1],dim(mskvar)[2],1), stat='basin_avg', mskvar, env=environment())
    # Setup LDASOUT variables to use
    variableNames <- c('ACCECAN','ACCEDIR','ACCETRAN','ACCPRCP','CANICE','CANLIQ','SFCRNOFF','SNEQV', 'UGDRNOFF', rep('SOIL_M',4))
    ldasoutVars <- as.list( variableNames ) 
    names(ldasoutVars) <- c('ACCECAN','ACCEDIR','ACCETRAN','ACCPRCP','CANICE','CANLIQ','SFCRNOFF','SNEQV', 'UGDRNOFF', paste0("SOIL_M",1:4))
    ldasoutVariableList <- list( ldasout = ldasoutVars )
    # For each variable, setup relevant areas and levels to do averaging
    cell <-  basin.surf
    level1 <- basin.level1
    level2 <- basin.level2
    level3 <- basin.level3
    level4 <- basin.level4
    ldasoutInd <- list( cell, cell, cell, cell, cell, cell, cell, cell, cell, level1, level2, level3, level4 )
    names(ldasoutInd) <- names(ldasoutVars)
    ldasoutIndexList <- list( ldasout = ldasoutInd )
    # Run GetMultiNcdf
    ldasoutFilesList <- list( ldasout = list.files(path=pathOutdir, pattern=pattern, full.names=TRUE))
    if (ncores > 1) {
        ldasoutDF <- GetMultiNcdf(indexList=ldasoutIndexList, 
                                  variableList=ldasoutVariableList, 
                                  filesList=ldasoutFilesList, parallel=TRUE )
        }
    else {
        ldasoutDF <- GetMultiNcdf(indexList=ldasoutIndexList, 
                                  variableList=ldasoutVariableList, 
                                  filesList=ldasoutFilesList, parallel=FALSE )
        }
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
#'    \item QBDRYRT: Mean accumulated flow volume routed outside of the domain from the boundary cells (mm)
#' }
#'
#' @param pathOutdir The full pathname to the output directory containing the RTOUT files.
#' @param pathDomfile The full pathname to the high-res hydro domain NetCDF file used in
#' the model run (for grabbing the basin mask).
#' @param mskvar The variable name in pathDomfile to use for the mask (DEFAULT="basn_msk").
#' @param basid The basin ID to use (DEFAULT=1)
#' @param ncores If multi-core processing is available, the number of cores to use (DEFAULT=1).
#' Must have doMC installed if ncores is more than 1.
#' @return A dataframe containing a time series of basin-wide mean water budget variables.
#'
#' @examples
#' ## Take an OUTPUT directory for an hourly routing timestep model run of Fourmile Creek (Basin ID = 1)
#' ## and create a new dataframe containing the basin-wide mean values for the major water budget
#' ## components over the time series.
#'
#' \dontrun{
#' modRtout1h.mod1.fc <- 
#'   ReadRtout("../RUN.MOD1/OUTPUT", "../DOMAIN/Fulldom_hires_hydrofile_4mile.nc", 
#'             basid=1, ncores=16)
#' }
#' @keywords IO univar ts
#' @concept dataGet
#' @family modelDataReads
#' @export
ReadRtout <- function(pathOutdir, pathDomfile, mskvar="basn_msk", basid=1, ncores=1) {
    if (ncores > 1) {
        doMC::registerDoMC(ncores)
        }
    # Setup mask
    msk <- ncdf4::nc_open(pathDomfile)
    mskvar <- ncdf4::ncvar_get(msk,mskvar)
    # Subset to basinID
    mskvar[which(mskvar != basid)] <- 0.0
    mskvar[which(mskvar == basid)] <- 1.0
    # Reverse y-direction for N->S hydro grids to S->N
    mskvar <- mskvar[,order(ncol(mskvar):1)]
    # Setup mean functions
    basin_avg <- function(myvar, minValid=-1e+30) {
        myvar[which(myvar<minValid)]<-NA
        sum(mskvar*myvar, na.rm=TRUE)/sum(mskvar, na.rm=TRUE)
        }
    basin.surf <-  list(start=c(1,1,1), end=c(dim(mskvar)[1],dim(mskvar)[2],1), stat='basin_avg', mskvar, env=environment())
    # Setup RTOUT variables to use
    variableNames <- c('QSTRMVOLRT','SFCHEADSUBRT','QBDRYRT')
    chrtoutVars <- as.list( variableNames )
    names(chrtoutVars) <- variableNames
    chrtoutVariableList <- list( chrtout = chrtoutVars )
    # For each variable, setup relevant areas and levels to do averaging
    cell <-  basin.surf
    chrtoutInd <- list( cell, cell, cell )
    names(chrtoutInd) <- names(chrtoutVars)
    chrtoutIndexList <- list( chrtout = chrtoutInd )
    # Run GetMultiNcdf
    chrtoutFilesList <- list( chrtout = list.files(path=pathOutdir, pattern=glob2rx('*.RTOUT_DOMAIN*'), full.names=TRUE))
    if (ncores > 1) {
        chrtoutDF <- GetMultiNcdf(indexList=chrtoutIndexList, 
                                  variableList=chrtoutVariableList, 
                                  filesList=chrtoutFilesList, parallel=TRUE )
        }
    else {
        chrtoutDF <- GetMultiNcdf(indexList=chrtoutIndexList, 
                                  variableList=chrtoutVariableList, 
                                  filesList=chrtoutFilesList, parallel=FALSE )
        }
    outDf <- ReshapeMultiNcdf(chrtoutDF)
    outDf
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
#' geoMsk <- CreateBasinMask("~/wrfHydroTestCases/Fourmile_Creek/DOMAIN/Fulldom_hydro_OrodellBasin_100m.nc", aggFact=10)
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
     basnmsk <- raster::as.matrix(raster::aggregate(raster::raster(basnmsk), fact=aggfact, fun=mean))
   }
   basnmsk
 }

#' Get geogrid cell indices from lat/lon (or other) coordinates.
#'
#' \code{GetGeogridIndex} reads in a set of lat/lon (or other) coordinates and 
#' generates a corresponding set of geogrid index pairs.
#'
#' \code{GetGeogridIndex} reads in a set of lat/lon (or other real-world) 
#' coordinates and a geogrid file and generates a corresponding set of 
#' geogrid index pairs.
#' 
#' @param xy The dataframe of lat/lon (or other) coordinates. The dataframe  
#' must contain one "x" and one "y" column at a minimum.
#' @param ncfile The full pathname to the WRF-Hydro geogrid domain file.
#' @param x The column name for the x coordinate value (DEFAULT="lon")
#' @param y The column name for the y coordinate value (DEFAULT="lat")
#' @param proj4 The proj4 string for the x/y coordinate projection 
#' (DEFAULT='+proj=longlat +datum=WGS84')
#' @return A dataframe containing the i, j indices (row, column).
#'
#' @examples
#' ## Take the geogrid (low-res) domain for Fourmile and a pair of lat/lon
#' ## coordinates for the Niwot Ridge SNOTEL site and get the index values.
#' \dontrun{
#' GetGeogridIndex(data.frame(lon=-105.54, lat=40.04), "~/wrfHydroTestCases/Fourmile_Creek/DOMAIN/geo_em.d01_1km_nlcd11.nc")
#' }
#' @keywords IO
#' @concept dataGet
#' @family modelDataReads
#' @export
GetGeogridIndex <- function(xy, ncfile, x="lon", y="lat", proj4='+proj=longlat +datum=WGS84') {
  # Create temp geogrid tif
  randnum <- round(runif(1)*10^8,0)
  ExportGeogrid(ncfile, "HGT_M", paste0("tmp_", randnum, ".tif"))
  geohgt <- raster::raster(paste0("tmp_", randnum, ".tif"))
  file.remove(paste0("tmp_", randnum, ".tif"))
  # Setup coords
  sp<-SpatialPoints(data.frame(x=xy[,x], y=xy[,y]))
  crs(sp)<-proj4
  sp2 <- spTransform(sp, crs(geohgt))
  outDf <- as.data.frame(rowColFromCell(geohgt, cellFromXY(geohgt, sp2)))
  outDf$ew <- outDf$col
  # Change row count from N->S to S->N
  outDf$sn <- dim(geohgt)[1] - outDf$row + 1
  outDf$row<-NULL
  outDf$col<-NULL
  outDf
}