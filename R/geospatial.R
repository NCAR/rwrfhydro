#' Creates a georeferenced TIF from a geogrid variable
#'
#' \code{ExportGeogrid} takes a NetCDF geogrid and converts the specified
#' variable into a georeferenced TIF file.
#'
#' \code{ExportGeogrid} takes a standard geogrid in NetCDF format and
#' converts the specified variable to a georeferenced geoTiff for use
#' in standard GIS tools.
#'
#' @param inFile A netcdf file containing the variable to be converted to a TIF. 
#' @param inVar The name of the variable to be converted, should exists in inFile.
#' @param outFile The geoTiff filename to create.
#' @param geoFile (OPTIONAL) The netcdf file containing the lat/lon
#' coordinates if they are not included in the inFile. This is useful,
#' for example, when creating a geotiff from an LDASOUT output file which
#' does not contain lat/lon coordinates but matches the spatial coordinate
#' system of the geogrid input file.
#' @param inLyr (OPTIONAL) The layer number to export if the variable has
#' more than 2 dimensions, e.g., for soil or snow layer variables.
#' @return NULL
#'
#' @examples
#' ## Export the HGT_M field from the geogrid file geo_em.d01_1km.nc
#' ## to a geoTiff called geogrid_hgt.tif.
#'
#' \dontrun{
#' ExportGeogrid("~/wrfHydroTestCases/Fourmile_Creek/DOMAIN/geo_em.d01_1km_nlcd11.nc",
#'              "HGT_M", "geogrid_hgt.tif")
#' ExportGeogrid("~/wrfHydroTestCases/Fourmile_Creek/RUN.RTTESTS/OUTPUT_ALLRT_DAILY/2013031500.LDASOUT_DOMAIN1",
#'              inVar="SOIL_M",
#'              outFile="20130315_soilm3.tif",
#'              geoFile="~/wrfHydroTestCases/Fourmile_Creek/DOMAIN/geo_em.d01_1km_nlcd11.nc",
#'              inLyr=3)
#' }
#' @keywords IO
#' @concept dataMgmt geospatial
#' @family geospatial
#' @export
ExportGeogrid <- function(inFile, inVar, outFile, geoFile=NA, inLyr=NA) {
  # Check packages
  if (!(require("rgdal") & require("raster") & require("ncdf4") )) {
    stop("Required packages not found. Must have R packages: rgdal (requires GDAL system install), raster, ncdf4")
  }
  
  inNC <- tryCatch(suppressWarnings(ncdf4::nc_open(inFile)),
                   error=function(cond) {message(cond); return(NA)})
  
  if (!all(is.na(inNC))){
    inNCVar <- ncdf4::ncvar_get(inNC, inVar)
    if (!is.na(inLyr)) inNCVar <- inNCVar[,inLyr,]
    varList <- names(inNC$var)
  }else{
    inNCVar<-inFile
  }
  # Data types
  typlist <- list("byte"="Byte",
                  "short"="Int16",
                  "int"="Int32",
                  "long"="Int32",
                  "float"="Float32",
                  "real"="Float32",
                  "double"="Float64",
                  "ubyte"="qmin_cfs",
                  "ushort"="UInt16",
                  "uint"="UInt32",
                  "int64"="Int64",
                  "uint64"="UInt64")
  # Get coords
  if (is.na(geoFile)) {
    coordNC <- inNC
    coordvarList <- varList
  } else {
    coordNC <- ncdf4::nc_open(geoFile)
    coordvarList <- names(coordNC$var)
  }
  if ("XLONG_M" %in% coordvarList & "XLAT_M" %in% coordvarList) {
    inNCLon <- ncdf4::ncvar_get(coordNC, "XLONG_M")
    inNCLat <- ncdf4::ncvar_get(coordNC, "XLAT_M")
  } else if ("XLONG" %in% coordvarList & "XLAT" %in% coordvarList) {
    inNCLon <- ncdf4::ncvar_get(coordNC, "XLONG")
    inNCLat <- ncdf4::ncvar_get(coordNC, "XLAT")
  } else if ("lon" %in% coordvarList & "lat" %in% coordvarList) {
    inNCLon <- ncdf4::ncvar_get(coordNC, "lon")
    inNCLat <- ncdf4::ncvar_get(coordNC, "lat")
  } else {
    stop('Error: Latitude and longitude fields not found (tried: XLAT_M/XLONG_M, XLAT/XLONG, lat/lon')
  }
  # Reverse column order to get UL in UL
  x <- as.vector(inNCLon[,ncol(inNCLon):1])
  y <- as.vector(inNCLat[,ncol(inNCLat):1])
  #coords <- data.frame(lon=x, lat=y)
  #coordinates(coords) <- c("lon", "lat")
  #proj4string(coords) <- CRS("+proj=longlat +a=6370000 +b=6370000 +no_defs")
  coords <- as.matrix(cbind(x, y))
  # Get geogrid and projection info
  map_proj <- ncdf4::ncatt_get(coordNC, varid=0, attname="MAP_PROJ")$value
  cen_lat <- ncdf4::ncatt_get(coordNC, varid=0, attname="CEN_LAT")$value
  cen_lon <- ncdf4::ncatt_get(coordNC, varid=0, attname="STAND_LON")$value
  truelat1 <- ncdf4::ncatt_get(coordNC, varid=0, attname="TRUELAT1")$value
  truelat2 <- ncdf4::ncatt_get(coordNC, varid=0, attname="TRUELAT2")$value
  if (map_proj==1) {
    geogrd.proj <- paste0("+proj=lcc +lat_1=",
                          truelat1, " +lat_2=", truelat2, " +lat_0=",
                          cen_lat, " +lon_0=", cen_lon,
                          " +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
    #geogrd.crs <- CRS(geogrd.proj)
  } else {
    stop('Error: Projection type not supported (currently this tool only works for Lambert Conformal Conic projections).')
  }
  dx <- ncdf4::ncatt_get(coordNC, varid=0, attname="DX")$value
  dy <- ncdf4::ncatt_get(coordNC, varid=0, attname="DY")$value
  if ( dx != dy ) {
    stop(paste0('Error: Asymmetric grid cells not supported. DX=', dx, ', DY=', dy))
  }
  #coords.proj <- spTransform(coords, geogrd.crs)
  projcoords <- rgdal::project(coords, geogrd.proj)
  ul <- projcoords[1,]
  # Define geo transform:
  # x coord of UL corner of UL cell
  gt0 = ul[1] - dx/2.0
  # x pixel resolution
  gt1 = dx
  # x rotation
  gt2 = 0.0
  # y coord of UL corner of UL cell
  gt3 = ul[2] + dy/2.0
  # y rotation
  gt4 = 0.0
  # y pixel resolution, should be negative
  gt5 = -dy
  gt = c(gt0,gt1,gt2,gt3,gt4,gt5)
  # Setup temp geotif
  d.drv <- new("GDALDriver", "GTiff")
  
  if (!all(is.na(inNC))) {
    typ<-typlist[[inNC$var[[inVar]]$prec]]
  }else{
    typ<-typlist[7]
  }
  tds.out <- new("GDALTransientDataset", driver = d.drv,
                 rows = dim(inNCVar)[2], cols = dim(inNCVar)[1],
                 bands = 1, type = typ)
  .Call("RGDAL_SetGeoTransform", tds.out, gt, PACKAGE = "rgdal")
  .Call("RGDAL_SetProject", tds.out, geogrd.proj, PACKAGE = "rgdal")
  # Prep NC variable
  inNCVarRast <- raster::as.matrix(raster::raster(inNCVar))
  inNCVarRast <- inNCVarRast[,ncol(inNCVarRast):1]
  # Insert data and export geotiff
  rgdal::putRasterData(tds.out, as.matrix(inNCVarRast))
  rgdal::saveDataset(tds.out, outFile)
  rgdal::GDAL.close(tds.out)
  if (!all(is.na(inNC)))  ncdf4::nc_close(inNC)
  if (!is.na(geoFile)) ncdf4::nc_close(coordNC)
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
#' @concept dataGet geospatial
#' @family geospatial
#' @export
GetGeogridIndex <- function(xy, ncfile, x="lon", y="lat", proj4='+proj=longlat +datum=WGS84') {
  # Create temp geogrid tif
  randnum <- round(runif(1)*10^8,0)
  ExportGeogrid(ncfile, "HGT_M", paste0("tmp_", randnum, ".tif"))
  geohgt <- raster::raster(paste0("tmp_", randnum, ".tif"))
  file.remove(paste0("tmp_", randnum, ".tif"))
  # Setup coords
  sp<-sp::SpatialPoints(data.frame(x=xy[,x], y=xy[,y]))
  raster::crs(sp)<-proj4
  sp2 <- sp::spTransform(sp, crs(geohgt))
  outDf <- as.data.frame(raster::rowColFromCell(geohgt, raster::cellFromXY(geohgt, sp2)))
  outDf$ew <- outDf$col
  # Change row count from N->S to S->N
  outDf$sn <- dim(geohgt)[1] - outDf$row + 1
  outDf$row<-NULL
  outDf$col<-NULL
  outDf
}

#' Pull necessary geospatial information from geogrid file used for
#' regridding and deprojection.
#' 
#' \code{GetGeogridSpatialInfo} Pull geospatial information about WRF-Hydro
#'  modeling domain from geogrid file.
#'  
#' @param geoFile The geogrid file information is being pulled from
#' @return spatialDF data frame containing geospatial information
#' @examples
#' \dontrun{
#' lccGeoDF <- GetGeogridSpatialInfo('./geo_em.d02.nc')
#' }
#' @keywords IO
#' @concept geospatial getData
#' @family geospatial
#' @export
GetGeogridSpatialInfo <- function(geoFile){
  #First check for existence of file
  if(!file.exists(geoFile)){
    warning(paste0('ERROR: Geogrid file: ',geoFile,' not found.'))
    return(0)
  }
  
  #Open geogrid file
  ncid <- ncdf4::nc_open(geoFile)
  
  #Pull grid type. Currently, only lambert conformal is accepted
  mapProj <- ncdf4::ncatt_get(ncid,varid=0,'MAP_PROJ')$value
  
  #Only support lambert conformal for now
  if(mapProj != 1){
    warning('ERROR: Geogrid either missing MAP_PROJ or contains invalid value.')
    warning('ERROR: Only MAP_PROJ of 1 (lambert conformal) is supported at this time')
    return(0)
  }
  
  if(mapProj == 1){
    #Create data frame to hold information
    dataOut = data.frame(matrix(NA, nrow=1, ncol=14))
    names(dataOut) <- c('DX','DY','GRID_TYPE','LAT1','LON1',
                        'REF_LAT','REF_LON',
                        'SPLAT1','SPLAT2','POLE_LAT','POLE_LON',
                        'MAP_PROJ','NCOL','NROW')
    
    #DX = Horizontal resolution in meters
    #DY = Vertical resoltuion in meters
    #GRID_TYPE = Type of staggered WRF grid - should be C
    #LAT1 = Latitude of lower left pixel cell in degrees
    #LON1 = Longitude of lower left pixel cell in degrees
    #REF_LAT = Reference latitude of domain in degrees
    #REF_LON = Reference longitude of domain in degrees
    #SPLAT1 = Standard parallel latitude 1 in degrees
    #SPLAT2 = Standard parallel latitude 2 in degrees.
    #         Note that SPLAT2 may be equal to SPLAT1
    #POLE_LAT = Latitude of the pole used.
    #POLE_LON = Longitude of the pole used.
    #MAP_PROJ = WRF defined map projection, should be 1
    #           for lambert conformal grids
    #NCOL = Number of columns in the WRF-Hydro domain
    #NROW = Number of rows in the WRF-Hydro domain
    
    dataOut$MAP_PROJ <- mapProj
    
    #Pull geospatial information important to the map projection
    #Grid type - only 'C' supported at this time
    gridType <- ncdf4::ncatt_get(ncid,varid=0,'GRIDTYPE')$value
    if(gridType != "C"){
      warning('ERROR: Only GRIDTYPE of C supported at this time')
      return(0)
    }
    dataOut$GRID_TYPE <- gridType
    
    #Resolution - should be in meters
    xRes <- ncdf4::ncatt_get(ncid,varid=0,'DX')$value
    yRes <- ncdf4::ncatt_get(ncid,varid=0,'DY')$value
    if((xRes != yRes) | (xRes <= 0) | (yRes <= 0)){
      warning('ERROR: Invalid DX or DY values')
      return(0)
    }
    dataOut$DX <- xRes
    dataOut$DY <- yRes
    
    #Extract lat/lon values from lower left pixel cell
    latArr <- ncdf4::ncvar_get(ncid,'XLAT_M')
    lonArr <- ncdf4::ncvar_get(ncid,'XLONG_M')
    lat1 <- latArr[1,1]
    lon1 <- lonArr[1,1]
    if((lat1 > 90.0) | (lat1 < -90.0)){
      warning('ERROR: Invalid LL latitude value')
      return(0)
    }
    if((lon1 > 360.0) | (lon1 < -180.0)){
      warning('ERROR: Invalid LL longitude value')
      return(0)
    }
    dataOut$LAT1 <- lat1
    dataOut$LON1 <- lon1
    
    #Reference (center) latitude
    refLat <- ncdf4::ncatt_get(ncid,varid=0,'MOAD_CEN_LAT')$value
    if((refLat > 90.0) | (refLat < -90.0)){
      warning('ERROR: Invalid reference latitude')
      return(0)
    }
    dataOut$REF_LAT <- refLat
    
    #Reference (center) longitude
    refLon <- ncdf4::ncatt_get(ncid,varid=0,'STAND_LON')$value
    if((refLon > 360.0) | (refLon < -180.0)){
      warning('ERROR: Invalid reference longitude')
      return(0)
    }
    dataOut$REF_LON <- refLon
    
    #Standard parallel latitude values. It's possible these two would be
    #equal.
    stdLat1 <- ncdf4::ncatt_get(ncid,varid=0,'TRUELAT1')$value
    stdLat2 <- ncdf4::ncatt_get(ncid,varid=0,'TRUELAT2')$value
    if((stdLat1 > 90.0) | (stdLat1 < -90.0)){
      warning('ERROR: Invalid standard parallel latitude 1')
      return(0)
    }
    if((stdLat2 > 90.0) | (stdLat2 < -90.0)){
      warning('ERROR: Invalid standard parallel latitude 2')
      return(0)
    }
    dataOut$SPLAT1 <- stdLat1
    dataOut$SPLAT2 <- stdLat2
    
    #Pole lat/lon
    poleLat <- ncdf4::ncatt_get(ncid,varid=0,'POLE_LAT')$value
    poleLon <- ncdf4::ncatt_get(ncid,varid=0,'POLE_LON')$value
    if((poleLat > 90.0) | (poleLon < -90.0)){
      warning('ERROR: Invalid pole latitude')
      return(0)
    }
    if((poleLon > 360.0) | (poleLon < -180.0)){
      warning('ERROR: Invalid pole longitude')
      return(0)
    }
    dataOut$POLE_LAT <- poleLat
    dataOut$POLE_LON <- poleLon
    
    #Grid dimensions
    nx <- ncdf4::ncatt_get(ncid,varid=0,'WEST-EAST_PATCH_END_UNSTAG')$value
    ny <- ncdf4::ncatt_get(ncid,varid=0,'SOUTH-NORTH_PATCH_END_UNSTAG')$value
    if((nx <= 0) | (ny <= 0)){
      warnings('ERROR: Invalid x or y dimension values')
      return(0)
    }
    dataOut$NCOL <- nx
    dataOut$NROW <- ny
  }
  return(dataOut)
} 




#' Pull projection information from geogrid file 
#'  
#' \code{GetProj} Pull projection information of WRF-Hydro
#'  modeling domain from geogrid file.
#'  
#' @param geoFile The geogrid file information is being pulled from
#' @return Character Projection
#' @examples
#' \dontrun{
#' proj4 <- GetProj('./geo_em.d02.nc')
#' }
#' @keywords IO
#' @concept geospatial getData
#' @family geospatial
#' @export
GetProj <- function(geoFile){
  
  # First check for existence of file
  if(!file.exists(geoFile)){
    warning(paste0('ERROR: Geogrid file: ',geoFile,' not found.'))
    return(0)
  }
  
  # open the geoFile
  coordNC <- tryCatch(suppressWarnings(ncdf4::nc_open(geoFile)),
                      error=function(cond) {message(cond); return(NA)})
  
  # Get geogrid and projection info
  mapProj <- ncdf4::ncatt_get(coordNC, varid=0, attname="MAP_PROJ")$value
  
  #Only support lambert conformal for now
  
  if(mapProj != 1){
    
    warning('ERROR: Geogrid either missing MAP_PROJ or contains invalid value.')
    warning('ERROR: Only MAP_PROJ of 1 (lambert conformal) is supported at this time')
    return(0)
    
  }else{
    
  cen_lat <- ncdf4::ncatt_get(coordNC, varid=0, attname="CEN_LAT")$value
  cen_lon <- ncdf4::ncatt_get(coordNC, varid=0, attname="STAND_LON")$value
  truelat1 <- ncdf4::ncatt_get(coordNC, varid=0, attname="TRUELAT1")$value
  truelat2 <- ncdf4::ncatt_get(coordNC, varid=0, attname="TRUELAT2")$value
  
  ncdf4::nc_close(coordNC)
  geogrd.proj <- paste0("+proj=lcc +lat_1=",
                          truelat1, " +lat_2=", truelat2, " +lat_0=",
                          cen_lat, " +lon_0=", cen_lon,
                          " +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
  return(geogrd.proj)
  }
}
