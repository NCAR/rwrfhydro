#' Creates raster file from polygon matching the \code{geoFile}
#'
#' \code{PolyToRaster} takes a NetCDF geogrid as well as a polygon
#' and return the attribute requested from polygon as a raster file.
#'
#' @param geoFile The geogrid NetCDF file.
#' @param useRfc Logical: if \code{TRUE} will use the loaded rfc polygon in rwrfhydro. 
#' Default is \code{FALSE}
#' @param polygon SpatialPolygon* This is used if you want to use a 
#' polygon already loaded in memory.
#' @param polygonAddress Character: Address to where polygonShapeFile is located.
#' @param polygonShapeFile Character: Name of a polygon shapefile.
#' @param field Numeric or character: The value(s) to be transferred. This can be
#'  a single number, or a vector of numbers that has the same length as the 
#'  number of spatial features (polygons).
#' @param fun Function or character: To determine what values to assign to cells 
#'  that are covered by multiple spatial features. You can use functions such as
#'  \code{min, max}, or \code{mean}, or one of the following character values: 
#'  \code{'first'}, \code{'last'}, \code{'count'}.
#' @param mask Logical: If \code{TRUE} the values of the input Raster object are
#'  'masked' by the spatial features of polygon shapefile. That is, cells that spatially
#'  overlap with the spatial features retain their values, the other cells
#'  become \code{NA}. Default is \code{FALSE}. 
#' @param maskValue Numeric: The value to be used for creating the mask. Default is \code{1}. 
#' @param getCoverMask Logical: If \code{TRUE}, the fraction of each grid cell that
#'   is covered by the polygons is returned (and the values of \code{field, fun,
#'   mask}. The fraction covered is estimated by
#'   dividing each cell into 100 subcells and determining presence/absence of
#'   the polygon in the center of each subcell
#' @param getCover Logical: If \code{TRUE}, the fraction of each grid cell that
#'   is covered by the each \code{field} is returned as a layer in the returned RasterStack.
#'   Name of the RasetrLayers in the RasterStacK is obtained from unique values of \code{field}.
#'   The fraction covered is estimated by
#'   dividing each cell into 100 subcells and determining presence/absence of
#'   the polygon in the center of each subcell
#' @param plot.it Logical: If \code{TRUE} will plot both the raster and the polygon on the same plot
#' @param parallel Logical: will be used only if \code{GetCover} is \code{TRUE} 
#' @return  return a RasterLayer or a RasterStack.
#'   
#' @examples
#' \dontrun{
#' 
#' #Example 1:
#' # To rasterize the rfc SpatialPolygonsDataFrame :
#'
#' r <- PolyToRaster(geoFile = "/glade/scratch/arezoo/IOC/ESMF/geo_em.d01.nc.conus_1km",
#'                  useRfc = TRUE, 
#'                  field ="BASIN_ID")
#'                  
#'# You can get the numbres assign to each BAISN_ID like :
#' 
#' # Example 2: 
#' # To return a mask with default value of 1 inside the polygons and NA outside
#' 
#' r1 <- PolyToRaster(geoFile = "/glade/scratch/arezoo/IOC/ESMF/geo_em.d01.nc.conus_1km",
#'                   useRfc = TRUE, 
#'                   field ="BASIN_ID", 
#'                   mask = TRUE)
#' 
#' # Example 3:
#' # To return a mask with value of 5 inside the polygons and NA outside
#'
#' r2 <- PolyToRaster(geoFile = "/glade/scratch/arezoo/IOC/ESMF/geo_em.d01.nc.conus_1km",
#'                               useRfc = TRUE, 
#'                               field ="BASIN_ID", 
#'                               mask = TRUE,
#'                               maskValue = 5)
#'
#' # Example 4:
#' # To return a raster with values to be the BASIN_ID (convert character to integer numbers)
#' # with fraction of each grid cell that is covered by the polygons
#'
#' r3 <- PolyToRaster(geoFile = "/glade/scratch/arezoo/IOC/ESMF/geo_em.d01.nc.conus_1km",
#'                               useRfc = TRUE, 
#'                               field ="BASIN_ID",
#'                               getCover = TRUE,
#'                               parallel = TRUE))
#' plot(r3)
#'
#' # Example 5:
#' # To read a shapefile (polygon) from disk and 
#' # return a raster with values of field 
#' # with fraction of each grid cell that is covered by the polygons
#'
#' r4 <- PolyToRaster(geoFile = "/glade/scratch/arezoo/IOC/ESMF/geo_em.d01.nc.conus_1km",
#'                   polygonAddress= "/glade/scratch/arezoo/QPF_verification_rwrfhydro/gis",
#'                   polygonShapeFile= "HUC4",
#'                   field ="HUC4")
#' plot(r4)
#' }
#' @keywords IO
#' @concept dataMgmt geospatial
#' @family geospatial
#' @export

PolyToRaster <- function(geoFile = NULL, 
                         useRfc = FALSE, 
                         polygon = NULL,
                         polygonAddress= NULL,
                         polygonShapeFile = NULL, 
                         field= NULL,
                         fun = 'last',
                         mask = FALSE, 
                         maskValue = 1, 
                         getCoverMask = FALSE,
                         getCover = FALSE,
                         plot.it = TRUE,
                         parallel = FALSE){
  
  ########################################################################
  #            read the geogrid of the WRF-HDRO domain                    #
  #            Create a raster file which will be used in rasetrize       #
  #########################################################################
  
  coordNC <- tryCatch(suppressWarnings(ncdf4::nc_open(geoFile)),
                      error=function(cond) {message(cond); return(NA)})
  
  coordvarList = names(coordNC[['var']])
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
  
  nrows <- dim(inNCLon)[2]
  ncols <- dim(inNCLon)[1]
  
  # Reverse column order to get UL in UL
  x <- as.vector(inNCLon[,ncol(inNCLon):1])
  y <- as.vector(inNCLat[,ncol(inNCLat):1])
  
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
  } else {
    stop('Error: Projection type not supported (currently this tool only works for Lambert Conformal Conic projections).')
  }
  
  dx <- ncdf4::ncatt_get(coordNC, varid=0, attname="DX")$value
  dy <- ncdf4::ncatt_get(coordNC, varid=0, attname="DY")$value
  if ( dx != dy ) {
    stop(paste0('Error: Asymmetric grid cells not supported. DX=', dx, ', DY=', dy))
  }
  
  projcoords <- rgdal::project(coords, geogrd.proj)
  
  # coordinates here refre to the cell center, 
  # We need to calculate the boundaries for the arster file
  
  xmn <- projcoords[1,1] - dx/2.0   # Left border 
  ymx <- projcoords[1,2] + dy/2.0   # upper border
  xmx <- xmn + ncols*dx             # Right border
  ymn <- ymx - nrows*dy             # Bottom border
  
  # Create a raster with using the geoFile 
  r <- raster::raster(resolution = dx,
                      xmn = xmn,
                      xmx = xmx,
                      ymn = ymn,
                      ymx = ymx,
                      crs = geogrd.proj)
  
  # if mask is true, craete a raster with all values equal to maskValue
  if (mask) {
    raster::values(r) <- maskValue
  }
  
  ##################################################################################
  #                  Get the polyon and transform the projection                   #
  #                  perform rasterization                                         #
  ##################################################################################
  
  # IF the polygon is going to be rfc, no need to feed any polygon since 
  # It has been already loaded with rwrfhydro package
  if (useRfc){
    
    polyg <- rwrfhydro::rfc
    
  }else if (!is.null(polygon)){
    
    polyg <- polygon
    
  }else{
    
    # read the polygon shape file
    polyg <- rgdal::readOGR(polygonAddress,polygonShapeFile)
  }
  
  # first transform the Geographich Coordinate System
  # of the polygon to raster

  polyg <- sp::spTransform(polyg, sp::CRS(geogrd.proj))
  
  # Crop teh polygon shapefile if it is bigger than the geoFile extent
  polyg <- raster::crop(polyg, r, snap = "out") 
  
  # I could not find any function to extend the polygon, therefore I will take a detour
  # will crop the raster file and then extend the new raster file at the end
  extr <- raster::extent(r) # save teh actual extent
  r <- raster::crop(r, polyg, snap = "out")
  
  # Both raster and polygon should have the same extent
  # so change the extent of the raster#
  #raster::extent(r)  <- raster::extent(polyg) 
  #setExtent(r, polyg, keepres= TRUE)
  
  if (getCover){
    
    st <- raster::stack()
    if (parallel){
      # Perform the parallel with foreach
      if (sum(is.element(search(), "package:foreach")) == 0){
        stop("foreach package is not loaded, no parallel job is submitted")
      }else{
        namesStack <- unique(polyg[[field]])
        st <- foreach::"%dopar%"(foreach::foreach(i = namesStack, .combine = function(...) raster::stack(...)), {
          
          # First choose only one polygon (maybe more than having the same name) 
          x <- polyg[polyg[[field]] == i,]
          
          # Since this new polygon would be smaller than the original polygon we can subset the raster
          rnew <- raster::crop(r, x, snap = "out")
          
          rnew <- raster::rasterize(x = x, 
                                    y = rnew, 
                                    field = field, 
                                    fun = fun,
                                    getCover = getCover)
          # change the extent to actual extent of geoFile 
          rnew <- raster::extend(rnew, extr)
        })
        names(st) <- namesStack
        return(st)
      }
    }else{
      
      # if it is not parallel, use for loop 
      
        namesStack <- unique(polyg[[field]])
        for (i in namesStack) {
          
          # First choose only one polygon (maybe more than having the same name) 
          x <- polyg[polyg[[field]] == i,]
          
          # Since this new polygon would be smaller than the original polygon we can subset the raster
          rnew <- raster::crop(r, x, snap = "out")
          
          rnew <- raster::rasterize(x = x, 
                                    y = rnew, 
                                    field = field, 
                                    fun = fun,
                                    getCover = getCover)
          # change the extent to actual extent of geoFile 
          rnew <- raster::extend(rnew, extr)
          st <- raster::stack(st, rnew)
        }
        names(st) <- namesStack
      return(st)
    } 
  }else{
    
    # if getCover is FALSE there is no need for 
    rnew <- raster::rasterize(x = polyg, 
                              y = r, 
                              field = field, 
                              fun = fun,
                              mask = mask,
                              getCover = getCoverMask)
    # change the extent to actual extent of geoFile 
    rnew <- raster::extend(rnew, extr)
    
    # add Raster Attributes Table:
    # simply the relationship between the integer number used in raster and the polygon property
    if (!(mask | getCoverMask)){
    rnew <- raster::as.factor(rnew)
    rat <- raster::levels(rnew)[[1]]
    rat[[field]] <- levels(droplevels(polyg[[field]]))
    levels(rnew) <- rat
    }
    
    if (plot.it){
    #  raster::plot(rnew)
    #  raster::plot(polyg, add =TRUE)
      plot(rnew)
      plot(polyg, add =TRUE)
    }
    return(rnew)
  }
}
