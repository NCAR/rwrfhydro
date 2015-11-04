#' Visualize WRF Hydro domain files. 
#'
#' \code{VisualizeDomain} creates basic plots of WRF Hydro domains.
#' 
#' Crude plots of the WRF Hydro domain files. The routine accepts a file path/name for either the geo 
#' (coarse resolution) or hydro (fine resolution) files. Spatial variables of interest are listed
#' if none is provided. The return is a function (a closure which encapuslates the domain data) which 
#' creates a plot when called. The arguments to the function can be changed to tailor the plot 
#' (arguments are passed to ggmap and ggplot inside the function). This function (the closure) returns
#' a ggplot object whose data can be accessed.
#' 
#' @param file A path/name to a geo or hydro domain file.
#' @param variable An optional variable name of interest within the file.
#' @param plot Logical: plot or not?
#' @return A function which can be called to plot the data and allow adjustment of its arguments, the plotting parameters.
#' @examples
#' ## See the vignette "WRF Hydro Domain and Channel Visualization", for details. 
#' ## set your test case path
#' \dontrun{
#' tcPath <- '~/wrfHydroTestCases/'
#' fcPath <- paste0(tcPath,'Fourmile_Creek/')
#' hydroFile<-paste0(fcPath,'/DOMAIN/hydro_OrodellBasin_100m.nc')
#' GgMapFunction <- VisualizeDomain(hydroFile, "CHANNELGRID")
#' ggMap1 <- GgMapFunction(zoom=11, pointshape=15, pointsize=7, 
#'                         source="google", maptype="terrain")
#' # Add a streamflow gauge point; compare reality and the model.
#' orodellLonLat <- data.frame(lon=c(254.6722259521484375, 254.67374999999998408)-360, 
#'                            lat=c(40.019321441650390625, 40.018666670000001773),
#'                            gauge=c('model','USGS'))
#' ggMap2 <- GgMapFunction(location=c(lon=orodellLonLat$lon[1], lat=orodellLonLat$lat[1]),
#'                         zoom=14, pointshape=15, pointsize=7, 
#'                         source="google", maptype="terrain", plot=FALSE) 
#' ggMap2 + geom_point(data=orodellLonLat, aes(x=lon,y=lat, shape=gauge)) +
#'          scale_x_continuous(limits=rev(orodellLonLat$lon+c( .01 ,-.01 ))) +
#'          scale_y_continuous(limits=rev(orodellLonLat$lat+c( .005,-.005))) 
#' }
#' @concept plot DART
#' @keywords hplot
#' @family domain
#' @export
VisualizeDomain <- function(file, variable=NULL, plot=TRUE) {
  
  nc <- ncdf4::nc_open(file)  
  varNames <- names(nc$var)
  # right now, long names are identical in all cases
  longNames <- plyr::laply(nc$var, '[[', 'longname')
  
  # Determine if this is coarse or fine grid file.
  resolution <- 'fine'
  if(any(tolower(names(nc$dim))=='land_cat')) resolution <- 'coarse'
  
  # A coarse file must have at least 3 dimensions, 
  # 2 in space and (time is the unlimited/last dimension)
  nD <- if(resolution=='coarse') 2 else 1
  atLeast3D <- plyr::laply(nc$var, function(ll) length(ll$varsize) > nD)
  varNames  <- varNames[atLeast3D]
  longNames <- longNames[atLeast3D]
  
  # if no variable specified, ask to user to choose
  if(!length(variable)) {
    varInd <- readline(prompt=cat(paste(1:length(varNames),varNames, sep=': '),sep='\n'))
    variable <- varNames[as.numeric(varInd)]
  }
  
  varDimIds <- nc$var[[variable]]$dimids
  allDimIds <- plyr::llply(nc$var, '[[', 'dimids')
  dimIds <- allDimIds[ which( plyr::laply(allDimIds, length) == length(varDimIds)) ]
  matchDimIds <- plyr::llply(dimIds, function(ll) all(ll==varDimIds))
    
  # what are the coordinate variables?
  abscissaeNames <- if(resolution=='coarse') c('XLAT_M','XLONG_M') else c('LATITUDE','LONGITUDE')
  variableDf <- data.frame(y    = as.vector(ncdf4::ncvar_get(nc, varid=abscissaeNames[1])),
                           x    = as.vector(ncdf4::ncvar_get(nc, varid=abscissaeNames[2])),
                           value= as.vector(ncdf4::ncvar_get(nc, varid=variable))      )
  
  # done with ncdf file
  ncdf4::nc_close(nc)
  
  leftBotRightTop <-  with(variableDf, c(left=min(x), bottom=min(y), right=max(x), top=max(y)) )
    
  yLabel <- paste0(nc$var[[variable]]$dim[[1]]$name,' : ',
                   nc$var[[abscissaeNames[1]]]$longname,'\n(',
                   nc$var[[abscissaeNames[1]]]$units,')' )
  xLabel <- paste0(nc$var[[variable]]$dim[[2]]$name,' : ',
                   nc$var[[abscissaeNames[2]]]$longname,'\n(',
                   nc$var[[abscissaeNames[2]]]$units,')' )
  valueLabel <- paste0(nc$var[[variable]]$longname,' (',nc$var[[variable]]$units,')')
    
  # RETURN a closure (encapuslate the data) which allows the plot parameters to be tweaked.
  # The closure returns the ggMapObject - might be ways to merge or build these.
  outFunc <- 
    function(location=leftBotRightTop, zoom=10, 
            source='google', maptype='hybrid', 
            pointsize=3, pointshape=15, plot=TRUE ) {
    # Calling library may be forbidden in the package, but this dosent actually get executed in 
    # the package. It seems that ggmap:: should take care of this, but it dosent.
    library(ggplot2) ## called in the closure.
    theMap <- ggmap::get_map(location, zoom = zoom, source = source, maptype=maptype)   
    ggMapObj <- 
      ggmap::ggmap(theMap, extent='normal') +
      ggplot2::geom_point(data=variableDf, aes(x=x, y=y, color=value), size=pointsize, shape=pointshape) +
      ggplot2::scale_color_gradientn(name=valueLabel, colours=c('red','green','blue')) +
      ggplot2::scale_x_continuous(name=xLabel) + 
      ggplot2::scale_y_continuous(name=yLabel)
    if(plot) print(ggMapObj) 
    ggMapObj
  }
  
  if(plot) outFunc()
  
  outFunc
}