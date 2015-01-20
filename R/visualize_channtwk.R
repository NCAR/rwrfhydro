#' Visualize WRF Hydro channel link indices and get coordinates.
#'
#' \code{VisualizeChanNtwk} shows the channel network indices and provides interactive selection to retrieve link coordinates.
#' 
#' Read a CHANRTOUT file and display the link indicies. Returns a function which allows interactive querying of individual links for lat and lon.
#' 
#' The arguments of the returned function are:
#' \describe{
#'   \item{\code{location}}{Most generically, the center of the google (or other) map. More specifically, this is the argument 
#'   passed to the \pkg{ggmap} for its argument of the same name. (Default\code{=c(lon=mean(range(linkDf$lon)),lat=mean(range(linkDf$lat)))})}
#' 
#'   \item{\code{zoom=11}}{The zoom level for the google (or other) map. See \pkg{ggmap} for more details.}
#' 
#'   \item{\code{source='google'}}{The source for the underlying map. See \pkg{ggmap} package for details.}
#' 
#'   \item{\code{maptype='terrain'}}{The map type for \pkg{ggmap}.}
#' 
#'   \item{\code{padPlot=.1}}{The fraction of the range (in both lon and lat) of the channel network to expand the plot by.}
#' 
#'   \item{\code{gaugeZoom=NULL}}{The name of the gauge you'd like to zoom in on. This will likely require finessing zoom and padPlot to make it look nice.}
#' 
#'  \item{\code{clickSelect=FALSE}}{Do you want to click on the plot to query a specfic point? You only get one click per function call.}
#' 
#'  \item{\code{linkShape=5}}{The shape code (ggplot2) for the gridded link elements.}
#' 
#'  \item{\code{gaugeShape=4}}{The shape code (ggplot2) for the gauges.}
#' }
#' @param file A path/name to an output YYYMMDDHHmm.CHRTOUT_DOMAIN* file or a hydroDART Posterior_Diag.nc file.
#' @param gaugePts Optional list of gauge points. Nearest stream links are found. See examples.
#' @param excludeInds Optional index of channel network to exclude. See examples.
#' @param gaugeAccuracy The number of digits printed for the gauge information.
#' @param plot Logical to plot or not.
#' @return A function which allows the plot to be interactively queried once each time it is run and returns 
#' the coordinates of the selected location. Details provided in details above. 
#' 
#' @examples
#' fileCh <- GetPkgDataPath("Fourmile_test_case_AD.201205150100.CHRTOUT_DOMAIN1.0001.nc")
#' ## The basic function call returns a function which you will use. 
#' LocLinkFun<-VisualizeChanNtwk(fileCh)
#' ## You can look at the function arguments with
#' args(LocLinkFun)
#' ## Next, click at the outlet point to see that it has (q=) 0 flow.
#' LocLinkFun(click=TRUE)
#' ## Any set of valid indices can be excluded.
#' LocLinkFun<-VisualizeChanNtwk(fileCh, exclude=350) 
#' ## Now clicking at the lowest point reveals that that index is 1.
#' LocLinkFun(click=TRUE) 
#' ## Add gauges and find their nearest links' coords
#' gaugePts <-
#'   list(orodell   =data.frame(lon=254.67374999999998408,
#'                              lat=40.018666670000001773),
#'        loganMill =data.frame(lon=254.63508330000001934,
#'                              lat=40.042027779999997961),
#'        sunshine  =data.frame(lon=254.65122220000000652,
#'                              lat=40.05761110000000258)  )
#' ## You can just get the location information.
#' VisualizeChanNtwk(fileCh, gaugePts=gaugePts, plot=FALSE)
#' ## But you'll still want to exclude the lowest point.
#' LocLinkFun <- VisualizeChanNtwk(fileCh, gaugePts=gaugePts, exc=350, plot=FALSE)
#' ## Increase the accuracy of the lon/lat ouput 
#' LocLinkFun <- VisualizeChanNtwk(fileCh, gaugePts=gaugePts, exc=350, plot=FALSE, gaugeAccuracy=17)
#' ## Now make the plot that was suppressed in the previous call. 
#' LocLinkFun()
#' ## Change the amount of padding around the domain and the shape of the gauge symbols.
#' LocLinkFun(pad=.3, gaugeShape=16)
#' ## Zoom to the orodell gauge. 
#' LocLinkFun(zoom=14, gaugeShape=16, gaugeZoom='orodell', pad=15)
#' ## Zoom to logan mill gauge
#' LocLinkFun(zoom=15, gaugeShape=16, gaugeZoom='loganMill', pad=15)
#' LocLinkFun(zoom=15, gaugeShape=16, gaugeZoom='loganMill', pad=15, click=TRUE)
#' ## the sunshine location is totally wrong, guess I'll have to tell the GS.
#' LocLinkFun(zoom=12, gaugeShape=16, gaugeZoom='sunshine', pad=5, click=TRUE)
#' @export
VisualizeChanNtwk <- function(file, gaugePts=NULL, excludeInds=NULL,
                              gaugeAccuracy=12, plot=TRUE) {

  ## Get the data.
  ncid <- ncdf4::nc_open(file)
  
  if(length(grep('_Diag.nc',file))) {
    lat <- ncdf4::ncvar_get(ncid, 'latRt')
    lon <- ncdf4::ncvar_get(ncid, 'lonRt')
    linkIndX <- ncdf4::ncvar_get(ncid, 'linkIndX')
    linkIndY <- ncdf4::ncvar_get(ncid, 'linkIndY')
    dum <- ncdf4::nc_close(ncid)    
    linkDf <- data.frame( ind = 1:length(linkIndX) )
    linkDf$lon <- plyr::laply( linkDf$ind, function(ii) lon[linkIndX[ii],linkIndY[ii]] )
    linkDf$lat <- plyr::laply( linkDf$ind, function(ii) lat[linkIndX[ii],linkIndY[ii]] )
    rm('lon','lat','linkIndX','linkIndY')
  } 
  
  if(length(grep('CHRTOUT',file))) {
    lat <- ncdf4::ncvar_get(ncid,'latitude')
    lon <- ncdf4::ncvar_get(ncid,'longitude')
    q <- ncdf4::ncvar_get(ncid,'streamflow')
    dum <- ncdf4::nc_close(ncid)
    linkDf <- data.frame( ind = 1:length(lat) )
    linkDf$lon <- lon
    linkDf$lat <- lat
    linkDf$q <- q
    rm('lon','lat')
  }

  if(length(excludeInds)) linkDf <- linkDf[-excludeInds,]
  
  ## standardize the lon just in case
  linkDf$lon <- StdLon(linkDf$lon)

  ## find nearest neighbors if gaugePts was defined.
  if(length(gaugePts)) {
    ## This is better way of handling 
    gaugePtsDf <- plyr::ldply(gaugePts, .id='location')
    
    StdLon <- StdLon
    ## standardize the lon to +-180
    gaugePtsDf <- plyr::ddply(gaugePtsDf,
                              plyr::.(location, lon, lat), 
                              plyr::summarize,
                              lon=StdLon(lon))
     
    ## the euclidean metric in lat/lon works fine.
    FindNn <- function(dd) {
      whMin <- which.min(sqrt( (dd$lon-linkDf$lon)^2 + (dd$lat-linkDf$lat)^2 ))
      dd$chanInd <- whMin
      dd$lon <- linkDf$lon[whMin]
      dd$lat <- linkDf$lat[whMin]
      dd$modelFile <- file
      if('q' %in% names(linkDf)) dd$q <- linkDf$q[whMin]
      dd$system <- 'model'
      
      dd
    }
    gaugePtsModelDf <- plyr::ddply(gaugePtsDf, plyr::.(location), FindNn)
  
    ## combine real world and modeled gauges
    gaugePtsDf$chanInd <- gaugePtsDf$modelFile <- NA
    if('q' %in% names(linkDf)) gaugePtsDf$q <- NA
    gaugePtsDf$system  <- 'gauge'
    gaugePtsBothDf <- rbind(gaugePtsDf, gaugePtsModelDf)

    gaugeVarsPrint <- c('system','chanInd','q', 'lon','lat') 
    if(!('q' %in% names(linkDf))) gaugeVarsPrint <- gaugeVarsPrint[-3]
    
    ## print out the gauge information?
    PrintGauge <- function(dd) {
      cat(paste0('** ',dd$location[1],' **********'),sep='\n')
      print(dd[gaugeVarsPrint], row.names=FALSE, digits=gaugeAccuracy)
      NULL
    }
    invisible(plyr::ddply(gaugePtsBothDf, plyr::.(location), PrintGauge)) 
  
  }

  ## This function is going to be returned as a closure.
  ## It's env is visChanNtwkm which includes linkDf, and maybe the reference to the
  ## viewport?
  GetChanPoint <- function(location=c(lon=mean(range(linkDf$lon)),
                                      lat=mean(range(linkDf$lat)) ),
                           zoom=11, source='google', maptype='terrain',
                           padPlot=.1,
                           gaugeZoom=NULL, clickSelect=FALSE,
                           linkShape=5, gaugeShape=4) {
    
    if(length(gaugeZoom)) {
      if(gaugeZoom %in% gaugePtsBothDf$location) {
        subGaugeDf <- subset(gaugePtsBothDf, location==gaugeZoom)
        plotLimX <- PadRange(range(subGaugeDf$lon),diff=padPlot)
        plotLimY <- PadRange(range(subGaugeDf$lat),diff=padPlot)
        location <- c(lon=mean(range(subGaugeDf$lon)),
                      lat=mean(range(subGaugeDf$lat)) )
      }
    } else {
      plotLimX <- PadRange(range(linkDf$lon),diff=padPlot)
      plotLimY <- PadRange(range(linkDf$lat),diff=padPlot)
    }
    
    theMap <- ggmap::get_map(location, zoom = zoom, source = source, maptype=maptype)

    library(ggplot2)  ## not called except in the closure 

    thePlot <-
      ggmap::ggmap(theMap, extent='normal') +
      ## the following 2 lines are CRITICAL to the click
      ggplot2::scale_x_continuous(expand=c(0,0), limits=plotLimX) +
      ggplot2::scale_y_continuous(expand=c(0,0), limits=plotLimY) 

    
    thePlot <- thePlot +
      if('q' %in% names(linkDf)) {
        ggplot2::geom_point( data=linkDf, ggplot2::aes(x=lon, y=lat, size=q),
                            color='darkblue', shape=linkShape )
      } else {
        ggplot2::geom_point( data=linkDf, ggplot2::aes(x=lon, y=lat),
                            color='darkblue', shape=linkShape )
      }
    
    if(length(gaugePts)) {
      thePlot <- 
        thePlot +
        ggplot2::geom_point(data=gaugePtsBothDf,
                            ggplot2::aes(x=lon, y=lat, shape=system, color=location),
                            size=4) +
        ggplot2::scale_shape_manual(values=c(gaugeShape,linkShape))
    }
    
    if(clickSelect) {
      print(thePlot + ggplot2::ggtitle('Please click to get info about the channel link'))
      gridNames <- grid::grid.ls(print=FALSE)[['name']]
      x <- gridNames[grep("panel.[1-9]-", gridNames)] #locate the panel
      grid::seekViewport(x)
      cat("Please click on plot to select nearest point to your click...",sep='\n')
      clickPt <-  grid::grid.locator("npc")
      clickPt <- as.numeric(substring(clickPt, 1, nchar(clickPt)-3))
      
      mapMinMax <- attributes(theMap)$bb
      locX <- as.numeric(mapMinMax['ll.lon'] + 
                         clickPt[1] * diff(as.numeric(mapMinMax[c('ll.lon','ur.lon')])))
      locY <- as.numeric(mapMinMax['ll.lat'] + 
                         clickPt[2] * diff(as.numeric(mapMinMax[c('ll.lat','ur.lat')])))
      
      locX <- min(plotLimX) + clickPt[1] * diff(range(plotLimX))
      locY <- min(plotLimY) + clickPt[2] * diff(range(plotLimY))
      
      whClosest <- which.min( sqrt((linkDf$lon-as.numeric(locX))^2 +
                                   (linkDf$lat-locY)^2))
      
      thePlot <- 
        thePlot +
        ggplot2::geom_point(data=data.frame(lon=linkDf$lon[whClosest],
                            lat=linkDf$lat[whClosest]),
       ggplot2::aes(x=lon,y=lat), color='cyan') +
       ggplot2::ggtitle('Selected point in cyan, information printed to terminal.')
      
      closestDf <- linkDf[whClosest,]
      cat('Selected point (in cyan on plot) data:',sep='\n')
      print(closestDf, digits=12, row.names=FALSE)
      print(thePlot)
    } else print(thePlot)
   
  }
  
  if(plot) GetChanPoint() 
  invisible(GetChanPoint)
}
