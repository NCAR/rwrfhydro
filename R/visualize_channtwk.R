#' Visualize WRF Hydro channel link indices and get coordinates.
#'
#' \code{VisualizeChanNtwk} shows the channel network indices and provides interactive selection to retrieve link coordinates.
#' 
#' Read a CHANRTOUT file and display the link indicies. Returns a function which allows interactive querying of individual links for lat and lon.
#' 
#' @param file A path/name to an output YYYMMDDHHmm.CHRTOUT_DOMAIN* file or a hydroDART Posterior_Diag.nc file.
#' @return A function which allows the plot to be interactively queried once each time it is run and returns 
#' the coordinates of the selected location.
#' @examples
#' fileCh <- GetPkgDataPath("Fourmile_test_case_AD.201205150100.CHRTOUT_DOMAIN1.0001.nc")
#' GetChanPt <- VisualizeChanNtwk(fileCh)
#' GetChanPt()
#' @export
VisualizeChanNtwk <- function(file, gaugePts=NULL) {

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
    rm('linkIndX','linkIndY')
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
  }

  ## standardize the lon just in case
  stdLon <- function(x) {
    x[which(x>180.)] <- x[which(x>180.)]-360.
    x
  }
  linkDf$lon <- stdLon(linkDf$lon)

  ## find nearest neighbors if gaugePts was defined.
  if(length(gaugePts)) {
    ## the euclidean metric in lat/lon works fine.
    ## first, standardize the lon to +-180
    gaugePts <- plyr::llply(gaugePts,
                            function(ll) {ll$lon <- stdLon(ll$lon)
                                          ll})
    findNn <- function(ll) {
      whMin <- which.min(sqrt( (ll$lon-linkDf$lon)^2 + (ll$lat-linkDf$lat)^2 ))
      ll$chanInd <- whMin
      ll$modelLon <- linkDf$lon[whMin]
      ll$modelLat <- linkDf$lat[whMin]
      ll$modelFile <- file
      ll
    }
    gaugePts <- plyr::llply(gaugePts, findNn)

    ## output the
    printCustom <- function(ll) {
      cat((names(gaugePts)[ll]),sep='\n')
      print(t(gaugePts[[ll]]))
    }
    invisible(plyr::llply(1:length(gaugePts),printCustom))   
   
  }


stop()  
  ## create the plot object
  location <- c(lon=mean(lon),lat=mean(lat))
  zoom=13
  source='google'
  maptype='hybrid'

  theMap <- ggmap::get_map(location, zoom = zoom, source = source, maptype=maptype)
  library(ggplot2)
  thePlot <-
    ggmap::ggmap(theMap, extent='normal') +
    ggplot2::geom_point( data=linkDf, ggplot2::aes(x=lon, y=lat) )
 
    if(length(gaugePts)) {

      gaugeDf <- plyr::ldply(gaugePts)

      thePlot <- thePlot +
        ggplot2::geom_point(data=plyr::ldply(gaugeDf),
                            ggplot2::aes(x=lon, y=lat, shape=system, color=location))
    }

  ##    ggplot2::coord_quickmap() +
  ##    ggplot2::scale_x_continuous(expand=c(0,0)) +
  ##    ggplot2::scale_y_continuous(expand=c(0,0)) 
  ##    ggplot2::scale_fill_gradientn(colours=RColorBrewer::brewer.pal(9,'YlGnBu'))
  print(thePlot)

  
  ## This function is going to be returned as a closure.
  ## It's env is visChanNtwkm which includes linkDf, and maybe the reference to the
  ## viewport?
  getChanPointInner <- function() {
    
    gridNames <- grid::grid.ls(print=FALSE)[['name']]
    x <- gridNames[grep("panel.[1-9]-", gridNames)] #locate the panel
    grid::seekViewport(x)
    clickPt <-  grid::grid.locator("npc")
    clickPt <- as.numeric(substring(clickPt, 1, nchar(clickPt)-3))
    
    locX <- min(thePlot$data$lon) + clickPt[1]*diff(range(thePlot$data$lon))
    locY <- min(thePlot$data$lat) + clickPt[2]*diff(range(thePlot$data$lat))
    
    whClosest <- which.min( (thePlot$data$lon-locX)^2 +
                            (thePlot$data$lat-locY)^2 )
    
    thePlot <- 
      ggplot2::ggplot( linkDf, ggplot2::aes(x=lon, y=lat, fill=ind) ) +
      ggplot2::geom_raster() +
      ggplot2::geom_point(data=thePlot$data[whClosest,],
                          ggplot2::aes(x=lon,y=lat), color='red') +
      ggplot2::scale_x_continuous(expand=c(0,0)) +
      ggplot2::scale_y_continuous(expand=c(0,0)) +
      ggplot2::ggtitle(paste('link index:',thePlot$data$ind[whClosest]))
    print(thePlot)
    
    #thePlot$data$ind[whClosest]
    closestDf <- thePlot$data[whClosest,]
    if (closestDf$lon < 0) closestDf$lon <- closestDf$lon + 360
    format(closestDf, digits=21)
    
  }
  
  list(linkDf=linkDf, getPt=getChanPointInner)
  getChanPointInner 
}
