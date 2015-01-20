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
#' 
#' GetChanPt <- VisualizeChanNtwk(fileCh)
#' GetChanPt()
#'
#' gaugePts <-
#'  list(orodell   =data.frame(lon=254.67374999999998408,
#'                             lat=40.018666670000001773),
#'       loganMill =data.frame(lon=254.63508330000001934,
#'                             lat=40.042027779999997961),
#'       sunshine  =data.frame(lon=254.65122220000000652,
#'                             lat=40.05761110000000258)  )
#'
#' locLinkFun<-VisualizeChanNtwk(fileCh, gaugePts=gaugePts)
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
  linkDf$lon <- StdLon(linkDf$lon)

  ## find nearest neighbors if gaugePts was defined.
  if(length(gaugePts)) {
    ## This is better way of handling 
    gaugePtsDf <- plyr::ldply(gaugePts, .id='location')
    
    ## standardize the lon to +-180
    gaugePtsDf <- 
      plyr::ddply(gaugePtsDf, plyr::.(location, lon, lat), 
                  plyr::summarize, 
                  lon=StdLon(lon))

    ## the euclidean metric in lat/lon works fine.
    FindNn <- function(dd) {
      whMin <- which.min(sqrt( (dd$lon-linkDf$lon)^2 + (dd$lat-linkDf$lat)^2 ))
      dd$chanInd <- whMin
      dd$lon <- linkDf$lon[whMin]
      dd$lat <- linkDf$lat[whMin]
      dd$modelFile <- file
      dd$system <- 'model'
      dd
    }
    gaugePtsModelDf <- plyr::ddply(gaugePtsDf, plyr::.(location), FindNn)
  
    ## combine real world and modeled gauges
    gaugePtsDf$chanInd <- gaugePtsDf$modelFile <- NA
    gaugePtsDf$system  <- 'gauge'
    gaugePtsBothDf <- rbind(gaugePtsDf, gaugePtsModelDf)
    
    ## print out the gauge information?
    PrintGauge <- function(dd) {
      cat(paste0('** ',dd$location[1],' **********'),sep='\n')
      print(dd[c('system','chanInd','lon','lat')], row.names=FALSE)
      NULL
    }
    invisible(plyr::ddply(gaugePtsBothDf, plyr::.(location), PrintGauge)) 
  
  }


  ## This function is going to be returned as a closure.
  ## It's env is visChanNtwkm which includes linkDf, and maybe the reference to the
  ## viewport?
  GetChanPoint <- function(location=c(lon=mean(lon),lat=mean(lat)),
                           zoom=11, source='google', maptype='hybrid',
                           gaugeZoom=NULL) {
## zoom to gauge.
    
    theMap <- ggmap::get_map(location, zoom = zoom, source = source, maptype=maptype)
    library(ggplot2)  ## not called except in the closure 
    thePlot <-
      ggmap::ggmap(theMap, extent='normal') +
      #ggplot2::ggplot()+
      ggplot2::geom_point( data=linkDf, ggplot2::aes(x=lon, y=lat) )
 
    if(length(gaugePtsBothDf)) {
      thePlot <- 
        thePlot +
        ggplot2::geom_point(data=gaugePtsBothDf,
                            ggplot2::aes(x=lon, y=lat, shape=system, color=location)) +
        ggplot2::scale_shape_manual(values=c(4,16))
    }
    print(thePlot)
    
    gridNames <- grid::grid.ls(print=FALSE)[['name']]
    x <- gridNames[grep("panel.[1-9]-", gridNames)] #locate the panel
    grid::seekViewport(x)
    clickPt <-  grid::grid.locator("npc")
    clickPt <- as.numeric(substring(clickPt, 1, nchar(clickPt)-3))
    
    mapMinMax <- attributes(theMap)$bb
    locX <- as.numeric(mapMinMax['ll.lon'] + 
              clickPt[1] * diff(as.numeric(mapMinMax[c('ll.lon','ur.lon')])))
    locY <- as.numeric(mapMinMax['ll.lat'] + 
              clickPt[2] * diff(as.numeric(mapMinMax[c('ll.lat','ur.lat')])))
    
    whClosest <- which.min( sqrt((lon-as.numeric(locX))^2 + (lat-locY)^2))
    
    thePlot2 <- 
      thePlot +
      ggplot2::geom_point(data=data.frame(lon=lon[whClosest],lat=lat[whClosest]),
                          ggplot2::aes(x=lon,y=lat), color='cyan') 
      thePlot2
    
      ggplot2::ggplot( linkDf, ggplot2::aes(x=lon, y=lat, fill=ind) ) +
      ggplot2::geom_raster() +
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
