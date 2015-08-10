#' file <- '~/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/Route_Link.nc'
#' doMC::registerDoMC(3)
#' PlotRouteLink <- VisualizeRouteLink(file, parallel=TRUE)
#' thePlot <- PlotRouteLink()
VisualizeRouteLink <- function(file, parallel=FALSE) {
  
  if(!length(grep(tolower('RouteLink|Route_Link'), tolower(file))))
      warning('Input files generally have names like "RouteLink" or "Route_Link')
       
  if(!length(grep(tolower('\\.nc'), tolower(file))))
    warning('Only netcdf files are processed by this function.')
  
  rl <- as.data.frame(GetNcdfFile(file, quiet=TRUE))
  
  FindLat <- function(to) if(length(ret <- rl$lat[which(rl$link==to)])) ret else NA
  FindLon <- function(to) if(length(ret <- rl$lon[which(rl$link==to)])) ret else NA
  
  rl$to_lat <- plyr::laply(rl$to, FindLat, .parallel=parallel)
  rl$to_lon <- plyr::laply(rl$to, FindLon, .parallel=parallel)
  
  rl$to_lat[which(is.na(rl$to_lat))] <- rl$lat[which(is.na(rl$to_lat))]
  rl$to_lon[which(is.na(rl$to_lon))] <- rl$lon[which(is.na(rl$to_lon))]
  
  PlotRouteLink <- function(location=c(lon=mean(range(rl$lon)),
                                       lat=mean(range(rl$lat)) ),
                            zoom=8, source='google', maptype='terrain',
                            padPlot=.01, linkColor='blue', textColor='orange', 
                            gageZoom=NULL) {

    rl <- rl
    nGage <- length(gageZoom)
    
    for(gg in 0:nGage) {
      
      if(length(gageZoom)) {
        if(gageZoom %in% gagePtsBothDf$location) {
          subgageDf <- subset(gagePtsBothDf, location==gageZoom)
          plotLimX <- PadRange(range(subgageDf$lon),diff=padPlot)
          plotLimY <- PadRange(range(subgageDf$lat),diff=padPlot)
          location <- c(lon=mean(range(subgageDf$lon)),
                        lat=mean(range(subgageDf$lat)) )
        }
      } else {
        plotLimY <- range(rl$lat)
        plotLimX <- range(rl$lon)
        #plotLimX <- PadRange(range(rl$lon),diff=padPlot)
        #plotLimY <- PadRange(range(rl$lat),diff=padPlot)
      }
      
      theMap <- ggmap::get_map(location, zoom = zoom, source = source, maptype=maptype)
      
      ggObj <- 
        ggmap::ggmap(theMap, extent='normal') + 
        ggplot2::geom_segment(data=rl, 
                              ggplot2::aes(x=lon, y=lat, xend=to_lon, yend=to_lat), 
                              color=linkColor) +
        ggplot2::theme_bw() #+
        ## the following 2 lines are CRITICAL to the click
        #ggplot2::scale_x_continuous(expand=c(0,0), limits=plotLimX) +
        #ggplot2::scale_y_continuous(expand=c(0,0), limits=plotLimY) 
      str(rl)
      print(ggObj)
      
      if(nGage > 1) ggObj <- 'Only returns the ggObj when number of gages <= 1.'
    } # for 0:nGage
    invisible(list(rl=rl, ggObj=ggObj))
  } # function  
  
  invisible(PlotRouteLink)  ## return the function
}    
    
