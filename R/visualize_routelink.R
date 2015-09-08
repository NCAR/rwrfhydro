#' Visualize RouteLink.nc files. 
#' 
#' @param file Character path/file to the desired Route_Link.nc netcdf file. 
#' @param parallel Logical, use a registerred parallel backend for plyr?
#' @examples
#' \dontrun{
#' library(rwrfhydro)
#' file <- '~/WRF_Hydro/DOMAIN_library/FRNG_1km_100m_1sqkm_routing_NHD_2015_08_10/Route_Link.nc'
#' doMC::registerDoMC(3)
#' PlotRouteLink <- VisualizeRouteLink(file, parallel=TRUE)
#' thePlot <- PlotRouteLink()
#' gagePoints <- data.frame(
#'    lat=c(40.04202778000000, 40.05761110000000, 40.01866667000000, 40.05165184000000),
#'    lon=c(-105.36491669999999,-105.34877779999999,-105.32625000000000,-105.17887540000000), 
#'    name=c('FOURMILE CREEK AT LOGAN MILL ROAD NEAR CRISMAN',
#'           'FOURMILE CANYON CREEK NEAR SUNSHINE',
#'           'FOURMILE CREEK AT ORODELL', 
#'           'BOULDER CREEK AT NORTH 75TH ST. NEAR BOULDER'),
#'    gageId=c(''))
#' thePlot <- PlotRouteLink(gageZoom=gagePoints)
#' thePlot <- PlotRouteLink(gageZoom=gagePoints[4,], pad=.03, zoom=11)
#' 
#' Second example, get all the gages in the domain:
#' library(rwrfhydro)
#' doMC::registerDoMC(3)
#' file <- '~/WRF_Hydro/DOMAIN_library/FRNG_1km_100m_1sqkm_routing_NHD_2015_08_10/Route_Link.nc'
#' # Find the proper radius in which to search for gages
#' rl <- GetNcdfFile(file)
#' lonRange <- range(rl$lon)
#' latRange <- range(rl$lat)
#' # These are identical by virtue of the mean
#' # (lonRange[2]-mean(lonRange))^2 + (latRange[2]-mean(latRange))^2
#' rad <- sqrt((lonRange[1]-mean(lonRange))^2 + (latRange[1]-mean(latRange))^2)*1.1
#' stnsInRad <- FindUsgsStns(stnLon=mean(lonRange), stnLat=mean(latRange), within=rad)
#' stnsInDom <- subset(stnsInRad, dec_lat_va >= latRange[1] &
#'                                dec_lat_va <= latRange[2] &
#'                                dec_long_va >= lonRange[1] &
#'                                dec_long_va <= lonRange[2] )
#' gagePoints2 <- stnsInDom[,c(7,8,10,11)]
#' renamer <- c("gageId",'name','lat','lon')
#' names(renamer) <- names(stnsInDom)[c(7,8,10,11)]
#' gagePoints2 <- plyr::rename(gagePoints2, renamer)
#' PlotRouteLink <- VisualizeRouteLink(file, parallel=TRUE)
#' thePlot <- PlotRouteLink(gageZoom=gagePoints2[17,], pad=.02, zoom=12)
#' thePlot <- PlotRouteLink(gageZoom=gagePoints2, pad=.02, zoom=12, 
#' plotPath='~/WRF_Hydro/DOMAIN_library/FRNG_1km_100m_1sqkm_routing_NHD_2015_08_10/gageIdPlots/')
#' thePlot <- PlotRouteLink(gageZoom=gagePoints2, pad=.02, zoom=12)
#' 
#' # just show the boulder creek domain
#' file <- '~/WRF_Hydro/DOMAIN_library/BoCr_100m_1km_NHDPlus_2015_08_11/Route_Link.nc'
#' PlotRouteLink <- VisualizeRouteLink(file, parallel=TRUE)
#' foo <- PlotRouteLink()
#' load('~/WRF_Hydro/DOMAIN_library/BoCr_100m_1km_NHDPlus_2015_08_11/Route_Link.reInd.Rdb')
#' foo$rl$link <- 1:nrow(reInd)
#' foo$ggObj + ggplot2::geom_text(data=foo$rl, 
#'                                ggplot2::aes(x=lon/2+to_lon/2, 
#'                                             y=lat/2+to_lat/2, 
#'                                label=as.character(link)), 
#'                                color='darkred')
#' }
#' @keywords manip vis
#' @concept dataMgmt dataVis
#' @family networkExpression nudging
#' @export
VisualizeRouteLink <- function(file='Route_Link.nc', parallel=FALSE) {
  
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
                            indices=FALSE,
                            comIds=FALSE,
                            zoom=if(length(gageZoom)) 13 else 8,
                            source='google', 
                            maptype='terrain',
                            padPlot=.01, 
                            linkColor='blue', 
                            textColor='darkred', 
                            gageColor='cyan',
                            gageShape=9,
                            gageZoom=NULL,
                            plotPath=NULL, 
                            plotType='pdf', 
                            doPlot=TRUE) {

    nGage <- if(length(gageZoom)) nrow(gageZoom)-1 else 0
    rl$ind <- 1:nrow(rl)
    rlSub <- rl
    
    for(gg in 0:nGage) {
      
      if(length(gageZoom)) {
        subGageZoom <- gageZoom[gg+1,]
        plotLimX <- PadRange(range(subGageZoom$lon),delta=padPlot*5)
        plotLimY <- PadRange(range(subGageZoom$lat),delta=padPlot*5)
        location <- c(lon=mean(plotLimX),lat=mean(plotLimY) )
        rlSub <- subset(rl, ( lon > plotLimX[1] & 
                              lon < plotLimX[2] & 
                              lat > plotLimY[1] & 
                              lat < plotLimY[2] )     |
                            ( to_lon > plotLimX[1] & 
                              to_lon < plotLimX[2] & 
                              to_lat > plotLimY[1] & 
                              to_lat < plotLimY[2] )    )
        if(nrow(rlSub)==0) next
      } else {
        plotLimX <- PadRange(range(rl$lon),diff=padPlot)
        plotLimY <- PadRange(range(rl$lat),diff=padPlot)
      }
      
      theMap <- ggmap::get_map(location, zoom = zoom, source = source, maptype=maptype)
      
      ggObj <- 
        ggmap::ggmap(theMap, extent='normal') + 
        ggplot2::geom_segment(data=rlSub, 
                              ggplot2::aes(x=lon, y=lat, xend=to_lon, yend=to_lat), 
                              color=linkColor) +
        ggplot2::theme_bw() +
        ggplot2::coord_cartesian(xlim = plotLimX, ylim = plotLimY)
        ## the following 2 lines are CRITICAL to the click
        #ggplot2::scale_x_continuous(expand=c(0,0), limits=plotLimX) +
        #ggplot2::scale_y_continuous(expand=c(0,0), limits=plotLimY) 
      
      if(length(gageZoom)) {
        ggObj <- ggObj + 
          ggplot2::geom_point(data=subGageZoom, ggplot2::aes(x=lon, y=lat), 
                              color=gageColor, size=7, shape=gageShape)
        if(indices) {
          ggObj <- ggObj +
            ggplot2::geom_text(data=rlSub, 
                               ggplot2::aes(x=lon/2+to_lon/2, 
                                            y=lat/2+to_lat/2, 
                                            label=as.character(ind)), 
                               color=textColor) + 
            ggplot2::ggtitle(paste0('Indices: ', subGageZoom$gageId, ' - ', subGageZoom$name))
        } else {
          ggObj <- ggObj +
            ggplot2::geom_text(data=rlSub, 
                               ggplot2::aes(x=lon/2+to_lon/2, 
                                            y=lat/2+to_lat/2, 
                                            label=as.character(link)), 
                               color=textColor) + 
            ggplot2::ggtitle(paste0('ComIds: ', subGageZoom$gageId, ' - ', subGageZoom$name))
        }
      } else {
        if(indices) {
          ggObj <- ggObj +
            ggplot2::geom_text(data=rlSub, 
                               ggplot2::aes(x=lon/2+to_lon/2, 
                                            y=lat/2+to_lat/2, 
                                            label=as.character(ind)), 
                               color=textColor) + 
            ggplot2::ggtitle(paste0('Indices'))
        } else {
          if(comIds){
            ggObj <- ggObj +
              ggplot2::geom_text(data=rlSub, 
                                 ggplot2::aes(x=lon/2+to_lon/2, 
                                              y=lat/2+to_lat/2, 
                                              label=as.character(link)), 
                                 color=textColor) + 
              ggplot2::ggtitle(paste0('ComIds'))
          }
        }
      }
      
      if(!is.null(plotPath)) { 
        print(plotFileName <- paste0(plotPath,
                                     subGageZoom$gageId,'_',
                                     gsub(',|\\.','',gsub(' ', '_', subGageZoom$name)),'.', 
                                     plotType))
        ggObj <- ggObj + ggplot2::ggsave(plotFileName)
      } else {
        if(doPlot) print(ggObj)
      }
        
      if(nGage > 0 & gg != nGage & is.null(plotPath)) readline("Return to continue...")
      
    } # for 0:nGage
    if(nGage > 0) ggObj <- 'Only returns the ggObj when number of gages <= 1.'
    invisible(list(rl=rl, ggObj=ggObj))
  } # function  
  
  invisible(PlotRouteLink)  ## return the function
}    
