#' ---
#' title: "WRF Hydro Domain and Channel Visualization."
#' author: "James McCreight"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{WRF Hydro Domain and Channel Visualization}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   \usepackage[utf8]{inputenc}
#' ---
#' 
#' # Background
#' When setting up a domain and forecast points, it can be useful to quickly visulaize and interact with domain and gage information. This vignette illustrates basic functionality for visualizing the WRF Hydro domain and channel files and potential forecast points. 
#' # Setup
#' Load the rwrfhydro package. 
## ---- results='hide'-----------------------------------------------------
library("rwrfhydro")

#' 
## ---- echo=FALSE---------------------------------------------------------
options(width = 120, warn=1)

#' 
#' # `VisualizeDomain()`
#' This is the path to the directory of WRF Hydro test cases that we recommend, but you may have to configure this to your machine. 
## ------------------------------------------------------------------------
 tcPath <- '~/wrfHydroTestCases/'

#' 
#' Setup: The specific test case and files should be the same once the above is set. 
## ------------------------------------------------------------------------
 fcPath <- paste0(tcPath,'Fourmile_Creek/')
 hydroFile<-paste0(fcPath,'/DOMAIN/Fulldom_hydro_OrodellBasin_100m.nc')

#'  
#' The following function plots the selected variable in the hydro file. 
## ---- restults='hold', fig.width = 12, fig.height = 10.29, out.width='700', out.height='600'----
 GgMapFunction <- VisualizeDomain(hydroFile, "CHANNELGRID")

#' Note that I know of no reliable function to translate bounding box to a google zoom level, so the default zoom may be very poor for other domains and will need to be set manually.... which we are about to describe how to do.
#' 
#' Notice that `VisualizeDomain()` returns a function. This is function is a closure, it is a function with data "inside" it (it's own environment). The idea here is to stash the plot data inside the returned function while letting arguments to the function control aspects of the plot. (Below we also illustrate that the return value from the returned function/closure is actually a ggplot object that can be manipulated outside the closure for finer-grained control of plotting. The data can also be removed from the ggplot object as well.) Now we change the map type, the zoom, and the point  attributes with arguments passed to the function. 
## ---- results='hold', fig.width = 12, fig.height = 10.29, out.width='700', out.height='600'----
  ggMap1 <- GgMapFunction(zoom=11, pointshape=15, pointsize=7, 
                         source="google", maptype="terrain")

#' For more information on the function returned by VisualizeDomain, see `?VisualizeDomain` or to just list its arguments and their default values `args(GgMapFunction)`.
#' 
#' Now add a streamflow gauge points to the domain. These points could be taken from frxst_points.txt or anywhere. Here we compare reality of gage location with that used in the model. This will illustrate an important point about WRF Hydro basins.
## ---- results='hold', fig.width = 12, fig.height = 10.29/2, out.width='700', out.height='300'----
orodellLonLat <- data.frame(lon=c(254.6722259521484375, 254.67374999999998408)-360, 
                            lat=c(40.019321441650390625, 40.018666670000001773),
                            gauge=c('model','USGS'))
ggMap2 <- GgMapFunction(location=c(lon=orodellLonLat$lon[1], lat=orodellLonLat$lat[1]),
                         zoom=14, pointshape=15, pointsize=7, 
                         source="google", maptype="terrain", plot=TRUE) 

#' Currently, the default plotting in `VisualizeDomain` always plots the whole domain. (You are free to improve this and do github pull requests on your work!) However, we can zoom in by using the the returned information from `ggMapFunction`. What is actually in `ggMap2` above? (Avoiding the full output of  `str()`.)
## ------------------------------------------------------------------------
class(ggMap2)

#' 
#' It's a `ggplot` object that one can use directly with `ggplot` (instead of through `GgMapFunction`.) A little of customization with `ggplot` functions is used to limit the part of the domain plotted.
#' 
## ---- results='hold', fig.width = 12, fig.height = 10.29, out.width='700', out.height='600'----
ggMap2 + geom_point(data=orodellLonLat, aes(x=lon,y=lat, shape=gauge)) +
          scale_x_continuous(limits=rev(orodellLonLat$lon+c( .01,-.01))) +
          scale_y_continuous(limits=rev(orodellLonLat$lat+c( .005,-.005))) 

#' 
#' Now we are zoomed in to the basin outlet. Note that the model point is above the gage and is not the last point in the basin. The gridded channel routing does not supply a flow into the last point in the domain, so this point cannot be used. 
#' 
#' #`VisualizeChanNtwk()`
#' Let's use look at simulated flows to also see that the flow at the basin outlet is zero. Setup the path to a "CHRTOUT" data file.
## ------------------------------------------------------------------------
 chrtFile <- paste0(fcPath,'/RUN.RTTESTS/OUTPUT_CHRT_DAILY/201308010000.CHRTOUT_DOMAIN1')

#' 
#' This is the basic function which shows the flow on the network
## ---- fig.width = 12, fig.height = 10.29/3, out.width='700', out.height='200'----
LocLinkFun<- VisualizeChanNtwk(chrtFile)

#'   
#' As with `VisualizeDomain`, `VisualizeChanNtwk` returns a closure. You can look at the closure function arguments with
## ------------------------------------------------------------------------
args(LocLinkFun)

#' 
#' A main issue is knowing/finding the index of a given point.  The click option to the closure lets you click and get the inded. Clicking at the outlet point to see that it has (q=) 0 flow and its index is 350. (We can't show the interaction in a static document, so you have to try it yourself.)
## ---- eval=FALSE---------------------------------------------------------
## LocLinkFun(click=TRUE)
## ## Please click on plot to select nearest point to your click...
## ## Selected point (in cyan on plot) data:
## ##  ind            lon           lat q
## ##  350 -105.325675964 40.0182723999 0

#'  
#' Using the original function, any set of valid indices can be excluded from the data. After excluding the lowest point and getting a new LocLinkFun, clicking on the lowest point reveals that that index is 1.
## ---- eval=FALSE---------------------------------------------------------
## LocLinkFun<-VisualizeChanNtwk(chrtFile, exclude=350)
## LocLinkFun(click=TRUE)
## ## Please click on plot to select nearest point to your click...
## ## Selected point (in cyan on plot) data:
## ## ind            lon           lat               q
## ##   1 -105.327774048 40.0193214417 0.0453463643789

#' 
#' We want to add the real-life gauge points and find the nearest stream channel grid cells. You can supply gage points in the following format to the `VisualizeChanNtwk` function and the nearest neighbors on the channel network are returned. 
## ------------------------------------------------------------------------
gaugePts <-
   list(orodell   =data.frame(lon=254.67374999999998408,
                              lat=40.018666670000001773),
        loganMill =data.frame(lon=254.63508330000001934,
                              lat=40.042027779999997961),
        sunshine  =data.frame(lon=254.65122220000000652,
                              lat=40.05761110000000258)  )
LocLinkFun <- VisualizeChanNtwk(chrtFile, gaugePts=gaugePts, exc=350, plot=FALSE)

#' 
#' Increase the accuracy of the lon/lat ouput and make the plot that was suppressed in the previous call. 
## ---- fig.width = 12, fig.height = 10.29/2, out.width='700', out.height='300'----
LocLinkFun <- VisualizeChanNtwk(chrtFile, gaugePts=gaugePts, exc=350, plot=FALSE, gaugeAccuracy=17)
LocLinkFun()

#' 
#' Change the amount of padding around the domain and the shape of the gauge symbols.
## ---- fig.width = 12, fig.height = 10.29/2, out.width='700', out.height='300'----
 LocLinkFun(pad=.3, gaugeShape=16)

#' 
#' Zoom to the orodell gauge. 
## ---- fig.width = 12, fig.height = 10.29*2/3, out.width='700', out.height='400'----
LocLinkFun(zoom=14, gaugeShape=16, gaugeZoom='orodell', pad=15)

#' 
#' Zoom to the Logan Mill gauge
## ---- fig.width = 12, fig.height = 10.29, out.width='700', out.height='600'----
 LocLinkFun(zoom=15, gaugeShape=16, gaugeZoom='loganMill', pad=15)

#' 
#' You can also click in the zoomed view. 
## ---- eval=FALSE---------------------------------------------------------
##  LocLinkFun(zoom=15, gaugeShape=16, gaugeZoom='loganMill', pad=15, click=TRUE)

#' 
