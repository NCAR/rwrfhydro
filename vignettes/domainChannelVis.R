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
#' 
#' # Setup
#' Load the rwrfhydro package. 
## ---- results='hide'-----------------------------------------------------
library("rwrfhydro")
library(rgdal) ## on some linux machines this appears needed

#' 
#' Set the path to the directory of WRF Hydro test cases.
## ------------------------------------------------------------------------
fcPath <- '~/wrfHydroTestCases/Fourmile_Creek_testcase_v2.0'

#' 
## ---- echo=FALSE---------------------------------------------------------
options(width = 120, warn=1)

#' 
#' ##Visualize Gridded Domain Data: `VisualizeDomain()`
#' 
#' When working with a new model domain, it can be highly informative to explore the physical or geospatial nature of the model through visualization. The WRF-Hydro model has gridded geospatial information for both the LSM and hydrologic components, which are potentially at different spatial resolutions. The hydrologic components may also employ a separate file containing vector information, which we do not discuss here.
#' 
#' This section demonstrates a basic functionality for visualizing the gridded geospatial information in R. These functions suitable for medium sized domains. Proper GIS software is better suited for visualizing large domains and more intensive spatial representations of model components. 
#' 
#' The same function `VisualizeDomain` helps visualize both LSM and hydro components. It plots the information over map data which can be retrieved from a variety of sources, with the help of the `ggmap` package.
#' 
#' To properly display the spatial information, R currently requires the use of the `rgdal` and `sp` packages though these are not required for general installation and use of rwrfhydro. We hope to relax this assumption in the near future. An underlying function `GetDomainCoordsProj` does the work of obtaining and transforming the projection and/or datum of the spatial data to render it a lat-lon WGS84 map. 
#' 
#' ### Land surface model pixels
#' We begin by taking a look at the spatial information file for the LSM. First we specify the path to the 'Fourmile_Creek' test case.
#' We pick the "geo" file, which holds the geospatial information for the LSM. The following commands plot the topographic height for each cell over a map. 
## ------------------------------------------------------------------------
geoFile <- paste0(fcPath,'/run.ChannelRouting/DOMAIN/geo_em_d01.Fourmile1km.nlcd11.nc')
coordsProj <- GetDomainCoordsProj(geoFile)
ClosureGeo <- VisualizeDomain(geoFile, plotVar='HGT_M', plot=FALSE, plotDf=coordsProj)

#' The step of getting coordsProj could be done internally to VisualizeDomain but in some cases (e.g. plotting many variables for a big domain) can speed up the visualization.
#' 
#' For illustration purposes, we  set plot=FALSE in the call to VisualizeDomain to skip the default plot output. Typically this first, default plot serves as a basis for tweaking the options to the returned function, named ClosureGeo in this example, which help achieve the desired graphic representation.
#' 
#' Note carefully that VisualizeDomain returns a function. A returned function is termed a closure: a function with data inside it. (Because our style convention is first-letter capitals for functions, ClosureGeo begins with a capital letter.) The returned function can also return objects (perhpas another closure even). The arguments to this closure allow most of the main plot attributes to be tweaked when called. Because the graphics in this function are handled using the ggplot2 package, which has a signifcant learning curve, we design these closures to give the user the 90% solution without th need to learn ggplot2. The return values of the closures give the data and the ggplot objects which can befurther manipulated as necessary, which can be useful if you do know ggplot2.
#' 
#' Let's examine the structure and the arguments of ClosureGeo and it's return value, so we know what we have to work with. 
#' 
## ---- fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
class(ClosureGeo)
args(ClosureGeo)
closureReturnGeo <- ClosureGeo(plot=FALSE)
class(closureReturnGeo)
names(closureReturnGeo)

#' 
#' We see `ClosureGeo` is a function and all its arguments. The `closureRreturnGeo` object is a list (the output of `str` was much too long to put here) and it has two top-level objects. Note below, we can reference some of these objects in calls to `ClosureGeo`. In calling `ClosureGeo` to tweak the plot, zoom and pointsize depend on our domain and on the size of the graphics device. These were adjusted to get the output in the vignette and may not match well to own graphics device.
#' 
## ---- fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
mapMargin <- .05*c(-1,1)
closureRtnGeo <-
  ClosureGeo(zoom=11, pointsize=9, 
             grad=topo.colors(15), alpha=.6, maptype='terrain',
             subsetRange=range(plotDf$value),
             xlim=range(plotDf$long)+mapMargin*1.5,
             ylim = range(plotDf$lat)+mapMargin)

#' We could work directly with the data and the plot object from `closureRtnGeo` instead of using `ClosureReturn`. The returnComponents argument to the closure function gives even more fine grained output. We'll illustrate working with the return object below.
#' 
#' ### Routing pixels
#' A similar set of function calls allows visualization of the hydrologic grids. We look at the gridded channel network file. This time we skip the call to GetDomainCoordsProj. 
## ---- fig.width = 12, fig.height = 10.29, out.width='700', out.height='600'----
hydroFile <- paste0(fcPath,'/run.ChannelRouting/DOMAIN/Fulldom_hires_hydrofile.Fourmile100m.nc')
ClosureHydro <- VisualizeDomain(hydroFile, plotVar='CHANNELGRID', plot=FALSE)
closureRtnHydro <-
  ClosureHydro(zoom=11, pointsize=1,
               ## can reference the internal plotDf (or other variables internal to the closure)
               location=c(lon=mean(plotDf$long), lat=mean(plotDf$lat)), alpha=.2,
               grad=c('white','blue'), maptype='terrain')

#' 
#' We highlight use of several of the available arguments to the closure. To see the full set of arguments to the closure, we could use the `args` function or the print (default) method on the closure by "calling" the function without its paraentheses. Printing the closure will return the entire code and the arguments are shown at the top with their default values (e.g. argument=default). Reading the coode will reveal two variables which actually come from the VisualizeDomain function: bbox and plotDf. (Because the returned function encloses this data, it is refered to as a closure). Several of the arguments to the closure can reference these variables: location, xlim, ylim, subsetRange.
#' 
#' While the above command shows all the hydrologic grid pixels, many of them are missing values (-9999) because they are not on the gridded channel network. We use the subsetRange keyword to limit the range of values represented in the plot. We again use the xlim and ylim keywords to zoom in. We also limit the color scale to blue. 
#' 
## ---- fig.width=12, fig.height=8.57, out.width='700', out.height='500'----
closureRtnHydro <-
  ClosureHydro(zoom=14, pointsize=1,
               ## can reference the internal plotDf (or other variables internal to the closure)
               location=c(lon=mean(plotDf$long), lat=mean(plotDf$lat)),
               subsetRange=c(0),
               grad='blue', maptype='terrain',
               xlim=mean(plotDf$long)+mapMargin/2,
               ylim =mean(plotDf$lat)+mapMargin/2/1.5)

#' 
#' To illustrate use of the return values of the above closures, we show how to combine the two plots.
#' 
## ---- fig.width = 12, fig.height = 6.51, out.width='700', out.height='380'----
names(closureRtnGeo)
names(closureRtnHydro)
closureRtnGeo$ggObj +
  geom_point(data=closureRtnHydro$plotDf,
             aes(x=long, y=lat), size=.5, shape=21, fill='white', color='darkblue') +
               ggtitle('Fourmile Creek, CO - Elevation and Stream Channel')

#' The returnComponents argument to the closures will return more fine-grained objects from the plot in case those are useful. 
#' 
#' #`VisualizeChanNtwk()`
#' Let's use look at simulated flows to also see that the flow at the basin outlet is zero. Setup the path to a "CHRTOUT" data file.
## ------------------------------------------------------------------------
chrtFile <- paste0(fcPath,'/run.FullRouting/201306010000.CHRTOUT_DOMAIN1')

#' 
#' This is the basic function which shows the flow on the network
## ---- fig.width = 12, fig.height = 10.29/3, out.width='700', out.height='200'----
LocLinkFun<- VisualizeChanNtwk(chrtFile)

#'   
#' As with `VisualizeDomain`, `VisualizeChanNtwk` returns a closure. You can look at the closure function arguments with
## ------------------------------------------------------------------------
args(LocLinkFun)

#' 
#' A main issue is knowing/finding the index of a given point.  The click option to the closure lets you click and get the index. Clicking at the outlet point shows that it has (q=) 0 flow and its index is 350. (We can't show the interaction in a static document, so you have to try it yourself.) 
## ---- eval=FALSE---------------------------------------------------------
## LocLinkFun(click=TRUE)
## ## Please click on plot to select nearest point to your click...
## ## Selected point (in cyan on plot) data:
## ##  ind            lon           lat q
## ##  350 -105.325675964 40.0182723999 0

#'  
#' We'll like to exclude that point since there is no valid flow at the most downstream pixel in gridded routing. Using the original function, any set of valid indices can be excluded from the data. After excluding the lowest point and getting a new LocLinkFun, clicking on the lowest point reveals that that index is 1.
## ---- eval=FALSE---------------------------------------------------------
## LocLinkFun<-VisualizeChanNtwk(chrtFile, exclude=350)
## LocLinkFun(click=TRUE)
## ##Please click on plot to select nearest point to your click...
## ##Selected point (in cyan on plot) data:
## ## ind            lon           lat             q
## ##   1 -105.327774048 40.0193214417 1.30229723454

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

