##' Visualize WRF Hydro domain spatial data. 
##'
##' \code{VisualizeSpatial} creates basic plots of spatial data covering WRF-Hydro domain.
##' 
##' Crude plots of the WRF Hydro spatial data. The routine accepts a grid of spatial data for either the geo 
##' (coarse resolution) or hydro (fine resolution) files. The return is a function 
##' (a closure which encapuslates the domain data) which 
##' creates a plot when called. The arguments to the function can be changed to tailor the plot 
##' (arguments are passed to ggmap and ggplot inside the function). This function (the closure) returns
##' a ggplot object whose data can be accessed.
##' 
##' @param input A 2D array of data matching domain file.
##' @param varName Character string specifying name of variable being plotted. Optional.
##' @param plot Logical: plot or not?
##' @param plotDf An optional data frame with the data from file already reprojected.
##' 
##' @return A function which can be called to plot the data and allow adjustment of its arguments, the plotting parameters.
##' @examples 
##' \dontrun{
##' geoFile <- '/home/karsten/geo_em.d02.nc'
##' coordsProj <- GetDomainCoordsProj(geoFile)
##' ClosureGeo <- VisualizeSpatial(data[,,],varName='Temperature',plot=FALSE,plotDf=coordsProj)
##' closureMap <- 
##'   ClosureGeo(zoom=11,pointsize=9,
##'              gradNColors=rainbow(25),alpha=.6,maptype='terrain',
##'              subsetRange=range(plotDf$value),
##'              xlim=range(plotDf$long)+mapMargin*1.5,
##'              ylim=range(plotDf$lat)+mapMargin)
##' }
##' @concept plot
##' @keywords hplot
##' @family domain
##' @export
VisualizeSpatial <- function(input, varName='Variable',plot=TRUE, plotDf=NULL) {
  
  ## get the file coordinates
  if(is.null(plotDf)) plotDf <- GetDomainCoordsProj(file)
  bbox <- plotDf@bbox
  plotDf <- as.data.frame(plotDf)
  plotDf$value <- as.vector(input)
  
  # RETURN a closure (encapuslate the data) which allows the plot parameters to be tweaked.
  # The closure returns the ggMapObject - might be ways to merge or build these.
  outFunc <- 
    function(location=as.numeric(bbox),
             zoom=10,
             extent='panel',
             source='google',
             maptype='hybrid', 
             pointsize=3,
             pointshape=15,
             alpha=1,
             gradNColors=c('red','green','blue'),
             subsetRange=NULL,
             xlim=NULL,
             ylim=NULL,
             plot=TRUE,
             returnComponents=FALSE) {
      ## Calling library may be forbidden in the package, but this dosent actually get executed in 
      ## the package. It seems that ggmap:: should take care of this, but it dosent.
      library(ggplot2) ## called in the closure.

      ## handle nonstandard evals for location, xlim, ylim here

      ## try to evaluate the promise normally, then try locally if that fails
      ## (cant quite get this to work as a separate function...)
      outFuncEnv <- environment()
      if(class(locationEval <- try(location, silent=TRUE))=='try-error')
        locationEval <- eval(substitute(location), outFuncEnv)
      if(class(xlimEval <- try(eval(xlim), silent=TRUE))=='try-error')
        xlimEval <- eval(substitute(xlim), outFuncEnv)
      if(class(ylimEval <- try(eval(ylim), silent=TRUE))=='try-error')
        ylimEval <- eval(substitute(ylim), outFuncEnv)   
      if(class(subRngEval <- try(eval(subsetRange), silent=TRUE))=='try-error')
        subRngEval <- eval(substitute(subsetRange), outFuncEnv)   
      
      ggMapObj <- ggmap::get_map(locationEval, zoom = zoom, source = source, maptype=maptype)

      ## actually reduces the size of the returned data frame
      if(!is.null(subRngEval))
        plotDf <- subset(plotDf, value<=max(subRngEval) & value>=min(subRngEval)) 
      
      ggPlotObj <- ggplot2::geom_point(data=plotDf,
                                       aes(x=long, y=lat, color=value),
                                       size=pointsize, shape=pointshape, alpha=alpha)
      ggColorScaleObj <- ggplot2::scale_color_gradientn(name=varName, colours=gradNColors)
      ggCoordObj <- ggplot2::coord_map(xlim=xlimEval, ylim=ylimEval) 

      if(plot)
        print(ggObj <- ggmap::ggmap(ggMapObj) + ggPlotObj +
                      ggColorScaleObj + ggCoordObj + theme_bw(base_size=20))

      outList <- list(plotDf=plotDf, ggObj=ggObj)
      if(returnComponents)
        outList <- c(outList,
                     list(plotDf=plotDf, ggMapObj=ggMapObj, ggPlotObj=ggPlotObj,
                          ggColorScaleObj=ggColorScaleObj, ggCoordObj=ggCoordObj))
      invisible(outList)
    }
  
  if(plot) outFunc()
  
  invisible(outFunc)
}
