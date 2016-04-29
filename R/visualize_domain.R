## GetDomainCoordsProj
##' \code{GetDomainCoordsProj} collects and reprojects spatial data from geo and hydro files.
##' 
##' Handle the projection and datum iformation associated with WRF-Hydro geo and hydro grid files.
##'
##' @param file A path/name to a geo or hydro domain file.
##' @param proj4Str the desired output projection.
##' @return An sp::SpatialPoints object.
##' @examples
##' ## See the vignette "WRF Hydro Domain and Channel Visualization", for details. 
##' ## set your test case path
##' \dontrun{
##' hydroFile<- '~/WRF_Hydro/TESTING/TEST_FILES/FRNG/2015-12-04_20:08:06.b8e1e01c4cc2/STD/DOMAIN/Fulldom_hires_netcdf_file_2015_11_20.nc'
##' hydroCoords <- GetDomainCoordsProj(hydroFile)
##' str(hydroCoords)
##' hydroCoords <- as.data.frame(hydroCoords)
##' Calculate/Plot the error of the longitude and latitidue coords in the Fulldom file
##' hydroCoords$latFile <- as.vector(ncdump(hydroFile,'LATITUDE', q=TRUE))
##' hydroCoords$lonFile <- as.vector(ncdump(hydroFile,'LONGITUDE', q=TRUE))
##' hydroCoords$latErr <- lat-hydroCoords$lat
##' hydroCoords$lonErr <- lon-hydroCoords$long
##' summary(hydroCoords$latErr)
##' summary(hydroCoords$lonErr)
##' theMap <- ggmap::get_map(location=c(lon=mean(hydroCoords$long)-.5,
##'                                     lat=mean(hydroCoords$lat))+.2, zoom = 13)
##' ggmap::ggmap(theMap) + geom_point(data=hydroCoords, aes(x=lonFile, y=latFile),size=1)
##' ggmap::ggmap(theMap) + geom_point(data=hydroCoords, aes(x=long, y=lat),size=.5)
##' ## test for unique pairs in lat/long
##' testLon <- hydroCoords$long
##' testLat <- hydroCoords$lat
##' tally=list()
##' for(i in 1:300) {
##'   matches <- (testLon[(i+1):length(testLat)] == testLon[i] &
##'               testLat[(i+1):length(testLat)] == testLat[i]   )
##'   whMatch <- which(matches)+i  
##'   if(length(whMatch)) {
##'     for(j in 1:length(whMatch)) tally[[length(tally)+1]] <- c(i,whMatch)
##'     print(i)
##'     print(length(tally))
##'   }
##' }
##' } #dontrun
##' @concept plot
##' @keywords hplot
##' @family domain
##' @export
GetDomainCoordsProj <- function(file, proj4Str="+proj=longlat +datum=WGS84") {

  if(!all(c('rgdal','sp') %in% installed.packages()[,"Package"]))  {
    warning('Using GetDomainCoordsProj correctly requires rgdal & sp packages for now.')
  }

  IsGeoFile <- function(file, globalAttToTest='CEN_LAT') {
    ncid <- ncdf4::nc_open(file)
    retVal <- ncdf4::ncatt_get(ncid, varid = 0, attname=globalAttToTest)$hasatt
    ncdf4::nc_close(ncid)
    retVal
  }
  
  if(IsGeoFile(file)) {

    ## geo grid files: use the coordinates which are in wrf sphereoid
    ## geogrid long/lat
    coordDf <- data.frame(long=as.vector(ncdump(file,'XLONG_M', q=TRUE)))
    coordDf$lat <- as.vector(ncdump(file,'XLAT_M', q=TRUE))
    ## lat/lon wrfSpheroid Datum
    sp::coordinates(coordDf) = c("long", "lat") # promote to SpatialPointsDataFrame  
    wrfSphereProj4="+proj=longlat +a=6370000 +b=6370000 +no_defs"
    sp::proj4string(coordDf) <- sp::CRS(wrfSphereProj4)

  } else {

    ## hydro grid files: use the coordinates in lambert conformal projection.
    
    ## if you wanted to use the lambert conformal projection
    ## information from the associated geo grid file, this is what you'd do.
    ## but it's a bad idea to make that dependency, so stop it!
    ##  geoNcId <- ncdf4::nc_open(geoFile)
    ##  cen_lat <- ncdf4::ncatt_get(geoNcId, varid = 0, attname = "CEN_LAT")$value
    ##  cen_lon <- ncdf4::ncatt_get(geoNcId, varid = 0, attname = "STAND_LON")$value
    ##  truelat1 <- ncdf4::ncatt_get(geoNcId, varid = 0, attname = "TRUELAT1")$value
    ##  truelat2 <- ncdf4::ncatt_get(geoNcId, varid = 0, attname = "TRUELAT2")$value
    ##  ncdf4::nc_close(geoNcId)
    
    ## scrape out the projection info
    hydroNcId <- ncdf4::nc_open(file)

    ## find "esri_pe_string" - take it from the first variable found with it.
    ## this assumes it's the same for all variables in the file.
    hydroVarAtts <-
      plyr::llply(NamedList(names(hydroNcId$var)), function(vv) ncdf4::ncatt_get(hydroNcId, vv))
    esriVars <-
      which(unlist(plyr::llply(hydroVarAtts,
                   function(ll) if(any(names(ll)=='esri_pe_string')) TRUE else FALSE )))
    if(!length(esriVars))
      warning(paste0('No variable has required attributes "esri_pe_sting" in file: ',file),
              immediate.=TRUE)
    esriVar <- names(esriVars)[1]

    esriProjStr <- ncdf4::ncatt_get(hydroNcId, varid = esriVar,
                                    attname = "esri_pe_string")$value
    esriProjVec <- unlist(strsplit(gsub('\\]',',',gsub('\\[',',',esriProjStr)),'(,|")'))
    esriProjVec <- esriProjVec[esriProjVec!='']
    cen_lat <- as.numeric(esriProjVec[which(esriProjVec=='latitude_of_origin')+1])
    cen_lon <- as.numeric(esriProjVec[which(esriProjVec=='central_meridian')+1])
    truelat1 <- as.numeric(esriProjVec[which(esriProjVec=='standard_parallel_1')+1])
    truelat2 <- as.numeric(esriProjVec[which(esriProjVec=='standard_parallel_2')+1])  
    ncdf4::nc_close(hydroNcId)    
    
    ##cen_lat; cen_lon; truelat1; truelat2
    lambertProj4 <- paste0("+proj=lcc +lat_1=", truelat1, 
                           " +lat_2=", truelat2, " +lat_0=", cen_lat, " +lon_0=", 
                           cen_lon, " +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")

    ## fulldom/hydrogrid x and y in lambert conformal conic
    x <- as.vector(ncdump(file,'x', q=TRUE))
    y <- as.vector(ncdump(file,'y', q=TRUE))
    coordDf <- data.frame(long=rep(x,length(y)))
    coordDf$lat <- rep(y,each=length(x))
    ## convert to lambert conf conic
    sp::coordinates(coordDf) = c("long", "lat") # promote to SpatialPointsDataFrame  
    sp::proj4string(coordDf) <- sp::CRS(lambertProj4)

  }
  
  ## transform to lat lon WGS84
  sp::spTransform(coordDf, sp::CRS(proj4Str))
}




##' Visualize WRF Hydro domain files. 
##'
##' \code{VisualizeDomain} creates basic plots of WRF Hydro domains.
##' 
##' Crude plots of the WRF Hydro domain files. The routine accepts a file path/name for either the geo 
##' (coarse resolution) or hydro (fine resolution) files. Spatial variables of interest are listed
##' if none is provided. The return is a function (a closure which encapuslates the domain data) which 
##' creates a plot when called. The arguments to the function can be changed to tailor the plot 
##' (arguments are passed to ggmap and ggplot inside the function). This function (the closure) returns
##' a ggplot object whose data can be accessed.
##' 
##' @param file A path/name to a geo or hydro domain file.
##' @param plotVar An optional variable name of interest within the file.
##' @param plot Logical: plot or not?
##' @param plotDf An optional data frame with the data from file already reprojected.
##' 
##' @return A function which can be called to plot the data and allow adjustment of its arguments, the plotting parameters.
##' @examples
##' ## See the vignette "WRF Hydro Domain and Channel Visualization", for details. 
##' ## set your test case path
##' \dontrun{
##' hydroFile <- '/home/adugger/WRF_Hydro/Fourmile_fire/DOMAIN/hydro_OrodellBasin_100m_8msk.nc'
##'  GgMapFunction <- VisualizeDomain(hydroFile, "CHANNELGRID")
##'  ggMap1 <- GgMapFunction(zoom=11, pointshape=15, pointsize=7, 
##'                          source="google", maptype="terrain")
##'  # Add a streamflow gauge point; compare reality and the model.
##'  orodellLonLat <- data.frame(lon=c(254.6722259521484375, 254.67374999999998408)-360, 
##'                             lat=c(40.019321441650390625, 40.018666670000001773),
##'                             gauge=c('model','USGS'))
##'  ggMap2 <- GgMapFunction(location=c(lon=orodellLonLat$lon[1], lat=orodellLonLat$lat[1]),
##'                          zoom=14, pointshape=15, pointsize=7, 
##'                          source="google", maptype="terrain", plot=FALSE) 
##' ggMap2$ggMapObj +
##'   geom_point(data=orodellLonLat, aes(x=lon,y=lat, shape=gauge)) +
##'   scale_x_continuous(limits=rev(orodellLonLat$lon+c( .01 ,-.01 ))) +
##'   scale_y_continuous(limits=rev(orodellLonLat$lat+c( .005,-.005)))
##' 
##' ## fourmile redux
##' ## geogrid
##' geoFile <- '/home/adugger/WRF_Hydro/Fourmile_fire/DOMAIN/geo_OrodellBasin_1km_8.nc'
##' coordsProj <- GetDomainCoordsProj(geoFile)
##' ClosureGeo <- VisualizeDomain(geoFile, plotVar='HGT_M', plot=FALSE, plotDf=coordsProj)
##' mapMargin <- .01*c(-1,1)
##' closureRtnGeo <-
##'   ClosureGeo(zoom=11, pointsize=35,
##'              grad=topo.colors(15), alpha=.4, maptype='terrain',
##'              subsetRange=range(plotDf$value),
##'              xlim=range(plotDf$long)+mapMargin,
##'              ylim = range(plotDf$lat)+mapMargin) ##gross reservoir
##' 
##' ## hydro
##' hydroFile <- '/home/adugger/WRF_Hydro/Fourmile_fire/DOMAIN/hydro_OrodellBasin_100m_8msk.nc'
##' ClosureHydro <- VisualizeDomain(hydroFile, plotVar='CHANNELGRID', plot=FALSE)
##' 
##' closureRtnHydro <-
##'   ClosureHydro(zoom=11, pointsize=2,
##'                ## can reference the internal plotDf (or other variables internal to the closure)
##'                location=c(lon=mean(plotDf$long), lat=mean(plotDf$lat)), alpha=.2,
##'                grad=c('white','blue'), maptype='terrain') ##gross reservoir
##' 
##' closureRtnHydro <-
##'   ClosureHydro(zoom=11, pointsize=2,
##'                ## can reference the internal plotDf (or other variables internal to the closure)
##'                location=c(lon=mean(plotDf$long), lat=mean(plotDf$lat)),
##'                subsetRange=c(0),
##'                grad='blue', maptype='terrain') ##gross reservoir
##' 
##' 
##' ## put them together
##' closureRtnGeo$ggObj +
##'   geom_point(data=closureRtnHydro$plotDf,
##'              aes(x=long, y=lat), color='darkblue') +
##'                ggtitle('Fourmile Creek, CO - Elevation and Stream Channel')
##' 
##' }
##' @concept plot
##' @keywords hplot
##' @family domain
##' @export
VisualizeDomain <- function(file, plotVar=NULL, plot=TRUE, plotDf=NULL) {
  
  ## get the file coordinates
  if(is.null(plotDf)) plotDf <- GetDomainCoordsProj(file)
  bbox <- plotDf@bbox
  plotDf <- as.data.frame(plotDf)

  # if no variable specified, ask to user to choose
  if(!length(plotVar)) {
    ncid <- ncdf4::nc_open(file)
    varNames <- names(ncid$var)
    ncdf4::nc_close(ncid)
    varInd <- readline(prompt=cat(paste(1:length(varNames),varNames, sep=': '),sep='\n'))
    plotVar <- varNames[as.numeric(varInd)]
  }

  plotVals <- ncdump(file, plotVar, q=TRUE)
  while(length(plotVals)!=nrow(plotDf)) {
    ncid <- ncdf4::nc_open(file)
    dimNames <- names(ncid$dim)[c((ncid$var[[plotVar]]$dimids)+1)]
    dimLens <- plyr::llply(dimNames, function(dd) ncid$dim[[dd]]$len)
    whDim <- which(dimLens>1)
    ncdf4::nc_close(ncid)
    cat('Enter desired hyperslab (e.g. [,,1]):',sep='\n')
    cat(paste(1:length(dimNames[whDim]),dimNames[whDim],dimLens[whDim], sep=': '),sep='\n')
    hyperSlab <- readline(prompt=':')
    plotVals <- eval(parse(text=paste0('plotVals',hyperSlab)))
  }
  plotDf$value <- as.vector(plotVals)
  
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
      ggColorScaleObj <- ggplot2::scale_color_gradientn(name=plotVar, colours=gradNColors)
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
