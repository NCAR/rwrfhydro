#' gage-gage distances on gridded channel network
#' @examples 
#' library(rwrfhydro)
#' ncdump('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/CHANNEL_CONNECTIVITY.nc')
#' TO_NODE <- 
#' ncdump('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/CHANNEL_CONNECTIVITY.nc',
#'        'TO_NODE', quiet=TRUE)
#' FROM_NODE <- 
#' ncdump('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/CHANNEL_CONNECTIVITY.nc',
#'        'FROM_NODE', quiet=TRUE)
#' doMC::registerDoMC(4)
#' newCon <- ReExpressGridChanConn(FROM_NODE, TO_NODE)
#' upstream <- newCon$upstream
#' downstream <- newCon$downstream
#' TO_NODE[which(is.na(TO_NODE))] <- 0
#' done <- for(ii in 1:length(TO_NODE)) CheckReGridConnUp(ii)
#' done <- for(ii in 1:length(TO_NODE)) CheckReGridConnDown(ii)
#'save(upstream,downstream, 
#'     file='~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/newConnectivity.Rdb')
#' @export
ReExpressGridChanConn <- function(upstream, downstream) {
    
  FindUpstream   <- function(down) list(upstream[which(downstream==down)])
  FindDownstream <- function(up) list(downstream[which(upstream==up)])
  
  downstream[which(is.na(downstream))] <- 0
  
  ## re express 
  for(ff in c(FindUpstream,FindDownstream)) {
    connList <- plyr::llply( upstream, ff, 
                             .parallel=foreach::getDoParWorkers() >1 )
    connLen <- plyr::laply( connList, function(ll) length(ll[[1]]) )
    whLenPos <- which(connLen > 0)
    cumSumLenPos <- cumsum(connLen[whLenPos])
    connStart <- as.integer(0*(1:length(connLen)))
    connStart[whLenPos] <- cumSumLenPos
    ## the cumulative sum dosent give the start, it gives the last in each range. fix
    whLenGt1 <- which(connLen > 1)
    cumAdj <- as.integer(0*(1:length(connLen)))
    cumAdj[whLenGt1] = cumAdj[whLenGt1] - connLen[whLenGt1] + 1
    connStart <- connStart + cumAdj
    conn <- unlist(connList)
    conn[which(is.na(conn))] <- 0
    connEnd <- connStart+connLen-1
    connEnd[which(connEnd < 0)] <- 0
    if(formalArgs(ff)[1]=='down') 
      upstreamList = list( upstream=conn, start=connStart, end=connEnd)
    if(formalArgs(ff)[1]=='up') 
      downstreamList = list( downstream=conn, start=connStart, end=connEnd)
  }
  list(upstream = upstreamList, downstream = downstreamList)
}
 
#'Check for re-rxpressed grid connectivity upstream. 
#'@export
CheckReGridConnUp <- function(ind, printInds=FALSE) {
 ogWay <-  FROM_NODE[which(TO_NODE==ind)]
 newWay <- upstream$upstream[upstream$start[ind]:upstream$end[ind]]
 if(!( all(ogWay %in% newWay) & all(newWay %in% ogWay))) stop('test failed upstream')
 if(printInds) print(paste(ind, ':', paste(newWay,collapse=', ')))
 TRUE
}

#'Check for re-expressed grid connectivity downstream. 
#'@export
CheckReGridConnDown <- function(ind, printInds=FALSE) {
  ogWay <-  TO_NODE[which(FROM_NODE==ind)]
  newWay <- downstream$downstream[downstream$start[ind]:(downstream$end[ind])]
  if(!( all(ogWay %in% newWay) & all(newWay %in% ogWay))) stop('test failed downstream')
  if(printInds) print(paste(ind, ':', paste(newWay,collapse=', ')))
  TRUE
}

#' Find the upstream links/nodes/cells with distances from a specified point
#' @examples 
#'  devtools::load_all()
#' 
#'  load('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/newConnectivity.Rdb')
#'  length <- 
#'  ncdump('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/CHANNEL_CONNECTIVITY.nc',
#'         'CHANLEN', quiet=TRUE)
#' whPour <- which(downstream$downstream==0)
#' upstreamSearch <- GatherUpstream(upstream, length, gridded=TRUE, 
#'                10567, indDist=list(ind=c(), dist=c()) )
#'  #Visualize
#' map <- VisualizeChanNtwk('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/200302012300.CHRTOUT_DOMAIN1')
#' mapOut <- map(zoom=10,click=TRUE)
#' mapOut$linkDf$dist <- NA
#' mapOut$linkDf$dist[upstreamSearch$ind] <- upstreamSearch$dist
#' mapOut$ggplot$layers[[4]] <- NULL
#' mapOut$ggplot +
#'   ggplot2::geom_point(data=mapOut$linkDf, 
#'                       aes(x=lon, y=lat, color=dist)) + 
#'   ggplot2::scale_color_gradientn(colours = c('red','orange','yellow','green'))
#' ## output gage-gage distances for Boulder creek above 75th.
#' ## Determine the indices of the gage locations.
#' frxst <- 
#'   ncdump("~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/Fulldom_hires_netcdf_file.nc", 
#'   'frxst_pts', quiet=TRUE)
#' frxst <- FlipVert(frxst)
#' whFrxst2D <- plyr::laply(1:4, function(ww) which(frxst == ww, arr.ind=TRUE))
#'          
#' chanX <- 
#'   ncdump('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/CHANNEL_CONNECTIVITY.nc',
#'          'CHANXI', quiet=TRUE)
#' chanY <- 
#'   ncdump('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/CHANNEL_CONNECTIVITY.nc',
#'          'CHANYJ', quiet=TRUE)
#' whFrxst <- c()
#' for(ff in 1:nrow(whFrxst2D)) {
#'   whFrxst <- append(whFrxst, which(chanX==whFrxst2D[ff,1] & 
#'                                    chanY==whFrxst2D[ff,2]))
#' }
#' ## some rather extensive tests on this...
#' GetGageGageDist(whFrxst[1], whFrxst[4], upstream, downstream, length)
#' GetGageGageDist(whFrxst[4], whFrxst[1], upstream, downstream, length)
#' GetGageGageDist(whFrxst[1], whFrxst[2], upstream, downstream, length)
#' GetGageGageDist(whFrxst[1], whFrxst[3], upstream, downstream, length)
#' GetGageGageDist(whFrxst[4], whFrxst[3], upstream, downstream, length)
#' GetGageGageDist(whFrxst[3], whFrxst[4], upstream, downstream, length)
#' GetGageGageDist(whFrxst[2], whFrxst[4], upstream, downstream, length)
#' GetGageGageDist(whFrxst[4], whFrxst[2], upstream, downstream, length)
#' @export
GetGageGageDist <- function(ind1, ind2, upstream, downstream, length, 
                            gridded=TRUE, maxIterDownstream=100) {
  #stop()
  inds <- c(ind1, ind2)
  otherInd <- c(2,1)
  nIter <- 0
  while(nIter <= (maxIterDownstream-1)) {
   
     for(ii in 1:2) {
      #print(ii)
      tmpInds <- inds 
      
      if(nIter > 0) {
        ## For NHD plus, downJunct could be an array! Fix this?
        #downJunct <- GoToDownstreamJunct(downstream, upstream, inds[ii], skip=TRUE)
        downJunct <- 
          IterateFunction(GoToDownstreamJunct, 
                          list(downstream=downstream, upstream=upstream, 
                               ind=inds[ii], skip=TRUE),
                          'ind', nIter)
        #print(paste0(nIter,' , ',ii,' : ', downJunct))
        tmpInds[ii] <- downJunct
        tmpInds[otherInd[ii]] <- inds[ii]
      }
      
      upstreamSearch <- 
        GatherUpstream(upstream, length, gridded=gridded, 
                       tmpInds[ii], indDist=list(ind=c(), dist=c()) )

      if(length(whInd <- which(upstreamSearch$ind == tmpInds[otherInd[ii]]))) {
        if(nIter==0) return(upstreamSearch$dist[whInd])
        downToStartDist <- upstreamSearch$dist[whInd]
      }

      if(nIter > 1) { 
        ## tmpInds[ii] <- downJunct # same
        tmpInds[otherInd[ii]] <- inds[otherInd[ii]]
        upstreamSearch <- 
          GatherUpstream(upstream, length, gridded=gridded, 
                         tmpInds[ii], indDist=list(ind=c(), dist=c()) )
        if(length(whInd <- which(upstreamSearch$ind == tmpInds[otherInd[ii]]))) {
          downToEndDist <- upstreamSearch$dist[whInd]
          ## For link/reach will have double counted the junction, subtract it off.
          if(!gridded) downToEndDist <- downToEndDist - length[downJunct]
          return( downToStartDist + downToEndDist )
        }
      } ## if(nIter > 1)
      
    } ## for(1 in 1:2)
    nIter <- nIter+1
  } ## while
  warning(paste0('Points (',ind1,' & ',ind2,
                 ' were not found to be connected upstream within ',
                 maxIterDownstream,' down stream junctions of each.'))
  NA
}

#' @export
GatherUpstream <- function(upstream, length, gridded=TRUE, 
                           start, indDist=list(ind=c(), dist=c())) {
  anyUpstream <- upstream$start[start] > 0
  if(!anyUpstream) return(indDist)
  upstreamInds <- upstream$upstream[upstream$start[start]:upstream$end[start]]
  for(ss in upstreamInds) {
    if(gridded) { # center point to center point for gridded
      indDist$ind  <- append(indDist$ind,  ss)  
      startDist <- indDist$dist[which(indDist$ind == start)]
      if(!length(startDist)) startDist=0
      indDist$dist <- append(indDist$dist, startDist + sum(length[c(start,ss)])/2)
      ## once this code is working for a while without err, can take this check out:
      if(any(is.na(indDist$dist)) | (length(indDist$dist)!=length(indDist$ind)) ) 
        stop()
    } else {    # count full links/reaches
      if(length(indDist$dist)==0) { ## first time count the start reach
        indDist$ind  <- append(indDist$ind,  start)
        indDist$dist <- append(indDist$dist, length[start])
      }
      indDist$ind  <- append(indDist$ind,  ss)
      startDist <- indDist$dist[which(indDist$ind == start)]
      indDist$dist <- append(indDist$dist, startDist + length[ss])/2
    }
    indDist <- GatherUpstream(upstream, length, gridded, ss, indDist)
  }
  indDist
}

#' @examples
#' ii <- GoToDownstreamJunct(downstream, upstream, 9296)
#' ii <- GoToDownstreamJunct(downstream, upstream, ii, skipFirst=TRUE)
#' @export
GoToDownstreamJunct <- function(downstream, upstream, ind, skipFirst=FALSE) {
  ## If a pourpoint is given
  if(ind==0 | downstream$downstream[downstream$start[ind]]==0) {
    print(paste0('Index ',ind,' is a pour point.')) 
    return(ind)
  }
  ## This is a junction
  if(!skipFirst) if(upstream$start[ind] < upstream$end[ind]) return(ind)
  ## handle possible splits going downstream
  downInds <- downstream$downstream[downstream$start[ind]:downstream$end[ind]]
  junctInds <- c()
  for(dd in downInds) {
    ## dont follow pour points.
    if(downstream$downstream[downstream$start[dd]]>0) {
      junctInds <- append(junctInds, 
                          GoToDownstreamJunct(downstream, upstream, dd))
    } else { 
      junctInds <- append(junctInds, dd)
      print(paste0('Index ',dd,' is a pour point.')) 
    }
  }
  junctInds
}
  
#' Iterate a function
#' @examples 
#' ii <- GoToDownstreamJunct(downstream, upstream, 9296)
#' ii <- GoToDownstreamJunct(downstream, upstream, ii, skipFirst=TRUE)
#' IterateFunction(GoToDownstreamJunct, 
#'                list(downstream=downstream, upstream=upstream, ind=9296, skip=TRUE),
#'                'ind', 3)
#' 
#' @export
IterateFunction <- function(f, args, resultArg, iterations=1){
    for(ii in 1:iterations) {
    args[[resultArg]] <- do.call(f, args)
  }
  args[[resultArg]]
}
  
##========================================================================

if(FALSE){
##----------------------
## create the gage-gage distances file
g1<-plyr::laply(whFrxst[2:4], GetGageGageDist, 
                whFrxst[1], upstream, downstream, length, gridded=TRUE)
g2<-plyr::laply(whFrxst[3:4], GetGageGageDist, 
                whFrxst[2], upstream, downstream, length, gridded=TRUE)
g3<-plyr::laply(whFrxst[4], GetGageGageDist, 
                whFrxst[3], upstream, downstream, length, gridded=TRUE)
gageIds <- c('06730200','06730160','06727410','06727500')
gg <- data.frame(gage1=gageIds[c(1,1,1,2,2,3)],
                 gage2=gageIds[c(2,3,4,3,4,4)],
                 dist=c(g1,g2,g3) )

## need to set the missing value used by ncdf4? i think it's NA by default
dimensionList <-
  list(  # n.b. the dimension order: z,y,x,t
    distancesIdInd=list(name='distances',
                      units='', 
                      values=1:nrow(gg),
                      unlimited=TRUE,
                      create_dimvar=FALSE),
    
    stationIdStrLen=list(name='stationIdStrLen',
                         units='', 
                         values=1:15,
                         unlimited=FALSE,
                         create_dimvar=FALSE)
  )


varList = list()
varList[[1]] <- 
  list( name='gageId1',
        longname='USGS station identifer for first of pair',
        units='',
        precision = 'char',
        #missing = ,
        dimensionList=dimensionList[c('stationIdStrLen','distancesIdInd')],
        data = gg$gage1 )

varList[[2]] <- 
  list( name='gageId2',
        longname='USGS station identifer for second of pair',
        units='',
        precision = 'char',
        #missing = ,
        dimensionList=dimensionList[c('stationIdStrLen','distancesIdInd')],
        data = gg$gage2 )

varList[[3]] <- 
  list( name='distance',
        longname='distance.meters',
        units='m',
        precision = 'float',
        #missing = ,
        dimensionList=dimensionList[c('distancesIdInd')],
        data = gg$dist )

MkNcdf( varList, 
        filename=paste0('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/',
                        'gageGageDistances_Bldr_Creek.nc'), 
        overwrite=TRUE )

ncdump(paste0('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/',
              'gageGageDistances_Bldr_Creek.nc'))

##----------------------
## nudging parameters file
gageParams <- read.csv('~/ncar/WRF_Hydro/DOMAIN_library/CONUS/nhdRtIntersect.csv',
                       colClasses = 'character' )
gageParams$R=2000
gageParams$G=.75
gageParams$tau=60

## need to set the missing value used by ncdf4? i think it's NA by default
dimensionList <-
  list(  # n.b. the dimension order: z,y,x,t
    stationIdInd=list(name='stationIdInd',
                        units='', 
                        values=1:nrow(gageParams),
                        unlimited=TRUE,
                        create_dimvar=FALSE),
    
    stationIdStrLen=list(name='stationIdStrLen',
                         units='', 
                         values=1:15,
                         unlimited=FALSE,
                         create_dimvar=FALSE)
  )

varList = list()
varList[[1]] <- 
  list( name='gageId',
        longname='USGS station identifer',
        units='-',
        precision = 'char',
        #missing = ,
        dimensionList=dimensionList[c('stationIdStrLen','stationIdInd')],
        data = gageParams$nhdRtIntersect )

varList[[2]] <- 
  list( name='R',
        longname='Radius of influence in meters',
        units='m',
        precision = 'float',
        #missing = ,
        dimensionList=dimensionList[c('stationIdInd')],
        data = gageParams$R )

varList[[3]] <- 
  list( name='G',
        longname='Amplitude of nudging',
        units='-',
        precision = 'float',
        #missing = ,
        dimensionList=dimensionList[c('stationIdInd')],
        data = gageParams$G )

varList[[4]] <- 
  list( name='tau',
        longname='Time tapering parameter',
        units='s',
        precision = 'float',
        #missing = ,
        dimensionList=dimensionList[c('stationIdInd')],
        data = gageParams$tau )

MkNcdf( varList, 
        filename=paste0('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/',
                        'nudgingParameters_Bldr_Creek.nc'), 
        overwrite=TRUE )

ncdump(paste0('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/',
              'nudgingParameters_Bldr_Creek.nc'))

}