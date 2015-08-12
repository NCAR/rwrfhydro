#' Get the "distance" between two indices on the network. 
#' 
#' If the gages lie up or downstream from one another, then this "symmetric" 
#' distance between them is returned. If the gages are connected by a common 
#' downstream (to both) confulence, requiring a downstream-upstream search, then 
#' the distance from each to the confluence is returned, which is not symmetric.
#' The output data frame contains:
#' ind1: if symmetric, the upstream index. If not symmetric, this index's 
#' distance to the confluence is reported.
#' ind2: if symmetric, the downstream index. If not symmetric, this is just a 
#' place holder.
#' symmetric: if the points are connected up/downstream from each other or if a
#' downstream-upstream search is required to connect them.
#' distance: in meters, as described for ind1 and ind2. 
#' 
#' @examples 
#'  \dontrun{
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
#' }
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
        ## With each iteration, try a successively downstream confluence.
        ## For NHD plus, downJunct could be an array! Fix this?
        downJunct <- 
          IterateFunction(GoToDownstreamJunct, 
                          list(downstream=downstream, upstream=upstream, 
                               ind=inds[ii], skip=TRUE),
                          'ind', nIter)
        #print(paste0(nIter,' , ',ii,' : ', downJunct))
        tmpInds[ii] <- downJunct
        #tmpInds[otherInd[ii]] <- inds[ii]
        tmpInds[otherInd[ii]] <- inds[otherInd[ii]] ## experimental
      }
      
      upstreamSearch <- 
        GatherUpstream(upstream, length, gridded=gridded, 
                       tmpInds[ii], indDist=list(ind=c(), dist=c()) )

      ## was the other index found in upstream search?
      if(length(whInd <- which(upstreamSearch$ind == tmpInds[otherInd[ii]]))) {
        ## if it was found with out iteration, the points are connected via
        ## simple upstream search.
        if(nIter==0) return(data.frame(ind1=tmpInds[otherInd[ii]],
                                       ind2=tmpInds[ii],
                                       symmetric=TRUE,
                                       dist=upstreamSearch$dist[whInd]))
        
        ## otherwise (you get here) this is the distance from the test common
        ## confluence in tmpInds[ii] UP to the start pt in tmpInds[otherInd[ii]], 
        ## which is not the one necessarily upstream from the downstreamsearch.
        downToStartDist <- upstreamSearch$dist[whInd]

        ## tmpInds[ii] <- downJunct # same
        tmpInds[otherInd[ii]] <- inds[ii]
        upstreamSearch <- 
          GatherUpstream(upstream, length, gridded=gridded, 
                         tmpInds[ii], indDist=list(ind=c(), dist=c()) )
        if(length(whInd <- which(upstreamSearch$ind == tmpInds[otherInd[ii]]))) {
          downToEndDist <- upstreamSearch$dist[whInd]
          ## first row is the first search above, second is the search directly above.
          return( data.frame(ind1=c(inds[otherInd[ii]], inds[ii]),
                             ind2=c(inds[ii],           inds[otherInd[ii]]),
                             symmetric=c(FALSE, FALSE),
                             dist=c(downToStartDist, downToEndDist) ) )
        } ## if match was found
      } ## if(nIter > 1)
      
    } ## for(1 in 1:2)
    nIter <- nIter+1
  } ## while
  warning(paste0('Points (',ind1,' & ',ind2,
                 ' were not found to be connected upstream within ',
                 maxIterDownstream,' down stream junctions of each.'))
  NA
}

if(FALSE){
## what is the comId for Boulder Creek at 75th?
## Alot of this going to change when reInd is applied, mostly just variable names
bc75ComId <- 2889214
## load the files
load("~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink.reInd.Rdb")
load("~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink.reExpTo.Rdb")
load("~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink.reExpFrom.Rdb")
whBcCom <- which(reInd$comId == bc75ComId)
upBc75 <- GatherUpstream(from, reInd$length, gridded=FALSE, start=whBcCom )
## dosent work:
downBc75 <- GatherUpstream(to, reInd$length, gridded=FALSE, start=whBcCom )

upstream <- from
upstream <- to

length   <- reInd$length
gridded=FALSE
start=whBcCom
indDist=list(ind=c(), dist=c())
}

#distances are the cumulative distance from the given start
#' @export
GatherUpstream <- function(upstream, length, gridded=TRUE, 
                           start, indDist=list(ind=c(), dist=c())) {
  anyUpstream <- upstream$start[start] > 0
  if(!anyUpstream) return(indDist)
  ##rename <- c('go', 'go')
  ##names(rename)<-c('to','from')
  ##stream <- plyr::rename(upstream, rename)
  upstreamInds <- upstream$from[upstream$start[start]:upstream$end[start]]
  #upstreamInds <- upstream$to[upstream$start[start]:upstream$end[start]]
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
      ## is this the correct distance calculation?
      startDist <- indDist$dist[which(indDist$ind == start)[1]]
      indDist$dist <- append(indDist$dist, startDist + length[ss])
    }
    indDist <- GatherUpstream(upstream, length, gridded, ss, indDist)
  }
  indDist
}





#' Find the next downstream junction from current grid cell.
#' 
#' @examples
#' \dontrun{
#' ii <- GoToDownstreamJunct(downstream, upstream, 9296)
#' ii <- GoToDownstreamJunct(downstream, upstream, ii, skipFirst=TRUE)
#' }
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
#' \dontrun{
#' ii <- GoToDownstreamJunct(downstream, upstream, 9296)
#' ii <- GoToDownstreamJunct(downstream, upstream, ii, skipFirst=TRUE)
#' IterateFunction(GoToDownstreamJunct, 
#'                list(downstream=downstream, upstream=upstream, ind=9296, skip=TRUE),
#'                'ind', 3)
#' }
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
g1<-plyr::ldply(whFrxst[2:4], GetGageGageDist, 
                whFrxst[1], upstream, downstream, length, gridded=TRUE)
g2<-plyr::ldply(whFrxst[3:4], GetGageGageDist, 
                whFrxst[2], upstream, downstream, length, gridded=TRUE)
g3<-plyr::ldply(whFrxst[4], GetGageGageDist, 
                whFrxst[3], upstream, downstream, length, gridded=TRUE)
gg <- data.frame(rbind(g1, g2, g3))
gageIndToId <- c('06730200','06730160','06727410','06727500')
names(gageIndToId) <- c(whFrxst)
gg$ind1 <- gageIndToId[as.character(gg$ind1)]
gg$ind2 <- gageIndToId[as.character(gg$ind2)]
gg
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
                         create_dimvar=FALSE),
    
    symmetricIdStrLen=list(name='symmetricIdStrLen',
                         units='', 
                         values=1,
                         unlimited=FALSE,
                         create_dimvar=FALSE)
  )


varList = list()
varList[[1]] <- 
  list( name='stationId1',
        longname='USGS station identifer for first of pair',
        units='',
        precision = 'char',
        #missing = ,
        dimensionList=dimensionList[c('stationIdStrLen','distancesIdInd')],
        data = formatC(gg$ind1, width=15) )

varList[[2]] <- 
  list( name='stationId2',
        longname='USGS station identifer for second of pair',
        units='',
        precision = 'char',
        #missing = ,
        dimensionList=dimensionList[c('stationIdStrLen','distancesIdInd')],
        data = formatC(gg$ind2, width=15))

varList[[3]] <- 
  list( name='symmetric',
        longname='pt to pt dist (vs distance to nearest confluence)?',
        units='-',
        precision = 'char',
        #missing = ,
        dimensionList=dimensionList[c('symmetricIdStrLen','distancesIdInd')],
        data = gg$symmetric )  ## results in T & F without coercion. 

varList[[4]] <- 
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


}
