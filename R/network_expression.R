## Re-express RouteLink.nc

## First, just re-order by index (no sorting). 
#' @examples
#' ReIndexRouteLink(routeLinkFile <- '~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink3.nc')
ReIndexRouteLink <- function(routeLinkFile) {
  ncid <- ncdf4::nc_open(routeLinkFile)
  link   <- ncdf4::ncvar_get(ncid,'link')
  nhdRe <- data.frame(from   = ncdf4::ncvar_get(ncid,'from'),
                      to     = ncdf4::ncvar_get(ncid,'to'),
                      Length = ncdf4::ncvar_get(ncid,'Length') )
  ncdf4::nc_close(ncid)

  ## Zero stands for 1st order or pourpoint, but dosenot have a comm/link id!
  ReExp <- 0:length(link)
  names(ReExp) <-  as.character(c(0,link))

  nhdRe$from   <- ReExp[as.character(nhdRe$from)]
  ## Wow, this next line took 15 minutes. But it seems like chunking it
  ## makes it takes only about 4 minutes or less.
  nhdRe$to     <- ReExp[as.character(nhdRe$to)]
  nhdRe$commId <- link

  base <- strsplit(basename(routeLinkFile),'\\.')[[1]][1]
  dir  <- dirname(routeLinkFile)
  outFile <- paste0(dir,'/',base,'.ReIndex.Rdb')
  save(nhdRe, file = outFile)
  outFile
}

## Reexpress the network
##              from   reind      to
##------------------------------------
## downstream  match  lookup
## downstream          match  lookup
##------------------------------------
##   upstream  lookup  match
##   upstream         lookup   match
##------------------------------------
#' @examples
#' doMC::registerDoMC(16)
#' ReExpNetwork("/home/jamesmcc/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink3.ReIndex.Rdb")
#' ReExpNetwork("/home/jamesmcc/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink3.ReIndex.Rdb", up=FALSE)

ReExpNetwork <- function(routeLinkReInd, upstream=TRUE) {
  load(routeLinkReInd)
  
  FindDownstream <- function(ind) list( union( nhdRe$reind[which(nhdRe$from == ind)], 
                                              nhdRe$to[which(nhdRe$reind == ind)] ) )
  FindUpstream   <- function(ind) list( union( nhdRe$reind[nhdRe$to   == ind], 
                                              nhdRe$from[nhdRe$reind == ind]) )

  FindFunc <- if(upstream) { FindUpstream } else { FindDownstream }
  
  theList <- plyr::llply( 1:length(nhdRe$to), FindFunc,
                         .parallel=foreach::getDoParWorkers() > 1)
  theLen <- plyr::laply( theList, function(ll) length(ll[[1]]) )
  whLenPos <- which(theLen > 0)
  cumSumLenPos <- cumsum(theLen[whLenPos])
  theStart <- as.integer(0*(1:length(theLen)))
  theStart[whLenPos] <- cumSumLenPos
  ## the cumulative sum dosent give the start, it gives the last in each range. fix
  whLenGt1 <- which(theLen > 1)
  cumAdj <- as.integer(0*(1:length(theLen)))
  cumAdj[whLenGt1] = cumAdj[whLenGt1] - toLen[whLenGt1] + 1
  theStart <- theStart + cumAdj

  base <- strsplit(basename(routeLinkReInd),'\\.')[[1]][1]
  dir  <- dirname(routeLinkReInd)
  if(upstream) {
    nhdTo = list( to=unlist(theList), start=theStart, length=theLen)
    outFile <- paste0(dir,'/',base,'.reExpTo.Rdb')
    save(nhdTo, file=outFile)   
  } else {
    nhdFrom = list( from=unlist(theList), start=theStart, length=theLen)
    outFile <- paste0(dir,'/',base,'.reExpFrom.Rdb')
    save(nhdFrom, file=outFile)
  }
  
  outFile
}
  
## 
# ##------------------------
# CheckConnUpstream <- function(ind, printInds=FALSE) {
#   myWay <- if(nhdFrom$start[ind] >0) {
#     nhdFrom$from[nhdFrom$start[ind]:(nhdFrom$start[ind]+nhdFrom$length[ind]-1)]
#   } else 0
#   nhdWay <- union(nhd$reind[nhd$to==ind], nhd$from[nhd$reind==ind])
#   test <- all(myWay %in% nhdWay) & all(nhdWay %in% myWay)
#   if(!test) stop(paste0('test failed at index: ',ind))
#   if(printInds) print(nhdWay)
#   test
# }
# 
# CheckConnDownstream <- function(ind, printInds=FALSE) {
#   myWay <- if(nhdTo$start[ind] >0) {
#     nhdTo$to[nhdTo$start[ind]:(nhdTo$start[ind]+nhdTo$length[ind]-1)]
#   } else 0
#   nhdWay <- union(nhd$reind[nhd$from==ind], nhd$to[nhd$reind==ind])
#   test <- all(myWay %in% nhdWay) & all(nhdWay %in% myWay)
#   #if(!test) stop(paste0('test failed at index: ',ind))
#   if(printInds) print(nhdWay)
#   test
# }
# #for (ii in seq(1,2720000,1000)) { print(ii); print(CheckConnTo(ii)) }
# #for (ii in seq(1,2720000,1000)) { print(ii); print(CheckConnUpstream(ii)) }
# 
# ##-----------------------
# load('~/ncar/WRF_Hydro/DOMAIN_library/CONUS/nhdConnOrdered.Rdb')
# load("~/ncar/WRF_Hydro/DOMAIN_library/CONUS/nhdToList.Rdb")
# load("~/ncar/WRF_Hydro/DOMAIN_library/CONUS/nhdFromList.Rdb")
# 
# whPour <- which(nhdTo$start == 0)
# aPour<- whPour[1]
# 
# wh1stOrd <- which(nhdFrom$start == 0)
# a1st <- wh1stOrd[1]
# 
# GatherUpstream <- function(start,gathered=c()) {
#   anyUpstream <- nhdFrom$start[start] > 0
#   if(!anyUpstream) return(gathered)
#   upstreamStart <- nhdFrom$start[start]
#   upstream <- nhdFrom$from[upstreamStart:
#                            (upstreamStart+nhdFrom$length[start]-1)]
#   for(ss in upstream) {
#     gathered <- append(gathered, ss)
#     gathered <- GatherUpstream(ss,gathered)
#   }
#   gathered
# }
# GatherUpstream(638, c())
# 
# GatherUpstream(7, c())
# GatherUpstream(9, c())
# 
# 




##==================================================================================================
## Rexpression of gridded channel network.
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


