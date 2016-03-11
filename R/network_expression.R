##' ReExpress the Route_Link.nc file.
##' 
##' A wrapper on the individual functions which perform this full reexpression and generate the netcdf file. 
##' @param routeLink.nc Character path/file to the desired Route_link.nc netcdf file for the link/reach-based routing.
##' @param parallel Logical use a registered backend for plyr?
##' @return Named Character vector for each of the 4 files created as outputs with full paths. 
##' @examples 
##' \dontrun{
##' ReExpressRouteLink("~/WRF_Hydro/DOMAIN_library/Boulder_Creek_100m_1km_2sqkm_full_2015_09_03/Route_Link.nc")
##' ReExpressRouteLink() # if there is a Route_Link.nc file in getwd() (the current directory).
##' }
##' @keywords manip
##' @concept dataMgmt nudging
##' @family networkExpression nudging
##' @export
ReExpressRouteLink <- function(routeLink.nc='Route_Link.nc', parallel=FALSE) {
  reInd.Rdb  <- ReIndexRouteLink(routeLink.nc)
  upstream.Rdb   <- ReExpNetwork(reInd.Rdb, parallel=parallel)
  downstream.Rdb <- ReExpNetwork(reInd.Rdb, up=FALSE, parallel=parallel)
  reExp.nc    <- NtwKReExToNcdf(downstream.Rdb, upstream.Rdb)
  outPath <- dirname(routeLink.nc)
  if(outPath==".") outPath <- getwd()
  outFiles <- paste0(outPath,'/',basename(c(reInd.Rdb, upstream.Rdb, downstream.Rdb, reExp.nc)))
  cat("Created the following files: ", outFiles, sep = '\n')
  names(outFiles) <- c('reInd.Rdb', 'upstream.Rdb', 'downstream.Rdb', 'reExp.nc')
  invisible(outFiles)
}

##' ReIndex RouteLink.nc (netcdf) files for indexed network traversal.
##' 
##' \code{ReIndexRouteLink} reIndexes by order in the RouteLink file,
##' replacing ComID with this index.
##' 
##' @param routeLinkFile The netcdf routelink file to process. 
##' @return The resulting file which was written to disk, of the form "infile.reInd.nc"
##'   
##' @examples
##'  \dontrun{
##'reIndFile <-
##'  ReIndexRouteLink(routeLinkFile <- '~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink_2015_07_31.nc')
##'reIndFile <-
##'  ReIndexRouteLink(routeLinkFile <- '~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink4.nc')
##'reIndFile <-
##'  ReIndexRouteLink(routeLinkFile <- '~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink3.nc')
##' reIndFile <-
##'  ReIndexRouteLink(routeLinkFile <- '~/WRF_Hydro/DOMAIN_library/BoCr_100m_1km_NHDPlus_2015_08_11/Route_Link.nc')
##' }
##' @keywords manip
##' @concept dataMgmt
##' @family networkExpression nudging
##' @export
ReIndexRouteLink <- function(routeLinkFile) {
  ncid <- ncdf4::nc_open(routeLinkFile)
  link  <- ncdf4::ncvar_get(ncid,'link')
  reInd <- data.frame(from   = ncdf4::ncvar_get(ncid,'from'),
                      to     = ncdf4::ncvar_get(ncid,'to'),
                      length = ncdf4::ncvar_get(ncid,'Length')
                      )
  if('NHD_Gage' %in% names(ncid$var))
    reInd$gage <- ncdf4::ncvar_get(ncid,'NHD_Gage')
  ncdf4::nc_close(ncid)

  ## Zero stands for 1st order or pourpoint, but dosenot have a comm/link id!
  ReExp <- 0:length(link)
  names(ReExp) <-  AsCharLongInt(c(0,link))
  ### bad: names(ReExp) <-  as.character(c(0,link))

  ##When there are NA's or issues with the following expressions,
  ## they take more than a few (ie 2) seconds
  ## it's a check that the inputs are all correct
  reInd$from  <- ReExp[AsCharLongInt(reInd$from)]
  reInd$to    <- ReExp[AsCharLongInt(reInd$to)]
  reInd$comId <- link

  #print(summary(reInd))
  
  base <- strsplit(basename(routeLinkFile),'\\.')[[1]]
  base <- paste(base[-length(base)+(-1:0)],collapse='.')
  dir  <- dirname(routeLinkFile)
  outFile <- paste0(dir,'/',base,'.reInd.Rdb')
  save(reInd, file = outFile)
  outFile
}


#' ReExpress stream networks indexed network traversal.
#' 
#' \code{ReExpNetwork} re-expresses topological relationships between three variables, 
#  [from, to, id] (as used by NHD+v2.1) into separate lists for index based
#' up- and down- stream traversal, depending on the upstream argument. 
#' 
#' @param routeLinkReInd The netcdf routelink file to process.
#' @param upstream Logical, re-express connectivity upstream (TRUE) or downstream (FALSE).
#' @param parallel Logical use a registered backend for plyr?
#' @return The resulting file which was written to disk, of the form the "infile.reExpTo.nc"
#'         (downstream) or "infile.reExpFrom.nc" (upstream).
#' @examples
#' \dontrun{
#'   library(rwrfhydro)
#'   doMC::registerDoMC(16)
#'   ReExpNetwork(reIndFile)
#'   ReExpNetwork(reIndFile, up=FALSE)
#' }
#' @keywords manip
#' @concept dataMgmt nudging
#' @family networkExpression nudging
#' @export
ReExpNetwork <- function(routeLinkReInd, upstream=TRUE, parallel=FALSE) {
  load(routeLinkReInd)
  
  ## Reexpress the network
  ##              from   reind      to
  ##------------------------------------
  ## downstream  match  lookup
  ## downstream          match  lookup
  ##------------------------------------
  ##   upstream  lookup  match
  ##   upstream         lookup   match
  ##------------------------------------
  # These first two are if commIds are needed.
  #FindUpstream   <- function(ind) union(reInd$comId[which(reInd$to   == ind)], 
  #                                      reInd$from[which(reInd$comId == ind)] )
  #FindDownstream <- function(ind) union(reInd$comId[which(reInd$from == ind)], 
  #                                      reInd$to[which(reInd$comId   == ind)] )
  FindUpstream   <- function(ind) {
    #theUnion <- union(which(reInd$to   == ind), reInd$from[ind])
    theUnion <- union(which(reInd$to   == ind), reInd$from[ind])
    if(length(theUnion)>1) theUnion <- setdiff(theUnion,0)
    theUnion
  }
  #FindDownstream <- function(ind) union(which(reInd$from == ind), reInd$to[ind]  )
  ## since the "from" field is defunct in Route_Link and b/c all spilts are removed.
  FindDownstream <- function(ind) unique( reInd$to[ind]  )

  FindFunc <- if(upstream) { FindUpstream } else { FindDownstream }

  theList <- plyr::llply( 1:length(reInd$to), FindFunc, .parallel=parallel)
  
  theLen <- plyr::laply( theList, function(ll) if(ll[1]==0) 0 else length(ll) )
  whLenPos <- which(theLen > 0)
  theList <- theList[whLenPos]
  cumSumLenPos <- cumsum(theLen[whLenPos])
  theStart <- as.integer(0*(1:length(theLen)))
  theStart[whLenPos] <- cumSumLenPos
  ## the cumulative sum dosent give the start, it gives the last in each range. fix
  whLenGt1 <- which(theLen > 1)
  cumAdj <- as.integer(0*(1:length(theLen)))
  cumAdj[whLenGt1] = cumAdj[whLenGt1] - theLen[whLenGt1] + 1
  theStart <- theStart + cumAdj
  theLen[which(theLen==0)] <- 1 ## adjust so end index can be calc by using start-len-1
  
  base <- strsplit(basename(routeLinkReInd),'\\.')[[1]]
  base <- paste(base[-length(base)+(0:1)],collapse='.')
  dir  <- dirname(routeLinkReInd)
  if(upstream) {
    from = list( from  = as.integer(unlist(theList)),
                 start = as.integer(theStart),
                 end   = as.integer(theStart+theLen-1) )
    outFile <- paste0(dir,'/',base,'.reExpFrom.Rdb')
    save(from, file=outFile)
  } else {
    to = list( to    = as.integer(unlist(theList)),
               start = as.integer(theStart),
               end   = as.integer(theStart+theLen-1) )
    outFile <- paste0(dir,'/',base,'.reExpTo.Rdb')
    save(to, file=outFile)   
  }
  
  outFile
}
  
#' CheckConn checks a re-expressed network.
#' 
#' \code{CheckConn} checks that a re-expressed network matches it's original expression.
#' 
#' @param ind The indices to check. 
#' @param upstream Logical, check connectivity upstream (TRUE) or downstream (FALSE).
#' @param printInds Logical, print the indices checked.
#' @return Logical, code halts at first FALSE.
#' @examples
#' \dontrun{
#'   for (ii in seq(1,2720000,1000)) { print(ii); print(CheckConn(ii)) }
#'   for (ii in seq(1,2720000,1000)) { print(ii); print(CheckConn(ii), up=FALSE) }
#'   for (ii in seq(1,2000)) { print(ii); print(CheckConn(ii)) }
#'   for (ii in seq(1,2000)) { print(ii); print(CheckConn(ii),up=FALSE) }
#' 
#' load("/home/jamesmcc/WRF_Hydro/DOMAIN_library/BoCr_100m_1km_NHDPlus_2015_08_11/Route_Link.reInd.Rdb")
#'
#' load("/home/jamesmcc/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink_2015_07_31.reExpFrom.Rdb")
#' load("/home/jamesmcc/WRF_Hydro/DOMAIN_library/BoCr_100m_1km_NHDPlus_2015_08_11/Route_Link.reExpFrom.Rdb")
#' ## number of contributing/upstream links.
#' nContrib<-from$end-from$start
#' nContrib[which(from$start>0)] <-nContrib[which(from$start>0)] +1
#' table(nContrib) 
#' for (ii in which(nContrib >3)) { print(ii); print(CheckConn(ii),up=FALSE) } 
#' comIdWhContribGt3 <-
#'   data.frame(nContrib = nContrib[which(nContrib > 16)],
#'              comId = reInd$comId[which(nContrib > 16)] )
#' comIdWhContribGt3 <- comIdWhContribGt3[order(comIdWhContribGt3$nContrib),]
#' write.table(comIdWhContribGt3, row.names=FALSE,
#'             file='~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink4.comIdWhContribGt3.txt')
#' 
#' load("/home/jamesmcc/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink_2015_07_31.reExpTo.Rdb")
#' load("/home/jamesmcc/WRF_Hydro/DOMAIN_library/BoCr_100m_1km_NHDPlus_2015_08_11/Route_Link.reExpFrom.Rdb")
#' ## number of downstream/outflow links.
#' nOut<-to$end-to$start
#' nOut[which(to$start>0)] <-nOut[which(to$start>0)] +1
#' table(nOut)
#' for (ii in which(nOut >1)) { print(ii); print(CheckConn(ii),up=FALSE) }
#' comIdWhOutGt1 <-
#'   data.frame(nOut = nOut[which(nOut > 1)],
#'              comId = reInd$comId[which(nOut > 1)] )
#' comIdWhOutGt1 <- comIdWhOutGt1[order(comIdWhOutGt1$nOut),]
#' write.table(comIdWhOutGt1, row.names=FALSE,
#'             file='~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink4.comIdWhOutGt1.txt')
#' 
#' load("/home/jamesmcc/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink_2015_07_31.reInd.Rdb")
#' }  #dontrun
#' @keywords manip
#' @concept dataMgmt nudging
#' @family networkExpression nudging
#' # @export
CheckConn <- function(ind, upstream=TRUE, printInds=FALSE) {
  newWay <- if(upstream) {
    if(from$start[ind] >0) from$from[from$start[ind]:from$end[ind]] else 0
  } else {
    if(to$start[ind] >0)   to$to[to$start[ind]:to$start[end]] else integer(0)
  }

  nhdWay <- if(upstream) {
    union(which(reInd$to==ind),  reInd$from[ind])
  } else {
    union(which(reInd$from==ind),reInd$to[ind]  )
  }
 
  test <- all(newWay %in% nhdWay) & all(nhdWay %in% newWay)
  if(!test) stop(paste0('test failed at index: ',ind))
  if(printInds) print(nhdWay)
  test
}



if(FALSE) {
  load("/home/jamesmcc/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink_2015_07_31.reExpFrom.Rdb")
  ## number of contributing/upstream links.
  nContrib<-from$end-from$start
  nContrib[which(from$start>0)] <-nContrib[which(from$start>0)] +1
  table(nContrib)
  for (ii in which(nContrib >3)) { print(ii); print(CheckConn(ii),up=FALSE) }
  comIdWhContribGt3 <-
    data.frame(nContrib = nContrib[which(nContrib > 16)],
               comId = reInd$comId[which(nContrib > 16)] )
  comIdWhContribGt3 <- comIdWhContribGt3[order(comIdWhContribGt3$nContrib),]
  write.table(comIdWhContribGt3, row.names=FALSE,
              file='~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink4.comIdWhContribGt3.txt')
  
  load("/home/jamesmcc/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink_2015_07_31.reExpTo.Rdb")
  ## number of downstream/outflow links.
  nOut<-to$end-to$start
  nOut[which(to$start>0)] <-nOut[which(to$start>0)] +1
  table(nOut)
  for (ii in which(nOut >1)) { print(ii); print(CheckConn(ii),up=FALSE) }
  comIdWhOutGt1 <-
    data.frame(nOut = nOut[which(nOut > 1)],
               comId = reInd$comId[which(nOut > 1)] )
  comIdWhOutGt1 <- comIdWhOutGt1[order(comIdWhOutGt1$nOut),]
  write.table(comIdWhOutGt1, row.names=FALSE,
              file='~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink4.comIdWhOutGt1.txt')
  
  load("/home/jamesmcc/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink_2015_07_31.reInd.Rdb")
}


## totally incomplete... 
## a few checks on RouteLink
#routeLinkFile <- '~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink3.nc'
CheckRouteLink <- function(routeLinkFile) {
  ncid <- ncdf4::nc_open(routeLinkFile)
  link  <- ncdf4::ncvar_get(ncid,'link')
  reInd <- data.frame(from   = ncdf4::ncvar_get(ncid,'from'),
                      to     = ncdf4::ncvar_get(ncid,'to'),
                      length = ncdf4::ncvar_get(ncid,'Length')
                      )
  ncdf4::nc_close(ncid)
  length(setdiff(reInd$from, link))
  setdiff(reInd$from, link) 
  length(setdiff(reInd$to, link))
  setdiff(reInd$to, link)
  #write.table(setdiff(reInd$to, link), file='~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink3.toLinkDiff.txt', row.names=FALSE)
}
# fromFile <- "~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink.reExpFrom.Rdb"
# toFile <- "~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink.reExpTo.Rdb"
# 
# fromFile <- "~/WRF_Hydro/DOMAIN_library/BoCr_100m_1km_NHDPlus_2015_08_11/Route_Link.reExpFrom.Rdb"
# toFile <- "~/WRF_Hydro/DOMAIN_library/BoCr_100m_1km_NHDPlus_2015_08_11/Route_Link.reExpTo.Rdb"



#' Output the network reexpression to netcdf.
#' 
#' @param  toFile The Rdb (r binary file) for the downstream connectivity created by ReExpNetwork.
#' @param  fromFile The Rdb (r binary file) for the upstream connectivity created by ReExpNetwork.
#' @return Character path/file for the resulting netcdf file. 
#' 
#' @examples 
#' \dontrun{
#' reExp.nc    <- NtwKReExToNcdf(downstream.Rdb, upstream.Rdb)
#' }
#' @keywords manip
#' @concept dataMgmt nudging
#' @family networkExpression nudging
#' @export
NtwKReExToNcdf <- function(toFile, fromFile) {

  load(toFile)
  load(fromFile)

  ## need to set the missing value used by ncdf4? i think it's NA by default
  dimensionList <-
    list(  # n.b. the dimension order: z,y,x,t
         baseDim=list(name='baseDim',
           units='-', 
           values=1:length(to$start),
           unlimited=FALSE,
           create_dimvar=FALSE),
         
         downDim=list(name='downDim',
           units='-', 
           values=1:length(to$to),
           unlimited=FALSE,
           create_dimvar=FALSE),
         
         upDim=list(name='upDim',
           units='-', 
           values=1:length(from$from),
           unlimited=FALSE,
           create_dimvar=FALSE)
         )
  
  varList = list()
  varList[[1]] <- 
    list( name='upGo',
         longname='indices in the upstream direction',
         units='-',
         precision = 'integer',
         dimensionList=dimensionList[c('upDim')],
         data = from$from )
  
  varList[[2]] <- 
    list( name='upStart',
         longname='start index in upGo for a given index',
         units='-',
         precision = 'integer',
         dimensionList=dimensionList[c('baseDim')],
         data = from$start )
  
  varList[[3]] <- 
    list( name='upEnd',
         longname='end index in upGo for a given index',
         units='-',
         precision = 'integer',
         dimensionList=dimensionList[c('baseDim')],
         data = from$end )
  
  varList[[4]] <- 
    list( name='downGo',
         longname='indices in the downstream direction',
         units='-',
         precision = 'integer',
         dimensionList=dimensionList[c('downDim')],
         data = to$to )
  
  varList[[5]] <- 
    list( name='downStart',
         longname='start index in downGo for a given index',
         units='-',
         precision = 'integer',
         dimensionList=dimensionList[c('baseDim')],
         data = to$start )
  
  varList[[6]] <- 
    list( name='downEnd',
         longname='end index in downGo for a given index',
         units='-',
         precision = 'integer',
         dimensionList=dimensionList[c('baseDim')],
         data = to$end )

  globalAttList <- list()
  globalAttList[[1]] <- list(name='This File Created',
                             value=format(Sys.time(),'%Y-%m-%d_%H:%M:%S'),
                             precision="text")
  globalAttList[[2]] <- list(name='toFile',  value=toFile,   precision="text" )
  globalAttList[[3]] <- list(name='fromFile',value=fromFile, precision="text" )

  base <- strsplit(basename(toFile),'\\.')[[1]]
  base <- paste(base[-length(base)+(0:1)],collapse='.')

  dir  <- dirname(toFile)
  
  MkNcdf( varList, globalAttList=globalAttList,
         filename=paste0(dir,'/',base,'.reExp.nc'), 
         overwrite=TRUE )

    #upGo <- ncdump(paste0(dir,'/',base,'.reExp.nc'),'upGo')
  paste0(dir,'/',base,'.reExp.nc')
}



##' \code{GatherStreamInds} gathers upstream or downstream indices and distances from a given starting location.
##'
##' A non-recursive function (recusive version runs in to stack overflow problems for large domains. Both are available internally).
##' @param stream List of stream information containing either from/to and start and end positions, returned from ReExpNetwork.
##' @param start Indexed location (NOT comID) of where stream starts
##' @param linkLengths Vector of link lengths for each re-indexed reach, contained in reExp.nc.
##' @return List containing indices and accumulated distance from start
##' @examples 
##' \dontrun{
##'      PlotRouteLink <- 
##'          VisualizeRouteLink(file='~/WRF_Hydro/DOMAIN_library/Boulder_Creek_100m_1km_2sqkm_full_2015_09_03/Route_Link.nc')
##'      PlotRouteLink()
##'      PlotRouteLink(comId=TRUE)
##'      PlotRouteLink(indices=TRUE)
##'      load('~/WRF_Hydro/DOMAIN_library/Boulder_Creek_100m_1km_2sqkm_full_2015_09_03/Route_Link.reInd.Rdb')
##'      load('~/WRF_Hydro/DOMAIN_library/Boulder_Creek_100m_1km_2sqkm_full_2015_09_03/Route_Link.reExpFrom.Rdb')
##'      upstreamInds <- GatherStreamInds(from, 379, linkLengths=reInd$length)
##'      load('~/WRF_Hydro/DOMAIN_library/Boulder_Creek_100m_1km_2sqkm_full_2015_09_03/Route_Link.reExpTo.Rdb')
##'      downstreamInds <- GatherStreamInds(to, 91, length=reInd$length)
##' }
##' @keywords manip
##' @concept nudging dataMgmt
##' @family networkExpression
##' @export
GatherStreamInds <- function(stream, start, linkLengths) {
  ## use the plural here: indDists
  indDists <- GatherStreamIndsNRInner(stream, start, linkLengths) 
  while(any(indDists$tip>1)){
    tipInds <- indDists$ind[which(indDists$tip>1)]
      for(ss in tipInds)
        indDists <- GatherStreamIndsNRInner(stream, ss, linkLengths, indDist=indDists) 
  }
  indDists
}


## Get the neighboring (next up or down) stream gages
GatherNeighborStreamGages <- function(stream, start, linkLengths, gageIndices) {
  ## use the plural here: indDists
  indDists <- GatherStreamIndsNRInner(stream, start, linkLengths) 
  while(any(indDists$tip>1)){
    ## once you arrive at at tip which is a gage, stop there.
    tipInds <- indDists$ind[which(indDists$tip>1)]
    whGages <- which(indDists$ind %in% gageIndices)
    if(length(whGages)) {
      indDists$tip[whGages] <- 0
      tipInds <- indDists$ind[which(indDists$tip>1)]
      if(!length(tipInds)) break
    }
    for(ss in tipInds)
      indDists <- GatherStreamIndsNRInner(stream, ss, linkLengths, indDist=indDists) 
  }
  whGages <- which(indDists$ind %in% gageIndices)
  if(!length(whGages)) return(NULL)
  c(plyr::llply(indDists[c('ind','dist','tip')], '[', whGages) ,
    indDists['startInd'] )
}


  
GatherStreamIndsNRInner <- function(stream, start, linkLengths=0,
                                    indDist = list(ind = c(), dist = c(),
                                                   tip=c(), startInd=NA)) {
  ## downstream only has one tip
  ## upstream can have multiple tips
  ## the "tip" has three states
  ## 0: not at tip
  ## 1: an end tip
  ## 2: a temporary tip (still solving)
  indDist$tip[which(indDist$ind==start)] <- 1
  anyStream <- stream$start[start] > 0
  if (!anyStream) return(indDist)

  indDist$tip[which(indDist$ind==start)] <- 0
  
  whGo <- which(!(names(stream) %in% c('start','end')))
  if(!(names(stream)[whGo] %in% c('to','from','go'))) 
    warning('Something wrong with stream variable.', immediate.=TRUE)
  names(stream)[whGo] <- 'go'
  streamInds <- stream$go[stream$start[start]:stream$end[start]]
  
  for (ss in streamInds) {
    if(ss==0) next
    if (length(indDist$dist) == 0) {
      indDist$startInd=start
      indDist$ind  <- ss
      indDist$tip  <- 2
      startDist = 0
      indDist$dist <- startDist + linkLengths[ss]/2 + linkLengths[start]/2
    } else {
      indDist$ind <- append(indDist$ind, ss)
      indDist$tip <- append(indDist$tip, 2 )
      startDist <- indDist$dist[which(indDist$ind == start)]
      if(!length(startDist)) startDist=0
      if (length(startDist) > 1)
        warning('Problem with input topology', immediate. = TRUE)
      indDist$dist <- append(indDist$dist, startDist + linkLengths[ss]/2 + linkLengths[start]/2)
    }
#    coll <<- c(coll,tail(indDist$ind,1))
#    indDist <- GatherStreamInds(stream, start=ss, length=length, indDist=indDist)
  }

 
  indDist
}



## Deprecated.
## testing non-recursive collection,
## see hydro:/home/jamesmcc/WRF_Hydro/CONUS/gageRelationship.R  
GatherStreamIndsRecursive <- function(stream, start, linkLengths=0,
                             indDist = list(ind = c(), dist = c())) {
  anyStream <- stream$start[start] > 0
  if (!anyStream) return(indDist)
  
  whGo <- which(!(names(stream) %in% c('start','end')))
  if(!(names(stream)[whGo] %in% c('to','from','go'))) 
    warning('Something wrong with stream variable.', immediate.=TRUE)
  names(stream)[whGo] <- 'go'
  streamInds <- stream$go[stream$start[start]:stream$end[start]]
  
  for (ss in streamInds) {
    if(ss==0) next
    if (length(indDist$dist) == 0) {
      indDist$ind  <- ss
      startDist = 0
      indDist$dist <- startDist + linkLengths[ss]/2 + linkLengths[start]/2
    } else {
      indDist$ind  <- append(indDist$ind,  ss)
      startDist <- indDist$dist[which(indDist$ind == start)]
      if(!length(startDist)) startDist=0
      if (length(startDist) > 1)
        warning('Problem with input topology', immediate. = TRUE)
      indDist$dist <- append(indDist$dist, startDist + linkLengths[ss]/2 + linkLengths[start]/2)
    }
#    coll <<- c(coll,tail(indDist$ind,1))
    indDist <- GatherStreamIndsRecursive(stream, start=ss, linkLengths=linkLengths,
                                         indDist=indDist)
  }
  indDist$startInd <- start
  indDist
}



#========================================================
##'Visualize upstream or downstream links determined from GatherStreamInds
##'
##' @param indDist List containing indies and accumulated distance from start, obtained from GatherStreamInds
##' @param ncFile Route Link file read in/initially processed with VisualizeRouteLink()
##' @param comIds Logical, show the comIds or the link indices in the Route_Link.nc file.
##' @param downstreamReExp Character, eliminate searching by providing the re-expressed network file
##' @param ... arguments to the function returned by VisualizeRouteLink.
##' @return Map of Route Links with selected upstream/downstream links highlighted in red, starting location in black
##' @examples
##' \dontrun{
##'  ## see example for GatherStream
##'  file <- '~/WRF_Hydro/DOMAIN_library/Boulder_Creek_100m_1km_2sqkm_full_2015_09_03/Route_Link.nc'
##'  VisualizeSubsetStream(upstreamInds, file)
##'  VisualizeSubsetStream(upstreamInds, file, com=FALSE, zoom=10, textColor='purple')
##'  VisualizeSubsetStream(downstreamInds, file, com=TRUE, zoom=10)
##'  VisualizeSubsetStream(downstreamInds, file, com=FALSE, zoom=10, linkColor='lightblue', maptype='satellite')
##' }
##' @keywords hplot
##' @concept nudging plot
##' @family networkExpression nudging
##' @export
VisualizeSubsetStream <- function(indDist,ncFile, comIds=TRUE, downstreamReExp='', ...){
  plotData <- VisualizeRouteLink(ncFile, downstreamReExp=downstreamReExp)(doPlot=FALSE, ...)
  plotData$rl$ind <- 1:nrow(plotData$rl)
  selectLinks <- plotData$rl[(plotData$rl$ind %in% indDist$ind),]
  startLink <- plotData$rl[(plotData$rl$ind %in% indDist$startInd),]
  
  ggObj <-
    plotData$ggObj + 
    ggplot2::geom_segment(data=selectLinks,ggplot2::aes(x=lon,y=lat,xend=to_lon,yend=to_lat),color="red1")

  if(!is.na(comIds)) {
    if(comIds) { # convert to comId by default
      ggObj <- ggObj + 
        ggplot2::geom_text(data=selectLinks,ggplot2::aes(x=lon/2+to_lon/2,y=lat/2+to_lat/2,label=as.character(link)),color="darkred") +
          ggplot2::geom_text(data=startLink,ggplot2::aes(x=lon/2+to_lon/2,y=lat/2+to_lat/2,label=as.character(link))) + 
            ggplot2::ggtitle("Link comIds")
    } else {
      ggObj <- ggObj + 
        ggplot2::geom_text(data=selectLinks,ggplot2::aes(x=lon/2+to_lon/2,y=lat/2+to_lat/2,label=as.character(ind)),color="darkred") +
          ggplot2::geom_text(data=startLink,ggplot2::aes(x=lon/2+to_lon/2,y=lat/2+to_lat/2,label=as.character(ind))) + 
            ggplot2::ggtitle("Link indices")
    }
  }
  
  print(ggObj)
  invisible(ggObj)
} 



## dummy check
## FRNG
#load("/d6/jamesmcc/WRF_Hydro/FRNG_NHD/4DAY/NHDPLUS/DOMAIN/Route_Link_2.reExpFrom.Rdb")
#rl <- as.data.frame(GetNcdfFile("/d6/jamesmcc/WRF_Hydro/FRNG_NHD/4DAY/NHDPLUS/DOMAIN/Route_Link_2.nc", quiet=TRUE))
#checkReExpFirstOrd(from, rl)

## Boulder_Creek
#load("/d6/jamesmcc/WRF_Hydro/Boulder_Creek_NHD/DOMAIN/Route_Link_NHD_2015_09_29.reExpFrom.Rdb")
#rl <- as.data.frame(GetNcdfFile("/d6/jamesmcc/WRF_Hydro/Boulder_Creek_NHD/DOMAIN/Route_Link_NHD_2015_09_29.nc", quiet=TRUE))
#checkReExpFirstOrd(from, rl)

checkReExpFirstOrd <- function(from, rl) {
  cat("Orders of links with no upstream links\n")
  whFrom1 <- which(from$start==0)
  print(table(rl$order[whFrom1]))
  
  cat("O2+ links with no upstream links\n")
  whGtO1 <- which(rl$order[whFrom1] > 1)
  print(rl[whFrom1[whGtO1],c('link','order','to')])

  cat('any of these links in "to"?\n')
  print(any(rl[whFrom1[whGtO1],c('link')] %in% rl$to))
  invisible(TRUE)
}



# Reformat a CHANNEL_CONNECTIVITY.nc file as a Route_Link.nc file
# The idea here is to simply reexpress the CHANNEL_CONNECTIVITY.nc file as
# a pseudo - Route_Link.nc file with only the basic information, so that it can
# be passed to all the existing network processing routines as any Route_Link.nc
ChanConnToRouteLink <- function(chanConnFile, fullDomFile, overwrite=TRUE) {

  ncid <- ncdf4::nc_open(chanConnFile)
  reInd <- data.frame(from   = ncdf4::ncvar_get(ncid,'FROM_NODE'),
                      to     = ncdf4::ncvar_get(ncid,'TO_NODE'),
                      Length = ncdf4::ncvar_get(ncid,'CHANLEN'),
                      lon    = ncdf4::ncvar_get(ncid,'LONGITUDE'),
                      lat    = ncdf4::ncvar_get(ncid,'LATITUDE')
                      )
  ccXi <- ncdf4::ncvar_get(ncid,'CHANXI')
  ccYj <- ncdf4::ncvar_get(ncid,'CHANYJ')
  ncdf4::nc_close(ncid)
  
  reInd <- reInd[sort(reInd$from, index.return=TRUE)$ix,]
  reInd$link <- 1:nrow(reInd) 
  reInd$from <- 0
  reInd$to[which(is.na(reInd$to))] <- 0

  ## Prep the above variables for ouput
  dimList <- list(linkDim=list(name='linkDim',values=reInd$link,
                  units='-', unlimited=FALSE,
                  create_dimvar=FALSE))
  varList <- list() 
  for(ll in 1:length(names(reInd))) {
    varList[[ll]] <- list(name=names(reInd)[ll],
                          longname=paste0(names(reInd)[ll],' from CHANNEL_CONNECTIVITY.nc'),
                          units='-',
                          precision = typeof(reInd[,ll]),
                          missing = -9999,
                          dimensionList = dimList,
                          data = reInd[,ll] )
  }
  
  ## bring in frxst_pts and make them "gages"
  ncid <- ncdf4::nc_open(fullDomFile)
  ## holy moley. see how I figured this out below
  frxst <- FlipUD(ncdf4::ncvar_get(ncid,'frxst_pts'))
  fLat <- FlipUD(ncdf4::ncvar_get(ncid,'LATITUDE'))
  fLon <- FlipUD(ncdf4::ncvar_get(ncid,'LONGITUDE'))
#  frxst <- FlipUD(RotateCw(RotateCw(ncdf4::ncvar_get(ncid,'frxst_pts'))))
#  fLat <- FlipUD(RotateCw(RotateCw(ncdf4::ncvar_get(ncid,'LATITUDE'))))
#  fLon <- FlipUD(RotateCw(RotateCw(ncdf4::ncvar_get(ncid,'LONGITUDE'))))
  ## Changrid useful to help eliminate head scratching
#  chanGrid <- FlipUD(RotateCw(RotateCw(ncdf4::ncvar_get(ncid,'CHANNELGRID'))))
   chanGrid <- FlipUD(ncdf4::ncvar_get(ncid,'CHANNELGRID'))
  ncdf4::nc_close(ncid)

  frxstValues <- frxst[frxst >= 0]
  for(ff in frxstValues) {#
    cat('fval:',ff,'\n')
    whF <- which(frxst==ff)
    whReInd <- which(reInd$lon == fLon[whF] & reInd$lat == fLat[whF])
#    if(length(whReInd)>1) next
    cat('whreind:',whReInd,'\n')
    cat('xi,yj:',ccXi[whReInd], ccYj[whReInd],'\n')
  }

stop()
  
  ## Have a known N-S flip, but also a double rotation... !
  ## There are assumptions from ARC, FORTRAN, and R all combining for these gymnastics
  whCC <- cbind(ccXi, ccYj)
  ## In this line keep flipping/rotating untill the following give TRUE
  whCg0 <- which(chanGrid == 0 , arr.ind=TRUE)
  for(i in 1:100) print(all(whCg0[which(whCg0[,2]==i)] %in% whCC[which(whCC[,2]==i)]))
  ## ultimately, you want this setdiff to be the empty set
  notOnChgrid <- setdiff(unique(as.vector(frxst)), unique(frxst[cbind(ccXi, ccYj)]))
  if(length(notOnChgrid)){
    warning(paste0('Apparently the following points are not on the CHANGRID in fulldom:',
                   paste(notOnChgrid, collapse=', ')))
  }

  length(which(chanGrid > -1 & frxst >= 0))  # make sure these are still lined up... 
  whFrxstValuesXy <- which(frxst >= 0, arr.ind=TRUE) 
  for(xy in 1:nrow(whFrxstValuesXy)) print(which(ccXi==whFrxstValuesXy[xy,1] & ccYj==whFrxstValuesXy[xy,2]))
  head(whFrxstValuesXy)
  head(whCC)
  
  frxst2 <- frxst[cbind(ccXi,ccYj)]
  reInd$gages <- formatC('',width=15)
  reInd$gages[which(frxst2 >= 0)] <- formatC(frxst2[which(frxst2 >= 0)],width=15)

#  hri <- head(reInd[which(frxst2 >= 0),])
#  whG <- which(frxst %in% as.numeric(hri$gages))
#  fLat[whG]
#  fLon[whG]
  
  dimList[['charDim']] <- list(name='IDLength', values=1:15,
                               units='', unlimited=FALSE, create_dimvar=FALSE)

  varList[[ll+1]] <- list(name='gages',
                          longname='gages from CHANNEL_CONNECTIVITY.nc',
                          units='',
                          precision = 'char',
                          #missing = formatC('',width=15),
                          dimensionList = list(dimList$charDim, dimList$linkDim),
                          data = reInd$gages )
  
  outPath <- dirname(chanConnFile)
  outBase <- paste0('Route_Link.',basename(chanConnFile))
  outFull <- paste0(outPath,'/',outBase)
  dum <- MkNcdf(varList, filename=outFull, overwrite=overwrite)
  ncdump(outFull)
  invisible(outFull)

}
