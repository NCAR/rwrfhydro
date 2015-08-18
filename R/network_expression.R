#' ReIndex RouteLink.nc (netcdf) files for indexed network traversal.
#' 
#' \code{ReIndexRouteLink} reIndexes by order in the RouteLink file,
#' replacing ComID with this index.
#' 
#' @param routeLinkFile The netcdf routelink file to process. 
#' @return The resulting file which was written to disk, of the form "infile.reInd.nc"
#'   
#' @examples
#'  \dontrun{
#'   ReIndexRouteLink(routeLinkFile <- '~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink_2015_07_31.nc')
#'   ReIndexRouteLink(routeLinkFile <- '~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink4.nc')
#'   ReIndexRouteLink(routeLinkFile <- '~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink3.nc')
#'   ReIndexRouteLink(routeLinkFile <- '~/WRF_Hydro/DOMAIN_library/BoCr_100m_1km_NHDPlus_2015_08_11/Route_Link.nc')
#' }
#' @keywords manip
#' @concept dataMgmt
#' @family networkExpression
#' @export
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
  names(ReExp) <-  format(c(0,link), trim=TRUE, nsmall=0, scientific=0)
  ### bad: names(ReExp) <-  as.character(c(0,link))

  ##When there are NA's or issues with the following expressions,
  ## they take more than a few (ie 2) seconds
  ## it's a check that the inputs are all correct
  reInd$from  <- ReExp[as.character(reInd$from)]
  reInd$to    <- ReExp[as.character(reInd$to)]
  reInd$comId <- link

  #print(summary(reInd))
  
  base <- strsplit(basename(routeLinkFile),'\\.')[[1]][1]
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
#' @return The resulting file which was written to disk, of the form the "infile.reExpTo.nc"
#'         (downstream) or "infile.reExpFrom.nc" (upstream).
#' @examples
#' \dontrun{
#'   library(rwrfhydro)
#'   doMC::registerDoMC(16)
#'   ReExpNetwork("/home/jamesmcc/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink_2015_07_31.reInd.Rdb")
#'   ReExpNetwork("/home/jamesmcc/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink_2015_07_31.reInd.Rdb", up=FALSE)
#' }
#' @keywords manip
#' @concept dataMgmt
#' @family networkExpression
#' @export
ReExpNetwork <- function(routeLinkReInd, upstream=TRUE) {
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
  FindUpstream   <- function(ind) union(which(reInd$to   == ind), reInd$from[ind])
  FindDownstream <- function(ind) union(which(reInd$from == ind), reInd$to[ind]  )

  FindFunc <- if(upstream) { FindUpstream } else { FindDownstream }

  theList <- plyr::llply( 1:length(reInd$to), FindFunc,
                         .parallel=foreach::getDoParWorkers() > 1)
  
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
  
  base <- strsplit(basename(routeLinkReInd),'\\.')[[1]][1]
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
#' @param index
#' @param upstream Logical, check connectivity upstream (TRUE) or downstream (FALSE).
#' @return Logical, code halts at first FALSE.
#' @examples
#' \dontrun{
#'   for (ii in seq(1,2720000,1000)) { print(ii); print(CheckConn(ii)) }
#'   for (ii in seq(1,2720000,1000)) { print(ii); print(CheckConn(ii), up=FALSE) }
#'   for (ii in seq(1,2000)) { print(ii); print(CheckConn(ii)) }
#'   for (ii in seq(1,2000)) { print(ii); print(CheckConn(ii),up=FALSE) }
#' }
#' @keywords manip
#' @concept dataMgmt
#' @family networkExpression
#' @export
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
routeLinkFile <- '~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink3.nc'
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


fromFile <- "~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink.reExpFrom.Rdb"
toFile <- "~/WRF_Hydro/CONUS_IOC/DOMAIN/RouteLink.reExpTo.Rdb"

fromFile <- "~/WRF_Hydro/DOMAIN_library/BoCr_100m_1km_NHDPlus_2015_08_11/Route_Link.reExpFrom.Rdb"
toFile <- "~/WRF_Hydro/DOMAIN_library/BoCr_100m_1km_NHDPlus_2015_08_11/Route_Link.reExpTo.Rdb"


##----------------------
## Output the network reexpression
## This results in 3 files 
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

  base <- strsplit(basename(toFile),'\\.')[[1]][1]
  dir  <- dirname(toFile)
  
  MkNcdf( varList, globalAttList=globalAttList,
         filename=paste0(dir,'/',base,'.reExp.nc'), 
         overwrite=TRUE )

    #upGo <- ncdump(paste0(dir,'/',base,'.reExp.nc'),'upGo')
  paste0(dir,'/',base,'.reExp.nc')
}


##==================================================================================================
#' ReExpress *gridded* stream networks indexed network traversal.
#' 
#' \code{ReExpNetwork} re-expresses topological relationships between two variables, 
#  [from, to] (used by WRF Hydro gridded channel model) into separate lists for index based
#' up- and down- stream traversal, depending on the upstream argument. 
#' 
#' @param upstream Integer, the index of the upstream chan grid from the index.
#' @param downstream Integer, the index of the downstream chan grid from the index. 
#' @return ... have to run this to correctly describe... JLM TODO
#' @examples
#' \dontrun{
#'   library(rwrfhydro)
#'   ncdump('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/CHANNEL_CONNECTIVITY.nc')
#'   TO_NODE <- 
#'   ncdump('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/CHANNEL_CONNECTIVITY.nc',
#'          'TO_NODE', quiet=TRUE)
#'   FROM_NODE <- 
#'   ncdump('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/CHANNEL_CONNECTIVITY.nc',
#'          'FROM_NODE', quiet=TRUE)
#'   doMC::registerDoMC(4)
#'   newCon <- ReExpressGridChanConn(FROM_NODE, TO_NODE)
#'   upstream <- newCon$upstream
#'   downstream <- newCon$downstream
#'   TO_NODE[which(is.na(TO_NODE))] <- 0
#'   done <- for(ii in 1:length(TO_NODE)) CheckReGridConnUp(ii)
#'   done <- for(ii in 1:length(TO_NODE)) CheckReGridConnDown(ii)
#'  save(upstream,downstream, 
#'       file='~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/newConnectivity.Rdb')
#' }
#' @keywords manip
#' @concept dataMgmt
#' @family networkExpression
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


