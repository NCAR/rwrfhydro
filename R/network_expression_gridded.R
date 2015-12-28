
##==================================================================================================
#' ReExpress *gridded* stream networks indexed network traversal.
#' 
#' \code{ReExpNetwork} re-expresses topological relationships between two variables, 
#  [from, to] (used by WRF Hydro gridded channel model) into separate lists for index based
#' up- and down- stream traversal, depending on the upstream argument. 
#' 
#' @param upstream Integer, the index of the upstream chan grid from the index.
#' @param downstream Integer, the index of the downstream chan grid from the index. 
#' @param parallel Logical use a registered backend for plyr?
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
ReExpressGridChanConn <- function(upstream, downstream, parallel=FALSE) {
  
  FindUpstream   <- function(down) list(upstream[which(downstream==down)])
  FindDownstream <- function(up) list(downstream[which(upstream==up)])
  
  downstream[which(is.na(downstream))] <- 0
  
  ## re express 
  for(ff in c(FindUpstream,FindDownstream)) {
    connList <- plyr::llply( upstream, ff, 
                             .parallel=parallel )
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