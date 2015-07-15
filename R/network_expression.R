# 
# library(rwrfhydro)
# nhd <- data.frame(
#   from  =ncdump('~/ncar/WRF_Hydro/DOMAIN_library/CONUS/RouteLink_from_bare.nc', 'from', quiet=TRUE), 
#   to    =ncdump('~/ncar/WRF_Hydro/DOMAIN_library/CONUS/RouteLink_to_bare.nc', 'to', quiet=TRUE),
#   length=ncdump('~/ncar/WRF_Hydro/DOMAIN_library/CONUS/RouteLink_Length_bare.nc','Length',quiet=TRUE)
# )
# binCon <- file("~/ncar/WRF_Hydro/DOMAIN_library/CONUS/RouteLink_link.bin", "rb")
# nhd$indexComm <- readBin(binCon, integer(), 2750000 )
# close(binCon)
# 
# ord<- order(nhd$indexComm)
# nhd<-nhd[ord,]
# nhd$reind<-1:nrow(nhd)
# ReInd <- c(0,nhd$reind)
# names(ReInd) <- c("0",as.character(nhd$indexComm))
# nhd$to <- ReInd[as.character(nhd$to)]
# nhd$from <- ReInd[as.character(nhd$from)]
# 
# save(nhd, file = '~/ncar/WRF_Hydro/DOMAIN_library/CONUS/nhdConnOrdered.Rdb')
# 
# ## --------------------
# 
# load('~/ncar/WRF_Hydro/DOMAIN_library/CONUS/nhdConnOrdered.Rdb')
# 
# ## ---------------
# load('nhdConnOrdered.Rdb')
# 
# ##              from   reind      to
# ## downstream  match  lookup
# ## downstream          match  lookup
# ##   upstream  lookup  match
# ##   upstream         lookup   match
# 
# load('nhdConnOrdered.Rdb')
# FindDownstream <- function(ind) list( union( nhd$reind[which(nhd$from == ind)], 
#                                              nhd$to[which(nhd$reind == ind)] ) )
# doMC::registerDoMC(16)
# toList <- plyr::llply( nhd$reind, FindDownstream, .parallel=TRUE)
# toLen <- plyr::laply( toList, function(ll) length(ll[[1]]) )
# whLenPos <- which(toLen > 0)
# cumSumLenPos <- cumsum(toLen[whLenPos])
# toStart <- as.integer(0*(1:length(toLen)))
# toStart[whLenPos] <- cumSumLenPos
# ## the cumulative sum dosent give the start, it gives the last in each range. fix
# whLenGt1 <- which(toLen > 1)
# cumAdj <- as.integer(0*(1:length(toLen)))
# cumAdj[whLenGt1] = cumAdj[whLenGt1] - toLen[whLenGt1] + 1
# toStart <- toStart + cumAdj
# to<-unlist(toList)
# nhdTo = list( to=to, start=toStart, length=toLen)
# save(nhdTo, file='nhdToList.Rdb')
# q()
# 
# load('nhdConnOrdered.Rdb')
# FindUpstream   <- function(ind) list( union( nhd$reind[nhd$to   == ind], 
#                                              nhd$from[nhd$reind == ind]) )
# doMC::registerDoMC(16)
# fromList <- plyr::llply( nhd$reind, FindUpstream, .parallel=TRUE)
# fromLen <- plyr::laply( fromList, function(ll) length(ll[[1]]) )
# whLenPos <- which(fromLen > 0)
# cumSumLenPos <- cumsum(fromLen[whLenPos])
# fromStart <- as.integer(0*(1:length(fromLen)))
# fromStart[whLenPos] <- cumSumLenPos
# ## the cumulative sum dosent give the start, it gives the last in each range. fix
# whLenGt1 <- which(fromLen > 1)
# cumAdj <- as.integer(0*(1:length(fromLen)))
# cumAdj[whLenGt1] = cumAdj[whLenGt1] - fromLen[whLenGt1] + 1
# fromStart <- fromStart + cumAdj
# ##
# from<-unlist(fromList)
# nhdFrom = list( from=from, start=fromStart, length=fromLen)
# save(nhdFrom, file='nhdFromList.Rdb')
# q()
# 
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
