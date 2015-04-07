#' @export
GetActiveHucData <- function(huc, parameterCd=c('00060','00065')) {
  dataRetrieval::readNWISdata(service='iv', huc=huc, siteStatus='active', 
                              parameterCd=parameterCd)
}

if(FALSE){
## want site status today

allHuc <- NamedList(formatC(1:21,width = 2, flag='0'))
require(doMC)
registerDoMC(4)
system.time(
  allActive <- plyr::llply(allHuc, GetActiveHucData, .parallel=TRUE)
)
## took 10. mins on 4 processors 


hucList <- NamedList(formatC(1:21,width=2,flag='0'))
CollectPullStats <- function(huc){
  theFile <- list.files(path='~/usgsStreamData/realTimeData/',
                        pattern=paste0('huc',huc), full.names=TRUE)
  print(theFile)
  if(!file.exists(theFile[1])) return(data.frame(time=NA,size=NA, nSTns=NA))
  load(theFile[1])
  data.frame(time=pullTime['elapsed'], 
             size=as.numeric(strsplit(obSize[1], ' ')[[1]][1]),
             nStns=length(unique(data$site_no)) )
}
pullDf<-plyr::ldply(hucList,CollectPullStats)

library(ggplot2)
ggplot(pullDf, aes(x=time, y=size, color=.id)) + geom_point() + theme_bw()
ggplot(pullDf, aes(x=time, y=nStns, color=.id)) + geom_point() + theme_bw()

hucData <- dataRetrieval::readNWISdata(service='iv', huc='10190005', 
                                       siteStatus='inactive', 
                                       parameterCd=c('00060','00065'))


hucData <- dataRetrieval::readNWISdata(service='iv', sites=c('06724970','06730160'),
                                       siteStatus='active', 
                                       parameterCd=c('00060','00065'))
}