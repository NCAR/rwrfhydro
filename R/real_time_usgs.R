if(FALSE){
## want site status today
GetActiveHucData <- function(huc, parameterCd=c('00060','00065')) {
  dataRetrieval::readNWISdata(service='iv', huc=huc, siteStatus='active', 
                              parameterCd=parameterCd)
}

allHuc <- NamedList(formatC(1:21,width = 2, flag='0'))
require(doMC)
registerDoMC(4)
system.time(
  allActive <- plyr::llply(allHuc, GetActiveHucData, .parallel=TRUE)
)
## took 10. mins on 4 processors 

hucData <- dataRetrieval::readNWISdata(service='iv', huc='10190005', 
                                       siteStatus='inactive', 
                                       parameterCd=c('00060','00065'))


hucData <- dataRetrieval::readNWISdata(service='iv', sites=c('06724970','06730160'),
                                       siteStatus='active', 
                                       parameterCd=c('00060','00065'))
}