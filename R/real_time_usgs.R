#' @export
GetActiveHucData <- function(huc, parameterCd=c('00060','00065')) {
  dataRetrieval::readNWISdata(service='iv', huc=huc, siteStatus='active', 
                              parameterCd=parameterCd)
}

if(FALSE){

## some linux commands for reference
#jamesmcc@hydro-c1:~/usgsStreamData/realTimeData> times=`ls | cut -c1-16 | uniq`  
#jamesmcc@hydro-c1:~/usgsStreamData/realTimeData> for i in $times; do find . -type f -name "$i*" -exec du -ch {} + | grep total$; done
  
allFiles <- list.files(path='/home/jamesmcc/usgsStreamData/realTimeData/', 
                       pattern='.huc.*RData$', full.names=TRUE)
  
CollectPullStats <- function(file){
  load(file)
  data.frame(time=pullTime['elapsed'], 
             size=as.numeric(strsplit(obSize[1], '\\ ')[[1]][1]),
             nStns=length(unique(data$site_no)), 
             when=strsplit(file,'/|\\.')[[1]][7] )
}
pullDf<-plyr::ldply(allFiles,CollectPullStats)

mPullDf <- reshape2::melt(pullDf, id=c('time','when'))

library(ggplot2)
ggplot(mPullDf, aes(x=time, y=value, color=when, group=when)) + 
  geom_point(size=5) + 
  geom_line() +
  facet_wrap(~variable,ncol=1, scales='free_y') +
  theme_bw(base_size=25)


}