#' @export
GetActiveHucData <- function(huc, parameterCd=c('00060','00065')) {
  dataRetrieval::readNWISdata(service='iv', huc=huc, siteStatus='active', 
                              parameterCd=parameterCd,
                              period='PT4H')
}
#system.time(for(i in 1:10) dum<-GetActiveHucData('10',parameterCd = '00060'))
##averaged over 10 pulls, pulling only 00060 using "period='PT4H"' took 
## huc06: 130% longer, huc14: 146% longer, huc10: 221% longer. 


if(FALSE){

## some linux commands for reference
#jamesmcc@hydro-c1:~/usgsStreamData/realTimeData> times=`ls | cut -c1-16 | uniq`  
#jamesmcc@hydro-c1:~/usgsStreamData/realTimeData> for i in $times; do find . -type f -name "$i*" -exec du -ch {} + | grep total$; done
  
allFiles <- list.files(path='/home/jamesmcc/usgsStreamData/realTimeData/', 
                       pattern='.huc.*RData$', full.names=TRUE)
  
CollectPullStats <- function(file){
  load(file)
  data.frame(huc02= strsplit(file,'\\.')[[1]][2],
             time.sec=pullTime['elapsed'], 
             size.MB=as.numeric(strsplit(obSize[1], '\\ ')[[1]][1]),
             nStns=length(unique(data$site_no)), 
             when=strsplit(file,'/|\\.')[[1]][7] )
}
library(doMC)
registerDoMC(25)
pullDf<-plyr::ldply(allFiles,CollectPullStats, .parallel=TRUE)

mPullDf <- reshape2::melt(pullDf, id=c('time.sec','when','huc02'))

library(ggplot2)
ggplot(mPullDf, aes(x=time.sec, y=value, color=huc02)) + 
  geom_text(size=3, aes(label=substr(huc02,4,5))) + 
  #geom_line() +
  facet_wrap(~variable,ncol=1, scales='free_y') +
  theme_bw(base_size=24) 
  

CollectLatency <- function(file){
  load(file)
  data.frame(file=file,
             deltaT.min = as.numeric(data$dateTime - attr(data, 'queryTime')),
             stringsAsFactors=FALSE)
}

latentDf <- plyr::ldply(allFiles, CollectLatency, .parallel=TRUE)

nhrs <- 5
nrow(latentDf)
library(ggplot2)
ggplot(subset(latentDf, abs(deltaT.min) < nhrs*60 & deltaT.min < 0), 
       aes(x=deltaT.min, y=..count../566558 )) + 
  geom_histogram(fill='lightblue', alpha=.5, binwidth=15, color='orange') + 
  scale_x_continuous(breaks=15*( (-1*(nhrs*4)):0 ), 
                     name='Time of obs relative to retrieval time (minutes)') +
  scale_y_continuous(name='Fraction of all data in bin', breaks=.01*2*(0:13)) +
  theme_bw()+
  theme(panel.grid.major=element_line(size=.5))

cdfDelta <- ecdf(latentDf$deltaT.min)
cumProb  <- cdfDelta(seq(-135,0, by=15))
names(cumProb) <- seq(-135,0, by=15)
cumProb
## yea! forecast observations are in the data set!
binProb  <- diff(cdfDelta(seq(-135,0, by=15)))
names(binProb) <- paste0(seq(-135,-15,by=15), ':', seq(-120,0,by=15))
binProb

realOld <- cdfDelta(-24*(3*(5:1)))
names(realOld) <- -3*(5:1) ## days
realOld

## are stations only reported once per pull?
CollectStnReps <- function(file){
  load(file)
  data.frame(file=file,
             maxStnReps = max(table(data$site_no)))
}

repsDf <- plyr::ldply(allFiles, CollectStnReps, .parallel=TRUE)

}
