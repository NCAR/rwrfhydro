#' @export
GetActiveHucData <- function(huc, parameterCd=c('00060','00065')) {
  dataRetrieval::readNWISdata(service='iv', huc=huc, siteStatus='active', 
                              parameterCd=parameterCd)
                              #period='PT4H')
}
#system.time(for(i in 1:10) dum<-GetActiveHucData('10',parameterCd = '00060'))
##averaged over 10 pulls, pulling only 00060 using "period='PT4H"' took 
## huc06: 130% longer, huc14: 146% longer, huc10: 221% longer. 

#' @export
PlotCollectStats <- function(path='/home/jamesmcc/usgsStreamData/realTimeData/', 
                             pattern='.huc.*RData$') {
  
  if(foreach::getDoParWorkers() <= 1)
    cat('PlotCollectStats: Note that this operation can be parallelized via\n doMC::registerDoMC(nCores)\n')

  allFiles <- list.files(path=path, pattern=pattern, full.names=TRUE)
    
  CollectPullStats <- function(file){
    load(file)
    data.frame(huc02= strsplit(file,'\\.')[[1]][2],
              time.sec=pullTime['elapsed'], 
              size.MB=as.numeric(strsplit(obSize[1], '\\ ')[[1]][1]),
              nStns=length(unique(data$site_no)), 
              when=strsplit(file,'/|\\.')[[1]][7] )
  }
  
  pullDf<-plyr::ldply(allFiles,CollectPullStats, .parallel=foreach::getDoParWorkers() > 1)
  
  mPullDf <- reshape2::melt(pullDf, id=c('time.sec','when','huc02'))
  
  print(
  ggplot2::ggplot(mPullDf, ggplot2::aes(x=time.sec, y=value, color=huc02)) + 
    ggplot2::geom_text(size=3, ggplot2::aes(label=substr(huc02,4,5))) + 
    ggplot2::facet_wrap(~variable,ncol=1, scales='free_y') +
    ggplot2::theme_bw(base_size=24) 
  )
}


#' @export
PlotLatency <- function(path='/home/jamesmcc/usgsStreamData/realTimeData/', 
                        pattern='.huc.*RData$') {
  
  if(foreach::getDoParWorkers() <= 1)
    cat('PlotLatency: Note that this operation can be parallelized via\n doMC::registerDoMC(nCores)\n')
  allFiles <- list.files(path=path, pattern=pattern, full.names=TRUE)
  
  CollectLatency <- function(file) {
    load(file)
    data.frame(file=file,
              deltaT.min = as.numeric(data$dateTime - attr(data, 'queryTime')),
              stringsAsFactors=FALSE)
  }
  
  latentDf <- plyr::ldply(allFiles, CollectLatency, .parallel=foreach::getDoParWorkers() > 1)
  
  nhrs <- 5
  nrowLatentDf <- nrow(latentDf)
  
  thePlot <-
  ggplot2::ggplot(subset(latentDf, abs(deltaT.min) < nhrs*60 & deltaT.min < 0), 
                  ggplot2::aes(x=deltaT.min, nrowLatentDf=nrowLatentDf, y=..count../nrowLatentDf), 
                  environment=environment()) + 
    ggplot2::geom_histogram(fill='lightblue', alpha=.5, binwidth=15, color='orange') + 
    ggplot2::scale_x_continuous(breaks=15*( (-1*(nhrs*4)):0 ), 
                                name='Time of obs relative to retrieval time (minutes)') +
    ggplot2::scale_y_continuous(name='Fraction of all data in bin', breaks=.01*2*(0:13)) +
    ggplot2::theme_bw(base_size=24) +
    ggplot2::theme(panel.grid.major=ggplot2::element_line(size=.5))
  
  print(thePlot)
  
  cdfDelta <- ecdf(latentDf$deltaT.min)
  cumProb  <- cdfDelta(seq(-135,0, by=15))
  names(cumProb) <- seq(-135,0, by=15)
  cat('Cumulative probabilities (minutes in the past):\n')
  print(cumProb)
  cat('\n')
  
  binProb  <- diff(cdfDelta(seq(-135,0, by=15)))
  names(binProb) <- paste0(seq(-135,-15,by=15), ':', seq(-120,0,by=15))
  print('Bin probabilities (minutes in the past):\n')
  print(binProb)
  cat('\n')

  realOld <- cdfDelta(-24*(3*(5:1)))
  names(realOld) <- -3*(5:1) ## days
  cat("Probability of 'really old' data, (days in the past):\n")
  print(realOld)
  cat('\n')
  ## are stations only reported once per pull?
  CollectStnReps <- function(file){
    load(file)
    data.frame(file=file,
              maxStnReps = max(table(data$site_no)))
  }

  repsDf <- plyr::ldply(allFiles, CollectStnReps, .parallel=foreach::getDoParWorkers() > 1)

}

#' @export
PlotNStnSlice <- function(pattern='*.nc', 
                          outPath = '~/usgsStreamData/timeSliceData/') {
  
  if(foreach::getDoParWorkers() <= 1)
    cat('PlotNStnSlice: Note that this operation can be parallelized via\n doMC::registerDoMC(nCores)\n')
  sliceFiles <- list.files(pattern=pattern, 
                              path=outPath, 
                              full.names=TRUE)
  
  nStn <- 
    plyr::ldply(NamedList(sliceFiles), 
                function(ff) { nc <- ncdump(ff, quiet=TRUE)
                              data.frame(nStn=nc$dim$stationId$len,
                                         time=as.POSIXct('1970-01-01 00:00:00',tz='UTC') + nc$dim$time$vals,
                                         nUniqueStn = length(unique(nc$dim$stationId$vals)) )},
                .parallel=foreach::getDoParWorkers() > 1)

  thePlot <- 
    ggplot2::ggplot(nStn, ggplot2::aes(x=time,y=nStn)) + 
      ggplot2::geom_point(color='red') + 
      ggplot2::geom_vline(xintercept=as.numeric(lubridate::with_tz(Sys.time(),'UTC')), color='cyan')+
      ggplot2::theme_bw(base_size=24)        
      ggplot2::scale_x_datetime(name='Date')
  
  print(thePlot)
  
  
  invisible(list(nStnDf=nStn, thePlot=thePlot))
  
}

if(FALSE){

  whMost<-which.max(slice$nStnDf$nUniqueStn)
  mostFile<-slice$nStnDf$.id[whMost]
  mostStn<-ncdump(mostFile,'stationId')
  length(mostStn);length(unique(mostStn))
theTable<-table(mostStn)
theTable[which(theTable>1)]

}