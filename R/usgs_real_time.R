#' @export
GetActiveHucData <- function(huc, parameterCd=c('00060','00065')) {

  if(!try(class(dataRetrieval::importWaterML2Jlm)) == 'function') {
    devtools::install_github("mccreigh/dataRetrieval")
    warning('must restart to access the newly installed mccreigh/dataRetrieval.', immediate. = TRUE)
  }
  dataRetrieval::readNWISdata(service='iv', huc=huc, siteStatus='active', 
                              parameterCd=parameterCd)
                              #period='PT4H')
}
#system.time(for(i in 1:10) 
#dum<-GetActiveHucData('10',parameterCd = '00060')
#)
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
                          outPath = '~/usgsStreamData/timeSliceData/', lastNDays=NA) {
  
  sliceFiles <- GetTimeSliceFiles(pattern=pattern, outPath=outPath, lastNDays=lastNDays)
  
  if(foreach::getDoParWorkers() <= 1)
    cat('PlotNStnSlice: Note that this operation can be parallelized via\n doMC::registerDoMC(nCores)\n')
  
  nStn <- 
    plyr::ldply(NamedList(sliceFiles), 
                function(ff) { nc <- ncdump(ff, quiet=TRUE)
                              data.frame(nStn=nc$dim$stationId$len,
                                         time=as.POSIXct('1970-01-01 00:00:00',tz='UTC') + nc$dim$time$vals,
                                         nUniqueStn = length(unique(nc$dim$stationId$vals)) )},
                .parallel=foreach::getDoParWorkers() > 1)

  now<-lubridate::now()
  nStn$when <- ifelse(nStn$time<now, 'past', 'future')
  thePlot <- 
    ggplot2::ggplot(nStn, ggplot2::aes(x=time,y=nStn, color=when)) + 
      ggplot2::geom_point() + 
      ggplot2::geom_vline(xintercept=as.numeric(now), color='cyan')+
      ggplot2::theme_bw(base_size=24)        
      ggplot2::scale_x_datetime(name='Date')
  
  print(thePlot)
  
  
  invisible(list(nStnDf=nStn, thePlot=thePlot))
  
}

#' @export
GetTimeSliceFiles <- function(pattern='*.nc', 
                              outPath = '~/usgsStreamData/timeSliceData/', lastNDays=NA) {

  sliceFiles <- list.files(pattern=pattern, 
                           path=outPath, 
                          full.names=TRUE)

  if(!is.na(lastNDays)) {
    sliceFilesPosix <- plyr::laply(strsplit(sliceFiles,'[/.]'), function(ll) ll[length(ll)-2])
    sliceFilesPosix <- as.POSIXct(sliceFilesPosix, format='%Y-%m-%d_%H:%M:%S', tz='UTC')
    fileCutoffPosix <- lubridate::now() - lubridate::period(lastNDays,'days')
    sliceFiles <- sliceFiles[which(sliceFilesPosix >= fileCutoffPosix)]
  }
  
  sliceFiles
}


#' Get variables from timeslice files.
#' 
#' @examples
#' doMC::registerDoMC(8)
#' si <- GetTimeSliceVars( c('stationId'), lastNDays=21)
#' asi<-unique(unlist(plyr::llply(si, function(tt) tt$vars$stationId)))
#' uniqueStIds <- asi
#' save(uniqueStIds, file='~/realTimeUniqueStns.RData')
#' #want to know if data are changing more than hourly
#' qq<-GetTimeSliceVars(c('discharge', 'stationId', 'time'), lastNDays=1, pattern='2015-06-06')
#' qDf <- plyr::ldply(qq, function(ll) {df <- as.data.frame(ll$vars[c("discharge","stationId")]) ;
#'                                     df$time<-ll$vars$time; df }, .parallel=TRUE )
#' qDf2 <- plyr::ddply(qDf, plyr::.(stationId), stnTsDiff)
#' obsTab <- table(qDf2$tDiff)
#' names(obsTab) <- as.numeric(names(obsTab))/60
#' cumsum(obsTab)/sum(obsTab)
#' minDiff <- plyr::daply(qDf2, plyr::.(stationId), function(dd) min(dd$tDiff))
#' minDiffTab <- table(minDiff)
#' names(minDiffTab) <- as.numeric(names(minDiffTab))/60
#' minDiffTab
#' cumsum(minDiffTab)/sum(minDiffTab)
stnTsDiff <- function(df) {
  timeSort <- sort(df$time, index.return=TRUE)$ix
  df <- df[timeSort,]
  data.frame(qDiff=diff(df$discharge),
             tDiff=diff(df$time),
             stnId=df$stationId[1] )
}



#' @export
GetTimeSliceVars <- function(vars, pattern='*.nc', 
                             outPath = '~/usgsStreamData/timeSliceData/', 
                             lastNDays=NA) {
  
  sliceFiles <- GetTimeSliceFiles(pattern=pattern, outPath=outPath, lastNDays=lastNDays)
  
  if(foreach::getDoParWorkers() <= 1)
    cat('GetTimeSliceVars: Note that this operation can be parallelized via\n doMC::registerDoMC(nCores)\n')
  
  DoGetVar <- function(vv, nc) ncdf4::ncvar_get(nc=nc,varid=vv)
  
  DoGetFile <- function(ff){    
    nc <- ncdf4::nc_open(ff)
    vars2 <- intersect(vars, c(names(nc$dim), names(nc$var)))
    if(any( !(vars %in% vars2) ) ) {
      warning(paste("No such variables (",setdiff(vars,vars2),") in file",ff))
      if(!length(vars2)) {
        warning(paste("No variables in file",ff))
        ncdf4::nc_close(nc)  
        return(list())
      }
    }
    outList <- plyr::llply(NamedList(vars), DoGetVar, nc)
    ncdf4::nc_close(nc)
    list( vars=outList,
          time=as.POSIXct('1970-01-01 00:00:00',tz='UTC') + nc$dim$time$vals,
          file=ff )
  }

  plyr::llply(NamedList(sliceFiles), DoGetFile, .parallel=foreach::getDoParWorkers() > 1)
}

if(FALSE){

  doMC::registerDoMC(8)
  slice <- PlotNStnSlice()

  sliceData1 <- reshape2::melt(slice$nStnDf[-1], id='time')
  ggplot2::ggplot(sliceData1, ggplot2::aes(x=time,y=value, color=variable)) + ggplot2::geom_point()

  sliceData2 <- within(slice$nStnDf, {diff=nStn-nUniqueStn; diff[diff<=1]<-NA})
  ggplot2::ggplot(sliceData2, ggplot2::aes(x=time,y=diff)) + ggplot2::geom_point()
  
  
  whMost<-which.max(slice$nStnDf$nStn)
  mostFile<-slice$nStnDf$.id[whMost]
  mostStn<-ncdump(mostFile,'stationId')
  mostTime<-ncdump(mostFile,'time')
  mostQueryTime<-as.POSIXct(ncdump(mostFile,'queryTime'), 
                            origin=as.POSIXct('1970-01-01 00:00:00 UTC') )
  
  ggplot2::ggplot(data.frame(qt=mostQueryTime), ggplot2::aes(x=qt,y=qt)) + ggplot2::geom_point()
  
  length(mostStn);length(unique(mostStn))
  theTable<-table(mostStn)
  theTable[which(theTable>1)]

  
  UnionStns <- function(files) {
    
  }
  
}
