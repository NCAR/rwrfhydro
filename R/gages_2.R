
## I have the reference data exported to a csv. 
## Import that csv and store as package data
#' @keywords internal
#' @export
ImportGages2RefAttrCsvToPkg <- function(file,...) {
  gages2RefAttr <- read.csv(file=file, sep=',', header=TRUE,
                            colClasses=c(rep('numeric',  2), 
                                         rep('character',4),
                                         'numeric','character',
                                         rep('numeric',2), 
                                         rep('character',3),
                                         rep('numeric',3) ), 
                            stringsAsFactors=FALSE )
  devtools::use_data(gages2RefAttr,...)
}

#' keywords internal
#' @export 
GetGages2Huc8 <- function() {
  chunkSize<- 500 ## how many can i get at a time, this was as high as i went.
  chunkStart=seq(1,nrow(gages2RefAttr), chunkSize)
  chunkEnd  =seq(chunkSize, nrow(gages2RefAttr), chunkSize)
  if(length(chunkEnd)<length(chunkStart)) chunkEnd <- c(chunkEnd, nrow(gages2RefAttr))
  chunkDf <- data.frame(start=chunkStart, end=chunkEnd)
  GetSomeHucs <- function(chunk) {
    data.frame( huc8 =  GetSiteHuc(gages2RefAttr$STAID[chunk$start:chunk$end]),
                site = gages2RefAttr$STAID[chunk$start:chunk$end], stringsAsFactors = FALSE )
  }
  g2HucSiteDf <- plyr::ddply(chunkDf, 1, GetSomeHucs)
  #gages2RefAttrPlus <- gages2RefAttr
  gages2RefAttrPlus$huc8 <- g2HucSiteDf$huc8
  devtools::use_data(gages2RefAttrPlus) 
}
  
  
  
  
  