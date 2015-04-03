
## I have the reference data exported to a csv. 
## Import that csv and store as package data

#' Gages-II attributes
#' 
#' These are the attributes taken from the USGS Gages-II data set. \cr
#' \link[http://water.usgs.gov/GIS/metadata/usgswrd/XML/gagesII_Sept2011.xml]{http://water.usgs.gov/GIS/metadata/usgswrd/XML/gagesII_Sept2011.xml}
#' 
#' @section Usage:
#'  \code{gages2Attr} \cr
#'  \code{data(gages2Attr)}
#'  
#' @format
#' data.frame:  9322 obs. of  16 variables:
#' \describe{
#'  \item{X}{}
#'  \item{Y}{}
#'  \item{STAID}{}
#'  \item{STANAME}{}
#'  \item{CLASS}{}
#'  \item{AGGECOREGI}{}
#'  \item{DRAIN_SQKM}{}
#'  \item{HUC02}{}
#'  \item{LAT_GAGE}{}
#'  \item{LNG_GAGE}{}
#'  \item{STATE}{}
#'  \item{HCDN_2009}{}
#'  \item{ACTIVE09}{}
#'  \item{FLYRS1900}{}
#'  \item{FLYRS1950}{}
#'  \item{FLYRS1990}{}
#' }
#' 
#' @section Citation:
#' Originator: James Falcone \cr
#' Publication_Date: 20111012 \cr
#' Title: GAGES-II: Geospatial Attributes of Gages for Evaluating Streamflow \cr
#' Geospatial_Data_Presentation_Form: vector digital data
#' Publication_Information: \cr
#' Publication_Place: Reston, Virginia \cr
#' Publisher: U.S. Geological Survey \cr
#' Online_Linkage: \link[http://water.usgs.gov/lookup/getspatial?gagesII_Sept2011]{http://water.usgs.gov/lookup/getspatial?gagesII_Sept2011} \cr
#' @concept data
#' @keywords data
"gages2Attr"

#  gages2Attr <- read.csv(file=file, sep=',', header=TRUE,
#                         colClasses=c(rep('numeric',  2), 
#                                      rep('character',4),
#                                      'numeric','character',
#                                      rep('numeric',2), 
#                                      rep('character',3),
#                                      rep('numeric',3) ), 
#                         stringsAsFactors=FALSE )
#  devtools::use_data(gages2Attr)


#' @keywords internal
#' Find the HUC8 of each gages2 station and add to an extended gages 2 data set. 
#' This could be used to get more metadata at this stage, though most if it will be gotten later.
#' @export 
GetGages2NwisMeta <- function() {
  chunkSize<- 500 ## how many can i get at a time, this was as high as i went.
  chunkStart=seq(1,nrow(gages2Attr), chunkSize)
  chunkEnd  =seq(chunkSize, nrow(gages2Attr), chunkSize)
  if(length(chunkEnd)<length(chunkStart)) chunkEnd <- c(chunkEnd, nrow(gages2Attr))
  chunkDf <- data.frame(start=chunkStart, end=chunkEnd)
  GetSomeHucs <- function(chunk) {
    inds <- chunk$start : chunk$end
    meta = dataRetrieval::readNWISsite(gages2Attr$STAID[inds])
    site =            gages2Attr$STAID[inds]  
    ## manual debugger
    #meta <- dataRetrieval::readNWISsite(gages2Attr$STAID[6001:(6000+500)])
    #site=gages2Attr$STAID[6001:(6000+500)]
    
    rep <- TRUE
    while (rep) {
      sites <- meta$site_no
      wh.eq <- which( sites[-nrow(meta)] == sites[-1] )
      if(length(wh.eq)) {    
        meta <- meta[-(wh.eq),]
        rep <- TRUE
      } else rep <- FALSE
    }
    huc8=meta$huc_cd
    
    siteMiss <- setdiff(site, meta$site_no )
    for(mm in siteMiss) {
      whMiss <- which(site==mm)
      if(whMiss==1)            huc8 <- c("",huc8) 
      if(whMiss==length(site)) huc8 <- c(huc8,"") 
      if(whMiss != 1 & whMiss != length(site))
                               huc8<- c(huc8[1:(whMiss-1)],'',huc8[whMiss:length(huc8)])
    }
    
    data.frame(huc8=huc8, site=site, stringsAsFactors = FALSE )
  }
  
  plyr::ddply(chunkDf, 1, GetSomeHucs, .inform=TRUE)
}

#g2NwisMeta<-GetGages2NwisMeta()
#gages2AttrPlus <- gages2Attr
#all(g2NwisMeta$site == gages2Attr$STAID)
#gages2AttrPlus$HUC8 <- g2NwisMeta$huc8
#devtools::use_data(gages2AttrPlus) 
  
  
  
  