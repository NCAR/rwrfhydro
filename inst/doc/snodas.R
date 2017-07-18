#' ---
#' title: "Collect the SNODAS product and build a local database."
#' author: "James McCreight"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Collect the SNODAS product and build a local database}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   \usepackage[utf8]{inputenc}
#' ---
#' 
#' # Background
#' Pull SNODAS data and collect into a "database" of local netcdf files. 
#' 
#' # Setup
#' Load the rwrfhydro package. 
## ---- results='hide'-----------------------------------------------------
library("rwrfhydro")

#' 
## ---- echo=FALSE---------------------------------------------------------
options(width = 120)

#' 
#' This is the path to the directory where you want your database to be built (it should exist and be empty for the purposes of this example):
## ------------------------------------------------------------------------
snodasPath <- '~/wrfHydroTestCases/snodas/' 

#' 
#' 
#' # Get some data
#' Grab a single date (both depth and SWE) and transform to netcdf. For the sake of illustration, grab three days ago.
## ---- results='hold'-----------------------------------------------------
threeDaysBack <- Sys.Date() + lubridate::period(-3,'day')
snodasGot <- GetSnodasDepthSweDate(threeDaysBack, outputDir=snodasPath, overwrite=TRUE)
if(snodasGot) {
  snodasList <- ReadSnodasDepthSweDate(threeDaysBack, outputDir=snodasPath)
  snodasNcFile <- PutSnodasNcdf(snodasList, outputDir=snodasPath)
  snodasNcFile
}

#' 
#' # Read ncdf and visualize
## ---- , fig.width = 12, fig.height = 6*1.5, out.width='700', out.height='525'----
swe <- ncdump(snodasNcFile, variable = 'SWE')
## Filter outliers.
swe[swe>quantile(as.vector(swe), .999, na.rm=TRUE)] <- NA
image(swe)

#' 
#' # Update 
#' The above can be repeated over multiple dates one might want to get, for example from three days ago until now
## ---- results='hold'-----------------------------------------------------
datesWanted <- seq(threeDaysBack, Sys.Date(), by = 'days')
datesWanted    

#' 
#' Encapsulate the above in a function with some file existance checking and return summary. 
## ----result='hold'-------------------------------------------------------
UpdateSnodas <- function(POSIXct, outPath='.') {
  ## check if we already processed this date/POSIXct, if we didnt process, we'll
  ## download again and process it. (note we could have downloaded but not processed 
  ## so it might not be efficient). 
  file <- paste0(outPath, 'SNODAS_',format(POSIXct,'%Y%m%d'),'.nc')
  processed <- file.exists(file)
  if(processed) 
    return(data.frame(date=POSIXct, snodasGot=FALSE, ncdfFile=file))
  
  snodasGot <- GetSnodasDepthSweDate(POSIXct, outputDir=outPath)
  if(snodasGot) {
    snodasList <- ReadSnodasDepthSweDate(POSIXct, outputDir=outPath)
    ncdfFile <- PutSnodasNcdf(snodasList, outputDir=outPath)
  } else ncdfFile <- ''
  data.frame(date=POSIXct, snodasGot=snodasGot, ncdfFile=ncdfFile)
}

update <- plyr::ldply(NamedList(datesWanted), UpdateSnodas, outPath=snodasPath)
update

#' 
#' Shows that the first file wasnt "got" - that's because it was already in place. 
#' 
