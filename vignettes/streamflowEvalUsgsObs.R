#' ---
#' title: "Collect USGS stream observations to evaluate streamflow simulation"
#' author: "Aubrey Dugger, James McCreight, Alyssa Hendricks"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Streamflow Evaluation}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   \usepackage[utf8]{inputenc}
#' ---
#' 
#' # Background
#' 
#' USGS streamflow observations are a primary source of hydrologic information and often used for
#' validation and calibration of hydrlogic models. Recently, web services have been developed
#' at [*NWIS*](http://waterdata.usgs.gov/nwis) and the [*dataRetrieval*](http://cran.r-project.org/web/packages/dataRetrieval/index.html) R package have emerged to make it easy to get USGS data into R.
#' 
#' This vignette demonstates some rwrfhydro tools to collect, store, and mainipulate USGS within a local database. This data is then used to evaluate model performance in estimating streamflow with and without overland, subsurface, and groundwater routing active. 
#' 
#' The fundamental layout of the local database is simply a directory containing:
#' 
#' * Metadata data base (metaDB): Organized hierarchically by HUC8 -> product id -> siteInfo, variableInfo, statisticInfo
#'   where the last three are information fields returned by dataRetrieval::readNWISuv. The information in this file is
#'   simply the aggregation of the metadata from all the data files also contained in the same directory.
#' * Individual data files collected by HUC8: These contain both the meta data for the HUC8 (aggregated in the metadata DB)
#'   and all the previously retrieved data for the HUC8.
#' 
#' Generally, there are two basic kinds of functions: "Get" and "Query". Get functions use dataRetrieval functions to actually go out to NWIS and "get" data and metadata. Query functions query the local database. There are exceptions to this.
#' 
#' WRF-Hydro has been used to predict streamflow for Fourmile Creek at the Orodell USGS gage for the ____ snowmelt period. WRF-Hydro was run with NoahMP as the LSM for a 3-year spinup period, (((this all needs to be changed to specifics)))
#' 
#' # Setup
#' Load the rwrfhydro package. 
## ------------------------------------------------------------------------
library("rwrfhydro")

#' 
## ---- echo=FALSE---------------------------------------------------------
options(width = 190)
library(printr)

#' Create a directory and path to where you want your database to be built:
## ------------------------------------------------------------------------
dbPath <- '~/wrfHydroTestCases/usgsDb/'

#' 
#' # Discover gage locations, get data, and save to local database.
#' 
#' Perhaps you know the lat/lon of a gage (e.g. from frxst_pts_out.txt) and you need the HUC8. The `within` argument is
#' taken to be in decimal degrees.
## ---- results='hold'-----------------------------------------------------
stnDf <- FindUsgsStns(stnLon=254.67374999999998408,
                      stnLat=40.018666670000001773,
                      within=.001)
str(stnDf)

#' 
#' Now you have the site_no or station ID number, "06727500". You could have also used `FindUsgsStns()` to reveal the gages in the HUC8, if you knew the HUC8 code. Because we organize the database by HUC8, we have a function to get HUC8 from station id. Then we get the above information for all locations in the HUC8.
## ---- results='hold'-----------------------------------------------------
huc8 <- GetSiteHuc(stnDf$site_no)
str(FindUsgsStns(huc=huc8))

#' 
#' FindUsgsStns is a wrapper on dataRetrieval::whatNWISsites which has been written to focus on instantaneous values. (It is worth noting the flexibility and generality of the underlying function.)
#' 
#' Now pull the data for this HUC8. Currently, this grabs all available products for the HUC. Note that the HUC data are organized by product code (e.g. `00060`) then by `data` and `meta` in the returned list. (Also note that this command sometimes fails on the remote end and may need to be rerun.) In `meta`, siteInfo is the meta that we use in querying the local data base in commands shown below.
#' 
#' 
## ---- echo=FALSE---------------------------------------------------------
dataName <- load(paste0("~/wrfHydroTestCases/usgsDb/",huc8,".data.RData"))
coData <- get(dataName); rm(list=c(dataName))

#' 
## ---- results='hold', eval=FALSE-----------------------------------------
## coData <- GetUsgsHucData(huc=huc8)  ## this can take a little while...
## str(coData)

#' 
## ------------------------------------------------------------------------
str(coData)

#' 
#' 
#' Now save this data to the local database. Note that this also could have been achieved by specifying the `outPath` argument to `GetUsgsHucData`.
## ---- results='hold', eval=FALSE-----------------------------------------
## coFiles <- SaveHucData(coData, outPath=dbPath)
## coFiles

#' 
#' # Query the local data
#' Now we work entirely locally, having grabbed the data of interest. For all HUC8 and products, any of the siteInfo metadata can be retrieved from the local DB. Note that the same site is repeated for multiple products.
## ------------------------------------------------------------------------
QuerySiteInfo(c('station_nm','site_no','dec_lat_va','dec_long_va'), path=dbPath)

#' 
#' Say you just want Orodell and you want your code to be readable: translate the name to the code with QuerySiteName (which translates both ways).
## ---- results='hold'-----------------------------------------------------
dataOrodell <- QuerySiteData(QuerySiteName("FOURMILE CREEK AT ORODELL, CO", path=dbPath),
                             product='00060', path=dbPath)
str(dataOrodell)

#' 
#' Now make it "pretty". The main difference here is meaningful column names and identification of variables and codes in the attributes. We have defined "prettyUsgs" as an S3 class.
## ------------------------------------------------------------------------
prettyOrodell <- PrettyUsgs(dataOrodell, metric=TRUE)
str(prettyOrodell)
class(prettyOrodell)

#' 
#' Plot the "pretty"" data.
## ----PlotPrettyOrodell, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
oroPlot <- PlotPrettyUsgs(prettyOrodell)

#' 
#' 
#' # Import modelled and observed datasets
#' 
#' Set a data path to the Fourmile Creek test case.
## ------------------------------------------------------------------------
dataPath <- '~/wrfHydroTestCases/Fourmile_Creek_testcase_v2.0/'

#' 
#' Model 1: Only channel routing turned on (hourly model run).
## ------------------------------------------------------------------------
modStrd.chrt.fc <- ReadFrxstPts(paste0(dataPath, '/run.ChannelRouting/frxst_pts_out.txt'))

#' 
#' Model 2: All WRF-Hydro routing options turned on (hourly model run).
## ------------------------------------------------------------------------
modStrd.allrt.fc <- ReadFrxstPts(paste0(dataPath, '/run.FullRouting/frxst_pts_out.txt'))

#' 
#' USGS gage observed data at 5-minute intervals. Find the nearest gage to the forecast point in the above files. (The following approach can be used when multiple forecast points are output by the model. The ddply function essentially performs a loop over unique st_id passing the corresponding subset of the data frame modStrd.chrt.fc to the function which just returns the first row's lon and lat.)
## ---- , results='asis'---------------------------------------------------
library(plyr)
fcLocation <- ddply(modStrd.chrt.fc, .(st_id), function(df) df[1,c('st_lon','st_lat')])
near <- FindUsgsStns(stnLon=c(fcLocation$st_lon), stnLat=c(fcLocation$st_lat), within=.005)
near

#' 
#' The following checks to see if the data are local and gets them if not. The '00060' product (streamflow) is returned and then made pretty, including conversion to metric. 
## ------------------------------------------------------------------------
dbPath <- '~/wrfHydroTestCases/usgsDb/'
obsStr5min.fc <- 
  PrettyUsgs(QueryHaveSite(near$site_no, path=dbPath, ret='00060', get=TRUE))
obsStr5min.fc <- plyr::rename(obsStr5min.fc, c(value='q_cms'))

#' 
#' 
#' # Plot hydrographs 
#' 
#' Compare hydrographs for the full model run.
## ----compHydrographs, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFluxCompare(obsStr5min.fc, "q_cms", modStrd.chrt.fc, "q_cms", strDf.mod2=modStrd.allrt.fc, 
     strCol.mod2="q_cms", labelObs="Observed Fourmile Creek at Orodell", 
     labelMod1="Channel Routing Only", labelMod2="All Routing", 
     title="Streamflow: Fourmile Creek")

#' 
#' Now limit the plot to the peak May flow period only. The reported stats are updated to the new time period. (Note that the R warning is innocuous because the subset adjusts for timezone, so it's ok that the timezones don't match.)
## ----compHydrographsSnow, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFluxCompare(obsStr5min.fc, "q_cms", modStrd.chrt.fc, "q_cms", strDf.mod2=modStrd.allrt.fc, 
     strCol.mod2="q_cms", labelObs="Observed Fourmile Creek at Orodell", 
     labelMod1="Channel Routing Only", labelMod2="All Routing", 
     title="Streamflow: Fourmile Creek", 
     stdate=as.POSIXct("2013-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC"), 
     enddate=as.POSIXct("2013-05-31 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC"))

#' 
#' # Review flow duration curves
#' 
#' <b>NOTE</b>: You generally evaluate flow duration curves and staistics over much longer time periods (e.g., multiple years) than what we demo here. To make the test case more portable, we are only evaluating once-a-day model output over 5 months. 
#' 
#' Calculate percent exceedances for flow duration curves. Note that we need to subset the observations to match our model run output times, and vice versa.
## ------------------------------------------------------------------------
obsStr5min.comp.fc <- CalcFdc(subset(obsStr5min.fc, POSIXct %in% c(modStrd.chrt.fc$POSIXct)))
modStrd.chrt.comp.fc <- CalcFdc(subset(modStrd.chrt.fc, POSIXct %in% c(obsStr5min.comp.fc$POSIXct)))
modStrd.allrt.comp.fc <- CalcFdc(subset(modStrd.allrt.fc, POSIXct %in% c(obsStr5min.comp.fc$POSIXct)))

#' 
#' Compare how the models are doing predicting flow values that will be exceeded 20% of the time.
#' First, calculate the fitted spline functions.
## ------------------------------------------------------------------------
fdc.obsStr5min.comp.fc <- CalcFdcSpline(obsStr5min.comp.fc)
fdc.modStrd.chrt.comp.fc <- CalcFdcSpline(modStrd.chrt.comp.fc)
fdc.modStrd.allrt.comp.fc <- CalcFdcSpline(modStrd.allrt.comp.fc)

#' 
#' Then, evaluate at the 20% exceedance percentage (high flows).
## ---- results='hold'-----------------------------------------------------
fdc.obsStr5min.comp.fc(0.2)
fdc.modStrd.chrt.comp.fc(0.2)
fdc.modStrd.allrt.comp.fc(0.2)

#' 
#' Now try the 80% exceedance percentage (low flows).
## ---- results='hold'-----------------------------------------------------
fdc.obsStr5min.comp.fc(0.8)
fdc.modStrd.chrt.comp.fc(0.8)
fdc.modStrd.allrt.comp.fc(0.8)

#' 
#' Plot flow duration curves for a more complete picture. This tool will do the date matching for us, so no need to subset the datasets.
## ----flowDurationCurves, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFdcCompare(obsStr5min.fc, "q_cms", modStrd.chrt.fc, "q_cms", strDf.mod2=modStrd.allrt.fc, 
     strCol.mod2="q_cms", labelObs="Observed Fourmile Creek", 
     labelMod1="Channel Routing Only", labelMod2="All Routing")

#' 
#' 
#' # Review model performance statistics
#' 
#' Calculate model performance stats (special formatting comands hidden). Again, this tool does the date matching for us.
## ---- results='hide'-----------------------------------------------------
CalcModPerf(modStrd.chrt.fc, obsStr5min.fc)

#' 
## ---- , results = "asis", echo=FALSE-------------------------------------
library(pander)
pander::pandoc.table(CalcModPerf(modStrd.chrt.fc, obsStr5min.fc),split.table=Inf)

#' 
## ---- results='hide'-----------------------------------------------------
CalcModPerf(modStrd.allrt.fc, obsStr5min.fc)

#' 
## ---- , results = "asis", echo=FALSE-------------------------------------
pander::pandoc.table(CalcModPerf(modStrd.allrt.fc, obsStr5min.fc), split.table=Inf)

#' 
#' Help on CalcModPerf gives details on the individual statistics returned.
## ---- results='hide'-----------------------------------------------------
help(CalcModPerf)

#' 
#' <div style="border:1px solid; border-radius: 25px; padding: 12px 25px;">
## ---- echo=FALSE---------------------------------------------------------
help(CalcModPerf)

#' </div>
#' <br><br>
#' Calculate flow duration curve performance statistics.
## ---- results='hide'-----------------------------------------------------
CalcFdcPerf(modStrd.allrt.fc, obsStr5min.fc)

#' 
## ---- , results = "asis", echo=FALSE-------------------------------------
pander::pandoc.table(CalcFdcPerf(modStrd.allrt.fc, obsStr5min.fc),split.table=Inf)

#' 
#' Again, help on CalcFdcPerf gives details on the individual statistics returned.
## ---- results='hide'-----------------------------------------------------
help(CalcFdcPerf)

#' <div style="border:1px solid; border-radius: 25px; padding: 12px 25px;">
## ---- echo=FALSE---------------------------------------------------------
help(CalcFdcPerf)

#' </div>
#' 
#' # GagesII Attributes
#' We've imported the gages-II atributes to be directly available in R.
## ------------------------------------------------------------------------
head(gages2Attr)
?gages2Attr

#' 
