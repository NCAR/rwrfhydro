#' ---
#' title: "Evaluate water budget partitioning with rwrfhydro"
#' author: "Aubrey Dugger"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Water Budget}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   \usepackage[utf8]{inputenc}
#' ---
#' 
#' # Background
#' We are using WRF-Hydro to predict streamflow for Fourmile Creek at the Orodell USGS gage for the 2013 snowmelt period. We ran WRF-Hydro with NoahMP as the LSM for a 3-year spinup period and then did a daily run for 5 months starting March 1, 2013. We want to evaluate the predicted water budget partitioning over this snowmelt period.
#' 
#' Load the rwrfhydro package. 
## ------------------------------------------------------------------------
library("rwrfhydro")

#' 
#' Set a data path to the Fourmile Creek test case.
## ------------------------------------------------------------------------
dataPath <- '~/wrfHydroTestCases/Fourmile_Creek_testcase_v2.0'

#' 
#' 
#' # Import modelled datasets
#' 
#' Calculate basin-averaged LSM water fluxes. The LSM was run at 1km resolution and the high-res hydro grid was 100m resolution, so our aggregation factor is 10. We only have 1 basin in our model domain, and our basin ID is 1. To speed up the netcdf file reads, we are going to use R's multi-core capability and run this summary over 3 cores.
## ------------------------------------------------------------------------
library(doMC)
registerDoMC(3)
modLdasoutWb1h.allrt.fc <- ReadLdasoutWb(paste0(dataPath, '/run.FullRouting'), 
                                         paste0(dataPath, '/DOMAIN/Fulldom_hires_hydrofile.Fourmile100m.nc'), 
                                         mskvar="basn_msk", basid=1, aggfact=10, ncores=3)

#' 
#' Calculate basin-averaged routing water fluxes. 
## ------------------------------------------------------------------------
modRtout1h.allrt.fc <- ReadRtout(paste0(dataPath, '/run.FullRouting'), 
                                 paste0(dataPath, '/DOMAIN/Fulldom_hires_hydrofile.Fourmile100m.nc'), 
                                 mskvar="basn_msk", basid=1, ncores=3)

#' 
#' Import groundwater outflow model output.
## ------------------------------------------------------------------------
modGwout.allrt.fc <- ReadGwOut(paste0(dataPath, '/run.FullRouting/GW_outflow.txt'))

#' 
#' 
#' # Evaluate the predicted water budget 
#' 
#' Calculate a water budget for the basin. Our modelled soil depths were 100, 300, 600, and 1000 mm, which match the defaults (therefore we do not need to specify). This model run had all routing options (subsurface, overland, groundwater, and channel) turned on, so we are providing rtout and gwout dataframes and switching sfcrt to TRUE. Our basin area is 63.1 km<sup>2</sup>.
## ------------------------------------------------------------------------
wb.allrt.fc <- CalcNoahmpWatBudg(modLdasoutWb1h.allrt.fc, rtoutDf=modRtout1h.allrt.fc, 
                                 gwoutDf=modGwout.allrt.fc, sfcrt=TRUE, basarea=63.1)

#' 
#' Take a look at the results. All output values are in mm.
## ----, results='hide'----------------------------------------------------
wb.allrt.fc

#' 
## ----, results = "asis", echo=FALSE--------------------------------------
suppressPackageStartupMessages(library(pander))
pander::pandoc.table(wb.allrt.fc, justify="left", caption="")

#' 
#' Plot the water budget as a pie chart.
## ----watbudgPie, fig.width = 8, fig.height = 8, out.width='600', out.height='600'----
PlotWatBudg(wb.allrt.fc)

#' 
#' Note that we are calculating the water budget for March-July only, so we see significant change in storage from a melting snowpack. Calculating the water budget for a full water year would likely result in a much smaller change in storage and the other water flux components would sum to close to 100%.
#' 
#' Plot the water budget as a bar chart.
## ----watbudgBar, fig.width = 8, fig.height = 8, out.width='600', out.height='600'----
PlotWatBudg(wb.allrt.fc, "bar")

