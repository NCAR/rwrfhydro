#' ---
#' title: "Evaluate evapotranspiration simulation with rwrfhydro"
#' author: "Aubrey Dugger"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{ET Evaluation}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   \usepackage[utf8]{inputenc}
#' ---
#' 
#' # Background
#' We are using WRF-Hydro to predict streamflow for Fourmile Creek at the Orodell USGS gage for the 2013 snowmelt period. We ran WRF-Hydro with NoahMP as the LSM for a 3-year spinup period and then did a daily output run for 5 months starting March 1, 2013. We want to evaluate the ET fluxes at a local Ameriflux tower site.
#' 
#' Load the rwrfhydro package. 
## ------------------------------------------------------------------------
library("rwrfhydro")

#' 
#' Set a data path to the Fourmile Creek test case.
## ------------------------------------------------------------------------
dataPath <- '~/wrfHydroTestCases/Fourmile_Creek'

#' 
#' 
#' # Import modelled datasets
#' 
#' Import LSM water fluxes at the cell where the Ameriflux tower is located. Since we do NOT want a basin-wide average, but instead a time series of values for a specified cell, we will use the GetMultiNcdf tool directly. First, we do a bit of setup.
#' 
#' Generate the list of filenames for the GetMultiNcdf tool by providing the pathname to the model OUTPUT directory and the filename pattern to match (in this case we want the LDASOUT files).
## ------------------------------------------------------------------------
lsmFiles <- list.files(path=paste0(dataPath, '/RUN.RTTESTS/OUTPUT_ALLRT_DAILY'), 
                       pattern=glob2rx('*LDASOUT*'), full.names=TRUE)
flList <- list(lsm=lsmFiles)

#' 
#' Now setup which LDASOUT variables we want to bring in. We will be working with the accumulated ET fluxes at our daily time step.
## ------------------------------------------------------------------------
lsmVars   <- list('ACCECAN','ACCEDIR','ACCETRAN','ACCPRCP')
names(lsmVars) <- lsmVars
varList <- list( lsm = lsmVars )

#' 
#' Setup the cell you want to extract and the stat you want to calculate. We are only running a single cell (i=2, j=3, time=1) so we specify the same start and end cell and use "CalcMeanMinrm" as the stat. "CalcMeanMinrm" is a utility function that calculates a mean with an enforced minimum valid value (-1e+30). This is useful to remove intended "no data" values that were not caught by NetCDF checks.
## ------------------------------------------------------------------------
niw <- list(start=c(2,3,1), end=c(2,3,1), stat='CalcMeanMinrm')

#' 
#' Match the variables to the desired cell and stat (here we are using the same for all variables).
## ------------------------------------------------------------------------
lsmInds <- list( niw, niw, niw, niw )
names(lsmInds) <- names(lsmVars)
indList <- list( lsm=lsmInds )

#' 
#' Run the tool. To speed things up, we will run this over 8 cores using the doMC package.
## ------------------------------------------------------------------------
library(doMC)
registerDoMC(8)
modDf <- GetMultiNcdf(file=flList,var=varList, ind=indList, parallel=TRUE)

#' 
#' Now we can "flatten" the output to make it work better with certain tools.
## ------------------------------------------------------------------------
modLsm.allrt.niw <- ReshapeMultiNcdf(modDf)
rm(modDf)

#' 
#' Some of the LDASOUT fields we exported are accumulated totals, so we need to break them out into individual (per time step) fluxes.
## ------------------------------------------------------------------------
modLsm.allrt.niw <- CalcNoahmpFluxes(modLsm.allrt.niw)

#' 
#' We want to examine total ET, so we need to calculate a sum column for all 3 evaporation and transpiration fluxes (in units of "mm"). We then use the density of water (1mm H<sub>2</sub>O * m<sup>2</sup> = 1 kg), the latent heat of vaporization (assuming evaporation and not sublimation during this summer period, LHVAP = 2.5106e+6 J/kg), and the time step (1 day = 86400 sec) to calculate total daily latent heat flux in W/m<sup>2</sup>.
## ------------------------------------------------------------------------
modLsm.allrt.niw$ET_mm <- with(modLsm.allrt.niw, DEL_ACCECAN + DEL_ACCETRAN + DEL_ACCEDIR)
modLsm.allrt.niw$LE <- with(modLsm.allrt.niw, ET_mm * 2.5106e+6 / 86400)

#' 
#' # Import observed datasets
#' 
#' Import Ameriflux station data for Niwot Ridge (Level 2 Standardized). We specify "America/Denver" as the time zone (Ameriflux timestamps are in local time).
## ------------------------------------------------------------------------
obsFlux30min.usnr1 <- ReadAmerifluxCSV(paste0(dataPath, '/OBS/AMF_USNR1_2013_L2_GF_V008.csv'), 
                                       "America/Denver")

#' 
#' Our LDASOUT time step was 1 day, so we need to aggregate the observations to a UTC daily time step for comparison.
## ------------------------------------------------------------------------
obsFlux1d.usnr1 <- aggregate(obsFlux30min.usnr1[names(obsFlux30min.usnr1) != c("POSIXct")], 
     by = list(as.POSIXct(round(as.POSIXct(format(obsFlux30min.usnr1$POSIXct, tz="UTC"), tz="UTC"), 
                                "days"))), 
     CalcMeanNarm)
names(obsFlux1d.usnr1)[1] <- "POSIXct"

#' 
#' 
#' # Plot & compare the time series of ET fluxes
#' 
## ----compFluxes, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFluxCompare(obsFlux1d.usnr1, "LE", modLsm.allrt.niw, "LE", 
     labelObs="Observed Niwot Ameriflux", 
     labelMod1="All Routing", title="ET: Niwot Ridge")

#' 
#' 
#' # Review model performance statistics
#' 
## ----, results='hide'----------------------------------------------------
CalcModPerf(modLsm.allrt.niw, obsFlux1d.usnr1, flxCol.mod="LE", flxCol.obs="LE")

#' 
## ----, results = "asis", echo=FALSE--------------------------------------
library(pander)
pander::pandoc.table(CalcModPerf(modLsm.allrt.niw, obsFlux1d.usnr1, 
                                 flxCol.mod="LE", flxCol.obs="LE"), 
                     split.table=Inf)

#' 
#' 
#' # Review hourly fluxes for May
#' 
#' Let's take a closer look at the month of May where our ET fluxes are significantly higher than the observed. We ran an hourly output run across the full month.
#' 
#' We will follow a similar setup as the daily fluxes, but we will grab the instantaneous water and energy fluxes from the LSM output and the wind inputs from the forcing data.
#' 
#' Provide the new pathname to the hourly output. We will limit the LDASIN data to just May 2013 since the FORCING directory contains a full year's worth of data.
## ------------------------------------------------------------------------
lsmFiles <- list.files(path=paste0(dataPath,'/RUN.FLUXCOMP/OUTPUT_ALLRT_MAY13_HOURLY'), 
                       pattern=glob2rx('*LDASOUT*'), full.names=TRUE)
forcFiles <- list.files(path=paste0(dataPath, '/FORCING'), pattern=glob2rx('201305*LDASIN*'), 
                        full.names=TRUE)
flList <- list(lsm=lsmFiles, forc=forcFiles)

#' 
#' Specify the variables for output
## ------------------------------------------------------------------------
lsmVars   <- list('ECAN', 'EDIR', 'ETRAN', 'RAINRATE', 'SWFORC', 'LWFORC', 
                  'FIRA', 'FSA', 'LH', 'HFX', 'GRDFLX')
names(lsmVars) <- lsmVars
forcVars <- list('T2D', 'Q2D', 'U2D', 'V2D', 'PSFC')
names(forcVars) <- forcVars
varList <- list(lsm=lsmVars, forc=forcVars)

#' 
#' Specify the indices and stats. We'll use the same "niw" location and stat as above.
## ------------------------------------------------------------------------
lsmInds   <- list( niw, niw, niw, niw, niw, niw, niw, niw, niw, niw, niw )
names(lsmInds) <- names(lsmVars)
forcInds <- list( niw, niw, niw, niw, niw )
names(forcInds) <- names(forcVars)
indList <- list(lsm=lsmInds, forc=forcInds)

#' 
#' Run the tool and flatten results.
## ------------------------------------------------------------------------
modDf <- GetMultiNcdf(file=flList,var=varList, ind=indList, parallel=TRUE)
modLsm.allrt.may.niw <- ReshapeMultiNcdf(subset(modDf, modDf$fileGroup=="lsm"))
modForc.allrt.may.niw <- ReshapeMultiNcdf(subset(modDf, modDf$fileGroup=="forc"))
rm(modDf)

#' 
#' 
#' # Plot & compare the time series of energy fluxes.
#' 
#' Latent heat:
## ----mayLatFluxes, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFluxCompare(obsFlux30min.usnr1, "LE", modLsm.allrt.may.niw, "LH", 
     labelObs="Observed Niwot Ameriflux", 
     labelMod1="All Routing", title="Latent Heat: Niwot Ridge (+ to atmosphere)")

#' 
#' Sensible heat:
## ----maySensFluxes, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFluxCompare(obsFlux30min.usnr1, "H", modLsm.allrt.may.niw, "HFX", 
     labelObs="Observed Niwot Ameriflux", 
     labelMod1="All Routing", title="Sensible Heat: Niwot Ridge (+ to atmosphere)")

#' 
#' Let's look at total net latent and sensible heat fluxes.
## ------------------------------------------------------------------------
obsFlux30min.usnr1$LE_H <- with(obsFlux30min.usnr1, LE + H)
modLsm.allrt.may.niw$LE_H <- with(modLsm.allrt.may.niw, LH + HFX)

#' 
#' 
## ----mayLatSensFluxes, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFluxCompare(obsFlux30min.usnr1, "LE_H", modLsm.allrt.may.niw, "LE_H", 
     labelObs="Observed Niwot Ameriflux", 
     labelMod1="All Routing", title="Net Latent & Sensible Heat: Niwot Ridge (+ to atmosphere)")

#' 
#' We see that the model is overestimating peak LE fluxes and underestimating SH fluxes, but the net heat exchange is fairly well represented. So we now examine our radiation fluxes to see if we are over-estimating available energy.
#' 
#' We do a few calculations to get net radiation and the overall energy balance.
#' 
## ------------------------------------------------------------------------
modLsm.allrt.may.niw$LWnet <- with(modLsm.allrt.may.niw, -FIRA)
modLsm.allrt.may.niw$Rnet <- with(modLsm.allrt.may.niw, FSA - FIRA)
modLsm.allrt.may.niw$EnResid <- with(modLsm.allrt.may.niw, Rnet - LE_H - GRDFLX)
obsFlux30min.usnr1$RgNet <- with(obsFlux30min.usnr1, Rg - RgOut)
obsFlux30min.usnr1$RglNet <- with(obsFlux30min.usnr1, Rgl - RglOut)
obsFlux30min.usnr1$EnResid <- with(obsFlux30min.usnr1, 
     (Rg - RgOut) + (Rgl - RglOut) - LE - H - FG)

#' 
#' Plot & compare the time series of incoming shortwave radiation
#' 
## ----maySWdwnFluxes, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFluxCompare(obsFlux30min.usnr1, "Rg", modLsm.allrt.may.niw, "SWFORC", 
     labelObs="Observed Niwot Ameriflux", 
     labelMod1="All Routing", title="Incoming Shortwave Radiation: Niwot Ridge (+ to surface)")

#' 
#' Plot & compare the time series of net shortwave radiation
#' 
## ----maySWnetFluxes, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFluxCompare(obsFlux30min.usnr1, "RgNet", modLsm.allrt.may.niw, "FSA", 
     labelObs="Observed Niwot Ameriflux", 
     labelMod1="All Routing", title="Net Shortwave Radiation: Niwot Ridge (+ to surface)")

#' 
#' Plot & compare the time series of net longwave radiation
#' 
## ----mayLWnetFluxes, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFluxCompare(obsFlux30min.usnr1, "RglNet", modLsm.allrt.may.niw, "LWnet", 
     labelObs="Observed Niwot Ameriflux", 
     labelMod1="All Routing", title="Net Longwave Radiation: Niwot Ridge (+ to surface)")

#' 
#' Plot & compare the time series of total net radiation
#' 
## ----mayRnetFluxes, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFluxCompare(obsFlux30min.usnr1, "Rn", modLsm.allrt.may.niw, "Rnet", 
     labelObs="Observed Niwot Ameriflux", 
     labelMod1="All Routing", title="Total Net Radiation: Niwot Ridge (+ to surface)")

#' 
#' Plot & compare the time series of energy residuals. The Ameriflux observations for ground heat flux do not cover this period, so we compare the observation residuals against the modelled ground heat flux.
#' 
## ----mayResidsFluxes, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFluxCompare(obsFlux30min.usnr1, "EnResid", modLsm.allrt.may.niw, "GRDFLX", 
     labelObs="Observed Niwot Ameriflux", 
     labelMod1="All Routing", title="Energy Residual: Niwot Ridge (+ to surface)")

#' 
#' # Plot & compare the time series of turbulent fluxes.
#' 
#' We do a few calculations to get the net wind speed.
#' 
## ------------------------------------------------------------------------
modForc.allrt.may.niw$Wind <- with(modForc.allrt.may.niw, sqrt(U2D^2 + V2D^2))

#' 
#' Plot & compare the time series of wind speeds. (This throws a warning, however the POSIXct timezones are handled correctly.)
#' 
## ----mayWindFluxes, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFluxCompare(subset(obsFlux30min.usnr1, 
                POSIXct >= as.POSIXct("2013-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC") & 
                POSIXct < as.POSIXct("2013-06-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC")), 
                "WS", modForc.allrt.may.niw, "Wind", 
                labelObs="Observed Niwot Ameriflux", 
                labelMod1="All Routing", title="Wind Speed: Niwot Ridge")

#' 
#' Now we convert specific humidity to relative humidity to compare to the flow tower observations.
#' 
## ------------------------------------------------------------------------
modForc.allrt.may.niw$RH <- with(modForc.allrt.may.niw, 
                            0.263*PSFC*Q2D*(exp((17.67*(T2D-273.16))/(T2D-29.65)))^(-1))

#' 
#' Plot & compare the time series of relative humidity. (Again, timzones are correctly handled.)
#' 
## ----mayRelHum, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFluxCompare(subset(obsFlux30min.usnr1, 
                POSIXct >= as.POSIXct("2013-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC") &
                POSIXct<as.POSIXct("2013-06-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC")), 
                "RH", modForc.allrt.may.niw, "RH", 
                labelObs="Observed Niwot Ameriflux", 
                labelMod1="All Routing", title="Relative Humidity: Niwot Ridge")

