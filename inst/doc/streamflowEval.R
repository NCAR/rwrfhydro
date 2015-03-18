## ------------------------------------------------------------------------
library("rwrfhydro")

## ------------------------------------------------------------------------
dataPath <- '/Users/jamesmcc/WRF_Hydro/rwrfHydroTestCases' 

## ------------------------------------------------------------------------
modStrh.chrt.fc <- ReadFrxstPts(paste0(dataPath,"/OUTPUT_FOURMILE_CHRT/frxst_pts_out.txt"))

## ------------------------------------------------------------------------
modStrh.allrt.fc <- ReadFrxstPts(paste0(dataPath,"/OUTPUT_FOURMILE_ALLRT/frxst_pts_out.txt"))

## ------------------------------------------------------------------------
obsStr5min.fc <- ReadUsgsGage(paste0(dataPath,"/OBS/5min_str_06727500_110401_140810.txt"))

## ----compHydrographs, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFluxCompare(obsStr5min.fc, "q_cms", modStrh.chrt.fc, "q_cms", strDf.mod2=modStrh.allrt.fc, 
                strCol.mod2="q_cms", labelObs="Observed Fourmile Creek at Orodell", 
	            	labelMod1="Channel Routing Only", labelMod2="All Routing", 
		            title="Streamflow: Fourmile Creek")

## ----compHydrographsSnow, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFluxCompare(obsStr5min.fc, "q_cms", modStrh.chrt.fc, "q_cms", strDf.mod2=modStrh.allrt.fc, 
                strCol.mod2="q_cms", labelObs="Observed Fourmile Creek at Orodell", 
            		labelMod1="Channel Routing Only", labelMod2="All Routing", 
		            title="Streamflow: Fourmile Creek", 
                stdate=as.POSIXct("2013-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC"), 
                enddate=as.POSIXct("2013-06-30 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC"))

## ------------------------------------------------------------------------
obsStr5min.fc <- CalcFdc(obsStr5min.fc)
modStrh.chrt.fc <- CalcFdc(modStrh.chrt.fc)
modStrh.allrt.fc <- CalcFdc(modStrh.allrt.fc)

## ------------------------------------------------------------------------
fdc.obsStr5min.fc <- CalcFdcSpline(obsStr5min.fc)
fdc.modStrh.chrt.fc <- CalcFdcSpline(modStrh.chrt.fc)
fdc.modStrh.allrt.fc <- CalcFdcSpline(modStrh.allrt.fc)

## ----, results='hold'----------------------------------------------------
fdc.obsStr5min.fc(0.2)
fdc.modStrh.chrt.fc(0.2)
fdc.modStrh.allrt.fc(0.2)

## ----, results='hold'----------------------------------------------------
fdc.obsStr5min.fc(0.8)
fdc.modStrh.chrt.fc(0.8)
fdc.modStrh.allrt.fc(0.8)

## ----flowDurationCurves, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
PlotFdcCompare(obsStr5min.fc, "q_cms", modStrh.chrt.fc, "q_cms", strDf.mod2=modStrh.allrt.fc, 
               strCol.mod2="q_cms", labelObs="Observed Fourmile Creek", 
		           labelMod1="Channel Routing Only", labelMod2="All Routing")

## ----, results='hide'----------------------------------------------------
CalcModPerf(modStrh.allrt.fc, obsStr5min.fc)

## ----, , results = "asis", echo=FALSE------------------------------------
library(pander)
pander::pandoc.table(CalcModPerf(modStrh.allrt.fc, obsStr5min.fc),split.table=Inf)

## ----, results='hide'----------------------------------------------------
CalcModPerf(modStrh.chrt.fc, obsStr5min.fc)

## ----, , results = "asis", echo=FALSE------------------------------------
pander::pandoc.table(CalcModPerf(modStrh.chrt.fc, obsStr5min.fc), split.table=Inf)

## ----, results='hide'----------------------------------------------------

help(CalcModPerf)

## ----, echo=FALSE--------------------------------------------------------

library(printr)
help(CalcModPerf)

## ----, results='hide'----------------------------------------------------
CalcFdcPerf(modStrh.allrt.fc, obsStr5min.fc)

## ----, , results = "asis", echo=FALSE------------------------------------
pander::pandoc.table(CalcFdcPerf(modStrh.allrt.fc, obsStr5min.fc),split.table=Inf)

## ----, results='hide'----------------------------------------------------
help(CalcFdcPerf)

## ----, echo=FALSE--------------------------------------------------------
help(CalcFdcPerf)

