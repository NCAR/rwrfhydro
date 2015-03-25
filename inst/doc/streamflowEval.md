---
title: "Evaluate streamflow simulation with rwrfhydro"
author: "Aubrey Dugger and James McCreight"
date: "2015-03-24"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

# Background
We are using WRF-Hydro to predict streamflow for Fourmile Creek at the Orodell USGS gage for the 2013 snowmelt period. We ran WRF-Hydro with NoahMP as the LSM for a 3-year spinup period and then did an hourly run for 4 months starting March 1, 2013. We want to evaluate model performance in estimating streamflow with and without overland, subsurface, and groundwater routing active.

Load the rwrfhydro package. 

```r
library("rwrfhydro")
```

Set a data path to the Fourmile Creek test case.

```r
dataPath <- '/Users/adugger/WRF_Hydro/test_cases/Fourmile_Creek'
```


# Import modelled and observed datasets

Model 1: Only channel routing turned on (hourly model run).

```r
modStrh.chrt.fc <- ReadFrxstPts(paste0(dataPath, '/OUTPUT_CHRT/frxst_pts_out.txt'))
```

Model 2: All WRF-Hydro routing options turned on (hourly model run).

```r
modStrh.allrt.fc <- ReadFrxstPts(paste0(dataPath, '/OUTPUT_CHRT/frxst_pts_out.txt'))
```

USGS gage observed data at 5-minute intervals.

```r
obsStr5min.fc <- ReadUsgsGage(paste0(dataPath, '/OBS/5min_str_06727500_110401_140810.txt'))
```


# Plot hydrographs 

Compare hydrographs for the full model run.

```r
PlotFluxCompare(obsStr5min.fc, "q_cms", modStrh.chrt.fc, "q_cms", strDf.mod2=modStrh.allrt.fc, 
                strCol.mod2="q_cms", labelObs="Observed Fourmile Creek at Orodell", 
	            	labelMod1="Channel Routing Only", labelMod2="All Routing", 
		            title="Streamflow: Fourmile Creek")
```

<img src="figure/compHydrographs-1.png" title="plot of chunk compHydrographs" alt="plot of chunk compHydrographs" width="700" height="350" />

Now limit the plot to the June recession period only. The reported stats are updated to the new time period. (Note that the R warning is innocuous because the subset adjusts for timezone, so it's ok that the timezones dont match.)

```r
PlotFluxCompare(obsStr5min.fc, "q_cms", modStrh.chrt.fc, "q_cms", strDf.mod2=modStrh.allrt.fc, 
                strCol.mod2="q_cms", labelObs="Observed Fourmile Creek at Orodell", 
            		labelMod1="Channel Routing Only", labelMod2="All Routing", 
		            title="Streamflow: Fourmile Creek", 
                stdate=as.POSIXct("2013-06-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC"), 
                enddate=as.POSIXct("2013-06-30 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC"))
```

```
## Warning in check_tzones(e1, e2): 'tzone' attributes are inconsistent
```

```
## Warning in check_tzones(e1, e2): 'tzone' attributes are inconsistent
```

<img src="figure/compHydrographsSnow-1.png" title="plot of chunk compHydrographsSnow" alt="plot of chunk compHydrographsSnow" width="700" height="350" />

# Review flow duration curves

Calculate percent exceedances for flow duration curves.

```r
obsStr5min.fc <- CalcFdc(obsStr5min.fc)
modStrh.chrt.fc <- CalcFdc(modStrh.chrt.fc)
modStrh.allrt.fc <- CalcFdc(modStrh.allrt.fc)
```

Compare how the models are doing predicting flow values that will be exceeded 20% of the time.
First, calculate the fitted spline functions.

```r
fdc.obsStr5min.fc <- CalcFdcSpline(obsStr5min.fc)
fdc.modStrh.chrt.fc <- CalcFdcSpline(modStrh.chrt.fc)
fdc.modStrh.allrt.fc <- CalcFdcSpline(modStrh.allrt.fc)
```

Then, evaluate at the 20% exceedance percentage (high flows).

```r
fdc.obsStr5min.fc(0.2)
fdc.modStrh.chrt.fc(0.2)
fdc.modStrh.allrt.fc(0.2)
```

```
## [1] 0.4911966
## [1] 0.3224199
## [1] 0.3224199
```

Now try the 80% exceedance percentage (low flows).

```r
fdc.obsStr5min.fc(0.8)
fdc.modStrh.chrt.fc(0.8)
fdc.modStrh.allrt.fc(0.8)
```

```
## [1] 0.03339262
## [1] 0.01294871
## [1] 0.01294871
```

Plot flow duration curves for a more complete picture.

```r
PlotFdcCompare(obsStr5min.fc, "q_cms", modStrh.chrt.fc, "q_cms", strDf.mod2=modStrh.allrt.fc, 
               strCol.mod2="q_cms", labelObs="Observed Fourmile Creek", 
		           labelMod1="Channel Routing Only", labelMod2="All Routing")
```

```
## [1] "Combined min flow for y-axis (capped at 0.001): 0.00821188551168"
## [1] "Combined max flow for y-axis: 5.337"
```

<img src="figure/flowDurationCurves-1.png" title="plot of chunk flowDurationCurves" alt="plot of chunk flowDurationCurves" width="700" height="350" />


# Review model performance statistics

Calculate model performance stats (special formatting comands hidden).

```r
CalcModPerf(modStrh.allrt.fc, obsStr5min.fc)
```


----------------------------------------------------------------------------------------------
   &nbsp;      nse    nselog   cor   rmse   rmsenorm   bias   mae   errcom   errmaxt   errfdc 
------------- ------ -------- ----- ------ ---------- ------ ----- -------- --------- --------
   **ts**      0.68    0.72   0.88   0.26     9.77     22.2  0.13     NA       NA       0.07  

  **daily**    0.79    0.71   0.92   0.21     9.03     23.5  0.12    0.21     -0.34     0.08  

 **monthly**   0.93    0.75   0.99   0.1      9.08     26.2  0.08   -0.45     -0.36      NA   

 **yearly**    -Inf    -Inf    NA    0.08     Inf      22.2  0.08     -2       -2        NA   

  **max10**   -1.14   -0.05   0.34   0.64    38.88     7.5   0.05     NA       NA        NA   

  **min10**   -107.4  -8.05   0.24   0.08    318.2    225.1   NA      NA       NA        NA   
----------------------------------------------------------------------------------------------


```r
CalcModPerf(modStrh.chrt.fc, obsStr5min.fc)
```


----------------------------------------------------------------------------------------------
   &nbsp;      nse    nselog   cor   rmse   rmsenorm   bias   mae   errcom   errmaxt   errfdc 
------------- ------ -------- ----- ------ ---------- ------ ----- -------- --------- --------
   **ts**      0.68    0.72   0.88   0.26     9.77     22.2  0.13     NA       NA       0.07  

  **daily**    0.79    0.71   0.92   0.21     9.03     23.5  0.12    0.21     -0.34     0.08  

 **monthly**   0.93    0.75   0.99   0.1      9.08     26.2  0.08   -0.45     -0.36      NA   

 **yearly**    -Inf    -Inf    NA    0.08     Inf      22.2  0.08     -2       -2        NA   

  **max10**   -1.14   -0.05   0.34   0.64    38.88     7.5   0.05     NA       NA        NA   

  **min10**   -107.4  -8.05   0.24   0.08    318.2    225.1   NA      NA       NA        NA   
----------------------------------------------------------------------------------------------

Help on CalcModPerf gives details on the individual statistics returned.

```r
help(CalcModPerf)
```

<div style="border:1px solid; border-radius: 25px; padding: 12px 25px;">

```
## Computes model performance statistics for WRF-Hydro flux output
## 
## Description:
## 
##      'CalcModPerf' calculates model performance statistics for flux
##      output.
## 
## Usage:
## 
##      CalcModPerf(flxDf.mod, flxDf.obs, flxCol.mod = "q_cms",
##        flxCol.obs = "q_cms", stdate = NULL, enddate = NULL)
##      
## Arguments:
## 
## flxDf.mod: The flux output dataframe (required). Assumes only one
##           forecast point per file, so if you have multiple forecast
##           points in your output dataframe, use subset to isolate a
##           single forecast point's data. Also assumes model output and
##           observation both contain POSIXct fields (called "POSIXct").
## 
## flxDf.obs: The observed flux dataframe. Assumes only one observation
##           point per file, so if you have multiple observation points in
##           your dataframe, use subset to isolate a single point's data.
##           Also assumes model output and observation both contain
##           POSIXct fields (called "POSIXct").
## 
## flxCol.mod: The column name for the flux time series for the MODEL data
##           (default="q_cms")
## 
## flxCol.obs: The column name for the flux time series for the OBSERVED
##           data (default="q_cms")
## 
##   stdate: Start date for plot/statistics (DEFAULT=NULL, all records
##           will be used). Date MUST be specified in POSIXct format with
##           appropriate timezone (e.g., as.POSIXct("2013-05-01 00:00:00",
##           format="%Y-%m-%d %H:%M:%S", tz="UTC"))
## 
##  enddate: End date for plot/statistics (DEFAULT=NULL, all records will
##           be used). Date MUST be specified in POSIXct format with
##           appropriate timezone (e.g., as.POSIXct("2013-05-01 00:00:00",
##           format="%Y-%m-%d %H:%M:%S", tz="UTC"))
## 
## Details:
## 
##      'CalcModPerf' reads a model flux time series (i.e., created using
##      'ReadFrxstPts') and an observation time series (i.e., created
##      using 'ReadUsgsGage') and calculates model performance statistics
##      (Nash-Sutcliffe Efficiency, Rmse, etc.) at various time scales and
##      for low and high fluxes. The tool will subset data to matching
##      time periods (e.g., if the observed data is at 5-min increments
##      and modelled data is at 1-hr increments, the tool will subset the
##      observed data to select only observations on the matching hour
##      break).
## 
##      Performance Statistics:
##      (mod = model output, obs = observations, n = sample size)
## 
##         • nse: Nash-Sutcliffe Efficiency
## 
##            nse = 1 - ( sum((obs - mod)^2) / sum((obs - mean(obs))^2) )  
##           
##         • nselog: log-transformed Nash-Sutcliffe Efficiency
## 
##           nselog = 1 - ( sum((log(obs) - log(mod))^2) / sum((log(obs) - mean(log(obs)))^2) ) 
##           
##         • cor: correlation coefficient
## 
##                                cor = cor(mod, obs)                      
##           
##         • rmse: root mean squared error
## 
##                       rmse = sqrt( sum((mod - obs)^2) / n )             
##           
##         • rmsenorm: normalized root mean squared error
## 
##                      rmsenorm = rmse / (max(obs) - min(obs))            
##           
##         • bias: percent bias
## 
##                      bias = sum(mod - obs) / sum(obs) * 100             
##           
##         • mae: mean absolute error
## 
##                            mae = mean(abs(mod - obs))                   
##           
##         • errcom: error in the center-of-mass of the flux, where
##           center-of-mass is the hour/day when 50% of
##           daily/monthly/water-year flux has occurred. Reported as
##           number of hours for daily time scale and number of days for
##           monthly and yearly time scales.
## 
##         • errmaxt: Error in the time of maximum flux. Reported as
##           number of hours for daily time scale and number of days for
##           monthly and yearly time scales).
## 
##         • errfdc: Error in the integrated flow duration curve between
##           0.05 and 0.95 exceedance thresholds (in native flow units).
## 
##      Time scales/Flux types:
## 
##         • ts = native model/observation time step (e.g., hourly)
## 
##         • daily = daily time step
## 
##         • monthly = monthly time step
## 
##         • yearly = water-year time step
## 
##         • max10 = high flows; restricted to the portion of the time
##           series where the observed flux is in the highest 10%
## 
##         • min10 = low flows; restricted to the portion of the time
##           series where the observed flux is in the lowest 10%
## 
## Value:
## 
##      A new dataframe containing the model performance statistics.
## 
## Examples:
## 
##      ## Take forecast point model output for Fourmile Creek (modStrh.mod1.fc) and a corresponding
##      ## USGS gage observation file (obsStrh.fc), both at an hourly time step, and calculate
##      ## model performance statistics. The model forecast point data was imported using ReadFrxstPts
##      ## and the gage observation data was imported using ReadUsgsGage.
##      
##      CalcModPerf(modStr1h.allrt.fc, obsStr5min.fc)
##      
##      > Output:
##                nse nselog  cor rmse rmsenorm  bias  mae errcom errmaxt errfdc
##      ts       0.57   0.61 0.79 1.43     9.48 -28.0 0.70     NA      NA  -0.42
##      daily    0.71   0.64 0.87 1.17     9.86 -28.1 0.61   0.19   -2.25  -0.37
##      monthly  0.80   0.70 0.93 0.89    12.73 -26.6 0.53  -0.18   -0.96     NA
##      yearly   0.05   0.37 0.36 0.55    41.50  -6.5 0.45  -1.50   -3.38     NA
##      max10   -7.50 -15.94 0.19 3.82    38.89 -24.5 0.04     NA      NA     NA
##      min10   -2.84  -1.83 0.10 0.05    33.36 -23.7   NA     NA      NA     NA
## 
```
</div>

Calculate flow duration curve performance statistics.

```r
CalcFdcPerf(modStrh.allrt.fc, obsStr5min.fc)
```


-------------------------------------------
 p.exceed   q.mod   q.obs   q.err   q.perr 
---------- ------- ------- ------- --------
   0.1      1.23    0.95    0.28     29.5  

   0.2      0.76    0.69    0.07     10.1  

   0.3      0.46    0.39    0.07     17.9  

   0.4      0.32    0.23    0.09     39.1  

   0.5      0.23    0.14    0.09     64.3  

   0.6      0.15     0.1    0.05      50   

   0.7      0.11    0.07    0.04     57.1  

   0.8      0.08    0.05    0.03      60   

   0.9      0.05    0.03    0.02     66.7  
-------------------------------------------

Again, help on CalcFdcPerf gives details on the individual statistics returned.

```r
help(CalcFdcPerf)
```
<div style="border:1px solid; border-radius: 25px; padding: 12px 25px;">

```
## Computes flow duration curve statistics for WRF-Hydro streamflow output
## 
## Description:
## 
##      'CalcFdcPerf' calculates flow duration curve statistics for
##      streamflow output.
## 
## Usage:
## 
##      CalcFdcPerf(strDf.mod, strDf.obs, strCol.mod = "q_cms",
##        strCol.obs = "q_cms", stdate = NULL, enddate = NULL)
##      
## Arguments:
## 
## strDf.mod: The forecast point output dataframe (required). Assumes only
##           one forecast point per file, so if you have multiple forecast
##           points in your output dataframe, use subset to isolate a
##           single forecast point's data. Also assumes model output and
##           observation both contain POSIXct fields (called "POSIXct").
## 
## strDf.obs: The observed streamflow dataframe. Assumes only one gage per
##           file, so if you have multiple gages in your dataframe, use
##           subset to isolate a single gage's data. Also assumes model
##           output and observation both contain POSIXct fields (called
##           "POSIXct").
## 
## strCol.mod: The column name for the streamflow time series for the
##           MODEL data (default="q_cms")
## 
## strCol.obs: The column name for the streamflow time series for the
##           OBSERVED data (default="q_cms")
## 
##   stdate: Start date for plot/statistics (DEFAULT=NULL, all records
##           will be used). Date MUST be specified in POSIXct format with
##           appropriate timezone (e.g., as.POSIXct("2013-05-01 00:00:00",
##           format="%Y-%m-%d %H:%M:%S", tz="UTC"))
## 
##  enddate: End date for plot/statistics (DEFAULT=NULL, all records will
##           be used). Date MUST be specified in POSIXct format with
##           appropriate timezone (e.g., as.POSIXct("2013-05-01 00:00:00",
##           format="%Y-%m-%d %H:%M:%S", tz="UTC"))
## 
## Details:
## 
##      'CalcFdcPerf' reads a model forecast point streamflow timeseries
##      (i.e., created using 'ReadFrxstPts') and a streamflow observation
##      timeseries (i.e., created using 'ReadUsgsGage') and calculates
##      flow duration curve statistics at various exceedance thresholds
##      (e.g., 10%, 20%, etc.). The tool will subset data to matching time
##      periods (e.g., if the observed data is at 5-min increments and
##      modelled data is at 1-hr increments, the tool will subset the
##      observed data to select only observations on the matching hour
##      break).
## 
##      Flow Duration Curve Statistics:
##      (mod = model output, obs = observations)
## 
##         • p.exceed: exceedance threshold (e.g., 0.2 means a flow value
##           that is exceeded 20% of the time)
## 
##         • q.mod: MODEL flow value at specified exceedance threshold (in
##           native flow units)
## 
##         • q.obs: OBSERVED flow value at specified exceedance threshold
##           (in native flow units)
## 
##         • q.err: difference between model and observed flow values
##           [mod-obs] (in native flow units)
## 
##         • q.perr: percent error in model flow [(mod-obs)/obs]
## 
## Value:
## 
##      A new dataframe containing the flow duration curve statistics.
## 
## Examples:
## 
##      ## Take forecast point model output for Fourmile Creek (modStrh.mod1.fc) and a corresponding
##      ## USGS gage observation file (obsStrh.fc), both at an hourly time step, and calculate
##      ## flow duration curve statistics. The model forecast point data was imported using ReadFrxstPts
##      ## and the gage observation data was imported using ReadUsgsGage.
##      
##      CalcFdcPerf(modStr1h.allrt.fc, obsStr5min.fc)
##      
##      Output:
##       p.exceed    q.mod   q.obs
##       0.1         3.07    5.25
##       0.2         1.35    2.31
##       0.3         0.82    1.06
##       0.4         0.48    0.65
##       0.5         0.29    0.45
##       0.6         0.18    0.34
##       0.7         0.14    0.25
##       0.8         0.11    0.19
##       0.9         0.08    0.16
## 
```
</div>
