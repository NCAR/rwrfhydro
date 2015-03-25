---
title: "Evaluate water budget partitioning with rwrfhydro"
author: "Aubrey Dugger and James McCreight"
date: "2015-03-24"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

# Background
We are using WRF-Hydro to predict streamflow for Fourmile Creek at the Orodell USGS gage for the 2013 snowmelt period. We ran WRF-Hydro with NoahMP as the LSM for a 3-year spinup period and then did an hourly run for 4 months starting March 1, 2013. We want to evaluate the predicted water budget partitioning over this snowmelt period.

Load the rwrfhydro package. 

```r
library("rwrfhydro")
```

Set a data path to the Fourmile Creek test case.

```r
dataPath <- '/Users/adugger/WRF_Hydro/test_cases/Fourmile_Creek'
```


# Import modelled datasets

Calculate basin-averaged LSM water fluxes. The LSM was run at 1km resolution and the high-res hydro grid was 100m resolution, so our aggregation factor is 10. We only have 1 basin in our model domain, and our basin ID is 1. We are going to use R's multi-core capability (make sure  doMC is installed) and run this summary over 8 cores.

```r
modLdasoutWb1h.allrt.fc <- ReadLdasoutWb(paste0(dataPath, '/OUTPUT_ALLRT'), 
                                         paste0(dataPath, '/DOMAIN/hydro_OrodellBasin_100m.nc'), 
                                         mskvar="basn_msk", basid=1, aggfact=10, ncores=8)
```

Calculate basin-averaged routing water fluxes. 

```r
modRtout1h.allrt.fc <- ReadRtout(paste0(dataPath, '/OUTPUT_ALLRT'), 
                                 paste0(dataPath, '/DOMAIN/hydro_OrodellBasin_100m.nc'), 
                                 mskvar="basn_msk", basid=1, ncores=8)
```

Import groundwater outflow model output.

```r
modGwout.allrt.fc <- ReadGwOut(paste0(dataPath, '/OUTPUT_ALLRT/GW_outflow.txt'))
```


# Evaluate the predicted water budget 

Calculate a water budget for the basin. Our modelled soil depths were 100, 300, 600, and 1000 mm, so match the defaults (therefore we do not need to specify). This model run had all routing options (subsurface, overland, groundwater, and channel) turned on, so we are providing rtout and gwout dataframes and switching sfcrt to TRUE. Our basin area is 63.1 km^2.

```r
wb.allrt.fc <- CalcNoahmpWatBudg(modLdasoutWb1h.allrt.fc, rtoutDf=modRtout1h.allrt.fc, 
                                 gwoutDf=modGwout.allrt.fc, sfcrt=TRUE, basarea=63.1)
```


```r
wb.allrt.fc
```

<hr><hr>

| LSM_PRCP   | LSM_ECAN   | LSM_ETRAN   | LSM_EDIR   | LSM_DELSWE   |
|:-----------|:-----------|:------------|:-----------|:-------------|
| 378.1      | 32.86      | 125.8       | 154.7      | -33.86       |

Table: Table continues below (continued below)

 

| LSM_DELCANWAT   | LSM_SFCRNOFF   | LSM_UGDRNOFF   |
|:----------------|:---------------|:---------------|
| -0.179          | 30.13          | 92.68          |

 

| LSM_DELSOILM   | HYD_QSTRMVOL   | HYD_DELSFCHEAD   | HYD_QBDRY   |
|:---------------|:---------------|:-----------------|:------------|
| 2.793          | 3.671          | -0.005656        | 0           |

Table: Table continues below

 

| HYD_GWOUT   | HYD_DELGWSTOR   | WB_SFCRNOFF   | WB_GWOUT   | ERROR   |
|:------------|:----------------|:--------------|:-----------|:--------|
| 93.51       | -0.8305         | 3.671         | 93.51      | -0.4091 |

Table: Table continues below

 

| RUN_FRAC   | EVAP_FRAC   | STOR_FRAC   |
|:-----------|:------------|:------------|
| 0.257      | 0.8289      | -0.08487    |
<hr><hr>

Plot the water budget as a pie chart.

```r
PlotWatBudg(wb.allrt.fc)
```

<img src="figure/watbudgPie-1.png" title="plot of chunk watbudgPie" alt="plot of chunk watbudgPie" width="600" height="600" />

Plot the water budget as a bar chart.

```r
PlotWatBudg(wb.allrt.fc, "bar")
```

<img src="figure/watbudgBar-1.png" title="plot of chunk watbudgBar" alt="plot of chunk watbudgBar" width="600" height="600" />
