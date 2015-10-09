#' ---
#' title: "Perfect model nudging with WRF Hydro."
#' author: "James McCreight"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Perfect model nudging with WRF Hydro}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   \usepackage[utf8]{inputenc}
#' ---
#' 
#' # Background
#' Synthetic data assimilation experiments known as perfect model assimilation are a convenient way to "know" the truth of what is being assimilated. This can help evaluate the data assimilation system and the limits of skill improvements through DA. 
#' 
#' # Setup and Visualize
#' Load the rwrfhydro package. 
## ---- results='hide'-----------------------------------------------------
library("rwrfhydro")

#' 
## ---- fig.width = 12, fig.height = 9, out.width='700', out.height='525'----
# set your own path
rlFile <- '~/WRF_Hydro/DOMAIN_library/Boulder_Creek_100m_1km_2sqkm_full_2015_09_03//Route_Link.nc' 
# currently there is no "gages" variable
ncdump(rlFile)  
# This function returns a function which plots the data
PlotRouteLink <- VisualizeRouteLink(rlFile)
PlotRouteLink() # basic plot

#' 
## ---- fig.width = 12, fig.height = 9, out.width='700', out.height='525'----
args(PlotRouteLink) # what are the options? unfortunately I havent really documented these.
PlotRouteLink(zoom=10, comIds=TRUE) # links on the edge of the domain arent fully plotted... because there's no "to" lat/lon.

#' 
#' # Create the gages variable
#' Define the synthetic gages to be used for the domain. In this example put gages on ALL the links.
## ------------------------------------------------------------------------
rlLink <- ncdump(rlFile, 'link', quiet=TRUE)
gageIds <- paste0('g',formatC(rlLink,width=3,flag='0'))
newCopyId <- 'gagesAllLinks'
newFile <- AddRouteLinkGage(rlFile, gageIds, rlLink, new=newCopyId, overwrite=TRUE)
print(newFile)
ncdump(newFile)
print(ncdump(newFile, 'gages', quiet=TRUE))

#' 
#' 
#' ## Run the model with doubled precip
#' Set the symlink to the new Route_Link.nc file.
#' ``` bash
#' jamesmcc@hydro-c1:~/WRF_Hydro/Col_Bldr_Creek/DOMAIN> l
#' lrwxrwxrwx  2 jamesmcc rap   93 Sep  9 09:42 Route_Link.nc -> 
#'               ../../DOMAIN_library/Boulder_Creek_100m_1km_2sqkm_full_2015_09_03/Route_Link.gagesAllLinks.nc
#' ```
#' Set up several model parameters in the namelists.
#' 
#' namelist.hrldas: 
#' ```
#'  START_YEAR  = 2012
#'  START_MONTH  = 08
#'  START_DAY  = 01
#'  START_HOUR  = 00
#'  START_MIN   = 00
#'  KHOUR = 48
#'  FORCING_TIMESTEP = 3600
#'  NOAH_TIMESTEP    = 900
#'  OUTPUT_TIMESTEP  = 7200
#'  FORC_TYP = 4
#' ```
#' 
#' hydro.namelist:
#' ```
#'  out_dt = 15 ! minutes
#'  DTRT_CH = 60
#'  DTRT_TER = 6
#'  SUBRTSWCRT = 1
#'  OVRTSWCRT = 1
#'  CHANRTSWCRT = 1
#'  channel_option = 2
#'  route_link_f = "DOMAIN/Route_Link.nc"
#' ```
#' 
#' Run without nudging and using precip doubling. The wrf_hydro.noNudging_doublePrecip.exe binary was built with the following environment variables: PRECIP_DOUBLE=1 WRF_HYDRO_NUDGING=0 using the daBranch of wrf_hydro_model.
#' ```
#' jamesmcc@hydro-c1:~/WRF_Hydro/Col_Bldr_Creek> mpirun -np 1 ./wrf_hydro.noNudging_doublePrecip.exe
#' . . .
#' 
#' jamesmcc@hydro-c1:~/WRF_Hydro/Col_Bldr_Creek> mkdir run.pmo
#' jamesmcc@hydro-c1:~/WRF_Hydro/Col_Bldr_Creek> ./cleanup.sh run.pmo
#' ```
#' 
#' ## Transform output to timeSlices
#' Check chanobs and frxst pts dimensions
#' ```
#' jamesmcc@hydro-c1:~/WRF_Hydro/Col_Bldr_Creek/run.pmo> wc -l frxst_pts_out.txt
#' 77779 frxst_pts_out.txt  ## = 403stns*(48hours*4output/hr+1output@0)
#' ```
#' 
## ----precipDoubleRun, fig.width = 12, fig.height = 6, out.width='700', out.height='350'----
frxst <- ReadFrxstPts("~/WRF_Hydro/Col_Bldr_Creek/run.pmo/frxst_pts_out.txt", stId='character')
ggplot2::ggplot(subset(frxst, POSIXct < as.POSIXct('2012-08-01 06:00:00', tz='UTC')), ## all the action is early on
                ggplot2::aes(x=POSIXct, y=q_cms, color=st_id)) +
  ggplot2::geom_line()  +
  ggplot2::scale_color_discrete(guide='none') +
  ggplot2::theme_bw()

#' 
#' Plot it by order
## ----orderPlot, fig.width = 12, fig.height = 9, out.width='700', out.height='525'----
rlOrder <- ncdump(rlFile, 'order', quiet=TRUE)
names(rlOrder) <- paste0('g',formatC(rlLink,width=3,flag='0'))
frxst$order <- rlOrder[trimws(frxst$st_id)]
ggplot2::ggplot(subset(frxst, POSIXct < as.POSIXct('2012-08-01 06:00:00', tz='UTC')), 
                ggplot2::aes(x=POSIXct, y=q_cms, color=st_id)) +
  ggplot2::geom_line()  +
  ggplot2::scale_color_discrete(guide='none') +
  ggplot2::facet_wrap(~order, ncol=1, scales='free_y') +
  ggplot2::theme_bw()

#' 
#' ## Create timeslice files
## ------------------------------------------------------------------------
chanObsFiles <- list.files('~/WRF_Hydro/Col_Bldr_Creek/run.pmo/', pattern = 'CHANOBS_DOMAIN', full.names=TRUE)
sliceResolutionMin <- 15
outputDir <- '~/WRF_Hydro/Col_Bldr_Creek/run.pmo/timeSlicePMO/'
dir.create(outputDir)
sliceFiles <- ChanObsToTimeSlice(chanObsFiles, sliceResolutionMin, outputDir) 

#' 
#' Put these in place
#' ```
#' jamesmcc@hydro-c1:~/WRF_Hydro/Col_Bldr_Creek> rm nudgingTimeSliceObs 
#' jamesmcc@hydro-c1:~/WRF_Hydro/Col_Bldr_Creek> ln -sf run.pmo/timeSlicePMO/ nudgingTimeSliceObs
#' ```
#' 
#' ## Create the nudging parameter file
## ------------------------------------------------------------------------
min(ncdump(rlFile, 'Length', quiet=TRUE))  ## use R less than this /2.
gageParams <- data.frame( gageId = paste0('g',formatC(1:403,width=3,flag='0')), stringsAsFactors=FALSE)
gageParams$R=24
gageParams$G=1
gageParams$tau=20
MkNudgingParams(gageId=gageParams$gageId, R=gageParams$R, 
               G=gageParams$G, tau=gageParams$tau, 
               outFile='~/WRF_Hydro/Col_Bldr_Creek/run.pmo/nudgingParams.PMOallGages.nc', 
               overwrite=TRUE)

#' 
#' Then put the param file in place. 
#' ```
#' jamesmcc@hydro-c1:~/WRF_Hydro/Col_Bldr_Creek> ln -s run.pmo/nudgingParams.PMOallGages.nc nudgingParams.nc
#' ```
#' 
#' ## Assimilate the observations using regular forcing
#' 
#' The wrf_hydro.nudging.exe binary was built with the following environment variables: `PRECIP_DOUBLE=0` and `WRF_HYDRO_NUDGING=1` using the daBranch of wrf_hydro_model.
#' ```
#' jamesmcc@hydro-c1:~/WRF_Hydro/Col_Bldr_Creek> mpirun -np 1 ./wrf_hydro.nudging.exe
#' jamesmcc@hydro-c1:~/WRF_Hydro/Col_Bldr_Creek> mkdir run.pmoNudge
#' jamesmcc@hydro-c1:~/WRF_Hydro/Col_Bldr_Creek> ./cleanup.sh run.pmoNudge
#' ```
#' 
#' ## Run the model without nudging using regular forcingings.
#' 
#' The wrf_hydro.noNudging.exe binary was built with the following environment variables: `PRECIP_DOUBLE=0` and `WRF_HYDRO_NUDGING=0` using the daBranch of wrf_hydro_model.
#' ```
#' jamesmcc@hydro-c1:~/WRF_Hydro/Col_Bldr_Creek> mpirun -np 1 ./wrf_hydro.noNudging.exe
#' jamesmcc@hydro-c1:~/WRF_Hydro/Col_Bldr_Creek> mkdir run.pmoNoNudge
#' jamesmcc@hydro-c1:~/WRF_Hydro/Col_Bldr_Creek> ./cleanup.sh run.pmoNoNudge
#' ```
#' 
#' ## Visualize
#' 
#' 77779 frxst_pts_out.txt  ## = 403stns*(48hours*4output/hr+1output@0)
## ---- fig.width = 12, fig.height = 9, out.width='700', out.height='525'----
frxstNoNudge <- ReadFrxstPts("~/WRF_Hydro/Col_Bldr_Creek/run.pmoNoNudge/frxst_pts_out.txt", stId='character')
frxstNudge <- ReadFrxstPts("~/WRF_Hydro/Col_Bldr_Creek/run.pmoNudge/frxst_pts_out.txt", stId='character')

## make sure the abscissae columns are equal before taking the differences
if(all(plyr::laply(NamedList(names(frxst)), function(colName) all(frxst[[colName]]==frxstNoNudge[[colName]]))[-6:-7])) 
{
  forcErr <- frxst
  forcErr$`q err obs-model (cms)` <- frxst$q_cms - frxstNoNudge$q_cms
  forcErr$`q err obs-model (cfs)` <- frxst$q_cfs - frxstNoNudge$q_cfs
  forcErr$q_cms <- forcErr$q_cfs <- NULL
  forcErr$error <- 'forcing'
} else warning('Abscissae do not line up with frxstNoNudge, please investigate', immediate.=TRUE)

if(all(plyr::laply(NamedList(names(frxst)), function(colName) all(frxst[[colName]]==frxstNudge[[colName]]))[-6:-7])) 
{
  nudgeErr <- frxst
  nudgeErr$`q err obs-model (cms)` <- frxst$q_cms - frxstNudge$q_cms
  nudgeErr$`q err obs-model (cfs)` <- frxst$q_cfs - frxstNudge$q_cfs
  nudgeErr$q_cms <- nudgeErr$q_cfs <- NULL
  nudgeErr$error <- 'nudging'
} else warning('Abscissae do not line up with frxstNudge, please investigate', immediate.=TRUE)

err <- rbind(forcErr, nudgeErr)

ggplot2::ggplot(subset(err, POSIXct < as.POSIXct('2012-08-01 06:00:00', tz='UTC')), ## all the action is early on
                ggplot2::aes(x=POSIXct, y=`q err obs-model (cms)`, color=st_id)) +
  ggplot2::geom_line()  +
  ggplot2::scale_color_discrete(guide='none') +
  ggplot2::facet_wrap(~error, ncol=1) + 
  ggplot2::theme_bw()

#' 
