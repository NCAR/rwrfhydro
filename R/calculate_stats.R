#' Calculate water fluxes from NoahMP output
#'
#' \code{CalcNoahmpFluxes} calculates water balance fluxes from accumulated water terms.
#'
#' Read a dataframe derived from NoahMP LDASOUT output (i.e., using \code{\link{GetMultiNcdf}}) and
#' calculate water budget component fluxes from accumulated water variables.
#' NOTE: Currently only works for runs using NoahMP as the LSM.
#' 
#' @param ldasoutDf The LDASOUT dataframe
#' @return The input dataframe with new water flux columns added.
#'
#' @examples
#' ## Take a NoahMP LDASOUT dataframe for a model run of Fourmile Creek
#' ## and generate a dataframe with water fluxes added.
#'
#' modLDASOUT1d.nort.fc <- CalcNoahmpFluxes(modLDASOUT1d.nort.fc)
#' @keywords manip
#' @concept aconcept
#' @export
CalcNoahmpFluxes <- function(ldasoutDf) {
    if ("ACCPRCP" %in% colnames(ldasoutDf)) { ldasoutDf$DEL_ACCPRCP[2:nrow(ldasoutDf)] <- diff(ldasoutDf$ACCPRCP) }
    if ("ACCECAN" %in% colnames(ldasoutDf)) { ldasoutDf$DEL_ACCECAN[2:nrow(ldasoutDf)] <- diff(ldasoutDf$ACCECAN) }
    if ("ACCETRAN" %in% colnames(ldasoutDf)) { ldasoutDf$DEL_ACCETRAN[2:nrow(ldasoutDf)] <- diff(ldasoutDf$ACCETRAN) }
    if ("ACCEDIR" %in% colnames(ldasoutDf)) { ldasoutDf$DEL_ACCEDIR[2:nrow(ldasoutDf)] <- diff(ldasoutDf$ACCEDIR) }
    if ("UGDRNOFF" %in% colnames(ldasoutDf)) { ldasoutDf$DEL_UGDRNOFF[2:nrow(ldasoutDf)] <- diff(ldasoutDf$UGDRNOFF) }
    if ("SFCRNOFF" %in% colnames(ldasoutDf)) { ldasoutDf$DEL_SFCRNOFF[2:nrow(ldasoutDf)] <- diff(ldasoutDf$SFCRNOFF) }
    ldasoutDf
}



#' Calculate water balance from WRF-Hydro (w/NoahMP) output
#'
#' \code{CalcNoahmpWatBudg} calculates water budget components from WRF-Hydro (w/NoahMP) model output.
#'
#' \code{CalcNoahmpWatBudg} reads dataframes derived from WRF-Hydro output (i.e., using \code{\link{GetMultiNcdf}})
#' and calculates water budget partitioning (e.g., surface runoff, evaporation, groundwater). Assumes WRF-Hydro
#' output dataframes have already been masked to the desired basin. See \code{\link{GetMultiNcdf}} documentation
#' for examples of how to do this. NOTE: Currently only works for model runs using NoahMP as the LSM.
#'
#' REQUIRED variables (these terms must be in your input dataframes):
#' \itemize{
#'    \item LDASOUT: ACCPRCP, ACCECAN, ACCETRAN, ACCEDIR, SFCRNOFF, UGDRNOFF, SOIL_M (all layers),
#'           SNEQV, CANICE, CANLIQ
#'    \item RTOUT (optional, use if overland or subsurface routing were activated): QSTRMVOLRT, SFCHEADSUBRT, QBDRY
#'    \item GWOUT (optional, use if groundwater bucket model was activated): q_cms, POSIXct
#' }
#'
#' OUTPUT water budget terms (may vary depending on model configuration):
#' \itemize{
#'    \item LSM_PRCP: Total precipitation (mm)
#'    \item LSM_ECAN: Total canopy evaporation (mm)
#'    \item LSM_ETRAN: Total transpiration (mm)
#'    \item LSM_EDIR: Total surface evaporation (mm)
#'    \item LSM_DELSWE: Change in snowpack snow water equivalent (mm)
#'    \item LSM_DELCANWAT: Change in canopy water storage (liquid + ice) (mm)
#'    \item LSM_SFCRNOFF: Surface runoff from LSM \emph{(for an LSM-only run)} (mm)
#'    \item LSM_UGDRNOFF: Subsurface runoff from LSM \emph{(for an LSM-only run)} (mm)
#'    \item LSM_DELSOILM: Change in total soil moisture storage (mm)
#'    \item HYD_QSTRMVOL: Total runoff into channel from land \emph{(routing model only)}  (mm)
#'    \item HYD_DELSFCHEAD: Change in surface storage \emph{(routing model only)} (mm)
#'    \item HYD_QBDRY: Total flow outside of domain \emph{(routing model only)} (mm)
#'    \item HYD_GWOUT: Total groundwater outflow \emph{(routing model only)} (mm)
#'    \item HYD_DELGWSTOR: Change in groundwater storage \emph{(routing model only)} (mm)
#'    \item WB_SFCRNOFF: Total surface runoff used in the water budget calculation (\emph{either} LSM_SFCRNOFF or HYD_QSTRMVOL) (mm)
#'    \item WB_GWOUT: Total groundwater outflow used in the water budget calculation (\emph{either} LSM_UGDRNOFF or HYD_GWOUT) (mm)
#'    \item ERROR: Remainder in water budget (mm)
#'    \item RUN_FRAC: Runoff fraction, runoff/precipitation
#'    \item EVAP_FRAC: Evaporative fraction, evapotranspiration/precipitation
#'    \item STOR_FRAC: Change in storage fraction, storagechange/precipitation
#' }
#' 
#' @param ldasoutDf The LDASOUT dataframe (required)
#' @param rtoutDf The RTOUT dataframe, if overland or subsurface routing was turned on (default=NULL)
#' @param gwoutDf The GW_OUT dataframe, if groundwater model was turned on (default=NULL)
#' @param sfcrt A flag whether surface overland flow routing was active. All other routing
#' options are determined based on the input dataframes, as needed (e.g., if gwoutDf is provided,
#' it is assumed that the groundwater model was active). (default=FALSE)
#' @param soildeps A list of soil layer depths in mm (top to bottom, default=c(100, 300, 600, 1000))
#' @param basarea The basin area in square km (necessary only if gwoutDf is provided)
#' @return A new dataframe containing the water budget components in mm.
#'
#' @examples
#' ## Take a NoahMP LDASOUT dataframe for a model run of Fourmile Creek with no routing
#' ## options turned on and return a water budget summary.
#'
#' wb.nort.fc <- CalcNoahmpWatBudg(modLDASOUT1d.nort.fc)
#' wb.nort.fc
#' ##
#' ## Take NoahMP LDASOUT, HYDRO model RTOUT, and GW_outflow dataframes for a model
#' ## run of Fourmile Creek with subsurface, overland, and groundwater routing options
#' ## turned on and return a water budget summary. The default soil depths were used
#' ## and the basin is 63.1 km2. NOTE: We MUST specify with the sfcrt flag that overland
#' ## flow routing was turned on. Otherwise the LSM surface runoff term is used.
#'
#' wb.allrt.fc <- CalcNoahmpWatBudg(modLDASOUT1d.allrt.fc, modRTOUT1h.allrt.fc, modGWOUT1h.allrt.fc, sfcrt=TRUE, basarea=63.1)
#' wb.allrt.fc
#' @keywords manip
#' @concept aconcept
#' @export
CalcNoahmpWatBudg <- function(ldasoutDf, rtoutDf=NULL, gwoutDf=NULL, sfcrt=FALSE, soildeps=c(100,300,600,1000), basarea=NULL) {
    wbDf <- as.data.frame(t(as.matrix(rep(0, 6))))
    colnames(wbDf) <- c("LSM_PRCP","LSM_ECAN","LSM_ETRAN","LSM_EDIR","LSM_DELSWE","LSM_DELCANWAT")
    # LSM water fluxes for all model cases
    wbDf[1,1] <- ldasoutDf$ACCPRCP[nrow(ldasoutDf)]-ldasoutDf$ACCPRCP[1]
    wbDf[1,2] <- ldasoutDf$ACCECAN[nrow(ldasoutDf)]-ldasoutDf$ACCECAN[1]
    wbDf[1,3] <- ldasoutDf$ACCETRAN[nrow(ldasoutDf)]-ldasoutDf$ACCETRAN[1]
    wbDf[1,4] <- ldasoutDf$ACCEDIR[nrow(ldasoutDf)]-ldasoutDf$ACCEDIR[1]
    wbDf[1,5] <- ldasoutDf$SNEQV[nrow(ldasoutDf)]-ldasoutDf$SNEQV[1]
    wbDf[1,6] <- (ldasoutDf$CANICE[nrow(ldasoutDf)] + ldasoutDf$CANLIQ[nrow(ldasoutDf)]) -
                        (ldasoutDf$CANICE[1] + ldasoutDf$CANLIQ[1])
    # LSM overlap water fluxes
    wbDf[1,"LSM_SFCRNOFF"] <- ldasoutDf$SFCRNOFF[nrow(ldasoutDf)]-ldasoutDf$SFCRNOFF[1]
    wbDf[1,"LSM_UGDRNOFF"] <- ldasoutDf$UGDRNOFF[nrow(ldasoutDf)]-ldasoutDf$UGDRNOFF[1]
    numsoil <- length(soildeps)
    soilm <- 0.0
    for (i in 1:numsoil) {
        soilm <- soilm + (ldasoutDf[nrow(ldasoutDf),paste0("SOIL_M",i)]-ldasoutDf[1,paste0("SOIL_M",i)])*soildeps[i]
        }
    wbDf[1,"LSM_DELSOILM"] <- soilm
    # HYDRO surface/subsurface water fluxes
    if (!is.null(rtoutDf)) {
        wbDf[1,"HYD_QSTRMVOL"] <- rtoutDf$QSTRMVOLRT[nrow(rtoutDf)]-rtoutDf$QSTRMVOLRT[1]
        wbDf[1,"HYD_DELSFCHEAD"] <- rtoutDf$SFCHEADSUBRT[nrow(rtoutDf)]-rtoutDf$SFCHEADSUBRT[1]
        wbDf[1,"HYD_QBDRY"] <- -(rtoutDf$QBDRY[nrow(rtoutDf)]-rtoutDf$QBDRY[1])
        }
    else {
        message('Message: No routing output dataframe (rtoutDf) was provided. Proceeding using LSM surface runoff.')
        wbDf[1,"HYD_QSTRMVOL"] <- NA
        wbDf[1,"HYD_DELSFCHEAD"] <- NA
        wbDf[1,"HYD_QBDRY"] <- NA
        }
    # HYDRO groundwater fluxes
    if (!is.null(gwoutDf)) {
        if (!is.null(basarea)) {
            dt <- as.integer(difftime(gwoutDf$POSIXct[2],gwoutDf$POSIXct[1],units="secs"))
            wbDf[1,"HYD_GWOUT"] <- sum(gwoutDf$q_cms/(basarea*1000*1000)*1000*dt, na.rm=T)
            wbDf[1,"HYD_DELGWSTOR"] <- (ldasoutDf$UGDRNOFF[nrow(ldasoutDf)]-ldasoutDf$UGDRNOFF[1]) - wbDf$HYD_GWOUT
            }
        else { stop('Error: Groundwater outflow dataframe (gwoutDf) provided but no basin area (basarea). Please provide the basin area.') }
        }
    else {
        message('Message: No groundwater outflow dataframe (gwoutDf) was provided. Proceeding using LSM underground runoff.')
        wbDf[1,"HYD_GWOUT"] <- NA
        wbDf[1,"HYD_DELGWSTOR"] <- NA
        }
    # Overland routing check
    if ( sfcrt & is.null(rtoutDf)) {
        stop('Error: Surface overland routing (sfcrt) is active (TRUE) but no routing output dataframe (rtoutDf) was specified.')
        }
    if ( !sfcrt & !is.null(rtoutDf) ) {
        message('Message: A routing output file (rtoutDf) was provided but surface overland routing (sfcrt) is inactive (FALSE). Proceeding using LSM surface runoff.')
        }
    wbDf[1,"WB_SFCRNOFF"] <- if (sfcrt) { wbDf[1, "HYD_QSTRMVOL"] } else { wbDf[1, "LSM_SFCRNOFF"] }
    # Groundwater routing check
    wbDf[1,"WB_GWOUT"] <- if (!is.null(gwoutDf)) { wbDf[1, "HYD_GWOUT"] } else { wbDf[1, "LSM_UGDRNOFF"] }
    # Water budget error
    wbDf[1,"ERROR"] <- with( wbDf, LSM_PRCP - LSM_ECAN - LSM_ETRAN - LSM_EDIR -
                                 WB_SFCRNOFF -
                                 ifelse(is.na(HYD_QBDRY), 0.0, HYD_QBDRY) -
                                 WB_GWOUT - LSM_DELSOILM -
                                 LSM_DELSWE - LSM_DELCANWAT -
                                 ifelse(is.na(HYD_DELSFCHEAD), 0.0, HYD_DELSFCHEAD) -
                                 ifelse(is.na(HYD_DELGWSTOR), 0.0, HYD_DELGWSTOR) )
    # Calculate various fractions
    wbDf[1,"RUN_FRAC"] <- with( wbDf, (WB_SFCRNOFF + ifelse(is.na(HYD_QBDRY), 0.0, HYD_QBDRY) +
                                        WB_GWOUT) / LSM_PRCP )
    wbDf[1,"EVAP_FRAC"] <- with( wbDf, (LSM_ECAN + LSM_ETRAN + LSM_EDIR) / LSM_PRCP )
    wbDf[1,"STOR_FRAC"] <- with( wbDf, (LSM_DELSOILM + LSM_DELSWE + LSM_DELCANWAT +
                                        ifelse(is.na(HYD_DELSFCHEAD), 0.0, HYD_DELSFCHEAD) +
                                        ifelse(is.na(HYD_DELGWSTOR), 0.0, HYD_DELGWSTOR) ) /
                                        LSM_PRCP )
    wbDf
}



#' Computes model performance statistics for WRF-Hydro flux output
#'
#' \code{CalcModPerf} calculates model performance statistics for flux output.
#'
#' \code{CalcModPerf} reads a model flux time series (i.e., created using \code{\link{ReadFrxstPts}}) and
#' an observation time series (i.e., created using \code{\link{ReadUsgsGage}}) and calculates model performance
#' statistics (Nash-Sutcliffe Efficiency, Rmse, etc.) at various time scales and for low
#' and high fluxes. The tool will subset data to matching time periods (e.g., if the
#' observed data is at 5-min increments and modelled data is at 1-hr increments, the tool
#' will subset the observed data to select only observations on the matching hour break).
#'
#' Performance Statistics:
#' \cr (mod = model output, obs = observations, n = sample size)
#' \itemize{
#' \item nse: Nash-Sutcliffe Efficiency
#' \deqn{nse = 1 - ( sum((obs - mod)^2) / sum((obs - mean(obs))^2) ) }
#' \item nselog: log-transformed Nash-Sutcliffe Efficiency
#' \deqn{nselog = 1 - ( sum((log(obs) - log(mod))^2) / sum((log(obs) - mean(log(obs)))^2) ) }
#' \item cor: correlation coefficient
#' \deqn{cor = cor(mod, obs) }
#' \item rmse: root mean squared error
#' \deqn{rmse = sqrt( sum((mod - obs)^2) / n ) }
#' \item rmsenorm: normalized root mean squared error
#' \deqn{rmsenorm = rmse / (max(obs) - min(obs)) }
#' \item bias: percent bias
#' \deqn{bias = sum(mod - obs) / sum(obs) * 100 }
#' \item mae: mean absolute error
#' \deqn{mae = mean(abs(mod - obs)) }
#' \item errcom: error in the center-of-mass of the flux, where center-of-mass is the
#' hour/day when 50\% of daily/monthly/water-year flux has occurred. Reported as number of hours for
#' daily time scale and number of days for monthly and yearly time scales.
#' \item errmaxt: Error in the time of maximum flux. Reported as number of hours for daily time scale
#' and number of days for monthly and yearly time scales).
#' \item errfdc: Error in the integrated flow duration curve between 0.05 and 0.95 exceedance thresholds
#' (in native flow units).
#' }
#'
#' Time scales/Flux types:
#' \itemize{
#' \item ts = native model/observation time step (e.g., hourly)
#' \item daily = daily time step
#' \item monthly = monthly time step
#' \item yearly = water-year time step
#' \item max10 = high flows; restricted to the portion of the time series where the observed flux is in the highest 10\%
#' \item min10 = low flows; restricted to the portion of the time series where the observed flux is in the lowest 10\%
#' }
#'
#' @param flxDf.mod The flux output dataframe (required). Assumes only one forecast
#' point per file, so if you have multiple forecast points in your output dataframe, use
#' subset to isolate a single forecast point's data. Also assumes model output and observation
#' both contain POSIXct fields (called "POSIXct").
#' @param flxDf.obs The observed flux dataframe. Assumes only one observation point per file, so if
#' you have multiple observation points in your dataframe, use subset to isolate a single point's data.
#' Also assumes model output and observation both contain POSIXct fields (called "POSIXct").
#' @param flxCol.mod The column name for the flux time series for the MODEL data (default="q_cms")
#' @param flxCol.obs The column name for the flux time series for the OBSERVED data (default="q_cms")
#' @param stdate Start date for plot/statistics (DEFAULT=NULL, all records will be used).
#' Date MUST be specified in POSIXct format with appropriate timezone
#' (e.g., as.POSIXct("2013-05-01 00:00:00", format="\%Y-\%m-\%d \%H:\%M:\%S", tz="UTC"))
#' @param enddate End date for plot/statistics (DEFAULT=NULL, all records will be used).
#' Date MUST be specified in POSIXct format with appropriate timezone
#' (e.g., as.POSIXct("2013-05-01 00:00:00", format="\%Y-\%m-\%d \%H:\%M:\%S", tz="UTC"))
#' @return A new dataframe containing the model performance statistics.
#'
#' @examples
#' ## Take forecast point model output for Fourmile Creek (modStrh.mod1.fc) and a corresponding
#' ## USGS gage observation file (obsStrh.fc), both at an hourly time step, and calculate
#' ## model performance statistics. The model forecast point data was imported using ReadFrxstPts
#' ## and the gage observation data was imported using ReadUsgsGage.
#'
#' CalcModPerf(modStr1h.allrt.fc, obsStr5min.fc)
#'
#' > Output:
#'           nse nselog  cor rmse rmsenorm  bias  mae errcom errmaxt errfdc
#' ts       0.57   0.61 0.79 1.43     9.48 -28.0 0.70     NA      NA  -0.42
#' daily    0.71   0.64 0.87 1.17     9.86 -28.1 0.61   0.19   -2.25  -0.37
#' monthly  0.80   0.70 0.93 0.89    12.73 -26.6 0.53  -0.18   -0.96     NA
#' yearly   0.05   0.37 0.36 0.55    41.50  -6.5 0.45  -1.50   -3.38     NA
#' max10   -7.50 -15.94 0.19 3.82    38.89 -24.5 0.04     NA      NA     NA
#' min10   -2.84  -1.83 0.10 0.05    33.36 -23.7   NA     NA      NA     NA
#' @keywords univar ts
#' @concept aconcept
#' @export
CalcModPerf <- function (flxDf.mod, flxDf.obs, flxCol.mod="q_cms", flxCol.obs="q_cms", stdate=NULL, enddate=NULL) {
    # Prepare data
    if (!is.null(stdate) && !is.null(enddate)) {
        flxDf.obs <- subset(flxDf.obs, POSIXct>=stdate & POSIXct<=enddate)
        flxDf.mod <- subset(flxDf.mod, POSIXct>=stdate & POSIXct<=enddate)
        }
    flxDf.mod <- CalcDates(flxDf.mod)
    flxDf.mod$qcomp <- flxDf.mod[,flxCol.mod]
    flxDf.obs$qcomp <- flxDf.obs[,flxCol.obs]
    if (as.integer(flxDf.obs$POSIXct[2])-as.integer(flxDf.obs$POSIXct[1]) >= 86400) {flxDf.obs$POSIXct=as.POSIXct(round(flxDf.obs$POSIXct,"days"), tz="UTC")}
    flxDf.mod <- merge(flxDf.mod, flxDf.obs[c("POSIXct","qcomp")], by<-c("POSIXct"), suffixes=c(".mod",".obs"))
    #flxDf.mod <- subset(flxDf.mod, !is.na(flxDf.mod$qcomp.mod) & !is.na(flxDf.mod$qcomp.obs))
    flxDf.mod$date <- as.POSIXct(round(flxDf.mod$POSIXct, "days"))
    results <- as.data.frame(matrix(nrow = 6, ncol = 10))
    colnames(results) = c("nse", "nselog", "cor", "rmse", "rmsenorm", "bias", "mae", "errcom", "errmaxt", "errfdc")
    rownames(results) = c("ts", "daily", "monthly", "yearly", "max10", "min10")
    exclvars <- names(flxDf.mod) %in% c("POSIXct", "secs", "timest", "date", "stat")
    # Base aggregations
    flxDf.mod.d <- aggregate(flxDf.mod[!exclvars], by = list(flxDf.mod$date), CalcMeanNarm)
    flxDf.mod.mwy <- aggregate(flxDf.mod[c("qcomp.mod","qcomp.obs")], by = list(flxDf.mod$month, flxDf.mod$wy), CalcMeanNarm)
    flxDf.mod.wy <- aggregate(flxDf.mod[c("qcomp.mod","qcomp.obs")], by = list(flxDf.mod$wy), CalcMeanNarm)
    # Time of center of mass aggregations
    flxDf.mod.dcom <- aggregate(flxDf.mod[c("qcomp.mod","qcomp.obs")], by = list(flxDf.mod$date), CalcCOM)
    flxDf.mod.mwycom <- aggregate(flxDf.mod.d[c("qcomp.mod","qcomp.obs")], by = list(flxDf.mod.d$month, flxDf.mod.d$wy), CalcCOM)
    flxDf.mod.wycom <- aggregate(flxDf.mod.d[c("qcomp.mod","qcomp.obs")], by = list(flxDf.mod.d$wy), CalcCOM)
    # Time of max aggregations
    flxDf.mod.dmax <- aggregate(flxDf.mod[c("qcomp.mod","qcomp.obs")], by = list(flxDf.mod$date), which.max)
    flxDf.mod.mwymax <- aggregate(flxDf.mod.d[c("qcomp.mod","qcomp.obs")], by = list(flxDf.mod.d$month, flxDf.mod.d$wy), which.max)
    flxDf.mod.wymax <- aggregate(flxDf.mod.d[c("qcomp.mod","qcomp.obs")], by = list(flxDf.mod.d$wy), which.max)
    # NAs cause which.max to return a list, so force back to ints
    flxDf.mod.dmax$qcomp.mod <- as.integer(flxDf.mod.dmax$qcomp.mod)
    flxDf.mod.dmax$qcomp.obs <- as.integer(flxDf.mod.dmax$qcomp.obs)
    flxDf.mod.mwymax$qcomp.mod <- as.integer(flxDf.mod.mwymax$qcomp.mod)
    flxDf.mod.mwymax$qcomp.obs <- as.integer(flxDf.mod.mwymax$qcomp.obs)
    flxDf.mod.wymax$qcomp.mod <- as.integer(flxDf.mod.wymax$qcomp.mod)
    flxDf.mod.wymax$qcomp.obs <- as.integer(flxDf.mod.wymax$qcomp.obs)
    # Mins and Maxes
    flxDf.mod.max10 <- subset(flxDf.mod, flxDf.mod$qcomp.obs>=quantile(flxDf.mod$qcomp.obs, 0.90, na.rm=TRUE))
    flxDf.mod.min10 <- subset(flxDf.mod, flxDf.mod$qcomp.obs<=quantile(flxDf.mod$qcomp.obs, 0.10, na.rm=TRUE))
    # FDCs
    flxDf.mod <- CalcFdc(flxDf.mod, "qcomp.mod")
    flxDf.mod <- CalcFdc(flxDf.mod, "qcomp.obs")
    flxDf.mod.d <- CalcFdc(flxDf.mod.d, "qcomp.mod")
    flxDf.mod.d <- CalcFdc(flxDf.mod.d, "qcomp.obs")

    # Compile summary statistics
    results["ts", "nse"] <- round(Nse(flxDf.mod$qcomp.mod, flxDf.mod$qcomp.obs), 2)
    results["ts", "nselog"] <- round(NseLog(flxDf.mod$qcomp.mod, flxDf.mod$qcomp.obs), 2)
    results["ts", "cor"] <- round(cor(flxDf.mod$qcomp.mod, flxDf.mod$qcomp.obs, use="na.or.complete"), 2)
    results["ts", "rmse"] <- round(Rmse(flxDf.mod$qcomp.mod, flxDf.mod$qcomp.obs), 2)
    results["ts", "rmsenorm"] <- round(RmseNorm(flxDf.mod$qcomp.mod, flxDf.mod$qcomp.obs), 2)
    results["ts", "bias"] <- round(sum(flxDf.mod$qcomp.mod-flxDf.mod$qcomp.obs, na.rm=T)/sum(flxDf.mod$qcomp.obs, na.rm=TRUE) * 100, 1)
    results["ts", "mae"] <- round(mean(abs(flxDf.mod$qcomp.mod-flxDf.mod$qcomp.obs), na.rm=T), 2)
    results["ts", "errcom"] <- NA
    results["ts", "errmaxt"] <- NA
    results["ts", "errfdc"] <- round(integrate(splinefun(flxDf.mod[,"qcomp.mod.fdc"], flxDf.mod[,"qcomp.mod"], method='natural'), 0.05, 0.95, subdivisions=1000)$value -
                                integrate(splinefun(flxDf.mod[,"qcomp.obs.fdc"], flxDf.mod[,"qcomp.obs"], method='natural'), 0.05, 0.95, subdivisions=1000)$value, 2 )
    results["daily", "nse"] <- round(Nse(flxDf.mod.d$qcomp.mod, flxDf.mod.d$qcomp.obs), 2)
    results["daily", "nselog"] <- round(NseLog(flxDf.mod.d$qcomp.mod, flxDf.mod.d$qcomp.obs), 2)
    results["daily", "cor"] <- round(cor(flxDf.mod.d$qcomp.mod, flxDf.mod.d$qcomp.obs, use="na.or.complete"), 2)
    results["daily", "rmse"] <- round(Rmse(flxDf.mod.d$qcomp.mod, flxDf.mod.d$qcomp.obs), 2)
    results["daily", "rmsenorm"] <- round(RmseNorm(flxDf.mod.d$qcomp.mod, flxDf.mod.d$qcomp.obs), 2)
    results["daily", "bias"] <- round(sum(flxDf.mod.d$qcomp.mod-flxDf.mod.d$qcomp.obs, na.rm=T)/sum(flxDf.mod.d$qcomp.obs, na.rm=TRUE) * 100, 1)
    results["daily", "mae"] <- round(mean(abs(flxDf.mod.d$qcomp.mod-flxDf.mod.d$qcomp.obs), na.rm=T), 2)
    results["daily", "errcom"] <- round(mean(flxDf.mod.dcom$qcomp.mod-flxDf.mod.dcom$qcomp.obs, na.rm=T), 2)
    results["daily", "errmaxt"] <- round(mean(flxDf.mod.dmax$qcomp.mod-flxDf.mod.dmax$qcomp.obs, na.rm=T), 2)
    results["daily", "errfdc"] <- round(integrate(splinefun(flxDf.mod.d[,"qcomp.mod.fdc"], flxDf.mod.d[,"qcomp.mod"], method='natural'), 0.05, 0.95, subdivisions=1000)$value -
                                    integrate(splinefun(flxDf.mod.d[,"qcomp.obs.fdc"], flxDf.mod.d[,"qcomp.obs"], method='natural'), 0.05, 0.95, subdivisions=1000)$value, 2 )
    results["monthly", "nse"] <- round(Nse(flxDf.mod.mwy$qcomp.mod, flxDf.mod.mwy$qcomp.obs), 2)
    results["monthly", "nselog"] <- round(NseLog(flxDf.mod.mwy$qcomp.mod, flxDf.mod.mwy$qcomp.obs), 2)
    results["monthly", "cor"] <- round(cor(flxDf.mod.mwy$qcomp.mod, flxDf.mod.mwy$qcomp.obs, use="na.or.complete"), 2)
    results["monthly", "rmse"] <- round(Rmse(flxDf.mod.mwy$qcomp.mod, flxDf.mod.mwy$qcomp.obs), 2)
    results["monthly", "rmsenorm"] <- round(RmseNorm(flxDf.mod.mwy$qcomp.mod, flxDf.mod.mwy$qcomp.obs), 2)
    results["monthly", "bias"] <- round(sum(flxDf.mod.mwy$qcomp.mod-flxDf.mod.mwy$qcomp.obs, na.rm=T)/sum(flxDf.mod.mwy$qcomp.obs, na.rm=TRUE) * 100, 1)
    results["monthly", "mae"] <- round(mean(abs(flxDf.mod.mwy$qcomp.mod-flxDf.mod.mwy$qcomp.obs), na.rm=T), 2)
    results["monthly", "errcom"] <- round(mean(flxDf.mod.mwycom$qcomp.mod-flxDf.mod.mwycom$qcomp.obs, na.rm=T), 2)
    results["monthly", "errmaxt"] <- round(mean(flxDf.mod.mwymax$qcomp.mod-flxDf.mod.mwymax$qcomp.obs, na.rm=T), 2)
    results["monthly", "errfdc"] <- NA
    results["yearly", "nse"] <- round(Nse(flxDf.mod.wy$qcomp.mod, flxDf.mod.wy$qcomp.obs), 2)
    results["yearly", "nselog"] <- round(NseLog(flxDf.mod.wy$qcomp.mod, flxDf.mod.wy$qcomp.obs), 2)
    results["yearly", "cor"] <- round(cor(flxDf.mod.wy$qcomp.mod, flxDf.mod.wy$qcomp.obs, use="na.or.complete"), 2)
    results["yearly", "rmse"] <- round(Rmse(flxDf.mod.wy$qcomp.mod, flxDf.mod.wy$qcomp.obs), 2)
    results["yearly", "rmsenorm"] <- round(RmseNorm(flxDf.mod.wy$qcomp.mod, flxDf.mod.wy$qcomp.obs), 2)
    results["yearly", "bias"] <- round(sum(flxDf.mod.wy$qcomp.mod-flxDf.mod.wy$qcomp.obs, na.rm=T)/sum(flxDf.mod.wy$qcomp.obs, na.rm=TRUE) * 100, 1)
    results["yearly", "mae"] <- round(mean(abs(flxDf.mod.wy$qcomp.mod-flxDf.mod.wy$qcomp.obs), na.rm=T), 2)
    results["yearly", "errcom"] <- round(mean(flxDf.mod.wycom$qcomp.mod-flxDf.mod.wycom$qcomp.obs, na.rm=T), 2)
    results["yearly", "errmaxt"] <- round(mean(flxDf.mod.wymax$qcomp.mod-flxDf.mod.wymax$qcomp.obs, na.rm=T), 2)
    results["yearly", "errfdc"] <- NA
    results["max10", "nse"] <- round(Nse(flxDf.mod.max10$qcomp.mod, flxDf.mod.max10$qcomp.obs), 2)
    results["max10", "nselog"] <- round(NseLog(flxDf.mod.max10$qcomp.mod, flxDf.mod.max10$qcomp.obs), 2)
    results["max10", "cor"] <- round(cor(flxDf.mod.max10$qcomp.mod, flxDf.mod.max10$qcomp.obs, use="na.or.complete"), 2)
    results["max10", "rmse"] <- round(Rmse(flxDf.mod.max10$qcomp.mod, flxDf.mod.max10$qcomp.obs), 2)
    results["max10", "rmsenorm"] <- round(RmseNorm(flxDf.mod.max10$qcomp.mod, flxDf.mod.max10$qcomp.obs), 2)
    results["max10", "bias"] <- round(sum(flxDf.mod.max10$qcomp.mod-flxDf.mod.max10$qcomp.obs, na.rm=T)/sum(flxDf.mod.max10$qcomp.obs, na.rm=TRUE) * 100, 1)
    results["max10", "mae"] <- round(mean(abs(flxDf.mod.max10$qcomp.mod-flxDf.mod.max10$qcomp.obs), na.rm=T), 2)
    results["max10", "errcom"] <- NA
    results["max10", "errmaxt"] <- NA
    results["max10", "errfdc"] <- NA
    results["min10", "nse"] <- round(Nse(flxDf.mod.min10$qcomp.mod, flxDf.mod.min10$qcomp.obs), 2)
    results["min10", "nselog"] <- round(NseLog(flxDf.mod.min10$qcomp.mod, flxDf.mod.min10$qcomp.obs), 2)
    results["min10", "cor"] <- round(cor(flxDf.mod.min10$qcomp.mod, flxDf.mod.min10$qcomp.obs, use="na.or.complete"), 2)
    results["min10", "rmse"] <- round(Rmse(flxDf.mod.min10$qcomp.mod, flxDf.mod.min10$qcomp.obs), 2)
    results["min10", "rmsenorm"] <- round(RmseNorm(flxDf.mod.min10$qcomp.mod, flxDf.mod.min10$qcomp.obs), 2)
    results["min10", "bias"] <- round(sum(flxDf.mod.min10$qcomp.mod-flxDf.mod.min10$qcomp.obs, na.rm=T)/sum(flxDf.mod.min10$qcomp.obs, na.rm=TRUE) * 100, 1)
    results["max10", "mae"] <- round(mean(abs(flxDf.mod.min10$qcomp.mod-flxDf.mod.min10$qcomp.obs), na.rm=T), 2)
    results["min10", "errcom"] <- NA
    results["min10", "errmaxt"] <- NA
    results["min10", "errfdc"] <- NA
    results
 }



#' Computes flow duration curve statistics for WRF-Hydro streamflow output
#'
#' \code{CalcFdcPerf} calculates flow duration curve statistics for streamflow output.
#'
#' \code{CalcFdcPerf} reads a model forecast point streamflow timeseries (i.e., created using \code{\link{ReadFrxstPts}}) and
#' a streamflow observation timeseries (i.e., created using \code{\link{ReadUsgsGage}}) and calculates flow duration curve
#' statistics at various exceedance thresholds (e.g., 10\%, 20\%, etc.). The tool will subset data to matching time periods
#' (e.g., if the observed data is at 5-min increments and modelled data is at 1-hr increments, the tool
#' will subset the observed data to select only observations on the matching hour break).
#'
#' Flow Duration Curve Statistics:
#' \cr (mod = model output, obs = observations)
#' \itemize{
#' \item p.exceed: exceedance threshold (e.g., 0.2 means a flow value that is exceeded 20\% of the time)
#' \item q.mod: MODEL flow value at specified exceedance threshold (in native flow units)
#' \item q.obs: OBSERVED flow value at specified exceedance threshold (in native flow units)
#' \item q.err: difference between model and observed flow values [mod-obs] (in native flow units)
#' \item q.perr: percent error in model flow [(mod-obs)/obs]
#' }
#'
#' @param strDf.mod The forecast point output dataframe (required). Assumes only one forecast
#' point per file, so if you have multiple forecast points in your output dataframe, use
#' subset to isolate a single forecast point's data. Also assumes model output and observation
#' both contain POSIXct fields (called "POSIXct").
#' @param strDf.obs The observed streamflow dataframe. Assumes only one gage per file, so if
#' you have multiple gages in your dataframe, use subset to isolate a single gage's data.
#' Also assumes model output and observation both contain POSIXct fields (called "POSIXct").
#' @param strCol.mod The column name for the streamflow time series for the MODEL data (default="q_cms")
#' @param strCol.obs The column name for the streamflow time series for the OBSERVED data (default="q_cms")
#' @param stdate Start date for plot/statistics (DEFAULT=NULL, all records will be used).
#' Date MUST be specified in POSIXct format with appropriate timezone
#' (e.g., as.POSIXct("2013-05-01 00:00:00", format="\%Y-\%m-\%d \%H:\%M:\%S", tz="UTC"))
#' @param enddate End date for plot/statistics (DEFAULT=NULL, all records will be used).
#' Date MUST be specified in POSIXct format with appropriate timezone
#' (e.g., as.POSIXct("2013-05-01 00:00:00", format="\%Y-\%m-\%d \%H:\%M:\%S", tz="UTC"))
#' @return A new dataframe containing the flow duration curve statistics.
#'
#' @examples
#' ## Take forecast point model output for Fourmile Creek (modStrh.mod1.fc) and a corresponding
#' ## USGS gage observation file (obsStrh.fc), both at an hourly time step, and calculate
#' ## flow duration curve statistics. The model forecast point data was imported using ReadFrxstPts
#' ## and the gage observation data was imported using ReadUsgsGage.
#'
#' CalcFdcPerf(modStr1h.allrt.fc, obsStr5min.fc)
#'
#' Output:
#'  p.exceed    q.mod   q.obs
#'  0.1         3.07    5.25
#'  0.2         1.35    2.31
#'  0.3         0.82    1.06
#'  0.4         0.48    0.65
#'  0.5         0.29    0.45
#'  0.6         0.18    0.34
#'  0.7         0.14    0.25
#'  0.8         0.11    0.19
#'  0.9         0.08    0.16
#' @keywords univar ts
#' @concept aconcept
#' @export
CalcFdcPerf <- function (strDf.mod, strDf.obs, strCol.mod="q_cms", strCol.obs="q_cms", stdate=NULL, enddate=NULL) {
    # Prepare data
    if (!is.null(stdate) && !is.null(enddate)) {
        strDf.obs <- subset(strDf.obs, POSIXct>=stdate & POSIXct<=enddate)
        strDf.mod <- subset(strDf.mod, POSIXct>=stdate & POSIXct<=enddate)
        }
    strDf.mod <- CalcDates(strDf.mod)
    strDf.mod$qcomp <- strDf.mod[,strCol.mod]
    strDf.obs$qcomp <- strDf.obs[,strCol.obs]
    if (as.integer(strDf.obs$POSIXct[2])-as.integer(strDf.obs$POSIXct[1]) >= 86400) {strDf.obs$POSIXct=as.POSIXct(round(strDf.obs$POSIXct,"days"), tz="UTC")}
    strDf.mod <- merge(strDf.mod, strDf.obs[c("POSIXct","qcomp")], by<-c("POSIXct"), suffixes=c(".mod",".obs"))
    #strDf.mod <- subset(strDf.mod, !is.na(strDf$qcomp.mod) & !is.na(strDf$qcomp.obs))
    strDf.mod <- CalcFdc(strDf.mod, "qcomp.mod")
    strDf.mod <- CalcFdc(strDf.mod, "qcomp.obs")
    fdc.mod <- splinefun(strDf.mod[,"qcomp.mod.fdc"], strDf.mod[,"qcomp.mod"], method='natural')
    fdc.obs <- splinefun(strDf.mod[,"qcomp.obs.fdc"], strDf.mod[,"qcomp.obs"], method='natural')

    # Compile summary statistics
    results <- as.data.frame(matrix(nrow = 9, ncol = 5))
    colnames(results) <- c("p.exceed","q.mod","q.obs","q.err","q.perr")
    results[, 1] <- seq(0.1, 0.9, 0.1)
    for (i in 1:9) {
        results[i, "q.mod"] <- round(fdc.mod(results[i,1]), 2)
        results[i, "q.obs"] <- round(fdc.obs(results[i,1]), 2)
        results[i, "q.err"] <- results[i, "q.mod"] - results[i, "q.obs"]
        results[i, "q.perr"] <- round(results[i, "q.err"] / results[i, "q.obs"] * 100, 1)
        }
    results
 }


