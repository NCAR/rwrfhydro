#' Calculate water fluxes from NoahMP output
#'
#' \code{CalcNoahmpFluxes} calculates water balance fluxes from accumulated water terms.
#'
#' Read a dataframe derived from NoahMP LDASOUT output (i.e., using \code{\link{GetMultiNcdf}}) and
#' calculate water budget component fluxes from accumulated water variables.
#' 
#' @param ldasoutDf The LDASOUT dataframe
#' @return The input dataframe with new water flux columns added.
#'
#' @examples
#' ## Take a NoahMP LDASOUT dataframe for a model run of Fourmile Creek
#' ## and generate a dataframe with water fluxes added.
#'
#' modLDASOUT.mod1.fc <- CalcNoahmpFluxes(modLDASOUT.mod1.fc)
#' @export

CalcNoahmpFluxes <- function(ldasoutDf) {
    ldasoutDf$DEL_ACCPRCP[2:nrow(ldasoutDf)] <- diff(ldasoutDf$ACCPRCP)
    ldasoutDf$DEL_ACCECAN[2:nrow(ldasoutDf)] <- diff(ldasoutDf$ACCECAN)
    ldasoutDf$DEL_ACCETRAN[2:nrow(ldasoutDf)] <- diff(ldasoutDf$ACCETRAN)
    ldasoutDf$DEL_ACCEDIR[2:nrow(ldasoutDf)] <- diff(ldasoutDf$ACCEDIR)
    ldasoutDf$DEL_UGDRNOFF[2:nrow(ldasoutDf)] <- diff(ldasoutDf$UGDRNOFF)
    ldasoutDf$DEL_SFCRNOFF[2:nrow(ldasoutDf)] <- diff(ldasoutDf$SFCRNOFF)
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
#'    \item PRCP: Total precipitation (mm)
#'    \item ECAN: Total canopy evaporation (mm)
#'    \item ETRAN: Total transpiration (mm)
#'    \item EDIR: Total surface evaporation (mm)
#'    \item DEL_SOILM: Change in total soil moisture storage (mm)
#'    \item DEL_SWE: Change in snowpack snow water equivalent (mm)
#'    \item DEL_CANWAT: Change in canopy water storage (liquid + ice) (mm)
#'    \item SFCRNOFF: Surface runoff from LSM \emph{(for an LSM-only run)} (mm)
#'    \item UGDRNOFF: Subsurface runoff from LSM \emph{(for an LSM-only run)} (mm)
#'    \item QSTRMVOL: Total runoff into channel from land \emph{(routing model only)}  (mm)
#'    \item DEL_SFCHEAD: Change in surface storage \emph{(routing model only)} (mm)
#'    \item QBDRY: Total flow outside of domain \emph{(routing model only)} (mm)
#'    \item GWOUT: Total groundwater outflow \emph{(routing model only)} (mm)
#'    \item DEL_GWSTOR: Change in groundwater storage \emph{(routing model only)} (mm)
#'    \item ERROR: Remainder in water budget (mm)
#'    \item RUN_FRAC: Runoff fraction, runoff/precipitation
#'    \item EVAP_FRAC: Evaporative fraction, evapotranspiration/precipitation
#'    \item STOR_FRAC: Change in storage fraction, storagechange/precipitation
#' }
#' 
#' @param ldasoutDf The LDASOUT dataframe (required)
#' @param rtoutDf The RTOUT dataframe, if overland or subsurface routing was turned on (default=NULL)
#' @param gwoutDf The GW_OUT dataframe, if groundwater model was turned on (default=NULL)
#' @param soildeps A list of soil layer depths in mm (top to bottom, default=c(100, 300, 600, 1000))
#' @param basarea The basin area in square km (necessary only if gwoutDf is provided)
#' @return A new dataframe containing the water budget components in mm.
#'
#' @examples
#' ## Take a NoahMP LDASOUT dataframe for a model run of Fourmile Creek with no routing
#' ## options turned on and return a water budget summary.
#'
#' wb.mod1.fc <- CalcNoahmpWatBudg(modLDASOUT.mod1.fc)
#' wb.mod1.fc
#' ##
#' ## Take NoahMP LDASOUT, HYDRO model RTOUT, and GW_outflow dataframes for a model
#' ## run of Fourmile Creek with subsurface, overland, and groundwater routing options
#' ## turned on and return a water budget summary. The default soil depths were used
#' ## and the basin is 63.1 km2.
#'
#' wb.mod2.fc <- CalcNoahmpWatBudg(modLDASOUT.mod2.fc, modRTOUT.mod2.fc, modGWout.mod2.fc, basarea=63.1)
#' wb.mod2.fc
#' @export

CalcNoahmpWatBudg <- function(ldasoutDf, rtoutDf=NULL, gwoutDf=NULL, soildeps=c(100,300,600,1000), basarea=NULL) {
# NO ROUTING CASE
if (is.null(rtoutDf) && is.null(gwoutDf)) {
    wbDf <- as.data.frame(t(as.matrix(rep(0, 13))))
    colnames(wbDf) <- c("PRCP","ECAN","ETRAN","EDIR",
                      "SFCRNOFF","UGDRNOFF",
                      "DEL_SOILM","DEL_SWE","DEL_CANWAT",
                      "ERROR","RUN_FRAC","EVAP_FRAC","STOR_FRAC")
    wbDf[1,1] <- ldasoutDf$ACCPRCP[nrow(ldasoutDf)]-ldasoutDf$ACCPRCP[1]
    wbDf[1,2] <- ldasoutDf$ACCECAN[nrow(ldasoutDf)]-ldasoutDf$ACCECAN[1]
    wbDf[1,3] <- ldasoutDf$ACCETRAN[nrow(ldasoutDf)]-ldasoutDf$ACCETRAN[1]
    wbDf[1,4] <- ldasoutDf$ACCEDIR[nrow(ldasoutDf)]-ldasoutDf$ACCEDIR[1]
    wbDf[1,5] <- ldasoutDf$SFCRNOFF[nrow(ldasoutDf)]-ldasoutDf$SFCRNOFF[1]
    wbDf[1,6] <- ldasoutDf$UGDRNOFF[nrow(ldasoutDf)]-ldasoutDf$UGDRNOFF[1]
    numsoil <- length(soildeps)
    soilm <- 0.0
    for (i in 1:numsoil) {
        soilm <- soilm + (ldasoutDf[nrow(ldasoutDf),paste0("SOIL_M",i)]-ldasoutDf[1,paste0("SOIL_M",i)])*soildeps[i]
        }
    wbDf[1,7] <- soilm
    wbDf[1,8] <- ldasoutDf$SNEQV[nrow(ldasoutDf)]-ldasoutDf$SNEQV[1]
    wbDf[1,9] <- (ldasoutDf$CANICE[nrow(ldasoutDf)] + ldasoutDf$CANLIQ[nrow(ldasoutDf)]) - 
                        (ldasoutDf$CANICE[1] + ldasoutDf$CANLIQ[1])
    wbDf[1,10] <- with(wbDf,PRCP - ECAN - ETRAN - EDIR - SFCRNOFF - UGDRNOFF - 
                                  DEL_SOILM - DEL_SWE - DEL_CANWAT)
    wbDf[1,11] <- with(wbDf,(SFCRNOFF+UGDRNOFF)/PRCP)
    wbDf[1,12] <- with(wbDf,(ECAN+ETRAN+EDIR)/PRCP)
    wbDf[1,13] <- with(wbDf,(DEL_SOILM+DEL_SWE+DEL_CANWAT)/PRCP)
    }
# GW ROUTING ONLY CASE
else if (is.null(rtoutDf) && !is.null(gwoutDf)) {
    dt <- as.integer(difftime(gwoutDf$POSIXct[2],gwoutDf$POSIXct[1],units="secs"))
    wbDf <- as.data.frame(t(as.matrix(rep(0, 14))))
    colnames(wbDf) <- c("PRCP","ECAN","ETRAN","EDIR",
                      "SFCRNOFF",
                      "DEL_SOILM","DEL_SWE","DEL_CANWAT", 
                      "GWOUT", "DEL_GWSTOR", 
                      "ERROR","RUN_FRAC","EVAP_FRAC","STOR_FRAC")
    wbDf[1,1] <- ldasoutDf$ACCPRCP[nrow(ldasoutDf)]-ldasoutDf$ACCPRCP[1]
    wbDf[1,2] <- ldasoutDf$ACCECAN[nrow(ldasoutDf)]-ldasoutDf$ACCECAN[1]
    wbDf[1,3] <- ldasoutDf$ACCETRAN[nrow(ldasoutDf)]-ldasoutDf$ACCETRAN[1]
    wbDf[1,4] <- ldasoutDf$ACCEDIR[nrow(ldasoutDf)]-ldasoutDf$ACCEDIR[1]
    wbDf[1,5] <- ldasoutDf$SFCRNOFF[nrow(ldasoutDf)]-ldasoutDf$SFCRNOFF[1]
    numsoil <- length(soildeps)
    soilm <- 0.0
    for (i in 1:numsoil) {
        soilm <- soilm + (ldasoutDf[nrow(ldasoutDf),paste0("SOIL_M",i)]-ldasoutDf[1,paste0("SOIL_M",i)])*soildeps[i]
        }
    wbDf[1,6] <- soilm
    wbDf[1,7] <- ldasoutDf$SNEQV[nrow(ldasoutDf)]-ldasoutDf$SNEQV[1]
    wbDf[1,8] <- (ldasoutDf$CANICE[nrow(ldasoutDf)] + ldasoutDf$CANLIQ[nrow(ldasoutDf)]) - 
                        (ldasoutDf$CANICE[1] + ldasoutDf$CANLIQ[1])
    wbDf[1,9] <- sum(gwoutDf$q_cms/(basarea*1000*1000)*1000*dt, na.rm=T)
    wbDf[1,10] <- ldasoutDf$UGDRNOFF[nrow(ldasoutDf)] - wbDf$GWOUT
    wbDf[1,11] <- with(wbDf, PRCP - ECAN - ETRAN - EDIR - SFCRNOFF - GWOUT - 
                                  DEL_SOILM - DEL_SWE - DEL_CANWAT - DEL_GWSTOR)
    wbDf[1,12] <- with(wbDf,(SFCRNOFF+GWOUT)/PRCP)
    wbDf[1,13] <- with(wbDf,(ECAN+ETRAN+EDIR)/PRCP)
    wbDf[1,14] <- with(wbDf,(DEL_SOILM+DEL_SWE+DEL_CANWAT+DEL_GWSTOR)/PRCP)
    }
# OVERLAND/SUBSURFACE ROUTING ONLY CASE
else if (!is.null(rtoutDf) && is.null(gwoutDf)) {
   wbDf <- as.data.frame(t(as.matrix(rep(0, 16))))
   colnames(wbDf) <- c("PRCP","ECAN","ETRAN","EDIR","UGDRNOFF",
                      "DEL_SOILM","DEL_SWE","DEL_CANWAT",
                      "QSTRMVOL","DEL_SFCHEAD","QBDRY",
                      "ERROR","RUN_FRAC","EVAP_FRAC","STOR_FRAC","RECH_FRAC")
   wbDf[1,1] <- ldasoutDf$ACCPRCP[nrow(ldasoutDf)]-ldasoutDf$ACCPRCP[1]
   wbDf[1,2] <- ldasoutDf$ACCECAN[nrow(ldasoutDf)]-ldasoutDf$ACCECAN[1]
   wbDf[1,3] <- ldasoutDf$ACCETRAN[nrow(ldasoutDf)]-ldasoutDf$ACCETRAN[1]
   wbDf[1,4] <- ldasoutDf$ACCEDIR[nrow(ldasoutDf)]-ldasoutDf$ACCEDIR[1]
   wbDf[1,5] <- ldasoutDf$UGDRNOFF[nrow(ldasoutDf)]-ldasoutDf$UGDRNOFF[1]
   numsoil <- length(soildeps)
   soilm <- 0.0
   for (i in 1:numsoil) {
        soilm <- soilm + (ldasoutDf[nrow(ldasoutDf),paste0("SOIL_M",i)]-ldasoutDf[1,paste0("SOIL_M",i)])*soildeps[i]
        }
   wbDf[1,6] <- soilm
   wbDf[1,7] <- ldasoutDf$SNEQV[nrow(ldasoutDf)]-ldasoutDf$SNEQV[1]
   wbDf[1,8] <- (ldasoutDf$CANICE[nrow(ldasoutDf)] + ldasoutDf$CANLIQ[nrow(ldasoutDf)]) - 
               (ldasoutDf$CANICE[1] + ldasoutDf$CANLIQ[1])
   wbDf[1,9] <- rtoutDf$QSTRMVOLRT[nrow(rtoutDf)]-rtoutDf$QSTRMVOLRT[1]
   wbDf[1,10] <- rtoutDf$SFCHEADSUBRT[nrow(rtoutDf)]-rtoutDf$SFCHEADSUBRT[1]
   wbDf[1,11] <- -(rtoutDf$QBDRY[nrow(rtoutDf)]-rtoutDf$QBDRY[1])
   wbDf[1,12] <- with(wbDf, PRCP - ECAN - ETRAN - EDIR - UGDRNOFF -
                                 QSTRMVOL - QBDRY -
                                 DEL_SOILM - DEL_SWE - DEL_CANWAT - 
                                 DEL_SFCHEAD)
   wbDf[1,13] <- with(wbDf, (QSTRMVOL+QBDRY)/PRCP)
   wbDf[1,14] <- with(wbDf, (ECAN+ETRAN+EDIR)/PRCP)
   wbDf[1,15] <- with(wbDf, (DEL_SOILM+DEL_SWE+DEL_CANWAT+DEL_SFCHEAD)/PRCP)
   wbDf[1,16] <- with(wbDf, UGDRNOFF/PRCP)
   }
# ALL ROUTING CASE
else if (!is.null(rtoutDf) && !is.null(gwoutDf)) {
   dt <- as.integer(difftime(gwoutDf$POSIXct[2],gwoutDf$POSIXct[1],units="secs"))
   wbDf <- as.data.frame(t(as.matrix(rep(0, 16))))
   colnames(wbDf) <- c("PRCP","ECAN","ETRAN","EDIR",
                      "DEL_SOILM","DEL_SWE","DEL_CANWAT",
                      "QSTRMVOL","DEL_SFCHEAD","QBDRY","GWOUT","DEL_GWSTOR",
                      "ERROR","RUN_FRAC","EVAP_FRAC","STOR_FRAC")
   wbDf[1,1] <- ldasoutDf$ACCPRCP[nrow(ldasoutDf)]-ldasoutDf$ACCPRCP[1]
   wbDf[1,2] <- ldasoutDf$ACCECAN[nrow(ldasoutDf)]-ldasoutDf$ACCECAN[1]
   wbDf[1,3] <- ldasoutDf$ACCETRAN[nrow(ldasoutDf)]-ldasoutDf$ACCETRAN[1]
   wbDf[1,4] <- ldasoutDf$ACCEDIR[nrow(ldasoutDf)]-ldasoutDf$ACCEDIR[1]
   numsoil <- length(soildeps)
   soilm <- 0.0
   for (i in 1:numsoil) {
        soilm <- soilm + (ldasoutDf[nrow(ldasoutDf),paste0("SOIL_M",i)]-ldasoutDf[1,paste0("SOIL_M",i)])*soildeps[i]
        }
   wbDf[1,5] <- soilm
   wbDf[1,6] <- ldasoutDf$SNEQV[nrow(ldasoutDf)]-ldasoutDf$SNEQV[1]
   wbDf[1,7] <- (ldasoutDf$CANICE[nrow(ldasoutDf)] + ldasoutDf$CANLIQ[nrow(ldasoutDf)]) - 
               (ldasoutDf$CANICE[1] + ldasoutDf$CANLIQ[1])
   wbDf[1,8] <- rtoutDf$QSTRMVOLRT[nrow(rtoutDf)]-rtoutDf$QSTRMVOLRT[1]
   wbDf[1,9] <- rtoutDf$SFCHEADSUBRT[nrow(rtoutDf)]-rtoutDf$SFCHEADSUBRT[1]
   wbDf[1,10] <- -(rtoutDf$QBDRY[nrow(rtoutDf)]-rtoutDf$QBDRY[1])
   wbDf[1,11] <- sum(gwoutDf$q_cms/(basarea*1000*1000)*1000*dt, na.rm=T)
   wbDf[1,12] <- ldasoutDf$UGDRNOFF[nrow(ldasoutDf)] - wbDf$GWOUT
   wbDf[1,13] <- with(wbDf, PRCP - ECAN - ETRAN - EDIR -
                                 QSTRMVOL - QBDRY - GWOUT -
                                 DEL_SOILM - DEL_SWE - DEL_CANWAT - 
                                 DEL_SFCHEAD - DEL_GWSTOR)
   wbDf[1,14] <- with(wbDf,(QSTRMVOL+QBDRY+GWOUT)/PRCP)
   wbDf[1,15] <- with(wbDf,(ECAN+ETRAN+EDIR)/PRCP)
   wbDf[1,16] <- with(wbDf, (DEL_SOILM+DEL_SWE+DEL_CANWAT+DEL_SFCHEAD+DEL_GWSTOR)/PRCP)
   }

wbDf
}



#' Plot water balance from WRF-Hydro (w/NoahMP) output
#'
#' \code{PlotWatBudg} plot water budget components from WRF-Hydro (w/NoahMP) model output.
#'
#' Read water budget dataframe (as generated from \code{\link{CalcNoahmpWatBudg}}) and plot water budget
#' components as a piechart or barchart.
#' NOTE: Currently only works for runs using NoahMP as the LSM.
#'
#' @param wbDf The water budget dataframe (required)
#' @param plottyp The plot type (pie or bar) (default=pie)
#' @return A plot of the water budget components in mm.
#'
#' @examples
#' ## Plot the water budget components from a water budget dataframe generated using
#' ## CalcNoahmpWatBudg. Plot as a piechart.
#'
#' PlotWatBudg(wb.mod1.fc)
#' @export

PlotWatBudg <- function(wbDf, plottyp="pie") {
if ("GWOUT" %in% colnames(wbDf)) {
    # ALL ROUTING CASE
    if ("QSTRMVOL" %in% colnames(wbDf)) {
        lbls <- c("Canopy Evap", "Transpiration", "Surface Evap", "Surface &\nSubsurface Runoff",
                 "Deep GW Outflow")
        pcts <- with(wbDf,c(ECAN/PRCP*100, ETRAN/PRCP*100, EDIR/PRCP*100,
                      (QSTRMVOL+QBDRY)/PRCP*100, GWOUT/PRCP*100))
        lbls_pcts=c()
        for (i in 1:length(lbls)) { lbls_pcts[i] <- paste0(lbls[i], "\n", round(pcts[i],1), "%") }
        if (plottyp == "pie") {
            if (wbDf$STOR_FRAC > 0) {
                lbls_pcts[length(lbls_pcts)+1] <- paste0("Change in Storage", "\n",
                                                    round( with( wbDf, (DEL_SOILM + DEL_SWE + DEL_CANWAT +
                                                    DEL_SFCHEAD + DEL_GWSTOR) / PRCP*100),1), "%")
                pie(as.matrix(with(wbDf, c(ECAN, ETRAN, EDIR, QSTRMVOL+QBDRY, GWOUT,
                                    DEL_SOILM+DEL_SWE+DEL_CANWAT+DEL_SFCHEAD+DEL_GWSTOR))),
                    col=c("chartreuse3","darkgreen","darkgoldenrod2","cornflowerblue","darkblue","grey30"),
                    main=c("Water Budget"), labels=lbls_pcts)
                }
            else {
                pie(as.matrix(with(wbDf, c(ECAN, ETRAN, EDIR, QSTRMVOL+QBDRY, GWOUT))),
                    col=c("chartreuse3","darkgreen","darkgoldenrod2","cornflowerblue","darkblue"),
                    main=c("Water Budget"), labels=lbls_pcts)
                text(0,-1, paste0("*Storage Loss: ",
                            round( with( wbDf, (DEL_SOILM + DEL_SWE + DEL_CANWAT + DEL_SFCHEAD + DEL_GWSTOR) /
                                        PRCP*100),1),"%"))
                } #end storage fraction split
            }
        else if (plottyp =="bar") {
            lbls_pcts[length(lbls_pcts)+1] <- paste0("Change in Storage", "\n",
                                    round( with( wbDf, (DEL_SOILM + DEL_SWE + DEL_CANWAT +
                                    DEL_SFCHEAD + DEL_GWSTOR) / PRCP*100),1), "%")
            plotDf <- with(wbDf,c(DEL_SOILM + DEL_SWE + DEL_CANWAT + DEL_SFCHEAD + DEL_GWSTOR, ECAN, ETRAN, EDIR, QSTRMVOL+QBDRY, GWOUT))
            plotDf1 <- abs(plotDf)
            ylabs <- round(c(0,cumsum(plotDf1))-((plotDf1[1]-plotDf[1])/2),0)
            par(mar = c(5.1, 4.1, 5.1, 12.1), xpd = TRUE)
            barplot(as.matrix(plotDf1), axes=FALSE,
                col=c("grey70", "chartreuse", "darkgreen", "orange", "cornflowerblue", "darkblue"),
                main=c("Water Budget"), width=0.2, space=0.2, ylab=c("Total Water (mm)"))
            axis(2,c(0,cumsum(plotDf1)),labels=ylabs)
            segments(0.0,cumsum(plotDf1)[1],0.36,cumsum(plotDf1)[1], lty=2)
            legend("topright", legend=lbls_pcts,fill=c("chartreuse", "darkgreen", "orange", "cornflowerblue", "darkblue","grey70"),
                inset=c(-0.5, 0), bg=c("white"), yjust=0.5, y.intersp=2)
            }
        } #end qstrmvol split
    # GW ROUTING ONLY CASE
    else {
        lbls <- c("Canopy Evap", "Transpiration", "Surface Evap", "Surface Runoff",
                 "Deep GW Outflow")
        pcts <- with(wbDf,c(ECAN/PRCP*100, ETRAN/PRCP*100, EDIR/PRCP*100,
                SFCRNOFF/PRCP*100, GWOUT/PRCP*100))
        lbls_pcts <- c()
        for (i in 1:length(lbls)) { lbls_pcts[i] <- paste0(lbls[i], "\n", round(pcts[i],1), "%") }
        if (plottyp == "pie") {
            if (wbDf$STOR_FRAC > 0) {
                lbls_pcts[length(lbls_pcts)+1] <- paste0("Change in Storage", "\n",
                                                    round( with( wbDf, (DEL_SOILM + DEL_SWE + DEL_CANWAT + DEL_GWSTOR) /
                                                    PRCP*100),1), "%")
                pie(as.matrix(with(wbDf, c(ECAN, ETRAN, EDIR, SFCRNOFF, GWOUT,
                                    DEL_SOILM+DEL_SWE+DEL_CANWAT+DEL_GWSTOR))),
                    col=c("chartreuse3","darkgreen","darkgoldenrod2","cornflowerblue","darkblue","grey30"),
                    main=c("Water Budget"), labels=lbls_pcts)
                }
            else {
                pie(as.matrix(with(wbDf, c(ECAN, ETRAN, EDIR, SFCRNOFF, GWOUT))),
                    col=c("chartreuse3","darkgreen","darkgoldenrod2","cornflowerblue","darkblue"),
                    main=c("Water Budget"), labels=lbls_pcts)
                text(0,-1, paste0("*Storage Loss: ",
                            round( with( wbDf, (DEL_SOILM + DEL_SWE + DEL_CANWAT + DEL_GWSTOR) /
                                        PRCP*100),1),"%"))
                } #end storage fraction split
            }
        else if (plottyp =="bar") {
            lbls_pcts[length(lbls_pcts)+1] <- paste0("Change in Storage", "\n",
                                    round( with( wbDf, (DEL_SOILM + DEL_SWE + DEL_CANWAT +
                                    DEL_GWSTOR) / PRCP*100),1), "%")
            plotDf <- with(wbDf,c(DEL_SOILM + DEL_SWE + DEL_CANWAT + DEL_GWSTOR, ECAN, ETRAN, EDIR, SFCRNOFF, GWOUT))
            plotDf1 <- abs(plotDf)
            ylabs <- round(c(0,cumsum(plotDf1))-((plotDf1[1]-plotDf[1])/2),0)
            par(mar = c(5.1, 4.1, 5.1, 12.1), xpd = TRUE)
            barplot(as.matrix(plotDf1), axes=FALSE,
                col=c("grey70", "chartreuse", "darkgreen", "orange", "cornflowerblue", "darkblue"),
                main=c("Water Budget"), width=0.2, space=0.2, ylab=c("Total Water (mm)"))
            axis(2,c(0,cumsum(plotDf1)),labels=ylabs)
            segments(0.0,cumsum(plotDf1)[1],0.36,cumsum(plotDf1)[1], lty=2)
            legend("topright", legend=lbls_pcts,fill=c("chartreuse", "darkgreen", "orange", "cornflowerblue", "darkblue", "grey70"),
                inset=c(-0.5, 0), bg=c("white"), yjust=0.5, y.intersp=2)
            }
        } #end qstrmvol split
    } #end gwout split

# OVERLAND/SUBSURFACE ROUTING ONLY CASE
else if ("QSTRMVOL" %in% colnames(wbDf)) {
    lbls <- c("Canopy Evap", "Transpiration", "Surface Evap", "Surface & Subsurface Runoff",
             "Deep GW Recharge")
    pcts <- with(wbDf,c(ECAN/PRCP*100, ETRAN/PRCP*100, EDIR/PRCP*100,
                  (QSTRMVOL+QBDRY)/PRCP*100, UGDRNOFF/PRCP*100))
    lbls_pcts=c()
    for (i in 1:length(lbls)) { lbls_pcts[i] <- paste0(lbls[i], "\n", round(pcts[i],1), "%") }
    if (plottyp == "pie") {
        if (wbDf$STOR_FRAC > 0) {
            lbls_pcts[length(lbls_pcts)+1] <- paste0("Change in Storage", "\n",
                                                round( with( wbDf, (DEL_SOILM + DEL_SWE + DEL_CANWAT +
                                                DEL_SFCHEAD) / PRCP*100),1), "%")
            pie(as.matrix(with(wbDf, c(ECAN, ETRAN, EDIR, QSTRMVOL+QBDRY, UGDRNOFF,
                                DEL_SOILM+DEL_SWE+DEL_CANWAT+DEL_SFCHEAD))),
                col=c("chartreuse3","darkgreen","darkgoldenrod2","cornflowerblue","darkblue","grey30"),
                main=c("Water Budget"), labels=lbls_pcts)
            }
        else {
            pie(as.matrix(with(wbDf, c(ECAN, ETRAN, EDIR, QSTRMVOL+QBDRY, UGDRNOFF))),
                col=c("chartreuse3","darkgreen","darkgoldenrod2","cornflowerblue","darkblue"),
                main=c("Water Budget"), labels=lbls_pcts)
            text(0,-1, paste0("*Storage Loss: ",
                        round( with( wbDf, (DEL_SOILM + DEL_SWE + DEL_CANWAT + DEL_SFCHEAD) /
                                    PRCP*100),1),"%"))
            } #end storage fraction split
        }
    else if (plottyp =="bar") {
        lbls_pcts[length(lbls_pcts)+1] <- paste0("Change in Storage", "\n",
                                round( with( wbDf, (DEL_SOILM + DEL_SWE + DEL_CANWAT +
                                DEL_SFCHEAD) / PRCP*100),1), "%")
        plotDf <- with(wbDf,c(DEL_SOILM + DEL_SWE + DEL_CANWAT + DEL_SFCHEAD, ECAN, ETRAN, EDIR, QSTRMVOL+QBDRY, UGDRNOFF))
        plotDf1 <- abs(plotDf)
        ylabs <- round(c(0,cumsum(plotDf1))-((plotDf1[1]-plotDf[1])/2),0)
        par(mar = c(5.1, 4.1, 5.1, 12.1), xpd = TRUE)
        barplot(as.matrix(plotDf1), axes=FALSE,
            col=c("grey70", "chartreuse", "darkgreen", "orange", "cornflowerblue", "darkblue"),
            main=c("Water Budget"), width=0.2, space=0.2, ylab=c("Total Water (mm)"))
        axis(2,c(0,cumsum(plotDf1)),labels=ylabs)
        segments(0.0,cumsum(plotDf1)[1],0.36,cumsum(plotDf1)[1], lty=2)
        legend("topright", legend=lbls_pcts,fill=c("chartreuse", "darkgreen", "orange", "cornflowerblue", "darkblue", "grey70"),
            inset=c(-0.5, 0), bg=c("white"), yjust=0.5, y.intersp=2)
        }
    } #end qstrmvol split
# NO ROUTING CASE
else {
    lbls <- c("Canopy Evap", "Transpiration", "Surface Evap", "Surface Runoff",
            "Subsurface Runoff")
    pcts <- with(wbDf,c(ECAN/PRCP*100, ETRAN/PRCP*100, EDIR/PRCP*100,
            SFCRNOFF/PRCP*100, UGDRNOFF/PRCP*100))
    lbls_pcts=c()
    for (i in 1:length(lbls)) { lbls_pcts[i] <- paste0(lbls[i], "\n", round(pcts[i],1), "%") }
    if (plottyp == "pie") {
        if (wbDf$STOR_FRAC > 0) {
            lbls_pcts[length(lbls_pcts)+1] <- paste0("Change in Storage", "\n",
                                                round( with( wbDf, (DEL_SOILM + DEL_SWE + DEL_CANWAT) /
                                                PRCP*100),1), "%")
            pie(as.matrix(with(wbDf,c(ECAN, ETRAN, EDIR, SFCRNOFF, UGDRNOFF,
                                DEL_SOILM+DEL_SWE+DEL_CANWAT))),
                col=c("chartreuse3","darkgreen","darkgoldenrod2","cornflowerblue","darkblue","grey30"),
                main=c("Water Budget"), labels=lbls_pcts)
            }
        else {
            pie(as.matrix(with(wbDf,c(ECAN, ETRAN, EDIR, SFCRNOFF, UGDRNOFF))),
                col=c("chartreuse3","darkgreen","darkgoldenrod2","cornflowerblue","darkblue"),
                main=c("Water Budget"), labels=lbls_pcts)
            text(0,-1, paste0("*Storage Loss: ",
                        round( with( wbDf, (DEL_SOILM + DEL_SWE + DEL_CANWAT) /
                                    PRCP*100),1),"%"))
            } #end storage fraction split
        }
    else if (plottyp =="bar") {
        lbls_pcts[length(lbls_pcts)+1] <- paste0("Change in Storage", "\n",
                                round( with( wbDf, (DEL_SOILM + DEL_SWE + DEL_CANWAT) / PRCP*100),1), "%")
        plotDf <- with(wbDf,c(DEL_SOILM + DEL_SWE + DEL_CANWAT, ECAN, ETRAN, EDIR, SFCRNOFF, UGDRNOFF))
        plotDf1 <- abs(plotDf)
        ylabs <- round(c(0,cumsum(plotDf1))-((plotDf1[1]-plotDf[1])/2),0)
        par(mar = c(5.1, 4.1, 5.1, 12.1), xpd = TRUE)
        barplot(as.matrix(plotDf1), axes=FALSE,
            col=c("grey70", "chartreuse", "darkgreen", "orange", "cornflowerblue", "darkblue"),
            main=c("Water Budget"), width=0.2, space=0.2, ylab=c("Total Water (mm)"))
        axis(2,c(0,cumsum(plotDf1)),labels=ylabs)
        segments(0.0,cumsum(plotDf1)[1],0.36,cumsum(plotDf1)[1], lty=2)
        legend("topright", legend=lbls_pcts,fill=c("chartreuse", "darkgreen", "orange", "cornflowerblue", "darkblue", "grey70"),
            inset=c(-0.5, 0), bg=c("white"), yjust=0.5, y.intersp=2)
        }
    } #end main split
} #end function def



#' Computes model performance statistics for WRF-Hydro streamflow output
#'
#' \code{CalcStrPerf} calculates model performance statistics for streamflow output.
#'
#' \code{CalcStrPerf} reads a model forecast point streamflow timeseries (i.e., created using \code{\link{ReadFrxstPts}}) and
#' a streamflow observation timeseries (i.e., created using \code{\link{ReadUsgsGage}}) and calculates model performance
#' statistics (Nash-Sutcliffe Efficiency, Rmse, etc.) at various time scales and for low
#' and high flows. Assumes model output and observation datasets have the same time step
#' (e.g., both hourly).
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
#' \item errcom: error in the center-of-mass of streamflow, where center-of-mass is the
#' hour/day when 50\% of daily/monthly/water-year streamflow has occurred. Reported as number of hours for
#' daily time scale and number of days for monthly and yearly time scales.
#' \item errmaxt: Error in the time of maximum streamflow. Reported as number of hours for daily time scale
#' and number of days for monthly and yearly time scales).
#' \item errfdc: Error in the integrated flow duration curve between 0.05 and 0.95 exceedance thresholds
#' (in native flow units).
#' }
#'
#' time scales/Flow Types:
#' \itemize{
#' \item ts = native model/observation time step (e.g., hourly)
#' \item daily = daily time step
#' \item monthly = monthly time step
#' \item yearly = water-year time step
#' \item max10 = high flows; restricted to the portion of the time series where the observed streamflow is in the highest 10%
#' \item min10 = low flows; restricted to the portion of the time series where the observed streamflow is in the lowest 10%
#' }
#'
#' @param stroutDf The forecast point output dataframe (required). Assumes only one forecast
#' point per file, so if you have multiple forecast points in your output dataframe, use
#' subset to isolate a single forecast point's data. Also assumes model output time step and
#' observation time step match, and both contain POSIXct fields (called "POSIXct").
#' @param obsDf The observed streamflow dataframe. Assumes only one gage per file, so if
#' you have multiple forecast points in your output dataframe, use subset to isolate
#' a single gage's data. Also assumes model output time step and observation time step match,
#' and both contain POSIXct fields (called "POSIXct").
#' @param strCol The column name for the streamflow time series for the MODEL data (default="q_cms")
#' @param obsCol The column name for the streamflow time series for the OBSERVED data (default="q_cms")
#' @return A new dataframe containing the model performance statistics.
#'
#' @examples
#' ## Take forecast point model output for Fourmile Creek (modStrh.mod1.fc) and a corresponding
#' ## USGS gage observation file (obsStrh.fc), both at an hourly time step, and calculate
#' ## model performance statistics. The model forecast point data was imported using ReadFrxstPts
#' ## and the gage observation data was imported using ReadUsgsGage.
#'
#' CalcStrPerf(modStrh.mod1.fc, obsStrh.fc)
#'
#' > Output:
#'           nse nselog  cor rmse rmsenorm  bias  mae errcom errmaxt errfdc
#' ts       0.57   0.61 0.79 1.43     9.48 -28.0 0.70     NA      NA  -0.42
#' daily    0.71   0.64 0.87 1.17     9.86 -28.1 0.61   0.19   -2.25  -0.37
#' monthly  0.80   0.70 0.93 0.89    12.73 -26.6 0.53  -0.18   -0.96     NA
#' yearly   0.05   0.37 0.36 0.55    41.50  -6.5 0.45  -1.50   -3.38     NA
#' max10   -7.50 -15.94 0.19 3.82    38.89 -24.5 0.04     NA      NA     NA
#' min10   -2.84  -1.83 0.10 0.05    33.36 -23.7   NA     NA      NA     NA
#' @export

CalcStrPerf <- function (stroutDf, obsDf, strCol="q_cms", obsCol="q_cms") {
    # START UTILS
    CalcMeanNarm <- function(myvar) {
        mean(myvar, na.rm=TRUE)
        }
    CalcWyd <- function (x) {
        tmp <- aggregate(x$yd, by = list(x$year), max)
        colnames(tmp) <- c("year", "n")
        tmp$year <- as.integer(as.character(tmp$year))
        x$wyd <- 0
        tmp2 <- subset(tmp, tmp$n == 365)
        new <- ifelse((x$year %in% tmp2$year), ifelse(x$yd >= 274,
            x$yd - 273, x$yd + 92), ifelse(x$yd >= 275, x$yd - 274, 
            x$yd + 92))
        new
    }
    CalcDates <- function (x) {
        x$day <- as.integer(format(x$POSIXct,"%d"))
        x$month <- as.integer(format(x$POSIXct,"%m"))
        x$year <- as.integer(format(x$POSIXct,"%Y"))
        x$wy <- ifelse(x$month >= 10, x$year + 1, x$year)
        x$yd <- as.integer(format(x$POSIXct,"%j"))
        x$wyd <- CalcWyd(x)
        x
    }
    Nse <- function (m, o) {
        err1 <- sum((m - o)^2, na.rm=T)
        err2 <- sum((o - mean(o, na.rm=T))^2, na.rm=T)
        ns <- 1 - (err1/err2)
        ns
    }
    NseLog <- function (m, o) {
        m <- log(m + 1e-04)
        o <- log(o + 1e-04)
        err1 <- sum((m - o)^2, na.rm=T)
        err2 <- sum((o - mean(o, na.rm=T))^2, na.rm=T)
        ns <- 1 - (err1/err2)
        ns
    }
    Rmse <- function (m, o) {
        err <- sum((m - o)^2, na.rm=T)/(min(sum(!is.na(m)),sum(!is.na(o))))
        rmserr <- sqrt(err)
        rmserr
    }
    RmseNorm <- function (m, o) {
        err <- sum((m - o)^2, na.rm=T)/(min(sum(!is.na(m)),sum(!is.na(o))))
        rmserr <- sqrt(err) / ( max(o, na.rm=T) - min(o, na.rm=T) ) * 100
        rmserr
    }
    CumsumNa <- function(x) {
        x[which(is.na(x))] <- 0
        return(cumsum(x))
    } 
    CalcCOM <- function (str) {
        cuml.str <- as.data.frame(CumsumNa(str)/sum(str, na.rm=T))
        colnames(cuml.str) <- c("x")
        cuml.str$ts <- seq(from = 1, to = length(cuml.str$x))
        tmp2 <- subset(cuml.str, cuml.str$x > 0.5)
        ts <- tmp2$ts[1]
        ts
    }
    # END UTILS

    # Prepare data
    stroutDf <- CalcDates(stroutDf)
    stroutDf$qcomp <- stroutDf[,strCol]
    obsDf$qcomp <- obsDf[,obsCol]
    if (as.integer(obsDf$POSIXct[2])-as.integer(obsDf$POSIXct[1]) >= 86400) {obsDf$POSIXct=as.POSIXct(round(obsDf$POSIXct,"days"), tz="UTC")}
    stroutDf <- merge(stroutDf, obsDf[c("POSIXct","qcomp")], by<-c("POSIXct"), suffixes=c(".mod",".obs"))
    stroutDf <- subset(stroutDf, !is.na(stroutDf$qcomp.mod) & !is.na(stroutDf$qcomp.obs))
    stroutDf$date <- as.POSIXct(round(stroutDf$POSIXct, "days"))
    results <- as.data.frame(matrix(nrow = 6, ncol = 10))
    colnames(results) = c("nse", "nselog", "cor", "rmse", "rmsenorm", "bias", "mae", "errcom", "errmaxt", "errfdc")
    rownames(results) = c("ts", "daily", "monthly", "yearly", "max10", "min10")
    exclvars <- names(stroutDf) %in% c("POSIXct", "secs", "timest", "date")
    stroutDf.d <- aggregate(stroutDf[!exclvars], by = list(stroutDf$date), CalcMeanNarm)
    stroutDf.mwy <- aggregate(stroutDf[c("qcomp.mod","qcomp.obs")], by = list(stroutDf$month, stroutDf$wy), CalcMeanNarm)
    stroutDf.wy <- aggregate(stroutDf[c("qcomp.mod","qcomp.obs")], by = list(stroutDf$wy), CalcMeanNarm)
    stroutDf.dcom <- aggregate(stroutDf[c("qcomp.mod","qcomp.obs")], by = list(stroutDf$date), CalcCOM)
    stroutDf.mwycom <- aggregate(stroutDf.d[c("qcomp.mod","qcomp.obs")], by = list(stroutDf.d$month, stroutDf.d$wy), CalcCOM)
    stroutDf.wycom <- aggregate(stroutDf.d[c("qcomp.mod","qcomp.obs")], by = list(stroutDf.d$wy), CalcCOM)
    stroutDf.dmax <- aggregate(stroutDf[c("qcomp.mod","qcomp.obs")], by = list(stroutDf$date), which.max)
    stroutDf.mwymax <- aggregate(stroutDf.d[c("qcomp.mod","qcomp.obs")], by = list(stroutDf.d$month, stroutDf.d$wy), which.max)
    stroutDf.wymax <- aggregate(stroutDf.d[c("qcomp.mod","qcomp.obs")], by = list(stroutDf.d$wy), which.max)
    stroutDf.max10 <- subset(stroutDf, stroutDf$qcomp.obs>=quantile(stroutDf$qcomp.obs, 0.90, na.rm=TRUE))
    stroutDf.min10 <- subset(stroutDf, stroutDf$qcomp.obs<=quantile(stroutDf$qcomp.obs, 0.10, na.rm=TRUE))
    stroutDf <- CalcFdc(stroutDf, "qcomp.mod")
    stroutDf <- CalcFdc(stroutDf, "qcomp.obs")
    stroutDf.d <- CalcFdc(stroutDf.d, "qcomp.mod")
    stroutDf.d <- CalcFdc(stroutDf.d, "qcomp.obs")

    # Compile summary statistics
    results["ts", "nse"] <- round(Nse(stroutDf$qcomp.mod, stroutDf$qcomp.obs), 2)
    results["ts", "nselog"] <- round(NseLog(stroutDf$qcomp.mod, stroutDf$qcomp.obs), 2)
    results["ts", "cor"] <- round(cor(stroutDf$qcomp.mod, stroutDf$qcomp.obs, use="na.or.complete"), 2)
    results["ts", "rmse"] <- round(Rmse(stroutDf$qcomp.mod, stroutDf$qcomp.obs), 2)
    results["ts", "rmsenorm"] <- round(RmseNorm(stroutDf$qcomp.mod, stroutDf$qcomp.obs), 2)
    results["ts", "bias"] <- round(sum(stroutDf$qcomp.mod-stroutDf$qcomp.obs, na.rm=T)/sum(stroutDf$qcomp.obs, na.rm=TRUE) * 100, 1)
    results["ts", "mae"] <- round(mean(abs(stroutDf$qcomp.mod-stroutDf$qcomp.obs), na.rm=T), 2)
    results["ts", "errcom"] <- NA
    results["ts", "errmaxt"] <- NA
    results["ts", "errfdc"] <- round(integrate(splinefun(stroutDf[,"qcomp.mod.fdc"], stroutDf[,"qcomp.mod"], method='natural'), 0.05, 0.95, subdivisions=1000)$value -
                                integrate(splinefun(stroutDf[,"qcomp.obs.fdc"], stroutDf[,"qcomp.obs"], method='natural'), 0.05, 0.95, subdivisions=1000)$value, 2 )
    results["daily", "nse"] <- round(Nse(stroutDf.d$qcomp.mod, stroutDf.d$qcomp.obs), 2)
    results["daily", "nselog"] <- round(NseLog(stroutDf.d$qcomp.mod, stroutDf.d$qcomp.obs), 2)
    results["daily", "cor"] <- round(cor(stroutDf.d$qcomp.mod, stroutDf.d$qcomp.obs, use="na.or.complete"), 2)
    results["daily", "rmse"] <- round(Rmse(stroutDf.d$qcomp.mod, stroutDf.d$qcomp.obs), 2)
    results["daily", "rmsenorm"] <- round(RmseNorm(stroutDf.d$qcomp.mod, stroutDf.d$qcomp.obs), 2)
    results["daily", "bias"] <- round(sum(stroutDf.d$qcomp.mod-stroutDf.d$qcomp.obs, na.rm=T)/sum(stroutDf.d$qcomp.obs, na.rm=TRUE) * 100, 1)
    results["daily", "mae"] <- round(mean(abs(stroutDf.d$qcomp.mod-stroutDf.d$qcomp.obs), na.rm=T), 2)
    results["daily", "errcom"] <- round(mean(stroutDf.dcom$qcomp.mod-stroutDf.dcom$qcomp.obs, na.rm=T), 2)
    results["daily", "errmaxt"] <- round(mean(stroutDf.dmax$qcomp.mod-stroutDf.dmax$qcomp.obs, na.rm=T), 2)
    results["daily", "errfdc"] <- round(integrate(splinefun(stroutDf.d[,"qcomp.mod.fdc"], stroutDf.d[,"qcomp.mod"], method='natural'), 0.05, 0.95, subdivisions=1000)$value -
                                    integrate(splinefun(stroutDf.d[,"qcomp.obs.fdc"], stroutDf.d[,"qcomp.obs"], method='natural'), 0.05, 0.95, subdivisions=1000)$value, 2 )
    results["monthly", "nse"] <- round(Nse(stroutDf.mwy$qcomp.mod, stroutDf.mwy$qcomp.obs), 2)
    results["monthly", "nselog"] <- round(NseLog(stroutDf.mwy$qcomp.mod, stroutDf.mwy$qcomp.obs), 2)
    results["monthly", "cor"] <- round(cor(stroutDf.mwy$qcomp.mod, stroutDf.mwy$qcomp.obs, use="na.or.complete"), 2)
    results["monthly", "rmse"] <- round(Rmse(stroutDf.mwy$qcomp.mod, stroutDf.mwy$qcomp.obs), 2)
    results["monthly", "rmsenorm"] <- round(RmseNorm(stroutDf.mwy$qcomp.mod, stroutDf.mwy$qcomp.obs), 2)
    results["monthly", "bias"] <- round(sum(stroutDf.mwy$qcomp.mod-stroutDf.mwy$qcomp.obs, na.rm=T)/sum(stroutDf.mwy$qcomp.obs, na.rm=TRUE) * 100, 1)
    results["monthly", "mae"] <- round(mean(abs(stroutDf.mwy$qcomp.mod-stroutDf.mwy$qcomp.obs), na.rm=T), 2)
    results["monthly", "errcom"] <- round(mean(stroutDf.mwycom$qcomp.mod-stroutDf.mwycom$qcomp.obs, na.rm=T), 2)
    results["monthly", "errmaxt"] <- round(mean(stroutDf.mwymax$qcomp.mod-stroutDf.mwymax$qcomp.obs, na.rm=T), 2)
    results["monthly", "errfdc"] <- NA
    results["yearly", "nse"] <- round(Nse(stroutDf.wy$qcomp.mod, stroutDf.wy$qcomp.obs), 2)
    results["yearly", "nselog"] <- round(NseLog(stroutDf.wy$qcomp.mod, stroutDf.wy$qcomp.obs), 2)
    results["yearly", "cor"] <- round(cor(stroutDf.wy$qcomp.mod, stroutDf.wy$qcomp.obs, use="na.or.complete"), 2)
    results["yearly", "rmse"] <- round(Rmse(stroutDf.wy$qcomp.mod, stroutDf.wy$qcomp.obs), 2)
    results["yearly", "rmsenorm"] <- round(RmseNorm(stroutDf.wy$qcomp.mod, stroutDf.wy$qcomp.obs), 2)
    results["yearly", "bias"] <- round(sum(stroutDf.wy$qcomp.mod-stroutDf.wy$qcomp.obs, na.rm=T)/sum(stroutDf.wy$qcomp.obs, na.rm=TRUE) * 100, 1)
    results["yearly", "mae"] <- round(mean(abs(stroutDf.wy$qcomp.mod-stroutDf.wy$qcomp.obs), na.rm=T), 2)
    results["yearly", "errcom"] <- round(mean(stroutDf.wycom$qcomp.mod-stroutDf.wycom$qcomp.obs, na.rm=T), 2)
    results["yearly", "errmaxt"] <- round(mean(stroutDf.wymax$qcomp.mod-stroutDf.wymax$qcomp.obs, na.rm=T), 2)
    results["yearly", "errfdc"] <- NA
    results["max10", "nse"] <- round(Nse(stroutDf.max10$qcomp.mod, stroutDf.max10$qcomp.obs), 2)
    results["max10", "nselog"] <- round(NseLog(stroutDf.max10$qcomp.mod, stroutDf.max10$qcomp.obs), 2)
    results["max10", "cor"] <- round(cor(stroutDf.max10$qcomp.mod, stroutDf.max10$qcomp.obs, use="na.or.complete"), 2)
    results["max10", "rmse"] <- round(Rmse(stroutDf.max10$qcomp.mod, stroutDf.max10$qcomp.obs), 2)
    results["max10", "rmsenorm"] <- round(RmseNorm(stroutDf.max10$qcomp.mod, stroutDf.max10$qcomp.obs), 2)
    results["max10", "bias"] <- round(sum(stroutDf.max10$qcomp.mod-stroutDf.max10$qcomp.obs, na.rm=T)/sum(stroutDf.max10$qcomp.obs, na.rm=TRUE) * 100, 1)
    results["max10", "mae"] <- round(mean(abs(stroutDf.max10$qcomp.mod-stroutDf.max10$qcomp.obs), na.rm=T), 2)
    results["max10", "errcom"] <- NA
    results["max10", "errmaxt"] <- NA
    results["max10", "errfdc"] <- NA
    results["min10", "nse"] <- round(Nse(stroutDf.min10$qcomp.mod, stroutDf.min10$qcomp.obs), 2)
    results["min10", "nselog"] <- round(NseLog(stroutDf.min10$qcomp.mod, stroutDf.min10$qcomp.obs), 2)
    results["min10", "cor"] <- round(cor(stroutDf.min10$qcomp.mod, stroutDf.min10$qcomp.obs, use="na.or.complete"), 2)
    results["min10", "rmse"] <- round(Rmse(stroutDf.min10$qcomp.mod, stroutDf.min10$qcomp.obs), 2)
    results["min10", "rmsenorm"] <- round(RmseNorm(stroutDf.min10$qcomp.mod, stroutDf.min10$qcomp.obs), 2)
    results["min10", "bias"] <- round(sum(stroutDf.min10$qcomp.mod-stroutDf.min10$qcomp.obs, na.rm=T)/sum(stroutDf.min10$qcomp.obs, na.rm=TRUE) * 100, 1)
    results["max10", "mae"] <- round(mean(abs(stroutDf.min10$qcomp.mod-stroutDf.min10$qcomp.obs), na.rm=T), 2)
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
#' statistics at various exceedance thresholds (e.g., 10\%, 20\%, etc.).
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
#' @param stroutDf The forecast point output dataframe (required). Assumes only one forecast
#' point per file, so if you have multiple forecast points in your output dataframe, use
#' subset to isolate a single forecast point's data. Also assumes model output time step and
#' observation time step match, and both contain POSIXct fields (called "POSIXct").
#' @param obsDf The observed streamflow dataframe. Assumes only one gage per file, so if
#' you have multiple forecast points in your output dataframe, use subset to isolate
#' a single gage's data. Also assumes model output time step and observation time step match,
#' and both contain POSIXct fields (called "POSIXct").
#' @param strCol The column name for the streamflow time series for the MODEL data (default="q_cms")
#' @param obsCol The column name for the streamflow time series for the OBSERVED data (default="q_cms")
#' @return A new dataframe containing the flow duration curve statistics.
#'
#' @examples
#' ## Take forecast point model output for Fourmile Creek (modStrh.mod1.fc) and a corresponding
#' ## USGS gage observation file (obsStrh.fc), both at an hourly time step, and calculate
#' ## flow duration curve statistics. The model forecast point data was imported using ReadFrxstPts
#' ## and the gage observation data was imported using ReadUsgsGage.
#'
#' CalcFdcPerf(modStrh.mod1.fc, obsStrh.fc)
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


CalcFdcPerf <- function (stroutDf, obsDf, strCol="q_cms", obsCol="q_cms") {
    # START UTILS
    CalcWyd <- function (x) {
        tmp <- aggregate(x$yd, by = list(x$year), max)
        colnames(tmp) <- c("year", "n")
        tmp$year <- as.integer(as.character(tmp$year))
        x$wyd <- 0
        tmp2 <- subset(tmp, tmp$n == 365)
        new <- ifelse((x$year %in% tmp2$year), ifelse(x$yd >= 274,
            x$yd - 273, x$yd + 92), ifelse(x$yd >= 275, x$yd - 274, 
            x$yd + 92))
        new
    }
    CalcDates <- function (x) {
        x$day <- as.integer(format(x$POSIXct,"%d"))
        x$month <- as.integer(format(x$POSIXct,"%m"))
        x$year <- as.integer(format(x$POSIXct,"%Y"))
        x$wy <- ifelse(x$month >= 10, x$year + 1, x$year)
        x$yd <- as.integer(format(x$POSIXct,"%j"))
        x$wyd <- CalcWyd(x)
        x
    }
    # END UTILS

    # Prepare data
    stroutDf <- CalcDates(stroutDf)
    stroutDf$qcomp <- stroutDf[,strCol]
    obsDf$qcomp <- obsDf[,obsCol]
    if (as.integer(obsDf$POSIXct[2])-as.integer(obsDf$POSIXct[1]) >= 86400) {obsDf$POSIXct=as.POSIXct(round(obsDf$POSIXct,"days"), tz="UTC")}
    stroutDf <- merge(stroutDf, obsDf[c("POSIXct","qcomp")], by<-c("POSIXct"), suffixes=c(".mod",".obs"))
    stroutDf <- subset(stroutDf, !is.na(stroutDf$qcomp.mod) & !is.na(stroutDf$qcomp.obs))
    stroutDf <- CalcFdc(stroutDf, "qcomp.mod")
    stroutDf <- CalcFdc(stroutDf, "qcomp.obs")
    fdc.mod <- splinefun(stroutDf[,"qcomp.mod.fdc"], stroutDf[,"qcomp.mod"], method='natural')
    fdc.obs <- splinefun(stroutDf[,"qcomp.obs.fdc"], stroutDf[,"qcomp.obs"], method='natural')

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
