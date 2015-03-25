
#' Plot time series comparing modeled and observed fluxes
#'
#' \code{PlotFluxCompare} plots a time series of an observed flux (e.g., streamflow, ET)
#' and up to 2 modelled fluxes.
#'
#' \code{PlotFluxCompare} reads modelled and observed dataframes (e.g., as generated
#' from \code{\link{ReadFrxstPts}} and \code{\link{ReadUsgsGage}}) and plot the time series
#' and summary statistics. The tool will subset data to matching time periods (e.g., if the
#' observed data is at 5-min increments and modelled data is at 1-hr increments, the tool
#' will subset the observed data to select only observations on the matching hour break).
#'
#' @param strDf.obs The OBSERVED flux time series dataframe (e.g., output from \code{\link{ReadUsgsGage}}).
#' The dataframe must contain a column of flux values and a POSIXct column.
#' @param strCol.obs The name of the column containing the flux values for the OBSERVED
#' dataframe (DEFAULT="q_cms").
#' @param strDf.mod1 The FIRST MODEL flux time series dataframe (e.g., output from \code{\link{ReadFrxstPts}}).
#' The dataframe must contain a column of flux values and a POSIXct column.
#' @param strCol.mod1 The name of the column containing the FIRST MODEL flux values
#' (DEFAULT="q_cms").
#' @param strDf.mod2 The SECOND MODEL flux time series dataframe (e.g., output from \code{\link{ReadFrxstPts}}).
#' The dataframe must contain a column of flux values and a POSIXct column.
#' @param strCol.mod2 The name of the column containing the SECOND MODEL flux values
#' (DEFAULT="q_cms").
#' @param stdate Start date for plot/statistics (DEFAULT=NULL, all records will be used).
#' Date MUST be specified in POSIXct format with appropriate timezone
#' (e.g., as.POSIXct("2013-05-01 00:00:00", format="\%Y-\%m-\%d \%H:\%M:\%S", tz="UTC"))
#' @param enddate End date for plot/statistics (DEFAULT=NULL, all records will be used).
#' Date MUST be specified in POSIXct format with appropriate timezone
#' (e.g., as.POSIXct("2013-05-01 00:00:00", format="\%Y-\%m-\%d \%H:\%M:\%S", tz="UTC"))
#' @param logy (TRUE or FALSE) Optional flag to set the y-axis to log-scale (DEFAULT=FALSE).
#' @param labelObs Optional label for the observed streamflow (DEFAULT="Observed")
#' @param labelMod1 Optional label for the FIRST MODEL (DEFAULT="Model 1")
#' @param labelMod2 Optional label for the SECOND MODEL (DEFAULT="Model 2")
#' @param title Optional for the plot (DEFAULT="Observed and Modelled Fluxes")
#' @return A plot of the hydrographs.
#'
#' @examples
#' ## Take a time series of observed 5-minute streamflow values for Fourmile Creek (obsStr5min.fc) and two model
#' ## runs (mod1Str1h.fc, mod2Str1h.fc), all with streamflow columns named "q_cms", and plot the
#' ## the hydrographs for all three over the May-June snowmelt period.
#'
#' PlotFluxCompare(obsStr5min.fc, "q_cms", modStrh.chrt.fc, "q_cms", strDf.mod2=modStrh.allrt.fc, strCol.mod2="q_cms", labelObs="Observed Fourmile Creek at Orodell", labelMod1="Channel Routing Only", labelMod2="All Routing", title="Streamflow: Fourmile Creek", stdate=as.POSIXct("2013-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC"), enddate=as.POSIXct("2013-06-30 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC"))
#' @export

PlotFluxCompare <- function(strDf.obs, strCol.obs="q_cms",
                            strDf.mod1, strCol.mod1="q_cms",
                            strDf.mod2=NULL, strCol.mod2="q_cms",
                            stdate=NULL, enddate=NULL, logy=FALSE,
                            labelObs="Observed", labelMod1="Model 1", labelMod2="Model 2",
                            title="Observed and Modelled Fluxes") {
    # PREP DATA
    if (!is.null(stdate) && !is.null(enddate)) {
        strDf.obs <- subset(strDf.obs, POSIXct>=stdate & POSIXct<=enddate)
        strDf.mod1 <- subset(strDf.mod1, POSIXct>=stdate & POSIXct<=enddate)
        if (!is.null(strDf.mod2)) {
            strDf.mod2 <- subset(strDf.mod2, POSIXct>=stdate & POSIXct<=enddate)
            }
        ttext <- paste0(title, " (", stdate, " to ", enddate, ")")
        }
    else {
        ttext <- title
        }
    strDf.obs$qcomp.obs <- strDf.obs[,strCol.obs]
    strDf.mod1$qcomp.mod1 <- strDf.mod1[,strCol.mod1]
    if (!is.null(strDf.mod2)) {
        strDf.mod2$qcomp.mod2 <- strDf.mod2[,strCol.mod2]
    }
    strDf <- merge(strDf.obs[c("POSIXct","qcomp.obs")], strDf.mod1[c("POSIXct","qcomp.mod1")], by<-c("POSIXct"))
    if (!is.null(strDf.mod2)) {
        strDf <- merge(strDf, strDf.mod2[c("POSIXct","qcomp.mod2")], by<-c("POSIXct"))
        }
    # STATS
    nseflow1 <- round(Nse(strDf$qcomp.mod1, strDf$qcomp.obs), 2)
    biasflow1 <- round(sum(strDf$qcomp.mod1-strDf$qcomp.obs, na.rm=TRUE)/sum(strDf$qcomp.obs, na.rm=TRUE) * 100, 1)
    maxflow <- max(max(strDf$qcomp.obs, na.rm=TRUE), max(strDf$qcomp.mod1, na.rm=TRUE))
    minflow <- min(min(strDf$qcomp.obs, na.rm=TRUE), min(strDf$qcomp.mod1, na.rm=TRUE))
    if (!is.null(strDf.mod2)) {
        maxflow <- max(maxflow, max(strDf$qcomp.mod2, na.rm=TRUE))
        minflow <- min(minflow, min(strDf$qcomp.mod2, na.rm=TRUE))
        nseflow2 <- round(Nse(strDf$qcomp.mod2, strDf$qcomp.obs), 2)
        biasflow2 <- round(sum(strDf$qcomp.mod2-strDf$qcomp.obs, na.rm=TRUE)/sum(strDf$qcomp.obs, na.rm=TRUE) * 100, 1)
        }
    # PLOT
    if (logy) {
        with(strDf, plot(POSIXct, log10(qcomp.mod1), typ='l', log='y', col='green2', ylab=paste0(strCol.mod1),
                                main=ttext, ylim=c(minflow,maxflow)))
        }
    else {
        with(strDf, plot(POSIXct, qcomp.mod1, typ='l', col='green2', ylab=paste0(strCol.mod1),
                                main=ttext, ylim=c(0,maxflow)))
        }
    if (!is.null(strDf.mod2)) { with(strDf, lines(POSIXct, qcomp.mod2, col='blue')) }
    with(strDf, lines(POSIXct, qcomp.obs, col='black'))
    if (!is.null(strDf.mod2)) {
        legend('topright', c(labelMod1, labelMod2, labelObs), col=c('green2','blue','black'), lty=c(1,1,1), bg="white")
        mtext(c(paste0("MODEL1: NSE=", nseflow1, " Bias=", biasflow1, "%  MODEL2: NSE=", nseflow2, " Bias=", biasflow2, "%")), side=3, line=0.0, cex=0.9)
        }
    else {
        legend('topright', c(labelMod1, labelObs), col=c('green2','black'), lty=c(1,1), bg="white")
        mtext(c(paste0("MODEL: NSE=", nseflow1, " Bias=", biasflow1, "%")), side=3, line=0.0, cex=0.9)
        }
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
#' PlotWatBudg(wb.allrt.fc)
#'
#' ## Plot the same as a barchart.
#'
#' PlotWatBudg(wb.allrt.fc, "bar")
#' @export

PlotWatBudg <- function(wbDf, plottyp="pie") {

    lbls <- c("Canopy Evap", "Transpiration", "Surface Evap", "Surface Runoff",
             "Groundwater Outflow")
    pcts <- with(wbDf,c(LSM_ECAN/LSM_PRCP*100, LSM_ETRAN/LSM_PRCP*100, LSM_EDIR/LSM_PRCP*100,
                  (WB_SFCRNOFF + ifelse(is.na(HYD_QBDRY), 0.0, HYD_QBDRY)) / LSM_PRCP * 100,
                  WB_GWOUT/LSM_PRCP*100))
    lbls_pcts=c()
    for (i in 1:length(lbls)) { lbls_pcts[i] <- paste0(lbls[i], "\n", round(pcts[i],1), "%") }
    if (plottyp == "pie") {
        if (wbDf$STOR_FRAC > 0) {
            lbls_pcts[length(lbls_pcts)+1] <- paste0("Change in\nStorage", "\n",
                                                round( with( wbDf, (LSM_DELSOILM + LSM_DELSWE + LSM_DELCANWAT +
                                                ifelse(is.na(HYD_DELSFCHEAD), 0.0, HYD_DELSFCHEAD) +
                                                ifelse(is.na(HYD_DELGWSTOR), 0.0, HYD_DELGWSTOR)) / LSM_PRCP * 100), 1), "%")
            pie(as.matrix(with(wbDf, c(LSM_ECAN, LSM_ETRAN, LSM_EDIR,
                                        (WB_SFCRNOFF + ifelse(is.na(HYD_QBDRY), 0.0, HYD_QBDRY)),
                                        WB_GWOUT, LSM_DELSOILM + LSM_DELSWE + LSM_DELCANWAT +
                                        ifelse(is.na(HYD_DELSFCHEAD), 0.0, HYD_DELSFCHEAD) +
                                        ifelse(is.na(HYD_DELGWSTOR), 0.0, HYD_DELGWSTOR)))),
                col=c("chartreuse3","darkgreen","darkgoldenrod2","cornflowerblue","darkblue","grey30"),
                main=c("Water Budget"), labels=lbls_pcts)
            }
        else {
            pie(as.matrix(with(wbDf, c(LSM_ECAN, LSM_ETRAN, LSM_EDIR,
                                        (WB_SFCRNOFF + ifelse(is.na(HYD_QBDRY), 0.0, HYD_QBDRY)),
                                        WB_GWOUT))),
                col=c("chartreuse3","darkgreen","darkgoldenrod2","cornflowerblue","darkblue"),
                main=c("Water Budget"), labels=lbls_pcts)
            text(0,-1, paste0("*Storage Loss: ",
                        round( with( wbDf, (LSM_DELSOILM + LSM_DELSWE + LSM_DELCANWAT +
                                        ifelse(is.na(HYD_DELSFCHEAD), 0.0, HYD_DELSFCHEAD) +
                                        ifelse(is.na(HYD_DELGWSTOR), 0.0, HYD_DELGWSTOR)) /
                                        LSM_PRCP * 100), 1),"%"))
            } # end storage fraction split
        } # end pie
    else if (plottyp =="bar") {
        lbls_pcts[length(lbls_pcts)+1] <- paste0("Change in Storage", "\n",
                                round( with( wbDf, (LSM_DELSOILM + LSM_DELSWE + LSM_DELCANWAT +
                                        ifelse(is.na(HYD_DELSFCHEAD), 0.0, HYD_DELSFCHEAD) +
                                        ifelse(is.na(HYD_DELGWSTOR), 0.0, HYD_DELGWSTOR)) /
                                        LSM_PRCP * 100), 1), "%")
        plotDf <- with(wbDf,c(LSM_DELSOILM + LSM_DELSWE + LSM_DELCANWAT +
                                        ifelse(is.na(HYD_DELSFCHEAD), 0.0, HYD_DELSFCHEAD) +
                                        ifelse(is.na(HYD_DELGWSTOR), 0.0, HYD_DELGWSTOR),
                                        LSM_ECAN, LSM_ETRAN, LSM_EDIR,
                                        (WB_SFCRNOFF + ifelse(is.na(HYD_QBDRY), 0.0, HYD_QBDRY)),
                                        WB_GWOUT))
        plotDf1 <- abs(plotDf)
        ylabs <- round(c(0,cumsum(plotDf1))-((plotDf1[1]-plotDf[1])/2),0)
        par(mar = c(5.1, 4.1, 5.1, 12.1), xpd = TRUE)
        barplot(as.matrix(plotDf1), axes=FALSE,
            col=c("grey70", "chartreuse", "darkgreen", "orange", "cornflowerblue", "darkblue"),
            main=c("Water Budget"), xlim=c(0,1), width=0.6, space=0.2, ylab=c("Total Water (mm)"))
        axis(2,c(0,cumsum(plotDf1)),labels=ylabs)
        if (plotDf[1]>=0) { segments(0.0, 0.0, 1.0, 0.0, lty=2) } else { segments(0.0, cumsum(plotDf1)[1], 1.0, cumsum(plotDf1)[1], lty=2) }
        legend("topright", legend=lbls_pcts,fill=c("chartreuse", "darkgreen", "orange", "cornflowerblue", "darkblue","grey70"),
            inset=c(-0.5, 0), bg=c("white"), yjust=0.5, y.intersp=2)
        } # end bar
}


