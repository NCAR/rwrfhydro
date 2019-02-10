
## This is a private function that requires having the source data locally.
WtMakeData <- function() {
    data_path <- '/Users/james/WRF_Hydro/rwrfhydro/data/NWM_v10_v11_v12_4Gages_TimingErrors.Rdata'
    print(load(data_path))

    pairedData$q_lateral <-pairedData$UTC_date <- pairedData$link <- NULL
    pairedData <- data.table::data.table(pairedData)

    sites_to_keep <- c("08159000", "09107000", "01075000", "06441500")
    data <- pairedData[ site_no %in% sites_to_keep, ]

    castdf <-  dcast(data, POSIXct + site_no ~ tag, value.var = "q_cms_mod")
    setkey(castdf, POSIXct, site_no)

    obsdf <- data[, c('POSIXct', 'site_no', 'q_cms_obs', 'action_flow')]
    obsdf <- unique(obsdf)

    setkey(obsdf, POSIXct, site_no)
    waveletTimingData <-  merge(castdf, obsdf, all=FALSE)

    out_file = '/Users/james/WRF_Hydro/rwrfhydro/data/waveletTimingData.rda'
    save(waveletTimingData, file=out_file)
}


WtGetEventData <- function(location=NA, event=NA, info=FALSE) {
    ## Start date is included (>=), end is included (<=).
    subset_info= list(

        onion_creek = list(
            site_no = '08159000',
            events = list(
                one_event  = list(start="2012-01-24 08:00:00", end="2012-01-27 07:00:00"),
                three_mos  = list(start="2012-01-01 08:00:00", end="2012-04-01 06:00:00"),
                one_year   = list(start="2012-01-01 08:00:00", end="2012-12-31 07:00:00"),
                five_years = list(start="2010-10-01 00:00:00", end="2016-11-30 23:00:00")
            )
         ),
        
        taylor_river= list(
            site_no = "09107000",
            events = list(
                one_season = list(start="2013-05-01 07:00:00", end="2013-07-01 05:00:00"),
                one_year   = list(start="2013-01-01 08:00:00", end="2013-12-31 06:00:00"),
                five_years = list(start="2010-10-01 00:00:00", end="2016-11-30 23:00:00")
            )
        ),
        
        pemigewasset_river= list(
            site_no = "01075000",
            events = list(
                one_event   = list(start="2012-03-01 08:00:00", end="2012-04-15 05:00:00"),
                small_event = list(start="2012-04-01 07:00:00", end="2012-05-26 05:00:00"),
                one_year    = list(start="2012-03-01 08:00:00", end="2013-02-28 06:00:00"),
                five_years  = list(start="2010-10-01 00:00:00", end="2016-11-30 23:00:00")
            )
        ),
        
        bad_river= list(
            site_no = "06441500",
            events = list(
                one_month  = list(start="2013-07-01 07:00:00", end="2013-07-25 05:00:00"),
                two_months = list(start="2013-05-15 07:00:00", end="2013-07-31 05:00:00"),
                one_year   = list(start="2013-01-01 08:00:00", end="2013-12-31 06:00:00"),
                five_years = list(start="2010-10-01 00:00:00", end="2016-11-30 23:00:00")
            )
        )
    )

    if (info) {
        print(subset_info)
        return(invisible(subset_info))
    }
    
    if (is.na(location)) {
        cat('Locations: ', paste0(names(subset_info), coll=', '), '\n')
        location <- readline(prompt=paste0("Please select a location:"))
        cat(
            'Events for ', location,': ',
            paste0(names(subset_info[[location]]$events), coll=', '),
            '\n'
        )
        event <- readline(prompt=paste0("Please select an event for this location:"))
    }
    
    output <-
        waveletTimingData[
            site_no == subset_info[[location]]$site_no &
            POSIXct >= as.POSIXct(subset_info[[location]]$events[[event]]$start, tz='UTC') &
            POSIXct <= as.POSIXct(subset_info[[location]]$events[[event]]$end, tz='UTC')
         ]

    return(output)
}


WtEventMask <- function(wt) {
    n_time <- length(wt$t)

    ## COI 0/1 mask: 0 means in the COI, 1 means not in the COI.
    coi_mask <- wt$signif * 0 # signif is a matrix of the correct dimension. 
    mode(coi_mask) <-  'logical' # save some space with a boolean that promotes
    mask <- list(coi = coi_mask)
    for (tt in 1:n_time) mask$coi[which(wt$period < wt$coi[tt]), tt] <- TRUE
    
    ## Signif mask
    signif_mask <- wt$signif * 0
    mode(signif_mask) <- 'logical'
    mask$signif <- signif_mask
    mask$signif[wt$signif >= 1] <- TRUE
    ## JLM TODO: is the signif mask the same for power and power.corr?
    ## Apparently so.
    
    ## Event mask
    mask$event <- mask$coi * mask$signif
    
    return(mask)
}


WtEventMtx <- function(wt) { # TODO JLM: The wt here is the extended one
    ## Event power spectra
    event_mtx <- list(power = wt$event_timing$mask$event * wt$power)
    event_mtx$power_corr <- wt$event_timing$mask$event * wt$power.corr

    ## Event clusters by period.
    n_periods = length(wt$period)

    event_mtx$period_clusters <- wt$event_timing$mask$event * -1
    options(warn=2)
    for (period in 1:n_periods){

        mask_vec <- wt$event_timing$mask$event[period,]

        result <- rle(mask_vec)
        class(result) <-  'list' # That is annoying
        result <- as.data.table(result)
        if (all(result$values %in% c(0))) next
        result$ends <- cumsum(result$lengths)
        result$starts <- c(1, result$ends[1:(nrow(result)-1)]+1)
        events <- result[values == 1,]

        for (ee in 1:nrow(events)) {
            event_mtx$period_clusters[period, events$starts[ee]:events$ends[ee]] <- ee
        }

        ## check
        if (!all(as.logical(event_mtx$period_clusters[period, ]) == as.logical(mask_vec)))
            stop('Problem with event cluster identification.')
    }
    
    return(event_mtx)
}

WtEventTiming <- function(time, obs, mod, max.scale=256) {
    ## TODO JLM: what determines max.scale? are those in hours?
    ## use ... or other kw for passing to wt, xwt.
    ## TODO JLM: are max.scale wt and xwt necessarily the same?
    ## JLM TODO: Gap handling. 
    ## JLM TODO: If there were discontinuities, would have a loop over the chunks.
    ## JLM TODO: If we break up the timeseries, should probably return the chunks of raw data too.
    
    n_time <- length(time)

    ## ---------------------------------------------------------------------------
    ## Observed.
    ## Observed timeseries is just one.
    obs_for_wt <- cbind(1:n_time, obs)
    wt_obs <- biwavelet::wt(obs_for_wt, max.scale=256)
    class(wt_obs) <- c("wavelet_timing", class(wt_obs))
    
    n_period <- length(wt_obs$period)

    output <- list(obs = list(wt = wt_obs))

    ## The masks 
    output[['obs']]$wt$event_timing$mask <- WtEventMask(output[['obs']]$wt)

    ## The event matrices
    output[['obs']]$wt$event_timing$event_mtx<- WtEventMtx(output[['obs']]$wt)
    
    ## Gather the "bulk" (all the) information needed for sampling phase/timing errors:
    ## No phase needed for the obs wt.
    wh_event_mask <- which(output[['obs']]$wt$event_timing$mask$event == 1, arr.ind=TRUE)
    output[['obs']]$wt$event_timing$all <-
        data.table::data.table(power_corr = output[['obs']]$wt$power.corr[wh_event_mask])
    output[['obs']]$wt$event_timing$all$period <- output[['obs']]$wt$period[wh_event_mask[,1]]
    output[['obs']]$wt$event_timing$all$time <-  output[['obs']]$wt$t[wh_event_mask[,2]]
    output[['obs']]$wt$event_timing$all$period_clusters <-
        output[['obs']]$wt$event_timing$event_mtx$period_clusters[wh_event_mask]
    ## sort all by period and time
    setkey(output[['obs']]$wt$event_timing$all, period, time)
    
    ## Calculate the time-averaged corrected wavelet power spectrum on the obs:
    output[['obs']]$wt$event_timing$time_avg <-
        output[['obs']]$wt$event_timing$all[,.(power_corr=mean(power_corr)),.(period)]

    ## Sort time_avg by period
    setkey(output[['obs']]$wt$event_timing$time_avg, period)
    
    ## Calculate the local maxima of the time-avg corrected WPS so we can sample timing
    ## on just the most important periods.
    output[['obs']]$wt$event_timing$time_avg$local_max <- 
        pastecs::turnpoints(output[['obs']]$wt$event_timing$time_avg$power_corr)$peaks

    
    ## ---------------------------------------------------------------------------
    ## Modeled.
    ## For the modeled timeseries, we loop over the named list of modeled timeseries.
    wt_mod = list()
    mod_names <-  names(mod)
    for (name in mod_names) {
        mod_for_wt <- cbind(1:n_time, mod[[name]])
        wt_mod[[name]] <- biwavelet::wt(mod_for_wt, max.scale=max.scale)
        class(wt_mod[[name]]) <- c("wavelet_timing", class(wt_mod[[name]]))
    }

    for (name in mod_names) {
        output[[name]] <- list(wt = wt_mod[[name]])
        output[[name]]$wt$event_timing$mask <- WtEventMask(output[[name]]$wt)
    }

    ## Do the modeled TS need event_mtx? I dont think so.
    
    ## The intersection stats.
    for (name in mod_names) {
        area_intersect <-
            sum(output[['obs']]$wt$event_timing$mask$event &
                output[[name]]$wt$event_timing$mask$event)
        output[[name]]$wt$event_timing$stats$obs_event_frac <-
            area_intersect / sum(output[['obs']]$wt$event_timing$mask$event)
        output[[name]]$wt$event_timing$stats$mod_event_frac <-
            area_intersect / sum(output[[name]]$wt$event_timing$mask$event)
    }
    ## TODO JLM: rename these?
    ## obs_event_frac is like a hit rate, 1 - mod_event_frac is like a FAR.
    
    ## The timing stats.

    ## Gather the "bulk" phase/timing errors:
    ##    No sampling, take all observed significant/event timing errors from the obs-mod xwt.
    for (name in mod_names) {
        mod_for_wt <- cbind(1:n_time, mod[[name]])
        output[[name]]$xwt <- biwavelet::xwt(obs_for_wt, mod_for_wt, max.scale=max.scale)
        class(output[[name]]$xwt) <- c("wavelet_timing", class(output[[name]]$xwt))
        
        ## The masks 
        output[[name]]$xwt$event_timing$mask <- WtEventMask(output[[name]]$xwt)

        ## The event matrices
        output[[name]]$xwt$event_timing$event_mtx<- WtEventMtx(output[[name]]$xwt)
    
        ## It's key that wh_event_mask is from the *obs* wt object and the modelex xwt object.
        wh_event_mask <-
            which(output[['obs']]$wt$event_timing$mask$event == 1 &
                  output[[name]]$xwt$event_timing$mask$event == 1  ,
                  arr.ind=TRUE)

        output[[name]]$xwt$event_timing$all <-
            data.table::data.table(phase = output[[name]]$xwt$phase[wh_event_mask])

        output[[name]]$xwt$event_timing$all$xwt_power_corr <-
            output[[name]]$xwt$power.corr[wh_event_mask]

        ## This is excessive but having trouble joining it from the obs later.
        output[[name]]$xwt$event_timing$all$obs_power_corr <-
            output[['obs']]$wt$power.corr[wh_event_mask]

        output[[name]]$xwt$event_timing$all$period <-
            output[[name]]$xwt$period[wh_event_mask[,1]]

        output[[name]]$xwt$event_timing$all$time <-
            output[[name]]$xwt$t[wh_event_mask[,2]]

        output[[name]]$xwt$event_timing$all$timing_err <-
            2*pi * output[[name]]$xwt$event_timing$all$phase *
            output[[name]]$xwt$event_timing$all$period

        output[[name]]$xwt$event_timing$all$period_clusters <-
            output[[name]]$xwt$event_timing$event_mtx$period_clusters[wh_event_mask]

        setkey(output[[name]]$xwt$event_timing$all, period, time)
        
        ## Stats on bulk timing errors
        output[[name]]$xwt$event_timing$all_stats <- list()
        output[[name]]$xwt$event_timing$all_stats$mean_timing_err <-
            mean(output[[name]]$xwt$event_timing$all$timing_err)
        output[[name]]$xwt$event_timing$all_stats$sd_timing_err <-
            sd(output[[name]]$xwt$event_timing$all$timing_err)        
    }


    ## Time-averaged maxima sampling of phase errors.
    wh_peak <- output[['obs']]$wt$event_timing$time_avg$local_max
    peak_periods <- output[['obs']]$wt$event_timing$time_avg$period[wh_peak]

    for (name in mod_names) {
        keep_cols <- c('obs_power_corr', 'time', 'period', 'timing_err', 'period_clusters')
        all_sub <- output[[name]]$xwt$event_timing$all[, keep_cols, with=FALSE]
        all_sub <- all_sub[ period %in% peak_periods, ]

        all_sub <- all_sub[,
                        pow_max := .(max_obs_power_corr=max(obs_power_corr)),
                        .(period, period_clusters)]
        output[[name]]$xwt$event_timing$time_avg <-
            all_sub[ obs_power_corr == pow_max, c('period_clusters', keep_cols), with=FALSE ]

        setkey(output[[name]]$xwt$event_timing$time_avg, period)
        
        ## Stats on period-averaged timing errors
        output[[name]]$xwt$event_timing$time_avg_stats <- list()
        output[[name]]$xwt$event_timing$time_avg_stats$mean_timing_err <-
            mean(output[[name]]$xwt$event_timing$time_avg$timing_err)
        output[[name]]$xwt$event_timing$time_avg_stats$sd_timing_err <-
            sd(output[[name]]$xwt$event_timing$time_avg$timing_err)
    }

    
    return(output)
}
