# These are the non-zero dimension variables we have in the wavelet transforms
wt_vars_1d <- c("coi", "t", "xaxis", "chunk")
wt_vars_2d <- c("wave", "power", "power.corr", "phase", "signif")


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


WtEventMtx <- function(wt) {
    ## TODO JLM: The input wt is the extended one (?)

    ## Event power spectra
    event_mtx <- list(power = wt$event_timing$mask$event * wt$power)
    event_mtx$power_corr <- wt$event_timing$mask$event * wt$power.corr

    ## Event clusters by period.
    n_periods = length(wt$period)

    event_mtx$period_clusters <- wt$event_timing$mask$event * -1

    for (period in 1:n_periods){
        mask_vec <- wt$event_timing$mask$event[period,]

        result <- rle(mask_vec)
        class(result) <-  'list' # That is annoying
        result <- as.data.table(result)
        if (all(result$values %in% c(NA,0))) next

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


WtTimeChunks <- function(
    input_data, obs_name, mod_name=NULL, max.scale=256,
    rm_chunks_warn=TRUE, rm_chunks_error=TRUE) {

    chunk_list <- list()
    the_chunks <- unique(input_data$chunk)
    for(cc in 1:length(the_chunks)) {
      input_chunk <- subset(input_data, chunk == the_chunks[cc])
      obs_for_wt <- cbind(1:nrow(input_chunk), input_chunk[[obs_name]])
      
      if(is.null(mod_name)) {
        ## regular wavelet transform
        result = tryCatch(
          {
            chunk_list[[cc]] <-
              biwavelet::wt(obs_for_wt, max.scale=max.scale)
            chunk_list[[cc]]$chunk <- chunk_list[[cc]]$t * 0 + cc
          }, warning = function(w) {
            print("Some wavelet transforms are returning warnings.")
            if(!rm_chunks_warn) stop("rm_chunks_warn == FALSE")
            chunk_list[[cc]] <- NA
          }, error = function(e) {
            print("Some wavelet transforms are returning errors.")
            if(!rm_chunks_error) stop("rm_chunks_error == FALSE")
            chunk_list[[cc]] <- NA
          })
        
      } else {
        
        ## xwt
        mod_for_wt <- cbind(1:nrow(input_chunk), input_chunk[[mod_name]])
        result = tryCatch({
          chunk_list[[cc]] <-
            biwavelet::xwt(obs_for_wt, mod_for_wt, max.scale=max.scale)
          chunk_list[[cc]]$chunk <- chunk_list[[cc]]$t * 0 + cc
        }, warning = function(w) {
          print("Some cross-wavelet transforms are returning warnings.")
          if(!rm_chunks_warn) stop("rm_chunks_warn == FALSE")
          chunk_list[[cc]] <- NA
        }, error = function(e) {
          print("Some cross-wavelet transforms are returning errors.")
          if(!rm_chunks_error) stop("rm_chunks_error == FALSE")
          chunk_list[[cc]] <- NA
        })
      }
    }

    ## any shenanigans about time or xaxis and gaps, or leave it to POSIXct?
    first_ind <- 1
    while(class(chunk_list[[first_ind]]) != 'biwavelet'){
      first_ind <- first_ind + 1
    }
    output <- chunk_list[[first_ind]]

    if(length(chunk_list) > first_ind) {
      for(cc in (first_ind+1):length(chunk_list)) {

        if(class(chunk_list[[cc]]) != "biwavelet") {
          print(cc)
          next
        }

        for(var in wt_vars_1d)
          output[[var]] <- c(output[[var]], chunk_list[[cc]][[var]])

        for(var in wt_vars_2d)
          output[[var]] <- cbind(output[[var]], chunk_list[[cc]][[var]])

        }
    }

    return(output)
}


WtEventTiming <- function(POSIXct, obs,
                          mod=NULL,
                          max.scale=256,
                          min_ts_length=max.scale * time_step_h,
                          time_step_h=NULL,
                          obs_wps_tavg_smooth_scale=NULL,
                          rm_chunks_warn=TRUE, rm_chunks_error=TRUE) {

    ## TODO JLM: max.scale is unitless but min_ts_length is not? max.scale*time_step_h gives
    ## a unit for it.

    ## use ... or other kw for passing to wt, xwt.
    ## TODO JLM: are max.scale wt and xwt necessarily the same?
    ## TODO JLM: option for reducing this object size if only stats are required.
    ##           most of the bloat is for plotting.

    n_time <- length(POSIXct)
    output <- list()

    ## ---------------------------------------------------------------------------
    ## Input data
    ## Save of a data frame, this facilitates plotting. Could make this optional.
    input_data <-
        as.data.table(
            data.frame(
                POSIXct=POSIXct,
                Time=as.numeric(POSIXct),
                input_index=1:length(POSIXct),
                obs=obs
            )
        )

    for(key in names(mod)) input_data[[key]] <- mod[[key]]

    ## -------------------------------------------------------
    ## Deal with missing data
    ## Missing data across all of (obs, model1, ..., modelN) should be removed.
    ## Generally, model data wont be missing but if we just did this on the obs and
    ## data were missing for one model instance and not another, the results would
    ## not be consistent.

    ## Break up the data in to contiguous time chunks.
    input_data <- input_data[complete.cases(input_data),]
    ## Label time chunks
    ## How do we know the timestep?
    if(is.null(time_step_h))
        time_step_h <- median(diff(input_data$POSIXct)) # This is a timediff.
    ## Need to ensure that a passed value is a timediff.
    wh_gt_time_step <- which(diff(input_data$POSIXct) > time_step_h) + 1
    input_data$chunk <- 0
    input_data$chunk[wh_gt_time_step] <- 1
    input_data$chunk <- cumsum(input_data$chunk) + 1

    ## Filter out chunks less than some size
    chunk_len = input_data[, .(len_h = difftime(max(POSIXct),  min(POSIXct), units='hours')),
                             by='chunk']
    rm_chunks <- subset(chunk_len, len_h < min_ts_length)$chunk
    if(length(rm_chunks))
        input_data <- input_data[ !(chunk %in% rm_chunks) ]

    if(nrow(input_data) == 0) {
        msg <- paste0("All contiguous chunks in the input data were shorter ",
                      "than min_ts_length. Returning.")
        cat(msg,'\n')
        cat(paste0("min_ts_length: ", min_ts_length), '\n')
        print(chunk_len)
        return(NULL)
    }

    ## Melt just for the output list
    output[['input_data']] <- melt(
        input_data,
        id.vars=c('Time', 'POSIXct', 'chunk', 'input_index'),
        variable.name='Streamflow',
        value.name='Streamflow (cms)'
    )
    ## Why this extra copy?
    output[['input_data']]$streamflow_values <- output[['input_data']]$`Streamflow (cms)`

    ## ---------------------------------------------------------------------------
    ## Observed wavelet transforms.
    ## Observed timeseries is just one.
    wt_obs <- WtTimeChunks(input_data, obs_name='obs', max.scale=max.scale,
                           rm_chunks_warn=rm_chunks_warn, rm_chunks_error=rm_chunks_error)
    class(wt_obs) <- c("wavelet_timing", class(wt_obs))
    n_period <- length(wt_obs$period)

    ## ---------------------------------------------------------------------------
    ## Modeled wavelet transforms
    ## For the modeled timeseries, we loop over the named list of modeled timeseries.
    wt_mod = list()
    mod_names <-  names(mod)
    for (name in mod_names) {
      wt_mod[[name]] <- WtTimeChunks(input_data, name, max.scale=max.scale,
                                     rm_chunks_warn=rm_chunks_warn, rm_chunks_error=rm_chunks_error)
      class(wt_mod[[name]]) <- c("wavelet_timing", class(wt_mod[[name]]))
    }

    ## Cross-wavelet transforms for each modeled timeseries
    xwts = list()
    for (name in mod_names) {
      xwts[[name]] <-
        WtTimeChunks(input_data, obs_name='obs', mod_name=name, max.scale=max.scale,
                     rm_chunks_warn=rm_chunks_warn, rm_chunks_error=rm_chunks_error)
      class(xwts[[name]]) <- c("wavelet_timing", class(xwts[[name]]))
    }
    
    # Reduce the modeled and observed to the same sets of chunks
    get_unique_chunks = function(ll) unique(ll$chunk)

    obs_chunks = get_unique_chunks(wt_obs)

    # for the modeled and xwt, only keep chunks which are in all models/xwts
    get_model_intersect_chunks = function(the_wts) {
      chunk_list = plyr::llply(the_wts, get_unique_chunks)
      return(list(union= Reduce(union, chunk_list),
                  intersection=Reduce(intersect, chunk_list)))
    }

    model_chunks = get_model_intersect_chunks(wt_mod)
    xwt_chunks = get_model_intersect_chunks(xwts)
    
    combined_intersect = intersect(
      intersect(obs_chunks, model_chunks$intersection), xwt_chunks$intersection)
    combined_union = union(
      union(obs_chunks, model_chunks$union), xwt_chunks$union)

    keep_chunks = function(wt, chunk_nums_vec) {
      wh_chunk = which(wt$chunk %in% chunk_nums_vec)
      for(var in wt_vars_1d) wt[[var]] <- wt[[var]][wh_chunk]
      for(var in wt_vars_2d) wt[[var]] <- wt[[var]][,wh_chunk]
      return(wt)
    }

    if(length(combined_intersect) != length(combined_union)) {
      # observed
      wt_obs <- keep_chunks(wt_obs, combined_intersect)
      # modeled
      for (name in mod_names) {
        wt_mod[[name]] <- keep_chunks(wt_mod[[name]], combined_intersect)
        xwts[[name]] <- keep_chunks(xwts[[name]], combined_intersect)
      }
    }

        
    ## -----------------------------------------------------------------------------
    ## Operate on the output list.

    ## -------------------------------------------------------
    ## Observed stats
    ## Put the obs in the output data 
    output[['obs']] = list(wt = wt_obs)

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

    output[['obs']]$wt$event_timing$all$time <-
        output[['input_data']][Streamflow == 'obs']$input_index[wh_event_mask[,2]]

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

    ## Smooth the time_avg WPS?
    if(!is.null(obs_wps_tavg_smooth_scale)) {
        output[['obs']]$wt$event_timing$time_avg$power_corr_smooth <-
            caTools::runmean(
                         output[['obs']]$wt$event_timing$time_avg$power_corr,
                         obs_wps_tavg_smooth_scale,
                         endrule=c("keep"))

        output[['obs']]$wt$event_timing$time_avg$local_max_smooth <-
            pastecs::turnpoints(output[['obs']]$wt$event_timing$time_avg$power_corr_smooth)$peaks

        attr(output[['obs']]$wt$event_timing$time_avg, 'obs_wps_tavg_smooth_scale') <-
            obs_wps_tavg_smooth_scale

        wh_smooth <- which(output[['obs']]$wt$event_timing$time_avg$local_max_smooth)
        wh_orig <- which(output[['obs']]$wt$event_timing$time_avg$local_max)
        wh_smooth_orig <- wh_smooth * NA
        for(ii in 1:length(wh_smooth)) wh_smooth_orig[ii] = wh_orig[which.min(abs(wh_orig - wh_smooth[ii]))]
        output[['obs']]$wt$event_timing$time_avg$local_max_nn_smooth <-
            output[['obs']]$wt$event_timing$time_avg$local_max * FALSE
        output[['obs']]$wt$event_timing$time_avg$local_max_nn_smooth[wh_smooth_orig] <- TRUE
    }

    if(is.null(mod)) return(output)

    # -------------------------------------------------------
    # event timing for the modeled
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
        output[[name]]$xwt <- xwts[[name]]
      
        ## Calculate the timing error matrix
        output[[name]]$xwt$event_timing$mtx$timing_err <- output[[name]]$xwt$phase * NA
        for(rr in 1:nrow(output[[name]]$xwt$phase)) {
            output[[name]]$xwt$event_timing$mtx$timing_err[rr,] <-
                 output[[name]]$xwt$period[rr] *
                output[[name]]$xwt$phase[rr,] / (2*pi)
        }

        ## The masks
        output[[name]]$xwt$event_timing$mask <- WtEventMask(output[[name]]$xwt)

        ## The event matrices
        output[[name]]$xwt$event_timing$event_mtx <- WtEventMtx(output[[name]]$xwt)

        ## It's key that wh_event_mask is from the *obs* wt object and the modelex xwt object.
        wh_event_mask <-
            which(output[['obs']]$wt$event_timing$mask$event == 1, arr.ind=TRUE)

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
            output[['input_data']][Streamflow == 'obs']$input_index[wh_event_mask[,2]]

        output[[name]]$xwt$event_timing$all$timing_err <-
            output[[name]]$xwt$event_timing$all$period *
            output[[name]]$xwt$event_timing$all$phase / (2*pi)

        ## The period clusters are FOR THE OBSERVATIONS, not the modeled
        output[[name]]$xwt$event_timing$all$period_clusters <-
            output[['obs']]$wt$event_timing$event_mtx$period_clusters[wh_event_mask]

        # Is this observed event significant in the XWT?
        output[[name]]$xwt$event_timing$all$xwt_signif <-
            output[['obs']]$wt$event_timing$mask$event[wh_event_mask] == 1 &
            output[[name]]$xwt$event_timing$mask$event[wh_event_mask] == 1

        setkey(output[[name]]$xwt$event_timing$all, period, time)
    }

    ## TODO JLM: strip off data.tables?
    return(output)
}


we_hydro_stats <- function(wt_event) {

    output <- list()
    mod_names <- setdiff(names(wt_event), c("input_data", "obs"))

    ## -------------------------------------------------------
    ## "Bulk" stats timing errors
    for (name in mod_names) {
        output[[name]]$xwt$event_timing$bulk_stats <- list()
        output[[name]]$xwt$event_timing$bulk_stats$mean_timing_err <-
            mean(wt_event[[name]]$xwt$event_timing$all$timing_err)
        output[[name]]$xwt$event_timing$bulk_stats$sd_timing_err <-
            sd(wt_event[[name]]$xwt$event_timing$all$timing_err)
    }

    ## -------------------------------------------------------
    ## Extract the periods of interest from the obs wt analysis.
    wh_peak <- wt_event[['obs']]$wt$event_timing$time_avg$local_max
    peak_periods <- wt_event[['obs']]$wt$event_timing$time_avg$period[wh_peak]

    ## -------------------------------------------------------
    ## Mean timing errors by period.

    for (name in mod_names) {
        keep_cols <- c('obs_power_corr', 'time', 'period', 'timing_err', 'period_clusters')
        all_sub <- wt_event[[name]]$xwt$event_timing$all[, keep_cols, with=FALSE]
        all_sub <- all_sub[ period %in% peak_periods, ]

        output[[name]]$xwt$event_timing$time_avg <-
            wt_event[[name]]$xwt$event_timing$all[
                period %in% peak_periods,
                .(time_err=mean(timing_err),
                  obs_power_corr=mean(obs_power_corr),
                  xwt_power_corr=mean(xwt_power_corr),
                  xwt_signif=mean(xwt_signif),
                  n_clusters=length(unique(period_clusters)),
                  time=mean(time)
                 ),
                .(period)
            ]

        setkey(output[[name]]$xwt$event_timing$time_avg, period)

    }

    ## -------------------------------------------------------
    ## Cluster-mean timing errors on maxima of time-averaged obs wt power.
    for (name in mod_names) {

        output[[name]]$xwt$event_timing$cluster_mean <-
            wt_event[[name]]$xwt$event_timing$all[
                period %in% peak_periods,
                .(time_err=mean(timing_err),
                  obs_power_corr=mean(obs_power_corr),
                  xwt_power_corr=mean(xwt_power_corr),
                  xwt_signif=mean(xwt_signif),
                  time=mean(time)
                  ),
                by=c("period_clusters", "period")
            ]

        output[[name]]$xwt$event_timing$cluster_mean_time_avg <-
            output[[name]]$xwt$event_timing$cluster_mean[
                 ,
                .(time_err=mean(time_err),
                  obs_power=mean(obs_power_corr),
                  xwt_power=mean(xwt_power_corr),
                  xwt_signif_frac=mean(xwt_signif),
                  n_clusters=.N
                 ),
                by='period'
            ]

        setkey(output[[name]]$xwt$event_timing$cluster_mean, period)
    }

    ## -------------------------------------------------------
    ## Cluster-MAX timing errors on maxima of time-averaged obs wt power.
    for (name in mod_names) {

        output[[name]]$xwt$event_timing$cluster_max <-
            wt_event[[name]]$xwt$event_timing$all[
                period %in% peak_periods,
                .(time_err=timing_err[which.max(obs_power_corr)],
                  obs_power_corr=obs_power_corr[which.max(obs_power_corr)],
                  xwt_power_corr=xwt_power_corr[which.max(obs_power_corr)],
                  xwt_signif=xwt_signif[which.max(obs_power_corr)],
                  time=time[which.max(obs_power_corr)]
                  ),
                by=c("period_clusters", "period")
            ]

        output[[name]]$xwt$event_timing$cluster_max_time_avg <-
            output[[name]]$xwt$event_timing$cluster_max[
                 ,
                .(time_err=mean(time_err),
                  obs_power=mean(obs_power_corr),
                  xwt_power=mean(xwt_power_corr),
                  xwt_signif_frac=mean(xwt_signif),
                  n_clusters=.N
                 ),
                by='period'
            ]

        setkey(output[[name]]$xwt$event_timing$cluster_max, period)
    }

    return(output)

}
