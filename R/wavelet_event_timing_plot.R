
get_data_plot_time_avg_power <- function(wt, event=FALSE) {
    ## Return the data to plot the time-averaged wavelet spectrum.
    ## Default excludes the COI.
    if(event) {
        power <- wt$power.corr * ifelse(wt$event_timing$mask$event, 1, NA)
    } else {
        power <- wt$power.corr * ifelse(wt$event_timing$mask$coi, 1, NA)
    }
    
    time_avg_power <- data.frame(
        period=wt$period,
        Period=length(wt$period):1,
        Power=rowMeans(power, na.rm=TRUE)
    )

    if(event) {
        
        event_cluster_count <-
            wt$event_timing$all[, .(event_count= uniqueN(period_clusters)), by = period]
        time_avg_power <- merge(time_avg_power, event_cluster_count, by='period', all.x=TRUE)
        time_avg_power$event_count[which(is.na(time_avg_power$event_count))] <- 0
        time_avg_power$Power[which(is.nan(time_avg_power$Power))] <- 0
        time_avg_power <- plyr::rename(time_avg_power, c('Power'='Avg Power'))
        time_avg_power$`Avg over` <- 'events'
        
    } else {
        
        time_avg_power <- plyr::rename(time_avg_power, c('Power'='Avg Power'))
        time_avg_power$`Avg over` <- 'time'

    }

    time_avg_power$avg_power_values <- time_avg_power$`Avg Power`
    
    return(time_avg_power)    
}


get_data_plot_power <- function(wt, input_data, wt_field=NULL, event=FALSE) {
    ## Return the data to plot the wavelet power spectrum

    ## The wt_field allows an arbitray field to be combined with the
    ## COI and significance of a different field.

    input_obs <- subset(input_data, Streamflow == 'obs')
    
    if(is.null(wt_field)) {
        wps_matrix <- wt$power.corr
    } else {
        wps_matrix <- wt_field
    }
    rownames(wps_matrix) <- length(wt$period):1
    colnames(wps_matrix) <- input_obs$POSIXct
    wps <- setNames(reshape2::melt(wps_matrix), c('Period', 'Time', 'Power'))
    plot_df <- wps
    
    chunk_matrix <- wps_matrix
    for(pp in 1:length(wt$period)) chunk_matrix[pp,] <- input_obs$chunk
    chunk <- setNames(reshape2::melt(chunk_matrix), c('Period', 'Time', 'chunk'))
    plot_df$chunk <- chunk$chunk
    chunk <- NULL
    
    coi_matrix <- wt$event_timing$mask$coi
    rownames(coi_matrix) <- 1:length(wt$period)
    colnames(coi_matrix) <- input_obs$POSIXct
    coi <- setNames(reshape2::melt(coi_matrix), c('Period', 'Time', 'COI'))
    plot_df$COI <- coi$COI
    coi <- NULL
    
    signif_matrix <- wt$event_timing$mask$signif * 1
    rownames(signif_matrix) <- 1:length(wt$period)
    colnames(signif_matrix) <- input_obs$POSIXct
    signif <- setNames(reshape2::melt(signif_matrix), c('Period', 'Time', 'Significance'))
    plot_df$Significance <- signif$Significance
    signif <- NULL

    period_df <- data.frame(Period=length(wt$period):1, period=wt$period)
    plot_df <- merge(plot_df, period_df, by='Period')

    return(plot_df)
}


merge_data_plot <- function(
                            ...,
                            time_axis_len=1,
                            time_trans=NULL,
                            time_breaks=NULL,
                            time_label_format=NULL,
                            
                            avg_power_axis_len=1/6,
                            avg_power_trans=NULL,
                            avg_power_breaks=NULL,
                            avg_power_label_format=NULL,
                            
                            period_axis_len=1,
                            period_trans=NULL,
                            period_breaks=NULL,
                            period_label_format=NULL,
                            
                            streamflow_axis_len=1/2,
                            streamflow_trans=NULL,
                            streamflow_breaks=NULL,
                            streamflow_label_format=NULL
                            ) {
        
    raw_data <- list(...)
    
    x_levels <- c('Time', 'Avg Power', 'Event Avg Power')
    y_levels <- c('Period', 'Streamflow (cms)')
    
    plot_list <- list()
    for(item in 1:length(raw_data)) {
        plot_list[[item]] <- raw_data[[item]]
        
        for(dim in c('x', 'y')) {
            dim_levels <- list(x=x_levels, y=y_levels)[[dim]]
            for(var_name in intersect(dim_levels, names(plot_list[[item]]) )) {
                plot_list[[item]][[dim]] <- plot_list[[item]][[var_name]]
                plot_list[[item]][[var_name]] <- NULL
                plot_list[[item]][[paste0(dim,'_var')]] <- factor(var_name, levels=dim_levels)
            }
        }    
    }
    
    ## Now homogenize with empty cols/vars to perform a rbind.
    all_var_names <- unlist(plyr::llply(plot_list, function(list) names(list)))
    for(item in 1:length(raw_data)) {
        for(absent_var in setdiff(all_var_names, names(plot_list[[item]]))){
            plot_list[[item]][[absent_var]] <- NA
        }
    }

    plot_data <- plyr::ldply(plot_list)

    ## -------------------------------------------------------
    ## What dimensions do we have?
    wh_time <- which(plot_data$x_var == 'Time')
    wh_avg_power <- which(plot_data$x_var == 'Avg Power')
    wh_period <- which(plot_data$y_var == 'Period')
    wh_streamflow <- which(plot_data$y_var == 'Streamflow (cms)')
    
    ## -------------------------------------------------------
    ## Axis Breaks and Labels

    ## x - time
    if(length(wh_time)) {
        use_time <- if('POSIXct' %in% names(plot_data)) "POSIXct" else "x"
        if(is.null(time_breaks))
            time_breaks <- scales::pretty_breaks()(subset(plot_data, x_var == 'Time')[[use_time]])
                
        if(is.null(time_label_format)) {
            time_labels <- time_breaks
            if(use_time == 'x')
                time_labels <- as.POSIXct(time_breaks, tz='UTC', origin=rwrfhydro::PosixOrigin())
            time_labels <- scales::date_format('%d %b\n%Y')(time_labels)
        } else {
            time_labels <- time_label_format(time_breaks)
        }
    }

    ## x - avg_power
    if(length(wh_avg_power)) {
        if(is.null(avg_power_breaks))
            avg_power_breaks <-
                scales::pretty_breaks()(subset(plot_data, x_var == 'Avg Power')$avg_power_values)
        avg_power_labels <- avg_power_breaks
        if(!is.null(avg_power_label_format))
            avg_power_labels <- avg_power_label_format(avg_power_breaks)
    }

    ## y - period
    if(length(wh_period)) {
        if(is.null(period_breaks))
            period_breaks <-
                scales::pretty_breaks()(subset(plot_data, y_var == 'Period')$y)

        ## The breaks probably need rounded, or the nearest neighbor in the set needs found.
        period_labels <- as.character(period_breaks * NA)
        plot_period <- subset(plot_data, y_var == 'Period' & !is.na(period))
        for(bb in 1:length(period_breaks))
            period_labels[bb] <-
                plot_period$period[which(plot_period$y == period_breaks[bb])[1]]

        ## Do NOT allow transform of period?
        if(is.null(period_label_format)) {
            period_labels <- scales::number_format()(as.numeric(period_labels))
        } else {
            period_labels <- period_label_format(as.numeric(period_labels))
        }
    }
    
    ## y - streamflow
    if(length(wh_streamflow)) {
        if(is.null(streamflow_breaks))
            streamflow_breaks <-
                scales::pretty_breaks()(subset(
                                         plot_data,
                                         y_var == 'Streamflow (cms)'
                                     )$streamflow_values)
        streamflow_labels <- streamflow_breaks
        if(!is.null(streamflow_label_format))
            streamflow_labels <- streamflow_label_format(streamflow_breaks)
    }

    
    ## -------------------------------------------------------
    ## Transformations of axis values.
    
    ## Transform Avg Power axis:
    if(!is.null(avg_power_trans)) {
        plot_data$x[wh_avg_power] <- avg_power_trans()$trans(plot_data$x[wh_avg_power])
        avg_power_breaks <- avg_power_trans()$trans(avg_power_breaks)
    }

    ## Transform Period axis:
    if(!is.null(period_trans)) {
        plot_data$y[wh_period] <- period_trans()$trans(plot_data$y[wh_period])
        period_breaks <- period_trans()$trans(period_breaks)
    }

    ## Transform streamflow axis:
    if(!is.null(streamflow_trans)) {
        plot_data$y[wh_streamflow] <- streamflow_trans()$trans(plot_data$y[wh_streamflow])
        streamflow_breaks <- streamflow_trans()$trans(streamflow_breaks)
    }
    
    ## -------------------------------------------------------
    ## Axis scaling for relative plot sizes
    ## x-axis: Time vs Avg Power
    if(length(wh_time) & length(wh_avg_power)) {

        time_range <- diff(range(plot_data$x[wh_time], na.rm=TRUE))
        avg_power_range <- diff(range(plot_data$x[wh_avg_power], na.rm=TRUE))

        # scales::trans_new()$domain[1] is the lower-bound on transformed values.
        if(!is.null(avg_power_trans))
            avg_power_range <-
                diff(range( pmax(plot_data$x[wh_avg_power], avg_power_trans()$domain[1]) ))
        
        ## Leave the time range alone, scale the avg_power_range to be above the time range.
        xform_avg_power_absc <- function(data) {
            max(plot_data$x[wh_time])*2 +
                data *
                time_range/avg_power_range * avg_power_axis_len/time_axis_len
        }

        plot_data$x[wh_avg_power] <- xform_avg_power_absc(plot_data$x[wh_avg_power])

        avg_power_breaks <- xform_avg_power_absc(avg_power_breaks)
    }


    #stop()
    ## y-axis: Period vs Streamflow, Relative sizes in plot
    if(length(wh_period) & length(wh_streamflow)) {

        period_range <- diff(range(plot_data$y[wh_period], na.rm=TRUE))
        streamflow_range <- diff(range(plot_data$y[wh_streamflow], na.rm=TRUE))

        if(!is.null(period_trans))
            period_range <-
                diff(range( pmax(plot_data$y[wh_period], period_trans()$domain[1]) ))

        if(!is.null(streamflow_trans)) {
            streamflow_range <-
                diff(range( pmax(plot_data$y[wh_streamflow], streamflow_trans()$domain[1]) ))
            # streamflow_range <- streamflow_trans()$inverse(streamflow_range)
        }

        ## Leave the streamflow_range alone, scale the period_range to be above the time range.
        xform_period_axis <- function(data) {
            max(plot_data$y[wh_streamflow])*3 + # This multiplier needs to be >2
                data *
                (streamflow_range/period_range) * (period_axis_len/streamflow_axis_len)
        }

        plot_data$y[wh_period] <- xform_period_axis(plot_data$y[wh_period])

        period_breaks <- xform_period_axis(period_breaks)

    }

    x_breaks <- as.numeric(c())
    x_labels <- as.character(c())
    if(length(wh_time)) {
        x_breaks <- c(x_labels, time_breaks)
        x_labels <- c(x_labels, time_labels)
    }
    if(length(wh_avg_power)) {
        x_breaks <- c(x_breaks, avg_power_breaks)
        x_labels <- c(x_labels, avg_power_labels)
    }
    attr(plot_data, 'x_breaks') <- x_breaks
    attr(plot_data, 'x_labels') <- x_labels

    y_breaks <- as.numeric(c())
    y_labels <- as.character(c())
    if(length(wh_streamflow)) {
        y_breaks <- c(y_labels, streamflow_breaks)
        y_labels <- c(y_labels, streamflow_labels)
    }
    if(length(wh_period)) {
        y_breaks <- c(y_breaks, period_breaks)
        y_labels <- c(y_labels, period_labels)
    }
    attr(plot_data, 'y_breaks') <- y_breaks
    attr(plot_data, 'y_labels') <- y_labels

    
    return(plot_data)
}


plot_wavelet_events <- function(plot_data, do_plot=TRUE, base_size=9) {
    library(ggplot2)
    gg <- ggplot()

    ##-------------------------------------------------------
    ## Wavelet spectrum plots
    subset_power <- subset(plot_data, x_var == 'Time' & y_var == 'Period' & is.na(period_clusters))

    if(nrow(subset_power) > 0) {
        gg <-
            gg +
            
            geom_raster(
                data=subset_power,
                aes(x=x, y=y, fill=Power),
                interpolate=FALSE
            )  +
            
            geom_contour(
                data=subset_power,
                aes(x=x, y=y, z=Significance, group=chunk),
                bins=1,
                color='black',
                size=.5
            ) +
            
            geom_raster(
                data=subset_power,
                aes(x=x, y=y, alpha=COI),
                interpolate=FALSE,
                fill='white'
            ) +
            
            scale_fill_distiller(
                palette = "Accent", #"Set3", #"Accent", #"BrBG","PiYG", #"PRGn", #"Spectral",
                direction=-1,
                trans='log2',
                labels = scales::scientific_format(digits=2),
                na.value="transparent"
            ) +
            
            ## Abstract this out? at lest the FALSE level.
            scale_alpha_manual(values=c('TRUE'=0, 'FALSE'=.6), guide=FALSE)
    }

    ## -------------------------------------------------------
    ## Input timeseries
    n_timeseries = length(unique(plot_data$Streamflow))
    if(n_timeseries > 1) {
        subset_input <- subset(plot_data, x_var=='Time' & y_var=='Streamflow (cms)')
        streamflow_colors =
            RColorBrewer::brewer.pal('Paired', n=3)[c(2, setdiff(1:n_timeseries,2))]
        names(streamflow_colors) = c('obs', setdiff(unique(subset_input$Streamflow), 'obs'))
    } else {
        subset_input <- subset(plot_data,
                               x_var=='Time' &
                               y_var=='Streamflow (cms)' &
                               Streamflow=='obs')
        streamflow_colors =
            RColorBrewer::brewer.pal('Paired', n=3)[2]
        names(streamflow_colors) = unique(subset_input$Streamflow)
    }
    
    if(nrow(subset_input) > 0) {
        gg <-
            gg +
            
            geom_line(
                data=subset_input,
                aes(x=x, y=y, color=Streamflow),
                size=1.1
            ) +
            
            scale_color_manual(
                values=streamflow_colors
            )
    }
    
    ## -------------------------------------------------------
    ## Time-averaged power plots
    ## Avg power
    subset_t_avg <- subset(plot_data, x_var=='Avg Power' & y_var == 'Period')

    if(nrow(subset_t_avg) > 0) {
        gg <-
            gg +            
            geom_path(
                data=subset_t_avg,
                aes(x=x, y=y, linetype=`Avg over`)
            )

        ## Event power
        ##geom_path(data=subset(plot_data, x_var=='Event Avg Power'),
        ##          aes(x=x, y=y)) + #, size=event_count)) +
        ##geom_point(data=subset(data, x_var=='Event Avg Power'),
        ##          aes(x=x, y=y, shape=as.factor(event_count))) +
    }


    ## -------------------------------------------------------
    ## Plot layout/style
    ##my_breaks <- function(x) { if (max(x) < 6000) seq(0, 5000, 1000) else seq(0, 15000, 5000) }

    x_breaks <- as.numeric(attr(plot_data, 'x_breaks'))
    x_labels <- attr(plot_data, 'x_labels')
    y_breaks <- as.numeric(attr(plot_data, 'y_breaks'))
    y_labels <- attr(plot_data, 'y_labels')
    
    gg <-
        gg +
        
        facet_grid(y_var ~ x_var, scales='free', space='free', switch='y') +
        
        scale_y_continuous(
            expand = c(0, 0),
            position = "right",
            breaks=y_breaks,
            labels=y_labels
        ) +
        
        scale_x_continuous(
            expand = c(0, 0),
            breaks=x_breaks,
            labels=x_labels
        ) +
        
        theme_bw(base_size=base_size) +
        
        theme(
            legend.position="left",
            ##legend.key.height=unit(3, "line"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.spacing = unit(.25, "lines"),
            strip.background = element_rect(fill = 'grey90')
        )

    ## handle the grob... only if plotting.
    ## extract this as a useful funciton
    if(
        do_plot &
        nrow(subset_power) > 0 &
        nrow(subset_input) > 0 &
        nrow(subset_t_avg) > 0
    ) {
       
        ## https://stackoverflow.com/questions/49521848/remove-unused-facet-combinations-in-2-way-facet-grid
        suppressMessages(library(grid))
        suppressMessages(library(gtable))

        grob <- ggplotGrob(gg)

        ## remove lower right panel
        idx <- which(grob$layout$name %in% c("panel-2-2"))
        for (i in idx) grob$grobs[[i]] <- nullGrob()

        ## move x-axis on right panels up one panel
        idx <- which(grob$layout$name %in% c("axis-b-2"))
        grob$layout[idx, c("t", "b")] <- grob$layout[idx, c("t", "b")] - c(2, 1)

        ## move the y-axis on the lower right panel left one panel
        idx <- which(grob$layout$name %in% c("axis-r-2"))
        grob$layout[idx, c("l", "r")] <- grob$layout[idx, c("l", "r")] - c(1.1, 1.1)
        
        #grid.newpage()
        grid.draw(grob)
        return(invisible(grob))
        
    } else {

        if(do_plot) print(gg)
        return(invisible(gg))
        
    }    
    
}


facet_grob_adj <- function(gg,
                           del=NULL,
                           adj=NULL) {
    ## Del is a vector of grob$layout$names to delete.
    ## adj is a named list in grob$layout$name names each containing a
    ## list with 2 vectors, positions and values.
    
    suppressMessages(library(grid))
    suppressMessages(library(gtable))

    if(!('grob' %in% class(gg))) {
        grob <- ggplotGrob(gg)
    } else {
        grob <- gg
    }
    
    if(!is.null(del)) {
        for(to_del in del) {
            idx <- which(grob$layout$name %in% to_del)
            for (i in idx) grob$grobs[[i]] <- nullGrob()
        }
    }

    if(!is.null(adj)) {
        for(the_name in names(adj)) {
            to_adj = adj[[the_name]]
            idx <- which(grob$layout$name %in% the_name)
            grob$layout[idx, to_adj$position] <-
                grob$layout[idx, to_adj$position] + to_adj$values
        }
    }

    return(invisible(grob))
}


cluster_palette <- function(brewer_pal="Accent") {
  pal6 <- RColorBrewer::brewer.pal(name=brewer_pal, n=6)
  return(colorRampPalette(pal6, space='Lab'))
}

step1_figure <- function(wt_event) {

    library(dplyr)
    library(ggplot2)
    library(relayer) ## git hash 8a1d49e1707d9fcc1aaa83476a3d9a15448a1065

    obs_power <- get_data_plot_power(wt_event$obs$wt, wt_event$input_data)
    obs_t_avg_power <- get_data_plot_time_avg_power(wt_event$obs$wt)
    obs_event_t_avg_power <- get_data_plot_time_avg_power(wt_event$obs$wt, event=TRUE)
    ## Should include the ar1 spectrum

    ## Data to plot the maxima on the time-avg spectra with this dataframe
    event_t_avg_max_data <- wt_event$obs$wt$event_timing$time_avg[local_max == TRUE,]
    setnames(event_t_avg_max_data, 'power_corr', 'Avg Power')
    event_t_avg_max_data <- merge(
        event_t_avg_max_data,
        obs_event_t_avg_power[, c('period', 'Period', 'Avg over')],
        by=c('period'),
        all.x=TRUE, all.y=FALSE
    )

    # =============================================================================
    ## Plot the cluster numbers

    max_periods <- wt_event$obs$wt$event_timing$time_avg[local_max == TRUE]$period
    max_period_clusters <- wt_event$obs$wt$event_timing$all[ period %in% max_periods ]

    event_t_avg_max_period_clusters <- merge(
        max_period_clusters,
        wt_event$input_data[ Streamflow == 'obs', c('Time', 'input_index', 'POSIXct')],
        by.x='time',
        by.y='input_index',
        all.x=TRUE, all.y=FALSE
    )

    event_t_avg_max_period_clusters$time <- NULL
    event_t_avg_max_period_clusters <- merge(
        event_t_avg_max_period_clusters,
        obs_power[, c('period', 'Time', 'Period')],
        by=c('period', 'Time'),
        all.x=TRUE, all.y=FALSE
    )
    ## Have to include the y-limits or it will be squashed and have to sample
    ## all the raster coordinates to build the raster.
    minmax <- as.data.table(obs_power)[
        period == min(period)  | period == max(period) | Time == min(Time)
    ]
    
    ## hmmmm.... is it guaranteed that the min(Time) is all in the COI?
    minmax <- minmax[, period_clusters := as.integer(-1) ]
    event_t_avg_max_period_clusters <- merge(
        event_t_avg_max_period_clusters,
        minmax[, c('period', 'Time', 'period_clusters', 'Period')],
        by=c('period', 'Time', 'period_clusters', 'Period'),
        all.x=TRUE, all.y=TRUE
    )

    plot_data <-
        merge_data_plot(
            subset(wt_event$input_data, Streamflow=='obs'),
            obs_power,
            event_t_avg_max_period_clusters,
            obs_t_avg_power,
            obs_event_t_avg_power,
            event_t_avg_max_data,
            avg_power_axis_len=1/5,
            avg_power_breaks=c(10000, 100000),
            avg_power_label_format=scales::scientific_format(digits=0),
            ##avg_power_breaks = c(1e2, 1e3, 1e4, 1e5),
            ##avg_power_trans = scales::log10_trans,
            streamflow_axis_len=2/3
            #streamflow_breaks=c(.1,1,10,100,1000),
            #streamflow_trans=scales::log2_trans
            ## avg_power_axis_trans=scales::log10_trans
        )
    
    ## Add a new vertical facet for showing the "event cluster"
    new_y_levels <- c('Streamflow (cms)', 'Period', 'c Period', 'd Period')
    new_y_labels <- c('Streamflow (cms)', 'Period', 'Period',    'Period')
    y_labeller <- function(string) {
        labs <- new_y_labels
        names(labs) <- new_y_levels
        labs[string]
    }
    
    ## Copy the data (same fill values, different mask) for the new
    ## plot, showing the event cluster.
    wt_copy <- subset(plot_data, x_var == 'Time' & y_var == 'Period')
    wt_copy$y_var <- ordered("c Period", levels=new_y_levels)
    wt_copy$Power[which(wt_copy$COI == FALSE | wt_copy$Significance == 0)] <- NA

    ## Transform the factor levels on the original data.
    plot_data$y_var <- ordered(plot_data$y_var, levels=new_y_levels)
    
    plot_data$y_var[which(!is.na(plot_data$period_clusters))] <-
        ordered("d Period", levels=new_y_levels)    
    plot_data$period_clusters[which(plot_data$period_clusters < 0)] <- NA
    
    ## Put the event spectrum on the new axis.
    wh_events <- which(plot_data$`Avg over` == 'events')
    plot_data$y_var[wh_events]<- ordered(new_y_levels[3], levels=new_y_levels)

    ## Merge the new and old data.
    pd <- rbind(plot_data, wt_copy)
    
    ## Get the standard plot
    gg <- plot_wavelet_events(pd, do_plot=FALSE)
    
    ## Extend the standard plot.    
    
    ## Use annotations for units labels on the y-axes.
    plot_data <- as.data.table(plot_data)
    
    y_labs <-
        plot_data[,
            .(y_center=min(y, na.rm=TRUE)+.5*(max(y, na.rm=TRUE)-min(y, na.rm=TRUE)),
              x_loc=.1*(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))+max(x, na.rm=TRUE)
             ),
            by=c('y_var', 'x_var')
        ]
    y_labs <- subset(y_labs, x_var == 'Avg Power' | y_var == 'Streamflow (cms)')
    relab <- c(
        `c Period`="Period (hours)",
        `Period`='Period (hours)',
        `Streamflow (cms)`='Streamflow (cms)'
    )
    y_labs$lab <- relab[y_labs$y_var]
    
    ## Use the facet titles/labels to enumerate the steps in the process.
    new_y_levels <-
        c('Streamflow (cms)', 'Period', 'c Period',  'TimePer',           'Time', 'd Period')
    new_y_labels <-
        c('a.  Timeseries',   'b.  Obs WT', 'c. Obs WT Events', 'd.  Timing Errors', '', 'd. Period Clusters')
    the_labeller <- function(string) {
        labs <- new_y_labels
        names(labs) <- new_y_levels
        labs[string]
    }

    clust_numbers <- sort(as.integer(setdiff(plot_data$period_clusters, "None")))
    clust_fill_colors <- c(NA, cluster_palette()(length(clust_numbers)))
    names(clust_fill_colors) <- c('None', clust_numbers)
    
    gg2 <-
        gg +
        
        geom_raster(
            data=subset(pd, y_var == new_y_levels[3] & is.na(period_clusters)),
            aes(x=x, y=y, fill=Power),
            interpolate=FALSE,
            na.rm=TRUE
        ) +
        
        geom_path(
            data=subset(plot_data, y_var == new_y_levels[3] & is.na(local_max)),
            aes(x=x, y=y, linetype=`Avg over`)
        ) +
        
        geom_point(
            data=subset(plot_data, local_max == TRUE),
            aes(x=x, y=y, shape="Maxima"),
            color='grey70'
        ) +
        
        geom_raster(
           data=subset(plot_data, y_var == new_y_levels[6]), # & !is.na(period_clusters)),
           aes(x=x, y=y, fill_cluster=factor(period_clusters)),
           interpolate=FALSE, na.rm=FALSE
        ) %>% rename_geom_aes(new_aes = c("fill" = "fill_cluster")) +
        
        guides(
            color = guide_legend(order = 1),
            fill = guide_colorbar(order = 2),
            linetype = guide_legend(order=3)
        ) +
        
        facet_grid(
            y_var ~ x_var,
            labeller = labeller(y_var=y_labeller),
            scales='free',
            space='free',
            switch='y'
        ) +
        
        scale_fill_distiller(
            aesthetics = "fill", guide = "legend",
            palette = "Spectral", #Accent", #"Set3", #"Accent", #"BrBG","PiYG", #"PRGn"
            ##direction=-1,
            ##trans='log2',
            labels = scales::scientific_format(digits=2),
            na.value="transparent"
        ) +
        
        scale_fill_manual(
            name='Cluster Number',
            aesthetics="fill_cluster",
            guide="legend",
            ##palette=cluster_palette(),
            values=clust_fill_colors,
            na.translate=FALSE,
            na.value="transparent"
        ) +
        
        theme(legend.title=element_text(size=rel(0.8)))

    ## return(gg2)
    
    ## Deal with the unused bits.
    ## daGrob <- ggplotGrob(gg2)
    ## gtable::gtable_show_layout(daGrob)
    ## daGrob

    grob <- facet_grob_adj(
        gg2,
        del=c("panel-2-4", "panel-1-3"),
        adj=list(
            `axis-r-1`=list(
                position=c('l', 'r'),
                values=c(-1.1, -1.1)
            ),
            `axis-r-4`=list(
                position=c('l', 'r'),
                values=c(-1.1, -1.1)
            ),
            `strip-t-2`=list(
                position=c('t', 'b'),
                values=(c(0, 0) + 2)
            )
        )
    )

    text_color = 'grey30'
    text_size = 11
    
    text_grob_1 <- grid.text(
        'Streamflow (cms)', x=-1.25, y=.6, hjust=.50, vjust=-3.5, rot=-90,
        gp=gpar(col=text_color, fontsize=text_size)
    )
    ## size = rel(0.8), colour = "grey30"
    t <- unique(grob$layout[grepl("panel-1-1",grob$layout$name), "t"])
    l <- unique(grob$layout[grepl("panel-1-1",grob$layout$name), "l"])
    g <- gtable::gtable_add_grob(grob, grobs=text_grob_1, t=t, l=l+1, clip='off')

    text_grob_4 <- grid.text(
        'Period (hours)', x=0, y=.5, hjust=.5, vjust=-3.5, rot=-90,
        gp=gpar(col=text_color, fontsize=text_size)
    )
    t <- unique(grob$layout[grepl("panel-2-2",grob$layout$name), "t"])
    l <- unique(grob$layout[grepl("panel-2-2",grob$layout$name), "l"])
    g <- gtable::gtable_add_grob(g, grobs=text_grob_4, t=t, l=l+1, clip='off')
    
    # Insert a new column for labels
    g <- gtable::gtable_add_cols(g, grid::unit(1,"line"), pos = -1)
    
    text_grob_2 <- grid.text(
        'Period (hours)', x=0, y=.5, hjust=.5, vjust=.5, rot=-90,
        gp=gpar(col=text_color, fontsize=text_size)
    )
    t <- unique(grob$layout[grepl("panel-2-1",grob$layout$name), "t"])
    g <- gtable::gtable_add_grob(g, grobs=text_grob_2, t=t, l=ncol(g), clip='off')

    text_grob_3 <- grid.text(
        'Period (hours)', x=0, y=.5, hjust=.5, vjust=.5, rot=-90,
        gp=gpar(col=text_color, fontsize=text_size)
    )
    t <- unique(grob$layout[grepl("panel-1-4",grob$layout$name), "t"])
    g <- gtable::gtable_add_grob(g, grobs=text_grob_3, t=t, l=ncol(g), clip='off')

    return(g)
}


step2_figure <- function(wt_event, n_phase_along_x=70, base_size=9) {
    library(dplyr)
    library(ggplot2)
    library(relayer) ## git hash 8a1d49e1707d9fcc1aaa83476a3d9a15448a1065
    
    ## Currently this is only configured to handle a single modeled timeseries.
    model_name <- setdiff(names(wt_event), c("input_data", "obs"))
    
    wt_power <- get_data_plot_power(wt_event$obs$wt, wt_event$input_data)
    xwt_power <- get_data_plot_power(wt_event[[model_name]]$xwt, wt_event$input_data)
    ## Sub phase for power... 
    xwt_phase <- get_data_plot_power(
        wt_event[[model_name]]$xwt,
        wt_event$input_data,
        wt_field=wt_event[[model_name]]$xwt$phase
    )
    ## Timing is associate with the obs COI/signif
    xwt_timing <- get_data_plot_power(
        wt_event$obs$wt,
        wt_event$input_data,
        wt_field=wt_event[[model_name]]$xwt$event_timing$mtx$timing_err
    )
    
    ## Restructuring for plotting, these cant be merged because the all have the same Period
    ## axis. So do them individually with the input, remove the input, rename the y-axis, and
    ## combine.
    streamflow_axis_len = .5
    wt_data <-  merge_data_plot(
        wt_event$input_data,
        wt_power,
        streamflow_axis_len=streamflow_axis_len,
        streamflow_breaks=c(.1,1,10,100,1000),
        streamflow_trans=scales::log2_trans
    )
    xwt_data <- merge_data_plot(
        wt_event$input_data,
        xwt_power,
        streamflow_axis_len=streamflow_axis_len,
        streamflow_trans=scales::log2_trans
    )
    xwt_phase_data <- merge_data_plot(
        wt_event$input_data,
        xwt_phase,
        streamflow_axis_len=streamflow_axis_len,
        streamflow_trans=scales::log2_trans
    )
    timing_data <- merge_data_plot(
        wt_event$input_data,
        xwt_timing,
        streamflow_axis_len=streamflow_axis_len,
        streamflow_trans=scales::log2_trans
    )
    
    xwt_data <- subset(xwt_data, y_var == 'Period')
    xwt_phase_data <- subset(xwt_phase_data, y_var == 'Period')
    timing_data <- subset(timing_data, y_var == 'Period')
    
    xwt_data$phase <- xwt_phase_data$Power
    ## Subsample the xwt_phase_data for plotting 
    ## This one is gridded, easy selection to even indices.
    periods_rm <- sort(unique(xwt_phase_data$y))[c(TRUE, TRUE, FALSE)]
    ## This one is not necessarily gridded.  Convert to hours.
    times <- sort(unique(xwt_phase_data$x))/3600
    mod_by <-  length(times) %/% n_phase_along_x
    times_rm <- times[(times %% mod_by) != 0]*3600
    wh_rm_ends <- which(xwt_data$x %in% times_rm | xwt_data$y %in% periods_rm)
    xwt_data$phase[wh_rm_ends] <- NA
    
    ## Add a new vertical facet for showing the "event cluster"
    new_y_levels <-
        c('Streamflow (cms)',
          'Period',
          'XwtPer',
          'TimePer',
          'Time')
    new_y_labels <-
        c('a.  Timeseries',
          'b. Obs WT',
          'c.  XWT',
          'd. Sampled Timing Errors',
          '')
    the_labeller <- function(string) {
        labs <- new_y_labels
        names(labs) <- new_y_levels
        labs[string]
    }
    
    ## Transform the factor levels on the original data.
    wt_data$y_var <- ordered(wt_data$y_var, levels=new_y_levels)
    
    ## Rename the period axis on the other fields.
    xwt_data$y_var <- ordered('XwtPer', levels=new_y_levels)
    timing_data$y_var <- ordered("TimePer", levels=new_y_levels)
    
    ## JLM subjective shit
    timing_data$Power[which(timing_data$COI == FALSE)] <- NA
    timing_data$Power[which(timing_data$Significance == 0)] <- NA
    ##timing_data$Power[which(timing_data$COI == TRUE | timing_data$Significance == 0)] <- NA

    ## Merge the new and old data. Wait for the phase... 
    plot_data <- rbind(wt_data, timing_data)
    plot_data$phase <- NA
    plot_data <- rbind(plot_data, xwt_data)

    ## Annotated the y-axes and set the limits (due to the annotation non-clip)
    plot_data <- as.data.table(plot_data)
    y_labs <- plot_data[
         ,
        .(y_center=min(y)+.5*(max(y)-min(y)), x_loc=.1*(max(x)-min(x))+max(x)),
        by=y_var
    ]
    relab <- c("Streamflow (cms)", rep('Period (hours)',3)); names(relab) <- y_labs$y_var
    y_labs$lab <- relab[y_labs$y_var]
    
    x_breaks <- as.numeric(attr(plot_data, 'x_breaks'))
    x_labels <- attr(plot_data, 'x_labels')
    y_breaks <- as.numeric(attr(plot_data, 'y_breaks'))
    y_labels <- attr(plot_data, 'y_labels')
    
    xvals <- unique(subset(wt_data, y_var == 'Streamflow (cms)')$x)
    xlim <- range(xvals) + c(-1,1) * diff(range(xvals))/length(xvals) / 2

    ## Could have used the standard plot, but it was a bit clearer to redo the whole thing.
    ## If the two start diverging in a bad way, then may consider using it again.

    gg2 <-
        ggplot() +
        
        ## This describes how the plot is arranged.
        facet_grid(
            y_var ~ x_var,
            labeller = labeller(y_var=the_labeller, x_var=the_labeller),
            scales='free',
            space='free',
            switch='y'
        ) +
        
        ## Input timeseries
        geom_line(
            data=subset(plot_data, y_var == "Streamflow (cms)"),
            aes(x=x, y=y, color=Streamflow),
            size=1.1
        ) +
        
        ## WT 
        geom_raster(
            data=subset(plot_data, y_var == 'Period'),
            aes(x=x, y=y, fill=Power),
            interpolate=TRUE
        )  +
        
        geom_contour(
            data=subset(plot_data, y_var == "Period"),
            aes(x=x, y=y, z=Significance, group=chunk),
            bins=1,
            color='black',
            size=.5
        ) +
        
        geom_raster(
            data=subset(plot_data, y_var == "Period"),
            aes(x=x, y=y, alpha=COI),
            interpolate=TRUE,
            fill='white'
        ) +
        
        ## XWT
        geom_raster(
            data=subset(plot_data, y_var == 'XwtPer'),
            aes(x=x, y=y, fill_xwt=Power),
            interpolate=FALSE
        ) %>% rename_geom_aes(new_aes = c("fill" = "fill_xwt")) +
        
        geom_text(
            data=subset(plot_data, y_var == 'XwtPer' & !is.na(phase)),
            aes(x=x, y=y, angle=(180/pi)*phase),
            label='>',
            size=2.5
        ) +
        
        # Timing
        geom_raster(
            data=subset(plot_data, y_var == 'TimePer'),
            aes(x=x, y=y, fill_timing=Power),
            interpolate=FALSE
            #na.rm=FALSE
        )  %>% rename_geom_aes(new_aes = c("fill" = "fill_timing")) +
        
        geom_text(
            data=y_labs,
            aes(x=x_loc, y=y_center, label=lab),
            size=3.5,
            angle=-90,
            color='grey30'
        ) +
        
        coord_cartesian(clip = 'off', xlim = xlim) +
        
        labs(y = "") +
        
        scale_y_continuous(
            expand = c(0, 0),
            position = "right",
            breaks=y_breaks,
            labels=y_labels
        ) +
        
        scale_x_continuous(
            expand = c(0, 0),
            breaks=x_breaks,
            labels=x_labels
        ) +
    
        scale_color_brewer(
            palette='Paired', #'Set2'
            guide=guide_legend(order=100),
        ) +
        
        scale_alpha_manual(values=c('TRUE'=0, 'FALSE'=.6), guide=FALSE) +
        
        scale_fill_distiller(
            aesthetics='fill',
            name="WT Power",
            guide=guide_colorbar(order=75),
            palette = "Spectral"
        ) +
        
        scale_fill_distiller(
            name="XWT Power",
            aesthetics='fill_xwt',
            guide=guide_colorbar(order=50, available_aes = 'fill_xwt'),
            palette = "Spectral"
        ) +
        
        scale_fill_distiller(
            name = "Timing Error\n  (hours)",
            aesthetics='fill_timing',
            guide=guide_colorbar(order=1, available_aes = 'fill_timing'),
            palette = "Spectral",
            na.value="transparent"
        ) +
        
        theme_bw(base_size=base_size) +
        
        theme(
            legend.position="left",
            legend.title=element_text(size=rel(0.8)),
            ##legend.key.height=unit(3, "line"),
            axis.title.x = element_blank(),
                                        #axis.title.y = element_blank(),
            axis.title.y = element_text(color='grey40'),
            panel.spacing = unit(.25, "lines"),
            strip.background = element_rect(fill = 'grey90')
        )
    

    library(grid)
    p <- gg2
    g <- ggplot_gtable(ggplot_build(p))
    
    g <- gtable::gtable_add_cols(g, grid::unit(.5,"line"), pos = -1)
    
    ## This just removes fill and line around the g$layout$name's axis-t-* to
    ## repurpose it as a figure title.
    wh_strip <- which(grepl('strip-t', g$layout$name))
    fills <- c("transparent")
    kk <- 1
    for (ii in wh_strip) {
        jj <- which(grepl('rect', g$grobs[[ii]]$grobs[[1]]$childrenOrder))
        g$grobs[[ii]]$grobs[[1]]$children[[jj]]$gp$fill <- fills[kk]
        g$grobs[[ii]]$grobs[[1]]$children[[jj]]$gp$lwd <- 0
        kk <- kk+1
    }

    ## Reorder the guides. This is a random mess.
    wh_guide_box <- which(g$layout$name == 'guide-box')
    ## Adjust the height of the individual guides using the panels on the main plot.
    ##gtable::gtable_show_layout(g)
    ##gtable::gtable_show_layout(g$grobs[[wh_guide_box]])
    g$grobs[[wh_guide_box]]$heights[3:9] <- g$heights[8:14]
    
    wh_guides <- which(grepl('guides', g$grobs[[wh_guide_box]]$layout$name))
    guide_layout <- g$grobs[[wh_guide_box]]$layout[wh_guides,]
    ## That order flummoxes me, but it works.
    guide_layout <- guide_layout[c(4,1,3,2),]
    colnames(guide_layout) <- 1:4
    g$grobs[[wh_guide_box]]$layout[1:4,] <- guide_layout

    return(g)
}


event_cluster_timing_by_period <- function(wt_event, all_timing=FALSE, n_periods=NULL, ncol=3) {

    library(ggplot2)
    
    ## Currently this is only configured to handle a single modeled timeseries.
    model_name <- setdiff(names(wt_event), c("input_data", "obs"))

    wh_max_periods <- wt_event$obs$wt$event_timing$time_avg$local_max
    max_periods <- wt_event$obs$wt$event_timing$time_avg$period[wh_max_periods]

    wh_period_rows = which(wt_event[[model_name]]$xwt$period %in% max_periods)

    timing_err_full <- wt_event[[model_name]]$xwt$event_timing$mtx$timing_err[wh_period_rows,]

    if(length(dim(timing_err_full)) > 1) {
        dimnames(timing_err_full)[[1]] <- wt_event[[model_name]]$xwt$period[wh_period_rows]
        names(dimnames(timing_err_full))[1] <- "period"
        dimnames(timing_err_full)[[2]] <- sort(unique(wt_event[['input_data']]$input_index))
        names(dimnames(timing_err_full))[2] <- "time"
        plot_data0 <- data.table(reshape2::melt(timing_err_full, value.name='timing_err'))
    } else{
        plot_data0 <- data.table(
            timing_err=timing_err_full,
            time=wt_event[[model_name]]$xwt$t,
            period=max_periods
        )
    }

    # Time here is input_index
    event_timing_all <- wt_event[[model_name]]$xwt$event_timing$all[period %in% max_periods]
    event_timing_all$timing_err <- NULL

    ## data table merge on the numeric causes a failure because of float noise...
    event_timing_all$per_str = as.character(event_timing_all$period)
    event_timing_all$period <- NULL
    plot_data0$per_str = as.character(plot_data0$period)

    ## Join in the obs signf and the wxt significance
    ## we_stats <- we_hydro_stats(wt_event)
    plot_data <- merge(
        plot_data0,
        event_timing_all,
        by=c('time', 'per_str'),
        all.x=TRUE,
        all.y=FALSE
    )
    
    label_clusters <- function(values) {
        inner_labeller <- function(value) if(is.na(value)) 'None' else as.character(value)
        plyr::laply(values, inner_labeller)
    }

    plot_data$period_clusters <- factor(label_clusters(plot_data$period_clusters))

    clust_numbers <- sort(as.integer(setdiff(levels(plot_data$period_clusters), "None")))
    fill_colors <- c(NA, cluster_palette()(length(clust_numbers)))
    names(fill_colors) <- c('None', clust_numbers)
    
    periods <- unique(plot_data$period)
    period_facet_labeller = paste0('Period = ', format(periods, digits=2, nsmall=1))
    names(period_facet_labeller) <- periods

    max_period_stats = wt_event$obs$wt$event_timing$time_avg[local_max == TRUE,]
    max_period_stats$per_str <- as.character(max_period_stats$period)
    setkey(max_period_stats, power_corr, physical=TRUE)

    if(!is.null(n_periods)) {
        n_period_use = min(n_periods, length(unique(plot_data$per_str)))
        plot_data = plot_data[per_str %in% rev(max_period_stats$per_str)[1:n_period_use],]
    }

    plot_data$period_factor = factor(
        plot_data$per_str,
        levels = rev(max_period_stats$per_str)
    )

    we_hist_plot <- function(data) {    
        result <-
            ggplot(data) +
            geom_histogram(
                aes(
                    x=timing_err,
                    fill=period_clusters,
                    alpha=xwt_signif
                ),
                color='grey70',
                size=.1#, 
                ## binwidth=.2
            ) +
            facet_wrap(
                ~period_factor,
                scales='free_x',
                labeller=as_labeller(period_facet_labeller),
                ncol=ncol
            ) +
            scale_alpha_manual(
                name='XWT Significant',
                values=c(`TRUE`=1, `FALSE`=.2),
                breaks=c(TRUE, FALSE),
                na.translate=TRUE,
                na.value=.2
            ) +
            scale_fill_manual(
                values=fill_colors,
                name='Event Cluster Number',
                na.translate=TRUE
            ) +
            scale_x_continuous(name='Timing Error (hrs)') + 
            theme_bw()
        return(result)
    }
    
    #we_hist_plot(plot_data)
    invisible(we_hist_plot(subset(plot_data, !is.na(phase))))
}
