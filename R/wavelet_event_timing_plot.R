
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
    
    return(time_avg_power)    
}


get_data_plot_power <- function(wt, event=FALSE) {
    ## Return the data to plot the wavelet power spectrum
    wps_matrix <- wt$power.corr
    rownames(wps_matrix) <- length(wt$period):1
    colnames(wps_matrix) <- wt$t
    wps <- setNames(reshape2::melt(wps_matrix), c('Period', 'Time', 'Power'))
    plot_df <- wps
    
    coi_matrix <- wt$event_timing$mask$coi
    rownames(coi_matrix) <- 1:length(wt$period)
    colnames(coi_matrix) <- wt$t
    coi <- setNames(reshape2::melt(coi_matrix), c('Period', 'Timde', 'COI'))
    plot_df$COI <- coi$COI
    coi <- NULL
    
    signif_matrix <- wt$event_timing$mask$signif * 1
    rownames(signif_matrix) <- 1:length(wt$period)
    colnames(signif_matrix) <- wt$t
    signif <- setNames(reshape2::melt(signif_matrix), c('Period', 'Time', 'Significance'))
    plot_df$Significance <- signif$Significance
    signif <- NULL

    return(plot_df)
}



merge_data_plot <- function(
                            ...,
                            time_axis_len=1,
                            time_axis_trans=NULL,
                            avg_power_axis_len=1/6,
                            avg_power_axis_trans=NULL,
                            period_axis_len=1,
                            period_axis_trans=NULL,
                            streamflow_axis_len=1/2,
                            streamflow_axis_trans=NULL
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

    ## Transformations.
    ## Where are the dimensions in the dataframe?
    wh_time <- which(plot_data$x_var == 'Time')
    wh_avg_power <- which(plot_data$x_var == 'Avg Power')
    wh_period <- which(plot_data$y_var == 'Period')
    wh_streamflow <- which(plot_data$y_var == 'Streamflow (cms)')

    ## Transform streamflow axis:
    if(!is.null(streamflow_axis_trans))
       plot_data$y[wh_streamflow] <- streamflow_axis_trans()$trans(plot_data$y[wh_streamflow])

    ## Transform Period axis:
    if(!is.null(period_axis_trans))
       plot_data$y[wh_period] <- period_axis_trans()$trans(plot_data$y[wh_period])

    ## Transform Avg Power axis:
    if(!is.null(avg_power_axis_trans))
       plot_data$x[wh_avg_power] <- avg_power_axis_trans()$trans(plot_data$x[wh_avg_power])

    
    ## x-axis: Time vs Avg Power, Relative sizes in plot
    if(length(wh_time) & length(wh_avg_power)) {

        time_range <- diff(range(plot_data$x[wh_time], na.rm=TRUE))
        avg_power_range <- diff(range(plot_data$x[wh_avg_power], na.rm=TRUE))

        if(!is.null(avg_power_axis_trans))
            avg_power_range <-
                diff(range(pmax(plot_data$x[wh_avg_power], avg_power_axis_trans()$domain[1])))
        
        ## Leave the time range alone, scale the avg_power_range
        plot_data$x[wh_avg_power] <-
            plot_data$x[wh_avg_power] *
            time_range/avg_power_range * avg_power_axis_len/time_axis_len

    }

    
    ## y-axis: Period vs Streamflow, Relative sizes in plot
    if(length(wh_period) & length(wh_streamflow)) {

        period_range <- diff(range(plot_data$y[wh_period], na.rm=TRUE))
        streamflow_range <- diff(range(plot_data$y[wh_streamflow], na.rm=TRUE))
        
        ## Leave the period range alone, scale the streamflow_range
        plot_data$y[wh_streamflow] <-
            plot_data$y[wh_streamflow] *
            period_range/streamflow_range * streamflow_axis_len/period_axis_len

    }


    
    return(plot_data)
}



plot_wavelet_events <- function(plot_data) {
    gg <- ggplot()

    ##-------------------------------------------------------
    ## Wavelet spectrum plots
    subset_power <- subset(plot_data, x_var=='Time' & y_var == 'Period')

    if(nrow(subset_power) > 0) {
        gg <-
            gg +
            
            geom_raster(
                data=subset_power,
                aes(x=x, y=y, fill=Power),
                interpolate=TRUE
            )  +
            
            geom_contour(
                data=subset_power,
                aes(x=x, y=y, z=Significance),
                bins=1,
                color='black',
                size=.5
            ) +
            
            geom_raster(
                data=subset_power,
                aes(x=x, y=y, alpha=COI),
                interpolate=TRUE,
                fill='white'
            ) +
            
            scale_fill_distiller(
                palette = "Accent", #"Set3", #"Accent", #"BrBG","PiYG", #"PRGn", #"Spectral",
                direction=-1,
                trans='log2',
                labels = scales::scientific_format(digits=2)
            ) +
            
            ## Abstract this out? at lest the FALSE level.
            scale_alpha_manual(values=c('TRUE'=0, 'FALSE'=.6), guide=FALSE)
    }

    ## -------------------------------------------------------
    ## Input timeseries
    subset_input <- subset(plot_data, x_var=='Time' & y_var=='Streamflow (cms)')
    if(nrow(subset_input) > 0) {
        gg <-
            gg +
            
            geom_line(
                data=subset_input,
                aes(x=x, y=y, color=Streamflow),
                size=1.1
            ) +
            
            scale_color_brewer(
                palette='Paired' #'Set2'
            )
    }
    
    ## -------------------------------------------------------
    ## Time-averaged power plots
    ## Avg power
    subset_t_avg <- subset(plot_data, x_var=='Avg Power' & y_var == 'Period')
    if(nrow(subset_t_avg) > 0) {
        gg <-
            gg +
            
            geom_path(data=subset_t_avg,
                      aes(x=x, y=y, linetype=`Avg over`))
        ## Event power
        ##geom_path(data=subset(plot_data, x_var=='Event Avg Power'),
        ##          aes(x=x, y=y)) + #, size=event_count)) +
        ##geom_point(data=subset(data, x_var=='Event Avg Power'),
        ##          aes(x=x, y=y, shape=as.factor(event_count))) +
    }


    ## -------------------------------------------------------
    ## Plot layout/style
    gg <-
        gg +
        
        facet_grid(y_var ~ x_var, scales='free', space='free', switch='y') +
        
        scale_y_continuous(expand = c(0, 0), position = "right") + #, labels=wt$period) +
        
        scale_x_continuous(expand = c(0, 0)) +
        
        theme_bw(base_size=13) +
        theme(
            legend.position="left",
            ##legend.key.height=unit(3, "line"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.spacing = unit(.25, "lines"),
            strip.background = element_rect(fill = 'grey90')
        )
    

    if(
        nrow(subset_power) > 0 &
        nrow(subset_input) > 0 &
        nrow(subset_t_avg) > 0
    ) {
       
        ## https://stackoverflow.com/questions/49521848/remove-unused-facet-combinations-in-2-way-facet-grid
        suppressMessages(library(grid))
        suppressMessages(library(gtable))

        grob <- ggplotGrob(gg)
        idx <- which(grob$layout$name %in% c("panel-2-2"))
        for (i in idx) grob$grobs[[i]] <- nullGrob()
        
        idx <- which(grob$layout$name %in% c("axis-b-2"))
        grob$layout[idx, c("t", "b")] <- grob$layout[idx, c("t", "b")] - c(2, 1)
        
        ## Move y axes right
        ## axis-l-2 needs to move 2 columns to the right
        ## axis-l-3 needs ot move 4 columns to the right
        idx <- which(grob$layout$name %in% c("axis-r-2"))
        grob$layout[idx, c("l", "r")] <- grob$layout[idx, c("l", "r")] - c(1.1, 1.1)
        
        grid.newpage()
        grid.draw(grob)

        return(invisible(grob))
        
    } else {

        print(gg)
        return(invisible(gg))

    }    
    
}

