# Put USGS observations into a DART obs_seq.in file which can be piped into
# create_obs_sequence. 
# http://www.image.ucar.edu/DAReS/DART/DART_Observations.php#obs_seq_overview
# they show an example of doing this interactively, but can also cat a file to
# create_obs_sequence.

## error model
## review/plot
## write to obs_seq_out

## create_obs_sequence wants tuples of: (??)
## [[[ type, location, time, expected error, and optionally a data value and/or a quality control indicator]]]


#=============================================================================================
#' Three-sigma errors specified as percent of observed plus some quantile of historical flows.
#' 
#' \code{Model3SdErrPctErrPlusQntlIncpt} models three sigma (standard deviation) errors (same 
#' units as input) as percent of observed plus some quantile of historical record. 
#' @param data, Numeric the values for which errors are to be modeled.
#' @param qntlIncpt Numeric the quantile of historical observations to be used as minimum error.
#' @param pctErr Numeric the percent error associated with the observations.
#' @export
Model3SdErrPctErrPlusQntlIncpt <- function(data, qntlIncpt=.005, pctErr=.1) {
  # may consider giving warning about length of timeseries? but how to do without time information.
  quantile(data, qntlIncpt) + (pctErr * data$Q.cms)
}

#=============================================================================================
#' Three-sigma error specification assuming errors are smaller near climatological median (or other 
#' quantile).
#' 
#' \code{Model3SdErrClimTaper} models three-sigma errors (same units as input) as 
#' smallest (\code{qntlIncpt} intercept is some climatological quantile) at 
#' "climatological" observations (\code{qntlClim}, e.g. median = .5) and grow to some maximum 
#' percent error (\code{pctErr}). This is expressed by
#' \code{quantile(data, qntlIncpt) + pmin( pctErr*data, pctErr*abs(data-quantile(data, qntlClim)) )}
#' @param data Numeric The values for which errors are to be modeled.
#' @param qntlIncpt Numeric The quantile of historical observations to be used as minimum error or intercept.
#' @param qntlClim Numeric The quantile of historical observations to be used as observation value of minimum error.
#' @param pctErr Numeric The percent error associated with the observations.
#' @export
Model3SdErrClimTaper <- function(data, qntlIncpt=.05, qntlClim=.5, pctErr=.15) {  
  # may consider giving warning about length of timeseries? but how to do without time information.
  quantile(data, qntlIncpt) + pmin( pctErr*data, pctErr*abs(data-quantile(data, qntlClim)) )
}


#=============================================================================================
#' Make variances for prettyUsgs discharge observations. 
#' 
#' \code{MkDischargeVariance} makes variances fr prettyUsgs discharge observations. The formulation 
#' of the variances is subjective. Assuming zero-mean Gaussian observation errors, the approach 
#' here is to supply a function which estimates the 3-sigma (inner 99.5% error quantiles) around 
#' the observations. This amount seems somewhat easier to conceptualize than 1-sigma, hence here
#' we are. This function divides the error amounts by 3 and either returns the standard deviation
#' or squares the result to return the variance (default).
#' @param prettyUsgs The prettyUsgs discharge observations to which variances are to be added. 
#' @param error3SdFunc Function which accepts the data and returns 3-sigma error estimates.
#' @param retVariance Logical Returns variance if TRUE, else returns 1-sigma error.
#' @examples
#' dbPath <- '~/usgsDb/'
#' prettyOrodell <- 
#'  PrettySiteData(QuerySiteData(QuerySiteName("FOURMILE CREEK AT ORODELL, CO", path=dbPath), 
#'                               product='00060', path=dbPath))
#' prettyOro <- MkDischargeVariance(prettyOrodell, Model3SdErrClimTaper)
#' @export
MkDischargeVariance <- function(prettyUsgs, error3SdFunc, retVariance=TRUE) {
  if(!('prettyUsgs' %in% class(prettyUsgs))) {
    warning("MkDischargeVariance needs a 'prettyUsgs' object as its first argument. Returning.")
    return(NULL)
  }
  if(!('Discharge code' %in% names(prettyUsgs))) {
    warning("MkDischargeVariance only applies to discharge data. Returning.")
    return(NULL)
  }
  
  if(retVariance) {
    errExp <- 2; errStr <- 'variance'; errAtt <- 'variances'
  } else {
    errExp <- 1; errStr <- 'st.dev.' ; errAtt <- 'st.devs.'
  }
  
  ## Is there more than one variable?
  variables <- attr(prettyUsgs, 'variables')
  varSplit <- strsplit(variables,'[] ()]')
  errNames  <- plyr::laply(varSplit, function(ch) paste0(ch[1],' ',errStr,' (',ch[3],'^',errExp,')'))
  names(errNames) <- names(variables) <- variables
  calcErr <- function(var) (error3SdFunc(var)/3)^(errExp)
  errs <- plyr::llply(variables, function(var) calcErr(prettyUsgs[[var]]))
  for (var in variables) {
    prettyUsgs[[errNames[var]]] <- errs[[var]]
  }
  names(errNames) <- NULL
  attr(prettyUsgs, errAtt) <- errNames
  prettyUsgs
}


#dataHourly$POSIXct <-
  #as.POSIXct(with(dataHourly, paste0(year,'/',month,'/',day,'_',hour)),format='%Y/%m/%d_%H')



MkDischargeObsSeq <- function(data, outPath, stationName ) {

  ## Discharge obs type currently in pre-release DART
  ## DART/obs_kind/DEFAULT_obs_kind_mod.F90
  typeQ=290
  
  ## remove missing observations
  whNa <- which(is.na(data$`discharge (cfs)`))
  if(length(whNa)) data <- data[-whNa,]
  
  ## time in year month day hour minute second
  data$year   <-  format(data$POSIXct, '%Y')
  data$month  <-  format(data$POSIXct, '%m')
  data$day    <-  format(data$POSIXct, '%d')
  data$hour   <-  format(data$POSIXct, '%H')
  data$minute <-  format(data$POSIXct, '%M')
  data$second <-  format(data$POSIXct, '%S')
  
  cfs2cms <- 0.0283168466  ## cms/cfs
  
  
  ## put the data onto hourly timestep
  dataHourly <- ddply(data, .(year, month, day, hour),
                      summarize,
                      ##Q.cms=mean(`discharge (cfs)`*cfs2cms, na.rm=TRUE) )
                      Q.cms=`discharge (cfs)`[which.max(minute)]*cfs2cms )
  ## lost lat, lon, ele but they are scalar anyway.
  
  ## error model
  ## what are the units of the expected error? = VARIANCE
  ## Heteroskedastic, e.g. Clark et al 2008, Noh et al 2011
  ## Specify the error as the 99.5% or the 3rd standard deviation
  
  
  #   ggplot( subset(dataHourly, POSIXct>=as.POSIXct('2013-05-01') & POSIXct<=as.POSIXct('2013-06-01')),
  #         aes(x=POSIXct, y=Q.cms, ymax=Q.cms+2*sqrt(error), ymin=Q.cms-2*sqrt(error))) +
  #           geom_errorbar(color='red') +
  #             geom_point(size=.85)
  
  require(scales)
  print(
    ggplot( dataHourly, aes(x=POSIXct, y=Q.cms, ymax=Q.cms+2*sqrt(error), ymin=Q.cms-2*sqrt(error))) +
      geom_errorbar(color='red') +
      geom_point(size=.85) + scale_x_datetime(breaks = date_breaks("1 month"), labels = date_format("%m\n%Y") )  
  )
  ## ###################################################################
  ## open a file for writing
  outFileName <- paste0(outPath,'/',stationName,'.Q.cms.MASTER.',fileSeqId,'.obs_seq.inputForCreateObsSeq')
  outSeqFileName <- paste0(stationName,'.Q.cms.MASTER.',fileSeqId,'.obs_seq.out')
  outCon <- file(outFileName, "w")  # open an output file connection
  
  ## create_obs_seqence meta info
  ## Input upper bound on number of observations in sequence
  ##2
  ## Input number of copies of data (0 for just a definition)
  ##1
  ## Input number of quality control values per field (0 or greater)
  ##1
  ## input meta data for data copy             1
  ##the obs
  ## input meta data for qc field             1
  ##missing
  
  cat(as.character(nrow(dataHourly)+1),file = outCon, sep = "\n")
  nCopies <- 1
  cat(as.character(nCopies),file = outCon, sep = "\n")
  nQuality <- 0
  cat(as.character(nQuality),file = outCon, sep = "\n")
  cat('"The observations"',file = outCon, sep = "\n")
  
  ##1
  ##      Input -1 * state variable index for identity observations
  ##      OR input the name of the observation kind from table below:
  ##      OR input the integer index, BUT see documentation...
  ##                  3 SOIL_MOISTURE
  ##                 20 STREAM_FLOW
  ##20
  ## location_mod: Ignoring vertical when computing distances; horizontal only
  ## location_mod: Using table-lookup approximation for distance computations
  ## Vertical coordinate options
  ##           -2  --> vertical coordinate undefined
  ##           -1  --> surface
  ##            1  --> model level
  ##            2  --> pressure
  ##            3  --> height
  ##            4  --> scale height
  ##-1
  ## Vertical coordinate height
  ##400
  ## Input longitude: value 0 to 360.0 or a negative number for 
  ## Uniformly distributed random location in the horizontal
  ##150
  ## Input latitude: value -90.0 to 90.0
  ##45
  ## input date (as integers): year month day hour minute second
  ##1999 2 2 2 2 2
  ## Input error variance for this observation definition 
  ##.4
  ## Enter value             1 for this observation
  ##40
  ## Enter quality control value             1 for this observation
  ##1
  ## input a -1 if there are no more obs
  ##1
  ##      Input -1 * state variable index for identity observations
  ##      OR input the name of the observation kind from table below:
  ##      OR input the integer index, BUT see documentation...
  ##                  3 SOIL_MOISTURE
  ##                 20 STREAM_FLOW
  ##2 [[[repeat]]]
  
  theEle <- as.character(data$ele.ft[1]*.3048)
  theLon <- format(data$lon[1] %% 360, digits=20)
  theLat <- format(data$lat[1], digits=20)
  
  for (i in 1:nrow(dataHourly) ) {
    cat(as.character(i),
        as.character(20),
        as.character(-1),
        theEle, theLon, theLat,
        paste(as.character(dataHourly$year[i]),
              as.character(dataHourly$month[i]),
              as.character(dataHourly$day[i]),
              as.character(dataHourly$hour[i]),
              as.character(0), as.character(0),
              sep=' '),
        as.character(dataHourly$error)[i], 
        as.character(dataHourly$Q.cms[i]),
        file=outCon, sep='\n')
  }
  
  cat(as.character(-1), file=outCon, sep='\n')
  cat(outSeqFileName, file=outCon, sep='\n')
  
  close(outCon)
}

#mkGaugeObsSeq( loganData, '~/boulderCreek/', 'loganMill' )
#mkGaugeObsSeq( sunshineData, '~/boulderCreek/', 'sunshine' ) 
2