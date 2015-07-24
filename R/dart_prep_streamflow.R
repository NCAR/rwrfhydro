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

#' Three-sigma errors specified as percent of observed plus some quantile of
#' historical flows.
#' 
#' \code{Model3SdErrPctErrPlusQntlIncpt} models three sigma (standard deviation)
#' errors (same units as input) as percent of observed plus some quantile of
#' historical record.
#' @param data, Numeric the values for which errors are to be modeled.
#' @param qntlIncpt Numeric the quantile of historical observations to be used
#'   as minimum error.
#' @param pctErr Numeric the percent error associated with the observations.
#' @export
Model3SdErrPctErrPlusQntlIncpt <- function(data, qntlIncpt=.005, pctErr=.1) {
  # may consider giving warning about length of timeseries? but how to do without time information.
  quantile(data, qntlIncpt) + (pctErr * pretty$Q.cms)
}

#=============================================================================================

#' Three-sigma error specification assuming errors are smaller near
#' climatological median (or other quantile).
#' 
#' \code{Model3SdErrClimTaper} models three-sigma errors (same units as input)
#' as smallest (\code{qntlIncpt} intercept is some climatological quantile) at 
#' "climatological" observations (\code{qntlClim}, e.g. median = .5) and grow to
#' some maximum percent error (\code{pctErr}). This is expressed by 
#' \code{quantile(data, qntlIncpt) + pmin( pctErr*data,
#' pctErr*abs(data-quantile(data, qntlClim)) )}
#' @param data Numeric The values for which errors are to be modeled.
#' @param qntlIncpt Numeric The quantile of historical observations to be used
#'   as minimum error or intercept.
#' @param qntlClim Numeric The quantile of historical observations to be used as
#'   observation value of minimum error.
#' @param pctErr Numeric The percent error associated with the observations.
#' @export
Model3SdErrClimTaper <- function(data, qntlIncpt=.05, qntlClim=.5, pctErr=.15) {  
  # may consider giving warning about length of timeseries? but how to do without time information.
  quantile(data, qntlIncpt) + pmin( pctErr*data, pctErr*abs(data-quantile(data, qntlClim)) )
}


#=============================================================================================

#' Make variances for prettyUsgs discharge observations.
#' 
#' \code{MkDischargeVariance} makes variances fr prettyUsgs discharge
#' observations. The formulation of the variances is subjective. Assuming
#' zero-mean Gaussian observation errors, the approach here is to supply a
#' function which estimates the 3-sigma (inner 99.5% error quantiles) around the
#' observations. This amount seems somewhat easier to conceptualize than
#' 1-sigma, hence here we are. This function divides the error amounts by 3 and
#' either returns the standard deviation or squares the result to return the
#' variance (default).
#' @param prettyUsgs The prettyUsgs discharge observations to which variances
#'   are to be added.
#' @param error3SdFunc Function which accepts the data and returns 3-sigma error
#'   estimates.
#' @param retVariance Logical Returns variance if TRUE, else returns 1-sigma
#'   error.
#' @examples
#' dbPath <- '~/usgsDb/'
#' prettyOrodell <- 
#'  PrettySiteData(QuerySiteData(QuerySiteName("FOURMILE CREEK AT ORODELL, CO", path=dbPath), 
#'                               product='00060', path=dbPath),metricOnly=FALSE, metric=TRUE)
#' prettyOro <- MkDischargeVariance(prettyOrodell, Model3SdErrClimTaper)
#' prettyO <- subset(prettyOro, dateTime < as.POSIXct('2012-01-01'))
#' oroPlot <- PlotPrettyData(prettyO)
#' @keywords manip
#' @concept DART dataMgmt
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
  
  ## only return one, NOT both
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



#=============================================================================================
#dataHourly <- plyr::ddply(pretty, .(year, month, day, hour),
                    #summarize,
                    ##Q.cms=mean(`discharge (cfs)`*cfs2cms, na.rm=TRUE) )
#                    Q.cms=`discharge (cfs)`[which.max(minute)]*cfs2cms )

#dataHourly$POSIXct <-
  #as.POSIXct(with(dataHourly, paste0(year,'/',month,'/',day,'_',hour)),format='%Y/%m/%d_%H')



#' Writes an ascii obs_seq.in file for DART binary create_obs_sequence.
#' 
#' \code{WriteDischargeObsSeq} makes variances for prettyUsgs discharge
#' observations. The formulation of the variances is subjective. Assuming
#' zero-mean Gaussian observation errors, the approach here is to supply a
#' function which estimates the 3-sigma (inner 99.5% error quantiles) around the
#' observations. This amount seems somewhat easier to conceptualize than
#' 1-sigma, hence here we are. This function divides the error amounts by 3 and
#' either returns the standard deviation or squares the result to return the
#' variance (default).
#' @param pretty The prettyUsgs discharge observations to which variances are to
#'   be added.
#' @param outPath     Character The directory where the file is to be written.
#' @param stationName Character To help identify the outputfile and the station
#'   data in the file.
#' @param errorId     Character To help identify the outputfile and the error
#'   function used.
#' @param typeQ       Numeric   Stream discharge obs type index in my
#'   pre-release version of DART. It could change. 
#'   DART/obs_kind/DEFAULT_obs_kind_mod.F90
#' @examples
#' #Following on examples for MkDischargeVariance
#' WriteDischargeObsSeq(prettyOro, '~/.', 'orodell', 'climTaperDefault')
#' mkGaugeObsSeq( loganData, '~/boulderCreek/', 'loganMill' )
#' mkGaugeObsSeq( sunshineData, '~/boulderCreek/', 'sunshine' ) 
#' @keywords manip
#' @concept DART dataMgmt
#' @family dartObs
#' @export
WriteDischargeObsSeq <- function(pretty, outPath='.', 
                                 stationName, errorId, 
                                 typeQ=20) {
  
  if(!('prettyUsgs' %in% class(pretty))) {
    warning('WriteDischargeObsSeq requires a prettyUsgs object as its first argument.')
    return(NULL)
  }
  
  variables <- attr(pretty,'variables')
  codes     <- attr(pretty,'codes')
  variances <- attr(pretty,'variances')
  stDevs    <- attr(pretty,'st.devs.')
  errVars   <- c(variances,stDevs)
  
  if(!length(errVars)) {
    warning(paste0('WriteDischargeObsSeq requires variances or std.devs. to be specified in ',
                   'the prettyUsgs object. See MkDischargeVariance.'))
    return(NULL)
  }
  
  siteInfo  <- attr(pretty,'siteInfo')
  
  ## If there is more than one variable, which to use?
  ## nly output a single a single variable+variance. 
  if(length(variables)>1 && length(errVars)) {
    errUnits <- plyr::laply(strsplit(errVars,'[(^)]'), '[[', 2)
    if(length(errVars)==1) { 
      ## this selects from more than one variable using the only available err info.
      theVar <- variables[grep(errUnits,variables)]
      theErr <- errVars
    } else { 
      ## otherwise, the variable is user selected.
      varsWErr <- variables[plyr::laply(errUnits, grep, variables)]
      whVar <- readline(prompt=paste0('Please select a single variable to plot with error bars: \n',
                                      paste(1:length(varsWErr),varsWErr, sep=': ', collapse=' \n'),' \n'))
      theVar <- variables[as.numeric(whVar)]
      theErr <- errVars[grep( strsplit(theVar,'[(^)]')[[1]][2], errUnits )]
    }
    pretty <- pretty[,c("dateTime","site_no", codes, theVar, theErr)]
    variables <- theVar
    errVars   <- theErr
  }
  
  ## remove missing observations
  for (var in variables) {
    whNa <- which(is.na(pretty[[var]]))
    if(length(whNa)) pretty <- pretty[-whNa,]
  }
  
  ## time in year month day hour minute second
  pretty$year   <-  format(pretty$dateTime, '%Y')
  pretty$month  <-  format(pretty$dateTime, '%m')
  pretty$day    <-  format(pretty$dateTime, '%d')
  pretty$hour   <-  format(pretty$dateTime, '%H')
  pretty$minute <-  format(pretty$dateTime, '%M')
  pretty$second <-  format(pretty$dateTime, '%S')
  
  outFileName <- paste0(outPath,'/',stationName,'.Q.cms.MASTER.',errorId,'.obs_seq.inputForCreateObsSeq')

  ## this is the file listed at the end of the file we will write, it is the name of the
  ## file to be made by create_obs_seq
  outSeqFileName <- paste0(stationName,'.Q.cms.MASTER.',errorId,'.obs_seq.out')
  
  ## open a file for writing
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
  
  cat(as.character(nrow(pretty)+1), file = outCon, sep = "\n")

  nCopies <- 1
  cat(as.character(nCopies), file = outCon, sep = "\n")
  
  nQuality <- 0
  cat(as.character(nQuality), file = outCon, sep = "\n")
  
  cat('"The observations"', file = outCon, sep = "\n")
  
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
  
  theEle <- as.character(siteInfo$alt_va[1]*feet2meters)
  theLon <- siteInfo$dec_long_va
  theLon <- format(theLon %% 360, digits=20)
  theLat <- format(siteInfo$dec_lat_va, digits=20)
  
  for (i in 1:nrow(pretty) ) {
    cat(as.character(i),
        as.character(typeQ),
        as.character(-1),
        theEle, theLon, theLat,
        paste(as.character(pretty$year[i]),
              as.character(pretty$month[i]),
              as.character(pretty$day[i]),
              as.character(pretty$hour[i]),
              as.character(pretty$minute[i]), 
              as.character(pretty$second[i]),
              sep=' '),
        as.character(pretty$error)[i], 
        as.character(pretty$Q.cms[i]),
        file=outCon, sep='\n')
  }
  
  cat(as.character(-1), file=outCon, sep='\n')
  cat(outSeqFileName, file=outCon, sep='\n')
  
  close(outCon)
  outFileName
}

