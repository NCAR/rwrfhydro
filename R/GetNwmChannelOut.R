#' Extract streamflow data from channel route netcdf files
#' @details This function can only be used for NWM model output from versions 1.1-1.2. 
#' Given a character vector of paths to channel route netcdf files, \code{get_nwmChannel} will extract selected data and return a dataframe. 
#' @param files A character vector of file paths to CHRTOUT files.
#' @param features Optional, A numeric vector of features for subsetting channel data prior to storing in database.
#' @param variables Optional, character vector of variabel names to extract
#' @return data.frame
#' @examples
#' \dontrun{
#' TODO - 1333
#' }
#' @import data.table
#' @export
GetNwmChannelOut <- function(files,features = NULL,variables = c('streamflow','velocity','nudge','q_lateral')) {
  
  variables <- c("feature_id",setdiff(variables,'feature_id'))
  
    ###############
    #Get channel_rt files
    
    #netCDF data
    dfList <- lapply(files,
                     function(file,features,variables) {
                       
                       chrtOut <-  rwrfhydro::GetNcdfFile(file, 
                                                          variables=variables,
                                                          quiet = TRUE)
                       
                       #Get attributes for fileInfo table
                       chrtOutAtts <- attributes(chrtOut)
                       
                       keyVar <- 'feature_id'
                       chrtOut <- data.table::as.data.table(chrtOut)
                       data.table::setkeyv(chrtOut, keyVar)
                       
                       #Subset to features if supplied
                       if(!is.null(features)) {
                         chrtOut <- chrtOut[ feature_id %in% features,] 
                       }
                       
                       #Covnert back to data.frame to use consistant base R subsetting
                       chrtOut <- as.data.frame(chrtOut)
                       
                       
                       ################
                       ###Make file info table
                       ################
                       
                       #Get init time attribute
                       chrtOut$initTime <- chrtOutAtts$global$model_initialization_time
                       
                       #Get valid time attribute
                       chrtOut$validTime <- chrtOutAtts$global$model_output_valid_time
                       
                       #Get model version
                       chrtOut$nwmVersion <- ifelse(!is.null(chrtOutAtts$global$model_version),
                                                    chrtOutAtts$global$model_version,
                                                    'v1.1')
                       return(chrtOut)
                       
                     },
                     features=features,
                     variables=variables)
    
    dfOut <- as.data.frame(data.table::rbindlist(dfList))
    #Create model run info dataframe
    #fileInfo <- data.frame(fileName = file)
    
    #Format as time class
    timeStrFmt <- '%Y-%m-%d_%H:%M:%S'
    dfOut$initTime <- as.POSIXct(dfOut$initTime, format=timeStrFmt, tz='UTC')
    dfOut$validTime <- as.POSIXct(dfOut$validTime, format=timeStrFmt, tz='UTC')
    
    #Calculate lead time
    #dfOut$leadTime <- as.numeric(difftime(dfOut$validTime,dfOut$initTime,units='hours'))
    
    dfOut <- dfOut[c("feature_id",
               "initTime",
               "validTime",
               "nwmVersion",
               #"leadTime", 
               "streamflow",
               "velocity",
               "q_lateral")]
    return(dfOut)
}
    