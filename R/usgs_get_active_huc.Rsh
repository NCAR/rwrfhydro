#!/opt/R/bin/Rscript
## arguments are
## 1: huc2 code: character, a huc two in 01:21
## 2: outPath: character, the directory where the output is to be placed.
## 3: dateId: character, a date time identifier
## 4; process:=TRUE OPTIONAL - do the processing or not?
options(warn=1)  ## we do not want fatals
suppressPackageStartupMessages(library(rwrfhydro))

####################
## Error handling functions
thisScript <- 'usgs_get_active_huc.Rsh'
errMsg <- function(message, fatal=FALSE){
  errType <- if(fatal) 'FATAL ERROR' else 'WARNING'
  sink(stderr(), type = "message")
  warning(paste0(errType, ': In ', thisScript, '\n',
                 message,'\n',
                 'END ', errType))
  sink(NULL, type = "message")
}

fatalGraceful <- function(message) {
  errMsg(message, fatal=TRUE)
  quit(save='no', status=1, runLast=FALSE)
}

####################
## Argument handling

args <- commandArgs(TRUE)

## arg 1: huc
huc <- formatC(as.numeric(args[1]), width=2, flag='0')
if(!(huc %in% formatC(1:21, width=2, flag='0'))) fatalGraceful(paste0("No such HUC two: ",huc))

## arg 2: outPath
outPath <- args[2]
if(!file.exists(outPath)) fatalGraceful(paste0("No such directory: ",outPath))
print(outPath)

## arg 3
dateId <- args[3]
if(is.na(dateId)) fatalGraceful("No dateId argument supplied.")

## arg 4
process <- args[4]
if(toupper(process)=='TRUE')   process <- TRUE
if(toupper(process)=='FALSE') process <- FALSE
if(!is.logical(process)) fatalGraceful("Supplied argument 'process' is not logical.")


####################
## Setup

## organize by time, then by HUC
outPrefix <- paste0(outPath,'/',dateId,'.huc',huc)
outTmp <- paste0(outPrefix,'.tmp')
outFile <- paste0(outPrefix,'.RData')

## create a tmp file that indicates this task is still working
## This file MUST be cleaned up.
system(paste0('touch ',outTmp))

## diagnositc
print(outFile)
print(paste(huc, dateId, sep=': '))

## a function to try a function for a while (3 minutes?)
tryForMinutes <- function(func, list, minutes=4, quitOnFail=FALSE) {

  t0=Sys.time()
  result <- try(log("a"), silent=TRUE)
  while(class(result) == 'try-error' & 
        difftime(Sys.time(),t0, unit='sec') < minutes*60) {
    result <- try(do.call(func, list))
  }
  
  if(class(result) == 'try-error') {
    message <- paste0("For HUC ", huc, ", function '", match.call()[2],
                      "' returned only a try-error for ", minutes,
                      " minutes.")    
    if(quitOnFail) {
      ## bail on the entire script with appropriate 1 return value to bash
      ## delete the temp file before quitting to signal the process is over
      system(paste0('rm ',outTmp))
      fatalGraceful(message)
    } else {
      errMsg(message)
    }
  }
  
  result
}

####################
## Get and process the data!

## The NWIS Filter options
service='iv'
modifiedSince='PT15M'
period='PT4H'
parameterCd='00060'

## default diagnostics
pullTime <- parseTime <- NULL

## wrap the get/parse chain and it's indiviudal components.
doPullParse <- function() {

  ## The following 3 commands constitute readNWISdata.
  ## 1. Forming the url should never fail
  urlCall <- dataRetrieval::readNWISdataUrlCall(service=service,
                                                huc=huc, 
                                                modifiedSince=modifiedSince, 
                                                period=period,
                                                parameterCd=parameterCd)
  
  ## 2. Getting the data apparaently fails often.
  ## Pull time is a diagnostic.
  pullTime <<- system.time( 
    waterMLList <- tryForMinutes(dataRetrieval::readNWISdataImport, 
                                list(urlCall=urlCall, service=service),
                                minutes=.3) 
  )
  if(class(waterMLList)=='try-error') {
    errMsg(paste0("For HUC ", huc, ", failed to get data (readNWISdataImport)"))
    return(waterMLList)
  }

  ## 3. Parsing the data may fail too, but it's unclear.
  ## parseTime is diagnostic
  if(process) {
    parseTime <<- system.time(
      data <- tryForMinutes(dataRetrieval::readNWISdataRetval, 
                            list(importList=waterMLList, service=service), 
                            minutes=.3)
    )
  } else {
    data <- NA  ## if not processed
  }
  
  if(class(data)=='try-error'){
    waterMLListOut <- paste0(outPath,dateId,'.waterMLList.RData')
    errMsg(paste0("For HUC ", huc, ", failed to parse data (readNWISdataRetval). ",
                  "Saving problematic data to: ", waterMLListOut))
    # Save waterMLList to file to diagnose what the problem is.
    save(waterMLList, file=waterMLListOut)
  }
  
  data
}

## The number of minutes here should be less than allowed "killAfterMins" in the 
## script /home/jamesmcc/usgsStreamData/scripts/getAllActiveHucs_saudiLogin.sh
data <- tryForMinutes(doPullParse, list(), minutes=3, quitOnFail=TRUE )

## another diagnostic
obSize <- format(object.size(data), units='MB')

####################
## OUTPUT
if(process) save(data, pullTime, parseTime, obSize, file=outFile)

####################
## CLEANUP
system(paste0('rm ',outTmp))

#####################
## RETURN SUCCESS
quit(save='no', status=0, runLast=FALSE)
