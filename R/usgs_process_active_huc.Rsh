#!/opt/R/bin/Rscript
## arguments are
## 1: queryTime: character, the dateId denoting the query time.
## 2: inPath:    character, the path for the input files. 
## 3: outPath:   character, the directory where the output is to be placed.
## 4: nCores:    character, the of cores to use in processing

####################
## Error handling functions
thisScript <- 'usgs_process_active_huc.Rsh'
errMsg <- function(message, fatal=FALSE){
  errType <- if(fatal) 'FATAL ERROR' else 'WARNING'
  sink(stderr(), type = "message")
  warning(paste0(errType, ': In ', thisScript, '\n',
                 message,'\n',
                 'END ',errType))
  sink(NULL, type = "message")
}

fatalGraceful <- function(message) {
  errMsg(message, fatal=TRUE)
  quit(save='no', status=1, runLast=FALSE)
}

####################
## SETUP
suppressPackageStartupMessages(library(rwrfhydro))
options(warn=1)

## these are the "parameters"
oldestDateToProc <- as.POSIXct('2015-04-15 00:00:00', tz='UTC')
#nearest number of minutes to round to
nearest <- 15

####################
## argument handling
args <- commandArgs(TRUE)

## arg 1: huc
queryTime <- args[1]

## arg 2: inPath
inPath <- args[2]
if(!file.exists(inPath)) {
  fatalGraceful(paste0('inPath DNE: ',inPath))
}

## arg 3: outPath
outPath <- args[3]
if(!file.exists(outPath)) {
  fatalGraceful(paste0('outPath DNE: ',outPath))
}

## arg 4: outPath
nCores <- args[4]
if(is.null(nCores)) nCores <- 8  ## saudi is only 8 cores on login

####################
## Process.
queryFiles <- list.files(path=inPath, pattern=queryTime, 
                         full.name=TRUE)
if(!length(queryFiles)) fatalGraceful("There were no real time files to process!")

# requsite packages
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(doMC))
registerDoMC(nCores)

ret1 <- try(MkUsgsTimeSlice( queryFiles, outPath=outPath, 
                             nearest=nearest, oldest=oldestDateToProc ))

if(class(ret1)=='try-error') fatalGraceful("Failed to make timeslice!")

## a message for standard out diagnostics
print(ret1)

## return success
quit(save='no', status=0, runLast=FALSE)