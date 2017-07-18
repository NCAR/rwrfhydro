#!/usr/bin/Rscript
## the  above path often needs customization.

## Scripts arguments are
## 1: modelFile
## 2: obsFile

## When we want to start calling more functions, we could make this a more general
## interface where we supply file names and then the name of the function and 
## followed by its arguments. 

## Avoid doing this for now.
#library(rwrfhydro)

args <- commandArgs(TRUE)
modelFile <- args[1]
obsFile   <- args[2]

## assuming the two files have the same format
## make a single "metafunction" and tweak once
readWebCsv <- function(file) {
  data <- read.csv(file, header=TRUE, skip=2,
                   stringsAsFactors=FALSE)
  data$POSIXct <- as.POSIXct(data$time, tz='UTC')
  data$time <- NULL
  names(data)[1] <- c("q_cms")
  data
}

modelData <- readWebCsv(modelFile)
obsData   <- readWebCsv(obsFile)

stats <- rwrfhydro::CalcModPerf(modelData, obsData)

## it occurs to me that it might be germane to somehow print 
## the units in or near the table.
## there are issues with using stdout from R on some platforms, 
## this may need help
write.csv(stats, file=stdout())
