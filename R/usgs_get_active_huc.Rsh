#!/opt/R/bin/Rscript
## arguments are
## 1: huc2 code: character, a huc two in 01:21
## 2: outPath: character, the directory where the output is to be placed.
## 3: dateId: character, a date time identifier
library(rwrfhydro)

args <- commandArgs(TRUE)

## arg 1: huc
huc <- formatC(as.numeric(args[1]), width=2, flag='0')
if(!(huc %in% formatC(1:21, width=2, flag='0'))) return(NULL)

## arg 2: outPath
outPath <- args[2]
print(outPath)
if(!file.exists(outPath)) return(NULL)

## arg 3
dateId <- args[3]

## organize by time, then by HUC
outPrefix <- paste0(outPath,'/',dateId,'.huc',huc)
outTmp <- paste0(outPrefix,'.tmp')
outFile <- paste0(outPrefix,'.RData')

## create a tmp file that indicates this task is still working
system(paste0('touch ',outTmp))

## diagnositc
print(outFile)
print(paste(huc, dateId, sep=': '))

## pullTime and obSize are also diagnostic
pullTime<- system.time( data<-GetActiveHucData(huc,parameterCd=c('00060')) )
obSize <- format(object.size(data), units='MB')

save(data, pullTime, obSize, file=outFile)

system(paste0('rm ',outTmp))
