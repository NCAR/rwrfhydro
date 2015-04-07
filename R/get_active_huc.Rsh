#!/usr/bin/Rscript
args <- commandArgs(TRUE)
library(rwrfhydro)
huc <- formatC(as.numeric(args[1]), width=2, flag='0')
print(huc)
if(!(huc %in% formatC(1:21, width=2, flag='0'))) return(NULL)
pullTime<- print(system.time(data<-GetActiveHucData(huc)))
obSize <- object.size(data)
save(data, pullTime, obSize, file=paste0('~/huc',huc,'.RData'))