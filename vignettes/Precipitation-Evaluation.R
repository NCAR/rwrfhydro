## ---- results='hide'-----------------------------------------------------
devtools::install_github('arezoorn/rwrfhydro', ref='devBranch')
library(rwrfhydro)

## ------------------------------------------------------------------------
countryCodeList=c("US")
networkCodeList=c("1","C")
sg <- SelectGhcnGauges(countryCode=countryCodeList,
                                 networkCode=networkCodeList)
str(sg)

## ------------------------------------------------------------------------
sg <- sg[seq(1,nrow(sg),100),]

## ---- eval = FALSE-------------------------------------------------------
## startDate="2015/06/01"
## endDate="2015/09/01"
## element="PRCP"
## obsPrcp<-GetGhcn(sg$siteIds, element, startDate, endDate, parallel=FALSE)

## ---- results='hide'-----------------------------------------------------
startDate="2015/05/01"
endDate="2015/10/01"
element="PRCP"
obsPrcp<-GetGhcn2(sg$siteIds, element, startDate, endDate, parallel=FALSE)
head(obsPrcp)

## ------------------------------------------------------------------------
dirPath<-"/glade/scratch/arezoo/QPF_verification_rwrfhydro/"
pathMRMS <- paste0(dirPath,"mrms/")
files<-list.files(path=pathMRMS,full.names = TRUE, pattern = "PRECIP_FORCING")

## ------------------------------------------------------------------------
geoGridAdd<-"/glade/scratch/arezoo/QPF_verification_rwrfhydro/geo_em.d01.nc.conus_3km_nlcd11"
rainGgaugeInds <- GetGeogridIndex(data.frame(lon=sg$longitude, lat=sg$latitude),
                                    geoGridAdd)
sg <- cbind(sg,rainGgaugeInds)
head(sg)

## ------------------------------------------------------------------------
flList <- list(lsm=files)
varList <- list(lsm=list(PRCP='precip_rate'))
prcpIndex <- list()
for (i in 1:length(sg$siteIds)) {
    if (!is.na(sg$ew[i]) & !is.na(sg$sn[i])) {
      prcpIndex[[as.character(sg$siteIds[i])]] <- list(start=c(sg$ew[i], sg$sn[i]),
                                                       end=c(sg$ew[i], sg$sn[i]), stat="mean")
   }
}
indList<-list(lsm= list(PRCP = prcpIndex))
prcpData <- GetMultiNcdf(file=flList,var=varList, ind=indList, parallel=FALSE)
head(prcpData)

## ------------------------------------------------------------------------
prcpData$POSIXct<-strptime(regmatches(basename(prcpData$POSIXct), regexpr("[0-9].*[0-9]", basename(prcpData$POSIXct))),format = "%Y%m%d%H%M", tz ="UTC")
#just keep value and time to make the size small
prcpData <- prcpData[,which(names(prcpData) %in% c('POSIXct','value','statArg'))]
prcpData$value<-prcpData$value*3600

## ------------------------------------------------------------------------
sg$reportTime<-obsPrcp$reportTime[match(sg$siteIds,obsPrcp$siteIds)]
        sg$reportTime[which (sg$reportTime=="")]<-700

## ------------------------------------------------------------------------
colnames(prcpData)[3]<- "DEL_ACCPRCP"
dailyData<-CalcDailyGhcn(sg,prcpData,parallel=FALSE)
head(dailyData)

## ------------------------------------------------------------------------
common<-merge(dailyData,obsPrcp,
              by.x=c("ghcnDay","statArg"),
              by.y=c("date","siteIds"))
head(common)

## ------------------------------------------------------------------------
stat<-data.frame()              
for (siteIds in unique(common$statArg)){
    df<-subset(common,common$statArg==siteIds)
    stat<-rbind(stat,cbind(siteIds,CalcMetCont(df$value,df$dailyPrcp)))
}
print(stat)

## ------------------------------------------------------------------------
# add rfc name
sg <- GetRfc(sg)

# check what is been added
head(sg)

## ------------------------------------------------------------------------
stat<-data.frame()              
for (rfcs in unique(sg$rfc)){
  if (!is.na(rfcs)) {
    sitesInRfc <- subset(sg,sg$rfc == rfcs)$siteIds
    df<-subset(common,common$statArg %in% sitesInRfc)
    stat<-rbind(stat,cbind(rfcs,CalcMetCont(df$value,df$dailyPrcp)))
  }
}

## ------------------------------------------------------------------------
stat

## ------------------------------------------------------------------------
# add HUC6 and HUC8 ids
sg<-GetPoly(sg,  polygonAddress= "/glade/scratch/arezoo/QPF_verification_rwrfhydro/gis/", polygonShapeFile= "huc6", join="HUC6")
sg<-GetPoly(sg,  polygonAddress= "/glade/scratch/arezoo/QPF_verification_rwrfhydro/gis/", polygonShapeFile= "huc8", join="HUC8")

# check what is been added
head(sg)

## ------------------------------------------------------------------------
devtools::install_github('arezoorn/rwrfhydro', ref='devBranch')
library(rwrfhydro)

## ------------------------------------------------------------------------
dirPath<-"/glade/scratch/arezoo/QPF_verification_rwrfhydro/"

## ------------------------------------------------------------------------
getPrcp<-function(file,var="precip"){
  nc<-ncdf::open.ncdf(as.character(file))
  precip_rate<- ncdf::get.var.ncdf(nc,var)
  ncdf::close.ncdf(nc)
  return(precip_rate[800:1000,800:1000])
}

## ------------------------------------------------------------------------
mgetPrcp<-function(file,var="precip_rate"){
  nc<-ncdf::open.ncdf(as.character(file))
  precip_rate<- ncdf::get.var.ncdf(nc,var)
  ncdf::close.ncdf(nc)
  # we need to convert rain rate (mm/s) to rain depth (mm) to be comparable with stageIV data
  return(precip_rate[800:1000,800:1000]*3600)
}

## ------------------------------------------------------------------------
pathMRMS <- paste0(dirPath,"mrms/")
pathStageIV <- paste0(dirPath,"stageIV/")

## ------------------------------------------------------------------------
MRMS <- list.files(path = pathMRMS,full.names = TRUE, pattern = glob2rx("*PRECIP_FORCING.nc"))

## ------------------------------------------------------------------------
run_dates <- as.POSIXct(substr(MRMS,nchar(MRMS)-29,nchar(MRMS)-20),format = "%Y%m%d%H",tz = "UTC")

## ------------------------------------------------------------------------
stageIV <- paste0(pathStageIV,format(run_dates,"%Y%m%d%H"),"00.PRECIP_FORCING.nc")

## ------------------------------------------------------------------------
ncores <- 16
library(doMC)
registerDoMC(ncores)

MRMSdepth <- plyr::laply(as.list(MRMS),mgetPrcp,var="precip_rate", .parallel=TRUE)
stageIVdepth <- plyr::laply(as.list(stageIV),getPrcp,var="precip",.parallel=TRUE)

## ------------------------------------------------------------------------
stat <- CalcMetContGrid(obs = stageIVdepth, mod = MRMSdepth)

## ------------------------------------------------------------------------
stat <- CalcMetContGrid(stageIVdepth,MRMSdepth,
                      statList = list('numPaired','minObs','minMod','maxObs','maxMod','meanObs','meanMod','stdObs','stdMod',
                                      'multiBias','ME','MSE','RMSE','MAE','pearsonCor','MAD','spearmanCor','kendallCor'))

## ------------------------------------------------------------------------
stat <- CalcMetContGrid(obs = stageIVdepth, mod = MRMSdepth, conRange = c(3,Inf))

## ------------------------------------------------------------------------
str(stat)

## ------------------------------------------------------------------------
stat<-CalcMetContGrid(obs = stageIV, mod = MRMS, .funObs = getPrcp, .funMod = mgetPrcp,
                    statList =  list('numPaired','meanObs','meanMod','stdObs','stdMod',
                                    'multiBias','RMSE','MAE','pearsonCor'), ncors = 10)

