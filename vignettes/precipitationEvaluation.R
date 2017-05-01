## ----results='hide', message=FALSE, warning=FALSE------------------------
library(rwrfhydro)

## ----eval = FALSE--------------------------------------------------------
## #setInternet2(use=FALSE) # If using windows, you may need this.
## 
## # Return all the gauges within US from observation network of COOP (C) and CoCoRaHS (1)
## countryCodeList <- c("US")
## networkCodeList <- c("1","C")
## sg <- SelectGhcnGauges(countryCode=countryCodeList,
##                        networkCode=networkCodeList)
## str(sg)

## ------------------------------------------------------------------------
sg <- SelectGhcnGauges(domain = TRUE, minLat = 40.0125, maxLat = 40.0682, 
                       minLon = -105.562, maxLon=-105.323)
str(sg)

## ----message=FALSE, warning=FALSE----------------------------------------
startDate <- "2013/01/01"
endDate <- "2013/09/30"
element <- "PRCP"
obsPrcp <- GetGhcn(sg$siteIds, element, startDate, endDate, parallel = FALSE)
str(obsPrcp)

## ------------------------------------------------------------------------
fcPath <- '~/wrfHydroTestCases/Fourmile_Creek_testcase_v2.0'

## ------------------------------------------------------------------------
forcingPath <- paste0(fcPath,"/FORCING")
files <- list.files(path = forcingPath, full.names = TRUE, pattern = glob2rx("201304*LDASIN_DOMAIN1"))

## ----message=FALSE, warning=FALSE----------------------------------------
geoFile <- paste0(fcPath,'/DOMAIN/geo_em_d01.Fourmile1km.nlcd11.nc')
rainGgaugeInds <- GetGeogridIndex(xy = data.frame(lon=sg$longitude, lat=sg$latitude),
                                  ncfile = geoFile)
sg <- cbind(sg,rainGgaugeInds)
head(sg)

## ----message=FALSE, warning=FALSE----------------------------------------
flList <- list(forcing = files)
varList <- list(forcing = list(PRCP = 'RAINRATE'))
prcpIndex <- list()
for (i in 1:length(sg$siteIds)) {
  if (!is.na(sg$we[i]) & !is.na(sg$sn[i])) {
    prcpIndex[[as.character(sg$siteIds[i])]] <- list(start=c(sg$we[i], sg$sn[i],1),
                                                     end=c(sg$we[i], sg$sn[i],1), stat="mean")
  }
}
indList <-list(forcing = list(PRCP = prcpIndex))
prcpData <- GetMultiNcdf(file = flList, var = varList, ind = indList, parallel=FALSE)
head(prcpData)

## ------------------------------------------------------------------------
prcpData$value <- prcpData$value*3600

## ------------------------------------------------------------------------
if ("reportTime" %in% names(prcpData)) {
  sg$reportTime <- obsPrcp$reportTime[match(sg$siteIds, obsPrcp$siteIds)]
  sg$reportTime[which (sg$reportTime=="" | is.na(sg$reportTime))] <-700
}else{
  sg$reportTime<- 700
}

## ------------------------------------------------------------------------
names(prcpData)[names(prcpData) == 'value'] <- 'DEL_ACCPRCP'
dailyData <- CalcDailyGhcn(sg = sg,prcp = prcpData)
head(dailyData)

## ------------------------------------------------------------------------
#usind data.table merge
common <- data.table:::merge.data.table(data.table::as.data.table(dailyData),
                                        data.table::as.data.table(obsPrcp),
                                        by.x=c("ghcnDay","statArg"),
                                        by.y=c("Date","siteIds"))
head(common)

## ----plot1, fig.width = 8, fig.height = 8, out.width='600', out.height='600'----
stat <- CalcStatCont(DT = common, obsCol = "dailyGhcn", modCol = "dailyPrcp" , 
                     obsMissing = -999.9, groupBy = "statArg")

# CalcStatCont will return a list having two elements of stat and plotList.
names(stat)

#To check the statistics 
stat$stat

## ----plot2, fig.width = 8, fig.height = 8, out.width='600', out.height='600'----
common2 <- common[statArg == unique(statArg)[1]]
stat <- CalcStatCont(DT = common2, obsCol = "dailyGhcn", modCol = "dailyPrcp", obsMissing = -999.9, title = common2$statArg)

## ----plot3, fig.width = 8, fig.height = 8, out.width='600', out.height='600'----
stat <- CalcStatCont(DT = common2, obsCol = "dailyGhcn", modCol = "dailyPrcp" , obsMissing = -999.9, plot.list = "scatterPlot")

## ----plot4, fig.width = 8, fig.height = 8, out.width='600', out.height='600'----
stat <- CalcStatCont(DT = common2, obsCol = "dailyGhcn", modCol = "dailyPrcp" , 
                     obsCondRange = c(1, Inf), plot.list = "scatterPlot")

## ------------------------------------------------------------------------
# add rfc name
sg <- GetRfc(sg)

# check what is been added
head(sg)

## ------------------------------------------------------------------------
# merge the common data.table with the sg data.frame
common <- data.table:::merge.data.table(common,data.table::as.data.table(sg[, c("siteIds", "rfc")]),
                                        by.x=c("statArg"),
                                        by.y=c("siteIds"))

# calculate statistics using grouping by rfc
stat <- CalcStatCont(DT = common, obsCol = "dailyGhcn", modCol = "dailyPrcp" , 
                     groupBy = "rfc", obsMissing = -999.9, plot.it = FALSE)

stat$stat

## ----results="hide", message=FALSE, warning=FALSE------------------------
# add HUC12 ids
polygonAddress <- paste0(path.expand(fcPath), "/polygons")
sg <- GetPoly (sg,  polygonAddress = polygonAddress,
               polygonShapeFile = "clipped_huc12",
               join="HUC12")

# check what is been added
head(sg)

# merge the common data.table with the sg data.frame
common <- data.table:::merge.data.table(common,data.table::as.data.table(sg[, c("siteIds","HUC12")]),
                                        by.x=c("statArg"),
                                        by.y=c("siteIds"))

# calculate statistics using grouping by HUC12
stat <- CalcStatCont(DT = common, obsCol = "dailyGhcn", modCol = "dailyPrcp", 
                     obsMissing = -999.9, groupBy = "HUC12", plot.it = FALSE)
stat$stat

## ------------------------------------------------------------------------
# calculate categorical statistics
stat <- CalcStatCategorical(DT = common, obsCol = "dailyGhcn", modCol = "dailyPrcp", 
                            obsMissing = -999.9, groupBy = "statArg", threshold = c(1,5))
stat

