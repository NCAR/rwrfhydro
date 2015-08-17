if(FALSE) {
##----------------------
## nudging parameters file
gageParams <- read.csv('~/ncar/WRF_Hydro/DOMAIN_library/CONUS/nhdRtIntersect.csv',
                       colClasses = 'character' )

## for boulder creek domain with obselete gages
gageParams <- rbind(gageParams, data.frame(nhdRtIntersect=c("06730160", "06727410")))

gageParams$R=2000
gageParams$G=.75
gageParams$tau=60

## need to set the missing value used by ncdf4? i think it's NA by default
dimensionList <-
  list(  # n.b. the dimension order: z,y,x,t
    stationIdInd=list(name='stationIdInd',
                      units='', 
                      values=1:nrow(gageParams),
                      unlimited=TRUE,
                      create_dimvar=FALSE),
    
    stationIdStrLen=list(name='stationIdStrLen',
                         units='', 
                         values=1:15,
                         unlimited=FALSE,
                         create_dimvar=FALSE)
  )

varList = list()
varList[[1]] <- 
  list( name='stationId',
        longname='USGS station identifer',
        units='-',
        precision = 'char',
        #missing = ,
        dimensionList=dimensionList[c('stationIdStrLen','stationIdInd')],
        data = formatC(gageParams$nhdRtIntersect, width=15) )

varList[[2]] <- 
  list( name='R',
        longname='Radius of influence in meters',
        units='m',
        precision = 'float',
        #missing = ,
        dimensionList=dimensionList[c('stationIdInd')],
        data = gageParams$R )

varList[[3]] <- 
  list( name='G',
        longname='Amplitude of nudging',
        units='-',
        precision = 'float',
        #missing = ,
        dimensionList=dimensionList[c('stationIdInd')],
        data = gageParams$G )

varList[[4]] <- 
  list( name='tau',
        longname='Time tapering parameter',
        units='s',
        precision = 'float',
        #missing = ,
        dimensionList=dimensionList[c('stationIdInd')],
        data = gageParams$tau )

MkNcdf( varList,
        filename=paste0('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/',
                        'nudgingParameters_Bldr_Creek.nc'), 
        overwrite=TRUE )

ncdump(paste0('~/ncar/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/',
              'nudgingParameters_Bldr_Creek.nc'))

}
