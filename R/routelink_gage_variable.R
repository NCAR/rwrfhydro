if(FALSE) {
# add a 'gages' column to a RouteLink.nc file
rlFile <- '~/WRF_Hydro/DOMAIN_library/BoCr_100m_1km_NHDPlus_2015_08_11/Route_Link.nc'
rl <- as.data.frame(GetNcdfFile(rlFile))
rl$gages <- formatC('', width=15)

gageIds <- c(
  '06730200', #BOULDER_CREEK_AT_NORTH_75TH_ST._NEAR_BOULDER_CO
  '06730160', #FOURMILE_CANYON_CREEK_NEAR_SUNSHINE_CO
#  '06727410', #FOURMILE_CREEK_AT_LOGAN_MILL_ROAD_NEAR_CRISMAN_CO
  '06727500'  #FOURMILE_CREEK_AT_ORODELL_CO
)

whGages <- c( which(rl$link==42),  #BOULDER_CREEK_AT_NORTH_75TH_ST._NEAR_BOULDER_CO
              which(rl$link==38),   #FOURMILE_CANYON_CREEK_NEAR_SUNSHINE_CO
#              which(rl$link==200),  #FOURMILE_CREEK_AT_LOGAN_MILL_ROAD_NEAR_CRISMAN_CO
              which(rl$link==82)   #FOURMILE_CREEK_AT_ORODELL_CO
)

rl$gages[whGages] = formatC(gageIds,width = 15)

identifier <- 'threeRealGages'

newDir <- dirname(rlFile)
origBase <- basename(rlFile)
newFile <- paste0(newDir, '/', gsub('\\.',paste0('.',identifier,'.'), origBase))
file.copy(rlFile, newFile, overwrite = TRUE)

ncid <- ncdf4::nc_open(newFile, write=TRUE)
link <- ncid$dim[['link']]
idDim <- ncdf4::ncdim_def( 'IDLength', '', 1:15)
gages <- ncdf4::ncvar_def('gages', 'usgsId', list(idDim,link), formatC('', width=15))
ncid <- ncdf4::ncvar_add(ncid, gages)
ret <- ncdf4::ncvar_put( ncid, 'gages', rl$gages)
ncdf4::nc_close(ncid)
gages2 <- print(ncdump(newFile,'gages'))
}