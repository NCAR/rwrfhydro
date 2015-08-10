# add a 'gages' column to a RouteLink.csv file
rl <- GetNcdfFile('~/WRF_Hydro/DOMAIN_library/Col_Bldr_Creek/Route_Link.nc')
rl$gages <- '               '

gageIds <- c(
  '06730200', #BOULDER_CREEK_AT_NORTH_75TH_ST._NEAR_BOULDER_CO
  '06730160', #FOURMILE_CANYON_CREEK_NEAR_SUNSHINE_CO
  '06727410', #FOURMILE_CREEK_AT_LOGAN_MILL_ROAD_NEAR_CRISMAN_CO
  '06727500'  #FOURMILE_CREEK_AT_ORODELL_CO
)

whGages <- c( which(rl$link==104),  #BOULDER_CREEK_AT_NORTH_75TH_ST._NEAR_BOULDER_CO
              which(rl$link==89),   #FOURMILE_CANYON_CREEK_NEAR_SUNSHINE_CO
              which(rl$link==104),  #FOURMILE_CREEK_AT_LOGAN_MILL_ROAD_NEAR_CRISMAN_CO
              which(rl$link==104)   #FOURMILE_CREEK_AT_ORODELL_CO
)