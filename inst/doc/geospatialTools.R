#' ---
#' title: "Spatial tools"
#' author: "Arezoo Rafieeinasab & Aubrey Dugger"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Vignette Title}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#' For most of the postprocessing, there is a need to create spatial maps, aggregate over spatial units and also produce georeference raster and shapefiles. Many of the existing functions in available spatial libraries such as SP, RGDAL, RGEOS and Raster has been wrapped in rwrfhydro to serve our purpose. Here we explain these spatial functions, their application as well as some examples.
#' 
#' ## List of the available functions
#' - GetProj
#' - GetGeogridSpatialInfo
#' - ExportGeogrid
#' - GetGeogridIndex
#' - GetTimeZone
#' - GetRfc
#' - GetPoly
#' - PolygonToRaster
#' 
#' ## General Info
#' The case study data should be placed at the home directory in order to use the following vignette without changing any line. Otherwise, change the addresses accordingly. To find out the address to the home directory use the following commnad.
#' 
## ------------------------------------------------------------------------
path.expand("~")

#' 
#' Load teh rwrfhydro package.
#' 
## ------------------------------------------------------------------------
library(rwrfhydro)
options(warn=1)

#' 
#' Set a data path to Fourmile Creek test case.
#' 
## ------------------------------------------------------------------------
fcPath <- '~/wrfHydroTestCases/Fourmile_Creek_testcase_v2.0'

#' 
#' Geogrid file is the main file containing all the base geographic information on the model domain such as the geographic coordinate system, latitude, longitude of each pixel and so on. We use this file frequently. Set a path to geogrid file.
#' 
## ------------------------------------------------------------------------
geoFile <- paste0(fcPath,'/DOMAIN/geo_em_d01.Fourmile1km.nlcd11.nc')

#' 
#' 
#' ## GetProj
#' 
#' To be able to use any of the spatial tools in R, projection information of the model domain is required. All the model input and output file are based on geogrid file domain. `GetProj` pull projection information of WRF-Hydro modeling domain from geogrid file. It takes only `geoFile` and return the projection information as a character.
#' 
## ------------------------------------------------------------------------
proj4 <- GetProj(geoFile)
proj4

#' 
#' ## GetGeogridSpatialInfo
#' 
#' It pull necessary geospatial information about WRF-Hydro modeling domain from geogrid file used for regridding and deprojection.
#' It only requires the address to the geogrid file and return a data frame containing geospatial information such as the projection information, number of rows and columns and size of the grids.
#' 
## ------------------------------------------------------------------------
geoInfo <- GetGeogridSpatialInfo(geoFile)
geoInfo

#' 
#' 
#' ## ExportGeogrid
#' 
#' If you need to create a georeferenced TIF file from any variable in a netcdf file, then you need to use `ExportGeogrid` function. It takes a NetCDF file having lat/lon information and converts the specified variable into a georeferenced TIF file for use in standard GIS tools.
#' Now, let's export one of the variable from the geogrid file. You can get a list of all available variables in the `geoFile` using `ncdump` function in rwrfhydro.
#' 
## ---- eval = FALSE-------------------------------------------------------
## head(ncdump(geoFile))

#' 
#' Now we will create a georeferenced TIF file from HGT_M field. You only need to provide the address to geogrid file (`geoFile`), the name of the variable (`HGT_M`) and the name of the output file (`geogrid_hgt.tif`).
#' 
## ---- results='hide', message=FALSE, warning=FALSE-----------------------
ExportGeogrid(geoFile,"HGT_M", "geogrid_hgt.tif")

#' 
#' You can now use the created file in any standard GIS platform. Here we will just read it into memory as a raster and dispaly it.
#' 
## ----plot1, fig.show = "hold", fig.width = 8, fig.height = 8, out.width='600', out.height='600'----
# read the saved tiff file
library(raster)
r <- raster("geogrid_hgt.tif")

# plot the imported raster from tif file
plot(r, main = "HGT_M")

# check the raser information and notice taht geographic coordinate information has been added.
r

#' 
#' 
#' Many of the input and output files such as LDASOUT output file does not contain lat/lon coordinates but matches the spatial coordinate system of the geogrid input file. In that case, you could feed the geogrid file `geoFile` which the lat/lon information will be taken from that file. 
#' 
## ----plot2, fig.width = 8, fig.height = 8, out.width='600', out.height='600'----
file = paste0(fcPath,"/run.FluxEval/RESTART.2013060100_DOMAIN1")
# ncdump(file) # check if the SOIL_T exist in the file

# we will read the third layer of soil temperature
ExportGeogrid(file,
             inVar="SOIL_T",
             outFile="20130315_soilm3.tif",
             inCoordFile=geoFile,
             inLyr=3)

# read the  created tiff file
r <- raster("20130315_soilm3.tif")

# plot the imported raster from tiff file
plot(r, main = "Soil Temperature") # in raster

# check the raster information and notice geographic coordinate information has been added
r

#' 
#' 
#' ## GetGeogridIndex
#' 
#' To be able to use a bunch of tools such as `GetMultiNcdf`, one needs to have the indices (x,y) or the location of each cell within the domain. `GetGeogridIndex` get geogrid cell indices from lat/lon (or other) coordinates. `GetGeogridIndex` reads in a set of lat/lon (or other) coordinates and generates a corresponding set of geogrid index pairs. Yo can assign a projection to the points using `proj4` argument which will be used to transform the point to the `geoFile` coordinate system. Ckeck the vignette on precipitation for usage.
#' 
## ------------------------------------------------------------------------
sg <- data.frame(lon = seq(-105.562, -105.323, length.out = 10), 
                 lat = seq(40.0125, 40.0682, length.out = 10))
GetGeogridIndex(sg, geoFile)

#' 
#' 
#' ## GetTimeZone
#' 
#' Many of the point observation are reported in local time and needs to be converted to UTC time to be comparable with WRF-Hydro input and outputs. `GetTimeZone` return the time zone for any point having longitude and latitude. It simply takes a dataframe containing at least two fields of `latitude` and `longitude`, overlays the `points` with a timezone shapefile (can be downloded from <http://efele.net/maps/tz/world/>). The shapefile is provided in rwrfhydro data and it is called `timeZone`.
#' 
## ------------------------------------------------------------------------
# timeZone has been provided by rwrfhydro as a SpatialPolygonDataFrame
class(timeZone)

# Shows the available timezone (TZID column in timeZone@data)
head(timeZone@data)

#' 
#' Function has three arguments. 
#' 
#' - `points`: A dataframe of the points. The dataframe should contain at least two fields called `latitude` and `longitude`.
#' - `proj4`: Projection of the `points` to be used in transforming the `points` projection to `timeZone` projection. Default is `+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0` which is the same as the `timezone` projection.
#' - `parallel`: If the number of points are high you can parallelize the process.
#' 
#' It will return the `points` dataframe with an added column called `timeZone`. It will return NA in case the point is not in any polygon. Now let's generate some random points and find their time zone information.
#' 
## ------------------------------------------------------------------------
# Provide a dataframe of 10 points having longitude and latitude as column name.
sg <- data.frame(longitude = seq(-110, -80, length.out = 10),
                 latitude = seq(30, 50, length.out = 10))

# Find the time zone for each point
sg <- GetTimeZone(sg)
sg

#' 
#' ## GetRfc
#' 
#' US has 13 offices of the River Forecast Center (RFC) which issue daily river forecasts using hydrologic models based on rainfall, soil characteristics, precipitation forecasts, and several other variables. Many of the statistics are desired to be grouped into River Forecast Center level so it would be easier to compare with the performance of the RFC models in the past. The RFC boundary shapefile is provided in rwrfhydro data and is called `rfc`.
#' 
## ------------------------------------------------------------------------
class(rfc)

# Shows the available rfc, name of the column is BASIN_ID
head(rfc@data)

#' 
#' `GetRfc` return the RFC name for any point having `longitude` and `latitude`. It takes a dataframe containing at least two fields of `latitude` and `longitude`, overlays the points with a `rfc` SpatialPolygonDataFrame and return the rfc's BASIN_ID. Function has three arguments. 
#' 
#' - `points`: A dataframe of the points. The dataframe should contain at least two fields called "latitude" and "longitude".
#' - `proj4`: Projection of the `points` to be used in transforming the `points` projection to `rfc` projection. Default is `+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0`.
#' - `parallel`: If the number of points are high you can parallelize the process.
#' 
#' It will return the points dataframe with an added column called `rfc`. It will return NA in case the point is not in any polygon.
#' 
## ------------------------------------------------------------------------
# Provide a dataframe of 10 points having longitude and latitude as column name.
sg <- data.frame(longitude = seq(-110, -80, length.out = 10), 
                 latitude = seq(30, 50, length.out = 10))

# Find the rfc for each point
sg <- GetRfc(sg)
sg

#' 
#' ## GetPoly
#' 
#' `Getpoly` is similar to `GetRfc`, it is a wrapper for function `sp::over`. It takes a dataframe containing at least two fields of `latitude` and `longitude`, overlays the points with a `SpatialPolygonDataFrame` and return the requested attribute from the polygon. One could use the available `SpatialPolygon*` loaded into memory or provide the address to the location of a polygon shapefile and the name of the shapefile and it will read the polygon using `rgdal::readOGR` function.
#' 
#' Let's get the RFC information from `GetPoly` instead of `GetRfc`. Here we provide the name of the `SpatialPolygon*` and using argument `join` request one of the attributes of the polygon. For example, here we have requested the `BASIN_ID`, `RFC_NAME` and `RFC_CITY`. 
#' 
## ------------------------------------------------------------------------
# Provide a dataframe of 10 points having longitude and latitude
sg <- data.frame(longitude = seq(-110, -80, length.out = 10), 
                 latitude = seq(30, 50, length.out = 10))

# Find the ID of RFC for each point
sg <- GetPoly(points = sg, polygon = rfc, join = "BASIN_ID")

# Find the full name of RFC for each point
sg <- GetPoly(points = sg, polygon = rfc, join = "RFC_NAME")

# Find the location/city of RFC for each point
sg <- GetPoly(points = sg, polygon = rfc, join = "RFC_CITY")
sg

#' 
#' Now let's provide the address to a shapefile on the disk as well as the name of the shapefile and perform the same process. We have clipped the `HUC12` shapefile and provided in the case study as a sample. The northeast of the clipped polygon covers partially the Fourmile Creek domain.
#'  
## ------------------------------------------------------------------------
# Provide a dataframe of 10 points within the Fourmile Creek domain having longitude and latitude
sg <- data.frame(longitude = seq(-105.562, -105.323, length.out = 10), 
                 latitude = seq(40.0125, 40.0682, length.out = 10))


# rgdal::readOG` has been used in the GetPoly function and it does not interpret the character/symbol `~`, 
# therefore, we need to use path.expand to get the full address to the case study location on your system. 
polygonAddress <- paste0(path.expand(fcPath), "/polygons")


# Find the HUC12 for each point
sg <- GetPoly(points = sg,
              polygonAddress = polygonAddress,
              polygonShapeFile = "clipped_huc12",
              join = "HUC12")
sg

#' 
#' ## PolyToRaster
#' 
#' If one wants to create a mask in the model domain (geogrid file), then needs to use `PolyToRaster`. It first picks up the required geographic information (like `proj4`) from the geogrid file (`geoFile`) and then use `raster::rasterize` function to grab the mask or attibute values from the `SpatialPolygonDataFrame`. This function is basically wrapping the `raster::rasterize` fucntion to serve our purpose. Below is a few different way one could use this function.
#' 
#' Example 1 : 
#' Let's get the RFC's ID for each pixel within the Fourmile Creek domain. This is equivalent to rasterizing the `rfc` `SpatialPolygonDataFrame` based on the `BASIN_ID`.
#' 
## ----plot3, fig.width = 8, fig.height = 8, out.width='600', out.height='600'----
r <- PolyToRaster(geoFile = geoFile,
                  useRfc = TRUE,
                  field ="BASIN_ID")

#' 
#' To know what are the corresponding values to the integer values used in rasterized output, you need to use the following command.
#' 
## ------------------------------------------------------------------------
r@data@attributes 

#' As the result shows all the case study domain falls into one RFC. 
#' 
#' 
#' Example 2 : 
#' rasterize the HUC12 `SpatialPolygonDataFrame` based on the `HUC12` field. The clipped HUC12 shapefile is provided with the test case which is quite larger than the model domain. You could read the shapefile and plot it as below.
## ----results="hide", plot4, fig.width = 8, fig.height = 8, out.width='600', out.height='600'----
polyg <- rgdal::readOGR(paste0(path.expand(fcPath), "/polygons"), "clipped_huc12")
plot(polyg, main = "Clipped HUC12") ## in raster

#' 
#' Our study domain partially covers a few basins at northeast of this shapefile.
#' 
## ----results="hide", plot5, fig.width = 8, fig.height = 8, out.width='600', out.height='600'----
polygonAddress <- paste0(path.expand(fcPath), "/polygons")
r <- PolyToRaster(geoFile = geoFile,
                  polygonAddress = polygonAddress,
                  polygonShapeFile = "clipped_huc12",
                  field ="HUC12")

#' 
#' To get the `HUC12` actual values:
#' 
## ------------------------------------------------------------------------
r@data@attributes

#' 
#' Example 3: You can get a unified mask over the study domain as follows:
#' 
## ----results="hide", plot6, fig.width = 8, fig.height = 8, out.width='600', out.height='600'----
r <- PolyToRaster(geoFile = geoFile,
                  polygonAddress = polygonAddress,
                  polygonShapeFile = "clipped_huc12",
                  mask =TRUE)

#' 
#' Example 4: You could also get a separate mask for each subbasin (HUC12 in this case) with the fraction of each grid cell that is covered by each polygon. The fraction covered is estimated by dividing each cell into 100 subcells and determining presence/absence of the polygon in the center of each subcell. 
#' 
#' 
## ----results="hide", plot7, fig.width = 8, fig.height = 8, out.width='600', out.height='600'----
r <- PolyToRaster(geoFile = geoFile,
                  polygonAddress = polygonAddress,
                  polygonShapeFile = "clipped_huc12",
                  field = "HUC12",
                  getCover = TRUE)
plot(r) ## in raster

#' 
