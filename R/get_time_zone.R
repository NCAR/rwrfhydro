#' Return the time zone for gauge locations.
#'
#' \code{GetTimeZone} gets a dataframe of the gauge locations having at least
#' two fields of latitude and longitude. it overlays the points with timezone
#' shapefile (can be downloded from \url{http://efele.net/maps/tz/world/})
#' and add the timezone as a field to the input dataframe.
#' Original R-script is from
#' \url{https://www.nceas.ucsb.edu/scicomp/usecases/point-in-polygon}.
#
# Input Data
#  * the dataframe of the gauge locations, there should cntain lat/lon columns in the dataframe
#  * Polygon shapefile containing the boundaries of timezones downloaded from http://efele.net/maps/tz/world/
#
# Workflow
#  1. Read the point lat/lon and tell R to treat it as a set of spatial points.
#  2. Read in the polygon shapefile.
#  3. Identify which points lie within the polygons.
#  4. For each point, get the name of the containing polygon (if any), and
#     add it to the point data table.
#  5. Write results to file, in both CSV and ESRI Shapefile formats,
#     and draw a map of the bear sightings and parks.


#' @param points A dataframe of the gauges, there should be at least two
#' fields of latitude ad longitude in the dataframe.
#' @param address Address to where polygonShapeFile is located.
#' @param polygonShapeFile Polygon shapefile containing the boundaries of
#' timezones (can be downloaded from \url{http://efele.net/maps/tz/world/}.
#' The TimeZone polygon has been included in rwrfhydro package. therefore,address and 
#' polygonShapeFile doen not have to be specified unless user has a different shapefile.
#'
#' @return  Point datafram with an extra column having the time zone for each gauge.
#

GetTimeZone<-function(points,address=NULL,polygonShapeFile=NULL){
# turn it dataframe into spatialPointsDataFrame

sp::coordinates(points) <- c("longitude","latitude")

# the the time zone polygons
if (!is.null(address) & !is.null(polygonShapeFile)) {
  timeZone <- rgdal::readOGR(address,polygonShapeFile)
}
# tell R that pointd coordinates are in the same lat/lon reference system
# as the timezone polygon data -- BUT ONLY BECAUSE WE KNOW THIS IS THE CASE!
sp::proj4string(points) <- sp::proj4string(timeZone)

# combine is.na() with over() to do the containment test; note that we
# need to "demote" timeZone to a SpatialPolygons object first
inside.timeZone <- !is.na(sp::over(points, as(timeZone, "SpatialPolygons")))

# use 'over' again, this time with timeZone polygon as a SpatialPolygonsDataFrame
# object, to determine which timeZone (if any) contains each point, and
# store the timeZone name as an attribute of the point data
points$timeZone <- sp::over(points, timeZone)$TZID
return(as.data.frame(points))
}



#' Time zone polygon
#' 
#' Polygon shapefile of world time zone classification.
#' 
#' @usage
#'  timeZone
#'  
#' @format
#' data.frame
#' 
#' @section Citation:
#' Online_Linkage: \link[http://efele.net/maps/tz/world/]{http://efele.net/maps/tz/world/} \cr
#' @family GetTimeZone
#' @concept data
#' @keywords data
"timeZone"



