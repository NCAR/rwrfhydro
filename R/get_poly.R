#' Return the time zone for gauge locations.
#'
#' \code{GetTimeZone} takes a dataframe of locations containing at least
#' two fields of latitude and longitude, overlays the points with a timezone
#' shapefile (can be downloded from \url{http://efele.net/maps/tz/world/}),
#' and adds the timezone as a field to the input dataframe.
#'
#' @param points A dataframe of the gauges. The dataframe should contain at least two
#' fields: "latitude" and "longitude".
#'
#' @return  Point dataframe with an extra column containing the time zone for each gauge.
#' 
#' @examples
#' \dontrun{
#' sg <- SelectGhcnGauges(countryCode="US",networkCode="C")[1:100,]
#' sg <- GetTimeZone(sg)
#' }
#' @keywords IO
#' @concept dataMgmt
#' @family geospatial
#' @export

GetTimeZone<-function(points){
# turn it dataframe into spatialPointsDataFrame

sp::coordinates(points) <- c("longitude","latitude")

# tell R that pointd coordinates are in the same lat/lon reference system
# as the timezone polygon data -- BUT ONLY BECAUSE WE KNOW THIS IS THE CASE!
sp::proj4string(points) <- sp::proj4string(timeZone)

# use 'over' with timeZone polygon as a SpatialPolygonsDataFrame
# object, to determine which timeZone (if any) contains each point, and
# store the timeZone name as an attribute of the point data

points$timeZone <-as.character(sp::over(points, timeZone)$TZID)
points <- as.data.frame(points, stringsAsFactors = FALSE)
return(points)
}

#' Return the RFC name for gauge locations.
#'
#' \code{GetRfc} takes a dataframe of gauge locations containing at least
#' two fields of latitude and longitude, overlays the points with RFC
#' shapefile, and adds the RFC name as a field to the input dataframe.
#
#' @param points A dataframe of the gauges. The dataframe should contain at least two
#' fields: "latitude" and "longitude".
#'
#' @return  Point dataframe with an extra column containing the RFC name for each gauge.
#' 
#' @examples
#' \dontrun{
#' sg <- SelectGhcnGauges(countryCode="US",networkCode="C")[1:100,]
#' sg <- GetRfc(sg)
#' }
#' @keywords IO
#' @concept dataMgmt
#' @family geospatial
#' @export

GetRfc<-function(points){
  
  # turn dataframe into spatialPointsDataFrame
  sp::coordinates(points) <- c("longitude","latitude")
  
  # tell R that pointd coordinates are in the same lat/lon reference system
  # as the rfc polygon data -- BUT ONLY BECAUSE WE KNOW THIS IS THE CASE!
  sp::proj4string(points) <- sp::proj4string(rfc)
  
  # use 'over' with rfc polygon as a SpatialPolygonsDataFrame
  # object, to determine which rfc (if any) contains each point, and
  # store the rfc name as an attribute of the point data
  points$rfc <- sp::over(points, rfc)$BASIN_ID
  return(as.data.frame(points))
}


#' Return the polygon attribute for each points.
#'
#' \code{GetPoly} takes a dataframe of points containing at least
#' two fields of latitude and longitude, overlays the points with a 
#' shapefile, and adds the attribute as a field to the input dataframe.
#'
#' @param points A dataframe of the points. The dataframe should contain at least two
#' fields: "latitude" and "longitude".
#' @param proj4 Projection to be used for points's dataframe.
#' @param polygonAddress Address to where polygonShapeFile is located.
#' @param polygonShapeFile Name of a polygon shapefile.
#' @param join Attribute from the polygon shapefile which will be added as a column to points dataframe
#' @return  Point dataframe with an extra column containing the attribute for each point.
#'
#' @examples
#' \dontrun{
#' sg<-GetPoly(sg,  polygonAddress= "/glade/scratch/arezoo/QPF_verification_rwrfhydro/gis/", polygonShapeFile= "huc6", join="HUC6")
#' }
#' @keywords IO
#' @concept dataMgmt
#' @family geospatial
#' @export


GetPoly<-function(points, proj4="+init=epsg:4269 +proj=longlat +ellps=GRS80
                  +datum=NAD83 +no_defs +towgs84=0,0,0", 
                  polygonAddress=NULL, polygonShapeFile=NULL, join=NULL ){
  
  if (is.data.frame(points) & all(c("latitude","longitude") %in% colnames(points))){
    
    # turn the dataframe into spatialPointsDataFrame
    sp::coordinates(points) <- c("longitude","latitude")
    
    # Define a coordinate system / projection system
    sp::proj4string(points) <- sp::CRS(proj4)
    
    # read the polygon shape file
    polyg <- rgdal::readOGR(polygonAddress,polygonShapeFile)
    
    # transform the points to polygon shapefile projections
    points<- sp::spTransform(points, sp::CRS(sp::proj4string(polyg)))
    
    # use 'over' this time with timeZone polygon as a SpatialPolygonsDataFrame
    # object, to determine which timeZone (if any) contains each point, and
    # store the timeZone name as an attribute of the point data
    
    points[[join]] <- sp::over(points, polyg)[[join]]
    return(as.data.frame(points))
  }else{
    stop("Input should be a dataframe having two columns named 
         latitude and longitude")
  }
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


# RFC boundary polygon
#' 
#' Polygon shapefile of River Forecast Center boundaries.
#' 
#' @usage
#'  rfc
#'  
#' @format
#' SpatialPolygonsDataFrame
#' 
#' @family GetRfc
#' @concept data
#' @keywords data
"rfc"

