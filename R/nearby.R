#' Nearby Function
#'
#' Function to find nearest polygons to a set of spatial points and return identifying column from polygons as a column in points attribute table.
#' @param points an sp SpatialPointsDataFrame
#' @param polys an sp SpatialPolygonsDataFrame
#' @param column a column in SpatialpointsDataFrame attribute table
#' @keywords spatial
#' @keywords proximity
#' @keywords gDistance
#' @export
#' @examples
#' library(rgdal); library(rgeos)
#' points <- readOGR('.','TestPoints')
#' polys <- readOGR('.','TestPolys')
#' proj4string(points) == proj4string(polys)
#' polys = spTransform(polys, CRS(proj4string(points)))
#' points@data <- nearby(points, polys, 'FieldName')
#' head(points@data)

nearby = function(points, polys, column){
  m = gDistance(points, polys, byid=TRUE)
  row = apply(m, 2, function(x) which(x==min(x)))
  row = sapply(row, "[",1)
  labels = unlist(polys@data[row,][[column]])
  points$missing <- labels
  # head(missing)
  return(points@data)
}
