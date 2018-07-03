#' Nearby Function
#'
#' Function to find nearest polygons to a set of spatial points and return identifying column from polygons as a column in points attribute table.
#' @param points an sf spatial points data frame
#' @param polys an sf polygons data frame
#' @param points_name column name to use in points
#' @param polys_name column name to use in polys
#' @keywords spatial
#' @keywords closes
#' @keywords sf
#' @export
#' @examples
#' library(sf)
#' a <- st_read('TestPoints.shp')
#' polys <- st_read('TestPoly.shps')
#' st_crs(a) == st_crs(b)
#' a = st_transform(a, st_crs(b))
#' for (i in 1:nrow(a)){
#'   a[i,c(points_name)] <- closest[[i]][,c(polys_name)]
#'}

closest <- list()
for(i in seq_len(nrow(a))){
  closest[[i]] <- b[which.min(
    st_distance(b, a[i,])),]
}
