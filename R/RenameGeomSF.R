#' RenameGeomSF Function
#'
#' Function to rename geometry column borrowed
#' from Barry Rowlingson (https://gis.stackexchange.com/questions/386584/sf-geometry-column-naming-differences-r)
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

RenameGeomSF <- function(frame, name){
  current = attr(frame, "sf_column")
  names(frame)[names(frame)==current] = name
  st_geometry(frame)=name
  frame
}
