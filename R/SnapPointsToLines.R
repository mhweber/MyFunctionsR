#' SnapPointsToLines Function
#'
#' Function to snap points to flowlines borrowed from Ryan
#' Peek (https://ryanpeek.org/mapping-in-R-workshop/03_vig_snapping_points_to_line.html#Snap_Points_to_Lines)
#' and Tim Salabim (https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf)
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

st_snap_points <- function(x, y, namevar, max_dist = 1000) {

  # this evaluates the length of the data
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)

  # this part:
  # 1. loops through every piece of data (every point)
  # 2. snaps a point to the nearest line geometries
  # 3. calculates the distance from point to line geometries
  # 4. retains only the shortest distances and generates a point at that intersection
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  # this part converts the data to a dataframe and adds a named column of your choice
  out_xy <- st_coordinates(out) %>% as.data.frame()
  out_xy <- out_xy %>%
    mutate({{namevar}} := x[[namevar]]) %>%
    st_as_sf(coords=c("X","Y"), crs=st_crs(x), remove=FALSE)

  return(out_xy)
}
