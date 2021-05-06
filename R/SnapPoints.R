#' Nearby Function
#'
#' Function to fsnap sf points to sf lines
#' Pulled from Tim Salabim's solution to question
#' on StackOverflow
#' @param points an sf spatial points data frame
#' @param lines an sf spatial lines data frame
#'
#' @keywords spatial
#' @keywords snap
#' @export
#' @examples
#' brew = st_transform(breweries, st_crs(trails))

#' tst = st_snap_points(brew, trails, 500)

#' mapview(breweries) + mapview(tst, col.regions = "red") + trails

st_snap_points = function(points, lines, max_dist = 1000) {

  if (inherits(points, "sf")) n = nrow(points)
  if (inherits(points, "sfc")) n = length(points)

  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(points)[i], lines)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(points)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  return(out)
}
