#' SpatialLineEndpoints Function
#'
#' Function to build a spatial points data frame of end nodes using a spatial lines data frame -
#' function is a modification of SpatialLinesMidPoints in the R maptools package.
#' @param sldf an sp SpatialLinesDataFrame
#' @keywords spatial
#' @keywords lines
#' @keywords endpoints
#' @export
#' @examples
#' library(rgdal)
#' spldf <- readOGR('.','MyLines')
#' endpoints = SpatialLinesEndPoints(spldf)

SpatialLinesEndPoints = function(sldf){
  Lns <- slot(sldf, "lines")
  hash_lns <- sapply(Lns, function(x) length(slot(x, "Lines")))
  N <- sum(hash_lns)
  endpoints <- matrix(NA, ncol = 2, nrow = N)
  Ind <- integer(length = N)
  ii <- 1
  for (i in 1:length(Lns)) {
    Lnsi <- slot(Lns[[i]], "Lines")
    for (j in 1:hash_lns[i]) {
      Ind[ii] <- i
      endpoints[ii, ] <- slot(Lnsi[[j]], "coords")[nrow(slot(Lnsi[[j]], "coords")),]
      ii <- ii + 1
    }
  }
  if (is(sldf, "SpatialLinesDataFrame")) {
    df0 <- slot(sldf, "data")[Ind, ]
    df <- as.data.frame(cbind(df0, Ind))
  }
  else df <- data.frame(Ind = Ind)
  spdf <- SpatialPointsDataFrame(endpoints, data = df, proj4string = CRS(proj4string(sldf)))
  return(spdf)
}
