#' Visualize point matching.
#'
#' Visualize a point matchings. Points and lines between the matched points
#' are drawn in order to visualize the assignment.
#'
#' @param x [Network | matrix]\cr
#'   Network or (n x 2) matrix.
#' @param y [Network | matrix]\cr
#'   Network or (n x 2) matrix.
#' @param point.matching [\code{matrix}]\cr
#'   Point matching received via \code{getOptimalPointMatching} for example.
#' @param highlight.longest [\code{integer(1)}]\cr
#'   Number of longest distances which should be particularly highlighted.
#'   Default is \code{0}.
#' @return [\code{\link[ggplot2]{ggplot}}]
#' @examples
#' # point matching on networks
#' x = generateRandomNetwork(n.points = 20L, upper = 100)
#' y = generateClusteredNetwork(n.points = 20L, n.cluster = 2L, upper = 100)
#' \dontrun{
#' pm = getOptimalPointMatching(x$coordinates, y$coordinates)
#' print(visualizePointMatching(x, y, pm, highlight.longest = 2L))
#' }
#'
#' # point matching on point clouds
#' x = matrix(runif(20L), 2L)
#' y = matrix(runif(20L), 2L)
#' \dontrun{
#' pm = getOptimalPointMatching(x, y)
#' print(visualizePointMatching(x, y, pm))
#' }
#' @seealso \code{\link{getOptimalPointMatching}}, \code{\link{morphInstances}},
#'   \code{\link{visualizeMorphing}}
#' @export
visualizePointMatching = function(x, y, point.matching, highlight.longest = 0L) {
  #FIXME: this is kind of ugly code
  if (inherits(x, "Network") & inherits(y, "Network")) {
    coords1 = x$coordinates
    coords2 = y$coordinates
    lower = x$lower
    upper = x$upper
  } else if (is.matrix(x) & is.matrix(y)) {
    coords1 = x
    coords2 = y
    lower = min(apply(rbind(coords1, coords2), 2L, min))
    upper = max(apply(rbind(coords1, coords2), 2L, max))
  } else {
    stopf("Both x and y need to be of type matrix or Network.")
  }

  assertMatrix(point.matching, mode = "numeric")

  if (highlight.longest > 0L) {
    assertInteger(highlight.longest, len = 1L, lower = 1L, upper = nrow(coords1), any.missing = FALSE)
  }

  df.points = as.data.frame(rbind(coords1, coords2), row.names = NULL)
  df.points = cbind(df.points, data.frame(type = rep(c("a", "b"), each = nrow(coords1))))
  colnames(df.points) = c("x1", "x2", "type")
  df.points$type = as.factor(df.points$type)

  df.lines = cbind(as.data.frame(coords1), as.data.frame(coords2[point.matching[, 2], ]))
  colnames(df.lines) = c("x1", "x2", "end1", "end2")

  # compute distances between matched points
  distances = computeDistancesOfPairedPoints(coords1, coords2, point.matching)
  longest.dist.idx = order(distances, decreasing = TRUE)[1:highlight.longest]

  pl = ggplot(df.lines, aes_string(x = "x1", y = "x2"))

  if (highlight.longest > 0) {
    # highlight the longest distances in particular
    pl = pl + geom_segment(
      data = df.lines[-longest.dist.idx, ],
      aes_string(
        x = "x1", y = "x2",
        xend = "end1", yend = "end2"),
        arrow = grid::arrow(length = grid::unit(0.1, "inches")
      ), colour = "gray")
    pl = pl + geom_segment(
      data = df.lines[longest.dist.idx, ],
      aes_string(
        x = "x1", y = "x2",
        xend = "end1", yend = "end2"),
        arrow = grid::arrow(length = grid::unit(0.1, "inches")
      ), colour = "darkgray", size = 0.9)
  } else {
    pl = pl + geom_segment(aes_string(
      x = "x1", y = "x2",
      xend = "end1", yend = "end2"
    ), arrow = grid::arrow(length = grid::unit(0.1, "inches")), colour = "gray")
  }
  pl = pl + geom_point(
    data = df.points,
    aes_string(
      x = "x1", y = "x2",
      shape = "type", colour = "type")
    )
  pl = pl + ggtitle("point mapping")
  pl = decorateGGPlot(pl, lower = lower, upper = upper)
  return(pl)
}

# Computes distances of
# @param x [Network]
#   First network.
# @param y [Network]
#   Second network.
# @param pm [matrix]
#   Point matching (IDs of matched nodes in each row)
# @return [numeric(1)]
#   Sum of euclidean distances between the matched points.
computeDistancesOfPairedPoints = function(x.coords, y.coords, pm) {
  n = nrow(x.coords)
  y.coords = y.coords[pm[, 2], ]
  distances = numeric(n)
  for (i in 1:n) {
    distances[i] = euklideanDistance(x.coords[i, ], y.coords[i, ])
  }
  return(distances)
}
