#' Visualize point matching.
#'
#' Draw the points and lines between the matched points for visualization.
#'
#' @param x [\code{matrix}]\cr
#'   First \code{Network} object.
#' @param y [\code{matrix}]\cr
#'   Second \code{Network} object.
#' @param point.matching [\code{matrix}]\cr
#'   Point matching received via \code{getOptimalPointMatching}.
#' @param highlight.longest [\code{integer(1)}]\cr
#'   Number of longest distances which should be particularly highlighted.
#'   Default is \code{0}.
#' @return [ggplot]
#'   ggplot2 object.
#' @export
visualizePointMatching = function(x, y, point.matching, highlight.longest = 0) {
    assertClass(x, "Network")
    assertClass(y, "Network")
    assertMatrix(point.matching, mode = "numeric")
    assertInteger(highlight.longest, len = 1L, lower = 1L, any.missing = FALSE)

    coords1 = x$coordinates
    coords2 = y$coordinates

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
        pl = pl + geom_segment(data = df.lines[-longest.dist.idx, ], aes_string(x = "x1", y = "x2", xend = "end1", yend = "end2"), arrow = grid::arrow(length = grid::unit(0.1, "inches")), colour = "gray")
        pl = pl + geom_segment(data = df.lines[longest.dist.idx, ], aes_string(x = "x1", y = "x2", xend = "end1", yend = "end2"), arrow = grid::arrow(length = grid::unit(0.1, "inches")), colour = "darkgray", size = 0.9)
    } else {
        pl = pl + geom_segment(aes_string(x = "x1", y = "x2", xend = "end1", yend = "end2"), arrow = grid::arrow(length = grid::unit(0.1, "inches")), colour = "gray")
    }
    pl = pl + geom_point(data = df.points, aes_string(x = "x1", y = "x2", shape = "type", colour = "type"))
    pl = pl + ggtitle("point mapping")
    pl = decorateGGPlot(pl, lower = x$lower, upper = x$upper)
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
        distances[i] = euclidean(x.coords[i, ], y.coords[i, ])
    }
    return(distances)
}

euclidean = function(x, y) {
    sqrt(sum((x - y)^2))
}