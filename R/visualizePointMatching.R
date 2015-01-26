#' Visualize point matching.
#'
#' Draw the points and lines between the matched points for visualization.
#'
#' @param coords1 [\code{matrix}]\cr
#'   Matrix of coordinates of the first point set.
#' @param coords2 [\code{matrix}]\cr
#'   Matrix of coordinates of the second point set.
#' @param point.matching [\code{matrix}]\cr
#'   Point matching received via \code{getOptimalPointMatching}.
#' @param highlight.longest [\code{integer(1)}]\cr
#'   Number of longest distances which should be particularly highlighted.
#'   Default is \code{0}.
#' @return [ggplot]
#'   ggplot2 object.
#' @export
visualizePointMatching = function(coords1, coords2, point.matching, highlight.longest = 0) {
    assertMatrix(coords1, mode = "numeric")
    assertMatrix(coords2, mode = "numeric")
    assertMatrix(point.matching, mode = "numeric")
    assertInteger(highlight.longest, len = 1L, lower = 1L, any.missing = FALSE)

    df.points = as.data.frame(rbind(coords1, coords2), row.names = NULL)
    df.points = cbind(df.points, data.frame(type = rep(c("a", "b"), each = nrow(coords1))))
    colnames(df.points) = c("x1", "x2", "type")
    df.points$type = as.factor(df.points$type)

    df.lines = cbind(as.data.frame(coords1), as.data.frame(coords2[point.matching[, 2], ]))
    colnames(df.lines) = c("x1", "x2", "end1", "end2")

    # compute distances between matched points
    distances = computeDistancesOfPairedPoints(coords1, coords2, point.matching)
    longest.dist.idx = order(distances, decreasing = TRUE)[1:highlight.longest]

    pl1 = ggplot(df.lines, aes_string(x = "x1", y = "x2"))

    if (highlight.longest > 0) {
        # highlight the longest distances in particular
        pl1 = pl1 + geom_segment(data = df.lines[-longest.dist.idx, ], aes_string(x = "x1", y = "x2", xend = "end1", yend = "end2"), arrow = grid::arrow(length = grid::unit(0.1, "inches")), colour = "gray")
        pl1 = pl1 + geom_segment(data = df.lines[longest.dist.idx, ], aes_string(x = "x1", y = "x2", xend = "end1", yend = "end2"), arrow = grid::arrow(length = grid::unit(0.1, "inches")), colour = "darkgray", size = 0.9)
    } else {
        pl1 = pl1 + geom_segment(aes_string(x = "x1", y = "x2", xend = "end1", yend = "end2"), arrow = grid::arrow(length = grid::unit(0.1, "inches")), colour = "gray")
    }
    pl1 = pl1 + theme(
        legend.position = "none",
        plot.title = element_text(size = rel(0.8), lineheight = 1.1, vjust = 1.6)
    )
    pl1 = pl1 + ggtitle("point mapping")
    pl1 = pl1 + xlab(expression(x[1])) + ylab(expression(x[2]))
    pl1 = pl1 + geom_point(data = df.points, aes_string(x = "x1", y = "x2", shape = "type", colour = "type"))
    return(pl1)
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