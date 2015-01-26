# Visualize point matching.
#
# Draw the points and lines between the matched points for visualization.
#
# @param coords1 [matrix]
#   Matrix of coordinates of the first point set.
# @param coords2 [matrix]
#   Matrix of coordinates of the second point set.
# @param point.matching [matrix]
#   Point matching received via \code{getOptimalPointMatching}.
# @return [ggplot]
#   ggplot2 object.
visualizePointMatching = function(coords1, coords2, point.matching) {
    rownames(coords1) = NULL
    rownames(coords2) = NULL
    df.points = as.data.frame(rbind(coords1, coords2), row.names = NULL)

    df.points = cbind(df.points, data.frame(type = rep(c("a", "b"), each = nrow(coords1))))
    colnames(df.points) = c("x1", "x2", "type")
    df.points$type = as.factor(df.points$type)

    df.lines = cbind(as.data.frame(coords1), as.data.frame(coords2[point.matching[, 2], ]))
    colnames(df.lines) = c("x1", "x2", "end1", "end2")

    pl1 = ggplot(df.lines, aes_string(x = "x1", y = "x2"))
    pl1 = pl1 + geom_segment(aes_string(x = "x1", y = "x2", xend = "end1", yend = "end2"), arrow = grid::arrow(length = grid::unit(0.1, "inches")), colour = "gray")
    pl1 = pl1 + theme(legend.position = "none") + ggtitle("point mapping")
    pl1 = pl1 + geom_point(data = df.points, aes_string(x = "x1", y = "x2", shape = "type", colour = "type"))
    return(pl1)
}