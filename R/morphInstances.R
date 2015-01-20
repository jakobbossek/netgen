#' Morphing of two networks with a convex combination of the coordinates.
#'
#' This function takes two (clustered) networks with equal number of nodes,
#' and generates another instance by applying a convex combination to the coordinates
#' of node pairs. The node pairs are determined by a point matching algorithm,
#' which solves this assignement problem via a integer programming procedure.
#' If both instances contain depots, point matching is done seperately on depots
#' and the remaining nodes.
#'
#' @param x [\code{Network}]\cr
#'   Network or ClusteredNetwork.
#' @param y [\code{Network}]\cr
#'   Network or ClusteredNetwork.
#' @param alpha [\code{numeric(1)}]\cr
#'   Coeffiecient alpha for convex combination.
#' @return [\code{Network}]
#'   Morphed Network instance.
#' @export
morphInstances = function(x, y, alpha) {
    assertClass(x, "Network")
    assertClass(y, "Network")
    assertNumber(alpha, lower = 0, upper = 1, na.ok = FALSE)

    getPointMatchingAndMorphCoordinates = function(coords1, coords2) {
        point.matching = getOptimalPointAssignment(coords1, coords2)
        coordinates = makeConvexCombination(coords1, coords2[point.matching[, 2], ], alpha)
        return(coordinates)
    }

    x.coordinates = x$coordinates
    y.coordinates = y$coordinates

    if ((hasDepots(x) && !hasDepots(y)) || (!hasDepots(x) && hasDepots(y))) {
        stopf("Both or none of the instances must have depots")
    }

    if (all(hasDepots(x), hasDepots(y))) {
        depot.idx = which(x$types == "depot")
        x.coordinates = x.coordinates[-depot.idx, , drop = FALSE]
        y.coordinates = y.coordinates[-depot.idx, , drop = FALSE]
    }

    coordinates = getPointMatchingAndMorphCoordinates(x.coordinates, y.coordinates)
    types = rep("customers", getNumberOfNodes(x) - 2)

    if (all(hasDepots(x), hasDepots(y))) {
        x.n.depots = getNumberOfDepots(x)
        y.n.depots = getNumberOfDepots(y)
        if (x.n.depots != y.n.depots) {
            stopf("Number of depots must be equal, but x has %i and y has $i depots.",
                x.n.depots, y.n.depots)
        }
        x.depot.coordinates = getDepotCoordinates(x)
        y.depot.coordinates = getDepotCoordinates(y)
        depot.coordinates = getPointMatchingAndMorphCoordinates(x.depot.coordinates, y.depot.coordinates)
        coordinates = rbind(depot.coordinates, coordinates)
        types = c(rep("depot", x.n.depots), types)
    }
    z = makeNetwork(coordinates = coordinates, types = types, lower = x$lower, upper = x$upper)
    # FIXME: ugly to do that here
    attr(z, "morphed") = TRUE
    attr(z, "morphing.grade") = alpha
    return(z)
}

# Visualize point matching.
#
# Draw the points and lines between the matched points for visualization.
#
# @param coords1 [matrix]
#   Matrix of coordinates of the first point set.
# @param coords2 [matrix]
#   Matrix of coordinates of the second point set.
# @param point.matching [matrix]
#   Point matching received via \code{getOptimalPointAssignment}.
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

# Component-wise convex combination of two matrizes.
#
# @param coords1 [matrix]
#   First matrix.
# @param coords2 [matrix]
#   Second matrix.
# @param alpha [numeric(1)]
#   Coefficient for convex combination.
# @return [matrix]
makeConvexCombination = function(coords1, coords2, alpha) {
  alpha * coords1 + (1 - alpha) * coords2
}

# Computes optimal point assignment for two sets of points of equal size.
#
# Internally it handles the points and the possible matchings as a bi-partite
# graphs and finds an  optimal matching due to euclidean distance by an
# efficient linear programming solver.
#
# @param coords1 [matrix]
#   Matrix of coordinates of the first point set.
# @param coords2 [matrix]
#   Matrix of coordinates of the second point set.
# @return [matrix]
#   Each row consists of the indizes of the pairwise matchings.
getOptimalPointAssignment = function(coords1, coords2) {
    dist.matrix = matrix(nrow = nrow(coords1), ncol = nrow(coords2))
    for (i in seq(nrow(coords1))) {
        for (j in seq(nrow(coords2))) {
            dist.matrix[i, j] = euklideanDistance(coords1[i, ], coords2[j, ])
        }
    }

    requirePackages("lpSolve", why = "getOptimalPointAssignment")
    lp.res = lp.assign(dist.matrix)
    if (lp.res$status != 0) {
        stop("Failed to find LP solution! No point matching possible.")
    }
    lp.res = lp.res$solution

    # now construct mapping matrix
    res = matrix(nrow = nrow(lp.res), ncol = 2)
    res[, 1] = 1:nrow(lp.res)
    for (i in 1:nrow(lp.res)) {
        res[i, 2] = which(lp.res[i, ] != 0)
    }
    #FIXME: what the fuck is going on here??? Each line consists of exactly one 1
    # but R does not find it! if I ask for != 0, it works! -.-w
    #res[, 2] = as.numeric(apply(lp.res, 1, function(row) as.numeric(which(row == 1))))
    res
}

euklideanDistance = function(x, y) {
    sqrt(sum((x - y)^2))
}