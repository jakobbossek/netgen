#' Morphing of two networks with a convex combination of the coordinates.
#'
#' @param x [\code{Network}]\cr
#'   Network or ClusteredNetwork.
#' @param y [\code{Network}]\cr
#'   Network or ClusteredNetwork.
#' @param alpha [\code{numeric(1)}]\cr
#'   Coeffiecient alpha for convex combination.
#' @return [\code{Network}]
#'   Morphed Network instance.
# @export
#FIXME: comment this extensively
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

    if (!all(hasDepots(x), hasDepots(y))) {
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
        catf("x: %i, y: %i", x.n.depots, y.n.depots)
        if (x.n.depots != y.n.depots) {
            stopf("Number of depots must be equal, but x has %i and y has $i depots.", x.n.depots, y.n.depots)
        }
        x.depot.coordinates = getDepotCoordinates(x)
        y.depot.coordinates = getDepotCoordinates(y)
        depot.coordinates = getPointMatchingAndMorphCoordinates(x.depot.coordinates, y.depot.coordinates)
        coordinates = rbind(depot.coordinates, coordinates)
        types = c(rep("depot", x.n.depots), types)
    }
    #FIXME: we need to handle customers and depots seperately
    z = makeNetwork(coordinates = coordinates, types = types, lower = x$lower, upper = x$upper)
    # FIXME: ugly to do that here
    attr(z, "morphed") = TRUE
    attr(z, "morphing.grade") = alpha
    return(z)
}

#FIXME: write documentation. Export
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
    print(pl1)
}

#FIXME: comment this extensively
makeConvexCombination = function(coords1, coords2, alpha) {
  alpha * coords1 + (1 - alpha) * coords2
}

#FIXME: comment this extensively
getOptimalPointAssignment = function(coords1, coords2) {
    dist.matrix = matrix(nrow = nrow(coords1), ncol = nrow(coords2))
    for (i in seq(nrow(coords1))) {
        for (j in seq(nrow(coords2))) {
            # print(typeof(coords1[i, ]))
            # print(typeof(coords2[j, ]))
            # print("---")
            dist.matrix[i, j] = euklideanDistance(unlist(coords1[i, ]), unlist(coords2[j, ]))
        }
    }
    #print(dist.matrix)

    requirePackages("lpSolve", why = "getOptimalPointAssignment")
    lp.res = lp.assign(dist.matrix)
    if (lp.res$status != 0) {
        stop("Failed to find LP solution! No point matching possible.")
    }
    lp.res = lp.res$solution
    # now construct mapping matrix
    res = matrix(nrow = nrow(lp.res), ncol = 2)
    res[, 1] = 1:nrow(lp.res)
    #stop()
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