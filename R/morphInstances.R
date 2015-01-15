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

    coords1 = x$coordinates
    coords2 = y$coordinates
    point.matching = getOptimalPointAssignment(coords1, coords2)
    print(point.matching)
    coordinates = makeConvexCombination(coords1, coords2[point.matching[, 2], ], alpha)
    # x = as_tsp_instance(x)
    # y = as_tsp_instance(y)

    # z = tspmeta::morph_instances(x, y, alpha)
    coordinates = as.data.frame(coordinates)
    colnames(coordinates) = c("x1", "x2")
    #FIXME: we need to handle customers and depots seperately
    z = makeNetwork(coordinates, types = "customers")
    attr(z, "morphed") = TRUE
    attr(z, "morphing.grade") = alpha
    return(z)
}

# as_tsp_instance = function(x) {
#     tspmeta::tsp_instance(coords = as.matrix(x$coordinates))
# }

#FIXME: comment this extensively
makeConvexCombination = function(coords1, coords2, alpha) {
  alpha * coords1 + (1 - alpha) * coords2
}

#FIXME: comment this extensively
getOptimalPointAssignment = function(coords1, coords2) {
    dist.matrix = matrix(nrow = nrow(coords1), ncol = nrow(coords2))
    for (i in seq(nrow(coords1))) {
        for (j in seq(nrow(coords2))) {
            dist.matrix[i, j] = euklideanDistance(coords1[i, ], coords2[j, ])
        }
    }

    library(lpSolve)
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