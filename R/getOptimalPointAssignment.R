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