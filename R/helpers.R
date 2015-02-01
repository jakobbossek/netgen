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

# Computes the euclidean distance between two vectors.
#
# @param x [numeric]
#   First numeric vector.
# @param y [numeric]
#   Second numeric vector.
# @return [numeric(1)]
euklideanDistance = function(x, y) {
    sqrt(sum((x - y)^2))
}
