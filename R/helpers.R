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
  sqrt(crossprod(x - y))
}

# Computes the euclidean distance between a vector a a matrix.
#
# @param x [numeric]
#   First numeric vector.
# @param y [numeric]
#   Numeric matrix.
# @return [numeric]
euklideanDistances = function(x, y) {
  assertNumeric(x, min.len = 2L, any.missing = FALSE, all.missing = FALSE)
  assertMatrix(y, any.missing = FALSE, all.missing = FALSE, ncol = length(x))
  sapply(1:nrow(y), function(i) {
    euclidean(x, y[i, ])
  })
}

# Generate random string.
#
# @param length [integer(1)]
#   Desired length of the string.
# @return [character(1)]
generateRandomString = function(length = 10L) {
  collapse(sample(c(0:9, letters, LETTERS), size = length, replace = TRUE), sep = "")
}

# Generate a (partially) random name.
#
# @param n.points [integer(1)]
#   Number of points.
# @param n.dim [integer(2)]
#   Number of dimensions.
# @param n.cluster [integer(1)]
#   Number of clusters. Default is 1.
# @return [character(1)]
generateName = function(n.points, n.dim, n.cluster = 1L) {
  paste(
    "n", n.points,
    "cl", n.cluster,
    "d", n.dim,
    generateRandomString(),
    sep = "_"
  )
}
