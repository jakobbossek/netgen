#' Generates a random graph in a hypercube.
#'
#' @param n.points [\code{integer(1)}]\cr
#'   Number of points.
#' @param n.dim [\code{integer(1)}]\cr
#'   Number of dimensions. Default ist 2.
#' @param lower [\code{numeric(1)}]\cr
#'   Lower bound of cube.
#' @param upper [\code{numeric(1)}]\cr
#'   Upper bound of cube.
#' @return [\code{data.frame}]
#' @export
generateRandomInstance = function(n.points, n.dim = 2L, lower = 0, upper = 1) {
    assertCount(n.points, na.ok = FALSE)
    assertInteger(n.dim, len = 1L, any.missing = FALSE, lower = 2L)
    assertNumber(lower, lower = 0)
    assertNumber(upper)

    if (upper <= lower) {
        stopf("Argument 'upper' must be greater than argument 'lower'.")
    }

    coordinates = runif(n.points * n.dim, min = lower, max = upper)
    coordinates = as.data.frame(matrix(coordinates, ncol = n.dim))
    colnames(coordinates) = paste("x", seq(n.dim), sep = "")
    makeNetwork(coordinates)
}