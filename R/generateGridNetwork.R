#' Generates a grid network in a hypercube.
#'
#' @param n.points [\code{integer(1)}]\cr
#'   Number of points. Ignored if \code{n.points.per.dim} is not \code{NULL}.
#'   Since the method generates only full grids, the root to base \code{n.dim}
#'   must be an integer \code{integer}. We recommend to use the parameter
#'   \code{n.points.per.dim} instead.
#' @param n.points.per.dim [\code{integer(1)}]\cr
#'   Number of points in each dimension.
#' @param n.dim [\code{integer(1)}]\cr
#'   Number of dimensions. Default ist 2.
#' @param lower [\code{numeric(1)}]\cr
#'   Lower box constraint of cube. Default is \code{0}.
#' @param upper [\code{numeric(1)}]\cr
#'   Upper box constraint of cube. Default is \code{100}.
#' @return [\code{Network}]
#' @examples
#'   x = generateGridNetwork(n.points.per.dim = 10L, upper = 50)
#' @export
generateGridNetwork = function(n.points = NULL, n.points.per.dim = NULL, n.dim = 2L, lower = 0, upper = 100) {
    if (is.null(n.points) && is.null(n.points.per.dim)) {
        stopf("At least one of the parameter n.points or n.points.per.dim must be provided.")
    }
    if (!is.null(n.points)) {
        assertCount(n.points, na.ok = FALSE)
    }
    if (!is.null(n.points.per.dim)) {
        assertCount(n.points.per.dim, na.ok = FALSE)
    }
    assertInteger(n.dim, len = 1L, any.missing = FALSE, lower = 2L)
    assertNumber(lower, lower = 0, finite = TRUE)
    assertNumber(upper, finite = TRUE)

    if (upper <= lower) {
        stopf("Argument 'upper' must be greater than argument 'lower'.")
    }

    # check whether the number of points is sufficient to generate a perfect grid layout
    if (is.null(n.points.per.dim)) {
        n.points.per.dim = n.points^(1 / n.dim)
        if (n.points.per.dim != round(n.points.per.dim, digits = 0L)) {
            stopf("Desired grid cannot be generated.")
        }
        if ((n.points.per.dim^(n.dim) - n.points) > 0.01) {
            stopf("Desired grid cannot be generated.")
        }
    }

    # build source sequence for each dimension
    source.seq = seq(lower, upper, length.out = round(n.points.per.dim, digits = 0L))
    x = list()
    for (i in 1:n.dim) {
        x[[i]] = source.seq
    }

    # generate coordinates
    coordinates = as.matrix(do.call(expand.grid, x))

    makeNetwork(
        coordinates = coordinates,
        lower = lower,
        upper = upper
    )
}