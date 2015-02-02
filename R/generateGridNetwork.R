#' Generates a grid network.
#'
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
#' @note Grid networks with depots are not supported at the moment.
#' @export
generateGridNetwork = function(n.points.per.dim = NULL, n.dim = 2L, lower = 0, upper = 100) {
    assertCount(n.points.per.dim, na.ok = FALSE)
    assertInteger(n.dim, len = 1L, any.missing = FALSE, lower = 2L)
    assertNumber(lower, lower = 0, finite = TRUE)
    assertNumber(upper, finite = TRUE)

    if (upper <= lower) {
        stopf("Argument 'upper' must be greater than argument 'lower'.")
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
