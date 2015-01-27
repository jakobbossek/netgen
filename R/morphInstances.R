#' Morphing of two networks with a convex combination of the coordinates.
#'
#' This function takes two (clustered) networks with equal number of nodes and,
#' if present, equal number of depots, and generates another instance by applying
#' a convex combination to the coordinates of node pairs. The node pairs are
#' determined by a point matching algorithm, which solves this assignement problem
#' via a integer programming procedure.
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
#' @examples
#'   x = generateRandomNetwork(n.points = 40L, n.depots = 2L)
#'   y = generateClusteredNetwork(n.points = 40L, n.cluster = 2L, n.depots = 2L)
#'   z = morphInstances(x, y, alpha = 0.2)
#'   \dontrun{
#'   library(gridExtra)
#'   plot.list = list(autoplot(x), autoplot(z), autoplot(y))
#'   plot.list$nrow = 1
#'   do.call(grid.arrange, plot.list)
#'   }
#' @seealso \code{\link{visualizeMorphing}}, \code{\link{visualizePointMatching}}
#' @export
morphInstances = function(x, y, alpha) {
    assertClass(x, "Network")
    assertClass(y, "Network")
    assertNumber(alpha, lower = 0, upper = 1, na.ok = FALSE)

    getPointMatchingAndMorphCoordinates = function(coords1, coords2) {
        point.matching = getOptimalPointMatching(coords1, coords2)
        coordinates = makeConvexCombination(coords1, coords2[point.matching[, 2], ], alpha)
        return(coordinates)
    }

    x.coordinates = x$coordinates
    y.coordinates = y$coordinates

    if ((hasDepots(x) && !hasDepots(y)) || (!hasDepots(x) && hasDepots(y))) {
        stopf("Both or none of the instances must have depots")
    }

    coordinates = getPointMatchingAndMorphCoordinates(x.coordinates, y.coordinates)
    depot.coordinates = NULL

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
    }
    z = makeNetwork(
        coordinates = coordinates,
        depot.coordinates = depot.coordinates,
        lower = x$lower,
        upper = x$upper
    )
    # FIXME: ugly to do that here
    attr(z, "morphed") = TRUE
    attr(z, "morphing.grade") = alpha
    return(z)
}
