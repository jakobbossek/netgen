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
morphInstances = function(x, y, alpha) {
    assertClass(x, "Network")
    assertClass(y, "Network")
    assertNumber(alpha, lower = 0, upper = 1, na.ok = FALSE)

    x = as_tsp_instance(x)
    y = as_tsp_instance(y)

    z = tspmeta::morph_instances(x, y, alpha)
    coordinates = as.data.frame(z$coords)
    colnames(coordinates) = c("x1", "x2")
    makeNetwork(coordinates)
}

as_tsp_instance = function(x) {
    tspmeta::tsp_instance(coords = as.matrix(x$coordinates))
}