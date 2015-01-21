#' Generate Network instance based on coordinates.
#'
#' @param coordinates [\code{matrix}]\cr
#'   Numeric matrix of 2D coordinates.
#' @param depot.coordinates [\code{matrix} | NULL]\cr
#'   Numeric matrix of 2D coordinates of depots. Default is \code{NULL}, which
#'   means no depots at all.
#' @param lower [\code{numeric(1)}]\cr
#'   Lower bound of bounding box.
#' @param upper [\code{numeric(1)}]\cr
#'   Upper bound of bounding box.
#' @return [\code{Network}]
#' @export
makeNetwork = function(coordinates, depot.coordinates = NULL, lower = 0, upper = 1) {
    assertMatrix(coordinates)
    if (!is.null(depot.coordinates))
        assertMatrix(depot.coordinates)
    makeS3Obj(
        coordinates = coordinates,
        depot.coordinates = depot.coordinates,
        lower = lower,
        upper = upper,
        classes = "Network"
    )
}

#' Generates clustered network instances based in coordinates
#' and membership vector.
#'
#' @param coordinates [\code{matrix}]\cr
#'   Numeric matrix of 2D coordinates.
#' @param depot.coordinates [\code{matrix} | NULL]\cr
#'   Numeric matrix of 2D coordinates of depots. Default is \code{NULL}, which
#'   means no depots at all.
#' @param membership [\code{numeric}]\cr
#'   Vector of memberships.
#' @param lower [\code{numeric(1)}]\cr
#'   Lower bound of bounding box.
#' @param upper [\code{numeric(1)}]\cr
#'   Upper bound of bounding box.
#' @return [\code{ClusteredNetwork}]
#' @export
makeClusteredNetwork = function(coordinates, depot.coordinates = NULL, membership, lower = 0, upper = 1) {
    network = makeNetwork(
        coordinates = coordinates,
        depot.coordinates = depot.coordinates,
        lower = lower,
        upper = upper
    )
    assertNumeric(membership, any.missing = FALSE)
    network$membership = membership
    network = addClasses(network, "ClusteredNetwork")
    return(network)
}