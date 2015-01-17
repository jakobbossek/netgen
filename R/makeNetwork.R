#' Generate Network instance based on coordinates.
#'
#' @param coordinates [\code{matrix}]\cr
#'   Numeric matrix of 2D coordinates.
#' @param types [\code{character}]\cr
#'   Vector of types for the nodes. For example to differentiate between
#'   depots and customers in the context of vehicle routing.
#' @param lower [\code{numeric(1)}]\cr
#'   Lower bound of bounding box.
#' @param upper [\code{numeric(1)}]\cr
#'   Upper bound of bounding box.
#' @return [\code{Network}]
#' @export
makeNetwork = function(coordinates, types = NULL, lower = 0, upper = 1) {
    assertMatrix(coordinates)
    if (!is.null(types))
        assertCharacter(types, any.missing = FALSE)
    makeS3Obj(
        coordinates = coordinates,
        types = types,
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
#' @param membership [\code{numeric}]\cr
#'   Vector of memberships.
#' @param types [\code{character}]\cr
#'   Vector of types for the nodes. For example to differentiate between
#'   depots and customers in the context of vehicle routing.
#' @param lower [\code{numeric(1)}]\cr
#'   Lower bound of bounding box.
#' @param upper [\code{numeric(1)}]\cr
#'   Upper bound of bounding box.
#' @return [\code{ClusteredNetwork}]
#' @export
makeClusteredNetwork = function(coordinates, membership, types = NULL, lower = 0, upper = 1) {
    network = makeNetwork(
        coordinates = coordinates,
        types = types,
        lower = lower,
        upper = upper
    )
    assertNumeric(membership, any.missing = FALSE)
    network$membership = membership
    network = addClasses(network, "ClusteredNetwork")
    return(network)
}