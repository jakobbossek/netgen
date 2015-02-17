#' Generate network based on coordinates.
#'
#' @param coordinates [\code{matrix}]\cr
#'   Numeric matrix of 2D coordinates.
#' @param distance.matrix [\code{matrix}]\cr
#'   Optional distance matrix.
#' @param name [\code{character(1)}]\cr
#'   Optional name of the network.
#' @param comment [\code{character(1)}]\cr
#'   Optional additional comments on instance.
#' @param depot.coordinates [\code{matrix} | NULL]\cr
#'   Numeric matrix of 2D coordinates of depots. Default is \code{NULL}, which
#'   means no depots at all.
#' @param lower [\code{numeric(1)}]\cr
#'   Lower box constraint of cube.
#' @param upper [\code{numeric(1)}]\cr
#'   Upper box constraint of cube.
#' @return [\code{Network}]
#' @export
makeNetwork = function(coordinates,
    distance.matrix = NULL,
    name = NULL, comment = NULL,
    depot.coordinates = NULL, lower = NULL, upper = NULL) {
    assertMatrix(coordinates)
    !is.null(name) && assertCharacter(name, len = 1L, any.missing = FALSE)
    !is.null(comment) && assertCharacter(comment, len = 1L, any.missing = FALSE)
    !is.null(depot.coordinates) && assertMatrix(depot.coordinates)
    !is.null(distance.matrix) && assertMatrix(distance.matrix)

    #FIXME: do we need this at all?
    if (is.null(lower) || is.null(upper)) {
        lower = min(coordinates)
        upper = max(coordinates)
    }

    if (is.null(distance.matrix)) {
        distance.matrix = as.matrix(dist(coordinates))
    }

    makeS3Obj(
        coordinates = coordinates,
        distance.matrix = distance.matrix,
        depot.coordinates = depot.coordinates,
        name = name,
        comment = comment,
        lower = lower,
        upper = upper,
        classes = "Network"
    )
}

#' Generates clustered network.
#'
#' @param coordinates [\code{matrix}]\cr
#'   Numeric matrix of 2D coordinates.
#' @param depot.coordinates [\code{matrix} | NULL]\cr
#'   Numeric matrix of 2D coordinates of depots. Default is \code{NULL}, which
#'   means no depots at all.
#' @param membership [\code{numeric}]\cr
#'   Vector of memberships.
#' @param lower [\code{numeric(1)}]\cr
#'   Lower box constraint of cube. Default is 0.
#' @param upper [\code{numeric(1)}]\cr
#'   Upper box constraint of cube. Default is 100.
#' @return [\code{ClusteredNetwork}]
#' @export
makeClusteredNetwork = function(coordinates, depot.coordinates = NULL, membership,
    lower = NULL, upper = NULL) {
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
