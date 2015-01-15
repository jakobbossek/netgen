#' Generate Network instance based on coordinates.
#'
#' @param coordinates [\code{matrix}]\cr
#'   Numeric matrix of 2D coordinates.
#' @param types [\code{character}]\cr
#'   Vector of types for the nodes. For example to differentiate between
#'   depots and customers in the context of vehicle routing.
#' @return [\code{Network}]
#' @export
makeNetwork = function(coordinates, types = NULL, lower = 0, upper = 1) {
    assertMatrix(coordinates)
    if (!is.null(types))
        assertCharacter(types, any.missing = FALSE)
    obj = makeS3Obj(
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
#' @return [\code{ClusteredNetwork}]
#' @export
#FIXME: copy&paste crap!
makeClusteredNetwort = function(coordinates, membership, types = NULL, lower = 0, upper = 1) {
    assertMatrix(coordinates)
    assertNumeric(membership, any.missing = FALSE)
    if (!is.null(types))
        assertCharacter(types, any.missing = FALSE)
    makeS3Obj(
        coordinates = coordinates,
        membership = membership,
        types = types,
        lower = lower,
        upper = upper,
        classes = c("Network", "ClusteredNetwork")
    )
}