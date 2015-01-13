#' Generate Network instance based on coordinates.
#'
#' @param coordinates [\code{matrix}]\cr
#'   Numeric matrix of 2D coordinates.
#' @return [\code{Network}]
#' @export
makeNetwork = function(coordinates) {
    assertDataFrame(coordinates)
    obj = makeS3Obj(
        coordinates = coordinates,
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
#' @return [\code{ClusteredNetwork}]
#' @export
makeClusteredNetwort = function(coordinates, membership) {
    assertDataFrame(coordinates)
    assertNumeric(membership, any.missing = FALSE)
    makeS3Obj(
        coordinates = coordinates,
        membership = membership,
        classes = c("Network", "ClusteredNetwork")
    )
}