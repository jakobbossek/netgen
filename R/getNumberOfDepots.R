#' Returns the number of depots of a network.
#'
#' @param x [\code{Network}]\cr
#'   Network.
#' @return [\code{integer(1)}]
#' @export
getNumberOfDepots = function(x) {
    assertClass(x, "Network")
    nrow(x$depot.coordinates)
}