#' Get coordinates of depots.
#'
#' @param x [\code{Network}]\cr
#'   Network.
#' @return [\code{matrix}]
#' @export
getDepotCoordinates = function(x) {
    if (!hasDepots(x)) {
        stop("Object has no depots.")
    }
    x$depot.coordinates
}