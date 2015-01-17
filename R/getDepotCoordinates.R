#' Get coordinates of depots.
#'
#' @param x [\code{Network}]\cr
#'   Network.
#' @return [\code{matrix}]
#' @export
getDepotCoordinates = function(x) {
    if (is.null(x$types))
        stopf("This object has no depots.")
    depots.idx = which(x$types == "depot")
    return(x$coordinates[depots.idx, , drop = FALSE])
}