#' Return number of nodes of a network.
#'
#' @param x [\code{Network}]\cr
#'   Network.
#' @return [\code{integer(1)}]
#'   Number of nodes of the network.
#' @export
getNumberOfNodes = function(x) {
    assertClass(x, "Network")
    nrow(x$coordinates)
}
