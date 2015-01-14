#' Return the number of clusters of a network.
#'
#' @param x [\code{Network}]\cr
#'   Network.
#' @return [\code{integer(1)}]
#'   Return the number of clusters.
#' @note For simple random instances this function always returns 1.
#' @export
getNumberOfClusters = function(x) {
    assertClass(x, "Network")
    n.cluster = 1L
    if (testClass(x, "ClusteredNetwork")) {
        n.cluster = length(unique(x$membership))
    }
    n.cluster
}