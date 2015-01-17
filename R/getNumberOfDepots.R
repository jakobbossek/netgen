#' Returns the number of depots of a network.
#'
#' @param x [\code{Network}]\cr
#'   Network.
#' @return [\code{integer(1)}]
#' @export
getNumberOfDepots = function(x) {
    if (is.null(x$types))
        return(0)
    return(sum(x$types == "depot"))
}