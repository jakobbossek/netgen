#' Check if network has depots.
#'
#' @param x [\code{Network}]\cr
#'   Network.
#' @return [\code{logical(1)}]
#' @export
hasDepots = function(x) {
    !is.null(x$depot.coordinates)
}
