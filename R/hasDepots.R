#' Check if network has depots.
#'
#' @param x [\code{Network}]\cr
#'   Network.
#' @return [\code{logical(1)}]
#' @export
hasDepots = function(x) {
    if (is.null(x$types))
        return(FALSE)
    any(x$types == "depot")
}
