#' Convert (clustered) instance to data frame.
#'
#' @param x [\code{Network}]\cr
#'   Cluster instance.
#' @param row.names [\code{character}]\cr
#'   Row names for the result. Default is \code{NULL}.
#' @param optional [any]\cr
#'   Currently not used.
#' @param include.extras [\code{logical(1)}]\cr
#'   Include additional information like membership, types as seperate columns?
#'   Default is \code{TRUE}.
#' @param ... [any]\cr
#'   Currently not used.
#' @export
as.data.frame.Network = function(x,
    row.names = NULL,
    optional = FALSE,
    include.extras = TRUE,
    ...) {
    n = nrow(x$coordinates)
    res = as.data.frame(x$coordinates)
    colnames(res) = paste("x", seq(ncol(res)), sep = "")

    assertFlag(include.extras)
    if (!is.null(row.names)) {
        assertCharacter(row.names, len = n, any.missing = FALSE)
    }

    if (include.extras) {
        res$types = x$types
        if (!is.null(x$membership)) {
            res$membership = x$membership
        }
    }
    as.data.frame(res, row.names = row.names, optional = optional, ...)
}