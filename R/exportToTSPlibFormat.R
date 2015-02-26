#' Exports a network to the TSPlib format.
#'
#' @note Currently we only support euclidean instances.
#'
#' @param x [\code{Network}]\cr
#'   Network to export.
#' @param filename [\code{character(1)}]\cr
#'   File name.
#' @param name [\code{character(1)} | \code{NULL}]\cr
#'   Character string describing the instance. Used for the NAME field in the
#'   TSPlib file format. Otherwise, the name of the instance is used. If the
#'   latter is \code{NULL}, this parameter is mandatory.
#' @param comment [\code{character(1)} | \code{NULL}]\cr
#'   Optional string with additional information about the instance. Used for
#'   the COMMENT field. If not provided the comment field of the instance is
#'   used. If the latter is \code{NULL}, no comment at all is saved.
#' @param use.extended.format [\code{logical(1)}]\cr
#'   Use the \dQuote{extended tsplib format} with additional information like cluster
#'   membership and bounds? Default is \code{TRUE}.
#' @param digits [\code{integer(1)}]\cr
#'   Round coordinates to this number of digits. Default is 2.
#' @return Nothing
#' @export
exportToTSPlibFormat = function(x, filename,
    name = NULL, comment = NULL,
    use.extended.format = TRUE,
    digits = 2L) {
    if (is.null(name) && is.null(x$name)) {
        stopf("Please provide a name for the instance via the 'name' parameter.")
    }
    if (hasDepots(x)) {
        stopf("Currently only instances without depots can be exported to the tsplib format.")
    }
    name = BBmisc::coalesce(name, x$name)
    comment = if (is.null(comment)) x$comment else comment
    coordinates = x$coordinates
    n = nrow(coordinates)
    n.cluster = getNumberOfClusters(x)
    out = paste("NAME : ", name, "\n", sep = "")
    if (!is.null(comment)) {
        for (com in comment) {
            out = paste(out, "COMMENT : ", com, "\n", sep = "")
        }
    }
    out = paste(out, "TYPE : TSP\n", sep = "")
    out = paste(out, "DIMENSION : ", n, "\n", sep = "")
    out = paste(out, "EDGE_WEIGHT_TYPE : EUC_2D\n", sep = "")
    if (use.extended.format) {
        out = paste(out, "LOWER : ", x$lower, "\n", sep = "")
        out = paste(out, "UPPER : ", x$upper, "\n", sep = "")
    }
    out = paste(out, "NODE_COORD_SECTION\n", sep = "")
    #FIXME: this works only for the 2d case
    for (i in seq(n)) {
        out = paste(out, i, " ", round(coordinates[i, 1], digits = digits), " ", round(coordinates[i, 2], digits = digits),
            if (i < n || n.cluster > 1L) "\n" else "", sep = "")
    }
    if (use.extended.format & n.cluster > 1L) {
        out = paste(out, "CLUSTER_MEMBERSHIP_SECTION\n", sep = "")
        membership = x$membership
        for (i in seq(n)) {
            out = paste(out, membership[i], if (i < n) "\n" else "", sep = "")
        }
    }
    #out = paste(out, "\nEOF", sep = "")
    write(x = out, file = filename)
}
