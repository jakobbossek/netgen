export = function(x, file.name) {
    assertClass(x, "Network")
    assertCharacter(file.name, len = 1L, any.missing = FALSE)
    write.table(as.data.frame(x), file = file.name, quote = FALSE, row.names = FALSE)
}

#FIXME: this way we do not save lower, upper bounds, request times, name and comment.
# better export to tsplib format?
import = function(file.name) {
    assertCharacter(file.name, len = 1L, any.missing = FALSE)
    df = read.table(file.name, header = TRUE, sep = " ")
    coords.idx = setdiff(names(df), c("types", "membership"))
    coordinates = as.matrix(df[, coords.idx])
    depot.coordinates = NULL
    colnames(coordinates) = NULL

    types = df$types
    if (is.null(types)) {
        stopf("Cannot read instance. Probably it is not stored in the netgen format.")
    }

    depot.idx = NULL
    if ("depot" %in% types) {
        depot.idx = which(types == "depot")
        depot.coordinates = coordinates[depot.idx, ]
        coordinates = coordinates[-depot.idx, ]
        types = types[-depot.idx]
    }

    has.clusters = !is.null(df$membership)
    if (!has.clusters) {
        return(makeNetwork(
            name = file.name, comment = paste("Imported from", file.name),
            coordinates = coordinates, depot.coordinates = depot.coordinates)
        )
    }

    membership = df$membership
    if (!is.null(depot.idx)) {
        membership = membership[-depot.idx]
    }

    makeClusteredNetwork(
        name = file.name, comment = paste("Imported from", file.name),
        coordinates = coordinates, depot.coordinates = depot.coordinates,
        membership = membership
    )
}

# Exports a network to the TSPlib format.
#
# @note: Currently we only support euclidean instances.
#
# @param x [Network]
#   Network to export.
# @param filename [character(1)]
#   File name.
# @param name [character(1) | NULL]
#   Character string describing the instance. Used for the NAME field in the
#   TSPlib file format. Otherwise, the name of the instance is used.
# @param comment [character(1) | NULL]
#   Optional string with additional information about the instance. Used for
#   the COMMENT field. If not provided the comment field of the instance is
#   used. If the latter is \code{NULL}, no comment at all is saved.
# @param digits [integer(1)]
#   Round coordinates to this number of digits.
# @return Nothing
exportNetworkToTSPlibFormat = function(x, filename, name = NULL, comment = NULL, digits = 2L) {
    name = if (is.null(name)) x$name else name
    name = if (is.null(name)) filename else name
    #FIXME: we should encode all the additional stuff to the comment section
    comment = if (is.null(comment)) x$comment else comment
    comment = sprintf("cl=%i;", getNumberOfClusters(x))
    coordinates = x$coordinates
    n = nrow(coordinates)
    n.cluster = getNumberOfClusters(x)
    out = paste("NAME : ", name, "\n", sep = "")
    if (!is.null(comment)) {
        out = paste(out, "COMMENT : ", comment, "\n", sep = "")
    }
    #FIXME: VRP
    out = paste(out, "TYPE : TSP\n", sep = "")
    out = paste(out, "DIMENSION : ", n, "\n", sep = "")
    out = paste(out, "EDGE_WEIGHT_TYPE : EUC_2D\n", sep = "")
    out = paste(out, "NODE_COORD_SECTION\n", sep = "")
    #FIXME: this works only for the 2d case
    for (i in seq(n)) {
        out = paste(out, i, " ", round(coordinates[i, 1], digits = digits), " ", round(coordinates[i, 2], digits = digits),
            if (i < n) "\n" else "", sep = "")
    }
    write(x = out, file = filename)
}
