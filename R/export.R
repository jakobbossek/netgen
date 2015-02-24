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