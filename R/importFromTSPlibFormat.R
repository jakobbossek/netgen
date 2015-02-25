#' Import network from (extended) TSPlib format.
#'
#' @note Currently only the import of symmetric TSP instances is possible.
#'
#' @param filename [\code{character(1)}]\cr
#'   Path to TSPlib file.
#' @return [\code{Network}]
#'   Network object.
#' @export
importFromTSPlibFormat = function(filename) {
    assertFile(filename, access = "r")

    fh = file(filename, open = "r")
    on.exit(close(fh))
    lines = readLines(fh)
    n = length(lines)
    i = 1L
    network = list()

    readSpecificationPart = function(lines, envir = parent.frame()) {
        i = envir$i
        repeat {
            line = lines[i]
            #FIXME: this is not tolerant enough
            line.parts = strsplit(line, split = " : ", fixed = TRUE)[[1]]

            # we reached NODE_COORD_SECTION for example
            if (length(line.parts) != 2L) {
                break
            }
            envir$network[[tolower(line.parts[1])]] = line.parts[2]
            i = i + 1L
        }
        envir$i = i
    }

    readNodeCoordinates = function(lines, n, envir = parent.frame()) {
        i = envir$i
        coordinates = matrix(NA, ncol = 2, nrow = n)
        for (j in (i + 1):(i + n)) {
            line = lines[j]
            coords = strsplit(line, split = " ", fixed = TRUE)[[1]]
            coords = coords[-1]
            coordinates[j - i, ] = as.numeric(coords)
        }
        envir$i = i + n + 1L
        return(coordinates)
    }

    readEdgeWeightsSection = function(lines, n, envir = parent.frame()) {

    }

    readClusterSection = function(lines, n, envir = parent.frame()) {
        i = envir$i
        envir$i = i + n + 1L
        return(as.integer(lines[(i + 1):(i + n)]))
    }

    readSpecificationPart(lines)
    #FIXME: we need to check the specification here
    n.points = as.integer(network$dimension)
    if (is.null(n.points)) {
        stopf("TSPlib format error: Mandatory DIMENSION specification is missing.")
    }
    while (lines[i] != "EOF" && lines[i] != "" && !is.na(lines[i])) {
        line = lines[i]
        if (line == "NODE_COORD_SECTION") {
            network[["coordinates"]] = readNodeCoordinates(lines, n.points)
        }
        if (line == "DISPLAY_DATA_SECTION") {
            network[["display_data"]] = readNodeCoordinates(lines, n.points)
        }
        if (line == "CLUSTER_MEMBERSHIP_SECTION") {
            network[["membership"]] = readClusterSection(lines, n.points)
        }
        if (line == "EDGE_WEIGHT_SECTION") {
            network[["edge_weights"]] = readEdgeWeightsSection(lines, n.points)
        }
    }

    getNetworkCoordinates = function(network) {
        if (!is.null(network$coordinates)) {
            return(network$coordinates)
        }
        if (!is.null(network$display_data_type)) {
            if (network$display_data_type == "NO_DISPLAY") {
                stopf("There are no coordinates available for the instance '%s'.", network$name)
            }
        }
        if (!is.null(network$display_data)) {
            return(network$display_data)
        }
        if (!is.null(network$edge_weights)) {
            #FIXME: implement this
            stopf("Guessing coordinates since no explicit information is given.")
        }
        stopf("No coordinates available for the given instance '%s'.", network$name)
    }

    # postprocessing
    network$coordinates = getNetworkCoordinates(network)

    #FIXME: this is ugly as sin! Why do we have makeClusteredNetwork?
    # It is simply makeNetwork with additional membership stuff.
    # Refactor this! Make membership an optional field of makeNetwork
    # and mark makeClusteredNetwork as deprecated.
    if (!is.null(network$membership)) {
        network = makeClusteredNetwork(
            name = network$name,
            comment = network$comment,
            coordinates = network$coordinates,
            lower = network$lower,
            upper = network$upper,
            membership = network$membership
        )
    } else {
        network = makeNetwork(
            name = network$name,
            comment = network$comment,
            coordinates = network$coordinates,
            lower = network$lower,
            upper = network$upper
        )
    }
    return(network)
}