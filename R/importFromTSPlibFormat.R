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
    requirePackages("stringr", why = "netgen::importFromTSPlibFormat")
    assertFile(filename, access = "r")

    fh = file(filename, open = "r")
    on.exit(close(fh))
    lines = readLines(fh)
    n = length(lines)
    i = 1L
    network = list()

    readSpecificationPart = function(lines, i, envir = parent.frame()) {
        repeat {
            line = lines[i]
            #FIXME: this is not tolerant enough
            line.parts = strsplit(line, split = "[ ]*:[ ]*")[[1]]

            # we reached NODE_COORD_SECTION for example
            if (length(line.parts) != 2L) {
                break
                stop()
            }
            envir$network[[tolower(line.parts[1])]] = str_trim(line.parts[2])
            i = i + 1L
        }
        envir$i = i
    }

    readNodeCoordinates = function(lines, i, n) {
        coordinates = matrix(NA, ncol = 2, nrow = n)
        for (j in (i + 1):(i + n)) {
            line = lines[j]
            #FIXME: ugly as sin!
            coords = strsplit(line, split = "[ ][ ]*")[[1]]
            coords = coords[which(coords != "")]
            coords = coords[-1]
            coordinates[j - i, ] = as.numeric(coords)
        }
        return(coordinates)
    }

    readEdgeWeightsSection = function(network, lines, i, n) {
        ewt = network$edge_weight_type
        ewf = network$edge_weight_format
        if (is.null(ewt)) {
            stopf("Edge weight section found, but not edge weight type specified.")
        }
        if (ewt != "EXPLICIT") {
            stopf("Currently only explicit edge weight types are supported.")
        }
        if (is.null(ewf)) {
            stopf("Edge weight section is found, but no edge weight format given.")
        } else if (ewf == "FULL_MATRIX") {
            edge.weights = readExplicitEdgeWeights(lines, i, n)
        } else if (ewf == "UPPER_ROW") {
            edge.weights = readUpperRowWeights(lines, i, n)
        } else {
            stopf("Unsupported EDGE_WEIGHT_FORMAT.")
        }

        network$edge_weights = edge.weights
        return(network)
    }

    getCleanNumerics = function(line, delete.first = FALSE) {
        x = strsplit(line, "[ ][ ]*")[[1]]
        x = x[which(x != "")]
        if (delete.first) {
            x = x[-1L]
        }
        return(as.numeric(x))
    }

    readUpperRowWeights = function(lines, i, n) {
        distance.matrix = matrix(0, ncol = n, nrow = n)
        j = 1
        for (j in (i+1):(i + n - 1)) {
            distance.matrix[j - i, (j - i + 1):n] = getCleanNumerics(lines[j])
        }
        return(distance.matrix)
    }

    readExplicitEdgeWeights = function(lines, i, n) {
        distance.matrix = matrix(NA, ncol = n, nrow = n)
        for (j in (i + 1):(i + n)) {
            #FIXME: see above. Very ugly.
            distance.matrix[j - i, ] = getCleanNumerics(lines[j])
        }
        return(distance.matrix)
    }

    readClusterSection = function(lines, i, n) {
        return(as.integer(lines[(i + 1):(i + n)]))
    }

    readSpecificationPart(lines, i)
    #FIXME: we need to check the specification here
    n.points = as.integer(network$dimension)
    if (is.null(n.points)) {
        stopf("TSPlib format error: Mandatory DIMENSION specification is missing.")
    }
    while (lines[i] != "EOF" && lines[i] != "" && !is.na(lines[i])) {
        line = lines[i]
        if (line == "NODE_COORD_SECTION") {
            network[["coordinates"]] = readNodeCoordinates(lines, i, n.points)
            i = i + n.points + 1L
        }
        if (line == "DISPLAY_DATA_SECTION") {
            catf("DISPLAY_DATA_SECTION")
            network[["display_data"]] = readNodeCoordinates(lines, i, n.points)
            i = i + n.points + 1L
        }
        if (line == "CLUSTER_MEMBERSHIP_SECTION") {
            network[["membership"]] = readClusterSection(lines, i, n.points)
            i = i + n.points + 1L
        }
        if (line == "EDGE_WEIGHT_SECTION") {
            network = readEdgeWeightsSection(network, lines, i, n.points)
            i = i + n.points + 1L
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
            lower = if (!is.null(network$lower)) as.numeric(network$lower) else NULL,
            upper = if (!is.null(network$upper)) as.numeric(network$upper) else NULL,
            membership = network$membership
        )
    } else {
        network = makeNetwork(
            name = network$name,
            comment = network$comment,
            coordinates = network$coordinates,
            lower = if (!is.null(network$lower)) as.numeric(network$lower) else NULL,
            upper = if (!is.null(network$upper)) as.numeric(network$upper) else NULL
        )
    }
    return(network)
}