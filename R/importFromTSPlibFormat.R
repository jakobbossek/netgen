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

    network = list()

    readSpecificationPart = function(fh, network) {
        repeat {
            line = readLines(fh, 1L)
            #FIXME: this is not tolerant enough
            line.parts = strsplit(line, split = "[ ]*:[ ]*")[[1]]

            # we reached the SECTIONs part
            if (length(line.parts) != 2L) {
                pushBack(line, fh)
                break
            }
            network[[tolower(line.parts[1])]] = str_trim(line.parts[2])
        }
        return(network)
    }

    readNodeCoordinates = function(fh, n) {
        # <integer> <real> <real>
        raw.coordinates = scan(fh, nmax = 3 * n, quiet = TRUE)
        # get rid of node number (every third element)
        raw.coordinates = raw.coordinates[-seq(1, 3 * n, by = 3)]
        coordinates = matrix(raw.coordinates, ncol = 2L, byrow = TRUE)
        return(coordinates)
    }

    readEdgeWeightsSection = function(fh, network, n) {
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
            #FIXME: all the read function have the same signature
            # Construct a mapping from EDGE_WEIGHT_TYPE to the corresponding
            # function to make the code nicer
            edge.weights = readExplicitEdgeWeights(fh, n)
        } else if (ewf == "UPPER_ROW") {
            edge.weights = readUpperRowWeights(fh, n)
        } else if (ewf == "LOWER_ROW") {
            edge.weights = readLowerRowWeights(fh, n)
        } else if (ewf == "UPPER_DIAG_ROW") {
            edge.weights = readUpperDiagRowWeights(fh, n)
        } else if (ewf == "LOWER_DIAG_ROW") {
            edge.weights = readLowerDiagRowWeights(fh, n)
        } else {
            #FIXME: add support for the remaining types
            # UPPER_COL, LOWER_COL, UPPER_DIAG_COL and LOWER_DIAG_COL
            stopf("Unsupported EDGE_WEIGHT_FORMAT.")
        }
        network$edge_weights = edge.weights
        return(network)
    }

    readUpperDiagRowWeights = function(fh, n) {
        distance.matrix = matrix(0, ncol = n, nrow = n)
        distances = scan(fh, nmax = (n * (n + 1)) / 2, quiet = TRUE)
        i = 1L
        j = 1L
        for (k in 1:length(distances)) {
            distance.matrix[i, j] = distances[k]
            j = j + 1L
            if (j > n) {
                i = i + 1L
                j = i
            }
        }
        distance.matrix[lower.tri(distance.matrix)] = t(distance.matrix)[lower.tri(distance.matrix)]
        return(distance.matrix)
    }

    readLowerDiagRowWeights = function(fh, n) {
        distance.matrix = matrix(0, ncol = n, nrow = n)
        distances = scan(fh, nmax = (n * (n + 1)) / 2, quiet = TRUE)
        i = 1L
        j = 1L
        for (k in 1:length(distances)) {
            distance.matrix[i, j] = distances[k]
            j = j + 1L
            if (j > i) {
                i = i + 1L
                j = 1L
            }
        }
        distance.matrix[upper.tri(distance.matrix)] = t(distance.matrix)[upper.tri(distance.matrix)]
        return(distance.matrix)
    }

    readUpperRowWeights = function(fh, n) {
        distance.matrix = matrix(0, ncol = n, nrow = n)
        distances = scan(fh, nmax = (n * (n - 1)) / 2, quiet = TRUE)
        i = 1L
        j = 2L
        for (k in 1:length(distances)) {
            distance.matrix[i, j] = distances[k]
            j = j + 1L
            if (j > n) {
                i = i + 1L
                j = i + 1L
            }
        }
        distance.matrix[lower.tri(distance.matrix)] = t(distance.matrix)[lower.tri(distance.matrix)]
        return(distance.matrix)
    }

    readLowerRowWeights = function(fh, n) {
        distance.matrix = matrix(0, ncol = n, nrow = n)
        distances = scan(fh, nmax = (n * (n - 1)) / 2, quiet = TRUE)
        i = 2L
        j = 1L
        for (k in 1:length(distances)) {
            distance.matrix[i, j] = distances[k]
            j = j + 1L
            if (j > i) {
                i = i + 1L
                j = 1L
            }
        }
        distance.matrix[upper.tri(distance.matrix)] = t(distance.matrix)[upper.tri(distance.matrix)]
        return(distance.matrix)
    }

    readExplicitEdgeWeights = function(fh, n) {
        distances = scan(fh, nmax = n * n, quiet = TRUE)
        matrix(distances, ncol = n, nrow = n, byrow = TRUE)
    }

    readClusterSection = function(fh, n) {
        membership = as.integer(scan(fh, nmax = n), quiet = TRUE)
        return(membership)
    }

    network = readSpecificationPart(fh, network)
    #FIXME: we need to check the specification here
    n.points = as.integer(network$dimension)
    if (is.null(n.points)) {
        stopf("TSPlib format error: Mandatory DIMENSION specification is missing.")
    }
    line = str_trim(readLines(fh, 1L))
    while (length(line) > 0 && line != "EOF" && line != "" && !is.na(line)) {
        #print(line)
        if (line == "NODE_COORD_SECTION") {
            network[["coordinates"]] = readNodeCoordinates(fh, n.points)
        }
        if (line == "DISPLAY_DATA_SECTION") {
            network[["display_data"]] = readNodeCoordinates(fh, n.points)
        }
        if (line == "CLUSTER_MEMBERSHIP_SECTION") {
            network[["membership"]] = readClusterSection(fh, n.points)
        }
        if (line == "EDGE_WEIGHT_SECTION") {
            network = readEdgeWeightsSection(fh, network, n.points)
        }
        line = str_trim(readLines(fh, 1L))
        #print(line)
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
    #print(network)

    if (!is.null(network$membership)) {
        print(network)
        catf("Name %s, Comment %s", network$name, network$comment)
        network = makeClusteredNetwork(
            name = network$name,
            comment = network$comment,
            coordinates = network$coordinates,
            distance.matrix = network$edge_weights,
            lower = if (!is.null(network$lower)) as.numeric(network$lower) else NULL,
            upper = if (!is.null(network$upper)) as.numeric(network$upper) else NULL,
            membership = network$membership
        )
        print(str(network))
    } else {
        network = makeNetwork(
            name = network$name,
            comment = network$comment,
            coordinates = network$coordinates,
            distance.matrix = network$edge_weights,
            lower = if (!is.null(network$lower)) as.numeric(network$lower) else NULL,
            upper = if (!is.null(network$upper)) as.numeric(network$upper) else NULL
        )
    }
    return(network)
}