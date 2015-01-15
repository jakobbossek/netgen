#' Function for generation of clustered instances.
#'
#' Testing algorithms for the Vehicle-Routing-Problem and understanding their
#' behaviour requires investigation of differnet instances. This function
#' generate clustered instances. It first generates n cluster centeres via a
#' LHS method and then distributes points to the clusters according to
#' gaussian distributions.
#'
#' @param n.cluster [\code{integer(1)}]\cr
#'   Desired number of clusters.
#' @param n.points [\code{integer(1)}]\cr
#'   Number of points for the instance.
#' @param n.dim [\code{integer(1)}]\cr
#'   Number of dimensions. Most often you want to generate 2-dimensional instances
#'   in the euclidean plane. Thus 2 is the default setting.
#' @param generator [\code{function}]\cr
#'   Function which generates cluster centers. Default is \code{\link[lhs]{maximinLHS}}.
#' @param lower [\code{numeric(1)}]\cr
#'   Lower bound for cube.
#' @param upper [\code{numeric(1)}]\cr
#'   Upper bound for cube.
#' @param sigmas [\code{list} | \code{NULL}]\cr
#'   Unnamed list of length \code{n.cluster} containing a covariance matrix
#'   for each cluster. Default is \code{NULL}. In this case the covariance
#'   matrix is a diagonal matrix containing the distance to the nearest
#'   cluster center as diogonal elements.
#' @param n.depots [\code{integer(1)}]\cr
#'   Number of depots in instances for the Vehicle Routing Problem (VRP).
#'   Default is NULL, i. e., no depots. The proceeding is as follows:
#'   If \code{n.depots} is 1, a random cluster center is defined to be the depot.
#'   If \code{n.depots} is 2, the second depot has maximal distance to the first.
#'   By convention the depots are placed as the first nodes in the coordinates
#'   matrix.
#' @param min.dist.to.bounds [\code{numeric(1)}]\cr
#'   Minimal distance of cluster centers to the bounding box. Default is
#'   (\code{upper} - \code{lower}) / 20.
#' @param distribution.strategy [\code{character(1)}]\cr
#FIXME: itemize all the strategies here.
#'   Define the strategy to distribute n.points on the n.cluster clusters. Default
#'   is 'equally.distributed'. Also available is 'random.partition'.
#' @param cluster.centers [\code{matrix}]\cr
#'   Data frame of cluster centres of dimension \code{n.cluster} x \code{n.dim}. If
#'   this is set, cluster centres are not generated automatically.
#'   Default is \code{NULL}.
#' @param ... [\code{any}]\cr
#'   Not used yet.
#' @return [\code{ClusterInstance}]
#'   Object of type \code{ClusterInstance}.
#' @export
generateClusteredInstance = function(n.cluster,
    n.points,
    n.dim = 2L,
    generator = lhs::maximinLHS,
    lower = 0,
    upper = 1,
    sigmas = NULL,
    n.depots = NULL,
    min.dist.to.bounds = (upper - lower) / 20,
    distribution.strategy = "equally.distributed",
    cluster.centers = NULL,
    ...) {
    assertInteger(n.cluster, lower = 2L, len = 1L, any.missing = FALSE)
    assertInteger(n.dim, lower = 2L, len = 1L, any.missing = FALSE)
    assertFunction(generator)
    assertNumber(lower, lower = 0, finite = TRUE)
    assertNumber(upper, finite = TRUE)

    if (!is.null(sigmas)) {
        assertList(sigmas, len = n.cluster, types = c("matrix"))
        lapply(sigmas, function(sigma) {
            assertMatrix(sigma, mode = "numeric", nrows = n.dim, ncols = n.dim)
        })
    }

    if (!is.null(n.depots)) {
        #FIXME: think about upper limit here
        assertInteger(n.depots, len = 1L, lower = 1L, upper = 2L)
    }

    # FIXME: think about a reasonable upper bound for this
    assertNumber(min.dist.to.bounds, lower = 0, finite = TRUE)
    assertChoice(distribution.strategy, choices = getPointDistributionStrategies())

    if (lower >= upper) {
        stop("Argument 'upper' must be greater than argument 'lower'.")
    }

    if (is.null(cluster.centers)) {
        cluster.centers = generateClusterCenters(
            n.cluster, n.dim, generator,
            lower, upper, min.dist.to.bounds
        )
    } else {
        assertDataFrame(cluster.centers, nrows = n.cluster, ncols = n.dim)
    }

    coordinates = list()
    # at the moment n.depos is only allowed to be in {1,2}
    depot.coordinates = matrix(NA, ncol = n.dim, nrow = 2L)

    # compute distances and ids to/of nearest neighbor cluster centers
    distances = computeDistancesToNearestClusterCenter(cluster.centers)

    #FIXME: make function out of this!
    if (!is.null(n.depots)) {
        # get first depot randomly
        depot.1.idx = sample(seq(n.cluster), 1L)
        depot.coordinates = matrix(cluster.centers[depot.1.idx, ], nrow = 1L)
        if (n.depots == 2L) {
            depot.2.idx = distances$max.distance.idx[depot.1.idx]
            depot.coordinates = rbind(depot.coordinates, matrix(cluster.centers[depot.2.idx, ], nrow = 1L))
        }
    }

    # deterime number of elements for each cluster
    n.points.in.cluster = determineNumberOfPointsPerCluster(
        n.cluster, n.points,
        strategy = distribution.strategy
    )
    distances = distances$min.distance

    membership = list()
    for (i in 1:nrow(cluster.centers)) {
        # get distance to nearest cluster center and set variance appropritely
        distance.to.nearest.neighbor = distances[i]
        sigma = diag(rep(distance.to.nearest.neighbor, n.dim))
        if (!is.null(sigmas)) {
            sigma = sigmas[[i]]
        }
        the.coordinates= mvtnorm::rmvnorm(
            mean = as.numeric(cluster.centers[i, ]),
            n = n.points.in.cluster[i],
            sigma = sigma
        )
        # the.coordinates = as.data.frame(the.coordinates)
        # colnames(the.coordinates) = paste("x", 1:2, sep = "")
        membership[[i]] = rep(i, n.points.in.cluster[i])
        coordinates[[i]] = the.coordinates
    }

    coordinates = do.call(rbind, coordinates)
    membership = do.call(c, membership)
    coordinates = forceToBounds(coordinates, lower, upper)

    types = rep("customer", n.points)

    # FIXME: these lines are ugly as sin!
    if (!is.null(n.depots)) {
        # print(names(depot.coordinates))
        # print(names(coordinates))
        # stop()
        coordinates = rbind(depot.coordinates, coordinates)
        types = c(rep("depot", n.depots), types)
        membership = c(rep(0, n.depots), membership)
    }
    #rownames(coordinates) = NULL

    makeClusteredNetwort(
        coordinates = coordinates,
        membership = membership,
        types = types,
        lower = lower,
        upper = upper
    )
}

# Force coordinates out of bounds to bounds.
#
# @param coordinates [matrix | data.frame]
#    Coordinates.
# @param lower [numeric(1)]
#   Lower bound for cube.
# @param upper [numeric(1)]
#   Upper bound for cube.
# @return [data.frame]
forceToBounds = function(coordinates, lower = 0, upper = 1) {
    # getting warnings and NAs here, if I do this without the conversions
    pmin(pmax(coordinates, lower), upper)
}

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
    colnames(res) = c("x1", "x2")

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

#' Get network information as a character string.
#'
#' @param x [\code{Network}]\cr
#'   Network.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return [\code{character(1)}]
as.character.Network = function(x, ...)   {
    n.points = getNumberOfNodes(x)
    n.clusters = getNumberOfClusters(x)

    char = paste("#Nodes:", n.points)
    if (n.clusters > 1L)
        char = paste(char, ", #Clusters:", n.clusters)
    if (hasAttributes(x, "morphed"))
        char = paste(char, "\n(Morphing coefficient ", attr(x, "morphing.grade"), ")", sep = "")
    char
}
