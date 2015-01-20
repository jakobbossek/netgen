#' Function for generation of clustered instances.
#'
#' This function generates clustered instances. It first generates n cluster
#' centeres via a latin hypercube design to ensure space-filling property.
#' It then then distributes points to the clusters according to
#' gaussian distributions using the cluster centers as the mean vector and
#' the distance to the nearest neighbor cluster as the variance.
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
#'   Minimal distance ratio of cluster centers to the bounding box. Default is 0,
#'   which means, that cluster centers can be located very close or even on the
#'   bounding box.
#' @param distribution.strategy [\code{character(1)}]\cr
#'   Define the strategy to distribute n.points on the n.cluster clusters. Default
#'   is 'equally.distributed', which is the only option at the moment.
#' @param cluster.centers [\code{matrix}]\cr
#'   Data frame of cluster centres of dimension \code{n.cluster} x \code{n.dim}. If
#'   this is set, cluster centres are not generated automatically.
#'   Default is \code{NULL}.
#' @param out.of.bounds.handling [\code{character(1)}]\cr
#'   Clusters are generated on base of a multivariate gaussian distribution with
#'   the cluster center as the mean vector. Possibly some of the points might fall
#'   out of bounds, i. e., get coordinates larger than upper or lower than lower.
#'   There are two strategies to force them to stick to the bounds:
#'   \describe{
#'     \item{\dQuote{reset}}{Set the violating coordinates to the bounds.}
#'     \item{\dQuote{mirror}}{Mirror the coordinates at the violated axis.}
#'   }
#'   Default is \dQuote{reset}.
#' @param ... [\code{any}]\cr
#'   Currently not used.
#' @return [\code{ClusteredNetwork}]
#'   Object of type \code{ClusteredNetwork}.
#' @examples
#'   x = generateClusteredInstance(n.points = 20L, n.cluster = 2L)
#'   y = generateClusteredInstance(n.points = 40L, n.cluster = 3L, n.depots = 2L)
#' @seealso \code{\link{generateRandomInstance}}
#' @export
generateClusteredInstance = function(n.cluster,
    n.points,
    n.dim = 2L,
    generator = lhs::maximinLHS,
    lower = 0,
    upper = 1,
    sigmas = NULL,
    n.depots = NULL,
    min.dist.to.bounds = 0,
    distribution.strategy = "equally.distributed",
    cluster.centers = NULL,
    out.of.bounds.handling = "reset",
    ...) {

    # do a load of sanity checks
    doSanityChecks(n.cluster, n.points, n.dim,
        generator, lower, upper, sigmas,
        n.depots, min.dist.to.bounds, distribution.strategy,
        cluster.centers, out.of.bounds.handling)

    if (is.null(cluster.centers)) {
        cluster.centers = generateClusterCenters(
            n.cluster, n.dim, generator,
            lower, upper, min.dist.to.bounds
        )
    }

    coordinates = list()
    # at the moment n.depos is only allowed to be in {1,2}
    depot.coordinates = matrix(NA, ncol = n.dim, nrow = 2L)

    # compute distances and ids to/of nearest neighbor cluster centers
    distances = computeDistancesToNearestClusterCenter(cluster.centers)

    #FIXME: make function out of this!
    if (!is.null(n.depots)) {
        depot.coordinates = buildDepots(n.depots, cluster.centers, distances)
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
    coordinates = forceToBounds(coordinates, out.of.bounds.handling, lower, upper)

    types = rep("customer", n.points)

    if (!is.null(n.depots)) {
        coordinates = rbind(depot.coordinates, coordinates)
        types = c(rep("depot", n.depots), types)
        membership = c(rep(0, n.depots), membership)
    }

    makeClusteredNetwork(
        coordinates = coordinates,
        membership = membership,
        types = types,
        lower = lower,
        upper = upper
    )
}


# Performs all the sanity checks for generateClusteredInstance.
#
# @params See params of generateClusteredInstance.
# @return Nothing
doSanityChecks = function(n.cluster,
    n.points,
    n.dim = 2L,
    generator = lhs::maximinLHS,
    lower = 0,
    upper = 1,
    sigmas = NULL,
    n.depots = NULL,
    min.dist.to.bounds = 0,
    distribution.strategy = "equally.distributed",
    cluster.centers = NULL,
    out.of.bounds.handling = "reset") {
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

    assertNumber(min.dist.to.bounds, lower = 0, upper = 1, na.ok = FALSE)
    assertChoice(distribution.strategy, choices = getPointDistributionStrategies())

    if (lower >= upper) {
        stop("Argument 'upper' must be greater than argument 'lower'.")
    }

    if (!is.null(cluster.centers)) {
        assertMatrix(cluster.centers, nrows = n.cluster, ncols = n.dim)
        # check if the coordinates are all in bounds
        for (i in seq(n.cluster)) {
            for (j in seq(n.dim)) {
                assertNumber(cluster.centers[i, j], lower = lower, upper = upper)
            }
        }
    }
}

# Select cluster centers to form depots.
#
# @param n.depots [integer(1)]
#   Number of depots. Currently at most 2.
# @param cluster.centers [\matrix]
#   Matrix containing the coordinates of the cluster centers.
# @param distances [list]
#   See return value of computeDistancesToNearestClusterCenters.
# @return [matrix]
#   Coordinates of the depots.
buildDepots = function(n.depots, cluster.centers, distances) {
    n.cluster = nrow(cluster.centers)
    # get first depot randomly
    depot.1.idx = sample(seq(n.cluster), 1L)
    depot.coordinates = matrix(cluster.centers[depot.1.idx, ], nrow = 1L)
    if (n.depots == 2L) {
        depot.2.idx = distances$max.distance.idx[depot.1.idx]
        depot.coordinates = rbind(depot.coordinates, matrix(cluster.centers[depot.2.idx, ], nrow = 1L))
    }
    return(depot.coordinates)
}

# Force coordinates out of bounds to bounds.
#
# @param coordinates [matrix | data.frame]
#   Coordinates.
# @param out.of.bounds.handling, [character(1)]
#   Strategy to handle out of bounds coordinates.
# @param lower [numeric(1)]
#   Lower bound for cube.
# @param upper [numeric(1)]
#   Upper bound for cube.
# @return [data.frame]
forceToBounds = function(coordinates, out.of.bounds.handling, lower = 0, upper = 1) {
    if (out.of.bounds.handling == "reset") {
        return(pmin(pmax(coordinates, lower), upper))
    } else if (out.of.bounds.handling == "mirror") {
        for (i in seq(ncol(coordinates))) {
            idx.lower = which(coordinates[, i] < lower)
            coordinates[idx.lower, i] = lower + abs(coordinates[idx.lower, i] - lower)
            idx.upper = which(coordinates[, i] > upper)
            coordinates[idx.upper, i] = upper - abs(coordinates[idx.upper, i] - upper)
        }
        return(coordinates)
    }
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
