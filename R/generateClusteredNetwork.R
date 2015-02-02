#' Function for generation of clustered instances.
#'
#' This function generates clustered instances. It first generates n cluster
#' centeres via a latin hypercube design to ensure space-filling property.
#' It then then distributes points to the clusters according to
#' gaussian distributions using the cluster centers as the mean vector and
#' the distance to the nearest neighbor cluster as the variance.
#'
#' @param n.cluster [\code{integer(1)}]\cr
#'   Desired number of clusters. This is ignored if \code{cluster.centers} is
#'   provided.
#' @param n.points [\code{integer(1)}]\cr
#'   Number of points for the instance.
#' @param n.dim [\code{integer(1)}]\cr
#'   Number of dimensions. Most often you want to generate 2-dimensional instances
#'   in the euclidean plane. Thus \code{2L} is the default setting.
#' @param generator [\code{function}]\cr
#'   Function which generates cluster centers. Default is \code{\link[lhs]{maximinLHS}}.
#' @param lower [\code{numeric(1)}]\cr
#'   Lower box constaint for cube. Default is \code{0}.
#' @param upper [\code{numeric(1)}]\cr
#'   Upper box constaint for cube. Default is \code{100}.
#' @param sigmas [\code{list} | \code{NULL}]\cr
#'   Unnamed list of length \code{n.cluster} containing a covariance matrix
#'   for each cluster. Default is \code{NULL}. In this case the covariance
#'   matrix is a diagonal matrix containing the distance to the nearest
#'   cluster center as diagonal elements.
#' @param n.depots [\code{integer(1)}]\cr
#'   Number of depots in instances for the Vehicle Routing Problem (VRP).
#'   Default is \code{NULL}, i. e., no depots. The proceeding is as follows:
#'   If \code{n.depots} is \code{1L}, a random cluster center is defined to be the depot.
#'   If \code{n.depots} is \code{2L}, the second depot has maximal distance to the first.
#'   By convention the depots are placed as the first nodes in the coordinates
#'   matrix. At the moment at most two depots are possible.
#' @param distribution.strategy [\code{character(1)}]\cr
#'   Define the strategy to distribute \code{n.points} on the \code{n.cluster} clusters.
#'   Default is \dQuote{equally.distributed}, which is the only option at the moment.
#' @param cluster.centers [\code{matrix}]\cr
#'   Data frame of cluster centres of dimension \code{n.cluster} x \code{n.dim}. If
#'   this is set, cluster centres are not generated automatically.
#'   Default is \code{NULL}.
#' @param out.of.bounds.handling [\code{character(1)}]\cr
#'   Clusters are generated on base of a multivariate gaussian distribution with
#'   the cluster center as the mean vector. Possibly some of the points might fall
#'   out of bounds, i. e., get coordinates larger than \code{upper} or lower than
#'   \code{lower}. There are two strategies to force them to stick to the bounds:
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
#'   x = generateClusteredNetwork(n.points = 20L, n.cluster = 2L)
#'   y = generateClusteredNetwork(n.points = 40L, n.cluster = 3L, n.depots = 2L)
#' @seealso \code{\link{generateRandomNetwork}}
#' @export
generateClusteredNetwork = function(n.cluster,
    n.points,
    n.dim = 2L,
    generator = lhs::maximinLHS,
    lower = 0,
    upper = 100,
    sigmas = NULL,
    n.depots = NULL,
    distribution.strategy = "equally.distributed",
    cluster.centers = NULL,
    out.of.bounds.handling = "reset",
    ...) {

    # do a load of sanity checks
    doSanityChecks(n.cluster, n.points, n.dim,
        generator, lower, upper, sigmas,
        n.depots, distribution.strategy,
        cluster.centers, out.of.bounds.handling)

    if (is.null(cluster.centers)) {
        cluster.centers = generateClusterCenters(
            n.cluster, n.dim, generator,
            lower, upper
        )
    }
    n.cluster = nrow(cluster.centers)

    coordinates = list()
    depot.coordinates = NULL

    # compute distances and ids to/of nearest neighbor cluster centers
    distances = computeDistancesToNearestClusterCenter(cluster.centers)

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
        membership[[i]] = rep(i, n.points.in.cluster[i])
        coordinates[[i]] = the.coordinates
    }

    coordinates = do.call(rbind, coordinates)
    membership = do.call(c, membership)
    coordinates = forceToBounds(coordinates, out.of.bounds.handling, lower, upper)

    makeClusteredNetwork(
        coordinates = coordinates,
        depot.coordinates = depot.coordinates,
        membership = membership,
        lower = lower,
        upper = upper
    )
}

# Performs all the sanity checks for generateClusteredNetwork.
#
# @params See params of generateClusteredNetwork.
# @return Nothing
doSanityChecks = function(n.cluster,
    n.points,
    n.dim = 2L,
    generator = lhs::maximinLHS,
    lower = 0,
    upper = 1,
    sigmas = NULL,
    n.depots = NULL,
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
        assertInteger(n.depots, len = 1L, lower = 1L, upper = 2L)
    }

    assertChoice(distribution.strategy, choices = getPointDistributionStrategies())

    if (lower >= upper) {
        stop("Argument 'upper' must be greater than argument 'lower'.")
    }

    if (!is.null(cluster.centers)) {
        assertMatrix(cluster.centers, ncols = n.dim)
        # check if the coordinates are all in bounds
        for (i in seq(nrow(cluster.centers))) {
            for (j in seq(n.dim)) {
                assertNumber(cluster.centers[i, j], lower = lower, upper = upper)
            }
        }
    }
}
