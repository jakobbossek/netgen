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

    assertChoice(distribution.strategy, choices = getPointDistributionStrategies())

    if (lower >= upper) {
        stop("Argument 'upper' must be greater than argument 'lower'.")
    }

    if (is.null(cluster.centers)) {
        cluster.centers = generateClusterCenters(n.cluster, n.dim, generator, lower, upper)
    } else {
        assertDataFrame(cluster.centers, nrows = n.cluster, ncols = n.dim)
    }

    the.cluster = list()

    distances = computeDistancesToNearestClusterCenter(cluster.centers)$min.distance

    # deterime number of elements for each cluster
    n.points.in.cluster = determineNumberOfPointsPerCluster(n.cluster, n.points, strategy = distribution.strategy)


    print(cluster.centers)
    print(n.points.in.cluster)
    for (i in 1:nrow(cluster.centers)) {
        # get distance to nearest cluster center and set variance appropritely
        distance.to.nearest.neighbor = distances[i]
        sigma = diag(rep(distance.to.nearest.neighbor, n.dim))
        if (!is.null(sigmas)) {
            sigma = sigmas[[i]]
        }
        tmp = mvtnorm::rmvnorm(mean = as.numeric(cluster.centers[i, ]), n = n.points.in.cluster[i], sigma = sigma)
        tmp = as.data.frame(tmp)
        colnames(tmp) = paste("x", 1:2, sep = "")
        tmp$cluster = i
        the.cluster[[i]] = tmp
    }

    the.cluster = do.call(rbind, the.cluster)

    # not a particluar great software design decision, but it works for now
    membership = as.numeric(the.cluster$cluster)
    the.cluster$cluster = NULL

    makeClusteredNetwort(coordinates = the.cluster, membership = membership)
}

#' Convert cluster instance to data frame.
#'
#' @param x [\code{ClusterInstance}]\cr
#'   Cluster instance.
#' @param row.names [\code{character}]\cr
#'   Row names for the result. Default is \code{NULL}.
#' @param optional [any]\cr
#'   Currently not used.
#' @param include.membership [\code{logical(1)}]\cr
#'   Include the membership as a seperate column? Default is \code{TRUE}.
#' @param ... [any]\cr
#'   Currently not used.
#' @export
as.data.frame.ClusteredNetwork = function(x, row.names = NULL, optional = FALSE, include.membership = TRUE, ...) {
    n = nrow(x$coordinates)
    res = x$coordinates

    assertFlag(include.membership)
    if (!is.null(row.names)) {
        assertCharacter(row.names, len = n, any.missing = FALSE)
    }

    if (include.membership) {
        res$membership = x$membership
    }
    as.data.frame(res, row.names = row.names, optional = optional, ...)
}

#' Autoplot function.
#'
#' Generates a \code{\link[ggplot2]{ggplot}} object. Nice possibility to
#' visualize 2-dimensional (clustered) networks in the euclidean plane.
#'
#' @param object [\code{ClusterInstance}]\cr
#'   Instance to visualize.
#' @param ... [any]\cr
#'   Currently not used.
#' @return [\code{\link[ggplot2]{ggplot}}]
#'   ggplot2 object.
#' @export
autoplot.Network = function(object, ...) {
    if (testClass(object, "ClusteredNetwork")) {
        df = as.data.frame(object, include.membership = TRUE)
        df$membership = as.factor(df$membership)
    } else {
        df = object$coordinates
    }
    pl = ggplot(data = df, mapping = aes_string(x = "x1", y = "x2"))
    if (!is.null(df$membership)) {
        pl = pl + geom_point(aes_string(colour = "membership"))
        title = paste("#Nodes:", nrow(df), ", #Clusters:", length(unique(df$membership)))
    } else {
        pl = pl + geom_point(colour = "tomato")
        title = paste("#Nodes:", nrow(df))
    }
    pl = pl + ggtitle(title)
    pl = pl + theme(legend.position = "top")
    #FIXME: probably it would be nice to save bounds in ClusterInstance
    #pl = pl + xlim(c(0, 1)) + ylim(c(0, 1))
    return(pl)
}