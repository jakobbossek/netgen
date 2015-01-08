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
#' @param lower [\code{numeric(1)}]\cr
#'   Lower bound for cube.
#' @param upper [\code{numeric(1)}]\cr
#'   Upper bound for cube.
#' @param generator [\code{function}]\cr
#'   Function which generates cluster centers. Default is \code{\link[lhs]{maximinLHS}}.
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
    ...) {
    assertInteger(n.cluster, lower = 2L, len = 1L, any.missing = FALSE)
    assertInteger(n.dim, lower = 2L, len = 1L, any.missing = FALSE)
    assertFunction(generator)
    assertNumber(lower, lower = 0)
    assertNumber(upper)

    if (lower >= upper) {
        stop("Argument 'upper' must be greater than argument 'lower'.")
    }

    cluster.centers = generateClusterCenters(n.cluster, n.dim, generator, lower, upper)
    the.cluster = list()

    distances = computeDistancesToNearestClusterCenter(cluster.centers)$min.distance

    # deterime number of elements for each cluster
    # FIXME: allow setting the number of points for each cluster seperately via another parameter
    n.points.in.cluster = determineNumberOfPointsPerCluster(n.cluster, n.points)

    for (i in 1:nrow(cluster.centers)) {
        # get distance to nearest cluster center and set variance appropritely
        distance.to.nearest.neighbor = distances[i]
        vvar = distance.to.nearest.neighbor #FIXME: magic number
        tmp = mvtnorm::rmvnorm(mean = as.numeric(cluster.centers[i, ]), n = n.points.in.cluster[i], sigma = diag(rep(vvar, n.dim)))
        tmp = as.data.frame(tmp)
        colnames(tmp) = paste("x", 1:2, sep = "")
        tmp$cluster = i
        the.cluster[[i]] = tmp
    }

    the.cluster = do.call(rbind, the.cluster)

    # not a particluar great software design decision, but it works for now
    membership = as.numeric(the.cluster$cluster)
    the.cluster$cluster = NULL

    return(makeS3Obj(
        coordinates = the.cluster,
        membership = membership,
        classes = "ClusterInstance"
    ))
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
as.data.frame.ClusterInstance = function(x, row.names = NULL, optional = FALSE, include.membership = TRUE, ...) {
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
#' visualize 2-dimensional (clustered) data sets in the euclidean plane.
#'
#' @param object [\code{ClusterInstance}]\cr
#'   Instance to visualize.
#' @param ... [any]\cr
#'   Currently not used.
#' @return [\code{\link[ggplot2]{ggplot}}]
#'   ggplot2 object.
#' @export
autoplot.ClusterInstance = function(object, ...) {
    df = as.data.frame(object, include.membership = TRUE)
    df$membership = as.factor(df$membership)
    pl = ggplot(data = df, mapping = aes_string(x = "x1", y = "x2"))
    pl = pl + geom_point(aes_string(colour = "membership"))
    title = paste("#Nodes:", nrow(df), ", #Clusters:", length(unique(df$membership)))
    pl = pl + ggtitle(title)
    pl = pl + theme(legend.position = "top")
    #FIXME: probably it would be nice to save bounds in ClusterInstance
    #pl = pl + xlim(c(0, 1)) + ylim(c(0, 1))
    return(pl)
}