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
#' @param n.customers [\code{integer(1)}]\cr
#'   Number of points for the instance.
#' @param n.dim [\code{integer(1)}]\cr
#'   Number of dimensions. Most often you want to generate 2-dimensional instances
#'   in the euclidean plane. Thus 2 is the default setting.
#' @param scale.factor [\code{numeric(1)}]\cr
#'   Instances are generaded in the \code{n.dims}-dimensional
#'   [0,1] hypercube. The numeric scaling parameter can be used to stretch dimensions
#'   to arbitrary dimensions.
#' @param ... [\code{any}]\cr
#'   Not used yet.
#' @return [\code{data.frame}]
#'   Data frame of the coordinates of the cluster points.
#' @export
generateClusteredInstance = function(n.cluster, n.customers, n.dim = 2L, scale.factor = 1L, ...) {
    cluster.centers = generateClusterCenters(n.cluster, n.dim, scale.factor)
    customers = list()

    distances = computeDistancesToNearestClusterCenter(cluster.centers)$min.distance

    # deterime number of elements for each cluster
    # FIXME: allow setting the number of points for each cluster seperately via another parameter
    n.customers.in.cluster = determineNumberOfPointsPerCluster(n.cluster, n.customers)

    for (i in 1:nrow(cluster.centers)) {
        # get distance to nearest cluster center and set variance appropritely
        distance.to.nearest.neighbor = distances[i]
        vvar = distance.to.nearest.neighbor / 500 #FIXME: magic number
        tmp = mvtnorm::rmvnorm(mean = as.numeric(cluster.centers[i, ]), n = n.customers.in.cluster[i], sigma = diag(rep(vvar, n.dim)))
        tmp = as.data.frame(tmp)
        colnames(tmp) = paste("x", 1:2, sep = "")
        tmp$cluster = i
        customers[[i]] = tmp
    }
    customers = do.call(rbind, customers)
    return(customers)
}