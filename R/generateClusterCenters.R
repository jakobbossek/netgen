# Helper function for generation of cluster centres.
#
# The function generates n cluster centeres by generating a LHS design of size
# n in the unit-cube.
#
# @param n.cluster [\code{integer(1)}]\cr
#   Number of clusters.
# @param n.dims [\code{integer(1)}]\cr
#   Number of dimensions. Default is 2.
# @param lower [\code{numeric(1)}]\cr
#   Lower bound for cube.
# @param upper [\code{numeric(1)}]\cr
#   Upper bound for cube.
# @param min.dist.to.bounds [\code{numeric(1)}]\cr
#   Minimal distance ratio of cluster centers to the bounding box. Default is 0.0,
#   which means, that cluster centers can be placed on the bounding box.
# @return [\code{matrix}]
#   Cluster center matrix. Each row contains the coordinates of one cluster center.
generateClusterCenters = function(
    n.cluster = 5L, n.dims = 2L,
    generator = lhs:::maximinLHS,
    lower = 0, upper = 1,
    min.dist.to.bounds = 0) {
    # FIXME: make use of min.dist.to.bounds
    cc = generator(n.cluster, n.dims)

    # apply minimal distance to bound by transformation
    cc = min.dist.to.bounds + (1 - 2 * min.dist.to.bounds) * cc

    # "stretch design"
    cc = cc * upper
    return(cc)
}