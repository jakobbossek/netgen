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
# @return [\code{data.frame}]
#   Cluster center coordinates rowwise in a data frame.
generateClusterCenters = function(
    n.cluster = 5L, n.dims = 2L,
    generator = lhs:::maximinLHS,
    lower = 0, upper = 1) {
    cc = as.data.frame(generator(n.cluster, n.dims))
    # "stretch design"
    cc = cc * upper
    colnames(cc) = paste("x", 1:2, sep = "")
    return(cc)
}