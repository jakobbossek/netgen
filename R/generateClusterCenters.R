# Helper function for generation of cluster centres.
#
# The function generates n cluster centeres by generating a LHS design of size
# n in the unit-cube.
#
# @param n.cluster [\code{integer(1)}]\cr
#   Number of clusters.
# @param n.dims [\code{integer(1)}]\cr
#   Number of dimensions. Default is 2.
# @param scale.factor [\code{integer(1)}]\cr
#   Scale factor for stretching the space (for more clusters the inter-cluster
#   distance might be to low and therefore we might want to stretch the room).
# @return [\code{data.frame}]
#   Design as R data frame.
generateClusterCenters = function(
    n.cluster = 5L, n.dims = 2L,
    scale.factor = 1L,
    generator = lhs:::maximinLHS) {
    cc = as.data.frame(generator(n.cluster, 2L))
    # "stretch design"
    cc = cc * scale.factor
    colnames(cc) = paste("x", 1:2, sep = "")
    return(cc)
}