# Helper function for distributing desired number of points
# more or less equally to the different clusters.
#
# @param n.cluster [\code{integer(1)}]\cr
#   Desired number of clusters.
# @param n.customers [\code{integer(1)}]\cr
#   Number of points for the instance.
# @return [\code{integer}]
#   Vector of length \code{length(n.cluster)} containing the number of
#   points assigned to this cluster.
determineNumberOfPointsPerCluster = function(n.cluster, n.customers) {
    # distribute equally over the clusters
    n = floor(n.customers / n.cluster)
    n.customers.in.cluster = rep(n, n.cluster)
    # n * n.cluster might be lower than n.customers. Add the remaining points to
    # a randomly chosen cluster
    # FIXME: we might want to implement different strategies here
    n.diff = n.customers - n * n.cluster
    # sample random cluster
    idx = sample(1:n.cluster, 1)
    n.customers.in.cluster[idx] = n + n.diff
    return(n.customers.in.cluster)
}