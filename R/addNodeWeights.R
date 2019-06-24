#' Add node weights.
#'
#' This function adds node weights to an edge-weighted graph. This is of interest
#' in the study of weighted TSP, where the distance between two nodes is not based
#' on the actual distance but additionally is influenced by the weight of the starting
#' node or all nodes prior in the permutation. This is indeed of practical interest.
#' E.g. consider a garbage collecting vehicle which requires the more gas per mile
#' the heavier its load.
#'
#' @template arg_network
#' @param weights [\code{numeric(1)}]\cr
#'   Numeric vector of weights.
#' @seealso \code{\link{generateRandomNetwork}}, \code{\link{generateClusteredNetwork}},
#' \code{\link{generateGridNetwork}}
#' @export
addNodeWeights = function(x, weights = NULL) {
  n = getNumberOfNodes(x)
  checkmate::assertNumeric(weights, len = n, any.missing = FALSE, all.missing = FALSE)
  x$node.weights = weights
  return(x)
}
