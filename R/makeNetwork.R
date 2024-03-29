#' @title Generate network based on coordinates.
#'
#' @description
#' Create a (clustered) network object.
#'
#' @param coordinates [\code{matrix}]\cr
#'   Numeric matrix of 2D coordinates.
#' @param distance.matrix [\code{matrix}]\cr
#'   Optional distance matrix.
#' @param name [\code{character(1)} | \code{NULL}]\cr
#'   Optional name of the network.
#' @param comment [\code{character} | \code{NULL}]\cr
#'   Optional additional comments on instance.
#' @param membership [\code{numeric} | \code{NULL}]\cr
#'   Optional vector of memberships for clustered networks.
#' @param depot.coordinates [\code{matrix} | \code{NULL}]\cr
#'   Numeric matrix of 2D coordinates of depots. Default is \code{NULL}, which
#'   means no depots at all.
#' @param lower [\code{numeric(1)}]\cr
#'   Lower box constraint of cube.
#' @param upper [\code{numeric(1)}]\cr
#'   Upper box constraint of cube.
#' @param opt.tour.length [\code{numeric(1)}]\cr
#'   Optional length of the optimal roundtrip tour.
#'   Default is \code{NULL}, which means the tour length is unknown.
#' @param opt.tour [\code{integer}]\cr
#'   Optional optimal permutation of node indizes.
#'   Default is \code{NULL}, which means the optimal tour is unknown.
#' @param edge.weight.type [\code{character(1)} | \code{NULL}]
#'   The edge weight type indicates how edge weights are represented in the TSPlib
#'   format. If \code{distance.matrix} is \code{NULL}, the passed value is ignored
#'   and EUC_2D is assigned. Otherwise the edge weight type must be one of the
#'   following \code{{EUC_2D, EUC_3D, MAX_2D, MAX_3D, MAN_2D, MAN_3D, CEIL_2D,
#'   GEO, ATT, EXPLICIT}}.
#' @param node.weights [\code{numeric}]\cr
#'   Vector of node weights (for weighted version of TSP). Default is \code{NULL},
#'   i.e., no node weights at all.
#' @return [\code{Network}]
#' @export
makeNetwork = function(coordinates,
  distance.matrix = NULL,
  name = NULL, comment = NULL,
  membership = NULL, edge.weight.type = NULL,
  depot.coordinates = NULL, lower = NULL, upper = NULL,
  opt.tour.length = NULL, opt.tour = NULL,
  node.weights = NULL) {
  assertMatrix(coordinates)
  n.points = nrow(coordinates)
  if (!is.null(name)) assertCharacter(name, len = 1L, any.missing = FALSE)
  if (!is.null(comment)) assertCharacter(comment, min.len = 1L, any.missing = FALSE)
  if (!is.null(membership)) assertNumeric(membership, any.missing = FALSE)
  if (!is.null(depot.coordinates)) assertMatrix(depot.coordinates)
  if (!is.null(distance.matrix)) assertMatrix(distance.matrix)
  if (!is.null(edge.weight.type)) assertChoice(edge.weight.type, getValidEdgeWeightTypes())
  if (!is.null(opt.tour.length)) assertNumber(opt.tour.length, na.ok = FALSE)
  if (!is.null(opt.tour)) assertInteger(opt.tour, len = n.points, any.missing = FALSE, all.missing = FALSE)
  if (!is.null(node.weights)) assertNumeric(node.weights, len = n.points, any.missing = FALSE, all.missing = FALSE)

  if (!is.null(opt.tour) && !is.null(depot.coordinates)) {
    stopf("Optimal tours for instances with depots not supported at the moment.")
  }

  if (is.null(lower) || is.null(upper)) {
    lower = min(coordinates)
    upper = max(coordinates)
  }

  if (is.null(distance.matrix)) {
    if (!is.null(edge.weight.type)) {
      warningf("No distance matrix passed to makeNetwork. Passed edge.weight.type '%s'
        will be replaced by 'EUC_2D'.", edge.weight.type)
    }
    edge.weight.type = "EUC_2D"
    distance.matrix = as.matrix(dist(coordinates))
  }

  # check for duplicated node coordinates
  tmp = distance.matrix
  diag(tmp) = Inf
  if (any(tmp == 0)) {
    warningf("There are zero distances in the distance matrix. Maybe there are
      duplicate node coordinates.")
  }

  network = makeS3Obj(
    coordinates = coordinates,
    distance.matrix = distance.matrix,
    depot.coordinates = depot.coordinates,
    membership = membership,
    name = name,
    comment = comment,
    lower = lower,
    upper = upper,
    opt.tour.length = opt.tour.length,
    opt.tour = opt.tour,
    edge.weight.type = edge.weight.type,
    node.weights = node.weights,
    classes = "Network"
  )
  if (!is.null(membership)) {
    network = addClasses(network, "ClusteredNetwork")
  }
  if (!is.null(node.weights)) {
    network = addClasses(network, "NodeWeightedNetwork")
  }
  return(network)
}

#' Check if object is \code{Network}.
#'
#' @param x [any]\cr
#'   Arbitrary R object.
#' @return [\code{logical(1)}]
#' @export
isNetwork = function(x) {
  return(inherits(x, "Network"))
}
