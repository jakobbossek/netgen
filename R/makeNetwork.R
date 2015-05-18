#' Generate network based on coordinates.
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
#' @return [\code{Network}]
#' @export
makeNetwork = function(coordinates,
  distance.matrix = NULL,
  name = NULL, comment = NULL,
  membership = NULL,
  depot.coordinates = NULL, lower = NULL, upper = NULL) {
  assertMatrix(coordinates)
  !is.null(name) && assertCharacter(name, len = 1L, any.missing = FALSE)
  !is.null(comment) && assertCharacter(comment, min.len = 1L, any.missing = FALSE)
  !is.null(membership) && assertNumeric(membership, any.missing = FALSE)
  !is.null(depot.coordinates) && assertMatrix(depot.coordinates)
  !is.null(distance.matrix) && assertMatrix(distance.matrix)

  if (is.null(lower) || is.null(upper)) {
    lower = min(coordinates)
    upper = max(coordinates)
  }

  if (is.null(distance.matrix)) {
    distance.matrix = as.matrix(dist(coordinates))
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
    classes = "Network"
  )
  if (!is.null(membership)) {
    network = addClasses(network, "ClusteredNetwork")
  }
  return(network)
}
