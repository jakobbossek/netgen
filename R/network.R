#' Generate a bare network (only the number of nodes is known).
#'
#' @param n [\code{integer(1)}]\cr
#'   Number of nodes.
#' @return [\code{Network}]
network = function(n.dims = 2L, lower, upper) {
  #n = asInt(n, lower = 2L)
  if (length(lower) == 1L)
    lower = rep(lower, n.dims)
  if (length(upper) == 1L)
    upper = rep(upper, n.dims)
  BBmisc::makeS3Obj(lower = lower, upper = upper, classes = "Network")
}

coordLHS = function(n, n.dims, lower = 0, upper = 1, method = lhs::maximinLHS) {
  coords = method(n, n.dims)
  # stretch
  coords = lower + (upper - lower) * coords
  return(coords)
}

coordUniform = function(n, n.dims, lower, upper) {
  coords = lapply(seq_len(n.dims), function(i) {
    runif(n, min = lower[i], max = upper[i])
  })
  coords = do.call(cbind, coords)
  return(coords)
}

coordGrid = function(n, n.dims, lower, upper) {
  m = sqrt(n)
  x1 = seq(lower[1], upper[2], length.out = m)
  x2 = seq(lower[2], upper[2], length.out = m)
  coords = expand.grid(x1, x2)
  names(coords) = NULL
  coords = as.matrix(coords)
  return(coords)
}

addCenters = function(network, n.centers, generator, ...) {
  if (!is.null(network$coordinates))
    stopf("Network already has coordinates! Place centers before coordinates.")
  if (!is.null(network$center.coordinates))
    stopf("Cluster centers already placed.")

  # generate cluster centers
  center.coordinates = generator(n.centers, n.dims = 2L, lower = network$lower, upper = network$upper, ...)
  network$center.coordinates = center.coordinates
  network$n.cluster = n.centers
  if (!("ClusteredNetwork" %in% class(network)))
    network = addClasses(network, "ClusteredNetwork")
  return(network)
}

addCoordinates = function(network, n, generator, by.centers = FALSE, par.fun = NULL, ...) {
  membership = NULL
  if (!by.centers) {
    coords = generator(n, n.dim = 2L, lower = network$lower, upper = network$upper, ...)
  } else {
    nc = network$n.cluster
    if (length(n) == nc) {
      n.per.cluster2 = n
    } else {
      n.per.cluster = floor(n / nc)
      n.per.cluster2 = rep(n.per.cluster, nc)
      if (nc * n.per.cluster != n) {
        idx = sample(seq_len(nc), 1L)
        n.per.cluster2[idx] = n.per.cluster2[idx] + 1L
      }
    }
    catf("n per cluster %s", collapse(n))
    coords = lapply(seq_len(nc), function(i) {
      gen.args = list(n = n.per.cluster2[i], n.dim = 2L)
      if (!is.null(par.fun))
        gen.args = c(gen.args, par.fun(network$center.coordinates[i, ]))
      gen.args = c(gen.args, list(...))
      do.call(generator, gen.args)
    })
    coords = do.call(rbind, coords)
    membership = rep(1:nc, n.per.cluster2)
  }
  catf("n is %i", network$n.nodes)
  network$n.nodes = if (!is.null(network$n.nodes)) network$n.nodes + sum(n) else sum(n)
  network$coordinates = if (!is.null(network$coordinates)) rbind(network$coordinates, coords) else coords
  network$membership = if (!is.null(network$membership)) c(network$membership, if (by.centers) membership else rep(0, nrow(coords))) else membership
  return(network)
}

addEdges = function(network, method = NULL) {
  adj.mat = matrix(1L, ncol = network$n.nodes, nrow = network$n.nodes)
  diag(adj.mat) = 0
  network$adj.mat = adj.mat
  return(network)
}

addWeights = function(network, method = "euclidean", weight.fun = NULL, symmetric = TRUE, ...) {
  n = network$n.nodes
  ws = network$weights
  n.weights = if (is.null(ws)) 0L else length(ws)

  if (method %in% "euclidean") {
    if (is.null(network$coordinates))
      stopf("Methods 'euclidean' needs coordinates.")
    ww = as.matrix(dist(network$coordinates, method = method))
    # if not all edges exist, set the remaining to infinifty
    # but keep zero distances on the diagonal
    if (!is.null(network$adj.mat)) {
      ww[network$adj.mat == 0] = Inf
      diag(ww) = 0
    }
  } else {
    if (is.null(weight.fun))
      stopf("You need to pass a weight fun.")
    # default to asymmetric weight matrix
    m = n * n - n

    # extract number of edges if adjacency matrix is available
    if (!is.null(network$adj.mat))
      m = sum(network$adj.mat)

    # save half of the work if matrix is symmetric
    if (symmetric)
      m = m / 2L

    weights = weight.fun(m, ...)

    #if (!is.null(adj.mat)) {
    ww = matrix(0, ncol = n, nrow = n)
    ww[upper.tri(ww)] = weights
    ww[lower.tri(ww)] = t(ww[upper.tri(ww)])
  }
  network$weights[[n.weights + 1L]] = ww
  if (!("moNetwork" %in% class(network)))
    network = BBmisc::addClasses(network, "moNetwork")
  return(network)
}
