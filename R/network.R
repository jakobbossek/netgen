#' Generate a bare network (only the number of nodes is known).
#'
#' @param n [\code{integer(1)}]\cr
#'   Number of nodes.
#' @return [\code{Network}]
network = function(n, n.dims = 2L, lower, upper) {
  n = asInt(n, lower = 2L)
  if (length(lower) == 1L)
    lower = rep(lower, n.dims)
  if (length(upper) == 1L)
    upper = rep(upper, n.dims)
  BBmisc::makeS3Obj(n = n, lower = lower, upper = upper, classes = "Network")
}

coordLHS = function(n, n.dims, lower = 0, upper = 1, method = lhs::maximinLHS) {
  coords = method(n, n.dims)
  # stretch
  coords = lower + (upper - lower) * coords
  return(coords)
}

coordUniform = function(n, n.dims, lower, upper) {
  print(lower)
  coords = lapply(seq_len(n.dims), function(i) {
    runif(n, min = lower[i], max = upper[i])
  })
  coords = do.call(cbind, coords)
  return(coords)
}

addCenters = function(network, n.centers, generator, ...) {
  print(generator)
  if (!is.null(network$coordinates))
    stopf("Network already has coordinates! Place centers before coordinates.")
  if (!is.null(network$center.coordinates))
    stopf("Cluster centers already placed.")

  # generate cluster centers
  center.coordinates = generator(n.centers, n.dims = 2L, lower = network$lower, upper = network$upper, ...)
  network$center.coordinates = center.coordinates
  network$n.cluster = n.centers
  return(network)
}

addCoordinates = function(network, generator, ...) {
  membership = NULL
  if (is.null(network$n.cluster)) {
    coords = generator(network$n, n.dim = 2L, lower = network$lower, upper = network$upper, ...)
  } else {
    n = network$n
    nc = network$n.cluster
    #FIXME: handle case, that n/n_c is no integer
    n.per.cluster = floor(n / nc)
    n.per.cluster2 = rep(n.per.cluster, nc)
    if (nc * n.per.cluster != n) {
      idx = sample(seq_len(nc), 1L)
      n.per.cluster2[idx] = n.per.cluster2[idx] + 1L
    }
    coords = lapply(seq_len(nc), function(i) {
      generator(n.per.cluster2[i], n.dim = 2L, lower = network$lower, upper = network$upper, ...)
    })
    coords = do.call(rbind, coords)
    membership = rep(1:nc, rep(n.per.cluster, nc))
  }
  network$coordinates = coords
  network$membership = membership
  return(network)
}

addEdges = function(network, method = NULL) {
  adj.mat = matrix(1L, ncol = network$n, nrow = network$n)
  diag(adj.mat) = 0
  network$adj.mat = adj.mat
  return(network)
}

addWeights = function(network, method = "euclidean", weight.fun = NULL, symmetric = TRUE, ...) {
  n = network$n
  ws = network$weights
  n.weights = if (is.null(ws)) 0L else length(ws)

  if (method %in% "euclidean") {
    if (is.null(network$coordinates))
      stopf("Methods 'euclidean' needs coordinates.")
    ww = as.matrix(dist(network$coordinates, method = method))
    # if not all edges exist, set the remaining to infinifty
    # but keep zero distances on the diagonal
    if (!is.null(network$adj.mat)) {
      ww[adj.mat == 0] = Inf
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

# workflow
n = 100
lower = 0
upper = 10
nw = network(n, lower = lower, upper = upper) %>%
  addCenters(n.centers = 3L, generator = coordLHS) %>%
  addCoordinates(generator = coordUniform) %>%
  # addEdges() %>%
  addWeights(method = "euclidean") %>%
#  addWeights(method = "random", weight.fun = runif, min = 10, max = 50)
  addWeights(method = "random", weight.fun = rnorm, mean = 6, sd = 10)
#  addWeights(method = "random", weight.fun = rexp)


print(nw)

opar = par(mfrow = c(1, 2))
plot(nw$coordinates, xlim = c(0, upper), ylim = c(0, upper))
points(nw$center.coordinates, col = "tomato")
plot(as.numeric(nw$weights[[1L]]), as.numeric(nw$weights[[2L]]))
par(opar)


  #   coordinates = coordinates,
  #   distance.matrix = distance.matrix,
  #   depot.coordinates = depot.coordinates,
  #   membership = membership,
  #   name = name,
  #   comment = comment,
  #   lower = lower,
  #   upper = upper,
  #   opt.tour.length = opt.tour.length,
  #   opt.tour = opt.tour,
  #   edge.weight.type = edge.weight.type,
  #   classes = "Network"
  # )
