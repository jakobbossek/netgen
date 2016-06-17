#' Autoplot function.
#'
#' Generates a \code{\link[ggplot2]{ggplot}} object. Nice possibility to
#' visualize 2-dimensional (clustered) networks in the euclidean plane.
#'
#' @param object [\code{Network}]\cr
#'   Network.
#' @param path [\code{integer}]\cr
#'   An integer vector containing the order of cities of a path or a list
#'   of multiple paths. Keep in mind
#'   that instances with \eqn{n} nodes and \eqn{m} depots have \eqn{n + m}
#'   coordinates, with the \eqn{1,\ldots,m} first coordinates belonging to
#'   the depots.
#' @param close.path [\code{logical(1)}]\cr
#'   Logical indicating whether the path passed by \code{path} should be
#'   closed to a cycle. Default is \code{FALSE}.
#' @param path.colour [\code{character(1)}]\cr
#'   Colour of the lines linking nodes on a path. Default is \dQuote{gray}.
#' @param use.opt.tour [\code{logical(1)}]\cr
#'   If the given network knows its optimal tour, should it be plotted?
#'   If this is the case and \code{path} is given additionally, the optimal
#'   tour is ignored.
#'   Default is \code{FALSE}.
#' @param ... [any]\cr
#'   Currently not used.
#' @return [\code{\link[ggplot2]{ggplot}}]
#' @examples
#' \dontrun{
#' # here we have no depots ...
#' x = generateClusteredNetwork(n.points = 30L, n.cluster = 2L)
#' pl = autoplot(x, path = 1:3)
#' # ... and here we have two depots: the path visits the depots in this case
#' x = generateRandomNetwork(n.points = 30L, n.depots = 2L)
#' pl = autoplot(x, path = 1:3, path.colour = "tomato")
#' }
#' @export
autoplot.Network = function(object,
  path = NULL, close.path = FALSE, path.colour = "gray",
  use.opt.tour = FALSE,
  ...) {
  if (!is.null(path)) {
    if (!testNumeric(path, min.len = 2L, any.missing = FALSE) & !testList(path, min.len = 2L, any.missing = FALSE)) {
      stopf("Path argument needs to be a vector or a list.")
    }
  }
  assertString(path.colour, na.ok = FALSE)
  assertFlag(close.path, na.ok = FALSE)

  if (ncol(object$coordinates) > 2L) {
    stopf("Only 2-dimensional networks can be plotted.")
  }

  df = as.data.frame(object, include.extras = TRUE)

  if (testClass(object, "ClusteredNetwork")) {
    df$membership = as.factor(df$membership)
  }

  if (hasDepots(object)) {
    depot.idx = which(df$types == "depot")
    df.depots = df[depot.idx, , drop = FALSE]
  }

  if (is.list(path)) {
    n = nrow(df)
    df = df[rep(seq_len(n), length(path)), ]
    ns = if (!is.null(names(path))) names(path) else as.character(seq_len(length(path)))
    df$Path = rep(ns, each = n)
  }

  pl = ggplot(data = df, mapping = aes_string(x = "x1", y = "x2"))

  # facets if multiple paths given
  if (is.list(path)) {
    pl = pl + facet_grid(. ~ Path)
  }

  if (!is.null(path) || !is.null(use.opt.tour)) {
    # if we have a list of pathes
    if (is.list(path)) {
      # sequentially build pathes (one path per facet)
      path.coords = data.frame()
      for (i in seq_len(length(path))) {
        p = path[[i]]
        pname = names(path)[i]
        if (close.path) {
          p = c(p, p[1])
        }
        the.path.coords = df[p, , drop = FALSE]
        the.path.coords$Path = pname
        path.coords = rbind(path.coords, the.path.coords)
      }
    } else {
      # do the same as above only for one path
      path = if (use.opt.tour) object$opt.tour else path
      close.path = if (use.opt.tour) TRUE else close.path
      if (close.path) {
        path = c(path, path[1])
      }
      path.coords = df[path, , drop = FALSE]
    }
    pl = pl + geom_path(data = path.coords, colour = path.colour)
  }

  if (!is.null(df$membership)) {
    pl = pl + geom_point(aes_string(colour = "membership"))
  } else {
    pl = pl + geom_point(colour = "tomato")
  }

  if (hasDepots(object)) {
    pl = pl + geom_point(data = df.depots, colour = "black", size = 4)
    pl = pl + geom_point(data = df.depots, colour = "white", size = 3)
  }
  pl = pl + ggtitle(as.character(object))
  pl = decorateGGPlot(pl, lower = object$lower, upper = object$upper)
  return(pl)
}
