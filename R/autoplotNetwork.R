#' Autoplot function.
#'
#' Generates a \code{\link[ggplot2]{ggplot}} object. Nice possibility to
#' visualize 2-dimensional (clustered) networks in the euclidean plane.
#'
#' @param object [\code{Network}]\cr
#'   Instance to visualize.
#' @param ... [any]\cr
#'   Currently not used.
#' @return [\code{\link[ggplot2]{ggplot}}]
#' @export
autoplot.Network = function(object, ...) {
    if (ncol(object$coordinates) > 2L) {
        stopf("Only 2-dimensional networks can be plotted.")
    }

    df = as.data.frame(object, include.extras = TRUE)

    if (testClass(object, "ClusteredNetwork")) {
        df$membership = as.factor(df$membership)
    }

    #FIXME: can be done easier now! depot.idx = 1:getNumberOfDepots(object)?
    if (hasDepots(object)) {
        depot.idx = which(df$types == "depot")
        df.depots = df[depot.idx, ]
        df = df[-depot.idx, ]
    }

    pl = ggplot(data = df, mapping = aes_string(x = "x1", y = "x2"))
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
