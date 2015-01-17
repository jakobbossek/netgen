#' Autoplot function.
#'
#' Generates a \code{\link[ggplot2]{ggplot}} object. Nice possibility to
#' visualize 2-dimensional (clustered) networks in the euclidean plane.
#'
#' @param object [\code{ClusterInstance}]\cr
#'   Instance to visualize.
#' @param ... [any]\cr
#'   Currently not used.
#' @return [\code{\link[ggplot2]{ggplot}}]
#'   ggplot2 object.
#' @export
autoplot.Network = function(object, ...) {
    if (ncol(object$coordinates) > 2L) {
        stopf("Only 2-dimensional networks can be plotted.")
    }

    df = as.data.frame(object, include.extras = TRUE)

    if (testClass(object, "ClusteredNetwork")) {
        df$membership = as.factor(df$membership)
    }

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
    pl = pl + theme(
        legend.position = "none",
        plot.title = element_text(size = rel(0.8), lineheight = 1.1, vjust = 1.6)
    )
    pl = pl + xlab(expression(x[1])) + ylab(expression(x[2]))
    pl = pl + xlim(c(object$lower, object$upper)) + ylim(c(object$lower, object$upper))
    return(pl)
}
