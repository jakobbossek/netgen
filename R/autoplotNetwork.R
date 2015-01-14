
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
    if (testClass(object, "ClusteredNetwork")) {
        df = as.data.frame(object, include.membership = TRUE)
        df$membership = as.factor(df$membership)
    } else {
        df = object$coordinates
    }
    pl = ggplot(data = df, mapping = aes_string(x = "x1", y = "x2"))
    if (!is.null(df$membership)) {
        pl = pl + geom_point(aes_string(colour = "membership"))
    } else {
        pl = pl + geom_point(colour = "tomato")
    }
    pl = pl + ggtitle(as.character(object))
    pl = pl + theme(
        legend.position = "none",
        plot.title = element_text(size = rel(0.8), lineheight = 1.1, vjust = 1.6)
    )
    pl = pl + xlab(expression(x[1])) + ylab(expression(x[2]))
    #FIXME: probably it would be nice to save bounds in ClusterInstance
    #pl = pl + xlim(c(0, 1)) + ylim(c(0, 1))
    return(pl)
}