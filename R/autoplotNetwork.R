
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
        title = paste("#Nodes:", nrow(df), ", #Clusters:", length(unique(df$membership)))
    } else {
        pl = pl + geom_point(colour = "tomato")
        title = paste("#Nodes:", nrow(df))
    }
    pl = pl + ggtitle()
    pl = pl + theme(legend.position = "none")
    #FIXME: probably it would be nice to save bounds in ClusterInstance
    #pl = pl + xlim(c(0, 1)) + ylim(c(0, 1))
    return(pl)
}