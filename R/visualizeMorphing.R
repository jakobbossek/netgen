#' Fancy visualization of morphing.
#'
#' Takes two instances of equal size and some alphas. Computes the point matching
#' and morphings and visualizes the points of the first instance gradually move
#' towards their matched counterparts of the second instance.
#'
#' @param x [\code{Network}]\cr
#'   Network or ClusteredNetwork.
#' @param y [\code{Network}]\cr
#'   Network or ClusteredNetwork.
#' @param alphas [\code{numeric}]\cr
#'   Coeffiecients 'alpha' for different convex combinations.
#' @param arrows [\code{logical(1)}]\cr
#'   Draw arrows originating in the points of \code{x} and ending in the
#'   points matched in \code{y}. Default is \code{TRUE}.
#' @return [\code{\link[ggplot2]{ggplot}}]
#' @seealso \code{\link{morphInstances}}
#' @export
visualizeMorphing = function(x, y, alphas = c(0.25, 0.5, 0.75), arrows = TRUE) {
    assertClass(x, "Network")
    assertClass(y, "Network")
    assertNumeric(alphas, any.missing = FALSE, lower = 0, upper = 0.75)

    # we compute the point matching here one time additionaly for later use
    point.matching = getOptimalPointMatching(x$coordinates, y$coordinates)

    # do the morphing
    instances = lapply(alphas, function(alpha) {
        morphInstances(x, y, alpha = alpha)
    })

    # put in the original instances as extreme instances
    alphas = c(0, alphas, 1)
    instances = c(list(x), instances, list(y))

    # build data frame of points
    df.points = lapply(1:length(instances), function(i) {
        df = as.data.frame(instances[[i]], include.extra = FALSE)
        df$alpha = alphas[i]
        df
    })

    df.points = do.call(rbind, df.points)
    df.points$alpha = as.factor(df.points$alpha)

    pl = ggplot(data = df.points, mapping = aes_string(x = "x1", y = "x2", colour = "alpha"))

    if (arrows) {
        # build arrows from x to (matched) y coordinates
        df.arrows = cbind(as.data.frame(x$coordinates), as.data.frame(y$coordinates[point.matching[, 2], ]))
        colnames(df.arrows) = c("x1", "x2", "end1", "end2")
        pl = pl + geom_segment(
            data = df.arrows,
            mapping = aes_string(x = "x1", y = "x2", xend = "end1", yend = "end2"),
                arrow = grid::arrow(type = "closed", length = grid::unit(0.1, "inches")),
                colour = "gray"
            )
    }

    pl = pl + geom_point()
    #pl = pl + geom_point(data = subset(df.points, alpha %in% c(0, 1)), colour = "black", size = 3)
    return(pl)
}