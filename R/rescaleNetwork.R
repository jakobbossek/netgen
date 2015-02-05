#' Normalize coordinates to unit cube maintaining its geography.
#'
#' @template arg_network
#' @param method [\code{character(1)}]\cr
#'   Rescaling method which actually modifies the coordinates. ...
#' @return [\code{Network}]
#' @export
rescaleNetwork = function(x, method = "by.dimension") {
    assertClass(x, "Network")
    if (hasDepots(x)) {
        stopf("Rescaling of networks with depots currently not supported.")
    }
    method.mapping = list(
        "by.dimension" = rescaleNetworkByDimension,
        "global" = rescaleNetworkGlobal
    )
    assertChoice(method, choices = names(method.mapping))
    rescaleMethod = method.mapping[[method]]
    x$coordinates = rescaleMethod(x$coordinates)

    # rescaling is a normalization to [0,1]
    x$lower = 0
    x$upper = 1
    return(x)
}

# Rescale by normalizing with global min/max values.
rescaleNetworkGlobal = function(x) {
    rg = range(x)
    (x - rg[1]) / (rg[2] - rg[1])
}

# Rescale by normalizing dimension-wise with min/max values
rescaleNetworkByDimension = function(x) {
    min = apply(x, 2, min)
    max = apply(x, 2, max)
    t((t(x) - min) / (max - min))
}
