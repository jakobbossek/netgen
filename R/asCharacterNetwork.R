#' Get basic network information as a string.
#'
#' @param x [\code{Network}]\cr
#'   Network.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return [\code{character(1)}]
#' @export
as.character.Network = function(x, ...)   {
    n.points = getNumberOfNodes(x)
    n.clusters = getNumberOfClusters(x)

    char = paste("#Nodes:", n.points)
    if (n.clusters > 1L)
        char = paste(char, ", #Clusters:", n.clusters)
    if (hasAttributes(x, "morphed"))
        char = paste(char, "\n(Morphing coefficient ", attr(x, "morphing.grade"), ")", sep = "")
    char
}
