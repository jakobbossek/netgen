#' @title
#' Filter TSPlib instances according to its specifications.
#'
#' @description
#' Given a directory of TSP problems in the TSPlib format with file extension .tsp,
#' this function reads the specifications of each TSPlib instance in that directory
#' and returns a data frame with rowwise information about each instance.
#'
#' Basically the function is a wrapper around \code{\link{getTSPInstancesOverview}}.
#'
#'
#' @param directory [\code{character(1)}]\cr
#'   Readable directory path.
#' @param expr [\code{expression}]\cr
#'   Expression wrapped with the \code{quote} function.
#' @param paths.only [\code{logical(1)}]\cr
#'   Should only the full file names of the instances be returned?
#'   Default is \code{FALSE}.
#' @param opt.known [\code{logical(1)}]\cr
#'   Filter instances x with unknown optimal tour length (given in file x.tsp.tour)?
#'   Default is \code{FALSE}.
#' @return [\code{data.frame}]
#'
#' @examples
#' \dontrun{
#' # Get a data frame of instances and its properties for all instances
#' # with more than 4000 nodes
#' filterTSPInstances("path/to/instances", quote(dimension > 4000))
#'
#' # Now get only the full file names of all instances with edge weight type
#' # EUC_2D or CEIL_2D (see tsplib documentation for details)
#' filterTSPInstances("path/to/instances",
#'   expr = quote(edge_weight_type %in% c("EUC_2D", "CEIL_2D")),
#'   paths.only = TRUE
#' )
#' }
#'
#' @seealso \code{\link{getTSPInstancesOverview}}
#' @export
filterTSPInstances = function(directory = NULL, expr, paths.only = FALSE, opt.known = FALSE) {
  assertDirectory(directory, access = "r")
  assertFlag(paths.only, na.ok = FALSE)
  assertFlag(opt.known, na.ok = FALSE)

  df = getTSPInstancesOverview(directory, append.filename = TRUE)

  idx = eval(expr, envir = df)
  df2 = df[idx, , drop = FALSE]
  if (opt.known) {
    df2 = df2[which(df2$opt.length.known), , drop = FALSE]
  }
  if (paths.only)
    return(df2$file.path)
  return(df2)
}
