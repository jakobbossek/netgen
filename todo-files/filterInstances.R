#' @title
#'   Filter instances.
#'
#' @description
#'   Offers the possibility to filter instances according to different specification
#'   parts of the TSPlib format, e.g., by dimension or by edge weight type.
#'
#' @note This function does not expect a list of \code{Network} objects, but a
#'   path to a folder which contains TSP instances in the TSPlib format.
#'
#' @family filter_functions
#'
#' @param path [character(1)]
#'   Path to instances in TSPlib format. All files with the file extension *.tsp
#'   will be considered.
#' @param by [character(1)]
#'   Keyword used for filtering, e.g., DIMENSION or EDGE_WEIGHT_TYPE.
#' @param expr [language(1)]
#'   This must be a quoted expression with the \dQuote{value} variable used. The latter
#'   is substiduted by the value of the \code{by} keyword before evaluation.
#' @param full.names [logical(1)]
#'   If \code{TRUE}, the directory path is prepended to the file names to give a
#'   relative file path. Otherwise, simply the file names are returned. Default is \code{FALSE}.
#'
#' @return [character]
#'   File names of the filtered files.
#'
#' @examples
#'   \dontrun{
#'   source.dir = "."
#'   filterInstancesBy(source.dir, expr = quote(value <= 1000))
#'   filterInstancesByDimension(source.dir, expr = quote(value >= 50 && value <= 200))
#'   filterInstancesByEdgeWeightType(source.dir, expr = quote(!(value %in% "EUC_2D")))
#'   }
#' @export
filterInstancesBy = function(path, by = "DIMENSION", expr = NULL, full.names = FALSE) {
  assertDirectoryExists(path, access = "r")
  assertCharacter(by, len = 1L)
  assertFlag(full.names)
  #FIXME: I do not know if this is the best solution, but it works well.
  if (typeof(expr) != "language") {
    stopf("Please wrap 'expr' with the quote function.")
  }

  # get all files in the path
  files = list.files(path, full.names = TRUE, pattern = "*.tsp$")

  # filter according to provided 'expr'
  filtered.files = Filter(function(x) {
    # open file and read in the specification part
    fh = file(x, "r")
    specs = readSpecificationPart(fh, list())

    # get the value of the 'by' argument
    needle = specs[[tolower(by)]]
    close(fh)
    #FIXME: this is ugly. We should do that in readSpecificationPart?
    if (by %in% c("DIMENSION")) {
      needle = as.integer(needle)
    }
    if (is.null(needle)) {
      stopf("Unknown specifier provided: %s", by)
    }
    # substitute 'value' in e.g. quote(value <= 100) and evaluate
    #FIXME: there needs to be a more elegant way to do that!
    return(eval(eval(substitute(substitute(expr, list(value = needle)), list(expr = expr)))))
  }, files)
  if (!full.names) {
    return(basename(filtered.files))
  }
  return(filtered.files)
}

#' @title
#'   Filter instances by dimension.
#' @family filter_functions
#' @inheritParams filterInstancesBy
#' @export
filterInstancesByDimension = function(path, expr = NULL, full.names = TRUE) {
  filterInstancesBy(path, by = "DIMENSION", expr = expr, full.names = full.names)
}

#' @title
#'  Filter instances by edge weight type.
#' @family filter_functions
#' @inheritParams filterInstancesBy
#' @export
filterInstancesByEdgeWeightType = function(path, expr = NULL, full.names = TRUE) {
  filterInstancesBy(path, by = "EDGE_WEIGHT_TYPE", expr = expr, full.names = full.names)
}
