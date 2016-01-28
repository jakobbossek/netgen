#' @title Get an overview of instances in a directory.
#'
#' @description This function expects a directory and returns a
#' data frame containing the most important properties, e. g., dimension,
#' edge weight type, of all TSPlib instances (with file extensions tsp) in
#' that directory. Moreover, the data frame contains information on the
#' availiability of the optimal tour length (files tsp.opt) and optimal
#' tour (tsp.tour).
#'
#' @param directory [\code{character(1)}]\cr
#'   Readable directory path.
#' @param append.filename [\code{logical(1)}]\cr
#'   Should the full file names be appended to the data frame?
#'   Default is \code{FALSE}.
#' @return [\code{data.frame}]
#' @export
getTSPInstancesOverview = function(directory, append.filename = FALSE) {
  assertDirectory(directory, access = "r")
  files = list.files(directory, full.names = TRUE, pattern = ".tsp$")

  # get the raw specifications for each file extracting ets specification part
  raw.specs = lapply(files, function(file.path) {
    fh = file(file.path, "r")
    on.exit(close(fh))
    specs = readSpecificationPart(fh, list())
    specs$file.path = file.path
    specs$opt.length.known = file.exists(paste0(file.path, ".opt"))
    specs$opt.tour.known = file.exists(paste0(file.path, ".tour"))
    return(specs)
  })

  # now build up the data frame
  specs = lapply(raw.specs, function(spec) {
    df = data.frame(
      name = spec$name,
      dimension = as.integer(spec$dimension),
      edge_weight_type = spec$edge_weight_type,
      opt.tour.known = as.logical(spec$opt.tour.known),
      opt.length.known = as.logical(spec$opt.length.known)
    )
    if (append.filename) {
      df$file.path = spec$file.path
    }
    return(df)
  })

  specs = do.call(rbind, specs)
  return(specs)
}
