#' Exports a network to a simple comma separated file (CSV).
#'
#' @note Currently we only support euclidean 2D instances.
#'
#' @param x [\code{Network}]\cr
#'   Network to export.
#' @param filename [\code{character(1)}]\cr
#'   File name.
#' @param full [\code{logical(1)}]\cr
#'   Should all meta data like cluster membership be stored. You most probably
#'   want to use the default setting, which is \code{TRUE}.
#' @param col.names [\code{logical(1)}]\cr
#'   Should there be a header row? Default is \code{TRUE}. Note that with this
#'   option set to \code{FALSE}, it is not possible to import the file again.
#' @param digits [\code{integer(1)}]\cr
#'   Round coordinates to this number of digits. Default is 2.
#' @return Nothing
#' @export
exportToCSVFormat = function(x, filename, full = TRUE, col.names = TRUE, digits = 2L) {
  assertClass(x, c("Network"))
  assertCharacter(filename, len = 1L, any.missing = FALSE)
  assertFlag(col.names, na.ok = FALSE)
  assertInt(digits, lower = 0L, upper = 10L, na.ok = FALSE)

  x.as.df = as.data.frame(x, include.extra = TRUE)

  cns = c("ID", "X", "Y")
  df = round(x$coordinates, digits = 2)
  if (hasDepots(x)) {
    df = rbind(round(x$depot.coordinates, digits = 2L), df)
  }
  colnames(df) = c("X", "Y")
  df = cbind(data.frame(ID = seq(nrow(df))), df)
  if (!is.null(x$arrival.times)) {
    arrival.times = round(x$arrival.times, 2L)
    if (hasDepots(x))
      arrival.times = c(rep(0, getNumberOfDepots(x)), arrival.times)
    df = cbind(df, data.frame("arrival.time" = arrival.times))
    cns = c(cns, "arrival.time")
  }
  if (full) {
    if (!is.null(x.as.df$types)) {
      df$types = x.as.df$types
    }
    if (!is.null(x.as.df$membership)) {
      df$membership =  x.as.df$membership
    }
    df$lower = x$lower
    df$upper = x$upper
  }

  write.table(df, file = filename, sep = ",",
    col.names = col.names,
    row.names = FALSE,
    quote = FALSE
  )
}
