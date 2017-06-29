# Decorator for ggplot objects produced by netgen.
#
# @param pl [ggplot]
#   Object to decorate.
# @param lower [numeric(1)]
#   Lower box constaint for cube.
# @param upper [numeric(1)]
#   Upper box constaint for cube.
# @return [ggplot]
#   Decorated plot object.
decorateGGPlot = function(pl, lower, upper) {
  # by default no legend and smaller text size as well as higher distance of title to plot
  pl = pl + theme(
    legend.position = "none",
    plot.title = element_text(size = rel(0.8), lineheight = 1.1, vjust = 1.6)
  )
  pl = pl + xlab(expression(x[1]))
  pl = pl + ylab(expression(x[2]))
  pl = if (length(lower) == 1L) {
    pl + xlim(c(lower, upper)) + ylim(c(lower, upper))
  } else {
    pl + xlim(c(lower[1L], upper[1L])) + ylim(c(lower[2L], upper[2L]))
  }
  return(pl)
}
