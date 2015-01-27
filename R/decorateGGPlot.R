decorateGGPlot = function(pl, lower, upper) {
    pl = pl + theme(
        legend.position = "none",
        plot.title = element_text(size = rel(0.8), lineheight = 1.1, vjust = 1.6)
    )
    pl = pl + xlab(expression(x[1]))
    pl = pl + ylab(expression(x[2]))
    pl = pl + xlim(c(lower, upper))
    pl = pl + ylim(c(lower, upper))
    return(pl)
}