# ToDo
# ====
#
# Unfinished stuff goes here.

# Helper function to export data frame to comma seperated format (CSV).
#
# @param x [Network]
#   Network to export.
# @param filename [character(1)]
#   Filename of the CSV file.
# @return Nothing
exportToCSV = function(x, filename) {
    assertClass(x, c("Network"))
    assertCharacter(filename, len = 1L, any.missing = FALSE)

    cns = c("ID", "X", "Y")
    df = round(x$coordinates, digits = 1)
    colnames(df) = c("X", "Y")
    df = cbind(data.frame(ID = seq(nrow(df))), df)
    if (!is.null(x$request.times)) {
        df = cbind(df, data.frame("Request time" = round(x$request.time, 2L)))
        cns = c(cns, "Request time")
    }

    #df[["Request Time"]] = round(runif(nrow(df)), digits = 2)
    write.table(df, file = filename, sep = ",",
        col.names = cns,
        row.names = FALSE,
        quote = FALSE
    )
}

# Exemplary, simple Poisson process based on exponential distribution.
#
# @param rate [numeric(1)]
#   Rate of the Poisson process.
# @preturn [numeric(1)]
#   Arrival time.
expPoissonProcess = function(rate) {
    -log(1.0 - runif(1)) / rate
}

# Add dynamic arrival times to nodes (for VRP).
#
# Arrival times result from a Poisson process.
#
# @param x [Network]
#   Network.
# @param n.dynamic [integer(1)]
#   Number of nodes, which should become dynamic. Ignored if dyn.customers.ratio
#   is not NULL.
# @param dyn.customers.ratio [numeric(1) | NULL]
#   Ratio of dynamic nodes. If this is set to a numeric value in (0, 1), the
#   parameter n.dynamic is ignored.
# @param poisson.rate [numeric(1)]
#   Rate of the poisson process for the arrival times.
# @param poisson.process [function(1)]
#   Function, which expects a poisson rate and returns a single numeric arrival time.
# @return [Network]
#   Modified network (has an additional list element 'request.times') and the
#   ratio of dynamic customers as an attribute.
addDynamic = function(x, n.dynamic, dyn.customers.ratio = NULL, poisson.rate = 1/40, poisson.process = expPoissonProcess) {
    n.points = nrow(x$coordinates)
    if (!is.null(dyn.customers.ratio)) {
        n.dynamic = floor(n.points * dyn.customers.ratio)
    }
    x$request.times = rep(0, n.points)
    idx.dyn = sample(1:n.points, replace = FALSE, size = n.dynamic)
    x$request.times[idx.dyn] = sapply(1:n.dynamic, function(x) poisson.process(poisson.rate))
    attr(x, "dyn.customers.ratio") = dyn.customers.ratio
    return(x)
}

# Get a meaningful default filename by concatenating all the information
# about the instance, i.e., instance size, number of clusters, morphed (yes/no), ...
#
# @param x [Network]
#   Network.
# @return [character(1)]
getFileName = function(x) {
    fn = "vrp"
    morphed = hasAttributes(x, "morphed")
    if (morphed) {
        fn = paste(fn, "_morphed", sep = "")
    }
    if (testClass(x, "ClusteredNetwork")) {
        fn = paste(fn, "_cl", length(unique(x$membership)), sep = "")
    }
    fn = paste(fn, "_n", nrow(x$coordinates), sep = "")
    if (morphed) {
        fn = paste(fn, "_gr", attr(x, "morphing.grade"), sep = "")
    }
    if (hasAttributes(x, "dyn.customers.ratio")) {
        fn = paste(fn, "_dyn", attr(x, "dyn.customers.ratio"), sep = "")
    }
    return(fn)
}