context("generate clustered network")

test_that("generate clustered network works as expected", {
    n.cluster = 5L
    n.points = 100L
    lower = 1
    upper = 100
    n.depots = 2L

    checkClusteredInstance = function(x, n.cluster, n.points, lower = 0, upper = 1) {
        expect_is(x, "Network")
        expect_is(x, "ClusteredNetwork")
        expect_equal(n.cluster, getNumberOfClusters(x))
        expect_true(length(setdiff(n.cluster, unique(x$membership))) == 0)
        expect_equal(n.points, getNumberOfNodes(x))
        expect_true(all(x$coordinates <= upper))
        expect_true(all(x$coordinates >= lower))
    }

    # WITHOUT DEPOTS
    x = generateClusteredNetwork(n.cluster, n.points, lower = lower, upper = upper)
    checkClusteredInstance(x, n.cluster, n.points, lower = lower, upper = upper)

    # check mirroring strategy
    x = generateClusteredNetwork(n.cluster, n.points, lower = lower, upper = upper, out.of.bounds.handling = "mirror")
    checkClusteredInstance(x, n.cluster, n.points, lower = lower, upper = upper)

    #FIXME: reenable when "random.partition" is finished
    # x = generateClusteredNetwork(n.cluster, n.points, lower = lower, distribution.strategy = "random.partition")
    # checkClusteredInstance(x, n.cluster, lower = lower, n.points)

    # WITH DEPOTS
    x = generateClusteredNetwork(n.cluster, n.points, lower = lower, upper = upper, n.depots = n.depots)
    # in this case we have to nodes (the depots) more!
    checkClusteredInstance(x, n.cluster, n.points, lower = lower, upper = upper)

    # check plotting
    library(ggplot2)
    pl = autoplot(x)
    expect_is(pl, c("gg", "ggplot"))
})