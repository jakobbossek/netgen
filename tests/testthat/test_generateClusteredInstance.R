context("generate clustered instance")

test_that("generate clustered instance works as expected", {
    n.cluster = 5L
    n.points = 100L
    upper = 100
    n.depots = 2L

    checkClusteredInstance = function(x, n.cluster, n.points, upper = 1) {
        expect_is(x, "Network")
        expect_is(x, "ClusteredNetwork")
        expect_equal(n.cluster, getNumberOfClusters(x))
        expect_true(length(setdiff(n.cluster, unique(x$membership))) == 0)
        expect_equal(n.points, getNumberOfNodes(x))
        expect_true(all(x$coordinates <= upper))
    }

    # WITHOUT DEPOTS
    inst = generateClusteredInstance(n.cluster, n.points, upper = upper)
    checkClusteredInstance(inst, n.cluster, n.points, upper = upper)

    inst = generateClusteredInstance(n.cluster, n.points, distribution.strategy = "random.partition")
    checkClusteredInstance(inst, n.cluster, n.points)

    # check if as.data.frame works as expected
    df = as.data.frame(inst)
    expect_equal(nrow(df), n.points)
    expect_equal(ncol(df), 3L) # two dimensions plus membership column

    df = as.data.frame(inst, include.membership = FALSE)

    expect_equal(nrow(df), n.points)
    expect_equal(ncol(df), 2L) # no membership column

    # WITH DEPOTS
    inst = generateClusteredInstance(n.cluster, n.points, n.depots = n.depots)
    # in this case we have to nodes (the depots) more!
    checkClusteredInstance(inst, n.cluster, n.points + n.depots)



    # check plotting
    library(ggplot2)
    pl = autoplot(inst)
    expect_is(pl, c("gg", "ggplot"))
})