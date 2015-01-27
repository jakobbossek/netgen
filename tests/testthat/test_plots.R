context("plots")

test_that("all of our plots produce nice gglot2 objects", {
    library(ggplot2)

    # helper function
    expect_is_ggplot = function(pl) {
        expect_is(pl, c("gg", "ggplot"))
    }

    # no cluster, no depots
    x = generateRandomInstance(n.points = 20L)
    expect_is_ggplot(autoplot(x))

    # no cluster, with depots
    x = generateRandomInstance(n.points = 20L, n.depots = 2L, upper = 100)
    expect_is_ggplot(autoplot(x))

    # some clusters, no depots
    x = generateClusteredInstance(n.points = 20L, n.cluster = 2L)
    expect_is_ggplot(autoplot(x))

    # some clusters, with depots
    x = generateClusteredInstance(n.points = 20L, n.cluster = 2L, n.depots = 2L)
    expect_is_ggplot(autoplot(x))

    # autoplot morphed instance
    x = generateRandomInstance(n.points = 10L, upper = 20)
    y = generateClusteredInstance(n.points = 10L, n.cluster = 2L, upper = 20)
    z = morphInstances(x, y, alpha = 0.3)
    expect_is_ggplot(autoplot(z))

    for (show.arrows in c(TRUE, FALSE)) {
        for (do.facets in c(TRUE, FALSE)) {
            pl = visualizeMorphing(x, y, arrows = show.arrows, in.one.plot = do.facets)
            expect_is_ggplot(pl)
        }
    }

    # test visualization of point matchings
    x = generateRandomInstance(n.points = 10L)
    y = generateClusteredInstance(n.points = 10L, n.cluster = 2L)
    pm = netgen::getOptimalPointMatching(x$coordinates, y$coordinates)
    pl = visualizePointMatching(x, y, pm)
    expect_is_ggplot(pl)
})