context("morphing")

test_that("morphing generates a network", {
    set.seed(1)
    reps = 3L
    n.points = 30L
    n.depots = 2L
    for (i in seq(reps)) {
        x = generateRandomNetwork(n.points = n.points)
        y = generateClusteredNetwork(n.points = n.points, n.cluster = sample(2:3, 1))

        z = morphInstances(x, y, alpha = 0.5)
        expect_is(z, "Network")
        expect_equal(getNumberOfNodes(z), n.points)
        expect_true(all(z$coordinates >= 0) && all(z$coordinates <= 1))
    }

    # check that fails if we have different number of nodes
    x = generateRandomNetwork(n.points = n.points)
    y = generateRandomNetwork(n.points = 2 * n.points)
    expect_error(morphInstances(x, y, alpha = 0.5))

    # check that depots are preserved
    x = generateRandomNetwork(n.points = n.points, n.depots = n.depots)
    y = generateClusteredNetwork(n.points = n.points, n.cluster = 2L, n.depots = n.depots)
    z = morphInstances(x, y, alpha = 0.5)

    expect_is(z, "Network")
    expect_true(hasDepots(z))
    expect_equal(ncol(getDepotCoordinates(z)), n.depots)
    expect_equal(nrow(getDepotCoordinates(z)), n.depots)
    expect_equal(getNumberOfDepots(z), n.depots)
})