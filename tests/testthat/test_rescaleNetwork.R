context("rescaling")

test_that("rescaling actually rescales network", {
    x = generateRandomNetwork(n.points = 10L)
    x.rg = range(x$coordinates)
    xr = rescaleNetwork(x, method = "global")

    # check for conditions that must be valid
    expect_true(all(xr$coordinates >= 0) && all(xr$coordinates <= 1))
    expect_true(any(xr$coordinates == 0) && any(xr$coordinates == 1))

    # scale back
    xr$coordinates = xr$coordinates * (x.rg[2] - x.rg[1]) + x.rg[1]
    expect_true(all((xr$coordinates - x$coordinates) < 0.00001))

    xr = rescaleNetwork(x, method = "by.dimension")
    expect_true(all(xr$coordinates >= 0) && all(xr$coordinates <= 1))
    expect_true(any(xr$coordinates[, 1] == 0))
    expect_true(any(xr$coordinates[, 1] == 1))
    expect_true(any(xr$coordinates[, 2] == 0))
    expect_true(any(xr$coordinates[, 2] == 1))
})