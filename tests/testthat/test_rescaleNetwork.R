context("rescaling")

test_that("rescaling actually rescales network", {
  expect_in_bounds = function(coords) {
    expect_true(all(coords >= 0) && all(coords <= 1))
    expect_true(any(coords == 0) && any(coords == 1))
  }
  x = generateRandomNetwork(n.points = 10L)
  x.rg = range(x$coordinates)
  # GLOBAL RESCALING
  xr = rescaleNetwork(x, method = "global")

  # check for conditions that must be valid
  expect_in_bounds(xr$coordinates)
  expect_true(any(xr$coordinates == 0) && any(xr$coordinates == 1))

  # scale back
  xr$coordinates = xr$coordinates * (x.rg[2] - x.rg[1]) + x.rg[1]
  expect_true(all((xr$coordinates - x$coordinates) < 0.00001))

  # GLOBAL RESCALING (2nd)
  xr = rescaleNetwork(x, method = "global2")
  expect_in_bounds(xr$coordinates)

  # check for conditions that must be valid
  expect_in_bounds(xr$coordinates)
  expect_true(any(xr$coordinates == 0) && any(xr$coordinates == 1))

  xr = rescaleNetwork(x, method = "by.dimension")
  expect_in_bounds(xr$coordinates)
  expect_true(any(xr$coordinates[, 1] == 0))
  expect_true(any(xr$coordinates[, 1] == 1))
  expect_true(any(xr$coordinates[, 2] == 0))
  expect_true(any(xr$coordinates[, 2] == 1))
})
