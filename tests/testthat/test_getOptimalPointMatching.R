context("point matching/assignment")

test_that("point matching algorithm works correctly!", {

  checkPointMatching = function(pm, n.points) {
    expect_true(all(pm[, 1] == 1:n.points))
    expect_true(all(sort(pm[, 2]) == 1:n.points))
  }

  # dimensions do not match
  x = generateClusteredNetwork(n.points = 10L, n.cluster = 2L)
  y = generateRandomNetwork(n.points = 12L)
  expect_error(getOptimalPointMatching(x$coordinates, y$coordinates))

  # dimension > 2 not allowed
  y = generateRandomNetwork(n.points = 10L, n.dim = 3L)
  expect_error(getOptimalPointMatching(x$coordinates, y$coordinates))

  y = generateRandomNetwork(n.points = 10L)
  x.coords = x$coordinates
  y.coords = y$coordinates
  n = nrow(x.coords)

  # build point matching(s)
  p1 = getOptimalPointMatching(x.coords, y.coords, method = "lp")
  p2 = getOptimalPointMatching(x.coords, y.coords, method = "push_relabel")
  p3 = getOptimalPointMatching(x.coords, y.coords, method = "random")
  p4 = getOptimalPointMatching(x.coords, y.coords, method = "greedy")

  # check if we really deal with assignments
  checkPointMatching(p1, n)
  checkPointMatching(p2, n)
  checkPointMatching(p3, n)
  checkPointMatching(p4, n)

  # lpSolve and push relable algorithm both generate the same (optimal) assignment
  expect_true(all(p1 == p2))

  # check if additional optimizer infos are returned
  p5 = getOptimalPointMatching(x.coords, y.coords, method = "lp", full.output = TRUE)
  expect_list(p5)
  expect_matrix(p5$assignment, ncols = 2L, nrows = nrow(x.coords))
  expect_number(p5$opt.weight)
  expect_equal(p5$opt.method, "lp")
})
