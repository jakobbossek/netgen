context("import of TSPlib files")

test_that("import works well for all EDGE_WEIGHT_TYPES/EDGE_WEIGHT_FORMATS", {
  testdata = system.file("testdata", package = "netgen")
  test.files = list.files(testdata, full.names = TRUE, pattern = ".tsp$")
  for (test.file in test.files) {
    for (round.distances in c(TRUE, FALSE)) {
      x = suppressWarnings(importFromTSPlibFormat(test.file, round.distances = round.distances))
      expect_is(x, "Network", info = sprintf("Object is not a 'Network' for test file %s", basename(test.file)))
      expect_true(is.matrix(x$coordinates))
      expect_true(is.matrix(x$distance.matrix))
      expect_true(x$edge.weight.type %in% getValidEdgeWeightTypes())
    }
  }
})

test_that("import of optimal tour and tour length works", {
  testdata = system.file("testdata", package = "netgen")
  test.file = file.path(testdata, "a280.tsp")
  x = suppressWarnings(importFromTSPlibFormat(test.file, read.opt = TRUE))
  expect_equal(x$name, "a280")
  expect_true(!is.null(x$opt.tour.length))
  expect_equal(length(x$opt.tour.length), 1L)
  expect_true(!is.null(x$opt.tour))
  expect_equal(length(x$opt.tour), getNumberOfNodes(x))
})
