context("filtering TSPlib instances")

test_that("filtering works well", {
  testdata = system.file("testdata", package = "netgen")

  files = filterInstancesByDimension(testdata, expr = quote(value > 400), full.names = FALSE)
  expect_equal(length(files), 1L)
  expect_equal(files, "fl417.tsp")

  files = filterInstancesByDimension(testdata, expr = quote(value <= 400))
  expect_equal(length(files), length(list.files(testdata, pattern = "*.tsp$")) - 1L)

  files = filterInstancesByEdgeWeightType(testdata, expr = quote(value %in% "ATT"), full.names = FALSE)
  expect_equal(files, "att48.tsp")
  expect_equal(length(files), 1L)
})
