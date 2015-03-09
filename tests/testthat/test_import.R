context("import of TSPlib files")

test_that("import works well for all EDGE_WEIGHT_TYPES/EDGE_WEIGHT_FORMATS", {
  testdata = system.file("inst/testdata", package = "netgen")
  test.files = list.files(testdata, full.names = TRUE)
  for (test.file in test.files) {
    x = importFromTSPlibFormat(test.file)
    expect_is(x, "Network", info = sprintf("Object is not a 'Network' for test file %s", basename(test.file)))
    expect_true(is.matrix(x$coordinates))
    expect_true(is.matrix(x$distance.matrix))
  }
})
