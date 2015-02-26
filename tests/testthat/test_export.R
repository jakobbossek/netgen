context("import and export to data formats")

test_that("import and export to TSPlib format is running fine", {
    expect_equal_networks = function(x, y) {
        expect_equal(getNumberOfNodes(x), getNumberOfNodes(y))
        expect_equal(getNumberOfClusters(x), getNumberOfClusters(y))
        expect_true(all((x$coordinates == y$coordinates) < 0.001))
        expect_equal(x$name, y$name)
        expect_equal(x$comment, y$comment)
    }
    x = generateRandomNetwork(n.points = 10L)
    x$name = "test"
    x$comment = c("n.points=10", "this is a nice")
    fn = tempfile(fileext = ".tsp")
    exportToTSPlibFormat(x, fn, use.extended.format = FALSE)
    unlink(fn)
    exportToTSPlibFormat(x, fn, use.extended.format = TRUE)
    y = importFromTSPlibFormat(fn)
    expect_equal_networks(x, y)
})