context("generate random instance")

test_that("generate random instance works as expected", {
    n.points = 100L
    n.dims = 2:4
    for (n.dim in n.dims) {
        inst = generateRandomInstance(n.points, n.dim = n.dim)
        expect_true(is.data.frame(inst))
        expect_equal(n.points, nrow(inst))
        expect_equal(n.dim, ncol(inst), info = paste("Number of columns does not match for n.dim", n.dim))
    }

    # check if points are within bounds
    lower = 0
    upper = 0.5

    inst = generateRandomInstance(n.points = 1000L, lower = lower, upper = upper)
    expect_true(all((inst >= lower) & (inst <= upper)))
})