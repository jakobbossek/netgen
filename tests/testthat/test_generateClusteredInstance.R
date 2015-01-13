context("generate clustered instance")

test_that("generate clustered instance works as expected", {
    n.cluster = 5L
    n.points = 100L
    inst = generateClusteredInstance(n.cluster, n.points)
    expect_is(inst, "ClusteredNetwork")
    expect_equal(n.cluster, length(unique(inst$membership)))
    expect_true(length(setdiff(n.cluster, unique(inst$membership))) == 0)
    expect_equal(n.points, nrow(inst$coordinates))

    # check if as.data.frame works as expected
    df = as.data.frame(inst)
    expect_equal(nrow(df), n.points)
    expect_equal(ncol(df), 3L) # two dimensions plus membership column

    df = as.data.frame(inst, include.membership = FALSE)

    expect_equal(nrow(df), n.points)
    expect_equal(ncol(df), 2L) # no membership column

    # check plotting
    library(ggplot2)
    pl = autoplot(inst)
    expect_is(pl, c("gg", "ggplot"))
})