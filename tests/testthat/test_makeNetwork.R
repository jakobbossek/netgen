context("makeNetwork function")

test_that("there is a warning on zero distances", {
  coordinates = matrix(c(1, 2, 1, 2), byrow = TRUE, ncol = 2L)
  expect_warning(makeNetwork(coordinates), regexp = "zero")
})
