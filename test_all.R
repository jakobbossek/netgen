library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(clugen)
}

test_dir("tests/testthat")
