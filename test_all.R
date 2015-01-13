library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(netgen)
}

test_dir("tests/testthat")
