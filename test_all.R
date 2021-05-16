library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(ecr3vis)
}

test_dir("tests/testthat")
