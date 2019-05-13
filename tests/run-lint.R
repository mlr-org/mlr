library(testthat)

if (getRversion() > "3.5.3") {
  suppressWarnings(RNGversion("3.5.0"))
}
set.seed(getOption("mlr.debug.seed"))

test_check("mlr", filter = "lint")
