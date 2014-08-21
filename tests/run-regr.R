library(testthat)
if (identical(Sys.getenv("TRAVIS"), "true")) {
  test_check("mlr", "_regr_")
}
