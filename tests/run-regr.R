library(testthat)
if (identical(Sys.getenv("TRAVIS"), "True") || identical(Sys.getenv("APPVEYOR"), "True") || identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "true") || identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("mlr", "_regr_")
}
