library(testthat)
# NOT_CRAN = true is set by devtools and is the default way introduced by testthat to use skip_on_cran().
if (identical(Sys.getenv("TRAVIS"), "True") || identical(Sys.getenv("APPVEYOR"), "True") || identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "true") || identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("mlr", filter = "base_")
}
