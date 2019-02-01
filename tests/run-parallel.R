library(testthat)
# FIXME: I am not sure if enabling this leads to travis 'hanging'. we currently must test it locally
# if (identical(Sys.getenv("TRAVIS"), "true") || identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "true")) {
if (identical(Sys.getenv("TRAVIS"), "True") || identical(Sys.getenv("APPVEYOR"), "True") || identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "true") || identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("mlr", "_parallel_")
}

