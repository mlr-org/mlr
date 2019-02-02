library(testthat)
# no Appveyor because XMeans is not available on Windows
if (identical(Sys.getenv("TRAVIS"), "True") || identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "true") || identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("mlr", "_cluster_")
}
