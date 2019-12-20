library(testthat)

# no tests on CRAN
if (identical(Sys.getenv("NOT_CRAN"), "true")) {

  set.seed(getOption("mlr.debug.seed"))
  test_check("mlr", filter = "lint")
}
