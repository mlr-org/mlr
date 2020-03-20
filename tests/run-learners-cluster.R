library(testthat)

# no tests on CRAN because the check limit is 10 mins

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  set.seed(getOption("mlr.debug.seed"))
  test_check("mlr", "_learners_all_cluster")
}
