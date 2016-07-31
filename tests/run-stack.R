library(testthat)
if (identical(Sys.getenv("TRAVIS"), "true") || identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "true")) {
  test_check("mlr", filter = "stack_base")
  test_check("mlr", filter = "stack_boostedStacking")
  test_check("mlr", filter = "stack_aggregatePredictions")
  test_check("mlr", filter = "stack_resample")
}
