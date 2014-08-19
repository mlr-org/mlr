library(testthat)
# we only check 'base' tests for CRAN, but all for TRAVIS (which takes looong)
# make sure YOU run ALL!
if (identical(Sys.getenv("TRAVIS"), "true"))
  test_check("mlr")
else
  test_check("mlr", filter = "base")
