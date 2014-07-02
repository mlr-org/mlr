library(testthat)
# we only check 'base' tests for CRAN
# make sure YOU run ALL!
test_check("mlr", filter = "base")
