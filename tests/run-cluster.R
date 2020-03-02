library(testthat)

# no tests on CRAN

set.seed(getOption("mlr.debug.seed"))
test_check("mlr", "_cluster_")
