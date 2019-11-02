library(testthat)

set.seed(getOption("mlr.debug.seed"))

test_check("mlr", "_classif_[a-l].*")
